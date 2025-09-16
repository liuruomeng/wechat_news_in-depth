import pandas as pd
import torch
from transformers import BertTokenizer, BertModel
import numpy as np
import gc
import time
import re
from tqdm import tqdm
import warnings
warnings.filterwarnings('ignore')

class BertAnalyzer:
    def __init__(self):
     
        print("初始化BERT分析器...")
        
        if torch.backends.mps.is_available():
            self.device = torch.device("mps")
            print("Apple Silicon (MPS)")
        else:
            self.device = torch.device("cpu")
            print("CPU")
        
        torch.set_num_threads(4)
        
        model_name = 'bert-base-chinese'
        self.tokenizer = BertTokenizer.from_pretrained(model_name)
        self.model = BertModel.from_pretrained(model_name)
        self.model.to(self.device)
        self.model.eval()
     
        self.personal_refs = ["我", "我们", "个人", "家庭", "经历", "感受", "故事", "情感", "内心", "自我"]
        self.macro_refs = ["社会", "国家", "集体", "制度", "结构", "公共", "历史", "发展", "公众", "体系"]
        
        self.reference_vectors = self._get_non_contextual_reference_vectors()
        
        if self.reference_vectors:
            print(“BERT initialization finished”)
        else:
            raise RuntimeError(“Failed”)

    def _get_non_contextual_reference_vectors(self):
        final_vectors = {}
        embedding_matrix = self.model.get_input_embeddings().weight.detach()

        for group_name, ref_words in [('personal', self.personal_refs), ('macro', self.macro_refs)]:
            group_vectors = []
            print(f"  Caulculate {group_name} dimensions reference vector… )
            for word in ref_words:
                token_ids = self.tokenizer.encode(word, add_special_tokens=False)
                if not token_ids:
                    print(f" {word} (segmentation failed)”)
                    continue
                
                word_embedding = torch.mean(embedding_matrix[token_ids], axis=0)
                group_vectors.append(word_embedding)
                print(f"    ✓ {word}")

            if group_vectors:
                final_vectors[group_name] = torch.mean(torch.stack(group_vectors), axis=0).cpu().numpy()
                print(f"  {group_name.capitalize()} dimension: {len(group_vectors)} effective“)
            else:
                print(f"  no {group_name} ")
                return None
        
        return final_vectors

    def _get_embeddings_for_occurrences(self, occurrences_batch, target_word):

        context_windows = []
        target_tokens = self.tokenizer.tokenize(target_word)

        # contextual window
        for occ in occurrences_batch:
            text = occ['text']
            start_pos = occ['start']
            end_pos = occ['end']
            
            # target-word
            window_start = max(0, start_pos - 250)
            window_end = min(len(text), end_pos + 250)
            context_windows.append(text[window_start:window_end])

        if not context_windows:
            return []
        inputs = self.tokenizer(context_windows, return_tensors='pt', 
                              truncation=True, max_length=512, padding=True)
        inputs = {k: v.to(self.device) for k, v in inputs.items()}
        
        with torch.no_grad():
            outputs = self.model(**inputs)
            hidden_states = outputs.last_hidden_state

        batch_embeddings = []
        for i in range(len(context_windows)):
            input_ids_i = inputs['input_ids'][i].cpu().tolist()
            tokens = self.tokenizer.convert_ids_to_tokens(input_ids_i, skip_special_tokens=True)
            
            target_indices_in_window = []
            found = False

            for j in range(len(tokens) - len(target_tokens) + 1):
                if tokens[j:j+len(target_tokens)] == target_tokens:
                    target_indices_in_window.extend(range(j + 1, j + 1 + len(target_tokens))) # +1 for [CLS]
                    found = True
                    break

            if found:
                embedding = hidden_states[i, target_indices_in_window].mean(dim=0)
                batch_embeddings.append(embedding.cpu().numpy())
            else:
                batch_embeddings.append(None)
        
        return batch_embeddings

    def process_word(self, data, target_word, batch_size=32):
        

        target_data = data[data['content'].str.contains(target_word, na=False)].copy()
        
        if target_data.empty:
            print(“fail to find the text”)
            return None
        occurrences = []
        for idx, row in target_data.iterrows():
            text = row['content']
           
            for match in re.finditer(re.escape(target_word), text):
                occurrences.append({
                    'original_index': idx, 
                    'text': text,
                    'start': match.start(),
                    'end': match.end(),
                    'year': row['year'],
                    'time_period': row['time_period']
                })
        
        total_occurrences = len(occurrences)
        print(f" {len(target_data)} articles, {total_occurrences} times’{target_word}’appearance”)
        if total_occurrences == 0:
            return None
        all_embeddings = []
        for i in tqdm(range(0, total_occurrences, batch_size), desc=f”process’{target_word}’occurence):
            batch_occurrences = occurrences[i:i+batch_size]
            embeddings = self._get_embeddings_for_occurrences(batch_occurrences, target_word)
            all_embeddings.extend(embeddings)
            
            if i % (batch_size * 20) == 0:
                gc.collect()

        for i, emb in enumerate(all_embeddings):
            occurrences[i]['embedding'] = emb
        
        successful_occurrences = [occ for occ in occurrences if occ['embedding'] is not None]

        if not successful_occurrences:
            return None


        occ_df = pd.DataFrame(successful_occurrences)

        def aggregate_embeddings(embeddings):
            return np.mean(np.vstack(embeddings), axis=0)


        agg_results = occ_df.groupby('original_index').agg(

            avg_embedding=('embedding', aggregate_embeddings),

            year=('year', 'first'),
            time_period=('time_period', 'first')
        ).reset_index()

        print(f" already aggregate to {len(agg_results)} independent articles”)


        final_results = []
        for _, row in agg_results.iterrows():
            avg_embed = row['avg_embedding']
            
            personal_sim = self._compute_cosine_similarity(avg_embed, self.reference_vectors['personal'])
            macro_sim = self._compute_cosine_similarity(avg_embed, self.reference_vectors['macro'])
            
            if personal_sim is not None and macro_sim is not None:
# normalized value
                denominator = personal_sim + macro_sim
                if denominator > 1e-6: 
                    narrative_bias = (personal_sim - macro_sim) / denominator
                else:
                    narrative_bias = 0.0

                final_results.append({
                    'target_word': target_word,
                    'year': row['year'],
                    'time_period': row['time_period'],
                    'personal_similarity': personal_sim,
                    'macro_similarity': macro_sim,
                    'narrative_bias': narrative_bias,
                    'method': 'BERT_Revised'
                })
        
        return pd.DataFrame(final_results) if final_results else None

    def _compute_cosine_similarity(self, vec1, vec2):
        try:
            vec1 = np.asarray(vec1, dtype=np.float32)
            vec2 = np.asarray(vec2, dtype=np.float32)
            dot_product = np.dot(vec1, vec2)
            norm1 = np.linalg.norm(vec1)
            norm2 = np.linalg.norm(vec2)
            if norm1 == 0 or norm2 == 0: return 0.0
            return float(dot_product / (norm1 * norm2))
        except (ValueError, TypeError):
            return None

def main():
    try:
        # file path
        data = pd.read_csv("../../../01_data/02_processed/2025-09-09_final_cleaned_data.csv")
    except FileNotFoundError:
        print(“error”)

    data['publish_time'] = pd.to_datetime(data['publish_time'])
    data['year'] = data['publish_time'].dt.year
    data['time_period'] = data['year'].apply(
        lambda x: 'early' if x <= 2016 else ('mid' if x <= 2020 else 'late')
    )
    print(f”data in total: {len(data)} rows”)
    
    target_words = ['真相', '复杂', '理解', '公正']
    
    analyzer = BertAnalyzer()
    
    all_results = []
    start_time = time.time()
    
    for word_idx, word in enumerate(target_words):
        print(f"\n progress: [{word_idx+1}/{len(target_words)}] start to process ’{word}'")
        word_start_time = time.time()
        result_df = analyzer.process_word(data, word, batch_size=32)
        
        word_duration = (time.time() - word_start_time) / 60
        
        if result_df is not None and not result_df.empty:
            all_results.append(result_df)
            print(f"'{word}' finished，{len(result_df)} samples get, time cost {word_duration:.1f} mins”)
        else:
            print(f"✗ '{word}' no effective samples，{word_duration:.1f} mins”)
        
        gc.collect()
    
    total_duration = (time.time() - start_time) / 60
    
    if all_results:
        final_results = pd.concat(all_results, ignore_index=True)
        
        output_filename = 'bert_full_analysis_results_revised.csv'
        final_results.to_csv(output_filename, index=False, encoding='utf-8-sig')

if __name__ == "__main__":
    main()