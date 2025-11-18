# sbert_contextual_analyzer.py
import pandas as pd
import torch
import numpy as np
from sentence_transformers import SentenceTransformer
from sklearn.metrics.pairwise import cosine_similarity
from tqdm import tqdm
import re
import time
import gc
from collections import defaultdict
import warnings

warnings.filterwarnings('ignore')

class SbertContextualAnalyzer:
    def __init__(self, model_name='shibing624/text2vec-base-chinese', batch_size=64):
        
        # auto
        if torch.cuda.is_available():
            self.device = 'cuda'
        elif torch.backends.mps.is_available():
            self.device = 'mps'
        else:
            self.device = 'cpu'
 

        self.model_name = model_name
        self.batch_size = batch_size
        
        print(f"Loading S-BERT '{self.model_name}'...")
        self.model = SentenceTransformer(self.model_name, device=self.device)
        
        self.ref_vec_individual, self.ref_vec_structural = self._get_reference_vectors()
    

    def _get_narrative_anchor_seeds(self):
        return {
            "Individual": ["内心", "自己", "亲身", "经历", "讲述", "回忆", "感受", "遭遇"],
            "Structural": ["体制", "制度", "国家", "规则", "权力", "系统", "历史", "政策"]
        }

    def _get_reference_vectors(self):
        seeds = self._get_narrative_anchor_seeds()
        individual_words = seeds["Individual"]
        structural_words = seeds["Structural"]
        
        all_seed_words = list(set(individual_words + structural_words))

        seed_embeddings = self.model.encode(
            all_seed_words, 
            convert_to_tensor=True, 
            device=self.device, 
            show_progress_bar=False
        )
        word_to_vec = {word: vec for word, vec in zip(all_seed_words, seed_embeddings)}
        
        ref_vec_ind = torch.mean(torch.stack([word_to_vec[w] for w in individual_words]), dim=0)
        ref_vec_str = torch.mean(torch.stack([word_to_vec[w] for w in structural_words]), dim=0)
        

        return ref_vec_ind.cpu().numpy(), ref_vec_str.cpu().numpy()

    def _calculate_narrative_bias(self, context_embedding):

        context_embedding = context_embedding.reshape(1, -1)
        ref_vec_ind_2d = self.ref_vec_individual.reshape(1, -1)
        ref_vec_str_2d = self.ref_vec_structural.reshape(1, -1)
        
        sim_individual = cosine_similarity(context_embedding, ref_vec_ind_2d)[0][0]
        sim_structural = cosine_similarity(context_embedding, ref_vec_str_2d)[0][0]
        
 
        numerator = sim_structural - sim_individual
        denominator = sim_structural + sim_individual
        
        if abs(denominator) < 1e-6: 
            return 0.0
            
        return numerator / denominator

    def analyze_corpus(self, df, target_words, text_column='content'):
        print(f"target words: {target_words}")
        sentences_to_process = []
        target_pattern = re.compile(r'|'.join(map(re.escape, target_words)))
        
        for index, row in tqdm(df.iterrows(), total=len(df), desc="scanning corpus for target words"):
            text = row[text_column]
            if not isinstance(text, str):
                continue
            
            sentences = re.split(r'([。？！])', text)
            sentences = ["".join(i) for i in zip(sentences[0::2], sentences[1::2])]

            for sentence in sentences:
               
                match = target_pattern.search(sentence)
                if match:
                   
                    sentences_to_process.append({
                        'doc_id': index,
                        'sentence': sentence.strip(),
                        'target_word_found': match.group(0) # 记录找到的第一个目标词
                    })

        if not sentences_to_process:
            print("no sentences found with target words.")
            return pd.DataFrame()
        
        print(f" {len(sentences_to_process)} sentences found containing target words.")

     
        print("\n Generating contextual embeddings using S-BERT...")
        
        unique_sentences = list(dict.fromkeys([s['sentence'] for s in sentences_to_process]))
        
        contextual_embeddings = self.model.encode(
            unique_sentences,
            batch_size=self.batch_size,
            show_progress_bar=True,
            convert_to_numpy=True,
            device=self.device
        )
        
        sentence_to_embedding_map = dict(zip(unique_sentences, contextual_embeddings))


        results_list = []
        
        for item in tqdm(sentences_to_process, desc="calculating narrative bias scores"):
            doc_id = item['doc_id']
            sentence = item['sentence']
            target_word = item['target_word_found']
            

            embedding = sentence_to_embedding_map[sentence]

            bias_score = self._calculate_narrative_bias(embedding)
            

            original_row = df.loc[doc_id]
            result_record = {
                'doc_id': doc_id,
                'target_word': target_word,
                'narrative_bias': bias_score,
                'sentence': sentence,
                **original_row.to_dict()
            }
            results_list.append(result_record)
            
        final_df = pd.DataFrame(results_list)

        del contextual_embeddings, sentence_to_embedding_map
        gc.collect()
        if self.device == 'cuda':
            torch.cuda.empty_cache()
        elif self.device == 'mps':
            torch.mps.empty_cache()
            
        return final_df


def main():
    start_time = time.time()
    
  
    print("读取数据...")
    try:
   # file path
        data_path = "../../../01_data/02_processed/2025-09-09_final_cleaned_data.csv"
        df = pd.read_csv(data_path)
    except FileNotFoundError:
        print(f"path error: '{data_path}'。")

        df['doc_id'] = df.index
        df.set_index('doc_id', inplace=True)

    if not df.index.is_unique:
        print("index contains duplicates, resetting index.")
        df.reset_index(drop=True, inplace=True)
        df['doc_id'] = df.index
        df.set_index('doc_id', inplace=True)

    df['publish_time'] = pd.to_datetime(df['publish_time'], errors='coerce')
    df.dropna(subset=['publish_time'], inplace=True) 
    df['year'] = df['publish_time'].dt.year
    df['month'] = df['publish_time'].dt.month
    df['year_month'] = df['publish_time'].dt.to_period('M')
    df['time_period'] = df['year'].apply(
        lambda x: 'early' if x <= 2016 else ('mid' if x <= 2020 else 'late')
    )
    print(f" {len(df)} valid records loaded.")

--
    target_words = ['真相', '复杂', '理解', '公正']
    
    analyzer = SbertContextualAnalyzer(batch_size=128)
    

    results_df = analyzer.analyze_corpus(df, target_words, text_column='content')

    if results_df is not None and not results_df.empty:

        timestamp = time.strftime("%Y%m%d")
        output_filename = f'sbert_contextual_analysis_{timestamp}.csv'
        

        results_df.to_csv(output_filename, index=False, encoding='utf-8-sig')
        
        total_duration = (time.time() - start_time) / 60
        

if __name__ == "__main__":
    main()