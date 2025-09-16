import pandas as pd
import jieba
import re
import numpy as np
from sentence_transformers import SentenceTransformer
from bertopic import BERTopic
from sklearn.feature_extraction.text import CountVectorizer
from hdbscan import HDBSCAN
from sklearn.metrics.pairwise import cosine_similarity
from scipy.optimize import linear_sum_assignment
from typing import List, Tuple, Dict, Optional

class AdvancedTopicComparison:
    def __init__(self):
        self.embedding_model = SentenceTransformer('paraphrase-multilingual-mpnet-base-v2')
        self.topic_model_p1 = None
        self.topic_model_p2 = None
        self.custom_stopwords = {
           '这篇', '新闻', '文章', '报道', '核心', '主题', '主要', '内容', '包括', '围绕',
           '展开', '分析', '探讨', '揭示', '反映', '展现', '通过', '以及', '同时', '现象',
           '问题', '提出', '形成', '指出', '强调', '方面', '层面', '关键', '一个', '几个',
           '这个', '这种', '这些', '的', '是', '了', '在', '也', '还', '并', '可以', '概括为',
           '归纳为', '聚焦于', '例如', '不仅', '而且', '成为', '如何', '进行', '甚至', '部分',
           '作为', '其中', '进行', '描述', '什么', '为什么', '一种', '这些', '那些', '我们',
           '他们', '她们', '它', '的', '地', '得',
           '中国', '发展', '影响', '案例', '文化', '历史' 
        }
        
    def preprocess_texts(self, texts: List[str]) -> List[str]:
        processed_texts = []
        print("preprocess....")
        for i, text in enumerate(texts):
            if i > 0 and i % 200 == 0: print(f"  - progress: {i}/{len(texts)}")
            if not isinstance(text, str): continue
            text = re.sub(r'[^\u4e00-\u9fff]', ' ', text)
            words = [word for word in jieba.cut(text) if len(word) > 1]
            processed_texts.append(' '.join(words))
        print("finished preprocess.")
        return processed_texts

    def guide_and_train_model(self, texts: List[str], period_name: str, num_topics: Optional[int] = None):
        processed_texts = self.preprocess_texts(texts)
        hdbscan_model = HDBSCAN(min_cluster_size=5, min_samples=1, metric='euclidean', 
                                cluster_selection_method='eom', prediction_data=True)
        vectorizer_model = CountVectorizer(stop_words=list(self.custom_stopwords), ngram_range=(1, 2))
        temp_model = BERTopic(
            embedding_model=self.embedding_model, hdbscan_model=hdbscan_model,
            vectorizer_model=vectorizer_model, language="multilingual",
            calculate_probabilities=True, nr_topics=num_topics, verbose=False
        ).fit(processed_texts)
        print(f" {period_name} model set, including {len(temp_model.get_topic_info())-1} topics.")
        return temp_model

    def _align_topics_with_threshold(self, threshold: float) -> Tuple[List, List, List]:
        if self.topic_model_p1.topic_embeddings_ is None or self.topic_model_p2.topic_embeddings_ is None: return [], [], []
    
        embeddings1 = self.topic_model_p1.topic_embeddings_[1:]
        embeddings2 = self.topic_model_p2.topic_embeddings_[1:]
        if embeddings1.shape[0] == 0 or embeddings2.shape[0] == 0: return [], [], []
        
        similarity_matrix = cosine_similarity(embeddings1, embeddings2)
        row_indices, col_indices = linear_sum_assignment(-similarity_matrix)
        
        topic_ids1 = sorted([k for k in self.topic_model_p1.get_topics() if k != -1])
        topic_ids2 = sorted([k for k in self.topic_model_p2.get_topics() if k != -1])
        
        alignments = []
        for row_idx, col_idx in zip(row_indices, col_indices):
            if row_idx < len(topic_ids1) and col_idx < len(topic_ids2):
                if similarity_matrix[row_idx, col_idx] >= threshold:
                    alignments.append({
                        'topic1_id': topic_ids1[row_idx], 
                        'topic2_id': topic_ids2[col_idx], 
                        'similarity': similarity_matrix[row_idx, col_idx]
                    })

        matched_t1 = {a['topic1_id'] for a in alignments}
        matched_t2 = {a['topic2_id'] for a in alignments}
        disappeared = [tid for tid in topic_ids1 if tid not in matched_t1]
        emerging = [tid for tid in topic_ids2 if tid not in matched_t2]
        
        return alignments, disappeared, emerging

    def generate_evolution_table(self, threshold: float, num_words: int):
        """
        Directly calculates topic evolution based on a specified threshold and number of keywords and saves it to a CSV file.
        This function handles three types of topics: aligned, disappearing, and emerging.
        """
        print("\n" + "="*60)
        print(f"start generating topic evolution table with parameters:")
        print(f" similarity threshold: {threshold}")
        print(f" topic number of each: {num_words}")
        print("="*60)
        
        alignments, disappeared, emerging = self._align_topics_with_threshold(threshold)
        
        def get_words(model, topic_id):
            try:
                return ' | '.join([word for word, _ in model.get_topic(topic_id)][:num_words])
            except Exception as e:
                print(f" warning")
                return ""

        data = []
        for align in alignments:
            data.append({
                'change_type': 'Aligned',
                'topic_label_p1': f"T1_{align['topic1_id']}",
                'keywords_p1': get_words(self.topic_model_p1, align['topic1_id']),
                'topic_label_p2': f"T2_{align['topic2_id']}",
                'keywords_p2': get_words(self.topic_model_p2, align['topic2_id']),
                'similarity': align['similarity']
            })
        for topic_id in disappeared:
            data.append({
                'change_type': 'Disappeared',
                'topic_label_p1': f"T1_{topic_id}",
                'keywords_p1': get_words(self.topic_model_p1, topic_id),
                'topic_label_p2': '',
                'keywords_p2': '',
                'similarity': np.nan
            })
        for topic_id in emerging:
            data.append({
                'change_type': 'Emerging',
                'topic_label_p1': '',
                'keywords_p1': '',
                'topic_label_p2': f"T2_{topic_id}",
                'keywords_p2': get_words(self.topic_model_p2, topic_id),
                'similarity': np.nan
            })
            
        df = pd.DataFrame(data)
        df = df[['change_type', 'topic_label_p1', 'keywords_p1', 'topic_label_p2', 'keywords_p2', 'similarity']]
        
        output_filename = f'topic_evolution_table_threshold_{threshold}_top_{num_words}_words.csv'
        df.to_csv(output_filename, index=False, encoding='utf-8-sig')
        
        print(f"final evolution table has been saved to {output_filename}")
    
        return df

analyzer = AdvancedTopicComparison()

try:
    df1 = pd.read_csv('../../01_data/web_data_with_abstractions_sample.csv')
    df2 = pd.read_csv('../../01_data/wechat_data_with_abstractions_sample.csv')
    
except FileNotFoundError as e:
    print(f"error:({e})")
    
NUM_TOPICS_TO_GENERATE = 10


analyzer.topic_model_p1 = analyzer.guide_and_train_model(df1['abstraction'].tolist(), "Period1", num_topics=NUM_TOPICS_TO_GENERATE)
analyzer.topic_model_p2 = analyzer.guide_and_train_model(df2['abstraction'].tolist(), "Period2", num_topics=NUM_TOPICS_TO_GENERATE)

def print_topic_keywords(model, period_name, num_words=10):
    if not model: return
    print(f"\n--- {period_name} Keywords (Top {num_words}) ---")
    topic_info = model.get_topic_info()
    for _, row in topic_info[topic_info.Topic != -1].iterrows():
        top_words = " | ".join([word for word, _ in model.get_topic(row.Topic)][:num_words])
        print(f"  Topic {row.Topic} (Docs: {row.Count}): {top_words}")

print_topic_keywords(analyzer.topic_model_p1, "Period1", num_words=10)
print_topic_keywords(analyzer.topic_model_p2, "Period2", num_words=10)

CHOSEN_THRESHOLD = 0.75
NUM_KEYWORDS = 10
try:
    if analyzer and analyzer.topic_model_p1 and analyzer.topic_model_p2:
   
        final_df = analyzer.generate_evolution_table(
            threshold=CHOSEN_THRESHOLD, 
            num_words=NUM_KEYWORDS
        )
        display(final_df)
        
    else:
        print("Error: Topic models for one or both periods are not initialized.")
except NameError:
    print("Error: Analyzer or topic models are not defined.")
except Exception as e:
    print(f"Error at execution {e}")