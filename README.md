# Description of In-depth Reporting on WeChat (2013-2025)

This project aims at quantitatively description of the content of in-depth reporting in China in the past decade. The studies were conducted in 2025 (April - September)

*   **Author**: Ruomeng Liu
*   **Contact**: [ruomeng.6@connect.hku.hk](mailto:ruomeng.6@connect.hku.hk)


### File Description

#### File Structure

The project is organized into the following directories and key files:

```

├── 01_data/
│   ├── 01_raw/
│   │   └── 2025-09-04_merged_raw_data.csv
│   ├── 02_processed/
│   │   ├── 2025-09-09_final_cleaned_data.csv
│   │   ├── web_data_with_abstractions_sample.csv
│   │   └── wechat_data_with_abstractions_sample.csv
│   └── 03_intermediate/
│       ├── 2025-09-09_enhanced_content_data.csv
│       ├── 2025-09-09_data_with_moral_dimensions_fixed.csv
│       ├── 09-11_stm_k21_results/
│       └── bert_analysis_final_corrected/
├── 02_scripts/
│   ├── time_trend_first-person.R
│   ├── moral_analysis_key.R
│   ├── stm_analysis.R
|   ├── 04_embedding_bert.py
|   ├── 05_hdbscan_model.py
│   └── fig_[].R
├── supplement_materials/
│   └── [original_chinese_docs]
├── environment.yml         # Python (Conda) environment dependencies
├── renv.lock               # R (renv) environment dependencies
└── README.md               # This file
```
##### data properties:
- content: the full-text of WeChat article
- publish_time: the timestamp when the WeChat post was released
- title: the title of the article
- url: the address for the WeChat article
- account: the news outlet that publishes the article

#### data

- 2025-09-04_merged_raw_data.csv: raw data
- 2025-09-09_final_cleaned_data.csv: clean data
- 2025-09-09_enhanced_content_data.csv: first-person pronouns illustration
- 2025-09-09_data_with_moral_dimensions_fixed: moral foundation analysis
- 09-11_stm_k21_results: structural topic model(stm) file

#### scirpts
- time_trend_first-person.R: plot the first-person pronouns trend
- moral_analysis_key.R: plot the moral prevalence temporal dynamics
- stm_analysis.R: structural topic model, build, parameter section, preliminary analysis
- embedding_bert.py: generate the embeddings using BERT and analyze
- hdbscan_model.py: process the case study data and run the HDBSCAN model
- fig_[].R: plot the figures in the paper



Data avalablity: [OSF.](https://osf.io/k4r65/overview)
