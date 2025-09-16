# Project: Description of In-depth Reporting on WeChat (2013-2025)

This project aims at quantitatively description of the content of in-depth reporting in China in the past decade. The studies were conducted in 2025 (April - September)

Author: Ruomeng Liu, M.Phil. candidate at Journalism and Media Studies Centre, HKU (date: September 2025).

Email: ruomeng.6@connect.hku.hk


### File Description
data properties:
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
- 01_time_trend_first-person.R: plot the first-person pronouns trend
- 02_moral_analysis_key.R: plot the moral prevalence temporal dynamics




Requirement
stm, tidyverse, dplyr, ggplot2, ggpubr, reshape2, wordcloud, plotly
