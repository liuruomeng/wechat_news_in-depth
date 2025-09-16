library(stm)
library(tidyverse)
library(dplyr)
library(corrplot)
library(reshape2)
library(RColorBrewer)
library(pheatmap)
library(gridExtra)
library(grid)



#in_depth_corpus <- read_csv("../../01_data/02_processed/2025-09-09_final_cleaned_data.csv") %>%
#  select(title, account, publish_time, content)

stm_df <- in_depth_corpus %>%
  mutate(
    content        = paste(title, content, sep = " "),
    publish_time = as.POSIXct(publish_time),
    days        = as.numeric(difftime(publish_time, min(publish_time), units = "days"))
  )

mixseg <- worker()
stm_df$text_seg <- sapply(stm_df$content, function(s) paste(segment(s, mixseg), collapse = " "))
meta_df <- stm_df %>% select(account, days) %>% as.data.frame(stringsAsFactors = FALSE)

# textProcessor + prepDocuments
processed <- textProcessor(
  documents = stm_df$text_seg,
  metadata  = meta_df,
  lowercase = FALSE
)
out <- prepDocuments(
  processed$documents,
  processed$vocab,
  processed$meta,
  lower.thresh = 20
)
docs  <- out$documents
vocab <- out$vocab
meta  <- out$meta


stm_model <- stm(
  documents  = docs,
  vocab      = vocab,
  K          = 21,
  prevalence = ~ account + s(days),
  data       = meta,
  max.em.its = 150,
  init.type  = "Spectral"
)
# saveRDS(stm_model, file = "09-11_stm_k21_results")

summary(stm_model)

labelTopics(stm_model,topics = 15, n = 30)

prep <- estimateEffect(1:21 ~ s(days), stm_model, meta = meta, uncertainty = "Global")
par(mfrow = c(5, 5), mar = c(3, 3, 2, 1))
for(i in 1:21) {
  plot(prep, "days", method = "continuous", topics = i,
       model = stm_model, printlegend = FALSE, 
       main = paste("topic", i), xlab = "", ylab = "")
}


time_points <- seq(min(meta$days), max(meta$days), length.out = 100)
all_predictions <- data.frame()

for (i in 1:21) {
  cat("Processing topic", i, "\n")  
  single_prep <- estimateEffect(i ~ s(days), stm_model, meta = meta, uncertainty = "Global")
  
  png(filename = tempfile())
  plot_result <- try({
    plot(single_prep, "days", method = "continuous", topics = 1,
         model = stm_model, printlegend = FALSE, 
         main = paste("Topic", i))
  }, silent = TRUE)
  dev.off()
  

  predictions <- sapply(time_points, function(day_val) {

    new_meta <- data.frame(days = day_val)
    
    closest_indices <- which(abs(meta$days - day_val) <= 5)  # 5-day window
    if(length(closest_indices) > 0) {
      mean(stm_model$theta[closest_indices, i], na.rm = TRUE)
    } else {
      NA
    }
  })
  
  # remove the NA
  valid_indices <- !is.na(predictions)
  
  if(sum(valid_indices) > 0) {
    topic_data <- data.frame(
      time = time_points[valid_indices],
      date = min(stm_df$publish_time) + time_points[valid_indices],
      topic = paste0("Topic ", i),
      proportion = predictions[valid_indices]
    )
    all_predictions <- rbind(all_predictions, topic_data)
  }
}


ggplot(all_predictions, aes(x = date, y = proportion)) +
  geom_line(color = "blue", alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.3) +
  facet_wrap(~ topic, scales = "free_y", ncol = 4) +
  scale_x_datetime(
    date_breaks = "1 year",     
    date_labels = "%Y",        
    expand = c(0.02, 0)
  ) +
  labs(title = "All Topics Over Time", 
       x = "Time", y = "Topic Proportion") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 8)
  )





topic_account <- data.frame(
  account = meta$account,
  stm_model$theta
)
names(topic_account)[2:22] <- paste0("Topic_", 1:21)

topic_account_summary <- topic_account %>%
  group_by(account) %>%
  summarise(across(starts_with("Topic_"), mean), .groups = 'drop')

print(topic_account_summary)


topic_account_long <- topic_account_summary %>%
  pivot_longer(cols = starts_with("Topic_"), 
               names_to = "Topic", 
               values_to = "Proportion") %>%
  mutate(Topic = str_replace(Topic, "Topic_", "Topic "))

topic_account_long <- topic_account_long %>%
  mutate(account = case_when(
    account == "三联生活周刊" ~ "Sanlian Lifeweek",
    account == "人物" ~ "Renwu Magazine",
    account == "冰点周刊" ~ "Bingdian Weekly", 
    account == "冷杉RECORD" ~ "Fir Record",
    account == "剥洋葱people" ~ "Peeling Onion: People",
    account == "北青深一度" ~ "Beijing Youth In-depth",
    account == "南方周末" ~ "Southern Weekly",
    account == "原点original" ~ "Original Point",
    account == "在人间living" ~ "The Livings",
    account == "智族Life" ~ "GQ Life",
    account == "极昼工作室" ~ "Polar Day Studio",
    account == "正面连接" ~ "Positive Connection",
    account == "澎湃人物" ~ "The Paper Profile",
    account == "猛犸工作室" ~ "Mammoth Studio",
    account == "真实故事计划" ~ "True Story Project",
    account == "谷雨实验室" ~ "Guyu Lab",
    TRUE ~ account  
  ))


# create the heatmap
ggplot(topic_account_long, aes(x = Topic, y = account, fill = Proportion)) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "lightblue", high = "darkblue",
                       midpoint = 0.1, name = "Proportion") +
  labs(title = "Topic Distribution Across Media Accounts",
       x = "Topics", y = "Media Accounts") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 14, hjust = 0.5),
    legend.position = "right"
  )

# top 3 topics for news outlets
top_topics <- topic_account_long %>%
  group_by(account) %>%
  slice_max(order_by = Proportion, n = 3) %>%
  ungroup()

ggplot(top_topics, aes(x = Proportion, y = reorder(account, Proportion))) +
  geom_segment(aes(x = 0, xend = Proportion, y = account, yend = account), 
               color = "gray60", size = 0.5) +
  geom_point(aes(color = Topic), size = 2.5) +
  scale_color_brewer(type = "qual", palette = "Set3", name = "Topic") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Top Topics by Media Account",
       x = "Topic Proportion", 
       y = "Media Accounts",
       caption = "Note: Shows top 3 topics per account") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    panel.grid.major.x = element_line(color = "gray90", size = 0.3),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
  ) +
  guides(color = guide_legend(nrow = 3, override.aes = list(size = 3)))


ggplot(topic_account_long, aes(x = reorder(Topic, Proportion, FUN = median), y = Proportion)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7, outlier.color = "red", outlier.size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, fill = "red") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Distribution of Topic Proportions Across Media Accounts",
       x = "Topics", 
       y = "Proportion",
       caption = "Red diamonds indicate means; red circles are outliers") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_line(color = "gray90", size = 0.3),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
  )



heatmap_matrix <- topic_account_summary %>%
  column_to_rownames("account") %>%
  select(starts_with("Topic_")) %>%
  as.matrix()

rownames(heatmap_matrix) <- c("Sanlian Lifeweek", "Renwu Magazine", "Bingdian Weekly", 
                              "Fir Record", "Peeling Onion: People", "Beijing Youth In-depth",
                              "Southern Weekly", "Original Point", "The Livings", "GQ Life",
                              "Polar Day Studio", "Positive Connection", "The Paper Profile", 
                              "Mammoth Studio", "True Story Project", "Guyu Lab")

pheatmap(heatmap_matrix,
         color = colorRampPalette(c("white", "lightblue", "darkblue"))(50),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         show_rownames = TRUE,
         show_colnames = TRUE,
         fontsize_row = 9,
         fontsize_col = 9,
         main = "Topic Distribution Across Media Accounts",
         border_color = "white")

topic_max_prop <- topic_account_long %>%
  group_by(Topic) %>%
  summarise(
    max_prop = max(Proportion),
    max_account = account[which.max(Proportion)],
    mean_prop = mean(Proportion),
    .groups = 'drop'
  ) %>%
  arrange(desc(max_prop))

print(topic_max_prop)


thresholds <- c(0.05, 0.08, 0.10, 0.12, 0.15, 0.20)

for(threshold in thresholds) {
  n_topics <- sum(topic_max_prop$max_prop > threshold)
  cat("阈值", threshold, ": 保留", n_topics, "个主题\n")
}

# threshold = 0.10
threshold <- 0.10

selected_topics <- topic_max_prop %>%
  filter(max_prop > threshold) %>%
  pull(Topic)

print(paste("threshold", threshold, "selected topics":", length(selected_topics), "个"))
print(selected_topics)

filtered_data <- topic_account_long %>%
  filter(Topic %in% selected_topics)


filtered_topic_nums <- str_extract(selected_topics, "\\d+")
filtered_matrix <- topic_account_summary %>%
  column_to_rownames("account") %>%
  select(all_of(paste0("Topic_", filtered_topic_nums))) %>%
  as.matrix()


rownames(filtered_matrix) <- c("Sanlian Lifeweek", "Renwu Magazine", "Bingdian Weekly", 
                               "Fir Record", "Peeling Onion: People", "Beijing Youth In-depth",
                               "Southern Weekly", "Original Point", "The Livings", "GQ Life",
                               "Polar Day Studio", "Positive Connection", "The Paper Profile", 
                               "Mammoth Studio", "True Story Project", "Guyu Lab")

pheatmap(filtered_matrix,
         color = colorRampPalette(c("white", "lightblue", "darkblue"))(50),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         show_rownames = TRUE,
         show_colnames = TRUE,
         fontsize_row = 10,
         fontsize_col = 10,
         main = paste("Topics with Max Proportion >", threshold),
         border_color = "white")


topic_champions <- topic_max_prop %>%
  filter(max_prop > threshold) %>%
  select(Topic, max_account, max_prop) %>%
  arrange(desc(max_prop))

ggplot(topic_champions, aes(x = max_prop, y = reorder(Topic, max_prop))) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = max_account), hjust = -0.05, size = 3) +
  scale_x_continuous(labels = scales::percent_format(), 
                     expand = expansion(mult = c(0, 0.2))) +
  labs(title = paste("Topics with Maximum Proportion >", scales::percent(threshold)),
       subtitle = "Labels show the media account with highest proportion for each topic",
       x = "Maximum Proportion", y = "Topics") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5)
  )





topic_max_prop %>%
  filter(max_prop > threshold) %>%
  print()

topic_stats <- topic_account_long %>%
  group_by(Topic) %>%
  summarise(
    mean_prop = mean(Proportion),
    sd_prop = sd(Proportion),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_prop))

ggplot(topic_stats, aes(x = mean_prop, y = reorder(Topic, mean_prop))) +
  geom_errorbarh(aes(xmin = mean_prop - sd_prop, xmax = mean_prop + sd_prop), 
                 height = 0.3, color = "gray50") +
  geom_point(size = 3, color = "steelblue") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "Average Topic Proportions Across Media Accounts",
       x = "Mean Proportion (± SD)", 
       y = "Topics") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_line(color = "gray90", size = 0.3)
  )







threshold <- 0.10
selected_topics <- topic_max_prop %>%
  filter(max_prop > threshold) %>%
  pull(Topic)

topic_champions <- topic_max_prop %>%
  filter(max_prop > threshold) %>%
  select(Topic, max_account, max_prop) %>%
  arrange(desc(max_prop))

filtered_topic_nums <- str_extract(selected_topics, "\\d+")
filtered_matrix <- topic_account_summary %>%
  column_to_rownames("account") %>%
  select(all_of(paste0("Topic_", filtered_topic_nums))) %>%
  as.matrix()

rownames(filtered_matrix) <- c("Sanlian Lifeweek", "Renwu Magazine", "Bingdian Weekly", 
                               "Fir Record", "Peeling Onion: People", "Beijing Youth In-depth",
                               "Southern Weekly", "Original Point", "The Livings", "GQ Life",
                               "Polar Day Studio", "Positive Connection", "The Paper Profile", 
                               "Mammoth Studio", "True Story Project", "Guyu Lab")


p_champions <- ggplot(topic_champions, aes(x = max_prop, y = reorder(Topic, max_prop))) +
  geom_col(fill = "darkred", alpha = 0.7, width = 0.6) +
  geom_text(aes(label = paste0(max_account, "\n(", scales::percent(max_prop, accuracy = 1), ")")), 
            hjust = -0.05, size = 2.8, lineheight = 0.8) +
  scale_x_continuous(labels = scales::percent_format(), 
                     expand = expansion(mult = c(0, 0.4)),
                     limits = c(0, max(topic_champions$max_prop) * 1.4)) +
  labs(title = "Topic Leaders", 
       x = "Max Proportion", y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5.5, 5.5, 5.5, 0)
  )


filtered_data <- topic_account_long %>%
  filter(Topic %in% selected_topics) %>%
  mutate(account = case_when(
    account == "三联生活周刊" ~ "Sanlian Lifeweek",
    account == "人物" ~ "Renwu Magazine",
    account == "冰点周刊" ~ "Bingdian Weekly", 
    account == "冷杉RECORD" ~ "Fir Record",
    account == "剥洋葱people" ~ "Peeling Onion: People",
    account == "北青深一度" ~ "Beijing Youth In-depth",
    account == "南方周末" ~ "Southern Weekly",
    account == "原点original" ~ "Original Point",
    account == "在人间living" ~ "The Livings",
    account == "智族Life" ~ "GQ Life",
    account == "极昼工作室" ~ "Polar Day Studio",
    account == "正面连接" ~ "Positive Connection",
    account == "澎湃人物" ~ "The Paper Profile",
    account == "猛犸工作室" ~ "Mammoth Studio",
    account == "真实故事计划" ~ "True Story Project",
    account == "谷雨实验室" ~ "Guyu Lab",
    TRUE ~ account
  ))

p_heatmap <- ggplot(filtered_data, aes(x = Topic, y = account, fill = Proportion)) +
  geom_tile(color = "white", size = 0.3) +
  scale_fill_gradient2(low = "white", mid = "lightblue", high = "darkblue",
                       midpoint = 0.1, name = "Proportion",
                       labels = scales::percent_format()) +
  labs(title = paste("Topic Distribution (Max Prop >", scales::percent(threshold), ")"),
       x = "Topics", y = "Media Accounts") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    plot.margin = margin(5.5, 0, 5.5, 5.5)
  )


grid.arrange(p_heatmap, p_champions, ncol = 2, widths = c(3, 1.5),
             top = textGrob("Topic Distribution and Leading Media Accounts", 
                            gp = gpar(fontsize = 14, fontface = "bold")))

