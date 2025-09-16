library(gridExtra)
library(grid)
library(dplyr)
library(ggplot2)


topic_labels <- c(
  "Topic 1" = "Sports and Competitions",
  "Topic 2" = "Wildlife and Environmental Conservation", 
  "Topic 3" = "Modern Urban Lifestyle and Youth Culture",
  "Topic 4" = "Entertainment and Live Streaming Industry",
  "Topic 5" = "Education and Schooling",
  "Topic 6" = "Crime, Law, and the Justice System",
  "Topic 7" = "Working Class and Migrant Labor",
  "Topic 8" = "Social Issues and Minority Group",
  "Topic 9" = "International Culture, Film, and Celebrities",
  "Topic 10" = "Art, History, and Cultural Heritage",
  "Topic 11" = "Modern Psychology, Relationships, and Mental Health",
  "Topic 12" = "Healthcare, Disease, and Public Health Crises",
  "Topic 13" = "Family Life and Personal Narratives",
  "Topic 14" = "Chinese Politics, Government, and History",
  "Topic 15" = "Social Justice, Victim Support, and Legal Rights",
  "Topic 16" = "Film and Television Industry",
  "Topic 17" = "Business, Finance, and Economics",
  "Topic 18" = "Literature, Publishing, and Intellectuals",
  "Topic 19" = "Technology and Internet Industry",
  "Topic 20" = "Science, Research, and Academia",
  "Topic 21" = "International Relations and Global Politics"
)


account_categories <- data.frame(
  account_english = c("Sanlian Lifeweek", "Renwu Magazine", "Fir Record", "GQ Life", "Mammoth Studio",
                      "Southern Weekly", "Bingdian Weekly", "Beijing Youth In-depth", "Peeling Onion: People", "Original Point",
                      "Polar Day Studio", "The Paper Profile", "Guyu Lab", "The Livings",
                      "True Story Project", "Positive Connection"),
  account_chinese = c("三联生活周刊", "人物", "冷杉RECORD", "智族Life", "猛犸工作室",
                      "南方周末", "冰点周刊", "北青深一度", "剥洋葱people", "原点original",
                      "极昼工作室", "澎湃人物", "谷雨实验室", "在人间living",
                      "真实故事计划", "正面连接"),
  category = c("Commercial Magazine", "Commercial Magazine", "Commercial Magazine", "Commercial Magazine", "Commercial Magazine",
               "Traditional Newspaper", "Traditional Newspaper", "Traditional Newspaper", "Traditional Newspaper", "Traditional Newspaper", 
               "Digital Platform", "Digital Platform", "Digital Platform", "Digital Platform",
               "Self-media", "Self-media"),
  stringsAsFactors = FALSE
)


threshold <- 0.10
selected_topics <- topic_max_prop %>%
  filter(max_prop > threshold) %>%
  pull(Topic)


topic_champions <- topic_max_prop %>%
  filter(max_prop > threshold) %>%
  mutate(Topic_Label = topic_labels[Topic]) %>%
  select(Topic, Topic_Label, max_account, max_prop) %>%
  arrange(desc(max_prop))


filtered_data <- topic_account_long %>%
  filter(Topic %in% selected_topics) %>%
  mutate(

    account = case_when(
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
    ),
    Topic_Label = topic_labels[Topic]
  ) %>%
  left_join(account_categories, by = c("account" = "account_english")) %>%

  arrange(category, account)


category_order <- c("Traditional Newspaper", "Commercial Magazine", "Digital Platform", "Self-media")
account_order <- filtered_data %>%
  distinct(account, category) %>%
  arrange(match(category, category_order), account) %>%
  pull(account)

filtered_data$account <- factor(filtered_data$account, levels = account_order)
filtered_data$category <- factor(filtered_data$category, levels = category_order)


p_heatmap <- ggplot(filtered_data, aes(x = Topic_Label, y = account, fill = Proportion)) +
  geom_tile(color = "white", size = 0.3) +

  geom_hline(yintercept = c(5.5, 10.5, 14.5), color = "red", size = 1, alpha = 0.8) +
  scale_fill_gradient2(low = "white", mid = "lightblue", high = "darkblue",
                       midpoint = 0.1, name = "Proportion",
                       labels = scales::percent_format()) +
  labs(title = "Topic Distribution by Media Category and Account",
       x = "", y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    plot.margin = margin(5.5, 0, 5.5, 5.5),
    panel.grid = element_blank()
  ) +

  annotate("text", x = 0.5, y = c(3, 8, 12.5, 15.5), 
           label = c("Traditional\nNewspaper", "Commercial\nMagazine", "Digital\nPlatform", "Self-media"),
           angle = 90, vjust = 0.5, hjust = 0.5, size = 3, fontface = "bold", color = "red")


topic_champions_display <- topic_champions %>%
  mutate(Topic_Label_Short = case_when(
    nchar(Topic_Label) > 25 ~ paste0(substr(Topic_Label, 1, 22), "..."),
    TRUE ~ Topic_Label
  ))

p_champions <- ggplot(topic_champions_display, aes(x = max_prop, y = reorder(Topic_Label_Short, max_prop))) +
  geom_col(fill = "darkred", alpha = 0.7, width = 0.6) +
  geom_text(aes(label = paste0(max_account, "\n(", scales::percent(max_prop, accuracy = 1), ")")), 
            hjust = -0.05, size = 2.5, lineheight = 0.8) +
  scale_x_continuous(labels = scales::percent_format(), 
                     expand = expansion(mult = c(0, 0.4)),
                     limits = c(0, max(topic_champions$max_prop) * 1.4)) +
  labs(title = "Leading Media for Each Topic", 
       x = "Max Proportion", y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5.5, 5.5, 5.5, 0)
  )


combined_plot <- grid.arrange(p_heatmap, p_champions, ncol = 2, widths = c(4, 2),
                              top = textGrob("Topic Distribution Across Media Categories with Leading Accounts", 
                                             gp = gpar(fontsize = 13, fontface = "bold")))

# optional: description of the instituitional affiliation group
category_legend <- data.frame(
  category = category_order,
  count = table(filtered_data$category)[category_order]
)

cat("institutional type:\n")
for(i in 1:nrow(category_legend)) {
  accounts_in_cat <- account_categories %>% 
    filter(category == category_legend$category[i]) %>% 
    pull(account_english)
  cat(category_legend$category[i], ":", paste(accounts_in_cat, collapse = ", "), "\n")
}


p_faceted <- ggplot(filtered_data, aes(x = Topic_Label, y = account, fill = Proportion)) +
  geom_tile(color = "white", size = 0.3) +
  
  geom_text(data = filter(filtered_data, Proportion > 0.12), 
            aes(label = scales::percent(Proportion, accuracy = 1)), 
            color = "white", size = 2.5, fontface = "bold") +
  
  facet_grid(category ~ ., scales = "free_y", space = "free_y",
             labeller = labeller(category = c(
               "Traditional Newspaper" = "Traditional Newspaper",
               "Commercial Magazine" = "Commercial Magazine", 
               "Digital Platform" = "Digital Platform",
               "Self-media" = "Self-media"
             ))) +
  
  scale_fill_gradient2(
    low = "white", 
    mid = "steelblue", 
    high = "darkblue",
    midpoint = 0.08, 
    name = "Proportion",
    labels = scales::percent_format(accuracy = 1)
  ) +
  
  labs(
    title = "Topic Distribution by Media Category",
    x = "Topics",
    y = "Media Accounts"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    strip.text.y = element_text(size = 10, face = "bold", angle = 0),
    strip.background = element_rect(fill = "gray90", color = "black"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.spacing = unit(0.5, "lines")
  )

print(p_faceted)
