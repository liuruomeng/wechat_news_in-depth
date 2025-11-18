library(gridExtra)
library(grid)
library(dplyr)
library(ggplot2)
library(scales)


topic_account_matrix <- as.data.frame(stm_model$theta) %>%
  mutate(account = meta$account) %>%
  group_by(account) %>%
  summarise(across(starts_with("V"), mean, na.rm = TRUE))


topic_account_long <- topic_account_matrix %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "Topic",
    values_to = "Proportion"
  ) %>%
  mutate(
    Topic = paste0("Topic ", as.numeric(gsub("V", "", Topic)))
  )



topic_max_prop <- topic_account_long %>%
  group_by(Topic) %>%
  summarise(
    max_prop = max(Proportion, na.rm = TRUE),
    max_account = account[which.max(Proportion)],
    .groups = "drop"
  ) %>%
  arrange(desc(max_prop))

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
    # replace en name
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
    # Label
    Topic_Label = topic_labels[Topic]
  ) %>%
  # Type
  left_join(account_categories, by = c("account" = "account_english")) %>%
  arrange(category, account)


category_order <- c("Traditional Newspaper", "Commercial Magazine", "Digital Platform", "Self-media")
account_order <- filtered_data %>%
  distinct(account, category) %>%
  arrange(match(category, category_order), account) %>%
  pull(account)

filtered_data$account <- factor(filtered_data$account, levels = account_order)
filtered_data$category <- factor(filtered_data$category, levels = category_order)

library(ggplot2)
library(dplyr)
library(scales)

topic_labels_numbered <- c(
  "Topic 1" = "1. Sports",
  "Topic 2" = "2. Wildlife",
  "Topic 3" = "3. Urban Life", 
  "Topic 4" = "4. Entertainment",
  "Topic 5" = "5. Education",
  "Topic 6" = "6. Crime & Law",
  "Topic 7" = "7. Labor",
  "Topic 8" = "8. Social Issues",
  "Topic 9" = "9. Intl. Culture",
  "Topic 10" = "10. Art & Heritage",
  "Topic 11" = "11. Psychology",
  "Topic 12" = "12. Healthcare",
  "Topic 13" = "13. Family",
  "Topic 14" = "14. Politics",
  "Topic 15" = "15. Justice",
  "Topic 16" = "16. Film & TV",
  "Topic 17" = "17. Business",
  "Topic 18" = "18. Literature",
  "Topic 19" = "19. Technology",
  "Topic 20" = "20. Science",
  "Topic 21" = "21. Intl. Relations"
)

filtered_data <- filtered_data %>%
  mutate(Topic_Numbered = topic_labels_numbered[Topic])

# Color Choice

colors_a <- c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C")

colors_b <- c("#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4", "#43A2CA", "#0868AC")


colors_c <- c("#FFF5F0", "#FEE5D9", "#FCBBA1", "#FC9272", "#FB6A4A", "#DE2D26", "#A50F15")


colors_d <- c("#FCFBFD", "#EFEDF5", "#DADAEB", "#BCBDDC", "#9E9AC8", "#756BB1", "#54278F")


colors_e <- c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#636363", "#252525")


colors_f <- c("#FDE725", "#B5DE2B", "#6DCD59", "#35B779", "#1F9E89", "#26828E", "#31688E")


colors_g <- c("#FFFFFF", "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C")

# heatmap

library(scales)



p_heatmap_improved <- ggplot(
  filtered_data, 
  aes(x = Topic_Numbered, y = account, fill = Proportion)
) +
  

  geom_tile(color = "white", linewidth = 0.6) +
  

  geom_text(
    data = filter(filtered_data, Proportion > 0.12), 
    aes(label = percent(Proportion, accuracy = 1)), 
    color = "white", 
    size = 3.2,  
    fontface = "bold"
  ) +
  

  facet_grid(
    category ~ ., 
    scales = "free_y", 
    space = "free_y",
    switch = "y"  
  ) +
  
 
  scale_fill_gradientn(
    colors = c("#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4", "#43A2CA", "#0868AC"),
    values = rescale(c(0, 0.05, 0.10, 0.15, 0.20, 0.30, 0.40)),
    name = "Proportion",
    labels = percent_format(accuracy = 1),
    limits = c(0, max(filtered_data$Proportion)),
    breaks = seq(0, 0.4, 0.1),
    guide = guide_colorbar(
      barwidth = 16,
      barheight = 0.8,
      title.position = "top",
      title.hjust = 0.5,
      frame.colour = "black",
      frame.linewidth = 0.5,
      ticks.colour = "black",
      ticks.linewidth = 0.5
    )
  ) +
  

  labs(
    x = NULL, 
    y = NULL,
    caption = "Note: Only proportions above 12% are labeled."
  ) +
  

  theme_minimal(base_size = 10, base_family = "sans") +
  theme(
 
    plot.title = element_text(
      size = 12,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    
  
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1,
      vjust = 1,
      size = 9.5,  
      color = "black",
      margin = margin(t = 2)  
    ),
    
 
    axis.text.y = element_text(
      size = 9.5, 
      color = "black",
      hjust = 1,
      margin = margin(r = 2)  
    ),
    

    strip.text.y.left = element_text(
      size = 10,
      face = "bold",
      color = "black",
      angle = 0  
    ),
    strip.background = element_rect(
      fill = "gray88", 
      color = "gray50", 
      linewidth = 0.6
    ),
    strip.placement = "outside",  
    
  
    legend.position = "bottom",
    legend.title = element_text(
      size = 10,
      face = "bold"
    ),
    legend.text = element_text(
      size = 9
    ),
    legend.margin = margin(t = 10),
    legend.box.margin = margin(t = 5),
    
   
    panel.grid = element_blank(),
    panel.spacing.y = unit(0.4, "lines"),
    panel.border = element_blank(),

    plot.caption = element_text(
      size = 8,
      hjust = 0, 
      margin = margin(t = 10),
      color = "gray30"
    ),
    

    plot.margin = margin(10, 15, 10, 15, "pt")
  )

print(p_heatmap_improved)

ggsave(
  "Figure_5_Topic_Distribution_Improved.pdf",
  plot = p_heatmap_improved,
  width = 10,      
  height = 6.5,    
  units = "in",
  dpi = 600
)
