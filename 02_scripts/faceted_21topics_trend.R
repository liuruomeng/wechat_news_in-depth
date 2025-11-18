library(readr)
library(dplyr)
library(jiebaR)
library(stm)
library(ggplot2)
library(tidyr)
library(lubridate)

### build the stm (ignore if finished)
in_depth_corpus <- read_csv("../01_data/02_processed/2025-09-09_final_cleaned_data.csv") %>%
  select(title, account, publish_time, content)

stm_df <- in_depth_corpus %>%
  mutate(
    content      = paste(title, content, sep = " "),
    publish_time = as.POSIXct(publish_time),
    days         = as.numeric(difftime(publish_time, min(publish_time), units = "days"))
  )

mixseg <- worker()
stm_df$text_seg <- sapply(stm_df$content, function(s) paste(segment(s, mixseg), collapse = " "))

meta_df <- stm_df %>% 
  select(account, days, publish_time) %>%  #  publish_time
  as.data.frame()

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

# save the model
saveRDS(stm_model, "stm_model_k21.rds")
saveRDS(meta, "meta_with_time.rds")


library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(tidyr)


topic_labels <- c(
  "1" = "Sports & Competitions",
  "2" = "Wildlife & Conservation",
  "3" = "Urban Youth Culture",
  "4" = "Entertainment & Streaming",
  "5" = "Education System",
  "6" = "Crime & Justice",
  "7" = "Working Class & Labor",
  "8" = "Social Issues & Minorities",
  "9" = "International Culture & Film",
  "10" = "Art & Cultural Heritage",
  "11" = "Psychology & Mental Health",
  "12" = "Healthcare & Public Health",
  "13" = "Family Life & Stories",
  "14" = "Politics & History",
  "15" = "Social Justice & Advocacy",
  "16" = "Film & TV Industry",
  "17" = "Business & Finance",
  "18" = "Literature & Publishing",
  "19" = "Technology & Internet",
  "20" = "Science & Research",
  "21" = "Global Politics"
)


topic_time_data <- as.data.frame(stm_model$theta) %>%
  mutate(
    publish_time = meta$publish_time,
    year_month = floor_date(publish_time, "month")
  ) %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "topic_num",
    values_to = "proportion"
  ) %>%
  mutate(
    topic_num = as.numeric(gsub("V", "", topic_num)),
    topic = factor(
      topic_num,
      levels = 1:21,
      labels = topic_labels[as.character(1:21)]
    )
  )

monthly_data <- topic_time_data %>%
  group_by(year_month, topic, topic_num) %>%
  summarise(
    proportion = mean(proportion, na.rm = TRUE),
    .groups = "drop"
  )

p1 <- ggplot(monthly_data, aes(x = year_month, y = proportion)) +
  
  geom_line(color = "#4575B4", linewidth = 0.5, alpha = 0.7) +
  

  geom_smooth(
    method = "gam", 
    formula = y ~ s(x, k = 15),
    se = TRUE,
    color = "#D73027",
    fill = "#D73027",
    alpha = 0.15,
    linewidth = 0.8
  ) +
  
  facet_wrap(~ topic, scales = "free_y", ncol = 4) +
  
  scale_x_datetime(
    breaks = date_breaks("3 years"),
    labels = date_format("%Y"),
    expand = c(0.02, 0)
  ) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  
  labs(x = NULL, y = NULL) +
  
  theme_classic(base_size = 10, base_family = "sans") +
  theme(
    strip.text = element_text(
      face = "bold", 
      size = 10,
      color = "black",
      margin = margin(b = 3, t = 3)
    ),
    strip.background = element_rect(
      fill = "gray92",
      color = "black",
      linewidth = 0.4
    ),
    
   
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1, 
      vjust = 1,
      size = 9
    ),
    axis.text.y = element_text(size = 9),
    
   
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black", linewidth = 0.35),
    axis.ticks.length = unit(2, "pt"),
    
    
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
    panel.spacing.x = unit(0.35, "cm"),
    panel.spacing.y = unit(0.35, "cm"),
    
    
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10, "pt")
  )


print(p1)


ggsave(
  "topic_trajectories_4col.pdf",
  plot = p1,
  width = 10,      
  height = 13,     
  units = "in",
  dpi = 600
)
