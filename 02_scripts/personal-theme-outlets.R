
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

pre_covid <- as.POSIXct("2020-01-01")
outbreak_end <- as.POSIXct("2020-06-01")
end_date <- as.POSIXct("2025-04-01")

# personal and psychological focus topic
soft_topics <- c(11, 13)

# outlets
focus_accounts <- c("人物", "谷雨实验室", "南方周末", "真实故事计划")

soft_news_data <- as.data.frame(stm_model$theta) %>%
  mutate(
    account = meta$account,
    publish_time = meta$publish_time,
    soft_news_total = rowSums(select(., paste0("V", soft_topics)))
  ) %>%
  filter(
    account %in% focus_accounts,
    publish_time >= as.POSIXct("2013-01-01"),
    publish_time <= end_date
  ) %>%
  mutate(
    account_en = case_when(
      account == "人物" ~ "Renwu Magazine",
      account == "谷雨实验室" ~ "Guyu Lab",
      account == "南方周末" ~ "Southern Weekly",
      account == "真实故事计划" ~ "True Story Project"
    ),
    period = case_when(
      publish_time < pre_covid ~ "Pre-COVID",
      publish_time >= pre_covid & publish_time <= outbreak_end ~ "COVID Outbreak",
      publish_time > outbreak_end ~ "Post-Outbreak"
    ),
    year_month = floor_date(publish_time, "month")
  )



monthly_soft_news <- soft_news_data %>%
  group_by(account_en, year_month) %>%
  summarise(
    soft_news_avg = mean(soft_news_total, na.rm = TRUE),
    .groups = "drop"
  )


period_means <- soft_news_data %>%
  group_by(account_en, period) %>%
  summarise(
    mean_value = mean(soft_news_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    period = factor(period, levels = c("Pre-COVID", "COVID Outbreak", "Post-Outbreak")),
    period_start = case_when(
      period == "Pre-COVID" ~ as.POSIXct("2013-01-01"),
      period == "COVID Outbreak" ~ pre_covid,
      period == "Post-Outbreak" ~ outbreak_end
    ),
    period_end = case_when(
      period == "Pre-COVID" ~ pre_covid,
      period == "COVID Outbreak" ~ outbreak_end,
      period == "Post-Outbreak" ~ end_date
    )
  )


change_summary <- period_means %>%
  tidyr::pivot_wider(
    id_cols = account_en,
    names_from = period,
    values_from = mean_value
  ) %>%
  mutate(
    outbreak_change = `COVID Outbreak` - `Pre-COVID`,
    post_change = `Post-Outbreak` - `Pre-COVID`,
    pct_growth = ((`Post-Outbreak` / `Pre-COVID`) - 1) * 100
  )


print(change_summary)

publication_color <- "#0072B2" 

p_publication <- ggplot() +
  
annotate(
  "rect",
  xmin = as.POSIXct("2013-01-01"), 
  xmax = pre_covid,
  ymin = -Inf, 
  ymax = Inf,
  fill = "gray95", 
  alpha = 0.6
) +
  
  annotate(
    "rect",
    xmin = outbreak_end, 
    xmax = end_date,
    ymin = -Inf, 
    ymax = Inf,
    fill = "gray90", 
    alpha = 0.6
  ) +
  
 
  geom_vline(
    xintercept = as.numeric(pre_covid),
    linetype = "dotted",
    color = "gray40",
    linewidth = 0.6
  ) +
  
  geom_vline(
    xintercept = as.numeric(outbreak_end),
    linetype = "dotted",
    color = "gray40",
    linewidth = 0.6
  ) +
  

annotate(
  "text",
  x = as.POSIXct("2016-06-01"),
  y = Inf,
  label = "Pre-COVID\n(2013-2019)",
  size = 3,
  family = "sans",
  fontface = "plain",
  color = "gray20",
  vjust = 1.5,
  lineheight = 1
) +
  
  annotate(
    "text",
    x = as.POSIXct("2020-03-15"),
    y = Inf,
    label = "Outbreak\n(Jan-Jun\n2020)",
    size = 3,
    family = "sans",
    fontface = "plain",
    color = "gray20",
    vjust = 1.5,
    lineheight = 0.95
  ) +
  
  annotate(
    "text",
    x = as.POSIXct("2022-09-01"),
    y = Inf,
    label = "Post-Outbreak\n(2020-2025)",
    size = 3,
    family = "sans",
    fontface = "plain",
    color = "gray20",
    vjust = 1.5,
    lineheight = 1
  ) +
  
  geom_point(
  data = monthly_soft_news,
  aes(x = year_month, y = soft_news_avg),
  color = publication_color,
  alpha = 0.2,
  size = 1.2
) +
  

geom_segment(
  data = period_means %>% 
    filter(period %in% c("Pre-COVID", "Post-Outbreak")),
  aes(
    x = period_start,
    xend = period_end,
    y = mean_value,
    yend = mean_value
  ),
  color = publication_color,
  linewidth = 2,
  alpha = 0.9
) +
  
geom_smooth(
  data = monthly_soft_news,
  aes(x = year_month, y = soft_news_avg),
  method = "loess",
  span = 0.2,
  se = TRUE,
  color = publication_color,
  fill = publication_color,
  linewidth = 1.2,
  alpha = 0.15
) +
  
 
geom_text(
  data = period_means %>% 
    filter(period == "Pre-COVID") %>%
    mutate(label_x = as.POSIXct("2016-06-01")),
  aes(
    x = label_x,
    y = mean_value,
    label = percent(mean_value, accuracy = 0.1)
  ),
  color = publication_color,
  size = 3.5,
  family = "sans",
  fontface = "bold",
  vjust = -0.8
) +
  
  
  geom_text(
    data = period_means %>% 
      filter(period == "Post-Outbreak") %>%
      mutate(label_x = as.POSIXct("2022-09-01")),
    aes(
      x = label_x,
      y = mean_value,
      label = percent(mean_value, accuracy = 0.1)
    ),
    color = publication_color,
    size = 3.5,
    family = "sans",
    fontface = "bold",
    vjust = -0.8
  ) +
  
geom_segment(
  data = change_summary,
  aes(
    x = as.POSIXct("2024-01-01"),
    xend = as.POSIXct("2024-01-01"),
    y = `Pre-COVID`,
    yend = `Post-Outbreak`
  ),
  color = publication_color,
  arrow = arrow(length = unit(0.15, "inches"), type = "closed", ends = "both"),
  linewidth = 1,
  alpha = 0.8
) +
  
geom_text(
  data = change_summary %>%
    filter(account_en != "Southern Weekly") %>%
    mutate(
      label_y = (`Pre-COVID` + `Post-Outbreak`) / 2,
      change_label = sprintf("%+.1f pp", post_change * 100)  # dispaly pp**
    ),
  aes(
    x = as.POSIXct("2024-08-01"), 
    y = label_y,
    label = change_label
  ),
  color = publication_color,
  size = 3.2,  
  family = "sans",
  fontface = "bold",
  hjust = 0
) +
  

facet_wrap(
  ~ account_en, 
  ncol = 2,
  scales = "free_y",
  labeller = labeller(account_en = c(
    "Renwu Magazine" = "(A) Renwu Magazine",
    "Guyu Lab" = "(B) Guyu Lab",
    "Southern Weekly" = "(C) Southern Weekly",
    "True Story Project" = "(D) True Story Project"
  ))
) +
  
 scale_x_datetime(
  date_breaks = "3 years",
  date_labels = "%Y",
  expand = expansion(mult = c(0.01, 0.08))  
) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.12))  
  ) +
  
labs(
  x = "Year",
  y = "Soft News Proportion"
) +
  
 
theme_classic(base_size = 11, base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "gray88", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    strip.text = element_text(
      size = 10,
      family = "sans",
      face = "bold",
      color = "black",
      margin = margin(t = 4, b = 4)
    ),
    
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(0.12, "cm"),
    axis.text = element_text(size = 9.5, color = "black", family = "sans"),
    axis.title = element_text(size = 11, color = "black", family = "sans", face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    
    plot.margin = margin(12, 12, 12, 12),
    legend.position = "none"
  )


ggsave(
  "Figure_SoftNews_Evolution_4Outlets.pdf",
  plot = p_publication,
  width = 10,
  height = 8,
  units = "in",
  dpi = 600
)