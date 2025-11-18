library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(tidyr)
library(zoo)

color_palette <- list(
  primary = "#2C3E50",
  secondary = "#34495E",
  accent = "#3498DB",
  topic_6 = "#C0392B",
  topic_11 = "#2980B9",
  topic_14 = "#27AE60",
  trend = "#E74C3C",
  mean = "#7F8C8D",
  ci_fill = "#3498DB",
  text_main = "#2C3E50",
  text_secondary = "#566573",
  text_light = "#7B8D93",
  strip_bg = "#ECF0F1",
  grid = "#F8F9FA"
)

row_sums <- rowSums(stm_model$theta)
if(any(abs(row_sums - 1) > 0.01)) {
  theta_normalized <- stm_model$theta / row_sums
} else {
  theta_normalized <- stm_model$theta
}

base_date <- as.Date("2013-01-20")

doc_data <- data.frame(
  days = meta$days,
  account = meta$account,
  theta_normalized,
  doc_id = 1:nrow(theta_normalized)
)

doc_data$date <- base_date + doc_data$days
doc_data$year <- year(doc_data$date)
doc_data$month <- month(doc_data$date)
doc_data$year_month <- format(doc_data$date, "%Y-%m")


selected_topics <- c(6, 11, 14)
topic_cols <- paste0("X", selected_topics)
temporal_data_wide <- doc_data %>%
  select(date, year, month, year_month, all_of(topic_cols))

colnames(temporal_data_wide)[5:7] <- paste0("Topic_", selected_topics)

temporal_data <- temporal_data_wide %>%
  pivot_longer(
    cols = starts_with("Topic_"),
    names_to = "topic",
    values_to = "proportion"
  ) %>%
  mutate(topic = as.numeric(gsub("Topic_", "", topic))) %>%
  group_by(year_month, topic) %>%
  summarise(
    date = first(date),
    proportion = mean(proportion),
    .groups = "drop"
  ) %>%
  arrange(topic, date)

temporal_data$date <- as.Date(paste0(temporal_data$year_month, "-01"))

temporal_data <- temporal_data %>%
  mutate(
    topic_label = case_when(
      topic == 6 ~ "Crime, Law & Justice System",
      topic == 11 ~ "Psychology & Mental Health",
      topic == 14 ~ "Politics & History"
    ),
    topic_label = factor(topic_label, levels = c(
      "Crime, Law & Justice System",
      "Psychology & Mental Health",
      "Politics & History"
    )),
    topic_color = case_when(
      topic == 6 ~ color_palette$topic_6,
      topic == 11 ~ color_palette$topic_11,
      topic == 14 ~ color_palette$topic_14
    )
  )

panel_a <- temporal_data %>%
  ggplot(aes(x = date, y = proportion)) +
  
  geom_ribbon(
    aes(ymin = 0, ymax = proportion, fill = factor(topic)),
    alpha = 0.15
  ) +
  
  geom_smooth(
    aes(color = factor(topic)),
    method = "loess",
    se = TRUE,
    linewidth = 0.8,
    span = 0.25,
    alpha = 0.2
  ) +
  
  geom_line(
    aes(color = factor(topic)),
    linewidth = 0.4, 
    alpha = 0.5
  ) +
  
  scale_color_manual(
    values = c("6" = color_palette$topic_6, 
               "11" = color_palette$topic_11, 
               "14" = color_palette$topic_14),
    guide = "none"
  ) +
  
  scale_fill_manual(
    values = c("6" = color_palette$topic_6, 
               "11" = color_palette$topic_11, 
               "14" = color_palette$topic_14),
    guide = "none"
  ) +
  
  facet_wrap(
    ~ topic_label, 
    ncol = 1,
    scales = "free_y",
    strip.position = "top"
  ) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = pretty_breaks(n = 4),
    expand = expansion(mult = c(0, 0.08))
  ) +
  
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y",
    limits = c(as.Date("2013-01-01"), as.Date("2025-01-01")),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  labs(

    x = "Year",
    y = "Topic Proportion"
  ) +
  
  theme_classic(base_size = 9, base_family = "sans") +
  theme(
    plot.title = element_text(
      size = 10, 
      hjust = 0, 
      face = "bold", 
      color = color_palette$text_main,
      margin = margin(b = 6)
    ),
    
    strip.background = element_rect(
      fill = color_palette$strip_bg, 
      color = "gray70",
      linewidth = 0.4
    ),
    strip.text = element_text(
      size = 8, 
      face = "bold", 
      color = color_palette$text_main,
      margin = margin(3, 3, 3, 3)
    ),
    
    axis.title.x = element_text(
      size = 9, 
      face = "bold", 
      color = color_palette$text_main,
      margin = margin(t = 6)
    ),
    axis.title.y = element_text(
      size = 9, 
      face = "bold", 
      color = color_palette$text_main,
      margin = margin(r = 6)
    ),
    
    axis.text.x = element_text(
      size = 8, 
      color = color_palette$text_secondary
    ),
    axis.text.y = element_text(
      size = 8, 
      color = color_palette$text_secondary
    ),
    
    axis.line = element_line(
      color = "gray60", 
      linewidth = 0.4
    ),
    axis.ticks = element_line(
      color = "gray60", 
      linewidth = 0.3
    ),
    axis.ticks.length = unit(2, "pt"),
    
    panel.spacing.y = unit(0.4, "cm"),
    panel.grid.major.y = element_line(
      color = color_palette$grid, 
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    plot.margin = margin(8, 8, 8, 8)
  )



personal_topics <- c(11, 13, 15)
personal_cols <- paste0("X", personal_topics)
doc_data$personal_stories_prop <- rowSums(
  doc_data[, personal_cols, drop = FALSE]
)

monthly_trend <- doc_data %>%
  group_by(year_month) %>%
  summarise(
    year = first(year),
    month = first(month),
    avg_personal_prop = mean(personal_stories_prop),
    sd_personal_prop = sd(personal_stories_prop),
    doc_count = n(),
    .groups = "drop"
  ) %>%
  arrange(year, month)

monthly_trend$date <- as.Date(paste0(monthly_trend$year_month, "-01"))

monthly_trend_smooth <- monthly_trend %>%
  arrange(date) %>%
  mutate(
    avg_smooth = rollmean(avg_personal_prop, k = 3, fill = NA, align = "center"),
    sd_smooth = rollapply(avg_personal_prop, width = 3, FUN = sd, fill = NA, align = "center"),
    se = sd_personal_prop / sqrt(doc_count)
  )

panel_b <- monthly_trend_smooth %>%
  filter(!is.na(avg_smooth)) %>%
  ggplot(aes(x = date, y = avg_smooth)) +
  
  geom_ribbon(
    aes(ymin = pmax(0, avg_smooth - 1.96 * se),
        ymax = avg_smooth + 1.96 * se),
    fill = color_palette$ci_fill,
    alpha = 0.25
  ) +
  
  geom_ribbon(
    aes(ymin = 0, ymax = avg_smooth),
    fill = color_palette$ci_fill,
    alpha = 0.1
  ) +
  
  geom_line(
    data = monthly_trend,
    aes(x = date, y = avg_personal_prop),
    color = "gray70",
    linewidth = 0.4,
    alpha = 0.4
  ) +
  
  geom_line(
    color = color_palette$primary, 
    linewidth = 1.0, 
    alpha = 0.9
  ) +
  
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = color_palette$trend,
    linewidth = 0.9,
    linetype = "dashed",
    span = 0.3
  ) +
  
  geom_hline(
    yintercept = mean(monthly_trend$avg_personal_prop),
    color = color_palette$mean,
    linetype = "dotted",
    linewidth = 0.6,
    alpha = 0.7
  ) +
  
  geom_point(
    data = monthly_trend_smooth %>% 
      filter(!is.na(avg_smooth)) %>%
      slice_max(avg_smooth, n = 1),
    aes(x = date, y = avg_smooth),
    color = color_palette$trend,
    size = 2,
    alpha = 0.9
  ) +
  
  geom_text(
    data = monthly_trend_smooth %>% 
      filter(!is.na(avg_smooth)) %>%
      slice_max(avg_smooth, n = 1),
    aes(x = date, y = avg_smooth,
        label = paste0("Peak: ", round(avg_smooth * 100, 1), "%\n",
                       format(date, "%Y-%m"))),
    vjust = -0.8,
    hjust = 0.5,
    size = 2.5,
    color = color_palette$trend,
    fontface = "bold"
  ) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 0.35, 0.05),
    limits = c(0, 0.35),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y",
    limits = c(as.Date("2013-01-01"), as.Date("2025-01-01")),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  labs(
    subtitle = "3-month moving average (Topics 11, 13, 15)",
    x = "Year",
    y = "Proportion (smoothed)"
  ) +
  
  theme_classic(base_size = 9, base_family = "sans") +
  theme(
    plot.title = element_text(
      size = 10, 
      hjust = 0, 
      face = "bold", 
      color = color_palette$text_main,
      margin = margin(b = 2)
    ),
    
    plot.subtitle = element_text(
      size = 7, 
      hjust = 0, 
      color = color_palette$text_secondary,
      margin = margin(b = 6)
    ),
    
    axis.title.x = element_text(
      size = 9, 
      face = "bold", 
      color = color_palette$text_main,
      margin = margin(t = 6)
    ),
    axis.title.y = element_text(
      size = 9, 
      face = "bold", 
      color = color_palette$text_main,
      margin = margin(r = 6)
    ),
    
    axis.text.x = element_text(
      size = 8, 
      color = color_palette$text_secondary
    ),
    axis.text.y = element_text(
      size = 8, 
      color = color_palette$text_secondary
    ),
    
    axis.line = element_line(
      color = "gray60", 
      linewidth = 0.4
    ),
    axis.ticks = element_line(
      color = "gray60", 
      linewidth = 0.3
    ),
    axis.ticks.length = unit(2, "pt"),
    
    panel.grid.major.y = element_line(
      color = color_palette$grid, 
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    plot.margin = margin(8, 8, 8, 8)
  )


ggsave(
  filename = "fig_panel_a_topic_trajectories.pdf",
  plot = panel_a,
  width = 3.5,
  height = 4.5,
  units = "in"
)


ggsave(
  filename = "fig_panel_b_personal_narratives.pdf",
  plot = panel_b,
  width = 3.5,
  height = 4.5,
  units = "in"
)

