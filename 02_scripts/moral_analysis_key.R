
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(scales)
library(RColorBrewer)
library(viridis)

moral_dimensions <- c(
  "altr_num" = "Altruism",
  "auth_num" = "Authority", 
  "care_num" = "Care",
  "dili_num" = "Diligence",
  "fair_num" = "Fairness",
  "general_num" = "General Morality",
  "libe_num" = "Liberty",
  "loya_num" = "Loyalty",
  "mode_num" = "Moderation",
  "resi_num" = "Resilience",
  "sanc_num" = "Sanctity",
  "wast_num" = "Frugality"
)

colors_palette <- c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A")


# moral_data <- read_csv("../01_data/02_processed/2025-09-09_data_with_moral_dimensions_fixed.csv")


moral_data <- moral_data %>%
  mutate(
    publish_time = as.POSIXct(publish_time, format = "%Y-%m-%d %H:%M:%S"),
    publish_date = as.Date(publish_time),
    publish_month = floor_date(publish_date, "month")
  ) %>%
  filter(!is.na(publish_time))


moral_cols <- names(moral_dimensions)

dimension_significance <- moral_data %>%
  summarise(across(all_of(moral_cols), list(
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE),
    nonzero_pct = ~mean(.x > 0, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = "measure", values_to = "value") %>%
  separate(measure, into = c("dimension", "statistic"), sep = "_(?=mean|sd|max|nonzero)") %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(
    dimension_name = moral_dimensions[dimension],
    significance_score = mean * nonzero_pct
  ) %>%
  arrange(desc(significance_score))


for(i in 1:5) {
  dim_data <- dimension_significance[i, ]
  cat(sprintf("%d. %s: Average Score=%.4f, Frequency=%.1f%%, Prevalence=%.6f\n", 
              i, dim_data$dimension_name, dim_data$mean, 
              dim_data$nonzero_pct*100, dim_data$significance_score))
}

### Top 5 Dimensions were selected for illustration
top_5_dimensions <- head(dimension_significance$dimension, 5)
top_5_dimension_names <- moral_dimensions[top_5_dimensions]


monthly_moral_top5 <- moral_data %>%
  group_by(publish_month) %>%
  summarise(
    across(all_of(top_5_dimensions), ~mean(.x, na.rm = TRUE)),
    article_count = n(),
    .groups = 'drop'
  ) %>%
  filter(!is.na(publish_month), article_count >= 10) %>%
  pivot_longer(cols = all_of(top_5_dimensions), 
               names_to = "dimension", 
               values_to = "avg_score") %>%
  mutate(dimension_name = moral_dimensions[dimension])

p1_simple <- ggplot(monthly_moral_top5, 
                    aes(x = publish_month, y = avg_score, color = dimension_name)) +
  geom_line(linewidth = 1.0, alpha = 0.85) +
  geom_point(size = 1.8, alpha = 0.75) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.15, linewidth = 0.8) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  scale_color_manual(values = colors_to_use) +
  labs(
    title = "Temporal Trends of Five Moral Dimensions",
    x = "Time (Month)",
    y = "Average Moral Score",
    color = "Moral Dimensions"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  )

print(p1_simple)
