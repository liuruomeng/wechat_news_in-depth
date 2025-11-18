
library(tidyverse)
library(lubridate)
library(scales)
library(RColorBrewer)


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

# Data Loading

moral_data <- read_csv("../01_data/02_processed/2025-09-09_data_with_moral_dimensions_fixed.csv") %>%
  mutate(
    publish_time = ymd_hms(publish_time, quiet = TRUE),
    publish_date = as.Date(publish_time),
    publish_month = floor_date(publish_date, "month")
  ) %>%
  filter(!is.na(publish_time))

# Identify Top 5
moral_cols <- names(moral_dimensions)

dimension_significance <- moral_data %>%
  summarise(across(all_of(moral_cols), list(
    mean = ~mean(.x, na.rm = TRUE),
    nonzero_pct = ~mean(.x > 0, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = "measure", values_to = "value") %>%
  separate(measure, into = c("dimension", "statistic"), sep = "_(?=mean|nonzero)") %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(
    dimension_name = moral_dimensions[dimension],
    significance_score = mean * nonzero_pct
  ) %>%
  arrange(desc(significance_score))

# Select top 5 dimensions
top_5_dimensions <- head(dimension_significance$dimension, 5)
top_5_dimension_names <- moral_dimensions[top_5_dimensions]

cat("Top 5 dimensions:", paste(top_5_dimension_names, collapse = ", "), "\n\n")

# Quarterly data

quarterly_moral <- moral_data %>%
  mutate(quarter = floor_date(publish_month, "quarter")) %>%
  group_by(quarter) %>%
  summarise(
    across(all_of(top_5_dimensions), ~mean(.x, na.rm = TRUE)),
    article_count = n(),
    .groups = 'drop'
  ) %>%
  filter(!is.na(quarter), article_count >= 10) %>%
  pivot_longer(cols = all_of(top_5_dimensions), 
               names_to = "dimension", 
               values_to = "avg_score") %>%
  mutate(dimension_name = moral_dimensions[dimension])

# Plot
p1_final <- ggplot(quarterly_moral, 
                   aes(x = quarter, y = avg_score, 
                       color = dimension_name,
                       linetype = dimension_name)) +
  
  # Vertical reference line for 2018
  geom_vline(xintercept = as.Date("2018-01-01"), 
             linetype = "dashed", color = "#D62728", linewidth = 0.6) +
  
  annotate("text", x = as.Date("2018-01-01"), y = 0.36,
           label = "2018", hjust = -0.15, 
           size = 3,
           color = "#D62728") +
  
  # Smoothed trend lines
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, 
              linewidth = 1.0, span = 0.3) +
  
  # Color scheme
  scale_color_manual(
    values = c("Authority" = "#B2182B", 
               "Care" = "#2166AC",
               "Fairness" = "gray35", 
               "Loyalty" = "gray50", 
               "Sanctity" = "gray65")
  ) +
  
  scale_linetype_manual(
    values = c("Authority" = "solid", 
               "Care" = "solid",
               "Fairness" = "dashed", 
               "Loyalty" = "dotted", 
               "Sanctity" = "dotdash")
  ) +
  
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  scale_y_continuous(
    labels = number_format(accuracy = 0.01),
    breaks = seq(0, 0.4, 0.05)
  ) +
  
  labs(
    x = "Time (Year)",
    y = "Average Moral Score",
    color = "Moral Dimensions",
    linetype = "Moral Dimensions"
  ) +
  
  theme_classic(base_size = 9) +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5,
                              margin = margin(b = 8)),
    axis.title.x = element_text(size = 9, face = "bold", margin = margin(t = 6)),
    axis.title.y = element_text(size = 9, face = "bold", margin = margin(r = 6)),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.width = unit(1.8, "lines"),
    legend.key.height = unit(0.8, "lines"),
    legend.spacing.y = unit(0.1, "lines"),
    legend.position.inside = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", color = "gray50", linewidth = 0.25),
    legend.margin = margin(4, 4, 4, 4),
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.3),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.25),
    panel.grid.major.x = element_blank()
  )

print(p1_final)
