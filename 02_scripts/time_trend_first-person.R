library(tidyverse)
library(lubridate)

#time_data <- read_csv("../01_data/02_processed/2025-09-09_enhanced_content_data.csv") %>%
#  select(title, account, publish_time, content, first_person_pronouns_count, quotations_count)

time_data <- time_data %>%
  mutate(publish_time = as.POSIXct(publish_time, format = "%Y-%m-%d %H:%M:%S"))

monthly_summary_pronouns <- time_data %>%
  group_by(
    month = floor_date(publish_time, "month")
  ) %>%
  summarise(
    average_count = mean(first_person_pronouns_count, na.rm = TRUE),
    .groups = "drop" 
  )


ggplot(data = monthly_summary_pronouns, aes(x = month, y = average_count)) +
  geom_line(color = "steelblue", linewidth = 1.0, alpha = 0.8) +
  geom_point(color = "steelblue", size = 2.0, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", linetype = "dashed", linewidth = 1.0) +
  

  theme_minimal(base_size = 14) +
  labs(
    title = "Monthly Average Trend of First-person Pronouns",
    subtitle = "The overall trend shows a significant increase over the decade",
    x = "Month",
    y = "Average Count per Month"
  ) +
  
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 13, color = "gray30", hjust = 0.5),
    axis.title = element_text(size = 12)
  )

