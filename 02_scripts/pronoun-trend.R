
library(tidyverse)
library(lubridate)

time_data <- read_csv("../01_data/02_processed/2025-09-09_enhanced_content_data.csv") %>%
  select(title, account, publish_time, content, first_person_pronouns_count, quotations_count)
time_data <- time_data %>%
  mutate(publish_time = as.POSIXct(publish_time, format = "%Y-%m-%d %H:%M:%S"))

monthly_summary_pronouns <- time_data %>%
  group_by(
    month = floor_date(publish_time, "month")
  ) %>%
  summarise(
    average_count = mean(first_person_pronouns_count, na.rm = TRUE),
    .groups = "drop" # Ungroup the data frame
  )


first_full_year <- 2014
last_full_year <- 2024


start_avg <- monthly_summary_pronouns %>%
  filter(year(month) == first_full_year) %>%
  summarise(val = mean(average_count)) %>%
  pull(val)


end_avg <- monthly_summary_pronouns %>%
  filter(year(month) == last_full_year) %>%
  summarise(val = mean(average_count)) %>%
  pull(val)

percentage_increase <- (end_avg - start_avg) / start_avg * 100


summary_text <- paste0(
  "Trend Analysis (", first_full_year, " vs ", last_full_year, "):\n\n",
  "Avg. in ", first_full_year, ": ", round(start_avg, 1), "\n",
  "Avg. in ", last_full_year, ": ", round(end_avg, 1), "\n\n",
  "Total Increase: +", round(percentage_increase, 0), "%"
)

print(summary_text)

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

monthly_summary_pronouns$month <- as.Date(monthly_summary_pronouns$month)

p_area_smooth <- ggplot(monthly_summary_pronouns, 
                        aes(x = month, y = average_count)) +
  

  geom_area(fill = "#2166AC", alpha = 0.3) +
  

  geom_line(color = "#2166AC", linewidth = 0.6, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, 
              color = "#D62728", linewidth = 1.2, 
              alpha = 0.15, span = 0.25) +
  
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  scale_y_continuous(
    labels = number_format(accuracy = 1),
    breaks = seq(0, 90, 10),
    expand = c(0, 0)
  ) +
  
  labs(x = "Time (Year)", y = "Average Count per Month") +
  
  theme_classic(base_size = 9) +
  theme(
    axis.title = element_text(size = 9, face = "bold"),
    axis.text = element_text(size = 8),
    axis.line = element_line(color = "black", linewidth = 0.4),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.25),
    panel.grid.major.x = element_blank()
  )

print(p_area_smooth)



