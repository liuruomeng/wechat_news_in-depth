# Ensure necessary libraries are loaded
library(tidyverse)
library(lubridate)

# ---  Load and Prepare Your Data ---
time_data <- read_csv("../01_data/02_processed/2025-09-09_enhanced_content_data.csv") %>%
  select(title, account, publish_time, content, first_person_pronouns_count, quotations_count)
time_data <- time_data %>%
  mutate(publish_time = as.POSIXct(publish_time, format = "%Y-%m-%d %H:%M:%S"))

# --- Aggregate the Data by Month (for Pronouns Only) ---

monthly_summary_pronouns <- time_data %>%
  group_by(
    month = floor_date(publish_time, "month")
  ) %>%
  summarise(
    average_count = mean(first_person_pronouns_count, na.rm = TRUE),
    .groups = "drop" # Ungroup the data frame
  )

# ---  Plot the Aggregated Time Trend for First-person Pronouns ---
ggplot(data = monthly_summary_pronouns, aes(x = month, y = average_count)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "steelblue", size = 2.5) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Monthly Average Trend of First-person Pronouns",
    subtitle = "Data aggregated by month to show long-term patterns (2013-2025)",
    x = "Month",
    y = "Average Count per Month"
  ) +
  
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, color = "gray30", hjust = 0.5),
    axis.title = element_text(size = 12)
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
