library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)


cat("=== DIAGNOSIS AND DATA CORRECTION ===\n")


row_sums <- rowSums(stm_model$theta)
cat("Row sums range:", sprintf("%.3f - %.3f", min(row_sums), max(row_sums)), "\n")

if(any(abs(row_sums - 1) > 0.01)) {
  cat("Normalizing theta matrix...\n")
  theta_normalized <- stm_model$theta / row_sums
} else {
  theta_normalized <- stm_model$theta
}


topic_means <- colMeans(theta_normalized)
cat("Target topics average proportions:\n")
for(topic in c(11, 13, 15)) {
  cat(sprintf("Topic %d: %.3f (%.2f%%)\n", topic, topic_means[topic], topic_means[topic] * 100))
}


extract_monthly_personal_stories <- function(topics_to_extract, meta_data, theta_matrix) {
  
  cat("\n=== MONTHLY AGGREGATION ===\n")
  

  personal_story_proportion <- rowSums(theta_matrix[, topics_to_extract, drop = FALSE])
  

  doc_data <- data.frame(
    days = meta_data$days,
    account = meta_data$account,
    personal_stories_prop = personal_story_proportion,
    doc_id = 1:length(personal_story_proportion)
  )
  
 
  base_date <- as.Date("2013-01-20") 
  doc_data$date <- base_date + doc_data$days
  doc_data$year_month <- format(doc_data$date, "%Y-%m")
  doc_data$year <- year(doc_data$date)
  doc_data$month <- month(doc_data$date)
  

  monthly_trend <- doc_data %>%
    group_by(year_month, year, month) %>%
    summarise(
      avg_personal_prop = mean(personal_stories_prop),
      median_personal_prop = median(personal_stories_prop),
      min_personal_prop = min(personal_stories_prop),
      max_personal_prop = max(personal_stories_prop),
      sd_personal_prop = sd(personal_stories_prop),
      doc_count = n(),
      .groups = "drop"
    ) %>%
    arrange(year, month)
  

  monthly_trend$date <- as.Date(paste(monthly_trend$year_month, "-01", sep = ""))
  
  cat("Monthly data summary:\n")
  cat("Time points:", nrow(monthly_trend), "\n")
  cat("Date range:", as.character(min(monthly_trend$date)), "to", as.character(max(monthly_trend$date)), "\n")
  cat("Personal stories proportion range:", 
      sprintf("%.2f%% - %.2f%%", 
              min(monthly_trend$avg_personal_prop) * 100,
              max(monthly_trend$avg_personal_prop) * 100), "\n")
  cat("Average documents per month:", round(mean(monthly_trend$doc_count)), "\n")
  
  return(list(
    monthly_trend = monthly_trend,
    document_level = doc_data
  ))
}


personal_topics <- c(11, 13, 15)
monthly_data <- extract_monthly_personal_stories(personal_topics, meta_data, theta_normalized)
monthly_trend <- monthly_data$monthly_trend


p_monthly_trend <- monthly_trend %>%
  ggplot(aes(x = date, y = avg_personal_prop)) +

  geom_ribbon(aes(ymin = pmax(0, avg_personal_prop - sd_personal_prop), 
                  ymax = avg_personal_prop + sd_personal_prop),
              fill = "#3498DB", alpha = 0.2) +
  

  geom_ribbon(aes(ymin = 0, ymax = avg_personal_prop),
              fill = "#3498DB", alpha = 0.1) +
  

  geom_line(color = "#2C3E50", size = 1.2, alpha = 0.9) +
  

  geom_point(color = "#34495E", size = 1.2, alpha = 0.7) +
  
  geom_smooth(method = "loess", se = TRUE,
              color = "#E74C3C", fill = "#E74C3C", alpha = 0.2,
              size = 1, linetype = "dashed", span = 0.3) +

  geom_hline(yintercept = mean(monthly_trend$avg_personal_prop),
             color = "#7F8C8D", linetype = "dotted", size = 1, alpha = 0.8) +
  
  
  

  geom_point(data = monthly_trend %>% slice_max(avg_personal_prop, n = 1),
             aes(x = date, y = avg_personal_prop),
             color = "#E74C3C", size = 3, alpha = 0.9) +
  
  geom_text(data = monthly_trend %>% slice_max(avg_personal_prop, n = 1),
            aes(x = date, y = avg_personal_prop,
                label = paste0("Peak: ", round(avg_personal_prop * 100, 1), "%\n",
                               format(date, "%Y-%m"))),
            vjust = -0.8, hjust = 0.5, size = 3.5, 
            color = "#E74C3C", fontface = "bold") +
  

  scale_y_continuous(
    labels = percent_format(accuracy = 0.1),
    breaks = pretty_breaks(n = 8),
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  

  labs(
    title = "Monthly Evolution of Personal Story Topics in Chinese Digital Media",
    subtitle = paste("Combined proportion of Topics", paste(personal_topics, collapse = ", "), 
                     "- Monthly aggregated data (2013-2024)"), 
    x = "Year",
    y = "Monthly Average Proportion",
    caption = "Note: Shaded area represents ±1 standard deviation. Dashed line shows smoothed trend."
  ) +
  
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(size = 14, hjust = 0, face = "bold", 
                              color = "#2C3E50", margin = margin(b = 8)),
    plot.subtitle = element_text(size = 10, hjust = 0, color = "#566573", 
                                 margin = margin(b = 15)),
    plot.caption = element_text(size = 9, hjust = 0, color = "#7B8D93", 
                                margin = margin(t = 12)),
    
    axis.title = element_text(size = 11, face = "bold", color = "#34495E"),
    axis.text = element_text(size = 9, color = "#2C3E50"),
    axis.line = element_line(color = "#34495E", size = 0.5),
    axis.ticks = element_line(color = "#34495E", size = 0.4),
    
    panel.grid.major = element_line(color = "#ECF0F1", size = 0.3),
    panel.grid.minor = element_blank(),
    
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  )

print(p_monthly_trend)


monthly_analysis <- monthly_trend %>%
  summarise(
    start_date = first(date),
    end_date = last(date),
    start_prop = first(avg_personal_prop) * 100,
    end_prop = last(avg_personal_prop) * 100,
    peak_prop = max(avg_personal_prop) * 100,
    peak_date = date[which.max(avg_personal_prop)],
    mean_prop = mean(avg_personal_prop) * 100,
    median_prop = median(avg_personal_prop) * 100,
    total_growth = ((last(avg_personal_prop) / first(avg_personal_prop)) - 1) * 100,
    monthly_volatility = sd(avg_personal_prop) * 100
  )

cat("\n=== MONTHLY ANALYSIS RESULTS ===\n")
cat("Analysis Period:", format(monthly_analysis$start_date, "%Y-%m"), "to", 
    format(monthly_analysis$end_date, "%Y-%m"), "\n")
cat("Total months:", nrow(monthly_trend), "\n\n")

cat("PROPORTION STATISTICS:\n")
cat(sprintf("• Starting: %.2f%%\n", monthly_analysis$start_prop))
cat(sprintf("• Ending: %.2f%%\n", monthly_analysis$end_prop))
cat(sprintf("• Peak: %.2f%% (%s)\n", monthly_analysis$peak_prop, 
            format(monthly_analysis$peak_date, "%Y-%m")))
cat(sprintf("• Mean: %.2f%%\n", monthly_analysis$mean_prop))
cat(sprintf("• Median: %.2f%%\n", monthly_analysis$median_prop))
cat(sprintf("• Total growth: %+.1f%%\n", monthly_analysis$total_growth))
cat(sprintf("• Monthly volatility (SD): %.2f%%\n", monthly_analysis$monthly_volatility))


yearly_summary <- monthly_trend %>%
  group_by(year) %>%
  summarise(
    avg_prop = mean(avg_personal_prop) * 100,
    max_prop = max(avg_personal_prop) * 100,
    min_prop = min(avg_personal_prop) * 100,
    total_docs = sum(doc_count),
    months = n(),
    .groups = "drop"
  )

cat("\nYEARLY SUMMARY:\n")
for(i in 1:nrow(yearly_summary)) {
  cat(sprintf("%d: %.2f%% avg (%.2f%%-%.2f%%, %d docs, %d months)\n",
              yearly_summary$year[i],
              yearly_summary$avg_prop[i],
              yearly_summary$min_prop[i],
              yearly_summary$max_prop[i],
              yearly_summary$total_docs[i],
              yearly_summary$months[i]))
}