library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

p_corrected <- temporal_data %>%
  mutate(
    topic_label = case_when(
      topic == 6 ~ "Topic 6: Crime, Law, and the Justice System",
      topic == 11 ~ "Topic 11: Modern Psychology, Relationships, and Mental Health",
      topic == 14 ~ "Topic 14: Chinese Politics, Government, and History"
    )
  ) %>%
  ggplot(aes(x = date, y = proportion, color = factor(topic))) +
  geom_line(size = 1.8, alpha = 0.85) +
  geom_point(size = 1.5, alpha = 0.8) +
  geom_vline(xintercept = as.Date("2017-01-01"), 
             linetype = "dashed", color = "#E74C3C", size = 1.2, alpha = 0.9) +
  annotate("text", x = as.Date("2017-02-15"), y = 0.145, 
           label = "Structural Break", size = 3.2, hjust = 0, 
           color = "#E74C3C", fontface = "bold") +
  geom_vline(xintercept = as.Date("2020-01-23"), 
             linetype = "dashed", color = "#8E44AD", size = 1.2, alpha = 0.9) +
  annotate("text", x = as.Date("2020-03-01"), y = 0.145, 
           label = "Wuhan Lockdown", size = 3.2, hjust = 0, 
           color = "#8E44AD", fontface = "bold") +
  scale_color_manual(
    values = c("6" = "#C0392B", "11" = "#2980B9", "14" = "#27AE60"),
    labels = c("6" = "Crime, Law, and Justice System",
               "11" = "Modern Psychology and Mental Health",
               "14" = "Chinese Politics and History"),
    name = "Topic Categories"
  ) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1),
    breaks = seq(0, 0.16, 0.02),
    limits = c(0, 0.16),
    expand = c(0, 0)
  ) +
  
  scale_x_date(
    date_breaks = "2 years", 
    date_labels = "%Y",
    limits = c(as.Date("2012-06-01"), as.Date("2024-06-01")),
    expand = c(0.01, 0)
  ) +
  
  labs(
    title = "Temporal Evolution of Topic Prevalence in Chinese Media Discourse",
    subtitle = "A longitudinal analysis of thematic patterns (2012-2024)",
    x = "Year",
    y = "Topic Proportion (%)",
    caption = "Note: Shaded regions indicate distinct analytical periods. Dashed lines mark key temporal breakpoints."
  ) +
  
  theme_classic() +
  theme(
  
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold", 
                              color = "#2C3E50", margin = margin(b = 8)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic",
                                 color = "#34495E", margin = margin(b = 20)),
    plot.caption = element_text(size = 10, hjust = 0, color = "#7F8C8D", 
                                margin = margin(t = 15)),
    
  
    axis.title = element_text(size = 13, face = "bold", color = "#2C3E50"),
    axis.text = element_text(size = 11, color = "#34495E"),
    axis.line = element_line(color = "#BDC3C7", size = 0.8),
    axis.ticks = element_line(color = "#BDC3C7", size = 0.6),

    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold", color = "#2C3E50"),
    legend.text = element_text(size = 10, color = "#34495E"),
    legend.margin = margin(t = 20),
    legend.key.width = unit(1.5, "cm"),
    

    panel.grid.major.y = element_line(color = "#ECF0F1", size = 0.5),
    panel.grid.minor.y = element_line(color = "#F8F9FA", size = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    

    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(25, 25, 25, 25)
  ) +
  

  guides(color = guide_legend(
    override.aes = list(size = 4, alpha = 1),
    nrow = 1,
    title.position = "top",
    title.hjust = 0.5
  ))

print(p_corrected)
