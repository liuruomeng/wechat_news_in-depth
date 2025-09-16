
library(tidyverse)
library(ggpubr)
library(broom) 


smoothed_gap_data <- absolute_gap_data %>%
  group_by(target_word_en) %>%

  do({

    current_data <- .
    

    model <- loess(absolute_gap ~ year, data = current_data, span = 0.8)
    predictions <- predict(model, se = TRUE)
    tibble(
      year = current_data$year,           
      absolute_gap_smooth = predictions$fit, 
      se = predictions$se.fit              
    )
  }) %>%
  mutate(
    ci_lower = absolute_gap_smooth - 1.96 * se,
    ci_upper = absolute_gap_smooth + 1.96 * se
  ) %>%
  ungroup()

--
primary_range <- range(c(yearly_similarities_raw$mean_personal, yearly_similarities_raw$mean_macro), na.rm = TRUE)
secondary_range_abs <- range(c(smoothed_gap_data$ci_lower, smoothed_gap_data$ci_upper), na.rm = TRUE, finite = TRUE)
scale_factor_abs <- 0.035 / (secondary_range_abs[2] - secondary_range_abs[1])
offset_abs <- -0.06 - secondary_range_abs[1] * scale_factor_abs

create_trend_plot <- function(word_to_plot, main_data, smooth_data) {
  
  plot_data_main <- main_data %>% filter(target_word_en == word_to_plot)
  plot_data_smooth <- smooth_data %>% filter(target_word_en == word_to_plot)
  
  ggplot() +
    geom_line(data = plot_data_main, aes(x = year, y = mean_personal, color = "Personal Narrative (Score)"), linewidth = 1.1) +
    geom_line(data = plot_data_main, aes(x = year, y = mean_macro, color = "Macro Narrative (Score)"), linewidth = 1.1) +
    geom_ribbon(
      data = plot_data_smooth,
      aes(
        x = year,
        ymin = ci_lower * scale_factor_abs + offset_abs,
        ymax = ci_upper * scale_factor_abs + offset_abs,
        fill = "Gap Strength Trend (95% CI)"
      ),
      alpha = 0.25
    ) +
    geom_line(
      data = plot_data_smooth,
      aes(x = year, y = absolute_gap_smooth * scale_factor_abs + offset_abs, color = "Gap Strength Trend"),
      linewidth = 1.3
    ) +
    scale_y_continuous(
      name = "Similarity Score",
      limits = c(-0.06, 0.02),
      sec.axis = sec_axis(
        trans = ~ (. - offset_abs) / scale_factor_abs,
        name = "Narrative Gap"
      )
    ) +
    scale_color_manual(
      name = "Data Series",
      values = c(
        "Personal Narrative (Score)" = "#d7191c",
        "Macro Narrative (Score)" = "#005a92",
        "Gap Strength Trend" = "black"
      )
    ) +
    scale_fill_manual(
      name = "Data Series",
      values = c(
        "Gap Strength Trend (95% CI)" = "black"
      )
    ) +
    guides(
      color = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid"))),
      fill = guide_legend(title="Data Series")
    ) +
    labs(
      title = word_to_plot,
      x = NULL,
      subtitle = "R)."
    ) +
    theme_pubr() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40"),
      legend.position = "bottom",
      legend.title = element_text(face="bold"),
      axis.title.y.left = element_text(face="bold"),
      axis.title.y.right = element_text(face="bold", color = "black"),
      axis.text.y.right = element_text(color = "black")
    )
}

p_understanding_trend <- create_trend_plot("Understanding", yearly_similarities_raw, smoothed_gap_data)
p_truth_trend <- create_trend_plot("Truth", yearly_similarities_raw, smoothed_gap_data)
p_complexity_trend <- create_trend_plot("Complexity", yearly_similarities_raw, smoothed_gap_data)
p_justice_trend <- create_trend_plot("Justice", yearly_similarities_raw, smoothed_gap_data)

final_plot_trend <- ggarrange(
  p_understanding_trend, p_truth_trend, p_complexity_trend, p_justice_trend,
  ncol = 2, nrow = 2, align = "hv", common.legend = TRUE, legend = "bottom"
)

annotated_plot_trend <- annotate_figure(
  final_plot_trend,
  top = text_grob("Trend Analysis: Narrative Scores and Their Gap Strength", face = "bold", size = 22),
  bottom = text_grob("Year", size = 14)
)

print(annotated_plot_trend)

