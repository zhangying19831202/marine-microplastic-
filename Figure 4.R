library(tidyverse)
library(ggplot2)
library(gridExtra)

# Load data
data <- readRDS("./data/pred_mp_09_20.rds")

# Define color settings
line_colors <- c(
  "Arctic Ocean" = "#377EB8",       # Deep blue
  "Atlantic Ocean" = "#E41A1C",     # Deep red
  "Indian Ocean" = "#A6CEE3",       # Soft gray-blue
  "Pacific Ocean" = "#4DAF4A",      # Deep green
  "Southern Ocean" = "#984EA3"      # Deep purple
)

# Calculate mean and standard error for five oceans
oceans_summary <- data %>%
  group_by(Oceans, year) %>%
  summarise(
    mp_mean = mean(mp, na.rm = TRUE),
    mp_se = sd(mp, na.rm = TRUE) / sqrt(n())
  )

# Get year range
year_range <- range(oceans_summary$year)

# Plot a
generate_plot <- function(data, colors, title, legend_pos, y_breaks, y_max) {
  ggplot(data, aes(x = year, y = mp_mean, color = Ocean_Region)) +
    geom_line(size = 1.5) +
    geom_ribbon(aes(ymin = mp_mean - mp_se, ymax = mp_mean + mp_se), alpha = 0.2) +
    labs(title = title, x = "Years", y = "Microplastic Concentrations (pieces/m³)") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 28, color = "black"),
      axis.title = element_text(size = 28, color = "black"),
      plot.title = element_text(size = 36, color = "black", face = "bold"),
      legend.text = element_text(size = 28),
      legend.title = element_text(size = 28),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      axis.ticks = element_line(color = "black"),
      axis.line = element_line(color = "black"),
      axis.ticks.length = unit(-0.5, "cm"),
      legend.position = legend_pos
    ) +
    scale_x_continuous(breaks = seq(2010, year_range[2], by = 2), limits = c(2010, year_range[2])) +
    scale_y_continuous(breaks = seq(0, y_max, by = y_breaks), expand = expansion(mult = c(0, 0.05))) +
    scale_color_manual(values = colors)
}

# Atlantic regions
atlantic_colors <- c("North Atlantic Ocean" = "#66CCAA", "Equatorial Atlantic Ocean" = "#FF9999", "South Atlantic Ocean" = "#99CCFF")
atlantic_regions <- data %>%
  filter(Ocean_Region %in% c("North Atlantic Ocean", "Equatorial Atlantic Ocean", "South Atlantic Ocean")) %>%
  group_by(Ocean_Region, year) %>%
  summarise(mp_mean = mean(mp, na.rm = TRUE), mp_se = sd(mp, na.rm = TRUE) / sqrt(n()))

# Pacific regions
pacific_colors <- c("North Pacific Ocean" = "#66CCAA", "Equatorial Pacific Ocean" = "#FF9999", "South Pacific Ocean" = "#99CCFF")
pacific_regions <- data %>%
  filter(Ocean_Region %in% c("North Pacific Ocean", "Equatorial Pacific Ocean", "South Pacific Ocean")) %>%
  group_by(Ocean_Region, year) %>%
  summarise(mp_mean = mean(mp, na.rm = TRUE), mp_se = sd(mp, na.rm = TRUE) / sqrt(n()))

# Indian regions
indian_colors <- c("North Indian Ocean" = "#66CCAA", "Equatorial Indian Ocean" = "#FF9999", "South Indian Ocean" = "#99CCFF")
indian_regions <- data %>%
  filter(Ocean_Region %in% c("North Indian Ocean", "Equatorial Indian Ocean", "South Indian Ocean")) %>%
  group_by(Ocean_Region, year) %>%
  summarise(mp_mean = mean(mp, na.rm = TRUE), mp_se = sd(mp, na.rm = TRUE) / sqrt(n()))

# Generate plots
plot_a <- generate_plot(oceans_summary, line_colors, "a", c(0.13, 0.8), 1, max(oceans_summary$mp_mean, na.rm = TRUE))
plot_b <- generate_plot(atlantic_regions, atlantic_colors, "b", c(0.19, 0.8), 5, max(atlantic_regions$mp_mean, na.rm = TRUE))
plot_c <- generate_plot(pacific_regions, pacific_colors, "c", c(0.19, 0.8), 1, max(pacific_regions$mp_mean, na.rm = TRUE))
plot_d <- generate_plot(indian_regions, indian_colors, "d", c(0.19, 0.8), 0.25, max(indian_regions$mp_mean, na.rm = TRUE))

# Combine and save the plots
combined_plot <- grid.arrange(plot_a, plot_b, plot_c, plot_d, ncol = 2)
ggsave("./results/Figure 4.png", plot = combined_plot, width = 12, height = 8, dpi = 300)

cat("Combined plot saved successfully!")
