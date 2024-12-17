library(tidyverse)
library(ggplot2)
library(sf)
library(terra)
library(dplyr)

# Load microplastic annual average data
data <- readRDS("./data/pred_mp_09_20.rds")

# Load and transform continent boundary data
m_continent_r <- read_sf("./data/world_continent_bound/World_Continents.shp") %>%
  st_transform(crs = '+proj=robin')

# Define color scheme and breaks
breaks <- c(-Inf, 0.01, 0.1, 1, 10, 100, 1000, Inf)
labels <- c(
  "< 0.01", 
  "0.01", 
  "0.1", 
  "1", 
  "10", 
  "100", 
  "1000", 
  "> 1000"
)
colors <- c("#e0f3f8", "#a6cee3", "#7fc97f", "#ffd700", "#fdae61", "#f46d43", "#e31a1c")

# Define years for plotting
years_to_plot <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

# Function to create and save microplastic maps
plot_microplastic_map <- function(year) {
  # Filter data
  data_year <- data %>% filter(year == !!year)
  
  # Convert to SpatRaster object
  data_rast <- rast(data_year[, c("lon", "lat", "mp")], crs = 'epsg:4326')
  data_rast_proj <- project(data_rast, "+proj=robin")
  rast_df <- as.data.frame(data_rast_proj, xy = TRUE)
  colnames(rast_df) <- c("x", "y", "mp")
  rast_df <- rast_df %>% filter(!is.na(mp))
  
  # Open graphics device
  png(
    filename = paste0("./results/global_Mp_", year, ".png"),
    width = 2400, height = 1400, res = 300
  )
  
  # Plot map
  par(mar = c(5, 1, 2, 1), oma = c(0, 0, 0, 0), xpd = TRUE)
  plot(st_geometry(m_continent_r), col = "#d0f0fd", border = NA, reset = FALSE,
       xlim = c(-1.3e7, 1.3e7), ylim = c(-7e6, 7e6))
  plot(data_rast_proj, col = colors, breaks = breaks, add = TRUE, legend = FALSE)
  plot(st_geometry(m_continent_r), col = "white", border = "black", lwd = 0.5, add = TRUE)
  
  # Add legend
  legend_y <- -9.5e6
  legend_x_start <- -8e6
  rect_width <- 2e6
  rect_height <- 1e6
  
  for (i in seq_along(colors)) {
    rect(
      xleft = legend_x_start + (i - 1) * rect_width,
      xright = legend_x_start + i * rect_width,
      ybottom = legend_y - rect_height,
      ytop = legend_y,
      col = colors[i], border = "black"
    )
  }
  
  text_x <- legend_x_start + rect_width * (seq_along(labels) - 1)
  text(
    x = text_x, 
    y = legend_y - 1.5e6,
    labels = labels, 
    cex = 0.8, 
    adj = c(0.5, 1)
  )
  
  unit_x <- legend_x_start + length(colors) * rect_width + rect_width * 0.5
  text(
    x = unit_x, 
    y = legend_y - 0.5e6,
    labels = expression(pieces/m^3), 
    cex = 1.0,
    adj = c(0, 0.5)
  )
  
  text(x = 0, y = 1.2e7, labels = paste("Year", year), cex = 2, font = 2)
  
  # Close graphics device
  dev.off()
}

# Generate and save microplastic maps for all years
for (year in years_to_plot) {
  plot_microplastic_map(year)
}

cat("All annual microplastic concentration maps have been generated and saved!\n")

# Combine individual year plots into a multi-panel figure
library(cowplot)
library(grid)
library(png)

# Load individual images
img_paths <- paste0("./results/global_Mp_", c(2010, 2012, 2014, 2016, 2018, 2020), ".png")
imgs <- lapply(img_paths, png::readPNG)

create_plot <- function(image) {
  ggplot() +
    annotation_custom(rasterGrob(image, width = unit(1, "npc"), height = unit(1, "npc"))) +
    theme_void() +
    theme(aspect.ratio = 0.5)
}

plots <- lapply(imgs, create_plot)

combined_plot <- plot_grid(
  plots[[1]], plots[[2]], plots[[3]],
  plots[[4]], plots[[5]], plots[[6]],
  ncol = 3, nrow = 2,
  align = "hv", rel_widths = c(1, 1, 1), rel_heights = c(1, 1)
)

combined_plot_with_labels <- ggdraw(combined_plot) +
  draw_plot_label(
    label = c("a", "b", "c", "d", "e", "f"),
    x = c(0.03, 0.35, 0.67, 0.03, 0.35, 0.67),
    y = c(0.99, 0.99, 0.99, 0.49, 0.49, 0.49),
    size = 36, fontface = "bold"
  )

ggsave("./results/Figure 3.png",
       plot = combined_plot_with_labels,
       width = 42, height = 16, units = "in", dpi = 300)

cat("Combined figure saved successfully!\n")
