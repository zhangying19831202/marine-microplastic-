library(ggplot2)
library(sf)
library(terra)
library(dplyr)
library(graticule)  # For drawing graticules (latitude and longitude lines)

# ---------------------------
# 1. Load Data
# ---------------------------
# Load annual average microplastic data
data <- readRDS("data/pred_mp_09_20.rds")

# Load and transform world continent boundary data
m_continent_r <- read_sf("data/world_continent_bound/World_Continents.shp") %>%
  st_transform(crs = '+proj=robin')

# ---------------------------
# 2. Define Colors and Breaks
# ---------------------------
breaks <- c(-Inf, 0.01, 0.1, 1, 10, 100, 1000, Inf)
labels <- c(
  "< 0.01", "0.01", "0.1", "1", "10", "100", "1000", "> 1000"
)
colors <- c("#e0f3f8", "#a6cee3", "#7fc97f", "#ffd700", "#fdae61", "#f46d43", "#e31a1c")

# Define years to plot
years_to_plot <- 2010:2020

# ---------------------------
# 3. Plot and Save Maps for Each Year
# ---------------------------
plot_microplastic_map <- function(year) {
  # Filter data for the specified year
  data_year <- data %>% filter(year == !!year)
  
  # Convert data to SpatRaster and project to Robinson
  data_rast <- rast(data_year[, c("lon", "lat", "mp")], crs = 'epsg:4326')
  data_rast_proj <- project(data_rast, "+proj=robin")
  rast_df <- as.data.frame(data_rast_proj, xy = TRUE)
  colnames(rast_df) <- c("x", "y", "mp")
  rast_df <- rast_df %>% filter(!is.na(mp))
  
  # Open device to save the PNG file
  png(
    filename = paste0("output/global_Mp_", year, ".png"),
    width = 2400, height = 1400, res = 300
  )
  
  # Set plot parameters
  par(mar = c(5, 1, 2, 1), oma = c(0, 0, 0, 0), xpd = TRUE)
  
  # Plot the base map
  plot(st_geometry(m_continent_r), col = "#d0f0fd", border = NA, reset = FALSE,
       xlim = c(-1.3e7, 1.3e7), ylim = c(-7e6, 7e6))
  
  # Overlay microplastic data
  plot(data_rast_proj, col = colors, breaks = breaks, add = TRUE, legend = FALSE)
  
  # Add continent boundaries
  plot(st_geometry(m_continent_r), col = "white", border = "black", lwd = 0.5, add = TRUE)
  
  # Add graticules (latitude and longitude lines)
  grat <- sf::st_graticule(lat = seq(-80, 80, by = 20), lon = seq(-180, 180, by = 60))
  grat_proj <- st_transform(grat, crs = "+proj=robin")
  plot(st_geometry(grat_proj), col = "gray60", lty = "dashed", lwd = 0.5, add = TRUE)
  
  # Add legend
  legend_y <- -9.5e6  # Y position for the legend
  legend_x_start <- -8e6  # Starting X position for legend
  rect_width <- 2e6  # Width of each color block
  rect_height <- 1e6  # Height of each color block
  
  for (i in seq_along(colors)) {
    rect(
      xleft = legend_x_start + (i - 1) * rect_width,
      xright = legend_x_start + i * rect_width,
      ybottom = legend_y - rect_height,
      ytop = legend_y,
      col = colors[i], border = "black"
    )
  }
  
  # Add legend labels
  text_x <- legend_x_start + rect_width * (seq_along(labels) - 1)
  text(x = text_x, y = legend_y - 1.5e6, labels = labels, cex = 0.8, adj = c(0.5, 1))
  
  # Add unit label
  unit_x <- legend_x_start + length(colors) * rect_width + rect_width * 0.5
  text(x = unit_x, y = legend_y - 0.5e6, labels = expression(pieces/m^3), cex = 1.0, adj = c(0, 0.5))
  
  # Add title
  text(x = 0, y = 1.2e7, labels = paste("Year", year), cex = 2, font = 2)
  
  # Close device
  dev.off()
}

# Generate maps for all years
for (year in years_to_plot) {
  plot_microplastic_map(year)
}

cat("All yearly microplastic concentration maps have been generated and saved!\n")


# ---------------------------
# 4. Combine Saved Maps into a Single Figure
# ---------------------------
library(ggplot2)
library(cowplot)
library(grid)
library(png)

# Load saved images
images <- lapply(years_to_plot, function(year) {
  png::readPNG(paste0("output/global_Mp_", year, ".png"))
})

# Function to create ggplot for each image
create_plot <- function(image) {
  ggplot() +
    annotation_custom(rasterGrob(image, width = unit(1, "npc"), height = unit(1, "npc"))) +
    theme_void()
}

# Generate plots for each year
plots <- lapply(images, create_plot)

# Combine all plots in a grid
combined_plot <- plot_grid(plotlist = plots, ncol = 3, nrow = 4)

# Add custom labels
combined_plot_with_labels <- ggdraw(combined_plot) +
  draw_plot_label(
    label = letters[1:length(years_to_plot)],
    x = rep(c(0.03, 0.35, 0.67), 4),
    y = rep(seq(0.995, 0.245, length.out = 4), each = 3),
    hjust = 0, vjust = 1, size = 12, fontface = "bold"
  )

# Save combined figure
ggsave("output/Extended Figure 3.png", 
       plot = combined_plot_with_labels, width = 20, height = 16, units = "in", dpi = 300)

cat("Combined microplastic maps have been saved!\n")
