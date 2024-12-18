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

# Load yearly image files (ensure file paths and names are correct)
img_2010 <- png::readPNG("data/processed/global_Mp_2010.png")
img_2011 <- png::readPNG("data/processed/global_Mp_2011.png")
img_2012 <- png::readPNG("data/processed/global_Mp_2012.png")
img_2013 <- png::readPNG("data/processed/global_Mp_2013.png")
img_2014 <- png::readPNG("data/processed/global_Mp_2014.png")
img_2015 <- png::readPNG("data/processed/global_Mp_2015.png")
img_2016 <- png::readPNG("data/processed/global_Mp_2016.png")
img_2017 <- png::readPNG("data/processed/global_Mp_2017.png")
img_2018 <- png::readPNG("data/processed/global_Mp_2018.png")
img_2019 <- png::readPNG("data/processed/global_Mp_2019.png")
img_2020 <- png::readPNG("data/processed/global_Mp_2020.png")

# Function to embed image into a ggplot object
create_plot <- function(image) {
  ggplot() +
    annotation_custom(rasterGrob(image, width = unit(1, "npc"), height = unit(1, "npc"))) +
    theme_void()
}

# Generate individual subplots
plot_2010 <- create_plot(img_2010)
plot_2011 <- create_plot(img_2011)
plot_2012 <- create_plot(img_2012)
plot_2013 <- create_plot(img_2013)
plot_2014 <- create_plot(img_2014)
plot_2015 <- create_plot(img_2015)
plot_2016 <- create_plot(img_2016)
plot_2017 <- create_plot(img_2017)
plot_2018 <- create_plot(img_2018)
plot_2019 <- create_plot(img_2019)
plot_2020 <- create_plot(img_2020)

# Combine 11 plots into a 6-row, 2-column layout (last slot is left empty)
combined_plot <- plot_grid(
  plot_2010, plot_2011,
  plot_2012, plot_2013,
  plot_2014, plot_2015,
  plot_2016, plot_2017,
  plot_2018, plot_2019,
  plot_2020, NULL,  # NULL placeholder for alignment
  ncol = 2, nrow = 6
)

# Add custom labels to each plot
combined_plot_with_labels <- ggdraw(combined_plot) +
  draw_plot_label(
    label = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"),
    x = rep(c(0.03, 0.53), length.out = 11),  # Adjust horizontal position for 2 columns
    y = c(0.995, 0.995, 0.83, 0.83, 0.66, 0.66, 0.495, 0.495, 0.33, 0.33, 0.165),  # Adjust vertical position for 6 rows
    hjust = 0, vjust = 1, size = 48, fontface = "bold"
  )

# Save the combined figure as a PNG file
ggsave("output/Extended Figure 3.png", 
       plot = combined_plot_with_labels, width = 28, height = 48, units = "in", dpi = 300)

cat("The figure has been successfully saved as a 6x2 layout!\n")
