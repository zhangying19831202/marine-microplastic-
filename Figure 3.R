# Required Libraries
library(ggplot2)
library(sf)
library(terra)
library(dplyr)
library(cowplot)
library(grid)
library(png)

# --------------------------- #
# Generate Microplastic Maps
# --------------------------- #

# Load annual microplastic average data
data <- readRDS("data/processed/pred_mp_09_20.rds")

# Load and transform continent boundary data
m_continent_r <- read_sf("data/processed/world_continent_bound/World_Continents.shp") %>%
  st_transform(crs = '+proj=robin')

# Define color scheme and breaks
breaks <- c(-Inf, 0.01, 0.1, 1, 10, 100, 1000, Inf)
labels <- c("< 0.01", "0.01", "0.1", "1", "10", "100", "1000", "> 1000")
colors <- c("#e0f3f8", "#a6cee3", "#7fc97f", "#ffd700", "#fdae61", "#f46d43", "#e31a1c")

# Define years to plot
years_to_plot <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

# Function to generate and save maps
plot_microplastic_map <- function(year) {
  # Filter data for the selected year
  data_year <- data %>% filter(year == !!year)
  
  # Convert to SpatRaster object and project
  data_rast <- rast(data_year[, c("lon", "lat", "mp")], crs = 'epsg:4326')
  data_rast_proj <- project(data_rast, "+proj=robin")
  rast_df <- as.data.frame(data_rast_proj, xy = TRUE)
  colnames(rast_df) <- c("x", "y", "mp")
  rast_df <- rast_df %>% filter(!is.na(mp))
  
  # Save map as PNG
  png(
    filename = paste0("output/global_Mp_", year, ".png"),
    width = 2400, height = 1400, res = 300
  )
  
  # Plot map
  par(mar = c(5, 1, 2, 1), oma = c(0, 0, 0, 0), xpd = TRUE)
  plot(st_geometry(m_continent_r), col = "#d0f0fd", border = NA, reset = FALSE,
       xlim = c(-1.3e7, 1.3e7), ylim = c(-7e6, 7e6))
  plot(data_rast_proj, col = colors, breaks = breaks, add = TRUE, legend = FALSE)
  plot(st_geometry(m_continent_r), col = "white", border = "black", lwd = 0.5, add = TRUE)
  
  # Add legend and labels
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
  text(x = text_x, y = legend_y - 1.5e6, labels = labels, cex = 1.0, adj = c(0.5, 1))
  unit_x <- legend_x_start + length(colors) * rect_width + rect_width * 0.5
  text(x = unit_x, y = legend_y - 0.5e6, labels = expression(pieces/m^3), cex = 1.1, adj = c(0, 0.5))
  text(x = 0, y = 1.2e7, labels = paste("Year", year), cex = 2, font = 2)
  
  dev.off()
}

# Generate maps for all years
for (year in years_to_plot) {
  plot_microplastic_map(year)
}

cat("Microplastic maps for all years have been generated and saved.\n")

# --------------------------- #
# Combine Selected Maps
# --------------------------- #

# Load map images
# Load required libraries
library(ggplot2)
library(cowplot)
library(grid)
library(png)

# Read saved images (ensure correct file paths and names)
img_2010 <- png::readPNG("data/processed/global_Mp_2010.png")
img_2012 <- png::readPNG("data/processed/global_Mp_2012.png")
img_2014 <- png::readPNG("data/processed/global_Mp_2014.png")
img_2016 <- png::readPNG("data/processed/global_Mp_2016.png")
img_2018 <- png::readPNG("data/processed/global_Mp_2018.png")
img_2020 <- png::readPNG("data/processed/global_Mp_2020.png")

# Function to embed an image in a ggplot object, remove margins, and smooth the image
create_plot <- function(image) {
  ggplot() +
    annotation_custom(rasterGrob(image, interpolate = TRUE, 
                                 width = unit(1, "npc"), height = unit(1, "npc"))) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0, "cm"))  # Remove any margins
}

# Generate individual subplots
plot_a <- create_plot(img_2010)
plot_b <- create_plot(img_2012)
plot_c <- create_plot(img_2014)
plot_d <- create_plot(img_2016)
plot_e <- create_plot(img_2018)
plot_f <- create_plot(img_2020)

# Combine into a 3x2 layout, with equal row and column proportions
combined_plot <- plot_grid(
  plot_a, plot_b, 
  plot_c, plot_d, 
  plot_e, plot_f,
  ncol = 2, nrow = 3,
  align = "hv", 
  rel_widths = c(1, 1),  # Equal column widths
  rel_heights = c(1, 1, 1)  # Equal row heights
)

# Add panel labels at specified positions
combined_plot_with_labels <- ggdraw(combined_plot) +
  draw_plot_label(
    label = c("a", "b", "c", "d", "e", "f"),
    x = c(0.03, 0.53, 0.03, 0.53, 0.03, 0.53),  # Horizontal positions
    y = c(0.99, 0.99, 0.66, 0.66, 0.33, 0.33),  # Vertical positions
    size = 42, fontface = "bold"  # Bold font and large size
  )

# Save the combined plot
ggsave("output/Figure3_combined.png", 
       plot = combined_plot_with_labels, width = 28, height = 24, units = "in", dpi = 300)

cat("Figure 3 has been saved successfully without artifacts!\n")
