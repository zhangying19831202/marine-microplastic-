library(tidyverse)
library(sf)
library(terra)
library(RColorBrewer)

# Load data
file_path <- "./data/pred_mp_09_20.rds"
data <- readRDS(file_path)

# Calculate average differences between 2018-2020 and 2010-2012
mp_diff <- data %>%
  filter(year %in% c(2010, 2011, 2012, 2018, 2019, 2020)) %>%
  group_by(lat, lon, Oceans) %>%
  summarise(
    avg_mp_early = mean(mp[year %in% c(2010, 2011, 2012)], na.rm = TRUE),
    avg_mp_late = mean(mp[year %in% c(2018, 2019, 2020)], na.rm = TRUE)
  ) %>%
  mutate(
    avg_mp_early = ifelse(avg_mp_early == 0, 0.00001, avg_mp_early),
    avg_mp_late = avg_mp_late + 0.00001,
    mp_diff = (avg_mp_late / avg_mp_early) - 1
  ) %>%
  ungroup()

# Save difference data
saveRDS(mp_diff, "./results/mp_diff_avg_2018_2020_minus_avg_2010_2012.rds")

#####################
library(ggplot2)
library(sf)
library(terra)
library(dplyr)
library(graticule)

# Load processed data
file_path <- "output/mp_diff_avg_2018_2020_minus_avg_2010_2012.rds"
data <- readRDS(file_path)

# Define color scheme and breaks
colors <- c("blue", "#7a90ff", "#b4c8fd", "#c2d5fb", "#ffffe6",
            "#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c",
            "#fc4e2a", "#cc2321", "#A31C1A")
breaks <- c(-1, -0.75, -0.5, -0.25, 0, 1, 1.5, 2, 10, 20, 50, 100, 700, 100000)

# Load and transform continent boundary data
m_continent_r <- read_sf("data/world_continent_bound/World_Continents.shp") %>%
  st_transform(crs = '+proj=robin')

# Convert data to SpatRaster object
ra <- rast(data[, c("lon", "lat", "mp_diff")], crs = 'epsg:4326')
rab <- project(ra, "+proj=robin")

# Create and transform latitude-longitude grid
grat <- graticule(lat = seq(-80, 80, by = 20), lon = seq(-180, 180, by = 60))
grat_sf <- st_as_sf(grat)
grat_proj <- st_transform(grat_sf, crs = "+proj=robin")

# Save output image
png("output/Figure_5a.png", width = 30, height = 10, units = "in", res = 300)

# Plot map
plot.new()
plot.window(xlim = c(-2.5e7, 2.5e7), ylim = c(-8e6, 8e6))
plot(rab, col = colors, breaks = breaks, box = FALSE, add = TRUE, legend = FALSE)
plot(st_geometry(m_continent_r), add = TRUE, col = "white", border = "black", lwd = 0.5)
plot(st_geometry(grat_proj), col = "gray60", lty = "dashed", lwd = 0.5, add = TRUE)

# Add legend
legend("bottom", legend = breaks[-1], fill = colors, title = expression(italic(Delta * Mp) - 1), 
       bty = "n", ncol = 7, cex = 1.2, inset = c(0, 0.05))

dev.off()
cat("Figure 5a saved!\n")

############################
library(magick)

# Load images
img_a <- image_read("output/Figure_5a.png")
img_b <- image_read("output/Figure_5b.png")

# Get image dimensions
width_a <- image_info(img_a)$width
height_a <- image_info(img_a)$height

# Create blank canvas
canvas_height <- height_a + 300  # Shift image B downward by 300 pixels
canvas_width <- width_a + image_info(img_b)$width
canvas <- image_blank(canvas_width, canvas_height, color = "white")

# Combine images
combined <- canvas %>%
  image_composite(img_a, offset = "+0+0") %>%  # Keep image A at original position
  image_composite(img_b, offset = paste0("+", width_a, "+300")) %>%  # Shift image B downward
  image_annotate("a", size = 160, weight = 700, location = "+0+0", gravity = "NorthWest") %>%  # Add label "a"
  image_annotate("b", size = 160, weight = 700, 
                 location = paste0("+", width_a + 20, "+20"), gravity = "NorthWest")  # Add label "b"

# Save final combined image
image_write(combined, path = "output/Figure_5.png", format = "png", density = 300)