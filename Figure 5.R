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

# Load processed difference data
file_path_diff <- "./results/mp_diff_avg_2018_2020_minus_avg_2010_2012.rds"
data_diff <- readRDS(file_path_diff)

# Colors and breaks for map
colors <- c("blue", "#7a90ff", "#b4c8fd", "#c2d5fb", "#ffffe6",
            "#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c",
            "#fc4e2a", "#cc2321", "#A31C1A")
breaks <- c(-1, -0.75, -0.5, -0.25, 0, 1, 1.5, 2, 10, 20, 50, 100, 700, 100000)

# Load and transform continent boundaries
m_continent_r <- read_sf("./data/world_continent_bound/World_Continents.shp") %>%
  st_transform(crs = '+proj=robin')

# Convert data to SpatRaster
ra <- rast(data_diff[, c("lon", "lat", "mp_diff")], crs = 'epsg:4326')
rab <- project(ra, "+proj=robin")

# Graticule grid
grat <- graticule(lat = seq(-80, 80, by = 20), lon = seq(-180, 180, by = 60))
grat_sf <- st_as_sf(grat)
grat_proj <- st_transform(grat_sf, crs = "+proj=robin")

# Save Figure 5a
png("./results/Figure_5a.png", width = 30, height = 10, units = "in", res = 300)
plot.new()
plot.window(xlim = c(-2.5e7, 2.5e7), ylim = c(-8e6, 8e6))
plot(rab, col = colors, breaks = breaks, box = FALSE, add = TRUE, legend = FALSE)
plot(st_geometry(m_continent_r), add = TRUE, col = "white", border = "black", lwd = 0.5)
plot(st_geometry(grat_proj), col = "gray60", lty = "dashed", lwd = 0.5, add = TRUE)
dev.off()

# Calculate average difference by region
regions <- c("North Atlantic Ocean", "Equatorial Atlantic Ocean", "South Atlantic Ocean", 
             "North Pacific Ocean", "Equatorial Pacific Ocean", "South Pacific Ocean",
             "North Indian Ocean", "Equatorial Indian Ocean", "South Indian Ocean",
             "Southern Ocean", "Arctic Ocean")

average_mp_diff <- data_diff %>%
  group_by(Ocean_Region, lat) %>%
  summarise(mean_mp_diff = mean(mp_diff, na.rm = TRUE))

# Plot average difference by latitude
colors_region <- c(
  "North Atlantic Ocean" = "#FFB300",
  "Equatorial Atlantic Ocean" = "#D32F2F",
  "South Atlantic Ocean" = "#FFD54F",
  "North Pacific Ocean" = "#388E3C",
  "Equatorial Pacific Ocean" = "#1976D2",
  "South Pacific Ocean" = "#64B5F6",
  "North Indian Ocean" = "#7B1FA2",
  "Equatorial Indian Ocean" = "#E91E63",
  "South Indian Ocean" = "#BA68C8",
  "Southern Ocean" = "#9E9E9E",
  "Arctic Ocean" = "#455A64"
)

plot_b <- ggplot(average_mp_diff, aes(x = lat, y = mean_mp_diff, color = Ocean_Region)) +
  geom_line(size = 2) +
  scale_color_manual(values = colors_region, breaks = regions) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Latitude", y = bquote("Microplastic Concentration Change (pieces/m"^3*")")) +
  scale_x_continuous(breaks = c(60, 15, 0, -15, -60), labels = c("60°N", "15°N", "0°", "15°S", "60°S")) +
  theme_minimal(base_size = 22) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.title = element_blank(),
    legend.text = element_text(size = 22, family = "Times New Roman"),
    legend.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title.x = element_text(size = 22, family = "Times New Roman"),
    axis.title.y = element_text(size = 22, family = "Times New Roman"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(size = 22, family = "Times New Roman"),
    axis.text.y = element_text(size = 22, family = "Times New Roman"),
    plot.title = element_blank()
  )
ggsave("./results/Figure_5b.png", plot = plot_b, width = 12, height = 9, units = "in", dpi = 300)

# Combine Figures 5a and 5b
library(magick)
img_a <- image_read("./results/Figure_5a.png")
img_b <- image_read("./results/Figure_5b.png")
img_a_resized <- image_resize(img_a, "2304x1536!")
img_b_resized <- image_resize(img_b, "1800x1536!")
combined <- image_append(c(img_a_resized, img_b_resized), stack = FALSE) %>%
  image_annotate("a", size = 90, location = "+20+20", gravity = "NorthWest", font = "Times New Roman-Bold") %>%
  image_annotate("b", size = 90, location = "+2320+20", gravity = "NorthWest", font = "Times New Roman-Bold")
image_write(combined, path = "./results/Figure_5.png", format = "png")

cat("Figures 5a and 5b combined and saved as Figure 5.png")
