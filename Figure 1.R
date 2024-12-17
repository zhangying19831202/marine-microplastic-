library(tidyverse)
library(sf)
library(ggplot2)

# ---------------------------
# 1. Load Data
# ---------------------------
df <- read_rds("./data/microplastic_data.rds")

# Define concentration intervals and labels
breaks <- c(-Inf, 0.01, 0.1, 1, 10, 100, 1000, Inf)
labels <- c("< 0.01", "0.01-0.1", "0.1-1", "1-10", "10-100", "100-1000", "> 1000")

# Add concentration category column
df <- df %>%
  mutate(
    concentration_category = cut(mp, breaks = breaks, labels = labels, right = FALSE)
  )
df$concentration_category <- factor(df$concentration_category, levels = labels)

# ---------------------------
# 2. Load and Transform Map Data
# ---------------------------
m_continent_r <- read_sf("./data/world_continent_bound/World_Continents.shp") %>%
  st_transform(crs = '+proj=robin')

outer_lines <- st_sfc(
  st_linestring(matrix(c(seq(-180, 180, length.out = 100), rep(-90, 100)), ncol = 2)),
  st_linestring(matrix(c(seq(-180, 180, length.out = 100), rep(90, 100)), ncol = 2)),
  st_linestring(matrix(c(rep(-180, 100), seq(-90, 90, length.out = 100)), ncol = 2)),
  st_linestring(matrix(c(rep(180, 100), seq(-90, 90, length.out = 100)), ncol = 2))
) %>%
  st_sfc(crs = 4326) %>%
  st_transform(crs = '+proj=robin') %>%
  st_sf()

outer_polygon <- outer_lines %>%
  st_union() %>%
  st_polygonize() %>%
  st_sf()

df_sf <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = '+proj=robin')

colors <- c("#c6dbef", "#a1d99b", "#41ab5d", "#feb24c", "#fd8d3c", "#f03b20", "#3f007d")

# ---------------------------
# 3. Generate Robinson Map
# ---------------------------
robinson_map <- ggplot() +
  # Ocean background
  geom_sf(data = outer_polygon, fill = "#d0f0fd", color = "black", size = 1.2) +
  # Dashed graticule lines
  geom_sf(data = sf::st_graticule(lat = seq(-80, 80, by = 20), lon = seq(-180, 180, by = 60)),
          color = "gray60", linetype = "dashed", size = 0.5) +
  # Continent boundaries
  geom_sf(data = m_continent_r, fill = "white", color = "black") +
  # Sampling points
  geom_sf(
    data = df_sf,
    aes(fill = concentration_category),
    color = "black", shape = 21, size = 2, alpha = 0.9
  ) +
  # Define color scale
  scale_fill_manual(
    values = colors,
    name = "Microplastic Concentration\n(pieces/m³)"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 6))
  )

# ---------------------------
# 4. Save the Map
# ---------------------------
ggsave(
  "./results/Figure 1.png",
  plot = robinson_map, width = 12, height = 8, dpi = 300
)

cat("Map generated and saved successfully.")
