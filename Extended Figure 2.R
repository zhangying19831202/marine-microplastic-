library(tidyverse)
library(sf)
library(ggplot2)
library(patchwork)

# ---------------------------
# 1. Load data
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

# ---------------------------
# 2. Robinson projection map plotting function
# ---------------------------
plot_robinson_map <- function() {
  ggplot() +
    geom_sf(data = outer_polygon, fill = "#d0f0fd", color = "black", size = 0.5) +
    geom_sf(data = m_continent_r, fill = "white", color = "black", size = 0.2) +
    geom_sf(data = st_sf(
      geometry = st_sfc(
        st_linestring(matrix(c(-180, 15, 180, 15), ncol = 2, byrow = TRUE)),
        st_linestring(matrix(c(-180, -15, 180, -15), ncol = 2, byrow = TRUE))
      ), crs = 4326) %>% 
        st_transform(crs = '+proj=robin'), linetype = "dashed", color = "black"
    ) +
    annotate("text", x = -17000000, y = 15 * 111319.5, label = "15°N", hjust = 1, size = 7, color = "black") +
    annotate("text", x = -17000000, y = -15 * 111319.5, label = "15°S", hjust = 1, size = 7, color = "black") +
    geom_text(data = data.frame(
      x = c(-3000000, -3000000, -3000000, -14000000, -14000000, -14000000, 
            6500000, 7000000, 7000000, 0, 0),
      y = c(4500000, 0, -3000000, 4500000, 0, -3000000, 
            2000000, 0, -3000000, 7500000, -6500000),
      label = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)", "(11)")
    ), aes(x = x, y = y, label = label), size = 6, color = "black") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

# ---------------------------
# 3. Generate and save map
# ---------------------------
robinson_map <- plot_robinson_map()

ggsave(
  "./results/Extended Figure 2.png",
  plot = robinson_map, width = 16, height = 10, dpi = 300
)

cat("Robinson projection map has been generated with light blue oceans and white continents.")
