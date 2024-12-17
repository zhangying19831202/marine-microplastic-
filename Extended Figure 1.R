library(tidyverse)
library(sf)
library(ggplot2)
library(patchwork)

# ---------------------------
# 1. Load data
# ---------------------------
df <- read_rds("./data/microplastic_data.rds")

# Define classification breaks and labels
breaks <- c(-Inf, 0.01, 0.1, 1, 10, 100, 1000, Inf)
labels <- c("< 0.01", "0.01-0.1", "0.1-1", "1-10", "10-100", "100-1000", "> 1000")

# Add classification column
df <- df %>%
  mutate(
    concentration_category = cut(mp, breaks = breaks, labels = labels, right = FALSE)
  )
df$concentration_category <- factor(df$concentration_category, levels = labels)

# ---------------------------
# 2. Load and transform map data
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
# 3. Map plotting function
# ---------------------------
plot_map <- function(data, year) {
  data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(crs = '+proj=robin')
  
  ggplot() +
    geom_sf(data = outer_polygon, fill = "#d0f0fd", color = "black", size = 1.2) +
    geom_sf(data = sf::st_graticule(lat = seq(-80, 80, by = 20), lon = seq(-180, 180, by = 60)),
            color = "gray60", linetype = "dashed", size = 0.5) +
    geom_sf(data = m_continent_r, fill = "white", color = "black") +
    geom_sf(
      data = data_sf,
      aes(fill = concentration_category),
      color = "black", shape = 21, size = 1.8, alpha = 0.9
    ) +
    scale_fill_manual(
      values = c("#c6dbef", "#a1d99b", "#41ab5d", "#feb24c", "#fd8d3c", "#f03b20", "#b30000"),
      name = "Microplastic Concentration\n(pieces/m\u00b3)"
    ) +
    labs(title = paste("Year", year)) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8),
      axis.text = element_blank(),
      axis.title = element_blank()
    ) +
    guides(
      fill = guide_legend(override.aes = list(size = 4))
    )
}

# ---------------------------
# 4. Generate maps by year
# ---------------------------
years <- 2010:2020
plots <- list()

for (year in years) {
  year_data <- df %>% filter(year == !!year)
  if (nrow(year_data) == 0) {
    warning(paste("No data for Year", year))
  }
  plots[[as.character(year)]] <- plot_map(year_data, year)
}

# Combine all maps
combined_map <- wrap_plots(plots, ncol = 3)

# ---------------------------
# 5. Save combined map
# ---------------------------
ggsave(
  "./results/Extended Figure 1.png",
  plot = combined_map, width = 16, height = 20, dpi = 300
)

cat("Maps for 2010-2020 sampling stations have been generated and combined.")
