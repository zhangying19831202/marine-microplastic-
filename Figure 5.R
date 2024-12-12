# 加载必要的库
library(tidyverse)
library(sf)
library(terra)
library(RColorBrewer)

# 读取数据
file_path <- "F:/科研2024/微塑料全球变化-2024/海洋数据处理/20240726/pred_mp_07_28.rds"
data <- readRDS(file_path)

# 计算2018-2020年与2010-2012年的微塑料平均值的倍数差异
mp_diff <- data %>%
  filter(year %in% c(2010, 2011, 2012, 2018, 2019, 2020)) %>%
  group_by(lat, lon, Oceans) %>%
  summarise(
    avg_mp_early = mean(mp[year %in% c(2010, 2011, 2012)], na.rm = TRUE),
    avg_mp_late = mean(mp[year %in% c(2018, 2019, 2020)], na.rm = TRUE)
  ) %>%
  mutate(
    avg_mp_early = ifelse(avg_mp_early == 0, 0.00001, avg_mp_early),  # 防止除以0
    avg_mp_late = avg_mp_late + 0.00001,  # 避免平均值为零的问题
    mp_diff = (avg_mp_late / avg_mp_early) - 1  # 计算倍数差异
  ) %>%
  ungroup()

# 查看计算结果
print(head(mp_diff))

# 保存计算结果
saveRDS(mp_diff, "F:/科研2024/微塑料全球变化-2024/海洋数据处理/20240726/mp_diff_avg_2018_2020_minus_avg_2010_2012.rds")
# 加载必要的库
library(dplyr)
library(readr)

# 读取保存的 RDS 文件
file_path <- "F:/科研2024/微塑料全球变化-2024/海洋数据处理/20240726/mp_diff_avg_2018_2020_minus_avg_2010_2012.rds"
data <- readRDS(file_path)
# 计算 mp_diff 列的 1%、50%、95% 和 99% 百分位数
percentiles <- data %>%
  summarise(
    min_value = min(mp_diff, na.rm = TRUE),
    p1 = quantile(mp_diff, 0.01, na.rm = TRUE),
    p50 = quantile(mp_diff, 0.50, na.rm = TRUE),
    p95 = quantile(mp_diff, 0.95, na.rm = TRUE),
    p99 = quantile(mp_diff, 0.99, na.rm = TRUE),
    max_value = max(mp_diff, na.rm = TRUE)
  )
####  min_value     p1   p50   p95   p99 max_value
####<dbl>  <dbl> <dbl> <dbl> <dbl>     <dbl>
####  1    -0.971 -0.737  2.79  135.  656.    92155.
# 查看结果
print(percentiles)
# 设置颜色方案和分割点
colors <- c("blue", "#7a90ff", "#b4c8fd", "#c2d5fb", "#ffffe6",
            "#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c",
            "#fc4e2a", "#cc2321", "#A31C1A")
breaks <- c(-1, -0.75, -0.5, -0.25, 0, 1, 1.5, 2, 10, 20, 50, 100, 700, 100000)

# 读取并转换大陆边界数据
m_continent_r <- read_sf("F:/科研2024/微塑料全球变化-2024/海洋数据处理/20240726/world_continent_bound/World_Continents.shp") %>%
  st_transform(crs = '+proj=robin')

# 手动创建经纬度线
lat_lines <- seq(-80, 80, by = 20)
lon_lines <- seq(-180, 180, by = 60)

# 创建纬度线
lat_sf <- st_sfc(lapply(lat_lines, function(lat) {
  st_linestring(matrix(c(seq(-180, 180, length.out = 100), rep(lat, 100)), ncol = 2, byrow = FALSE))
}), crs = 4326)

# 创建经度线
lon_sf <- st_sfc(lapply(lon_lines, function(lon) {
  st_linestring(matrix(c(rep(lon, 100), seq(-90, 90, length.out = 100)), ncol = 2, byrow = FALSE))
}), crs = 4326)

# 转换为 sf 对象并投影转换
lat_sf <- st_transform(st_sf(geometry = lat_sf), crs = '+proj=robin')
lon_sf <- st_transform(st_sf(geometry = lon_sf), crs = '+proj=robin')

# 转换数据为 SpatRaster 对象
ra <- rast(mp_diff[, c("lon", "lat", "mp_diff")], crs = 'epsg:4326')
rab <- project(ra, "+proj=robin")

# 扩展到目标范围
robin_ext <- ext(-17005759, 17005759, -8625155, 8625155)
rab <- extend(rab, robin_ext)

# 保存输出图片，调整画布尺寸
png("F:/科研2024/微塑料全球变化-2024/海洋数据处理/20240726/Figure 4a.png", 
     width = 20, height = 10, units = "in", res = 300)

# 设置布局参数和边距
layout(matrix(c(1, 2), ncol = 1), heights = c(0.90, 0.10))  # 90% 绘图，10% 图例
par(mar = c(6, 6, 6, 6), xpd = TRUE)  # 设置足够的边距

# 绘制主图
plot.new()
plot.window(xlim = c(-2.5e7, 2.5e7), ylim = c(-8e6, 8e6))  # 缩小地图显示范围，横向变窄
plot(rab, col = colors, breaks = breaks, box = FALSE, add = TRUE, legend = FALSE)  # 关闭自动图例

# 添加大陆边界
plot(st_geometry(m_continent_r), add = TRUE, col = "white")

# 添加经纬度网格线
plot(st_geometry(lat_sf), add = TRUE, col = "gray", lty = 2)
plot(st_geometry(lon_sf), add = TRUE, col = "gray", lty = 2)

# 格式化纬度标签
lat_labels_formatted <- sapply(lat_lines, function(lat) {
  if (lat > 0) {
    paste0(lat, "°N")
  } else if (lat < 0) {
    paste0(abs(lat), "°S")
  } else {
    paste0(lat, "°")
  }
})

# 添加纬度标签，增大字号并向左移动
lat_labels <- st_transform(st_sf(geometry = st_sfc(lapply(lat_lines, function(lat) {
  st_point(c(-180, lat))
}), crs = 4326)), crs = '+proj=robin')

text(st_coordinates(lat_labels)[,1] - 200000, st_coordinates(lat_labels)[,2], 
     labels = lat_labels_formatted, pos = 2, cex = 1.6, adj = c(1, 0.5))

# 格式化经度标签
lon_labels_formatted <- sapply(lon_lines, function(lon) {
  if (lon > 0) {
    paste0(lon, "°E")
  } else if (lon < 0) {
    paste0(abs(lon), "°W")
  } else {
    paste0(lon, "°")
  }
})
# 添加经度标签
lon_labels <- st_transform(st_sf(geometry = st_sfc(lapply(lon_lines, function(lon) {
  st_point(c(lon, -90))
}), crs = 4326)), crs = '+proj=robin')

text(st_coordinates(lon_labels)[,1], st_coordinates(lon_labels)[,2] - 200000,
     labels = lon_labels_formatted, pos = 1, cex = 1.6)

# 绘制图例
par(mar = c(0, 0, 0, 0))  # 重设边距以绘制图例

# 初始化图例绘图区域
plot.new()
plot.window(xlim = c(-180, 180), ylim = c(0, 1))

# 计算矩形宽度，使其紧密连接
legend_x <- seq(-60, 60, length.out = length(colors) + 1)  # +1 确保没有空隙
rect_width <- legend_x[2] - legend_x[1]
rect_height <- 0.2

# 绘制紧密连接的颜色矩形
for (i in seq_along(colors)) {
  rect(xleft = legend_x[i], 
       ybottom = 0.5 - rect_height / 2, 
       xright = legend_x[i + 1], 
       ytop = 0.5 + rect_height / 2, 
       col = colors[i], 
       border = "black")
}

# 绘制图例标签
labels <- c("-1", "-0.75", "-0.5", "-0.25", "0", "1", "1.5", "2", "10", "20", "50", "100", "700","100000")
for (i in seq_along(labels)) {
  text(x = legend_x[i], y = 0.5 - rect_height / 2 - 0.05, labels = labels[i], cex = 1.2, adj = c(0.5, 1))
}
# 在矩形图例最右端上方添加斜体文本
text(x = legend_x[length(legend_x)] + rect_width / 2, y = 0.5 + rect_height/4 + 0.2, 
     labels = expression(italic(Delta * Mp)-1), cex = 1.6, adj = c(0.5, 0))
# 关闭设备
dev.off()
#####################
library(tidyverse)
library(sf)
library(terra)
library(RColorBrewer)

# 读取数据
file_path <- "F:/科研2024/微塑料全球变化-2024/海洋数据处理/20240726/pred_mp_09_20.rds"
data <- readRDS(file_path)

# 计算2018-2020年与2010-2012年的微塑料平均值的差值
mp_diff <- data %>%
  filter(year %in% c(2010, 2011, 2012, 2018, 2019, 2020)) %>%
  group_by(lat, lon, Ocean_Region) %>%  # 包含 Ocean_Region 列
  summarise(
    avg_mp_early = mean(mp[year %in% c(2010, 2011, 2012)], na.rm = TRUE),
    avg_mp_late = mean(mp[year %in% c(2018, 2019, 2020)], na.rm = TRUE)
  ) %>%
  mutate(
    mp_diff = avg_mp_late - avg_mp_early  # 计算差值
  ) %>%
  ungroup()

# 查看计算结果
print(head(mp_diff))

# 保存计算结果
saveRDS(mp_diff, "F:/科研2024/微塑料全球变化-2024/海洋数据处理/20240726/mp_diff_avg_2018_2020_minus_avg_2010_2012_2.rds")

# 读取计算后的差值数据
data <- readRDS("F:/科研2024/微塑料全球变化-2024/海洋数据处理/20240726/mp_diff_avg_2018_2020_minus_avg_2010_2012_2.rds")

# 提取11个区域的数据
regions <- c("North Atlantic Ocean", "Equatorial Atlantic Ocean", "South Atlantic Ocean", 
             "North Pacific Ocean", "Equatorial Pacific Ocean", "South Pacific Ocean",
             "North Indian Ocean", "Equatorial Indian Ocean", "South Indian Ocean",
             "Southern Ocean", "Arctic Ocean")

# 计算每个区域的平均差值
average_mp_diff <- data %>%
  group_by(Ocean_Region, lat) %>%
  summarise(mean_mp_diff = mean(mp_diff, na.rm = TRUE))  # 计算每个区域的平均差值


colors <- c(
  "North Atlantic Ocean" = "#FFB300",       # Bright amber for North Atlantic
  "Equatorial Atlantic Ocean" = "#D32F2F",  # Vivid red for Equatorial Atlantic
  "South Atlantic Ocean" = "#FFD54F",       # Bright yellow for South Atlantic
  "North Pacific Ocean" = "#388E3C",        # Vivid green for North Pacific
  "Equatorial Pacific Ocean" = "#1976D2",   # Bright blue for Equatorial Pacific
  "South Pacific Ocean" = "#64B5F6",        # Light blue for South Pacific
  "North Indian Ocean" = "#7B1FA2",         # Rich purple for North Indian Ocean
  "Equatorial Indian Ocean" = "#E91E63",    # Bright pink for Equatorial Indian Ocean
  "South Indian Ocean" = "#BA68C8",         # Light purple for South Indian Ocean
  "Southern Ocean" = "#9E9E9E",             # Neutral gray for Southern Ocean
  "Arctic Ocean" = "#455A64"                # Dark gray-blue for Arctic Ocean
)


# 绘制图表
ggplot(average_mp_diff, aes(x = lat, y = mean_mp_diff, color = Ocean_Region)) +
  geom_line(size = 2) +  # 增加线条粗细
  scale_color_manual(values = colors, breaks = regions) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Latitude", y = bquote("Microplastic Concentration Change (pieces/m"^3*")")) +
  scale_x_continuous(breaks = c(60, 15, 0, -15, -60), labels = c("60°N", "15°N", "0°", "15°S", "60°S")) +
  theme_minimal(base_size = 20) +  # 增加基础字号
  
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = c(1, 1),  # 图例位置设置为右上角
    legend.justification = c(1, 1),  # 图例对齐方式设置为右上角
    legend.title = element_blank(),
    legend.text = element_text(size = 20, family = "Times New Roman"),  # 图例文本字号增加
    legend.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title.x = element_text(size = 20, family = "Times New Roman"),  # 横轴标题字号增加
    axis.title.y = element_text(size = 20, family = "Times New Roman"),  # 纵轴标题字号增加
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(size = 20, family = "Times New Roman"),  # 横轴数值字号增加
    axis.text.y = element_text(size = 20, family = "Times New Roman"),
    plot.title = element_blank()
  ) 

  

# 保存图表为 PNG 和 TIFF 格式
# 保存图表为 PNG 和 TIFF 格式，设置白色背景
ggsave("F:/科研2024/微塑料全球变化-2024/海洋数据处理/20240726/Figure 4b.png", 
       plot = last_plot(), 
       width = 12, height = 9, 
       units = "in", 
       dpi = 300, 
       bg = "white")



# 加载必要的包
library(dplyr)

# Equatorial Atlantic Ocean
equatorial_atlantic_increase <- average_mp_diff %>%
  filter(Ocean_Region == "Equatorial Atlantic Ocean" & mean_mp_diff > 0)
lat_range_increase <- range(equatorial_atlantic_increase$lat)
max_increase_value <- max(equatorial_atlantic_increase$mean_mp_diff, na.rm = TRUE)
cat("The latitude range of microplastic increase in the Equatorial Atlantic Ocean is from",
    lat_range_increase[1], "to", lat_range_increase[2], "degrees.\n")
cat("The maximum microplastic concentration increase in the Equatorial Atlantic Ocean is", 
    max_increase_value, "pieces/m³.\n")#54.02939 pieces/m³

# Equatorial Pacific Ocean
equatorial_pacific_increase <- average_mp_diff %>%
  filter(Ocean_Region == "Equatorial Pacific Ocean" & mean_mp_diff > 0)
max_increase_equatorial_pacific <- max(equatorial_pacific_increase$mean_mp_diff, na.rm = TRUE)
cat("The maximum microplastic concentration increase in the Equatorial Pacific Ocean is", 
    max_increase_equatorial_pacific, "pieces/m³.\n")#26.42034 pieces/m³

# North Pacific Ocean - 获取最大和次大值
north_pacific_increase <- average_mp_diff %>%
  filter(Ocean_Region == "North Pacific Ocean" & mean_mp_diff > 0) %>%
  arrange(desc(mean_mp_diff))
top2_north_pacific <- north_pacific_increase$mean_mp_diff[1:2]
cat("The two highest microplastic concentration increases in the North Pacific Ocean are", 
    top2_north_pacific[1], "and", top2_north_pacific[2], "pieces/m³.\n")
#North Pacific Ocean are 4.012468 and 3.944642 pieces/m³.
# South Indian Ocean - 获取最大和次大值
south_indian_increase <- average_mp_diff %>%
  filter(Ocean_Region == "South Indian Ocean" & mean_mp_diff > 0) %>%
  arrange(desc(mean_mp_diff))
top2_south_indian <- south_indian_increase$mean_mp_diff[1:2]
cat("The two highest microplastic concentration increases in the South Indian Ocean are", 
    top2_south_indian[1], "and", top2_south_indian[2], "pieces/m³.\n")
#South Indian Ocean are 5.007205 and 4.966976 pieces/m

# 加载必要的库
library(ggplot2)
library(cowplot)
library(png)
library(grid)

# 安装并加载 magick 包（如果尚未安装）
if (!require(magick)) install.packages("magick")
library(magick)

# 读取图像
img_a <- image_read("F:/科研2024/微塑料全球变化-2024/海洋数据处理/20240726/Figure 4a.png")
img_b <- image_read("F:/科研2024/微塑料全球变化-2024/海洋数据处理/20240726/Figure 4b.png")

# 调整图 a 和图 b 的尺寸
img_a_resized <- image_resize(img_a, "1600x1536!")  # 放大图 a 到宽 1600，保持高 1536
img_b_resized <- image_resize(img_b, "1800x1536!")  # 缩小图 b 到宽 1600，保持高 1536

# 水平合并图像并添加加粗标注
combined <- image_append(c(img_a_resized, img_b_resized), stack = FALSE) %>%
  image_annotate("a", size = 70, location = "+20+20", gravity = "NorthWest", font = "Times New Roman-Bold") %>%
  image_annotate("b", size = 70, location = "+1620+20", gravity = "NorthWest", font = "Times New Roman-Bold")


# 保存合并后的图像 
image_write(combined, path = "F:/科研2024/微塑料全球变化-2024/海洋数据处理/paper/paper_10_25/Figure 5.png", format = "png")
###################
