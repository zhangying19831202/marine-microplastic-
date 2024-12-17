library(tidyverse)
library(ggplot2)
library(lattice)
library(magrittr)
library(ranger)
library(xgboost)
library(caret)
library(randomForest)
library(deeper)
library(devtools)
library(usethis)
library(ncdf4)
library(lubridate)
library(terra)
library(party)
library(missForest)

set.seed(1234)

df <- readRDS("./data/microplastic_data.rds")
df$log_mp <- log(df$mp + 0.0001)

library(mice)

# Convert character columns to factors if needed
df[] <- lapply(df, function(x) if (is.character(x)) as.factor(x) else x)

# Subset columns 6 to 14 for imputation
sub_df <- df[, 6:14]

# Perform multiple imputation using mice
imputed_sub_df <- mice(sub_df)

# Complete the imputation process
completed_data <- complete(imputed_sub_df)

# Replace imputed values back into the original dataframe
df[, 6:14] <- completed_data
df <- df[complete.cases(df), ]

df <- as.data.frame(df)
df$Oceans <- as.factor(df$Oceans)

size <- caret::createDataPartition(y = df$log_mp, p = 0.9, list = FALSE)
trainset <- df[size, ]
testset <- df[-size, ]

y <- c("log_mp")
x <- colnames(df)[-c(5, 15)]

# Tune model parameters
ranger <- tuningModel(
  basemodel = 'SL.ranger',
  params = list(num.trees = 500),
  tune = list(mtry = 6)
)

# Predict using tuned model
pred_m2_3000 <- predictModel(
  Y = trainset[, y],
  X = trainset[, x],
  newX = testset[, x],
  base_model = c(ranger)
)

pred_m2_predictions <- as.vector(pred_m2_3000$single_pre)

df_plot <- data.frame(Observed = testset$log_mp, Predicted = pred_m2_predictions)

# Filter Observed values
filtered_df_plot <- df_plot %>%
  filter(Observed > -9.21034)

# Calculate R² and slope
model_fit <- lm(Predicted ~ Observed, data = filtered_df_plot)
r_squared <- summary(model_fit)$r.squared
slope <- coef(model_fit)[2]
r_squared_text <- bquote(R^2 == .(format(r_squared, digits = 3)))
slope_text <- bquote(beta == .(format(slope, digits = 3)))

plot <- ggplot(filtered_df_plot, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.5, color = "darkseagreen3", size = 3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "solid", fill = "grey80") +
  labs(x = "Observed log(MP)", y = "Predicted log(MP)") +
  theme_minimal() +
  xlim(-7, 8) +
  ylim(-7, 8) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 13, color = "black"),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(color = "black")
  ) +
  annotate("text", x = 1, y = 6.5, label = as.expression(r_squared_text), size = 5, hjust = 0, color = "black") +
  annotate("text", x = 1, y = 5.5, label = as.expression(slope_text), size = 5, hjust = 0, color = "black")

# Save plot
ggsave("./results/Figure 2.png", 
       plot = plot, width = 8, height = 6, dpi = 300)

cat("Figure updated and saved successfully.")
