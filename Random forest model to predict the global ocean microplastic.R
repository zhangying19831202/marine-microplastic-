library(ggplot2)
library(lattice)
library(magrittr)
library(ranger)
library(xgboost)
library(caret)
library(randomForest)
library(mice)
library(tidyverse)
library(SuperLearner)
set.seed(1234)

# ---------------------------
# 1. Load and Prepare Data
# ---------------------------
# Load dataset
input_data <- "data/microplastic_data.rds"
df <- readRDS(input_data)

# Log-transform microplastic concentration
df$log_mp <- log(df$mp + 0.0001)

# Convert character columns to factors
df[] <- lapply(df, function(x) if (is.character(x)) as.factor(x) else x)

# Subset relevant columns for imputation
subset_df <- df[, 6:14]

# Perform multiple imputation using mice
imputed_data <- mice(subset_df)
completed_data <- complete(imputed_data)

# Replace imputed values back into the original dataframe
df[, 6:14] <- completed_data

# Remove rows with incomplete cases
df <- df[complete.cases(df), ]
df <- as.data.frame(df)

# Set 'Oceans' column as a factor
df$Oceans <- as.factor(df$Oceans)

# ---------------------------
# 2. Split Data into Training and Test Sets
# ---------------------------
size <- caret::createDataPartition(y = df$log_mp, p = 0.9, list = FALSE)
trainset <- df[size, ]
testset <- df[-size, ]

y <- c("log_mp")
x <- colnames(df)[-c(5, 15)]

# ---------------------------
# 3. Train Model with Ranger (Random Forest)
# ---------------------------
ranger_model <- tuningModel(
  basemodel = 'SL.ranger',
  params = list(num.trees = 500),
  tune = list(mtry = 6)
)

# Predict using the tuned model
predictions <- predictModel(
  Y = trainset[, y],
  X = trainset[, x],
  newX = testset[, x],
  base_model = c(ranger_model)
)

# Save the trained model
output_model <- "output/pred_log_mp.rds"
saveRDS(predictions, output_model)

# ---------------------------
# 4. Evaluate Model Performance
# ---------------------------
predicted_values <- predictions$single_pre

# R-squared calculation
r_squared <- caret::R2(obs = testset$log_mp, pred = predicted_values)
print(paste("R-squared:", r_squared))

# Plot observed vs. predicted values
df_plot <- data.frame(Observed = testset$log_mp, Predicted = predicted_values)

ggplot(df_plot, aes(x = Observed, y = Predicted)) +
  geom_point(aes(color = "Predicted"), alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Observed", y = "Predicted", title = "Observed vs Predicted") +
  theme_minimal() +
  xlim(0, 8)

# ---------------------------
# 5. Predict on Grid Data
# ---------------------------
# Load grid data
grid_data <- readRDS("data/predenvironment9_with_ocean.rds")

# Rename column for consistency
grid_data <- grid_data %>% rename("Oceans" = "ocean")

# Ensure grid data structure matches testset
grid_data_combined <- bind_rows(grid_data, testset)
grid_data_filtered <- grid_data_combined[1:nrow(grid_data), ]

# Predict for grid data
pred_grid <- SuperLearner::predict.SuperLearner(
  object = predictions$model_summary,
  newdata = grid_data_filtered[, x]
)

# Extract predictions and save
predicted_grid_values <- pred_grid$pred
output_grid <- "output/pred_grid_19_07.rds"
saveRDS(predicted_grid_values, output_grid)

# Display head of predictions
head(predicted_grid_values)

cat("Model training, evaluation, and grid prediction complete. Outputs saved in 'output/' folder.\n")
