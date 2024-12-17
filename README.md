Global Ocean Microplastic Prediction Repository
This repository provides datasets and R scripts for analyzing and predicting global ocean microplastic concentrations (2010–2020) using machine learning methods. A Random Forest model was utilized to predict microplastic distributions based on global environmental data and spatial-temporal covariates.
1. Data
microplastic_data.rds
Content: Observed global ocean microplastic concentrations (2010–2020).
Format: RDS file.
Variables:
Longitude: Longitude of the sampling point.
Latitude: Latitude of the sampling point.
year: Sampling year.
mp: Microplastic concentration (pieces/m³).
2. Code
Figures Code
Scripts to generate Figures 1–5 and Extended Figures 1–3.
Visualizes:
Observed and predicted microplastic concentrations.
Spatial and temporal trends in global oceans.
Prediction Code
random_forest_prediction.R:
Prepares observed microplastic data.
Handles missing data using multiple imputations.
Trains a Random Forest model using:
Environmental predictors.
Spatial coordinates (Longitude, Latitude).
Temporal covariate (year).
Predicts global raster grids of microplastic concentrations.
Outputs model performance (R-squared, RMSE) and visualizations.
3. Data Sources
Environmental data for the analysis were sourced from the E.U. Copernicus Marine Service Information. Variables include:
chl: Chlorophyll concentration.
no3: Nitrate concentration.
po4: Phosphate concentration.
si: Silicate concentration.
nppv: Net Primary Production.
fe: Iron concentration.
phyc: Phytoplankton carbon concentration.
pH: pH levels.
spco2: Surface partial pressure of CO₂.
4. Methodology
Observed Data:
microplastic_data.rds contains log-transformed concentrations:
log_mp=log(mp+0.0001)
Model:
Random Forest model implemented using the Ranger algorithm.
Predictors include nine environmental variables, spatial coordinates, and time.
Predicted Data:
Predictions were log-transformed and then inverse-log transformed to the original scale:
Concentration=𝑒log_predicted−0.0001
Outputs:
Predicted global ocean microplastic grids (2010–2020).
Figures visualizing spatial-temporal trends.
5. Usage Instructions
Observed Data Analysis:
Load microplastic_data.rds for exploratory analysis.
Model Training and Prediction:
Run random_forest_prediction.R to:
Train the Random Forest model.
Predict microplastic concentrations on global grid data.
Figures Generation:
Execute the provided scripts to reproduce Figures 1–5 and Extended Figures 1–3.
Repository Outputs
Observed Data: microplastic_data.rds
Figures: Code for all visualizations.
Predicted Data: Global microplastic concentrations (2010–2020) generated using the Random Forest model.
