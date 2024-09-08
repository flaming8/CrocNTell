# Install necessary packages if not already installed
# install.packages(c("tidyverse", "lubridate", "randomForest", "sf", "readxl", "caret", "e1071", "xgboost", "forecast"))

# Load required libraries
library(tidyverse)    # Data manipulation
library(lubridate)    # Date handling
library(randomForest) # Random Forest model
library(sf)           # Spatial data for mapping zones
library(readxl)       # Reading Excel files
library(caret)        # Model evaluation (RMSE)
library(e1071)        # Support Vector Machine
library(xgboost)      # XGBoost model
library(forecast)     # Time series forecasting
library(corrplot)     # Correlation plots

# Read the shapefile for zone assignment
zones <- st_read("NT_Croc_Capture_Zones.shp")

# Function to assign zones based on lat/long
assign_zones <- function(data, zones) {
  # Perform spatial join to assign zones
  joined <- st_join(data, zones)
  
  # Add zone information back to the original data
  data$ZONE <- joined$ZONENAME  # Adjust ZONE_NAME to match shapefile's column name
  return(data)
}

# Step 1: Read the shapefile for zones and inspect the column names
zones <- st_read("NT_Croc_Capture_Zones.shp")

# Inspect the column names in the shapefile
colnames(zones)

# Convert the shapefile to a dataframe for merging
zones_df <- as.data.frame(zones)

# Step 2: Load the crocodile capture data and inspect columns
croc_data <- read_excel("NT Crocodile Capture.xlsx")
colnames(croc_data)

# Step 3: Perform the join using 'ZONE' from croc_data and 'ZONENAME' from zones
croc_data_with_zones <- left_join(croc_data, zones_df, by = c("ZONE" = "ZONENAME"))

# Verify the joined data
print(head(croc_data_with_zones))

# Step 4: Plot the zones from the shapefile
ggplot() + 
  geom_sf(data = zones, aes(fill = ZONENAME)) + 
  labs(title = "Crocodile Capture Zones") +
  theme_minimal()

# Step 5: Load additional datasets (Crocodile Survey and Capture Number data)
survey_data <- read_excel("Crocodile_Survey_Data_2021_22.xlsx")
capture_number_data <- read_excel("NT Crocodile Capture number.xlsx")

# Step 6: Assign zones to the survey data
if (!("ZONE" %in% colnames(survey_data))) {
    if (!("Latitude" %in% colnames(survey_data)) || !("Longitude" %in% colnames(survey_data))) {
        stop("Both Latitude and Longitude are missing from survey_data. Can't assign zones.")
    } else {
        survey_data_sf <- st_as_sf(survey_data, coords = c("Longitude", "Latitude"), crs = 4326)  # Assuming WGS84
        
        if (st_crs(survey_data_sf) != st_crs(zones)) {
            survey_data_sf <- st_transform(survey_data_sf, crs = st_crs(zones))
        }
        
        survey_data_with_zones <- assign_zones(survey_data_sf, zones)
    }
} else {
    survey_data_with_zones <- survey_data %>%
        left_join(zones, by = "ZONE")  # Adjust this if necessary based on your data structure
}

# Step 7: Extract YEAR and MONTH from DATE_CAPTURED in croc_data_with_zones
if ("DATE_CAPTURED" %in% colnames(croc_data_with_zones)) {
    croc_data_with_zones <- croc_data_with_zones %>%
        mutate(YEAR = year(DATE_CAPTURED),  # Extract year from DATE_CAPTURED
               Month = month(DATE_CAPTURED))  # Extract month from DATE_CAPTURED
}

# Check the structure of the crocodile data with zones
print("Crocodile Data with Zones Columns:")
print(colnames(croc_data_with_zones))

# Step 8: Ensure YEAR is present in both datasets
if (!("YEAR" %in% colnames(survey_data_with_zones))) {
    stop("YEAR column is missing from survey_data_with_zones.")
}

if (!("YEAR" %in% colnames(croc_data_with_zones))) {
    stop("YEAR column is missing from croc_data_with_zones.")
}

# Step 9: Merge the datasets by ZONE and YEAR
merged_data <- survey_data_with_zones %>%
    left_join(croc_data_with_zones, by = c("ZONE", "YEAR")) %>%
    left_join(capture_number_data, by = c("ZONE", "YEAR"))

# Verify the joined data
print(head(merged_data))

# Step 10: Plot the zones from the shapefile
ggplot() + 
    geom_sf(data = zones, aes(fill = ZONENAME)) + 
    labs(title = "Crocodile Capture Zones") +
    theme_minimal()

# Step 11: Select relevant columns for feature engineering
feature_data <- merged_data %>%
    select(Latitude, Longitude, ZONE, YEAR, Month, CROCODILE_COUNT)

# View the feature data
print(head(feature_data))

# Step 12: Calculate the correlation matrix
cor_matrix <- cor(feature_data %>% select(Latitude, Longitude, CROCODILE_COUNT, Month, YEAR), use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)

# Step 13: Supervised Learning - Train Linear Regression, Random Forest, and SVM models
# Linear Regression model
linear_model <- lm(CROCODILE_COUNT ~ Latitude + Longitude + ZONE + YEAR + Month, data = feature_data)
summary(linear_model)

# Random Forest model
rf_model <- randomForest(CROCODILE_COUNT ~ Latitude + Longitude + ZONE + YEAR + Month, data = feature_data, ntree = 500)
print(rf_model)

# SVM model
svm_model <- svm(CROCODILE_COUNT ~ Latitude + Longitude + ZONE + YEAR + Month, data = feature_data)
summary(svm_model)

# Step 14: RMSE Evaluation
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Linear Regression RMSE
linear_predictions <- predict(linear_model, feature_data)
rmse_linear <- rmse(feature_data$CROCODILE_COUNT, linear_predictions)

# Random Forest RMSE
rf_predictions <- predict(rf_model, feature_data)
rmse_rf <- rmse(feature_data$CROCODILE_COUNT, rf_predictions)

# SVM RMSE
svm_predictions <- predict(svm_model, feature_data)
rmse_svm <- rmse(feature_data$CROCODILE_COUNT, svm_predictions)

# Print RMSE results
print(paste("Linear Regression RMSE:", rmse_linear))
print(paste("Random Forest RMSE:", rmse_rf))
print(paste("SVM RMSE:", rmse_svm))

# Step 15: Build Correlation Matrix
corrplot(cor_matrix, method = "color", type = "upper")

# Step 16: Residual Analysis for Linear Regression
par(mfrow = c(2, 2))
plot(linear_model)

# Step 17: Unsupervised Learning - K-Means and Time Series
# Apply K-Means clustering
set.seed(123)
kmeans_model <- kmeans(feature_data %>% select(Latitude, Longitude, CROCODILE_COUNT), centers = 3)

# Add cluster to the data
feature_data$cluster <- kmeans_model$cluster

# View the clustered data
head(feature_data)

# Prepare data for time series analysis (group by year and month)
ts_data <- feature_data %>%
  group_by(YEAR, Month) %>%
  summarize(monthly_crocodile_count = sum(CROCODILE_COUNT, na.rm = TRUE))

# Convert to time series for ARIMA (monthly frequency)
ts_data <- ts(ts_data$monthly_crocodile_count, frequency = 12)

# Fit ARIMA model and forecast future crocodile counts
arima_model <- auto.arima(ts_data)
print(arima_model)
forecast(arima_model, h = 12)

# Step 18: Create Safety Alert Based on Best Model
predicted_count <- predict(rf_model, feature_data)

# Define risk assessment
risk_assessment <- case_when(
  predicted_count < 10 ~ "Low",
  predicted_count < 50 ~ "Moderate",
  predicted_count < 100 ~ "High",
  TRUE ~ "Very High"
)

# Generate safety alert message
alert_message <- paste(
  "Safety Alert: Crocodile Encounter Risk\n",
  "Estimated number of crocodiles in the area:", round(predicted_count, 2), "\n",
  "Risk level:", risk_assessment, "\n",
  case_when(
    risk_assessment == "Low" ~ "Exercise normal caution around water bodies.",
    risk_assessment == "Moderate" ~ "Be alert near water. Avoid water activities if possible.",
    risk_assessment == "High" ~ "High danger! Stay away from water edges. Notify local authorities if you must be near water.",
    risk_assessment == "Very High" ~ "Extreme danger! Do not approach any water bodies. Seek guidance from local wildlife authorities before any activities in the area."
  )
)

# Print the alert message
print(alert_message)




