# This code performs several steps to analyze the relationship between pollen removal rates, 
# the human footprint index (raster values), and the year.  
# The process includes data filtering, GLM modeling, and creating 3D interaction plots.  
# The model complexity is increased by introducing polynomial terms for both the year and raster values.  
# Predictions are made, visualized, and exported.

library(readxl)
library(dplyr)
library(sf)
library(raster)
library(mgcv)
library(plotly)
install.packages("writexl")
library(writexl)


# data
data_path <- "X:/NewPolliniaData.xlsx"
pollinia_data <- read_excel(data_path)

# filter
pollinia_filtered <- pollinia_data %>%
  filter(Year >= 1925 & Year <= 2020)

# Calculate Catalog Number 
pollinia_avg <- pollinia_filtered %>%
  group_by(`Catalog Number`) %>%
  summarise(
    Avg_Pollen_Removal_Rate = sum(`Pollen Removal Rate`, na.rm = TRUE) / n(),
    Latitude = first(Latitude),
    Longitude = first(Longitude),
    Year = first(Year)
  )

# Ensure data integrity
pollinia_avg <- pollinia_avg %>%
  filter(!is.na(Latitude) & !is.na(Longitude) & !is.na(Avg_Pollen_Removal_Rate))

#  sf 
pollinia_sf <- st_as_sf(pollinia_avg, coords = c("Longitude", "Latitude"), crs = 4326)

# raster data
raster_path <- "X:/Botanical Districts of Australia/hfp2017/hfp2017.tif"
raster_data <- raster(raster_path)

# Converts sf objects to the same projected coordinate system as raster data
pollinia_sf <- st_transform(pollinia_sf, crs = crs(raster_data))

# The raster value corresponding to each point is extracted and added to the data
pollinia_avg$raster_value <- extract(raster_data, st_coordinates(pollinia_sf))

# Check raster_value 
pollinia_avg <- pollinia_avg %>%
  filter(!is.na(raster_value))

# check raster_value maxvalue
summary(pollinia_avg$raster_value)

# check raster_value Distribution map
ggplot(pollinia_avg, aes(x = raster_value)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "white") +
  labs(title = "Distribution of Footprint Index (raster_value)", x = "Footprint Index", y = "Count")



# 111Flat display 3D drawings
glm_model <- glm(Avg_Pollen_Removal_Rate ~ Year * raster_value, 
                 data = pollinia_avg, 
                 family = gaussian(link = "identity"))

# Summary
summary(glm_model)

# Create the forecast data box 
# ensure that the raster_value and Year ranges are correct
new_data <- expand.grid(
  Year = seq(min(pollinia_avg$Year), max(pollinia_avg$Year), length.out = 100),
  raster_value = seq(min(pollinia_avg$raster_value), max(pollinia_avg$raster_value), length.out = 100)
)

# Generate predicted values, combined by Year and raster_value
new_data$predicted <- predict(glm_model, newdata = new_data)

# Transform the predicted values into matrices for 3D mapping
z_matrix <- matrix(new_data$predicted, nrow = 100, ncol = 100, byrow = FALSE)

# Properly ordered
year_values <- seq(min(pollinia_avg$Year), max(pollinia_avg$Year), length.out = 100)
raster_values <- seq(min(pollinia_avg$raster_value), max(pollinia_avg$raster_value), length.out = 100)

# Create 3D interaction diagrams
plot_ly(x = ~year_values, y = ~raster_values, z = ~z_matrix) %>%
  add_surface() %>%
  layout(
    title = "Interaction of Year and Footprint Index on Pollen Removal Rate",
    scene = list(
      xaxis = list(title = "Year", range = c(min(year_values), max(year_values))),
      yaxis = list(title = "Footprint Index", range = c(min(raster_values), max(raster_values))),
      zaxis = list(title = "Predicted Pollen Removal Rate")
    )
  )




# 222 Optimizations (surfaces) Increase model complexity (e.g. using polynomial or piecewise models)
glm_3model <- glm(Avg_Pollen_Removal_Rate ~ poly(Year, 2) * poly(raster_value, 2), 
                 data = pollinia_avg)

# summary
summary(glm_3model)

# fotecast data
new_data <- expand.grid(
  Year = seq(min(pollinia_avg$Year), max(pollinia_avg$Year), length.out = 100),
  raster_value = seq(min(pollinia_avg$raster_value), max(pollinia_avg$raster_value), length.out = 100)
)

# Generate predicted values
new_data$predicted <- predict(glm_3model, newdata = new_data)

# Transform the predicted values into matrices for 3D mapping
z_matrix <- matrix(new_data$predicted, nrow = 100, ncol = 100, byrow = FALSE)

# 11
year_values <- seq(min(pollinia_avg$Year), max(pollinia_avg$Year), length.out = 100)
raster_values <- seq(min(pollinia_avg$raster_value), max(pollinia_avg$raster_value), length.out = 100)

# prediction data and 3D graphics are generated again
new_data$predicted <- predict(glm_3model, newdata = new_data)

z_matrix <- matrix(new_data$predicted, nrow = 100, ncol = 100, byrow = FALSE)

# figure
plot_ly(x = ~year_values, y = ~raster_values, z = ~z_matrix) %>%
  add_surface() %>%
  layout(
    title = "Interaction of Year and Footprint Index on Pollen Removal Rate",
    scene = list(
      xaxis = list(title = "Year", range = c(min(year_values), max(year_values))),
      yaxis = list(title = "Human Footprint Index", range = c(min(raster_values), max(raster_values))),
      zaxis = list(title = "Predicted Pollen Removal Rate")
    )
  )


# make Predicted value
new_data <- expand.grid(
  Year = seq(min(pollinia_avg$Year), max(pollinia_avg$Year), length.out = 10),
  raster_value = seq(min(pollinia_avg$raster_value), max(pollinia_avg$raster_value), length.out = 10)
)

# Predicted value
new_data$predicted <- predict(glm_3model, newdata = new_data)

# print
print(new_data)

# CSV 
write.csv(new_data, file = "X:/new_data_interaction_table.csv", row.names = FALSE)


