# This code performs a segmented regression analysis to examine the relationship between the average pollen removal rate and year for species with Food Deception pollination syndrome, 
# using data after 1930.  It fits an initial linear regression model, applies a segmented regression with a breakpoint set at 1980, 
# and visualizes the results with a plot.  The model summary, segment slopes, breakpoint, and other statistical metrics (R-squared, AIC, BIC) are extracted and displayed.

library(readxl)
library(dplyr)
library(segmented)
library(ggplot2)

# Data
file_path <- "/Users/bofeng/Downloads/Honours/数据/NewPolliniaData.xlsx"
data <- read_excel(file_path, sheet = 1)
subgenus_data <- read_excel(file_path, sheet = 2)

# merge
data <- data %>%
  left_join(subgenus_data, by = c("Species" = "Species"))

# filter
food_deception_data <- data %>%
  filter(`Pollination Syndrome` == "Food deception") %>%
  filter(Year > 1930 & Year <= 2020) %>% # 剔除1930年及以前的数据
  mutate(`Pollen Removal Rate` = as.numeric(`Pollen Removal Rate`))

# Calculate the average Pollen Removal Rate and SE
annual_data <- food_deception_data %>%
  group_by(Year) %>%
  summarise(
    Avg_Pollen_Removal_Rate = mean(`Pollen Removal Rate`, na.rm = TRUE),
    SE_Pollen_Removal_Rate = sd(`Pollen Removal Rate`, na.rm = TRUE) / sqrt(n()),
    Sample_Size = n()
  ) %>%
  ungroup()

# Piecewise Regression
# liner
lm_model <- lm(Avg_Pollen_Removal_Rate ~ Year, data = annual_data)

# Piecewise Regression
seg_model <- segmented(lm_model, seg.Z = ~Year)

# print summary 
summary(seg_model)

# Extract the slopes for each segment of the regression
slopes <- slope(seg_model)$Year
cat("Segment Slopes:\n")
print(slopes)

# breakpoint
breakpoint <- seg_model$psi
cat("Breakpoint (Year):", breakpoint[, "Est."], "\n")

# info
R_squared <- summary(seg_model)$r.squared
AIC_value <- AIC(seg_model)
BIC_value <- BIC(seg_model)

cat("R-squared: ", R_squared, "\n")
cat("AIC: ", AIC_value, "\n")
cat("BIC: ", BIC_value, "\n")

# check
fitted_values <- predict(seg_model, newdata = data.frame(Year = annual_data$Year))

# double check
if (length(fitted_values) == nrow(annual_data)) {
  # 将拟合值与年度数据对齐
  annual_data$fitted <- fitted_values
} else {
  stop("Mismatch between fitted values and annual data rows.")
}

# seesee
p <- ggplot(annual_data, aes(x = Year, y = Avg_Pollen_Removal_Rate)) +
  geom_point(color = "blue", size = 2) +
  geom_line(aes(y = fitted), color = "red", size = 1) +  # 使用重新对齐的拟合值
  geom_vline(xintercept = breakpoint[, "Est."], color = "green", linetype = "dashed", size = 1) +
  labs(
    title = "Segmented Regression: Food Deception (Post-1930)",
    x = "Year",
    y = "Average Pollen Removal Rate"
  ) +
  theme_minimal()

# print
print(p)
