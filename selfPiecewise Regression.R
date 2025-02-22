# # This code is about self pollination Piecewise Regression

library(readxl)
library(dplyr)
library(segmented)
library(ggplot2)

# data
file_path <- "/Users/bofeng/Downloads/Honours/数据/NewPolliniaData.xlsx"
data <- read_excel(file_path, sheet = 1)
subgenus_data <- read_excel(file_path, sheet = 2)

# merge
data <- data %>%
  left_join(subgenus_data, by = c("Species" = "Species"))

# filter
self_pollination_data <- data %>%
  filter(`Pollination Syndrome` == "Self-pollination" & Year >= 1980) %>%
  mutate(`Pollen Removal Rate` = as.numeric(`Pollen Removal Rate`))

# Calculate the average Pollen Removal Rate and SE
annual_data <- self_pollination_data %>%
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
seg_model <- segmented(lm_model, seg.Z = ~Year) # 初始断点假设为1995年

# print summary 
summary(seg_model)

# decliine
slopes <- slope(seg_model)$Year
cat("Segment Slopes:\n")
print(slopes)

# breakpoint
breakpoint <- seg_model$psi
cat("Breakpoint (Year):", breakpoint[, "Est."], "\n")

# see
plot(annual_data$Year, annual_data$Avg_Pollen_Removal_Rate, pch = 16, col = "blue",
     xlab = "Year", ylab = "Average Pollen Removal Rate",
     main = "Segmented Regression: Self-Pollination")
lines(annual_data$Year, fitted(seg_model), col = "red", lwd = 2)
abline(v = breakpoint[, "Est."], col = "green", lwd = 2, lty = 2)
legend("topright", legend = c("Observed", "Fitted", "Breakpoint"),
       col = c("blue", "red", "green"), pch = c(16, NA, NA), lty = c(NA, 1, 2))

# info
cat("R-squared:", summary(seg_model)$r.squared, "\n")
cat("AIC:", AIC(seg_model), "\n")
cat("BIC:", BIC(seg_model), "\n")
