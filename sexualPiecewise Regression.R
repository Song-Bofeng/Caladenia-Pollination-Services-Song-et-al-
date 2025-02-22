# This code is about Sexual deception Piecewise Regression


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

# fitter
sexual_deception_data <- data %>%
  filter(`Pollination Syndrome` == "Sexual deception") %>%
  filter(Year >= 1920 & Year <= 2020) %>%
  mutate(`Pollen Removal Rate` = as.numeric(`Pollen Removal Rate`))

# Calculate the average Pollen Removal Rate and SE
annual_data <- sexual_deception_data %>%
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

# summary
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

# figure
ggplot(annual_data, aes(x = Year, y = Avg_Pollen_Removal_Rate)) +
  geom_point(color = "blue", size = 2) +
  geom_line(aes(y = fitted(seg_model)), color = "red", size = 1) +
  geom_vline(xintercept = breakpoint[, "Est."], color = "green", linetype = "dashed", size = 1) +
  labs(
    title = "Segmented Regression: Sexual Deception",
    x = "Year",
    y = "Average Pollen Removal Rate"
  ) +
  theme_minimal()

# print
print(p)

