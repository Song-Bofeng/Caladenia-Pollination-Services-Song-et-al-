# This code performs a segmented regression analysis on the average pollen removal rates from 1925 to 2020.  
# It first loads and preprocesses the data, calculates the annual average pollinia removal rate, 
# and then fits both a linear regression model and a segmented regression model with a breakpoint initially set at 1970.  
# The results, including the breakpoint and slopes before and after the breakpoint are extracted and displayed


library(readxl)
library(dplyr)
library(segmented)
library(ggplot2)

# input data
file_path <- "/Users/bofeng/Downloads/Honours/数据/NewPolliniaData.xlsx"
data <- read_excel(file_path, sheet = 1)

# Data preprocessing
# Filter out data outside the range
# Ensure the pollen removal rate is numeric
data <- data %>%
  filter(Year >= 1925 & Year <= 2020) %>%
  mutate(`Pollen Removal Rate` = as.numeric(`Pollen Removal Rate`))

# Calculate the average pollen removal rate per year
annual_data <- data %>%
  group_by(Year) %>%
  summarise( # for each year
    Avg_Pollen_Removal_Rate = mean(`Pollen Removal Rate`, na.rm = TRUE)
  ) %>%
  ungroup()

# Piecewise Regression
# 1. linear
lm_model <- lm(Avg_Pollen_Removal_Rate ~ Year, data = annual_data)

# 2. Pice
seg_model <- segmented(lm_model, seg.Z = ~Year)

# summary
cat("分段回归模型结果：\n")
summary(seg_model)

# break info
breakpoint <- seg_model$psi[2]  # 提取自动拟合的断点
cat("\n自动拟合的断点为：", breakpoint, "\n")


slopes <- slope(seg_model)$Year
cat("\n断点前后的斜率分别为：\n")
cat("断点前：", slopes[1], "\n")
cat("断点后：", slopes[2], "\n")

# Seesee figure
ggplot(annual_data, aes(x = Year, y = Avg_Pollen_Removal_Rate)) +
  geom_point(color = "blue", size = 2) +
  geom_line(aes(y = fitted(seg_model)), color = "red", size = 1) +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "darkgreen", size = 1) +
  labs(
    title = "分段回归分析",
    x = "年份",
    y = "平均花粉移除率"
  ) +
  theme_minimal()
