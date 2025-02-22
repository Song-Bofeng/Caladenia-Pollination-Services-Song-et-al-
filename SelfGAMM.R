# # This code performs an analysis on the pollinia removal rates of species using the self pollination from 1980 to 2020. 

library(readxl)
library(dplyr)
library(ggplot2)
library(mgcv)

# data
file_path <- "/Users/bofeng/Downloads/Honours/数据/NewPolliniaData.xlsx"
data <- read_excel(file_path, sheet = 1)
subgenus_data <- read_excel(file_path, sheet = 2)


# merge
data <- data %>% 
  left_join(subgenus_data, by = c("Species" = "Species"))

# filter Self-pollination and 1980 befro
self_pollination_data <- data %>% 
  filter(`Pollination Syndrome` == "Self-pollination" & Year >= 1980)

# Convert Pollina Removal Rate to numeric and filter data for years after 1980
self_pollination_data <- self_pollination_data %>% 
  filter(Year >= 1980) %>% 
  mutate(`Pollen Removal Rate` = as.numeric(`Pollen Removal Rate`))

# Calculate SE
annual_data <- self_pollination_data %>%
  group_by(Year) %>%
  summarise(
    Avg_Pollen_Removal_Rate = mean(`Pollen Removal Rate`, na.rm = TRUE),
    SE_Pollen_Removal_Rate = sd(`Pollen Removal Rate`, na.rm = TRUE) / sqrt(n()),
    Sample_Size = n()
  ) %>% ungroup()

# adding excel calculated SE
standard_errors <- data.frame(
  Year = c(1984, 1985, 1986, 1989, 1990, 1992, 1994, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005, 2006, 2007, 2010, 2012),
  SE_Pollen_Removal_Rate = c(0.5, 0.163663418, 0, 0.125, 0.098692754, 0.077475164, 0.094987138, 0.121967344, 0.109561368, 0.104972776, 0.146986184, 0, 0.067343503, 0.121967344, 0.333333333, 0.333333333, 0.152752523, 0.333333333, 0, 0.5)
)
annual_data <- annual_data %>% 
  left_join(standard_errors, by = "Year")

# Ensure correct column names after merging
annual_data <- annual_data %>% 
  rename(SE_Pollen_Removal_Rate.x = SE_Pollen_Removal_Rate.x)

# Merge the annual data back into the main dataset
data <- data %>% 
  left_join(annual_data, by = "Year")

# gamm
if ("Species" %in% colnames(data) & "Subgenus.x" %in% colnames(data)) {
  model_pollen_removal <- gamm(Avg_Pollen_Removal_Rate ~ s(Year), random = list(Subgenus.x = ~1, Species = ~1 | Subgenus.x), data = data)
  summary_model_pollen_removal <- summary(model_pollen_removal$gam)
  print(summary_model_pollen_removal)
} else {
  print("Species或Subgenus.x列不存在，请检查数据集。")
}

# Summary
summary_model_pollen_removal <- summary(model_pollen_removal$lme)
print(summary_model_pollen_removal)

# sample number
sample_size <- sum(annual_data$Sample_Size)
cat("Sample Size: ", sample_size, "\n")

# decline number
years <- seq(1920, 2020)
predicted_rates <- predict(model_pollen_removal$gam, newdata = data.frame(Year = years, Subgenus.x = unique(self_pollination_data$Subgenus.x)[1]))  # 假设Subgenus.x为第一个Subgenus值
decline_rate <- (predicted_rates[length(predicted_rates)] - predicted_rates[1]) / (2020 - 1920)
cat("Overall Annual Decline Rate: ", decline_rate * 100, "% per year\n")

# figure
p <- ggplot(annual_data, aes(x = Year, y = Avg_Pollen_Removal_Rate)) +
  geom_point(position = position_dodge(0.05)) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, 
              color = "#a6d854", size = 4, position = position_dodge(0.05)) +
  geom_errorbar(
    aes(ymin = Avg_Pollen_Removal_Rate - SE_Pollen_Removal_Rate.x, ymax = Avg_Pollen_Removal_Rate + SE_Pollen_Removal_Rate.x), 
    width = 1,  # 设置误差棒的宽度为1
    position = position_dodge(0.05)
  ) +
  geom_segment(
    aes(x = Year - 0.1, xend = Year + 0.1, y = Avg_Pollen_Removal_Rate + SE_Pollen_Removal_Rate.x, yend = Avg_Pollen_Removal_Rate + SE_Pollen_Removal_Rate.x),
    size = 0  # 隐藏线条
  ) +
  geom_segment(
    aes(x = Year - 0.1, xend = Year + 0.1, y = Avg_Pollen_Removal_Rate - SE_Pollen_Removal_Rate.x, yend = Avg_Pollen_Removal_Rate - SE_Pollen_Removal_Rate.x),
    size = 0  # 隐藏线条
  ) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 10)) +  # 设置 x 轴刻度从 1920 到 2020
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +       # 设置 y 轴刻度从 0 到 1，每 0.25 一条线
  labs(
    title = "Historical Changes in Pollen Removal Rates (Sexual Deception)",
    x = "Year",
    y = "Average Pollen Removal Rate"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),  # 设置背景为白色
    plot.background = element_rect(fill = "white", colour = "black", size = 2),   # 设置绘图区域背景为白色并加粗黑框
    panel.grid.major = element_blank(),  # 移除主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线
    axis.line = element_line(colour = "black"),  # 保留X轴和Y轴的线
    panel.grid.major.y = element_line(colour = "grey90"),  # 保留Y轴的主要网格线
    plot.title = element_text(hjust = 0.5)  # 设置标题居中
  )

# print
print(p)

# save
ggsave("/Users/bofeng/Downloads/Honours/Result图/Pollination Syndrome/NewGAM/211SelfPollinationPollenRemovalRateGAMFig2.png", plot = p, width = 20, height = 10, dpi = 600)
