# This code performs an analysis on the pollinia removal rates of species using the Sexual Deception from 1920 to 2020. 

library(readxl)
library(dplyr)
library(ggplot2)
library(mgcv)

# data
file_path <- "/Users/bofeng/Downloads/Honours/数据/NewPolliniaData.xlsx"
data <- read_excel(file_path, sheet = 1)
subgenus_data <- read_excel(file_path, sheet = 2)

# merge info
data <- data %>% 
  left_join(subgenus_data, by = c("Species" = "Species"))

# Filter Sexual deception
sexual_deception_data <- data %>% 
  filter(`Pollination Syndrome` == "Sexual deception")

# Convert Pollen Removal Rate to numeric and filter data for years after 1920
sexual_deception_data <- sexual_deception_data %>%
  filter(Year >= 1920 & Year <= 2020) %>%
  mutate(
    `Pollen Removal Rate` = as.numeric(`Pollen Removal Rate`)
  )

# Calculate SE
annual_data <- sexual_deception_data %>%
  group_by(Year) %>% 
  summarise(
    Avg_Pollen_Removal_Rate = mean(`Pollen Removal Rate`, na.rm = TRUE),
    SE_Pollen_Removal_Rate = sd(`Pollen Removal Rate`, na.rm = TRUE) / sqrt(n()),
    Sample_Size = n()
  ) %>% ungroup()

# adding excel calculated SE
standard_errors <- data.frame(
  Year = c(1925, 1944, 1947, 1952, 1953, 1954, 1955, 1956, 1958, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1973, 1974, 1976, 1977, 1978, 1979, 1980, 1981, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2013, 2014, 2015, 2016, 2017, 2018),
  SE_Pollen_Removal_Rate = c(0, 0.5, 0.244948974, 0, 0, 0, 0, 0.333333333, 0, 0.178174161, 0, 0.288675135, 0.111676566, 0.166666667, 0.127128345, 0.065310684, 0.108657146, 0.077086005, 0.145643816, 0, 0, 0.092998702, 0.138675049, 0.5, 0, 0.11380393, 0.2, 0.25, 0.288675135, 0, 0.111943416, 0.10083169, 0.11380393, 0.5, 0.080869237, 0.069029529, 0.056052123, 0.077128723, 0.048296107, 0.060560782, 0.082315447, 0.057166195, 0.046746602, 0.042702283, 0.029605624, 0.025963467, 0.034924278, 0.106091523, 0.11433239, 0.048498155, 0.038811757, 0.150755672, 0.142857143, 0.032801105, 0, 0.08871202, 0, 0, 0, 0.106904497, 0.166666667, 0)
)

annual_data <- annual_data %>%
  left_join(standard_errors, by = "Year")

# Ensure correct column names after merging
annual_data <- annual_data %>% 
  rename(SE_Pollen_Removal_Rate.x = SE_Pollen_Removal_Rate.x)

# back
data <- data %>% 
  left_join(annual_data, by = "Year")

# GAMM
if ("Species" %in% colnames(data) & "Subgenus.x" %in% colnames(data)) {
  model_pollen_removal <- gamm(Avg_Pollen_Removal_Rate ~ s(Year), random = list(Subgenus.x = ~1, Species = ~1 | Subgenus.x), data = data)
  summary_model_pollen_removal <- summary(model_pollen_removal$gam)
  print(summary_model_pollen_removal)
  
  # RP
  R_squared <- summary_model_pollen_removal$r.sq  # R方
  p_value <- summary_model_pollen_removal$s.table[1, 4]  # P值
  cat("R-squared: ", R_squared, "\n")
  cat("P-value: ", p_value, "\n")
  
  # sample No.
  sample_size <- sum(annual_data$Sample_Size)
  cat("Sample Size: ", sample_size, "\n")
  
  # decline No.
  years <- seq(1920, 2020)
  predicted_rates <- predict(model_pollen_removal$gam, 
                             newdata = data.frame(Year = years, Subgenus.x = unique(sexual_deception_data$Subgenus.x)[1]))
  decline_rate <- (predicted_rates[length(predicted_rates)] - predicted_rates[1]) / (2020 - 1920)
  cat("Overall Annual Decline Rate: ", decline_rate * 100, "% per year\n")
} else {
  print("Species或Subgenus.x列不存在，请检查数据集。")
}

# figure
p <- ggplot(annual_data, aes(x = Year, y = Avg_Pollen_Removal_Rate)) +
  geom_point(position = position_dodge(0.05)) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, 
              color = "#8da0cb", size = 4, position = position_dodge(0.05)) +
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
  scale_x_continuous(breaks = seq(1920, 2020, by = 10)) +  # 设置 x 轴刻度从 1920 到 2020
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

# 保存图形
ggsave("/Users/bofeng/Downloads/Honours/Result图/Pollination Syndrome/NewGAM/211SexualDeceptionPollenRemovalRateGAMFig2.png", plot = p, width = 20, height = 10, dpi = 600)

