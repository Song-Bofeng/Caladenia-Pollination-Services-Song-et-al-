#This code analyzes the changes in pollinia removal rates for all flowers from 1925 to 2020, 
#using a Generalized additive mixed models  (GAMM) to fit the trend. 
#It also generates corresponding charts to display the overall trend. 
#This part of the code is crucial to the study, 
#as it reveals the trend in pollen removal rates through time analysis of historical data, 
#providing a macro-level understanding of long-term changes.


library(readxl)
library(dplyr)
library(ggplot2)
library(mgcv)

# input dataset
# Distinguish sheet1, 2
file_path <- "/Users/bofeng/Downloads/Honours/数据/NewPolliniaData.xlsx"
data <- read_excel(file_path, sheet = 1)
subgenus_data <- read_excel(file_path, sheet = 2)

# Merge Subgenus information
data <- data %>%
  left_join(subgenus_data, by = "Species")


# Pollen Removal Rate is converted to a numeric type
data <- data %>%
  filter(Year >= 1925 & Year <= 2020) %>%
  mutate(`Pollen Removal Rate` = as.numeric(`Pollen Removal Rate`))

# In terms of years
# The average pollinia removal rate was calculated
annual_data <- data %>%
  group_by(Year) %>%
  summarise(
    Avg_Pollen_Removal_Rate = mean(`Pollen Removal Rate`, na.rm = TRUE)
  ) %>% ungroup()

# Use excel to calculate the standard error data of each year
# add the corresponding standard error data into R
standard_errors <- data.frame(
  Year = c(1925, 1926, 1928, 1930, 1931, 1936, 1937, 1939, 1944, 1946, 1947, 1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2020, 2022),
  SE_Pollen_Removal_Rate = c(0.08304548, 0.143222975, 0, 0.178174161, 0.166666667, 0.40824829, 0.112833867, 0.223606798, 0.5, 0.202030509, 0.133234678, 0.223606798, 0.353553391, 0.107279781, 0.244948974, 0.058241997, 0.07321326, 0, 0.074614598, 0.288675135, 0.090010287, 0.182574186, 0.070143802, 0.03877242, 0.061275761, 0.120761473, 0.03325936, 0.045434833, 0.042352088, 0.024678892, 0.050173749, 0.044394425, 0.034325419, 0.083092619, 0.087188359, 0.117201808, 0.050681193, 0.079135074, 0.064072148, 0.095694875, 0.17251639, 0.068138103, 0.133333333, 0.095789389, 0.067599057, 0.140859042, 0.101324561, 0.042315417, 0.034593062, 0.050977217, 0.059660054, 0.043242701, 0.025009642, 0.018786693, 0.03791682, 0.020444715, 0.03286936, 0.025239497, 0.034015889, 0.024923737, 0.033529487, 0.016494761, 0.014094187, 0.015942329, 0.015730377, 0.046106288, 0.035226034, 0.019670115, 0.022659612, 0.089845282, 0.068946628, 0.033364765, 0.162650012, 0.061858957, 0.166666667, 0.098035626, 0.074584684, 0.056535385, 0.078278036, 0.07269493, 0.15430335, 0, 0.052106824, 0.077510678)
)

annual_data <- annual_data %>%
  left_join(standard_errors, by = "Year")

# merge
data <- data %>%
  left_join(annual_data, by = "Year")

# Check Subgenus.x列
print(head(data))

# GAM
if ("Species" %in% colnames(data) & "Subgenus.x" %in% colnames(data)) {
  # 计算GAM模型，Use Nested Random Effect Structure
  model_pollen_removal <- gamm(Avg_Pollen_Removal_Rate ~ s(Year), random = list(Subgenus.x = ~1, Species = ~1|Subgenus.x), data = data)
  summary_model_pollen_removal <- summary(model_pollen_removal$gam)
  print(summary(model_pollen_removal$gam))
} else {
  print("Species或Subgenus.x列不存在，请检查数据集。")
}

# Model summary
summary_model_pollen_removal <- summary(model_pollen_removal$lme)
print(summary_model_pollen_removal)

# Figure
p <- ggplot(annual_data, aes(x = Year, y = Avg_Pollen_Removal_Rate)) +
  geom_point(position = position_dodge(0.05)) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, position = position_dodge(0.05)) +
  geom_errorbar(
    aes(ymin = Avg_Pollen_Removal_Rate - SE_Pollen_Removal_Rate,
        ymax = Avg_Pollen_Removal_Rate + SE_Pollen_Removal_Rate),
    width = 1,  # 设置误差棒的宽度为1
    position = position_dodge(0.05)
  ) +
  geom_segment(
    aes(x = Year - 0.1, xend = Year + 0.1,
        y = Avg_Pollen_Removal_Rate + SE_Pollen_Removal_Rate,
        yend = Avg_Pollen_Removal_Rate + SE_Pollen_Removal_Rate),
    size = 0  # 隐藏线条
  ) +
  geom_segment(
    aes(x = Year - 0.1, xend = Year + 0.1,
        y = Avg_Pollen_Removal_Rate - SE_Pollen_Removal_Rate,
        yend = Avg_Pollen_Removal_Rate - SE_Pollen_Removal_Rate),
    size = 0  # 隐藏线条
  ) +
  scale_x_continuous(breaks = c(seq(1925, 2010, by = 10), 2020)) +  # 设置 x 轴刻度从 1925 到 2020，并添加 2020
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +  # 设置 y 轴刻度从 0 到 1，每 0.25 一条线
  labs(
    title = "Historical Changes in Pollen Removal Rates",
    x = "Year",
    y = "Average Pollen Removal Rate"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),  # 设置背景为白色
    plot.background = element_rect(fill = "white", colour = "black", size = 2), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(colour = "black"),  # 保留X和Y
    panel.grid.major.y = element_line(colour = "grey90"),  
    plot.title = element_text(hjust = 0.5)  # 草泥马标题居中
  )

# 原神！启动！！！！
print(p)

# save
ggsave("/Users/bofeng/Downloads/Honours/Result图/NewGAM/究极超级无敌飞天螺旋Nested版GAM.png", plot = p, width = 10, height = 6, dpi = 300)



# two major periods
periods <- list(
  `1925-1970` = c(1925, 1970),
  `1970-2020` = c(1970, 2020)
)

decline_rates <- lapply(periods, function(period) {
  period_data <- annual_data %>% filter(Year >= period[1] & Year <= period[2])
  model <- lm(Avg_Pollen_Removal_Rate ~ Year, data = period_data)
  slope <- coef(model)[2]
  annual_decline_rate <- slope * 100  # 转换
  return(annual_decline_rate)
})

# Calculated decline rate
decline_rates <- do.call(rbind, decline_rates)
rownames(decline_rates) <- names(periods)
colnames(decline_rates) <- "Annual Decline Rate (%)"
print(decline_rates)

# Calculated year decline rate
overall_model <- lm(Avg_Pollen_Removal_Rate ~ Year, data = annual_data)
overall_slope <- coef(overall_model)[2]
overall_annual_decline_rate <- overall_slope * 100  # 转换为百分比形式

cat("Overall Annual Decline Rate: ", overall_annual_decline_rate, "%\n")
2


