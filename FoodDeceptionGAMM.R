# This code analyzes the pollen removal rates for species using the Food Deception over the period from 1920 to 2020.  
# The analysis includes data preprocessing, calculation of average rates, and standard errors, 
# followed by fitting a GAMM to capture temporal trends.  
# The results are visualized with a smooth regression curve, error bars, and additional plot details.  
# The model output, including R-squared and p-values, is also reported.

library(readxl)
library(dplyr)
library(ggplot2)
library(mgcv)

# Data
file_path <- "/Users/bofeng/Downloads/Honours/数据/NewPolliniaData.xlsx"
data <- read_excel(file_path, sheet = 1)
subgenus_data <- read_excel(file_path, sheet = 2)

# Merge the Subgenus and Pollination Syndrome data by Species
data <- data %>% 
  left_join(subgenus_data, by = c("Species" = "Species"))

# Filter Food deception species
food_deception_data <- data %>% 
  filter(`Pollination Syndrome` == "Food deception")

# Convert Pollinia Removal Rate to numeric and filter data for years after 1920
food_deception_data <- food_deception_data %>% 
  filter(Year >= 1920 & Year <= 2020) %>% 
  mutate(`Pollen Removal Rate` = as.numeric(`Pollen Removal Rate`))

# Calculate
annual_data <- food_deception_data %>% 
  group_by(Year) %>% 
  summarise(
    Avg_Pollen_Removal_Rate = mean(`Pollen Removal Rate`, na.rm = TRUE),
    SE_Pollen_Removal_Rate = sd(`Pollen Removal Rate`, na.rm = TRUE) / sqrt(n()),
    Sample_Size = n()
  ) %>% ungroup()

# Adding pre-calculated (Excel) standard error
standard_errors <- data.frame(
  Year = c(1925, 1926, 1928, 1930, 1931, 1936, 1937, 1939, 1944, 1946, 1947, 1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2020, 2022),
  SE_Pollen_Removal_Rate = c(0.08304548, 0.143222975, 0, 0.178174161, 0.166666667, 0.40824829, 0.112833867, 0.223606798, 0.5, 0.202030509, 0.133234678, 0.223606798, 0.353553391, 0.107279781, 0.244948974, 0.058241997, 0.07321326, 0, 0.074614598, 0.288675135, 0.090010287, 0.182574186, 0.070143802, 0.03877242, 0.061275761, 0.120761473, 0.03325936, 0.045434833, 0.042352088, 0.024678892, 0.050173749, 0.044394425, 0.034325419, 0.083092619, 0.087188359, 0.117201808, 0.050681193, 0.079135074, 0.064072148, 0.095694875, 0.17251639, 0.068138103, 0.133333333, 0.095789389, 0.067599057, 0.140859042, 0.101324561, 0.042315417, 0.034593062, 0.050977217, 0.059660054, 0.043242701, 0.025009642, 0.018786693, 0.03791682, 0.020444715, 0.03286936, 0.025239497, 0.034015889, 0.024923737, 0.033529487, 0.016494761, 0.014094187, 0.015942329, 0.015730377, 0.046106288, 0.035226034, 0.019670115, 0.022659612, 0.089845282, 0.068946628, 0.033364765, 0.162650012, 0.061858957, 0.166666667, 0.098035626, 0.074584684, 0.056535385, 0.078278036, 0.07269493, 0.15430335, 0, 0.052106824, 0.077510678)
)
annual_data <- annual_data %>% 
  left_join(standard_errors, by = "Year")

# go back
data <- data %>% 
  left_join(annual_data, by = "Year")

# fit model 
if ("Species" %in% colnames(data) & "Subgenus.x" %in% colnames(data)) {
  model_pollen_removal <- gamm(Avg_Pollen_Removal_Rate ~ s(Year), random = list(Subgenus.x = ~1, Species = ~1 | Subgenus.x), data = data)
  summary_model_pollen_removal <- summary(model_pollen_removal$gam)
  print(summary_model_pollen_removal)
} else {
  print("Species或Subgenus.x列不存在，请检查数据集。")
}

# ！！！summary model
summary_model_pollen_removal <- summary(model_pollen_removal$lme)
print(summary_model_pollen_removal)

# see
sample_size <- sum(annual_data$Sample_Size)
cat("Sample Size: ", sample_size, "\n")

# Calculate decline rate
years <- seq(1920, 2020)
predicted_rates <- predict(model_pollen_removal$gam, newdata = data.frame(Year = years, Subgenus.x = unique(food_deception_data$Subgenus.x)[1]))  # 假设Subgenus.x为第一个Subgenus值
decline_rate <- (predicted_rates[length(predicted_rates)] - predicted_rates[1]) / (2020 - 1920)
cat("Overall Annual Decline Rate: ", decline_rate * 100, "% per year\n")

# 修复问题 -(列名，确保正确引用
annual_data <- annual_data %>% 
  left_join(standard_errors, by = "Year")

# double check name
annual_data <- annual_data %>% 
  rename(SE_Pollen_Removal_Rate.x = SE_Pollen_Removal_Rate.x)

# Refit GAMM model
if ("Species" %in% colnames(data) & "Subgenus.x" %in% colnames(data)) {
  model_pollen_removal <- gamm(Avg_Pollen_Removal_Rate ~ s(Year), random = list(Subgenus.x = ~1, Species = ~1 | Subgenus.x), data = data)
  summary_model_pollen_removal <- summary(model_pollen_removal$gam)
  print(summary_model_pollen_removal)
} else {
  print("Species或Subgenus.x列不存在，请检查数据集。")
}

# R / P
R_squared <- summary_model_pollen_removal$r.sq  
p_value <- summary_model_pollen_removal$s.table[1, 4]  
cat("R-squared: ", R_squared, "\n")
cat("P-value: ", p_value, "\n")

# Figure
p <- ggplot(annual_data, aes(x = Year, y = Avg_Pollen_Removal_Rate)) +
  geom_point(position = position_dodge(0.05)) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, 
              color = "#fc8d62", size = 4, position = position_dodge(0.05)) +  
  geom_errorbar(
    aes(ymin = Avg_Pollen_Removal_Rate - SE_Pollen_Removal_Rate,
        ymax = Avg_Pollen_Removal_Rate + SE_Pollen_Removal_Rate),
    width = 1,  # 设置误差棒的宽度为1
    position = position_dodge(0.05)
  ) +
  geom_segment(
    aes(x = Year - 0.1, xend = Year + 0.1, y = Avg_Pollen_Removal_Rate + SE_Pollen_Removal_Rate, yend = Avg_Pollen_Removal_Rate + SE_Pollen_Removal_Rate),
    size = 0  
  ) +
  geom_segment(
    aes(x = Year - 0.1, xend = Year + 0.1, y = Avg_Pollen_Removal_Rate - SE_Pollen_Removal_Rate, yend = Avg_Pollen_Removal_Rate - SE_Pollen_Removal_Rate),
    size = 0  
  ) +
  scale_x_continuous(breaks = seq(1920, 2020, by = 10)) + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +  
  labs(
    title = "Historical Changes in Pollen Removal Rates (Food Deception)",
    x = "Year",
    y = "Average Pollen Removal Rate"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),  
    plot.background = element_rect(fill = "white", colour = "black", size = 2),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(colour = "black"),  
    panel.grid.major.y = element_line(colour = "grey90"),  
    plot.title = element_text(hjust = 0.5)  
  ) +
  annotate("text", x = max(annual_data$Year) - 10, y = max(annual_data$Avg_Pollen_Removal_Rate) - 0.05, 
           label = paste("P-value: ", format(p_value, digits = 3)), 
           size = 4, color = "black", hjust = 1)

# print
print(p)

# save
ggsave("/Users/bofeng/Downloads/Honours/Result图/Pollination Syndrome/NewGAM/FoodDeceptionPollenRemovalRateGAMFig2.png", plot = p, width = 20, height = 10, dpi = 600)

