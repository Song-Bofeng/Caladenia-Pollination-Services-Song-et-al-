# This code processes and analyzes the pollen removal rates for different pollinator groups (Wasps, Bees, and Flies) over two time periods (1925-1975 and 1975-2020).

# data
file_path <- "/Users/bofeng/Downloads/Honours/数据/NewPolliniaData.xlsx"
data <- read_excel(file_path, sheet = 1)
pollinator_info_path <- "/Users/bofeng/Downloads/Honours/Result图/Pollinator/Pollinator-Info.xlsx"
pollinator_data <- read_excel(pollinator_info_path)

# merge
data <- data %>%
  left_join(pollinator_data, by = c("Species" = "Species"))

# filter
data <- data %>%
  filter(Year >= 1925 & Year <= 2020)

# SE
standard_errors <- data.frame(
  Year = c(1925, 1926, 1928, 1930, 1931, 1936, 1937, 1939, 1944, 1946, 1947, 1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2020, 2022),
  SE_Pollen_Removal_Rate = c(0.08304548, 0.143222975, 0, 0.178174161, 0.166666667, 0.40824829, 0.112833867, 0.223606798, 0.5, 0.202030509, 0.133234678, 0.223606798, 0.353553391, 0.107279781, 0.244948974, 0.058241997, 0.07321326, 0, 0.074614598, 0.288675135, 0.090010287, 0.182574186, 0.070143802, 0.03877242, 0.061275761, 0.120761473, 0.03325936, 0.045434833, 0.042352088, 0.024678892, 0.050173749, 0.044394425, 0.034325419, 0.083092619, 0.087188359, 0.117201808, 0.050681193, 0.079135074, 0.064072148, 0.095694875, 0.17251639, 0.068138103, 0.133333333, 0.095789389, 0.067599057, 0.140859042, 0.101324561, 0.042315417, 0.034593062, 0.050977217, 0.059660054, 0.043242701, 0.025009642, 0.018786693, 0.03791682, 0.020444715, 0.03286936, 0.025239497, 0.034015889, 0.024923737, 0.033529487, 0.016494761, 0.014094187, 0.015942329, 0.015730377, 0.046106288, 0.035226034, 0.019670115, 0.022659612, 0.089845282, 0.068946628, 0.033364765, 0.162650012, 0.061858957, 0.166666667, 0.098035626, 0.074584684, 0.056535385, 0.078278036, 0.07269493, 0.15430335, 0, 0.052106824, 0.077510678)
)

# Calculate
annual_data <- data %>%
  group_by(Year) %>%
  summarise(
    Avg_Pollen_Removal_Rate = mean(`Pollen Removal Rate`, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(standard_errors, by = "Year")

# Add standard error data to the annual data
data <- data %>%
  left_join(annual_data, by = "Year")

# Filter and calculate the GAM model for Wasps
wasps_species <- pollinator_data %>%
  filter(`Categrories 1` == "Wasps" | `Categrories 2` == "Wasps") %>%
  pull(Species)
wasps_data <- data %>%
  filter(Species %in% wasps_species)

# Calculate Wasps
wasps_gam <- gamm(Avg_Pollen_Removal_Rate ~ s(Year), random = list(Subgenus = ~1, Species = ~1|Subgenus), data = wasps_data)

# Filter and calculate the GAM model for bees
bees_species <- pollinator_data %>%
  filter(`Categrories 1` == "Bees" | `Categrories 2` == "Bees") %>%
  pull(Species)

bees_data <- data %>%
  filter(Species %in% bees_species)

#  Calculate  Bees 
bees_gam <- gamm(Avg_Pollen_Removal_Rate ~ s(Year), random = list(Subgenus = ~1, Species = ~1|Subgenus), data = bees_data)

# Filter and calculate the GAM model for flies
flies_species <- pollinator_data %>%
  filter(`Categrories 1` == "Flies" | `Categrories 2` == "Flies") %>%
  pull(Species)

flies_data <- data %>%
  filter(Species %in% flies_species)

# Calculate  flies
flies_gam <- gamm(Avg_Pollen_Removal_Rate ~ s(Year), random = list(Subgenus = ~1, Species = ~1|Subgenus), data = flies_data)

# Calculate the overall
overall_annual_data <- data %>% 
  group_by(Year) %>% 
  summarise(
    Avg_Pollen_Removal_Rate = mean(`Pollen Removal Rate`, na.rm = TRUE),
    SE_Pollen_Removal_Rate = sd(`Pollen Removal Rate`, na.rm = TRUE) / sqrt(n())
  ) %>% ungroup()

# Plot the three GAM models for different pollinator groups and add the overall average scatter plot and error bars
p <- ggplot() +
 
  geom_point(data = overall_annual_data, aes(x = Year, y = Avg_Pollen_Removal_Rate), 
             color = "grey35", size = 2) +  
  geom_errorbar(data = overall_annual_data, aes(x = Year, ymin = Avg_Pollen_Removal_Rate - SE_Pollen_Removal_Rate, ymax = Avg_Pollen_Removal_Rate + SE_Pollen_Removal_Rate), 
                width = 1, color = "grey35") +  
  
  # Wasps 
  geom_smooth(data = wasps_data, aes(x = Year, y = Avg_Pollen_Removal_Rate), 
              method = "gam", formula = y ~ s(x), se = TRUE, color = "#CA0E12", size = 1.5) +
  
  # Bees 
  geom_smooth(data = bees_data, aes(x = Year, y = Avg_Pollen_Removal_Rate), 
              method = "gam", formula = y ~ s(x), se = TRUE, color = "#25377F", size = 1.5) +
  
  # Flies 
  geom_smooth(data = flies_data, aes(x = Year, y = Avg_Pollen_Removal_Rate), 
              method = "gam", formula = y ~ s(x), se = TRUE, color = "#F6BD21", size = 1.5) +
  

  scale_x_continuous(breaks = c(seq(1925, 2010, by = 10), 2020)) + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) + 
  

  labs(
    title = "Historical Changes in Pollen Removal Rates by Pollinator Category",
    x = "Year",
    y = "Average Pollen Removal Rate"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),  
    panel.grid.major = element_blank(),  
    plot.background = element_rect(fill = "white", colour = "black", size = 1), 
    panel.grid.minor = element_blank(),  
    axis.line = element_line(colour = "black"),
    panel.grid.major.y = element_line(colour = "grey90"),  
    plot.title = element_text(hjust = 0.5)  
  )

# 原神！启动！！！！
print(p)

# save
ggsave("/Users/bofeng/Downloads/Honours/Result图/Pollinator/三组对比.png", plot = p, width = 10, height = 6, dpi = 300)



# The following code is about calculating specific data and is not included in the main part of this code
#load
library(readxl)
library(dplyr)
library(ggplot2)
library(mgcv)
library(segmented)

# Load data
file_path <- "/Users/bofeng/Downloads/Honours/数据/NewPolliniaData.xlsx"
data <- read_excel(file_path, sheet = 1)

pollinator_info_path <- "/Users/bofeng/Downloads/Honours/Result图/Pollinator/Pollinator-Info.xlsx"
pollinator_data <- read_excel(pollinator_info_path)

# Merge pollinator information
data <- data %>%
  left_join(pollinator_data, by = "Species") %>%
  filter(Year >= 1925 & Year <= 2020) %>%
  mutate(`Pollen Removal Rate` = as.numeric(`Pollen Removal Rate`))

# Define time periods
periods <- list(
  `1925-1975` = c(1925, 1975),
  `1975-2020` = c(1975, 2020)
)

# Prepare pollinator groups
wasps_species <- pollinator_data %>%
  filter(`Categrories 1` == "Wasps" | `Categrories 2` == "Wasps") %>%
  pull(Species)

bees_species <- pollinator_data %>%
  filter(`Categrories 1` == "Bees" | `Categrories 2` == "Bees") %>%
  pull(Species)

flies_species <- pollinator_data %>%
  filter(`Categrories 1` == "Flies" | `Categrories 2` == "Flies") %>%
  pull(Species)

# Function to run segmented regression for each pollinator group and period
run_segmented_regression <- function(data, species_group, period_name, period_range) {
  period_data <- data %>%
    filter(Species %in% species_group, Year >= period_range[1], Year <= period_range[2])
  
  # Fit the linear model first
  lm_model <- lm(Avg_Pollen_Removal_Rate ~ Year, data = period_data)
  
  # Apply segmented regression
  seg_model <- segmented(lm_model, seg.Z = ~Year, psi = period_range[1] + (period_range[2] - period_range[1]) / 2)
  
  # Output the segmented regression summary
  summary(seg_model)
  
  # Extract slope of each segment and statistical significance
  slopes <- slope(seg_model)$Year
  p_values <- summary(seg_model)$coefficients[, 4]
  
  cat("\nSegmented Regression for", period_name, "\n")
  cat("Slope 1:", slopes[1], "\n")
  cat("Slope 2:", slopes[2], "\n")
  cat("P-value 1:", p_values[1], "\n")
  cat("P-value 2:", p_values[2], "\n")
}

# 确保每年的平均花粉移除率被计算并合并回原数据

# 1. 计算每年的平均花粉移除率
annual_data <- data %>%
  group_by(Year) %>%
  summarise(
    Avg_Pollen_Removal_Rate = mean(`Pollen Removal Rate`, na.rm = TRUE),
    SE_Pollen_Removal_Rate = sd(`Pollen Removal Rate`, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

# 2. 将 annual_data 合并回原始 data
data <- data %>%
  left_join(annual_data, by = "Year")

# 3. 检查数据中是否存在 Avg_Pollen_Removal_Rate 列
if ("Avg_Pollen_Removal_Rate" %in% colnames(data)) {
  print("Avg_Pollen_Removal_Rate 列存在")
} else {
  print("Avg_Pollen_Removal_Rate 列不存在，请检查数据处理步骤")
}

# Run segmented regression for each pollinator group
# 1925-1975 period
run_segmented_regression(data, wasps_species, "Wasps (1925-1975)", periods[["1925-1975"]])
run_segmented_regression(data, bees_species, "Bees (1925-1975)", periods[["1925-1975"]])
run_segmented_regression(data, flies_species, "Flies (1925-1975)", periods[["1925-1975"]])

# 1975-2020 period
run_segmented_regression(data, wasps_species, "Wasps (1975-2020)", periods[["1975-2020"]])
run_segmented_regression(data, bees_species, "Bees (1975-2020)", periods[["1975-2020"]])
run_segmented_regression(data, flies_species, "Flies (1975-2020)", periods[["1975-2020"]])

# Statistical comparison between pollinator groups using ANOVA or Kruskal-Wallis
# ANOVA (for normally distributed data) or Kruskal-Wallis (for non-normal data)
# Compare the Pollen Removal Rates of the three groups in each period
anova_test <- function(data, period_name, period_range) {
  period_data <- data %>%
    filter(Year >= period_range[1], Year <= period_range[2], !is.na(`Pollen Removal Rate`))
  
  period_data <- period_data %>%
    mutate(Pollinator_Group = case_when(
      Species %in% wasps_species ~ "Wasps",
      Species %in% bees_species ~ "Bees",
      Species %in% flies_species ~ "Flies"
    ))
  
  # 试试
  kruskal_result <- kruskal.test(`Pollen Removal Rate` ~ Pollinator_Group, data = period_data)
  cat("\nKruskal-Wallis Test for", period_name, "\n")
  print(kruskal_result)
}

# Kruskal-Wallis test for each period
anova_test(data, "1925-1975", periods[["1925-1975"]])
anova_test(data, "1975-2020", periods[["1975-2020"]])


# 计算1925-1975年的样本数量
n_1925_1975 <- data %>%
  filter(Year >= 1925 & Year <= 1975) %>%
  summarise(N = n())

# 打印1925-1975年的样本数量
cat("1925-1975年样本数量:", n_1925_1975$N, "\n")

# 计算1975-2020年的样本数量
n_1975_2020 <- data %>%
  filter(Year >= 1975 & Year <= 2020) %>%
  summarise(N = n())

# 打印1975-2020年的样本数量
cat("1975-2020年样本数量:", n_1975_2020$N, "\n")



# 加载必要的库
library(readxl)
library(dplyr)
library(mgcv)

# 定义时间段
periods <- list(
  `1925-1975` = c(1925, 1975),
  `1975-2020` = c(1975, 2020)
)

# 准备数据
pollinator_info_path <- "/Users/bofeng/Downloads/Honours/Result图/Pollinator/Pollinator-Info.xlsx"
pollinator_data <- read_excel(pollinator_info_path)

# 定义传粉者群体
wasps_species <- pollinator_data %>%
  filter(`Categrories 1` == "Wasps" | `Categrories 2` == "Wasps") %>%
  pull(Species)

bees_species <- pollinator_data %>%
  filter(`Categrories 1` == "Bees" | `Categrories 2` == "Bees") %>%
  pull(Species)

flies_species <- pollinator_data %>%
  filter(`Categrories 1` == "Flies" | `Categrories 2` == "Flies") %>%
  pull(Species)

# 计算 GAM 模型自由度函数
calculate_gam_df <- function(data, species_group, period_name, period_range) {
  period_data <- data %>%
    filter(Species %in% species_group, Year >= period_range[1], Year <= period_range[2])
  
  # 计算 GAM 模型
  gam_model <- gam(Avg_Pollen_Removal_Rate ~ s(Year), data = period_data)
  
  # 输出模型摘要中的自由度 (edf)
  edf <- summary(gam_model)$s.table[, "edf"]
  cat("\nGAM model for", period_name, "(", length(species_group), "species)", "\n")
  cat("edf:", edf, "\n")
}

# 加载数据
file_path <- "/Users/bofeng/Downloads/Honours/数据/NewPolliniaData.xlsx"
data <- read_excel(file_path, sheet = 1) %>%
  left_join(pollinator_data, by = "Species") %>%
  filter(Year >= 1925 & Year <= 2020) %>%
  mutate(`Pollen Removal Rate` = as.numeric(`Pollen Removal Rate`))

# 计算每年的平均花粉移除率并合并回原始数据
annual_data <- data %>%
  group_by(Year) %>%
  summarise(
    Avg_Pollen_Removal_Rate = mean(`Pollen Removal Rate`, na.rm = TRUE)
  ) %>%
  ungroup()

data <- data %>%
  left_join(annual_data, by = "Year")

# 1925-1975 和 1975-2020 时期分别计算不同传粉者群体的 GAM 模型
# 1925-1975
calculate_gam_df(data, wasps_species, "Wasps (1925-1975)", periods[["1925-1975"]])
calculate_gam_df(data, bees_species, "Bees (1925-1975)", periods[["1925-1975"]])
calculate_gam_df(data, flies_species, "Flies (1925-1975)", periods[["1925-1975"]])

# 1975-2020
calculate_gam_df(data, wasps_species, "Wasps (1975-2020)", periods[["1975-2020"]])
calculate_gam_df(data, bees_species, "Bees (1975-2020)", periods[["1975-2020"]])
calculate_gam_df(data, flies_species, "Flies (1975-2020)", periods[["1975-2020"]])
