# This code analyzes the relationship between rainfall and pollen removal rates, focusing on the years 1920 to 2020. 


library(readxl)
library(dplyr)
library(ggplot2)
library(mgcv)

# pollen data
file_path <- "/Users/bofeng/Downloads/Honours/数据/NewPolliniaData.xlsx"
data <- read_excel(file_path, sheet = 1)
subgenus_data <- read_excel(file_path, sheet = 2)

# rainfall data
rainfall_file_path <- "/Users/bofeng/Downloads/Honours/Result图/降雨/1920-2020降雨数据.csv"
rainfall_data <- read.csv(rainfall_file_path)

# merge Subgenus 
data <- data %>% 
  left_join(subgenus_data, by = c("Species" = "Species"))

# merge rainfall data
data <- data %>%
  left_join(rainfall_data, by = c("Year" = "Year"))

# Filter and convert
data <- data %>%
  filter(Year >= 1920 & Year <= 2020) %>%  
  mutate(
    `Pollen Removal Rate` = as.numeric(`Pollen Removal Rate`)
  )

# Calculate average and SE
annual_data <- data %>%
  group_by(Year) %>%
  summarise(
    Avg_Pollen_Removal_Rate = mean(`Pollen Removal Rate`, na.rm = TRUE),
    SE_Pollen_Removal_Rate = sd(`Pollen Removal Rate`, na.rm = TRUE) / sqrt(n()),
    Rainfall = mean(Rainfall, na.rm = TRUE),
    Sample_Size = n()
  ) %>% ungroup()

# Limit the maximum length of error bars
max_se <- 0.05  
annual_data <- annual_data %>%
  mutate(SE_Pollen_Removal_Rate = ifelse(SE_Pollen_Removal_Rate > max_se, max_se, SE_Pollen_Removal_Rate))

# GLM
model_pollen_removal_glm <- glm(Avg_Pollen_Removal_Rate ~ Rainfall, data = annual_data, family = gaussian())

# summary
summary_model_pollen_removal_glm <- summary(model_pollen_removal_glm)
print(summary_model_pollen_removal_glm)

# p
p_value_rainfall <- summary_model_pollen_removal$s.table[1, 4]
cat("P-value for Rainfall: ", p_value_rainfall, "\n")

# figure
p_rainfall <- ggplot(annual_data, aes(x = Rainfall, y = Avg_Pollen_Removal_Rate)) +
  geom_point(position = position_dodge(0.05)) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE, position = position_dodge(0.05)) +
  labs(
    title = "Rainfall vs Pollen Removal Rate",
    x = "Rainfall",
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
  )

# print
print(p_rainfall)

# save
ggsave("/Users/bofeng/Downloads/Honours/Result图/降雨/降雨和花粉的关系.png", plot = p_rainfall, width = 10, height = 6, dpi = 300)

