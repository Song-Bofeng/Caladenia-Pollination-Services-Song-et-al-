# This code analyzes the relationship between temperature anomalies and pollen removal rates over the period from 1920 to 2020. 

library(readxl)
library(dplyr)
library(ggplot2)
library(mgcv)

# pollen data
file_path <- "/Users/bofeng/Downloads/Honours/数据/NewPolliniaData.xlsx"
data <- read_excel(file_path, sheet = 1)
subgenus_data <- read_excel(file_path, sheet = 2)

#  temperature anomalies data
temperature_file_path <- "/Users/bofeng/Downloads/Honours/Result图/温度/1920-2020温度异常值.csv"
temperature_data <- read.csv(temperature_file_path)

# merge Subgenus info
data <- data %>% 
  left_join(subgenus_data, by = c("Species" = "Species"))

# merge temperature anomalies data
data <- data %>%
  left_join(temperature_data, by = c("Year" = "Year"))

# Filter and convert
# Year 
# convert pollen removal rate to numeric
data <- data %>%
  filter(Year >= 1920 & Year <= 2020) %>%  
  mutate(
    `Pollen Removal Rate` = as.numeric(`Pollen Removal Rate`)
  )

# Calculate the average pollen removal rate and SE by Year
annual_data <- data %>%
  group_by(Year) %>%
  summarise(
    Avg_Pollen_Removal_Rate = mean(`Pollen Removal Rate`, na.rm = TRUE),
    SE_Pollen_Removal_Rate = sd(`Pollen Removal Rate`, na.rm = TRUE) / sqrt(n()),
    Temperature_Anomaly = mean(Temperature_Anomaly, na.rm = TRUE),
    Sample_Size = n()
  ) %>% ungroup()

# Limit the maximum length of error bars
max_se <- 0.05  
annual_data <- annual_data %>%
  mutate(SE_Pollen_Removal_Rate = ifelse(SE_Pollen_Removal_Rate > max_se, max_se, SE_Pollen_Removal_Rate))

# GLM
model_pollen_removal_glm <- glm(Avg_Pollen_Removal_Rate ~ Temperature_Anomaly, data = annual_data, family = gaussian(link = "identity"))
summary_model_pollen_removal_glm <- summary(model_pollen_removal_glm)

# summary
print(summary_model_pollen_removal_glm)

# p
p_value_temp <- coef(summary_model_pollen_removal_glm)["Temperature_Anomaly", "Pr(>|t|)"]

# p
cat("P-value for Temperature Anomaly: ", p_value_temp, "\n")

# figure
p_temp_glm <- ggplot(annual_data, aes(x = Temperature_Anomaly, y = Avg_Pollen_Removal_Rate)) +
  geom_point(position = position_dodge(0.05)) +
  geom_smooth(method = "glm", formula = y ~ x, se = TRUE, position = position_dodge(0.05)) +
  labs(
    title = "Temperature Anomaly vs Pollen Removal Rate",
    x = "Temperature Anomaly",
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
print(p_temp_glm)

# save
ggsave("/Users/bofeng/Downloads/Honours/Result图/温度/New温度和花粉的关系.png", plot = p_temp_glm, width = 10, height = 6, dpi = 600)
