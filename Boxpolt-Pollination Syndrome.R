# This code generates a boxplot visualization to compare the pollinia removal rates 
# across different pollination syndromes (Food Deception, Self-pollination, and Sexual Deception).  
# It reshapes the data into a long format, performs data checks (e.g., missing values), 
# and produces a boxplot.  


library(readxl)
library(ggplot2)
library(dplyr)
library(car)  # ANOVA分析
library(multcomp)  # Tukey HSD检验
library(tidyr)  # pivot_longer函数

# data
file_path <- '/Users/bofeng/Downloads/Honours/数据/数据处理/SuccessRateByYear.xlsx'
data <- read_excel(file_path)

# Check data structure
print(head(data))
str(data)

# check missing value
print(sum(is.na(data)))

# Convert the data to long format for easier plotting
data_long <- data %>%
  pivot_longer(cols = c("Food deception", "Self-pollination", "Sexual deception"),
               names_to = "Pollination Syndrome",
               values_to = "Pollen Removal Rate")

# Create boxplot
p <- ggplot(data_long, aes(x = `Pollination Syndrome`, y = `Pollen Removal Rate`, fill = `Pollination Syndrome`)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "",
       x = "Pollination Syndrome",
       y = "Pollen Removal Rate (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Self-pollination" = "#a6d854", "Food deception" = "#fc8d62", "Sexual deception" = "#8da0cb")) +
  theme(plot.background = element_rect(colour = "black", size = 2))

# print
print(p)

# output
ggsave("/Users/bofeng/Downloads/Honours/Result图/Pollination Syndrome/箱线图/NewPollenRemovalRate_Boxplot1.png", plot = p, width = 10, height = 6, dpi = 300)
