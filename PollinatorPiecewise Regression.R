# This code performs segmented regression analysis for pollen removal rates across three different pollinator groups (Wasps, Bees, and Flies) over the years 1925 to 2020.  

library(readxl)
library(dplyr)
library(segmented)
library(ggplot2)

# data
file_path <- "/Users/bofeng/Downloads/Honours/数据/NewPolliniaData.xlsx"
pollinator_info_path <- "/Users/bofeng/Downloads/Honours/Result图/Pollinator/Pollinator-Info.xlsx"

data <- read_excel(file_path, sheet = 1)
pollinator_data <- read_excel(pollinator_info_path)

# merge Pollinator Categories info
data <- data %>%
  left_join(pollinator_data, by = c("Species" = "Species")) %>%
  filter(Year >= 1925 & Year <= 2020) %>%
  mutate(`Pollen Removal Rate` = as.numeric(`Pollen Removal Rate`))

# Define pollinator species categories (Wasps, Bees, Flies)
wasps_species <- pollinator_data %>%
  filter(`Categrories 1` == "Wasps" | `Categrories 2` == "Wasps") %>%
  pull(Species)

bees_species <- pollinator_data %>%
  filter(`Categrories 1` == "Bees" | `Categrories 2` == "Bees") %>%
  pull(Species)

flies_species <- pollinator_data %>%
  filter(`Categrories 1` == "Flies" | `Categrories 2` == "Flies") %>%
  pull(Species)

# Define a function for running segmented regression analysis
run_segmented_regression <- function(data, species_group, group_name, initial_breakpoint = 1975) {
  group_data <- data %>%
    filter(Species %in% species_group) %>%
    group_by(Year) %>%
    summarise(
      Avg_Pollen_Removal_Rate = mean(`Pollen Removal Rate`, na.rm = TRUE),
      SE_Pollen_Removal_Rate = sd(`Pollen Removal Rate`, na.rm = TRUE) / sqrt(n())
    ) %>%
    ungroup()
  
  # liner
  lm_model <- lm(Avg_Pollen_Removal_Rate ~ Year, data = group_data)
  
  # segmented regression model
  seg_model <- segmented(lm_model, seg.Z = ~Year, psi = initial_breakpoint)
  
  # summary
  cat("\nSegmented Regression for", group_name, "\n")
  print(summary(seg_model))
  
  # break
  breakpoint <- seg_model$psi
  cat("Estimated Breakpoint:", breakpoint[, "Est."], "\n")
  
  # decline number
  slopes <- slope(seg_model)$Year
  print(slopes)
  
  # figure
  fitted_values <- predict(seg_model, newdata = data.frame(Year = group_data$Year))
  group_data$fitted <- fitted_values
  
  p <- ggplot(group_data, aes(x = Year, y = Avg_Pollen_Removal_Rate)) +
    geom_point(color = "blue", size = 2) +
    geom_line(aes(y = fitted), color = "red", size = 1) +
    geom_vline(xintercept = breakpoint[, "Est."], color = "green", linetype = "dashed", size = 1) +
    labs(
      title = paste("Segmented Regression:", group_name),
      x = "Year",
      y = "Average Pollen Removal Rate"
    ) +
    theme_minimal()
  
  print(p)
}

# Run segmented regression analysis for each pollinator group 
run_segmented_regression(data, wasps_species, "Wasps")
run_segmented_regression(data, bees_species, "Bees")
run_segmented_regression(data, flies_species, "Flies")


# sample size 
wasps_n <- data %>% filter(Species %in% wasps_species) %>% nrow()
bees_n <- data %>% filter(Species %in% bees_species) %>% nrow()
flies_n <- data %>% filter(Species %in% flies_species) %>% nrow()

# sample size 
cat("Sample size for each pollinator group:\n")
cat("Wasps: ", wasps_n, "\n")
cat("Bees: ", bees_n, "\n")
cat("Flies: ", flies_n, "\n")

