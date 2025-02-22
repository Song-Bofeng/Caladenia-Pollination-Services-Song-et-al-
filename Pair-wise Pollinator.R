# This code processes pollinator data, classifies species into pollinator groups (Wasps, Bees, Flies), and performs a series of statistical analyses. 
# It includes filtering species based on pollination syndrome, handling overlapping categories, 
# reclassifying species, and performing pairwise comparisons using the Wilcoxon rank-sum test. 
# The code also calculates statistical metrics (such as sample sizes, p-values, and adjusted p-values) and outputs the results.


library(readxl)
library(dplyr)
library(ggplot2)
library(segmented)
library(multcomp)

# Define the path to pollinator information
pollinator_info_path <- "/Users/bofeng/Downloads/Honours/Result图/Pollinator/Pollinator-Info.xlsx"
pollinator_data <- read_excel(pollinator_info_path)

# Define pollinator categories based on species
wasps_species <- pollinator_data %>%
  filter(`Categrories 1` == "Wasps" | `Categrories 2` == "Wasps") %>%
  pull(Species)

bees_species <- pollinator_data %>%
  filter(`Categrories 1` == "Bees" | `Categrories 2` == "Bees") %>%
  pull(Species)

flies_species <- pollinator_data %>%
  filter(`Categrories 1` == "Flies") %>%
  pull(Species)

# data
file_path <- "/Users/bofeng/Downloads/Honours/数据/NewPolliniaData.xlsx"
data <- read_excel(file_path, sheet = 1) %>%
  left_join(pollinator_data, by = "Species") %>%
  filter(Year >= 1925 & Year <= 2020) %>%
  mutate(`Pollen Removal Rate` = as.numeric(`Pollen Removal Rate`))

# Assign pollinator categories based on species
data <- data %>%
  mutate(Pollinator_Group = case_when(
    Species %in% wasps_species ~ "Wasps",
    Species %in% bees_species ~ "Bees",
    Species %in% flies_species ~ "Flies",
    TRUE ~ NA_character_
  ))

# Calculate
annual_data <- data %>%
  group_by(Year, Pollinator_Group) %>%
  summarise(
    Avg_Pollen_Removal_Rate = mean(`Pollen Removal Rate`, na.rm = TRUE),
    SE_Pollen_Removal_Rate = sd(`Pollen Removal Rate`, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()


# Check flies_species and bees_species 
overlapping_species <- intersect(flies_species, bees_species)
cat("Overlapping species between Flies and Bees:\n")
print(overlapping_species)

# Update classification logic to prioritize Flies
data <- data %>%
  mutate(
    pollinator_group = case_when(
      Species %in% flies_species ~ "Flies",  # Flies 优先
      Species %in% wasps_species ~ "Wasps",
      Species %in% bees_species ~ "Bees",
      TRUE ~ NA_character_
    )
  )

# Check updated classification results
table(data$pollinator_group, useNA = "ifany")

# Check species not matched to Flies
mismatched_species <- setdiff(flies_species, unique(data$Species))
cat("Mismatched Flies species:\n")
print(mismatched_species)

# double check
reverse_mismatch <- setdiff(unique(data$Species), flies_species)
cat("Species in data but not in flies_species:\n")
print(reverse_mismatch)

# make sure
flies_final_check <- data %>%
  filter(pollinator_group == "Flies")

cat("Number of Flies species after reclassification:", nrow(flies_final_check), "\n")
print(flies_final_check)

# View data
flies_data <- data %>%
  filter(pollinator_group == "Flies")

cat("Number of Flies records:", nrow(flies_data), "\n")
print(head(flies_data))

# Perform pairwise Wilcoxon rank-sum test for pollinator groups
pairwise_result <- data %>%
  filter(!is.na(pollinator_group)) %>%  # NA 
  group_by(pollinator_group) %>%
  summarise(Avg_Pollen_Removal_Rate = list(`Pollen Removal Rate`)) %>%
  ungroup() %>%
  summarise(
    test_result = list(pairwise.wilcox.test(
      x = unlist(Avg_Pollen_Removal_Rate), 
      g = rep(pollinator_group, sapply(Avg_Pollen_Removal_Rate, length)), 
      p.adjust.method = "BH"
    ))
  )

# print 
print(pairwise_result$test_result[[1]])

#info

# Perform pairwise Wilcoxon tests and output more statistical details
# sample
stat_results <- data %>%
  filter(!is.na(pollinator_group)) %>%  
  group_by(pollinator_group) %>%
  summarise(
    N = n(),  
    Avg_Pollen_Removal_Rate = list(`Pollen Removal Rate`)  
  ) %>%
  ungroup()

# Initialize list to store pairwise comparison results
pairwise_comparisons <- list()

# Perform pairwise comparisons between pollinator groups
groups <- unique(stat_results$pollinator_group)
for (i in 1:(length(groups) - 1)) {
  for (j in (i + 1):length(groups)) {
    group1 <- groups[i]
    group2 <- groups[j]
    
    # data
    data1 <- unlist(stat_results$Avg_Pollen_Removal_Rate[stat_results$pollinator_group == group1])
    data2 <- unlist(stat_results$Avg_Pollen_Removal_Rate[stat_results$pollinator_group == group2])
    
    # Wilcoxon 
    wilcox_result <- wilcox.test(data1, data2, exact = FALSE, correct = TRUE)
    
    # result
    pairwise_comparisons <- append(pairwise_comparisons, list(data.frame(
      Group1 = group1,
      Group2 = group2,
      N1 = length(data1),
      N2 = length(data2),
      W = wilcox_result$statistic,
      p_value = wilcox_result$p.value
    )))
  }
}

# Combine all pairwise comparison results into one data frame
pairwise_results_df <- do.call(rbind, pairwise_comparisons)

# p（BH ）
pairwise_results_df$p_adjusted <- p.adjust(pairwise_results_df$p_value, method = "BH")

# print
print(pairwise_results_df)





