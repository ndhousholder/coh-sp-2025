require(tidyverse)

ggplot(chronicTrend, aes(x = Measure, y = Change_2020_to_2024, fill = Change_2020_to_2024 > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Change in Indiana Prevalence (2020–2024)", y = "Change (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("FALSE" = "darkgreen", "TRUE" = "red"))

###
### Estimate Cost Changes (Based on Prevalence Shifts)
###

spending_2020 = list(hospital = 4155, nursing = 749, drugs = 1500, physician = 2500, other = 1613)

chronicTrend <- tribble(
  ~Measure, ~Avg_Prevalence, ~Avg_Prevalence_2024, ~Change_2020_to_2024,
  "Obesity among adults", 35.3, 39.0, 3.70,
  "Cancer (non-skin) or melanoma among adults", 7.13, 8.16, 1.03,
  "Current asthma among adults", 10.2, 11.2, 0.958,
  "Arthritis among adults", 28.2, 28.7, 0.547,
  "Stroke among adults", 3.5, 3.58, 0.083,
  "Coronary heart disease among adults", 7.36, 7.38, 0.024,
  "Diagnosed diabetes among adults", 11.9, 11.8, -0.079,
  "Chronic obstructive pulmonary disease among adults", 9.42, 8.93, -0.490,
  "High blood pressure among adults", 35.1, 34.5, -0.589,
  "Current cigarette smoking among adults", 23.4, 19.2, -4.19)

hospital_change = (-0.052 * 0.2) + (0.105 * 0.5) + (0.094 * 0.15)  # COPD, obesity, asthma
nursing_change = 0.019  # Arthritis
drugs_change = (-0.017 * 0.4) + (-0.007 * 0.3) + (-0.052 * 0.2) + (0.094 * 0.1)  # HBP, diabetes, COPD, asthma
physician_change = (0.144 * 0.2) + (0.094 * 0.2)  # Cancer, asthma

spending_2024 = list(
  hospital = spending_2020$hospital * (1 + hospital_change),
  nursing = spending_2020$nursing * (1 + nursing_change),
  drugs = spending_2020$drugs * (1 + drugs_change),
  physician = spending_2020$physician * (1 + physician_change),
  other = spending_2020$other)  # Assume stable

total_2024 = sum(unlist(spending_2024))

cat("2024 Estimates (per capita):\n",
    "Hospital: $", round(spending_2024$hospital), "\n",
    "Nursing Home: $", round(spending_2024$nursing), "\n",
    "Drugs: $", round(spending_2024$drugs), "\n",
    "Physician: $", round(spending_2024$physician), "\n",
    "Other: $", spending_2024$other, "\n",
    "Total: $", round(total_2024), "\n",
    "Change from 2020: $", round(total_2024 - 10517), " (+", round((total_2024 - 10517) / 10517 * 100, 2), "%)\n")

###
### Plot 2024 Est. against 2020
###

cost_data = tribble(
  ~Category, ~Year, ~Cost,
  "Hospital", "2020", 4155,
  "Hospital", "2024", 4388,
  "Nursing Home", "2020", 749,
  "Nursing Home", "2024", 763,
  "Drugs", "2020", 1500,
  "Drugs", "2024", 1485,
  "Physician", "2020", 2500,
  "Physician", "2024", 2619,
  "Other", "2020", 1613,
  "Other", "2024", 1613,
  "Total", "2020", 10517,
  "Total", "2024", 10868)

ggplot(cost_data, aes(x = Category, y = Cost, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0("$", round(Cost))), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(title = "Indiana Healthcare Spending: 2020 vs. 2024 Estimates",
       x = "Spending Category",
       y = "Cost (Per Capita, $)",
       fill = "Year") +
  scale_fill_manual(values = c("2020" = "grey", "2024" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
