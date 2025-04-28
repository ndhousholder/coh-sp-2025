require(tidyverse)
places2024 = read.csv("PLACESlocalhealthdata-county2024.csv", header = T, sep = ",", stringsAsFactors = F) %>% mutate(Year = 2024)

INplaces24 = places2024 %>% filter(StateAbbr == "IN")

###
### Indiana Chronic Conditions (2024)
###

measures = places2024 %>%
  distinct(Measure) %>%
  arrange(Measure)
print(measures)

INchronic24 = INplaces24 %>%
  filter(str_detect(Measure, "Arthritis|asthma|Cancer|Chronic|COPD|Coronary|diabetes|Heart|Kidney|kidney|Stroke|smoking|Obesity|High blood pressure|Hypertension")) %>%
  group_by(Measure) %>%
  summarise(Avg_Prevalence = mean(Data_Value, na.rm = T))

USchronic24 <- places2024 %>%
  filter(str_detect(Measure, "Arthritis|asthma|Cancer|Chronic|COPD|Coronary|diabetes|Heart|Kidney|kidney|Stroke|smoking|Obesity|High blood pressure|Hypertension")) %>%
  group_by(Measure) %>%
  summarise(US_Avg_Prevalence = mean(Data_Value, na.rm = T))

chronic24comp = INchronic24 %>%
  left_join(USchronic24, by = "Measure") %>%
  mutate(Diff_vs_US = Avg_Prevalence - US_Avg_Prevalence) %>%
  arrange(desc(Diff_vs_US))

print(chronic24comp)

chronic24complong = chronic24comp %>%
  pivot_longer(cols = c(Avg_Prevalence, US_Avg_Prevalence), 
               names_to = "Group", 
               values_to = "Prevalence") %>%
  mutate(Group = recode(Group, "Avg_Prevalence" = "Indiana", "US_Avg_Prevalence" = "US"))

ggplot(chronic24complong, aes(x = Measure, y = Prevalence, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Indiana vs US Chronic Condition Prevalence (2024)",
       x = "Condition", y = "Prevalence (%)", fill = "Region") +
  scale_fill_manual(values = c("Indiana" = "blue", "US" = "lightgrey")) +
  theme_minimal() +
  geom_text(aes(label = round(Prevalence, 1)), 
            position = position_dodge(width = 0.9), 
            hjust = -0.2, size = 3)

ggplot(chronic24comp, aes(x = Measure, y = Diff_vs_US, fill = Diff_vs_US > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Indiana vs. US Prevalence Difference (2024)", 
       x = "Condition", y = "Difference (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("FALSE" = "darkgreen", "TRUE" = "red"))

###
### Compare 2020 and 2024
###

comp2020 = tribble(
  ~Measure, ~Avg_Prevalence, ~US_Avg_Prevalence, ~Diff_vs_US,
  "Current smoking among adults aged >=18 years", 23.4, 20.7, 2.65,
  "Chronic obstructive pulmonary disease among adults aged >=18 years", 9.42, 8.42, 0.996,
  "High blood pressure among adults aged >=18 years", 35.1, 34.8, 0.356,
  "Current asthma among adults aged >=18 years", 10.2, 9.92, 0.312,
  "Obesity among adults aged >=18 years", 35.3, 35.1, 0.222,
  "Cancer (excluding skin cancer) among adults aged >=18 years", 7.13, 7.14, -0.003,
  "Diagnosed diabetes among adults aged >=18 years", 11.9, 12.0, -0.083,
  "Chronic kidney disease among adults aged >=18 years", 2.93, 3.16, -0.231,
  "Stroke among adults aged >=18 years", 3.50, 3.75, -0.252,
  "Coronary heart disease among adults aged >=18 years", 7.36, 7.72, -0.362,
  "Arthritis among adults aged >=18 years", 28.2, 28.6, -0.373)

print("2020 Measures:")
print(comp2020$Measure)
print("2024 Measures:")
print(chronic24comp$Measure)

comp2020_clean <- comp2020 %>%
  mutate(Measure = str_replace(Measure, " aged >=18 years", "") %>%
           str_replace("Current smoking", "Current cigarette smoking") %>%
           str_replace("Cancer \\(excluding skin cancer\\)", "Cancer (non-skin) or melanoma"))

chronic24comp20 = INplaces24 %>%
  filter(str_detect(Measure, "Arthritis|asthma|Cancer|Chronic|COPD|Coronary|diabetes|Heart|Kidney|kidney|Stroke|smoking|Obesity|High blood pressure|Hypertension")) %>%
  group_by(Measure) %>%
  summarise(Avg_Prevalence = mean(Data_Value, na.rm = TRUE))

chronic24comp_clean = chronic24comp20 %>%
  mutate(Measure = trimws(Measure))

print("2020 Cleaned Measures:")
print(comp2020_clean$Measure)
print("2024 Cleaned Measures:")
print(chronic24comp_clean$Measure)

chronicTrend = comp2020_clean %>%
  left_join(select(chronic24comp_clean, Measure, Avg_Prevalence_2024 = Avg_Prevalence), by = "Measure") %>%
  mutate(Change_2020_to_2024 = Avg_Prevalence_2024 - Avg_Prevalence)

print(chronicTrend %>% arrange(desc(Change_2020_to_2024)))

ggplot(chronicTrend, aes(x = Measure, y = Change_2020_to_2024, fill = Change_2020_to_2024 > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Change in Indiana Prevalence (2020–2024)", y = "Change (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("FALSE" = "darkgreen", "TRUE" = "red"))
