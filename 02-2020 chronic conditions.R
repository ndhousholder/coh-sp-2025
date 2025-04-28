require(tidyverse)
places2020 = read.csv("PLACESlocalhealthdata-county2020.csv", header = T, sep = ",", stringsAsFactors = F) %>% mutate(Year = 2020)

INplaces20 = places2020 %>% filter(StateAbbr == "IN")

###
### Indiana Chronic Conditions (2020)
###

measures = places2020 %>%
  distinct(Measure) %>%
  arrange(Measure)
print(measures)

INchronic20 = INplaces20 %>%
  filter(str_detect(Measure, "Arthritis|asthma|Cancer|Chronic|COPD|Coronary|Diabetes|diabetes|Heart|Kidney|Stroke|smoking|Obesity|High blood pressure|Hypertension")) %>%
  group_by(Measure) %>%
  summarise(Avg_Prevalence = mean(Data_Value, na.rm = TRUE))

USchronic20 = places2020 %>%
  filter(str_detect(Measure, "Arthritis|asthma|Cancer|Chronic|COPD|Coronary|Diabetes|diabetes|Heart|Kidney|Stroke|smoking|Obesity|High blood pressure|Hypertension")) %>%
  group_by(Measure) %>%
  summarise(US_Avg_Prevalence = mean(Data_Value, na.rm = TRUE))

chronic20Comp = INchronic20 %>%
  left_join(USchronic20, by = "Measure") %>%
  mutate(Diff_vs_US = Avg_Prevalence - US_Avg_Prevalence) %>%
  arrange(desc(Diff_vs_US))

print(chronic20Comp)

chronic20complong = chronic20Comp %>%
  pivot_longer(cols = c(Avg_Prevalence, US_Avg_Prevalence), 
               names_to = "Group", 
               values_to = "Prevalence") %>%
  mutate(Group = recode(Group, "Avg_Prevalence" = "Indiana", "US_Avg_Prevalence" = "US"))

ggplot(chronic20complong, aes(x = Measure, y = Prevalence, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Indiana vs US Chronic Condition Prevalence (2020)",
       x = "Condition", y = "Prevalence (%)", fill = "Region") +
  scale_fill_manual(values = c("Indiana" = "blue", "US" = "lightgrey")) +
  theme_minimal() +
  geom_text(aes(label = round(Prevalence, 1)), 
            position = position_dodge(width = 0.9), 
            hjust = -0.2, size = 3)

ggplot(chronic20Comp, aes(x = Measure, y = Diff_vs_US, fill = Diff_vs_US > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Indiana vs. US Prevalence Difference (2020)", 
       x = "Condition", y = "Difference (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("FALSE" = "darkgreen", "TRUE" = "red"))
