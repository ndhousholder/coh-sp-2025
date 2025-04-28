require(tidyverse)
percap = read.csv("totalPHCspendbyStateServicePerCap.csv", header = T, sep = ",", stringsAsFactors = F)

###
### Indiana (PHC)
###

INpercap = percap %>% 
  filter(State_Name == "Indiana" & Code == 1)
INlong = INpercap %>%
  select(Y1991:Y2020) %>%
  pivot_longer(cols = everything(), 
               names_to = "Year", 
               values_to = "Spending") %>%
  mutate(Year = as.numeric(gsub("Y", "", Year)))

ggplot(INlong, aes(x = Year, y = Spending)) +
  geom_line(color = "red") +
  labs(title = "Indiana Per Capita Personal Healthcare Spending (1991-2020)", 
       x = "Year", 
       y = "Spending ($ per capita)") +
  theme_minimal()

###
### Indiana vs Great Lakes Region (PHC)
###

GLpercap = percap %>% 
  filter(Region_Name == "Great Lakes" & Code ==1 & State_Name %in% c("Indiana", "Illinois", "Michigan", "Ohio", "Wisconsin"))
GLlong = GLpercap %>% 
  select(State_Name, Y1991:Y2020) %>%
  pivot_longer(cols = Y1991:Y2020, 
               names_to = "Year", 
               values_to = "Spending") %>%
  mutate(Year = as.numeric(gsub("Y", "", Year)))

GLsplitIN = GLlong %>% 
  filter(State_Name == "Indiana")
GLother = GLlong %>% 
  filter(State_Name != "Indiana") %>%
  group_by(Year) %>%
  summarise(Spending = mean(Spending, na.rm = TRUE)) %>%
  mutate(State_Name = "Great Lakes Avg. (excl. Indiana)")

GLplotboth = bind_rows(GLsplitIN, GLother)

ggplot(GLplotboth, aes(x = Year, y = Spending, color = State_Name)) +
  geom_line(size = 1) +
  labs(title = "Indiana vs. Great Lakes Region (excl. Indiana) Per Capita Spending (1991-2020)",
       x = "Year",
       y = "Spending ($ per capita)",
       color = "Region/State") +
  scale_color_manual(values = c("Indiana" = "red", "Great Lakes Avg. (excl. Indiana)" = "blue")) +
  theme_minimal()

###
### IN Spending by Service
###
IN2020 = percap %>% 
  filter(State_Name == "Indiana" & Code %in% 2:10) %>%
  select(Item, Y2020) %>%
  rename(Spending = Y2020)

ggplot(IN2020, aes(x = reorder(Item, -Spending), y = Spending)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Indiana Per Capita Healthcare Spending by Service Type (2020)", 
       x = "Service Type", 
       y = "Spending ($ per capita)") +
  theme_minimal() +
  geom_text(aes(label = Spending), hjust = -0.2, size = 3)

###
### Indiana vs US Spending by Service
###
Nat_IN2020 = percap %>% 
  filter(State_Name == "Indiana" & Code %in% 2:10) %>%
  select(Item, Y2020) %>%
  rename(Spending = Y2020) %>%
  mutate(Group = "Indiana")

Nat_US2020 = percap %>%
  filter(Group == "United States" & Code %in% 2:10) %>%
  select(Item, Y2020) %>%
  rename(Spending = Y2020) %>%
  mutate(Group = "US Average")

Natplotboth = bind_rows(Nat_IN2020, Nat_US2020)

ggplot(Natplotboth, aes(x = Item, y = Spending, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  # Flip for readability
  labs(title = "Indiana vs US Average Per Capita Spending by Service Type (2020)",
       x = "Service Type",
       y = "Spending ($ per capita)",
       fill = "Region/State") +
  scale_fill_manual(values = c("Indiana" = "blue", "US Average" = "lightgrey")) +
  theme_minimal() +
  geom_text(aes(label = Spending), 
            position = position_dodge(width = 0.9), 
            hjust = -0.2, size = 3)
