library(readxl)
library(dplyr)
library(ggplot2)

location_data = read_excel("School Location 2023.xlsx", sheet = 2)
location_data %>% filter(location_data$`Calendar Year` == 2023)
numeracy_data = read.csv("2023NAPLAN_scores_yr5.csv")
school_data = inner_join(location_data, numeracy_data, by = "ACARA.SML.ID") # cleans data
summary(school_data)

# State
mean_state_score <- school_data %>%
  group_by(State) %>%
  summarise(Mean_Numeracy_Score = mean(NAPLAN))
ggplot(mean_state_score, aes(x = State, y = Mean_Numeracy_Score)) +
  geom_bar(stat = "identity") +
  labs(x = "State", y = "Average Numeracy Score") +
  scale_y_continuous(breaks = seq(0, max(mean_state_score$Mean_Numeracy_Score) + 50, by = 20)) # Best states seem to be ACT and VIC, worst are NT and TAS

# School Sector
mean_sector_score <- school_data %>%
  group_by(`School Sector`) %>%
  summarise(Mean_Numeracy_Score = mean(NAPLAN))
ggplot(mean_sector_score, aes(x = `School Sector`, y = Mean_Numeracy_Score)) +
  geom_bar(stat = "identity") +
  labs(x = "School Sector", y = "Average Numeracy Score") +
  scale_y_continuous(breaks = seq(0, max(mean_state_score$Mean_Numeracy_Score) + 50, by = 20)) # Independent performs best, government performs worst

# ABS Remoteness
mean_remoteness_score <- school_data %>%
  group_by(`ABS Remoteness Area Name`) %>%
  summarise(Mean_Numeracy_Score = mean(NAPLAN))
ggplot(mean_remoteness_score, aes(x = `ABS Remoteness Area Name`, y = Mean_Numeracy_Score)) +
  geom_bar(stat = "identity") +
  labs(x = "Remoteness Area", y = "Average Numeracy Score") +
  scale_y_continuous(breaks = seq(0, max(mean_state_score$Mean_Numeracy_Score) + 50, by = 20)) # Major cities and inner regional are best, very remote is worst
cor(school_data$NAPLAN, school_data$`ABS Remoteness Area`)

# Suburb
mean_suburb_score <- school_data %>%
  group_by(Suburb) %>%
  summarise(Mean_Numeracy_Score = mean(NAPLAN)) %>%
  arrange(desc(Mean_Numeracy_Score), Suburb)

top_suburb_scores <- mean_suburb_score %>%
  slice_head(n = 10)
bottom_suburb_scores <- mean_suburb_score %>%
  slice_tail(n = 10)

