library(readxl)
library(dplyr)
library(tidyr)
library(caret)

profile_data = read_excel("School Profile 2023.xlsx", sheet = 2)
numeracy_data = read.csv("2023NAPLAN_scores_yr5.csv")
location_data = read_excel("School Location 2023.xlsx", sheet = 2)
metrics_data = read_excel("school_metrics_2023.xlsx", sheet = 2)

school_data = inner_join(profile_data, metrics_data, by = "ACARA.SML.ID")
school_data = inner_join(school_data, location_data, by = "ACARA.SML.ID")
school_data = inner_join(school_data, numeracy_data, by = "ACARA.SML.ID")
school_data %>% filter(school_data$`Calendar Year.x` == 2023)
school_data <- school_data %>%
  drop_na()
summary(school_data)

# Exploring data
school_data$student.teacher.ratio = school_data$`Full Time Equivalent Enrolments` / school_data$`Full Time Equivalent Teaching Staff` # Uses equivalent columns
cor(school_data$student.teacher.ratio, school_data$NAPLAN) # from this, near zero correlation
plot(school_data$NAPLAN, school_data$student.teacher.ratio)

cor(school_data$ICSEA, school_data$NAPLAN, use = "complete.obs") # Relatively solid positive correlation
plot(school_data$NAPLAN, school_data$ICSEA)

# Model Fitting
set.seed(69420)
k <- 10 # Number of folds for cross-validation
train_control <- trainControl(method = "cv", number = k)

naplan_model_knn <- train(NAPLAN ~ State.x + `School Sector.x` + ICSEA + sport_index + teach_edu_index + stu_atd + teach_tenure + student.teacher.ratio, data = school_data, method = "knn", trControl = train_control)
print(naplan_model_knn)
