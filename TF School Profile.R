library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(glmnet)

profile_data = read_excel("School Profile 2023.xlsx", sheet = 2)
numeracy_data = read.csv("2023NAPLAN_scores_yr5.csv")
location_data = read_excel("School Location 2023.xlsx", sheet = 2)
location_subset_shared_data <- select(location_data, -`Calendar Year`, -`Location AGE ID`, -`School AGE ID`, -`School Name`, -Suburb, -State, -Postcode, -`School Sector`, -`School Type`, -`Campus Type`)
metrics_data = read_excel("school_metrics_2023.xlsx", sheet = 2)

school_data = inner_join(profile_data, metrics_data, by = "ACARA.SML.ID")
school_data = inner_join(school_data, location_subset_shared_data, by = "ACARA.SML.ID")
school_data = inner_join(school_data, numeracy_data, by = "ACARA.SML.ID")
school_data %>% filter(school_data$`Calendar Year` == 2023)
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

# KNN
formula <- NAPLAN ~ State + `School Sector` + ICSEA + sport_index + teach_edu_index + stu_atd + teach_tenure + student.teacher.ratio
naplan_model_knn <- train(formula, data = school_data, method = "knn", trControl = train_control) # Need to separate into different datasets later
print(naplan_model_knn)

# Multiple linear
naplan_model_linear <- train(formula, data = school_data, method = "lm", trControl = train_control)
print(naplan_model_linear)

# Elastic Net
tune_grid <- expand.grid(
  alpha = seq(0, 1, length = 20),
  lambda = seq(0.001, 0.1, length = 20)
)
naplan_model_elastic <- train(formula, data = school_data, method = "glmnet", trControl = train_control, tuneGrid = tune_grid)
print(naplan_model_elastic)

