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

train_prob <- 0.6 # Proportion of data used for training
training_index <- createDataPartition(school_data$NAPLAN, p = train_prob, list = FALSE)
train_data <- school_data[training_index,]
remaining_data <- school_data[-training_index,]

validation_prob <- 0.5 # Proportion of remaining data used for validation
validation_index <- createDataPartition(remaining_data$NAPLAN, p = validation_prob, list = FALSE)
validation_data <- remaining_data[validation_index,]
test_data <- remaining_data[-validation_index,]

# KNN
formula <- NAPLAN ~ State + `School Sector` + ICSEA + sport_index + teach_edu_index + stu_atd + teach_tenure + student.teacher.ratio
tune_grid <- expand.grid(k = seq(1, 20))
naplan_model_knn <- train(formula, data = train_data, method = "knn", trControl = train_control, tuneGrid = tune_grid) # Need to separate into different datasets later
best_knn <- naplan_model_knn$bestTune$k

knn_predictions <- predict(naplan_model_knn, newdata = validation_data)
data.frame(Actual = validation_data$NAPLAN, knn_predictions)
postResample(pred = knn_predictions, obs = validation_data$NAPLAN)

# Multiple linear
naplan_model_linear <- train(formula, data = train_data, method = "lm", trControl = train_control)
best_linear <- naplan_model_linear$finalModel
summary(best_linear)

linear_predictions <- predict(naplan_model_linear, newdata = validation_data)
data.frame(Actual = validation_data$NAPLAN, linear_predictions)
postResample(pred = linear_predictions, obs = validation_data$NAPLAN)

# Elastic Net
tune_grid <- expand.grid(
  alpha = seq(0, 1, length = 20),
  lambda = seq(0.001, 0.1, length = 20)
)
naplan_model_elastic <- train(formula, data = train_data, method = "glmnet", trControl = train_control, tuneGrid = tune_grid)
best_elastic <- naplan_model_elastic$finalModel
best_coeff <- coef(best_elastic, naplan_model_elastic$bestTune$lambda)
print(best_coeff)

elastic_predictions <- predict(naplan_model_elastic, newdata = validation_data)
data.frame(Actual = validation_data$NAPLAN, elastic_predictions)
postResample(pred = elastic_predictions, obs = validation_data$NAPLAN)
