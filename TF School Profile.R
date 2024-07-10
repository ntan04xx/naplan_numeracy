library(readxl)
library(dplyr)
library(tidyr)
library(caret)
library(glmnet)
library(car)

profile_data = read_excel("School Profile 2023.xlsx", sheet = 2)
numeracy_data = read.csv("2023NAPLAN_scores_yr5.csv")
location_data = read_excel("School Location 2023.xlsx", sheet = 2)
location_subset_shared_data <- select(location_data, -`Calendar Year`, -`Location AGE ID`, -`School AGE ID`, -`School Name`, -Suburb, -State, -Postcode, -`School Sector`, -`School Type`, -`Campus Type`)
census_data = read_excel("2021Census_G02_AUST_SA2.xlsx", sheet = 2)
metrics_data = read_excel("school_metrics_2023.xlsx", sheet = 2)

school_data = inner_join(profile_data, metrics_data, by = "ACARA.SML.ID")
school_data = inner_join(school_data, location_subset_shared_data, by = "ACARA.SML.ID")
school_data = inner_join(school_data, census_data, by = "Statistical Area 2")
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

train_prob <- 0.8 # Proportion of data used for training
training_index <- createDataPartition(school_data$NAPLAN, p = train_prob, list = FALSE)
train_data <- school_data[training_index,]
test_data <- school_data[-training_index,]

# KNN
formula <- NAPLAN ~ State + `School Sector` + ICSEA + sport_index + teach_edu_index + stu_atd + teach_tenure + student.teacher.ratio
tune_grid <- expand.grid(k = seq(1, 20))
naplan_model_knn <- train(formula, data = train_data, method = "knn", trControl = train_control, tuneGrid = tune_grid) # Need to separate into different datasets later
best_knn <- naplan_model_knn$bestTune$k
final_knn <- train(formula, data = train_data, method = "knn", tuneGrid = data.frame(k = best_knn))

knn_predictions <- predict(final_knn, newdata = test_data)
knn_rmse <- sqrt(mean((knn_predictions - test_data$NAPLAN)^2))
knn_r_squared <- cor(knn_predictions, test_data$NAPLAN)^2
knn_mae <- mean(abs(knn_predictions - test_data$NAPLAN))

# Multiple linear
naplan_model_linear <- train(formula, data = train_data, method = "lm", trControl = train_control)
final_linear <- naplan_model_linear
best_linear <- final_linear$finalModel
summary(best_linear)
vif(best_linear)
plot(best_linear)

linear_predictions <- predict(final_linear, newdata = test_data)
lm_rmse <- sqrt(mean((linear_predictions - test_data$NAPLAN)^2))
lm_r_squared <- cor(linear_predictions, test_data$NAPLAN)^2
lm_mae <- mean(abs(linear_predictions - test_data$NAPLAN))

# Elastic Net
tune_grid <- expand.grid(
  alpha = seq(0, 1, length = 20),
  lambda = seq(0.001, 0.1, length = 20)
)
naplan_model_elastic <- train(formula, data = train_data, method = "glmnet", trControl = train_control, tuneGrid = tune_grid)
best_elastic <- naplan_model_elastic$finalModel
best_coeff <- coef(best_elastic, naplan_model_elastic$bestTune$lambda)
print(best_coeff)
best_lambda <- naplan_model_elastic$bestTune$lambda
best_alpha <- naplan_model_elastic$bestTune$alpha
final_elastic <- train(formula, data = train_data, method = "glmnet", tuneGrid = expand.grid(alpha = best_alpha, lambda = best_lambda))

elastic_predictions <- predict(final_elastic, newdata = test_data)
elastic_net_rmse <- sqrt(mean((elastic_predictions - test_data$NAPLAN)^2))
elastic_net_r_squared <- cor(elastic_predictions, test_data$NAPLAN)^2
elastic_net_mae <- mean(abs(elastic_predictions - test_data$NAPLAN))

# Criteria for Funding
# For Teachers
ratio_ranks <- rank(school_data$student.teacher.ratio)
affordability_ranks <- rank(school_data$Median_rent_weekly)
remote_ranks <- rank(school_data$`ABS Remoteness Area`)
school_data$teacher_criteria = 0.4 * ratio_ranks + 0.4 * affordability_ranks + 0.2 * remote_ranks
min_index <- which(school_data$teacher_criteria == min(school_data$teacher_criteria))
best_school_teacher <- school_data[min_index, ]$`School Name`

# For Equipment
student_number_ranks <- rank(school_data$`Full Time Equivalent Enrolments`)
school_data$equipment_criteria = 0.8 * student_number_ranks + 0.2 * remote_ranks
min_index <- which(school_data$equipment_criteria == min(school_data$equipment_criteria))
best_school_equipment <- school_data[min_index, ]$`School Name`

# General
school_data$general_criteria <- 0.5 * school_data$teacher_criteria + 0.5 * school_data$equipment_criteria
min_index <- which(school_data$general_criteria == min(school_data$general_criteria))
best_school <- school_data[min_index, ]$`School Name`
