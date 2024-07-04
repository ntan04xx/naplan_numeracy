library(readxl)
library(dplyr)

metrics_data = read_excel("school_metrics_2023.xlsx", sheet = 2)
numeracy_data = read.csv("2023NAPLAN_scores_yr5.csv")
school_data = inner_join(metrics_data, numeracy_data, by = "ACARA.SML.ID") # cleans data
summary(school_data)
school_score = school_data$NAPLAN

# Sports Index
cor(school_score, school_data$sport_index)
plot(school_score, school_data$sport_index) # Positive linear relationship, high-leverage points?

# Teach Educational Index
cor(school_score, school_data$teach_edu_index)
plot(school_score, school_data$teach_edu_index) # still high-leverage points, not much of relationship

# Tech Index
cor(school_score, school_data$tech_index)
plot(school_score, school_data$tech_index) # almost no relationship

# Student Attendance
cor(school_score, school_data$stu_atd)
plot(school_score, school_data$stu_atd) # Generally positive linear relationship

# Teaching Tenure
cor(school_score, school_data$teach_tenure)
plot(school_score, school_data$teach_tenure) # almost no relationship

lin_model <- lm(NAPLAN ~ sport_index + teach_edu_index + tech_index + stu_atd + teach_tenure, data = school_data)
summary(lin_model) # Sport, teach_edu, student attendance and teach tenure are considered significant, largest impact is attendance and sport index
