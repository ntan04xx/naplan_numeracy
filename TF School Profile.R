library(readxl)
library(dplyr)

profile_data = read_excel("School Profile 2023.xlsx", sheet = 2)
numeracy_data = read.csv("2023NAPLAN_scores_yr5.csv")
school_data = inner_join(profile_data, numeracy_data, by = "ACARA.SML.ID") # cleans data
summary(school_data)

school_data$student.teacher.ratio = school_data$`Full Time Equivalent Enrolments` / school_data$`Full Time Equivalent Teaching Staff` # Uses equivalent columns
cor(school_data$student.teacher.ratio, school_data$NAPLAN) # from this, near zero correlation
plot(school_data$NAPLAN, school_data$student.teacher.ratio)

cor(school_data$ICSEA, school_data$NAPLAN, use = "complete.obs") # Relatively solid positive correlation
plot(school_data$NAPLAN, school_data$ICSEA)
