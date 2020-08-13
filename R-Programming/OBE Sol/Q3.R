library(dplyr)

data<-read.csv("Salaries.csv", TRUE, ",")
dsalary<-data.frame(data)
dsalary

#number of observations & variables
str(dsalary)

#average salary of all faculty members
average_salary_all<-mean(dsalary$Salary)
average_salary_all

#average salary of male and female faculty members
lapply(split(dsalary, dsalary$Sex), function(x) {
  mean(x$Salary)
})

#rank wise salary
lapply(split(dsalary, dsalary$Rank), function(x) {
  mean(x$Salary)
})

#number of Male AsstProf
male_asst<-filter(dsalary, dsalary$Sex=="Male", dsalary$Rank=="AsstProf")
num_male_asst<-length(male_asst)
num_male_asst

#number of Female AssocProf
female_assoc<-filter(dsalary, dsalary$Sex=="Female", dsalary$Rank=="AssocProf")
num_female_assoc<-length(female_assoc)
num_female_assoc

#Chi-squared test between sex and rank
chi_test<-chisq.test(dsalary$Sex, dsalary$Rank)
chi_test

#compare and print higher salary drawing between male & female AssocProf with maximum salaries
male<- filter(dsalary, dsalary$Sex=="Male", dsalary$Rank=="AssocProf")
male <- male[order(male$Yrs.service, male$Salary, decreasing = TRUE), ]
male_max<-max(male[1, ]$Salary)

female<-filter(dsalary, dsalary$Sex=="Female", dsalary$Rank=="AssocProf")
female <- female[order(female$Yrs.service, female$Salary, decreasing = TRUE), ]
female_max<-max(female[1, ]$Salary)

comparison<-max(male_max, female_max)
comparison

#new coloumn
dsalary<-mutate(dsalary, special_allowance = 0.05 * dsalary$Salary)
dsalary$special_allowance

#histogram for top 20 faculty members
salary_hl<- dsalary[order(dsalary$Salary), ]
top<-head(salary_hl, 20)
hist(x = top$Salary)
