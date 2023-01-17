rm(list=ls())  #remove all variables stored previously 
library(Hmisc) #import



data <- read.csv("C:/Users/annfe/Downloads/COVID19_line_list_data.csv")
View(COVID19_line_list_data)
describe (data) 
# cleaned up the death column
data$death_dummy <-  as.integer(data$death  !=0)
unique(data$death_dummy)

# death rate 
sum(data$death_dummy) / nrow(data)
 # Age
# claim: people who died of covid is older compared to the ppl survived

dead = subset(data, death_dummy== 1)
alive = subset(data, death_dummy ==0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
# is this statistically significant ?
t.test(alive$age, dead$age, alternative = "two.sided",conf.level = 0.95)
# normally, if p-value <0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and conclude that this is 
# stastistically insignificant 

#Gender
#claim: gender has no effect
men = subset(data, gender== "male")
women = subset(data, gender =="female")
mean(men$death_dummy, na.rm = TRUE) #8.5%
mean(women$death_dummy, na.rm = TRUE) # 3.7%
# is this statistically significant ?
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)
#99% confidence: men have from 0.8% to 8.8% higher chance of dying
#p-value = 0.002 < 0.05, so this is statistically significant 