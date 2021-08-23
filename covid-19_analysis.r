rm(list=ls()) # removes all variables stored previously
library(Hmisc) # import
#importing covid 19 data set below and storing it in a variable "c19_data"
c19_data <- read_csv("C:/ALL/R_Programming/Covid_R/COVID19_line_list_data.csv")
describe(c19_data) #Having an overview of the data set using the describe command associated with (Hmisc) package
#the death column contains some inconsistent data, which need to be cleaned out
#first use the unique command to know the unique values
#then assign death "1" to anything other than alive "0"

c19_data$death_dummy <- as.integer(c19_data$death != 0) # cleaned up death column
# calculating the death rate of the population
sum(c19_data$death_dummy) / nrow(c19_data) #total deaths/ total population

# DEATH AGE CLAIM
# claim: people who die are older

#creating variable death by sorting death as cleaned data from dataset using the subset command
dead = subset(c19_data, death_dummy == 1) 

#creating variable alive by sorting alive as a cleaned data from dataset using the subset command
alive = subset(c19_data, death_dummy == 0)

#finding the average age of people from subset "dead" data
mean(dead$age, na.rm = TRUE)

#finding the average age of people from subset "alive" data
mean(alive$age, na.rm = TRUE)

# is this statistically significant?
#determine the credibility of this claim with the t- test
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99)
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant

# GENDER vunerability 
# claim: gender has no effect

#creating variable "men" by sorting the dataset by gender and  using the subset command
men = subset(c19_data, gender == "male")

#creating variable "women" by sorting the dataset by gender and  using the subset command
women = subset(c19_data, gender == "female")

#finding the average death of people from subset "men" data
mean(men$death_dummy, na.rm = TRUE) #8.5%!

#finding the average death of people from subset "women" data
mean(women$death_dummy, na.rm = TRUE) #3.7%

# is this statistically significant?
#determine the credibility of this claim with the t- test
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# 99% confidence: men have from 0.8% to 8.8% higher chance
# of dying.
# p-value = 0.002 < 0.05, so this is statistically
# significant