## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################

mean(data$year_published)
sd(data$year_published)
summary(data$year_published)
table(data$year_published) 

mean(data$season_of_show)
sd(data$season_of_show)
summary(data$season_of_show)
table(data$season_of_show)

mean(data$women_led)
sd(data$women_led)
summary(data$women_led)
table(data$women_led)
\
mean(data$emotion)
sd(data$emotion)
summary(data$emotion)
table(data$emotion)

mean(data$effort)
sd(data$effort)
summary(data$effort)
table(data$effort)

mean(data$drama)
sd(data$drama)
summary(data$drama)
table(data$drama)

mean(data$excitement)
sd(data$excitement)
summary(data$excitement)
table(data$excitement)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
# BOX PLOT

  
ggplot(data, aes(x = drama, y = excitement)) +
  geom_boxplot() +
  labs(title = "Grey's Anatomy drama and excitement",
       x = "drama",
       y = "excitement") +
  theme_minimal()


##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(data$effort, data$excitement)
print(linear_plot)

meany <- mean(data$effort)
meanx <- mean(data$excitement)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")

linear_relationship <- lm(effort ~ excitement, data = data)
summary(linear_relationship)
abline(linear_relationship, col = "red")


##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################

# Plot the residuals
plot(data$excitement, residuals(linear_relationship))
# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

plot(data$effort, residuals(linear_relationship))
# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################

table(data$drama, data$emotion)




