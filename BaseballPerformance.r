teams <- read.csv("/Users/keerthanakola/Downloads/BaseballRegression/data/Teams.csv")
head(teams)

salaries <- read.csv("/Users/keerthanakola/Downloads/BaseballRegression/data/Salaries.csv")
head(salaries)

install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

salaries_aggregated <- salaries %>%
  group_by(teamID, yearID) %>%
  summarise(total_payroll = sum(salary))

print(salaries_aggregated)

team_data <- teams %>%
  inner_join(salaries_aggregated, by = c("teamID", "yearID"))


head(team_data)


ggplot(team_data, aes(x = total_payroll, y = W)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Wins vs Total Payroll", x = "Total Payroll", y = "Wins")




ggplot(team_data, aes(x = R, y = W)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Wins vs Runs Scored", x = "Runs Scored", y = "Wins")




ggplot(team_data, aes(x = ERA, y = W)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Wins vs ERA", x = "ERA", y = "Wins")


cor(team_data %>% select(W, total_payroll, R, ERA), use = "complete.obs")


 install.packages("stargazer")


model <- lm(W ~ total_payroll + R + ERA, data = team_data)


library(stargazer)
stargazer(model, type = "text", title = "Regression Results", digits = 3)


model <- lm(W ~ total_payroll + R + ERA, data = team_data)
summary(model)


plot(model, which = 1)  # Residuals vs Fitted


plot(model, which = 2)  # QQ Plot

install.packages("caTools")
library(caTools)

set.seed(123)
split <- sample.split(team_data$W, SplitRatio = 0.7)
train_data <- subset(team_data, split == TRUE)
test_data <- subset(team_data, split == FALSE)
train_model <- lm(W ~ total_payroll + R + ERA, data = train_data)
predictions <- predict(train_model, newdata = test_data)




actual <- test_data$W
mean_squared_error <- mean((predictions - actual)^2)
print(paste("Mean Squared Error:", mean_squared_error))


#finding the teams with the highest salaries
highest_salary_teams <- team_data %>%
  group_by(teamID) %>%
  summarise(max_payroll = max(total_payroll, na.rm = TRUE)) %>%
  arrange(desc(max_payroll))


top_5_highest_salaries <- head(highest_salary_teams, 5)
print(top_5_highest_salaries)


