install.packages("MASS")
library(MASS)


hours <- 8 
mean_efficiency <- c(10, 10) 
std_dev <- c(5, 5) 
correlation_matrix <- matrix(c(1, 0.3, 0.3, 1), 2, 2) 

# Simulate 
set.seed(123)
efficiencies <- mvrnorm(n = hours, mu = mean_efficiency, Sigma = std_dev %*% t(std_dev) * correlation_matrix)

# Convert to data frame
efficiencies_df <- data.frame(hour = 1:hours,
                              Employee1 = round(efficiencies[,1]),
                              Employee2 = round(efficiencies[,2]))
print(efficiencies_df)

## Tests

#. 1. Correlation Test:
cor.test(efficiencies_df$Employee1, efficiencies_df$Employee2)

# 2. Consistency Test
plot(efficiencies_df$hour, efficiencies_df$Employee1, type = 'b', col = 'blue', ylim = c(0, max(efficiencies_df[,2:3])),
     xlab = "Hour", ylab = "Customers Served", main = "Employee Efficiency Over Time")
lines(efficiencies_df$hour, efficiencies_df$Employee2, type = 'b', col = 'red')
legend("topright", legend = c("Employee 1", "Employee 2"), col = c("blue", "red"), lty = 1)

total_served <- colSums(efficiencies_df[,2:3])
print(total_served)

# 3. Variability Test
sd_employee1 <- sd(efficiencies_df$Employee1)
sd_employee2 <- sd(efficiencies_df$Employee2)
print(c(sd_employee1, sd_employee2))

# 4. Regression Test
lm1 <- lm(Employee1 ~ hour, data = efficiencies_df)
lm2 <- lm(Employee2 ~ hour, data = efficiencies_df)
summary(lm1)
summary(lm2)



# 5. ANOVA Test for hourly efficiency 
anova1 <- aov(Employee1 ~ factor(hour), data = efficiencies_df)
anova2 <- aov(Employee2 ~ factor(hour), data = efficiencies_df)
summary(anova1)
summary(anova2)


