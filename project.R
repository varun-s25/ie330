library(readxl)

#import data
data <- read_excel("C:/Users/Ninja/Downloads/IE330_Project_Data.xlsx")

#linear regression model
model <- lm(performance_index ~ hours_studied, data = data)

#summary of model
summary(model)

#calculating p value
p_value <- summary(model)$coefficients["hours_studied", "Pr(>|t|)"]

#checking if regression is significant
if(p_value < 0.05) {
  cat("The regression is significant (p-value =", p_value, ")\n")
} else {
  cat("The regression is not significant (p-value =", p_value, ")\n")
  
}

#confidence interval for slope and intercept
confidence <- confint(model)

cat("Confidence Interval for slope: \n")
print(confidence["hours_studied", ])

cat("Confidence Interval for intercept: \n")
print(confidence["(Intercept)", ])

#confidence interval for some value of x

#x value
x1 <- 6

#new data
new_data <- data.frame(hours_studied = x1)

#confidence interval
confidencex1 <- predict(model, newdata = new_data, interval = "confidence", level = .95)

cat("Confidence Interval for Mean Response (Hours Studied =", x1, "):\n")
cat("Lower Bound:", confidencex1[1], "\n")
cat("Upper Bound:", confidencex1[2], "\n")

#prediction interval
predictionx1 <- predict(model, newdata = new_data, interval = "prediction", level = 0.95)
cat("Prediction Interval for Mean Response (Hours Studied =", x1, "):\n")
cat("Lower Bound:", predictionx1[1], "\n")
cat("Upper Bound:", predictionx1[2], "\n")

#Residual Analysis
#Residuals v Fitted Plot
plot(model, which = 1)

#Normality of Residuals
#Q-Q plot of residuals
qqnorm(resid(model))
qqline(resid(model))

#R-Squared
summary(model)$r.squared

#Adjusted R-Squared
summary(model)$adj.r.squared

#MSE
mean((resid(model))^2)
