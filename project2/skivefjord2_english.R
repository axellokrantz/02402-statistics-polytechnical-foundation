setwd("/Users/axel/Desktop/DTU/Statistics/project2-skivefjord")

# Read the dataset 'skivefjord2_data.csv' into R
D <- read.table("skivefjord2_data.csv", header = TRUE, sep = ";")

# Add log-chlorophyl to the dataset
D$logchlorophyl <- log(D$chlorophyl)


#################################################################

# A)

# Summary Statistics

# Descriptive analysis and summary of the data
# For totalP
cat("Summary for totalP:\n")
summary(D$totalP)

# For logchlorophyl
cat("\nSummary for logchlorophyl:\n")
summary(D$logchlorophyl)

# For temp
cat("\nSummary for temp:\n")
summary(D$temp)

# Number of observations for totalP
cat("Number of observations for totalP: ", length(D$totalP), "\n")

# Number of observations for temp
cat("Number of observations for temp: ", length(D$temp), "\n")

# Number of observations for logchlorophyl
cat("Number of observations for logchlorophyl: ", length(D$logchlorophyl), "\n")

# Standard deviation for totalP
cat("Standard deviation for totalP: ", sd(D$totalP), "\n")

# Standard deviation for temp
cat("Standard deviation for temp: ", sd(D$temp), "\n")

# Standard deviation for logchlorophyl
cat("Standard deviation for logchlorophyl: ", sd(D$logchlorophyl), "\n")

# Scatter plots

# Set up the graphics layout to have 1 row and 2 columns
par(mfrow=c(1,2))

# Scatter plot of log-chlorophyl against totalP
plot(D$logchlorophyl ~ D$totalP, xlab = "totalP", ylab = "log(chlorophyl)")

# Scatter plot of log-chlorophyl against temp
plot(D$logchlorophyl ~ D$temp, xlab = "temp", ylab = "log(chlorophyl)")

# Box plots

par(mfrow=c(1,3))  # arrange plots in 1 row and 3 columns

boxplot(D$totalP, main="Box plot of totalP", ylab="totalP")
boxplot(D$temp, main="Box plot of temp", ylab="temp")
boxplot(D$logchlorophyl, main="Box plot of log(chlorophyl)", ylab="log(chlorophyl)")

# Histograms

par(mfrow=c(1,3))  # arrange plots in 1 row and 3 columns

hist(D$totalP, main="Histogram of totalP", xlab="totalP")
hist(D$temp, main="Histogram of temp", xlab="temp")
hist(D$logchlorophyl, main="Histogram of log(chlorophyl)", xlab="log(chlorophyl)")

#################################################################

# Subset containing the first 234 observations (for model estimation)
D_model <- D[1:234, ]

# Subset containing the last 6 observations (for validation)
D_test <- D[235:240,]

# Estimate multiple linear regression model
fit <- lm(logchlorophyl ~ totalP + temp, data = D_model)

# Show parameter estimates etc.
summary(fit)

# Add squared-totalP to the dataset
D_model$squaredtotalP <- (D_model$totalP)^2

# Estimate multiple linear regression model with squared-transformed totalP
fit_squaredtotalP <- lm(logchlorophyl ~ squaredtotalP + temp, data = D_model)

# Show parameter estimates etc.
summary(fit_squaredtotalP)

# Residuals against each of the explanatory variables
plot(D_model$totalP, fit$residuals, 
     xlab = "totalP concentration", ylab = "Residuals")

# Residuals against each of the explanatory variables
plot(D_model$temp, fit$residuals, 
     xlab = "Surface Temprature", ylab = "Residuals")

# Observations against fitted values
plot(fit$fitted.values, D_model$logchlorophyl, 
     xlab = "Fitted values", ylab = "log(chlorophyl concentration)")

# Residuals against fitted values
plot(fit$fitted.values, fit$residuals, xlab = "Fitted values", 
     ylab = "Residuals")

# Normal QQ-plot of the residuals
qqnorm(fit$residuals, ylab = "Residuals", xlab = "Z-scores", 
       main = "")
qqline(fit$residuals)

# Confidence 95% interval with 231 df
qt(0.975, df = 231)

# Confidence intervals for the model coefficients
confint(fit, level = 0.95)

#################################################################

# F)
# Hypothesis testing for null hypothesis: b1 = 5

tobs = -2.476650135

p_value <- 2 * (1 - pt(abs(tobs), 231))
print(p_value)

#################################################################

# G)
# Estimate multiple linear regression model

confint(lm(logchlorophyl ~ totalP + temp, data = D_model))

#################################################################

# H)

fit <- lm(logchlorophyl ~ totalP + temp, data = D_model)

# Predictions and 95% prediction intervals
pred <- predict(fit, newdata = D_test, 
                interval = "prediction", level = 0.95)

# Observed values and predictions
cbind(year = D_test$year, month = D_test$month, 
      logchlorophyl = D_test$logchlorophyl, pred)

#################################################################
