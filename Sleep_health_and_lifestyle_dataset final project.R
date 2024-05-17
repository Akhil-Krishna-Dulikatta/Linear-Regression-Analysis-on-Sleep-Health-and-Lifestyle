
Sleep_health_and_lifestyle_dataset <- read.csv("C:\\Users\\akhil\\OneDrive\\Documents\\Sleep_health_and_lifestyle_dataset.csv")

#  head of the dataset
head(Sleep_health_and_lifestyle_dataset)

# second-order linear regression model
model_second_order <- lm(Heart.Rate ~ Age + Sleep.Duration + Quality.of.Sleep + Physical.Activity.Level + Stress.Level + Daily.Steps +
                           Age.2 + Sleep.Duration.2 + Quality.of.Sleep.2 + Physical.Activity.Level.2 + Stress.Level.2 + Daily.Steps.2 +
                           Age.Sleep.Duration + Age.Quality.of.Sleep + Age.Physical.Activity.Level + Age.Stress.Level + Age.Daily.Steps +
                           Sleep.Duration.Quality.of.Sleep + Sleep.Duration.Physical.Activity.Level + Sleep.Duration.Stress.Level + Sleep.Duration.Daily.Steps +
                           Quality.of.Sleep.Physical.Activity.Level + Quality.of.Sleep.Stress.Level + Quality.of.Sleep.Daily.Steps +
                           Physical.Activity.Level.Stress.Level + Physical.Activity.Level.Daily.Steps +
                           Stress.Level.Daily.Steps,
                         data = Sleep_health_and_lifestyle_dataset)

#  coefficients
coefficients <- coef(model_second_order)

#  R-squared
r_squared <- summary(model_second_order)$r.squared

#  Adjusted R-squared
adjusted_r_squared <- summary(model_second_order)$adj.r.squared

#  Sum of Squares Total (SST)
sst <- sum((Sleep_health_and_lifestyle_dataset$Heart.Rate - mean(Sleep_health_and_lifestyle_dataset$Heart.Rate))^2)

#  Sum of Squares Regression (SSR)
ssr <- sum(model_second_order$fitted.values - mean(Sleep_health_and_lifestyle_dataset$Heart.Rate))^2

#  Sum of Squares Error (SSE)
sse <- sum(model_second_order$residuals^2)

#  degrees of freedom
model_summary <- summary(model_second_order)
df_model <- model_summary$df[1]
df_residual <- model_summary$df[2]

#  Mean Squared Errors
mse_regression <- ssr / df_model
mse_error <- sse / df_residual

#  F-statistic
f_statistic <- mse_regression / mse_error

#  Standard Errors
standard_errors <- sqrt(diag(vcov(model_second_order)))

#  t-values
t_values <- coefficients / standard_errors

#  p-values
p_values <- 2 * pt(abs(t_values), df = df_residual, lower.tail = FALSE)

#  Confidence Intervals
conf_int <- confint(model_second_order)


# Obtain residuals
residuals <- residuals(model_second_order)

# Convert residuals to numeric type
residuals <- as.numeric(residuals)

# Create diagnostic plots
plot(model_second_order, which = 1)  # Scatterplot of standardized residuals vs. standardized predicted values
plot(model_second_order, which = 2)  # QQ plot of residuals
plot(model_second_order, which = 3)  # Residuals vs. fitted values

# Identify potential outliers based on residuals
outliers <- which(abs(residuals) > 2 * sd(residuals))
outliers


# Print the coefficients
cat("Coefficients:\n")
print(coefficients)

# Print R-squared
cat("R-squared (R²):", r_squared, "\n")

# Print Adjusted R-squared
cat("Adjusted R-squared:", adjusted_r_squared, "\n")

# Print Sum of Squares Total (SST)
cat("Sum of Squares Total (SST):", sst, "\n")

# Print Sum of Squares Regression (SSR)
cat("Sum of Squares Regression (SSR):", ssr, "\n")

# Print Sum of Squares Error (SSE)
cat("Sum of Squares Error (SSE):", sse, "\n")

# Print degrees of freedom
cat("Degrees of Freedom:\n")
cat("Model:", df_model, "\n")
cat("Residual:", df_residual, "\n")

# Print mean squares
cat("Mean Squares:\n")
cat("Regression:", mse_regression, "\n")
cat("Error:", mse_error, "\n")

# Print F-statistic
cat("F-statistic:", f_statistic, "\n")

# Print Standard Errors
cat("Standard Errors:\n")
print(standard_errors)

# Print t-values
cat("t-values:\n")
print(t_values)

# Print p-values
cat("p-values:\n")
print(p_values)

# Print Confidence Intervals
cat("Confidence Intervals:\n")
print(conf_int)


# correlation matrix
correlation_matrix <- cor(Sleep_health_and_lifestyle_dataset[, c("Heart.Rate", "Age", "Sleep.Duration", "Quality.of.Sleep", "Physical.Activity.Level", "Stress.Level", "Daily.Steps")])
print(correlation_matrix)

#  pairplot with linear regression lines Heart.Rate vs Stress.Level
library(ggplot2)
ggplot(Sleep_health_and_lifestyle_dataset, aes(x = Heart.Rate, y = Stress.Level)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Heart Rate", y = "Stress Level") +
  ggtitle("Heart Rate vs Stress Level")

# boxplot of Sleep Quality by Heart Rate
ggplot(Sleep_health_and_lifestyle_dataset, aes(x = as.factor(Heart.Rate), y = Quality.of.Sleep)) +
  geom_boxplot() +
  labs(x = "Heart Rate", y = "Sleep Quality") +
  ggtitle("Boxplot of Sleep Quality by Heart Rate")

# age groups
age_groups <- c(20, 30, 40, 50, 60)

# Add age group column to the dataframe
Sleep_health_and_lifestyle_dataset$Age.Group <- cut(Sleep_health_and_lifestyle_dataset$Age, breaks = age_groups, labels = c("20-30", "30-40", "40-50", "50-60"))

# the plot
ggplot(Sleep_health_and_lifestyle_dataset, aes(x = Sleep.Duration, fill = as.factor(Stress.Level))) +
  geom_bar(position = "stack") +
  facet_wrap(~Age.Group) +
  labs(x = "Sleep Duration", y = "Count") +
  ggtitle("Stacked Bar Chart of Sleep Duration and Stress Level")

#  countplot of Age by Quality of Sleep
ggplot(Sleep_health_and_lifestyle_dataset, aes(x = as.factor(Age), fill = as.factor(Quality.of.Sleep))) +
  geom_bar(position = "fill") +
  labs(x = "Age", y = "Proportion") +
  ggtitle("Countplot of Age by Quality of Sleep")

# Scatter plot for visualization (Heart.Rate vs Daily.Steps)
ggplot(Sleep_health_and_lifestyle_dataset, aes(x = Heart.Rate, y = Daily.Steps)) +
  geom_point() +
  labs(x = "Heart Rate", y = "Daily Steps") +
  ggtitle("Heart Rate vs Daily Steps")



# Predictive model for Physical.Activity.Level when Heart.Rate is 150
model_physical_activity <- lm(Physical.Activity.Level ~ Heart.Rate, data = Sleep_health_and_lifestyle_dataset)
heart_rate <- 150
predicted_physical_activity <- predict(model_physical_activity, newdata = data.frame(Heart.Rate = heart_rate))
predicted_physical_activity

# Plot the predictive model for Physical.Activity.Level when Heart.Rate is 150
library(ggplot2)
ggplot(Sleep_health_and_lifestyle_dataset, aes(x = Heart.Rate, y = Physical.Activity.Level)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(x = heart_rate, y = predicted_physical_activity, color = "blue", size = 5) +
  labs(x = "Heart Rate", y = "Physical Activity Level") +
  ggtitle("Predictive Model for Physical Activity Level at Heart Rate 150")

# Predictive model for Stress.Level when Daily.Steps >= 6000 and Age between 40-50
subset_data <- subset(Sleep_health_and_lifestyle_dataset, Daily.Steps >= 6000 & Age >= 40 & Age <= 50)
model_stress_level <- lm(Stress.Level ~ Daily.Steps, data = subset_data)
predicted_stress_level <- predict(model_stress_level, newdata = data.frame(Daily.Steps = 10000))
predicted_stress_level

# Plot the predictive model for Stress.Level when Daily.Steps >= 6000 and Age between 40-50
ggplot(subset_data, aes(x = Daily.Steps, y = Stress.Level)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(x = 10000, y = predicted_stress_level, color = "blue", size = 5) +
  labs(x = "Daily Steps", y = "Stress Level") +
  ggtitle("Predictive Model for Stress Level at Daily Steps >= 6000 and Age between 40-50")

# Predictive model for Sleep.Duration when Heart.Rate is 100
model_sleep_duration <- lm(Sleep.Duration ~ Heart.Rate, data = Sleep_health_and_lifestyle_dataset)
heart_rate <- 100
predicted_sleep_duration <- predict(model_sleep_duration, newdata = data.frame(Heart.Rate = heart_rate))
predicted_sleep_duration

# Plot the predictive model for Sleep.Duration when Heart.Rate is 100
ggplot(Sleep_health_and_lifestyle_dataset, aes(x = Heart.Rate, y = Sleep.Duration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(x = heart_rate, y = predicted_sleep_duration, color = "blue", size = 5) +
  labs(x = "Heart Rate", y = "Sleep Duration") +
  ggtitle("Predictive Model for Sleep Duration at Heart Rate 100")








