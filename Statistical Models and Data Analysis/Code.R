library(ggplot2)

emissionssw <- read.table("emissionssw.dat", header = TRUE)

ggplot(emissionssw, aes(x = noxem, y = nox)) + geom_point() + geom_smooth(method = "lm", col = "blue") + labs(title = "NOx␣vs␣NOx␣Emissions", x = "NOx␣Emissions␣(noxem)", y = "NOx␣Concentration␣(nox)")
ggsave("NOx_vs_noxem.png", plot = plot1, width = 8, height = 6, dpi = 300)

ggplot(emissionssw, aes(x = ws, y = nox)) + geom_point() + geom_smooth(method = "lm", col = "red") + labs(title = "NOx␣vs␣Wind␣Speed", x = "Wind␣Speed␣(ws)", y = "NOx␣Concentration␣(nox)")
ggsave("NOx_vs_WindSpeed.png", plot = plot2, width = 8, height = 6, dpi = 300)

ggplot(emissionssw, aes(x = humidity , y = nox)) + geom_point() + geom_smooth(method = "lm", col = "green") + labs(title = "NOx␣vs␣Humidity", x = "Humidity", y = "NOx␣Concentration␣(nox)")



ggplot(emissionssw, aes(x = sqrt(noxem), y = log(nox))) + geom_point() + geom_smooth(method = "lm", col = "blue") + labs(title = "log(NOx) vs sqrt(noxem)", x = "sqrt(noxem)", y = "log(NOx)")
ggsave("/Statistical Models and Data Analysis/NOx_vs_sqrtnoxem.png", plot = plot4, width = 8, height = 6, dpi = 300)

ggplot(emissionssw, aes(x = sqrt(ws), y = log(nox))) + geom_point() + geom_smooth(method = "lm", col = "red") + labs(title = "log(NOx) vs sqrt(Wind␣Speed)", x = "sqrt(Wind Speed)", y = "log(NOx)")
ggsave("/Statistical Models and Data Analysis/NOx_vs_sqrtws.png", plot = plot5, width = 8, height = 6, dpi = 300)

ggplot(emissionssw, aes(x = sqrt(humidity), y = log(nox))) + geom_point() + geom_smooth(method = "lm", col = "green") + labs(title = "log(NOx)␣vs␣sqrt(Humidity)", x = "sqrt(Humidity)", y = "log(NOx)")
ggsave("/Statistical Models and Data Analysis/NOx_vs_sqrthum.png", plot = plot6, width = 8, height = 6, dpi = 300)


# Model fitting using lm function
model_linear <- lm(nox ~ humidity + noxem + ws, data = train_data)
summary(model_linear)

# Calculate and print the model performance metrics
train_predictions <- predict(model_linear, newdata = train_data)
train_actual <- train_data$nox

ss_total_train <- sum((train_actual - mean(train_actual))^2)
ss_residual_train <- sum((train_actual - train_predictions)^2)

r_squared_train <- 1 - (ss_residual_train / ss_total_train)
mse_train <- mean((train_actual - train_predictions)^2)

mae_train <- mean(abs(train_actual - train_predictions)) 16
test_predictions <- predict(model_linear, newdata = test_data)
test_actual <- test_data$nox

mse_test <- mean((test_actual - test_predictions)^2)
ss_total_test <- sum((test_actual - mean(test_actual))^2)
ss_residual_test <- sum((test_actual - test_predictions)^2)
r_squared_test <- 1 - (ss_residual_test / ss_total_test)

mae_test <- mean(abs(test_actual - test_predictions)) 26
cat("Linear Model (Baseline Model) Performance:\n")
cat("Training Data:\n")
cat("Mean Squared Error (Training):", mse_train, "\n")
cat("Mean Absolute Error (Training):", mae_train, "\n")
cat("R-squared (Training):", r_squared_train, "\n")
cat("\nTesting␣Data:\n")
cat("Mean Squared Error (Testing):", mse_test, "\n")
cat("Mean Absolute Error (Testing):", mae_test, "\n")
cat("R-squared(Testing):", r_squared_test, "\n")
cat("AIC:", AIC(model_linear), "\n")