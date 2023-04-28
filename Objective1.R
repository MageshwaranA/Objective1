data(mtcars)
head(mtcars)
plot(mtcars$wt, mtcars$mpg, xlab = "Weight", ylab = "Miles per Gallon")
model <- glm(mpg ~ wt, data = mtcars, family = poisson(link = "log"))
summary(model)
new_data <- data.frame(wt = c(2.5, 3.0, 3.5))
predicted_values <- predict(model, newdata = new_data, type = "response")
predicted_values
plot(new_data$wt, predicted_values, type = "l", xlab = "Weight", ylab = "Predicted Miles per Gallon")