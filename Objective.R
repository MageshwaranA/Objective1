#To demonstrate the use of probability as a foundation of statistical modeling,
#including inference and maximum likelihood estimation, 
#we can use the built-in data source in R, called "mtcars".

#The mtcars dataset contains information about various car models, 
#including their miles per gallon (mpg), horsepower, and weight, among other variables.

#We can use this dataset to calculate the likelihood function
#and maximum likelihood estimates for a simple linear regression model that 
#predicts mpg based on horsepower.


# Load the mtcars dataset
data(mtcars)

# Create a simple linear regression model
model <- lm(mpg ~ hp, data = mtcars)

# Calculate the likelihood function
likelihood <- function(beta, x, y) {
  mu <- beta[1] + beta[2]*x
  -sum(dnorm(y, mean = mu, sd = 1, log = TRUE))
}

# Find the maximum likelihood estimates for the model
fit <- optim(c(0,0), likelihood, x = mtcars$hp, y = mtcars$mpg)

# Print the results
summary(model)
cat("Maximum Likelihood Estimates:\n")
cat("Intercept: ", fit$par[1], "\n")
cat("Slope: ", fit$par[2], "\n")

