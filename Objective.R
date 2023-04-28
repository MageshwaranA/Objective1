
data(iris)


sepal_length <- iris$Sepal.Length


sample_mean <- mean(sepal_length)
sample_sd <- sd(sepal_length)


normal_dist <- function(x, mu, sd) {
  (1/(sd * sqrt(2*pi))) * exp(-((x-mu)^2)/(2*sd^2))
}

hist(sepal_length, main = "Histogram of Sepal Length", xlab = "Sepal Length")


curve(normal_dist(x, sample_mean, sample_sd), col = "red", add = TRUE)


pnorm(5.5, mean = sample_mean, sd = sample_sd)


log_likelihood <- function(mu, sd) {
  -sum(dnorm(sepal_length, mean = mu, sd = sample_sd, log = TRUE))
}


mle <- optimize(log_likelihood, interval = c(0,10), maximum = TRUE)
mu_mle <- mle$maximum
sd_mle <- mu_mle/mle$objective


cat("Maximum Likelihood Estimates:\n")
cat("mu = ", mu_mle, "\n")
cat("sd = ", sd_mle, "\n")
