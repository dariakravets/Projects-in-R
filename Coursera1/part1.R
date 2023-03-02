set.seed(5327)

lambda <- 0.2
n <- 40
size <- 1000

simulated_sample = NULL
for (i in 1 : size) simulated_sample = c(simulated_sample, mean(rexp(n, lambda)))

#sample mean vs. theoretical mean

sample_mean <- mean(simulated_sample)
theor_mean <- lambda^(-1)

mean_dif <- abs(sample_mean - theor_mean)

#sample variance vs. theoretical variance

sample_var <-var(simulated_sample)
theor_var <- 1 / (lambda^2 * n)

var_dif <- abs(sample_var - theor_var)


xx <- seq(min(simulated_sample), max(simulated_sample), length=1000)
hist(simulated_sample, breaks = n, freq=F, col='#0665a5', xlab='Means', ylab='Density', main='Histogram of means')
lines(xx, dnorm(xx, mean=sample_mean, sd=sd(simulated_sample)), col='red', lwd=2)

qqnorm(simulated_sample, xlab='Theoretical quantiles', ylab='Sample quantiles', main = 'Q-Q plot')
qqline(simulated_sample, col='red', lwd=2)
