m <- 1000            #number of replicas
mu_verd <- 0       #true value of the population parameter
mu_test <- c(seq(-6.5, 7, .54))   #test values
M <- length(mu_test)              #number of Monte Carlo replicas
power <- numeric(M)               #vector to storage the empirical power of the test
nobs <- c(10, 20, 30, 80)        #sampling size
power_nobs <- matrix(0,length(nobs),M)  #matrix to storage the empirical power of the test for each size n
c <- 1
for(j in nobs) {
  for (i in 1:M) {
    mu <- mu_test[i]
    p_value <- replicate(m, expr = {
      x <- runif(j, mu - sqrt(3), mu + sqrt(3))
      s_test <- SignTest(x, mu = mu_verd)
      s_test$p.value})
    power[i] <- mean(p_value <= 0.05)
  }
  power_nobs[c,] <- power
  c = c+1
}

#Plots to see the performance varying n
par(mfrow=c(2,2))
plot(mu_test, power_nobs[1,], type = "l", xlab = bquote(theta), ylab = "Power", main = "n = 10")
abline(v = mu_verd, lty = 1)
abline(h = .05, lty = 1)
plot(mu_test, power_nobs[2,], type = "l", xlab = bquote(theta), ylab = "Power", main = "n = 20")
abline(v = mu_verd, lty = 1)
abline(h = .05, lty = 1)
plot(mu_test, power_nobs[3,], type = "l", xlab = bquote(theta), ylab = "Power", main = "n = 30")
abline(v = mu_verd, lty = 1)
abline(h = .05, lty = 1)
plot(mu_test, power_nobs[4,], type = "l", xlab = bquote(theta), ylab = "Power", main = "n = 80", ylim = c(-.001, 1))
abline(v = mu_verd, lty = 1)
abline(h = .05, lty = 1)
