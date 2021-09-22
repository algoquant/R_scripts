########################
### Scripts for testing manager skill


## test mean of coin flips and distributions

# unbiased coin tosses (binomial)
rbinom(n=20, size=1, prob=0.5)

# Kolmogorov-Smirnov test for two distributions
ks.test(rbinom(n=1000, size=1, prob=0.6), 
        rbinom(n=1000, size=1, prob=0.5))

# Wilcoxon test for the means of two samples
wilcox.test(rbinom(n=1000, size=1, prob=0.6), 
            rbinom(n=1000, size=1, prob=0.5))


# other tests
# To calculate the confidence level as a function of sample length,
# you must perform MC simulation to obtain distribution of test statistic.
foo <- sapply(1:1000, function(x) tseries::jarque.bera.test(rnorm(1000))$p.value)
foo <- t(foo)


########################
## You have two coins, and one coin has a higher probability 
# of heads than the other.
# Assume that you know the probability of heads for both coins, 
# but you don't know which coin has a higher probability, 
# and you try to find out by flipping the two coins n times each.
# You guess which coin has the higher probability by selecting 
# the coin that produced more heads.
# If both coins produced the same number of heads, then you 
# select the coin randomly.
# What is the probability that you will select the right coin, 
# given all the possible combinations of obtaining heads or 
# tails for both coins? 

# Calculate the probability by summing over all possible 
# combinations of obtaining heads or tails for both coins. 
probab_1 <- function(nu_m, p1, p2) {
  binom_1 <- choose(nu_m, 0:nu_m) * p1^(0:nu_m) * (1-p1)^(nu_m:0)
  binom_2 <- choose(nu_m, 0:nu_m) * p2^(0:nu_m) * (1-p2)^(nu_m:0)
  out_er <- binom_1 %o% binom_2
  sum(out_er[lower.tri(out_er)]) + sum(diag(out_er))/2
}  # end probab_1

# Simplify the calculation by calculating the cumulative 
# probability, without calculating the outer product.
probab_2 <- function(nu_m, p1, p2) {
  binom_1 <- choose(nu_m, 0:nu_m) * p1^(0:nu_m) * (1-p1)^(nu_m:0)
  binom_2 <- choose(nu_m, 0:nu_m) * p2^(0:nu_m) * (1-p2)^(nu_m:0)
  cum_binom_2 <- cumsum(binom_2)
  cum_binom_2 <- c(0, cum_binom_2[-NROW(cum_binom_2)])
  sum(binom_1 * (cum_binom_2 + binom_2/2))
}  # end probab_2

# Use normal approximation for the binomial coefficient.
probab_3 <- function(nu_m, p1, p2) {
  if (p1^nu_m > 1e-10)
    binom_1 <- choose(nu_m, 0:nu_m) * p1^(0:nu_m) * (1-p1)^(nu_m:0)
  else
    binom_1 <- dnorm(0:nu_m, mean=nu_m*p1, sd=sqrt(nu_m*p1*(1-p1)))
  if (p2^nu_m > 1e-10)
    binom_2 <- choose(nu_m, 0:nu_m) * p2^(0:nu_m) * (1-p2)^(nu_m:0)
  else
    binom_2 <- dnorm(0:nu_m, mean=nu_m*p2, sd=sqrt(nu_m*p2*(1-p2)))
  cum_binom_2 <- cumsum(binom_2)
  cum_binom_2 <- c(0, cum_binom_2[-NROW(cum_binom_2)])
  sum(binom_1 * (cum_binom_2 + binom_2/2))
}  # end probab_3

probab_3(5, 0.6, 0.5)

prob_s <- sapply(1:500, probab_3, p1=0.6, p2=0.5)

# plot in window
x11()
plot(prob_s,
     xlab="number of coin flips", 
     ylab="probability", 
     main="probability of guessing correct coin", 
     lwd=2, col="blue", t="l")
abline(h=0.95, lwd=2, col="red")
text(x=50, y=0.95,
     labels="0.95 probability",
     lwd=2, pos=3)
abline(v=findInterval(0.95, prob_s), lwd=2, col="red")
text(x=findInterval(0.95, prob_s), y=0.8,
     labels=paste(findInterval(0.95, prob_s), "coin flips"),
     srt=90, lwd=2, pos=2)



########################
## The binomial coin flipping model can be adapted for manager selection.
# Imagine two managers, one with skill, and the other without skill.
# The daily returns of both managers are binary: either positive +1 or negative -1.
# The returns of the manager without skill are random with zero mean.
# The returns of the skilled manager are also random, but with a positive bias, so that he produces a positive daily return more often than a negative return.
# If the probability of a positive return is equal to p > 0.5, then the average daily return is equal to (2*p-1), and the daily standard deviation is equal to 1.
# The annual return is is equal to 250*(2*p-1), the annual standard deviation is equal to sqrt(250), and the annual Sharpe ratio is equal to sqrt(250)*(2*p-1).

sharpe_ratio <- sqrt(250)*(2*prob_ab-1)

sharpe_ratio <- 0.4
prob_ab <- (sharpe_ratio/sqrt(250)+1)/2

# probability of selecting correct manager with 20 years of data
probab_3(20*250, prob_ab, 0.5)

# annual probabilities of selecting correct manager
prob_s <- sapply(250*(1:50), probab_3, p1=prob_ab, p2=0.5)

# Years of data needed to select the correct manager, with 95% confidence
findInterval(0.95, prob_s)

# plot in window
x11()
plot(prob_s,
     xlab="number of years", 
     ylab="probability", 
     main="probability of selecting correct manager", 
     lwd=2, col="blue", t="l")
abline(h=0.95, lwd=2, col="red")
text(x=10, y=0.95,
     labels="0.95 probability",
     lwd=2, pos=3)
abline(v=findInterval(0.95, prob_s), lwd=2, col="red")
text(x=findInterval(0.95, prob_s), y=0.8,
     labels=paste(findInterval(0.95, prob_s), "years"),
     srt=90, lwd=2, pos=2)

