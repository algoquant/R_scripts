# You have two coins, and one coin has a higher probability 
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

probab_2(5, 0.6, 0.5)

sapply(1:11, probab_2, p1=0.6, p2=0.5)

