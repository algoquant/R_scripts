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
probab1 <- function(nu_m, p1, p2) {
  binom1 <- choose(nu_m, 0:nu_m) * p1^(0:nu_m) * (1-p1)^(nu_m:0)
  binom2 <- choose(nu_m, 0:nu_m) * p2^(0:nu_m) * (1-p2)^(nu_m:0)
  out_er <- binom1 %o% binom2
  sum(out_er[lower.tri(out_er)]) + sum(diag(out_er))/2
}  # end probab1

# Simplify the calculation by calculating the cumulative 
# probability, without calculating the outer product.
probab2 <- function(nu_m, p1, p2) {
  binom1 <- choose(nu_m, 0:nu_m) * p1^(0:nu_m) * (1-p1)^(nu_m:0)
  binom2 <- choose(nu_m, 0:nu_m) * p2^(0:nu_m) * (1-p2)^(nu_m:0)
  cum_binom2 <- cumsum(binom2)
  cum_binom2 <- c(0, cum_binom2[-NROW(cum_binom2)])
  sum(binom1 * (cum_binom2 + binom2/2))
}  # end probab2

probab2(5, 0.6, 0.5)

sapply(1:11, probab2, p1=0.6, p2=0.5)

