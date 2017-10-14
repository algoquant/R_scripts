##############################
### Permutation puzzle:
# There are n people and they randomly switch their hats (perform permutations).
# Calculate the average number of people who keep their own hat.
# Answer: on average only one person will keep their own hat, for all values of n.

## Proof using recursion
# Let avg(n) be equal to the average number of people in permutations of n elements who keep their own hat
# Add an extra element (n+1) to the n elements, and divide the permutations of (n+1) elements as follows:
# first, all the n! permutations where the extra element is unchanged,
# second, all the n*n! permutations where the extra element is changed (replaced by one of the n original elements),
# Then we can write avg(n+1) as equal to the sum over all the permutations:
avg(n+1) <- (avg(n) + n * avg(n-1)) / (n+1)

# So we get avg(n+1)=avg(n), 
# and since avg(1)=1, then it proves that avg(n)=1, for all values of n

# The above puzzle is equivalent to asking, what is the number of elements that remain unchanged in all the permutations of n elements?
# Answer: it's equal to the number of permutations of n elements.


## We can gain additional insight by using R to enumerate all the permutations of n elements, 
# and calculating the average number of people who keep their own hat.

# References about enumerating all permutations using R:
# https://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r

# Enumerate all permutations (create a matrix of all permutation numbers)
permuta_tions <- function(n) {
  if (n==1) {
    return(matrix(1))
  } else {
    sp <- permuta_tions(n-1)
    n_row <- NROW(sp)
    A <- matrix(nrow=n*n_row, ncol=n)
    for(i in 1:n){
      A[(i-1)*n_row + (1:n_row), ] <- cbind(i, sp + (sp>=i))
    }
    return(A)
  }  # end if
}  # end permuta_tions

# or
# a_ll <- expand.grid(p1=letters[1:3], p2=letters[1:3], p3=letters[1:3], stringsAsFactors=FALSE)
# permuta_tions <- a_ll[apply(a_ll, 1, function(x) {length(unique(x))==3}), ]

# or
# permuta_tions <- function( x, prefix=c()) {
#   if(length(x) == 0 ) return(prefix)
#   do.call(rbind, sapply(1:NROW(x), 
#                         FUN = function(idx) permuta_tions(x[-idx], c(prefix, x[idx])), simplify=FALSE))
# }  # end permuta_tions


# Enumerate all permutations (produce a matrix of all permutation numbers)
n_um <- 4
enumer_ations <- permuta_tions(n_um)
# enumerate all permutations of letters
# enumer_ations <- matrix(letters[1:n_um][permuta_tions(n_um)], ncol=n_um)

# Calculate number of elements that are unchanged (left in their original positions)?
num_elements <- sapply(1:NROW(enumer_ations), 
                       function(i) sum(enumer_ations[1, ]==enumer_ations[i, ]))
# enumerate the permutations where no elements are left in their original position
enumer_ations[num_elements==0, ]

# Calculate the average number of people who keep their own hat: it's always equal to 1
sum(num_elements) / factorial(n_um)


## Proof the hard way,
# by calculating the number of permutations where no elements are left in their original positions

# Calculate number of permutations where no elements are left unchanged in their original positions (recursive function).
# The number of permutations where no elements are left unchanged is equal to num_permutations(n).
num_permutations <- function(n) {
  if (n<4) {
    return(n-1)
  } else {
    # return((n-1)*(num_permutations(n-2) + (n-2)*(num_permutations(n-2)+num_permutations(n-3))))
    return((n-1)*((n-1)*num_permutations(n-2) + (n-2)*num_permutations(n-3)))
  }  # end if
}  # end num_permutations

num_permutations(1)
num_permutations(2)
num_permutations(3)
num_permutations(4)
num_permutations(5)


# Calculate number of permutations where no elements are left in their original positions,
# by performing recurrence on a vector.
perm_vector <- integer(20)
for (i in 1:3) perm_vector[i] <- (i-1)
for (i in 4:20) 
  perm_vector[i] <- (i-1)*((i-1)*perm_vector[i-2] + (i-2)*perm_vector[i-3])

perm_vector[1:11]


# calculate the total number of permutations in two equivalent ways
n_um <- 10
# first way:
factorial(n_um)
# second way:
# The number of permutations where no elements are left unchanged is equal to perm_vector[n].
# The number of permutations where exactly k elements are unchanged is equal to: (n choose k) times perm_vector[n-k]
# The total number of n permutations is equal to the sum over k: 
sum(choose(n_um, 0:(n_um-1)) * perm_vector[n_um:1]) + 1


# Calculate the average number of elements that remain unchanged.
n_um <- 10
(sum((0:(n_um-1)) * choose(n_um, 0:(n_um-1)) * perm_vector[n_um:1]) + n_um) / factorial(n_um)

# Calculate the average number of elements that remain unchanged, for a vector of element numbers:
sapply(2:20, function(n_um) 
  (sum((0:(n_um-1)) * choose(n_um, 0:(n_um-1)) * perm_vector[n_um:1]) + n_um) / factorial(n_um))


