# Fama-MacBeth regression
# https://github.com/charlesbeck3/empirical-finance/blob/master/Assignment2/assignment2.R

# include necessary libararies

# load data
ff25dat <- read.csv("data/FF25.csv")
ff3dat <- read.csv("data/ff3.csv")

# convert ff3 data to same percentage format 0.01 for 1%
ff3dat[, 3:6] <- ff3dat[, 3:6] / 100.0

# merge datasets default merge on matching column names (i.e. year, month)
modeldat <- merge(ff25dat, ff3dat)

# create consolidated period field
modeldat$period <- modeldat$year * 100 + modeldat$month

# Restrict data sample to between Jan/1946 to Dec/2011
modeldat <- modeldat[modeldat$period >= 194601 & modeldat$period <= 201112, ]

# select the list of portfolios
portfolios <- names(modeldat)[grepl("_vwret", names(modeldat))]

# Compute excess returns from raw returns on the 25 portfolios
# (Subtract RF from vwret for each portfolio)
modeldat[, portfolios] <- modeldat[, portfolios] - modeldat$RF

# Rename the columns to match reality (e.g. vwret to exret)
names(modeldat) <- gsub("_vwret", "_exret", names(modeldat))
portfolios <- gsub("_vwret", "_exret", portfolios)

# Create variables to hold the regression coefficient estimates
ols1.coef <- list()
ols2.coef <- list()
ols2.models <- list()

# create a list of factors for regression
ind.vars <- c("MKT", "SMB", "HML")

# Perform the 2-pass Fama-Macbeth regression
# First pass, for each portfolio
for(portfolio in portfolios)
{
  # create regression formula
  formula <- paste(c(portfolio, "~",paste(ind.vars, collapse=" + ")), collapse=" ")
  
  # time series regression of exvwret for each portfolio on MRP
  fit <- lm(formula, data=modeldat)
  
  # add current coefficient estimates to the list
  ols1.coef[[paste(portfolio, sep="")]] <- coef(fit)
}

# convert ols.coef list to matrix
ols1.coef <- do.call(rbind, ols1.coef)

# Second pass, for each time period
for(period in unique(modeldat$period))
{
  # select subset of period returns for given portfolios
  period.return <- unlist(modeldat[modeldat$period == period, portfolios])
  
  # regress expected returns for each time period on the given factors (excluding intercept) 
  fit <- lm(period.return ~ ., data=data.frame(period.return, ols1.coef[,ind.vars]))
  
  # save the model
  ols2.models[[paste(period, sep="")]] <- fit
  
  # store the coefficients estimates from the cross sectional regression
  ols2.coef[[paste(period, sep="")]] <- coef(fit)
}

# convert ols.coef list to matrix
ols2.coef <- do.call(rbind, ols2.coef)

# calculate the mean estimate for each of the coefficients across time
ols2.coef.mean = apply(ols2.coef, MARGIN=2, mean)

# calculate the sample standard deviation for each estimator without Shanken correction
ols2.coef.se = apply(ols2.coef, MARGIN=2, sd) / sqrt(length(ols2.coef[,1]))

# calculate std.dev for us in Shanken calculation
std.dev <-apply(ols2.coef, MARGIN=2, sd)

# Shanken standard error correction for the standard error of each estimator
ols2.coef.shanken.se = sqrt(std.dev^2 * (1 + ols2.coef.mean / std.dev^2)) / sqrt(length(ols2.coef[,1]))

# Compute and report t-statistics, with and without the Shanken’s correction
# Ho = coefficient = 0
ols2.coef.t <- ols2.coef.mean / ols2.coef.se
  
# Compute and report t-statistics, with and without the Shanken’s correction
# Ho = coefficient = 0
ols2.coef.shanken.t <- ols2.coef.mean / ols2.coef.shanken.se
  
# Compute the R-square for each coeff
ols2.coef.rsq <- do.call(rbind, lapply(ols2.models, FUN=function(x) summary(x)$r.square))

# Compute the adjusted R-square for each coeff
ols2.coef.adj.rsq <- do.call(rbind, lapply(ols2.models, FUN=function(x) summary(x)$adj.r.square))

# Compile and print report
report <- rbind(mean = ols2.coef.mean,
                se = ols2.coef.se,
                shanken.se = ols2.coef.shanken.se,
                t = ols2.coef.t,
                shanken.t = ols2.coef.shanken.t)
print(report)

