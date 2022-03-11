
############## hw
# Summary: Calculate the betas for a list of ETFs.
# Calculate the beta convexity (skew) as the difference 
# between bull-market and bear-market betas.

# 1. (30pts) Create a function called calc_betas() which
# performs a regression and returns a vector of betas.
# calc_betas() should accept the following arguments:
#   se_ries - time series of asset returns,
#   mar_ket - time series of market returns,
#   calc_bull_bear - Boolean if TRUE then calculate the 
#     bull-market and bear-market betas, else only the 
#     single beta.  Default is FALSE.
#   thresh_old - threshold level for market returns.
#     For bull-market only select returns above thresh_old.
#     For bear-market only select returns below -thresh_old.
#     Default is thresh_old=0.01.
# 
# You can use the functions lm(), summary(), and c().

calc_betas <- function(se_ries, mar_ket, calc_bull_bear=FALSE, thresh_old=0.01) {
  # calculate beta
  betav <- summary(lm(se_ries ~ mar_ket))$coefficients[2, 1]
  if (calc_bull_bear) {
    # calculate bull beta
    series_sub <- se_ries[mar_ket>thresh_old]
    market_sub <- mar_ket[mar_ket>thresh_old]
    bull_beta <- summary(lm(series_sub ~ market_sub))$coefficients[2, 1]
    # calculate bear beta
    series_sub <- se_ries[mar_ket<(-thresh_old)]
    market_sub <- mar_ket[mar_ket<(-thresh_old)]
    bear_beta <- summary(lm(series_sub ~ market_sub))$coefficients[2, 1]
    c(beta=betav, bull_beta=bull_beta, bear_beta=bear_beta)
  } else
    betav  # return single beta
}  # end calc_betas

# Extract all the columns from rutils::etfenv$returns, 
# excluding "VXX" and "SVXY", and call it returns.
# Then remove from returns any rows with NA values.
# You can use the functions na.omit(() and NCOL().

ncols <- NCOL(rutils::etfenv$returns)
returns <- na.omit(rutils::etfenv$returns[, -(.n_cols-1).n_cols)])

# You should get the following output:
# > colnames(returns)
#  [1] "VTI" "VEU" "IEF" "VNQ" "DBC" "XLY" "XLP" "XLE" "XLF" "XLV"
# [11] "XLI" "XLB" "XLK" "XLU" "VYM" "IVW" "IWB" "IWD" "IWF"
# 
# > dim(returns)
# [1] 2870   19

# Call calc_betas() to verify that it works correctly.
# 
# You should get the following output:
# > calc_betas(se_ries=returns$XLB, mar_ket=returns$VTI)
# [1] 1.069499
# 
# > calc_betas(se_ries=returns$XLB, mar_ket=returns$VTI, calc_bull_bear=TRUE)
#      beta bull_beta bear_beta 
# 1.0694993 0.7825841 1.0732377


# 2. (20pts) Perform an sapply loop over the columns 
# of returns, and apply calc_betas() all the columns 
# excluding the first one "VTI".
# Pass the arguments: 
# mar_ket=returns$VTI, calc_bull_bear=TRUE, thresh_old=0.005
# through the dots arguments of sapply.
# Call the output matrix etf_betas.
# You can also use the function t(().

etf_betas <- sapply(returns[, -1], calc_betas, 
  mar_ket=returns$VTI, calc_bull_bear=TRUE, thresh_old=0.005)

etf_betas <- t(etf_betas)

# You should get the following output:
# > etf_betas
#           beta   bull_beta  bear_beta
# VEU  1.0983544  1.14437555  1.1425766
# IEF -0.1500854 -0.09851751 -0.1508391
# VNQ  1.3239180  1.57588489  1.4878354
# DBC  0.4457915  0.35404568  0.5556382
# XLY  0.9998306  0.94817335  1.0009160
# XLP  0.5621511  0.56021053  0.5393479
# XLE  1.2024247  1.24901728  1.3372083
# XLF  1.4603194  1.60313308  1.5787563
# XLV  0.7273719  0.73641544  0.6402054
# XLI  1.0131387  0.93097105  0.9768738
# XLB  1.0694993  0.86847099  1.1199029
# XLK  0.9435144  0.95040960  0.8698043
# XLU  0.6319806  0.77424611  0.7191581
# VYM  0.8882295  0.86813171  0.9070439
# IVW  0.9286276  0.88530266  0.9683756
# IWB  0.9784908  0.96921340  0.9970054
# IWD  1.0359291  1.06748279  1.0688054
# IWF  0.9360126  0.90889562  0.9642072

# Calculate the names of the ETFs whose bear_beta
# is greater than its bull_beta.
# You can use the function names().

is_bear <- etf_betas[, "bear_beta"] > etf_betas[, "bull_beta"]
names(is_bear[is_bear])

# You should get the following output:
# [1] "DBC" "XLY" "XLE" "XLI" "XLB" "VYM" "IVW" "IWB" "IWD" "IWF"


# 3. (20pts) Perform an sapply loop over a vector of years, 
# and in each year calculate a vector of single ETF betas.
# Call the output matrix etf_betas.
# You can use the functions sapply() and t().

years <- c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

etf_betas <- sapply(years, function(ye_ar) {
  sapply(returns[ye_ar, -1], calc_betas, mar_ket=returns$VTI[ye_ar])
})  # end sapply
etf_betas <- t(etf_betas)

# You should get the following matrix:
# > head(etf_betas, 4)
#           VEU        IEF      VNQ       DBC       XLY       XLP      XLE
# 2007 1.121196 -0.2076440 1.377129 0.1359258 1.0030783 0.5368351 1.274202
# 2008 1.099747 -0.1165963 1.497806 0.3387708 0.9707902 0.5458075 1.265559
# 2009 1.146093 -0.1048234 2.028408 0.6295683 1.0836689 0.4904936 1.165126
# 2010 1.205369 -0.2023569 1.269364 0.7486003 1.0576577 0.5656963 1.172690
#           XLF       XLV       XLI       XLB       XLK       XLU       VYM
# 2007 1.373577 0.6547480 0.9414264 1.2946113 0.8773252 0.7872215 0.8648441
# 2008 1.499027 0.6902807 0.8887891 0.9128316 0.9290150 0.7899060 0.8781388
# 2009 2.093525 0.5307688 1.1286618 1.1151009 0.8736443 0.5337112 1.0531758
# 2010 1.266368 0.6920645 1.1464961 1.2174637 0.9509395 0.6745481 0.8023169
#            IVW       IWB      IWD       IWF
# 2007 0.9011439 0.9852344 1.048638 0.9259162
# 2008 0.9003459 0.9741961 1.045737 0.9109054
# 2009 0.8749811 0.9891499 1.125170 0.8637095
# 2010 1.0147004 0.9765099 1.054079 0.9771028


# Calculate a vector of the names of the ETFs 
# with the highest beta in every year.
# You can use the functions apply(), names(x), 
# and which.max().

apply(etf_betas, MARGIN=1, FUN=function(x) {
  names(x)[which.max(x)]
})

# You should get the following vector:
# 
#  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016  2017  2018
# "VNQ" "XLF" "XLF" "VNQ" "XLF" "XLE" "XLF" "XLI" "XLE" "XLE" "XLF" "XLK"

