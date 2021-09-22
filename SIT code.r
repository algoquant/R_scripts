###############################################################################
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################
# Systematic Investor Toolbox (SIT)
# Copyright (C) 2012  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################
# Systematic Investor Toolbox (SIT)
#
# Systematic Investor Toolbox is a collection of tools that I use
# in my investment research. I will demonstrate and document 
# various uses of toolbox in the Systematic Investor blog at
#	www.SystematicInvestor.wordpress.com
#
#
###############################################################################
# Example Usage:
###############################################################################
#
#
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
#con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
#    source(con)
#close(con)
#
#
###############################################################################
# Load Systematic Investor Toolbox (SIT): Windows only
############################################################################### 
# Load Systematic Investor Toolbox (SIT)
#setInternet2(TRUE)
#con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
#	source(con)
#close(con)
#
#
###############################################################################
# Load Systematic Investor Toolbox (SIT): from file, if for example you saved sit.gz to c:/temp/sit.gz
############################################################################### 
#con = gzcon(file('c:/temp/sit.gz', 'rb'))
#	source(con)
#close(con)
#
#
###############################################################################
# Load Systematic Investor Toolbox (SIT): Requires RCurl package
############################################################################### 
#require(RCurl)
#sit = getURLContent('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
#	con = gzcon(rawConnection(sit, 'rb'))
#	source(con)
#close(con)
#
#
###############################################################################
# Example Usage:
############################################################################### 
# Run plota test
#plota.test()
#
#
#
#
#
#
#More to come,
#
#Michael Kapler
#TheSystematicInvestor at gmail
#

bl.compute.risk.aversion <- function(bench, risk.free = 0)
{
lambda = mean(coredata(bench) - coredata(risk.free)) / var(coredata(bench))
return( as.double(lambda) )
}
bl.compute.eqret <- function
(
risk.aversion,
cov,
cap.weight,
risk.free = 0
)
{
return( risk.aversion * cov %*% as.vector(cap.weight) +  as.double(risk.free))
}
bl.compute.posterior <- function
(
mu,
cov,
pmat=NULL,
qmat=NULL,
tau=0.025
)
{
out = list()
if( !is.null(pmat) ) {
omega = diag(c(1,diag(tau * pmat %*% cov %*% t(pmat))))[-1,-1]
temp = solve(solve(tau * cov) + t(pmat) %*% solve(omega) %*% pmat)
out$cov = cov + temp
out$expected.return = temp %*% (solve(tau * cov) %*% mu + t(pmat) %*% solve(omega) %*% qmat)
} else {
temp = tau * cov
out$cov = cov + temp
out$expected.return = temp %*% (solve(tau * cov) %*% mu )
}
return(out)
}
bl.compute.optimal <- function(risk.aversion, mu, cov)
{
return( (1/risk.aversion) * solve(cov) %*% mu )
}
aa.black.litterman.examples <- function()
{
data =
'1,0.4880,0.4780,0.5150,0.4390,0.5120,0.4910
0.4880,1,0.6640,0.6550,0.3100,0.6080,0.7790
0.4780,0.6640,1,0.8610,0.3550,0.7830,0.6680
0.5150,0.6550,0.8610,1,0.3540,0.7770,0.6530
0.4390,0.3100,0.3550,0.3540,1,0.4050,0.3060
0.5120,0.6080,0.7830,0.7770,0.4050,1,0.6520
0.4910,0.7790,0.6680,0.6530,0.3060,0.6520,1'
Corrmat = matrix( as.double(spl( gsub('\n', ',', data), ',')),
nrow = len(spl(data, '\n')), byrow=TRUE)
RiskAversion = 2.5
stdevs = c(16.0, 20.3, 24.8, 27.1, 21.0,  20.0, 18.7)/100
MktWeight = c(1.6, 2.2, 5.2, 5.5, 11.6, 12.4, 61.5)/100
tau = 0.05
Covmat = Corrmat * (stdevs %*% t(stdevs))
EqRiskPrem = RiskAversion * Covmat %*% MktWeight
EqRiskPrem = bl.compute.eqret(RiskAversion, Covmat, MktWeight)
AssetNames = c('Australia','Canada','France','Germany','Japan','UK','USA')
Table2 = cbind(AssetNames, round(cbind(stdevs, MktWeight, EqRiskPrem) * 100,1))
colnames(Table2) = c('Assets','Std Dev','Weq','PI')
Table2
P = matrix(c(0, 0, -29.5, 100, 0, -70.5, 0)/100, nrow=1)
Q = 5/100
Omega = diag(c(1,diag(tau * P %*% Covmat %*% t(P))))[-1,-1]
PostCov = solve(solve(tau*Covmat) + (t(P) %*% solve(Omega) %*% P))
SigmaP = Covmat + PostCov
ExpRet = PostCov %*% (solve(tau * Covmat) %*% EqRiskPrem + t(P) %*% solve(Omega) %*% Q)
post = bl.compute.posterior(EqRiskPrem, Covmat, P, Q, tau)
ExpRet = post$expected.return
SigmaP = post$cov
OptimalWeights = (1/RiskAversion) * solve(SigmaP) %*% ExpRet
OptimalWeights = bl.compute.optimal(RiskAversion, ExpRet, SigmaP)
Tab4Col4 = OptimalWeights - (MktWeight)/(1+tau)
Table4 = cbind(AssetNames, round(cbind(t(P), ExpRet, OptimalWeights, round(Tab4Col4 * 1000)/1000)*100,1))
colnames(Table4) = c('Assets', 'P', 'MU', 'W','W - Weq/1+tau')
Table4
x =
c(0.001005,0.001328,-0.000579,-0.000675,0.000121,0.000128,-0.000445,-0.000437 ,
0.001328,0.007277,-0.001307,-0.000610,-0.002237,-0.000989,0.001442,-0.001535 ,
-0.000579,-0.001307,0.059852,0.027588,0.063497,0.023036,0.032967,0.048039 ,
-0.000675,-0.000610,0.027588,0.029609,0.026572,0.021465,0.020697,0.029854 ,
0.000121,-0.002237,0.063497,0.026572,0.102488,0.042744,0.039943,0.065994 ,
0.000128,-0.000989,0.023036,0.021465,0.042744,0.032056,0.019881,0.032235 ,
-0.000445,0.001442,0.032967,0.020697,0.039943,0.019881,0.028355,0.035064 ,
-0.000437,-0.001535,0.048039,0.029854,0.065994,0.032235,0.035064,0.079958 )
varCov <- matrix(x, ncol = 8, nrow = 8)
mu <- c(0.08, 0.67,6.41, 4.08, 7.43, 3.70, 4.80, 6.60) / 100
pick <- matrix(0, ncol = 8, nrow = 3, dimnames = list(NULL, letters[1:8]))
pick[1,7] <- 1
pick[2,1] <- -1; pick[2,2] <- 1
pick[3, 3:6] <- c(0.9, -0.9, .1, -.1)
post = bl.compute.posterior(mu, varCov, pick, c(0.0525, 0.0025, 0.02), tau = 0.025)
library(BLCOP)
confidences <- 1 / c(0.000709, 0.000141, 0.000866)
myViews <- BLViews(pick, c(0.0525, 0.0025, 0.02), confidences, letters[1:8])
myPosterior <- posteriorEst(myViews, tau = 0.025, mu, varCov )
myPosterior
myPosterior@posteriorMean - post$expected.return
myPosterior@posteriorCovar - post$cov
}
Rglpk.read.model <- function(file, type = c("MPS_fixed", "MPS_free", "CPLEX_LP", "MathProg"), ignore_first_row = FALSE, verbose = FALSE){
if(!file.exists(file)) stop(paste("There is no file called", file, "!"))
type_db <- c("MPS_fixed" = 1L, "MPS_free"  = 2L, "CPLEX_LP"  = 3L, "MathProg"  = 4L)
obj <- list(file = tools::file_path_as_absolute(file), type = type_db[match.arg(type)])
meta_data <- Rglpk:::glp_get_meta_data_from_file(obj, verbose)
milp_data <- Rglpk:::glp_retrieve_MP_from_file(meta_data, ignore_first_row, verbose)
MP_data <- Rglpk:::glp_merge_MP_data(meta_data, milp_data)
dir_db <- c("free" = 1L, ">=" = 2L, "<=" = 3L, "DB" = 4L, "==" = 5L)
MP_data$direction_of_constraints <- names(dir_db[MP_data$direction_of_constraints])
types <- rep("C", length.out = MP_data$n_objective_vars)
if(any(MP_data$objective_var_is_integer)) types[MP_data$objective_var_is_integer] <- "I"
if(any(MP_data$objective_var_is_binary)) types[MP_data$objective_var_is_binary] <- "B"
MP_data$types = types
index = which(MP_data$direction_of_constraints == 'free')
if( length(index) > 0 ) {
MP_data$constraint_matrix = as.matrix(MP_data$constraint_matrix)[-index,]
MP_data$direction_of_constraints = MP_data$direction_of_constraints[-index]
MP_data$right_hand_side = MP_data$right_hand_side[-index]
}
MP_data
}
Rglpk.create.constraints <- function( prob )
{
n = prob$n_objective_vars
lb = rep(NA,n)
lb[prob$bounds$lower$ind] = prob$bounds$lower$val
ub = rep(NA,n)
ub[prob$bounds$upper$ind] = prob$bounds$upper$val
constraints = new.constraints(n, lb = lb, ub = ub)
constraints$binary.index = which(prob$objective_var_is_binary == 1)
if(len(constraints$binary.index) == 0) constraints$binary.index = 0
if(is.null(dim(prob$constraint_matrix))) {
prob$constraint_matrix = matrix(prob$constraint_matrix)
} else {
prob$constraint_matrix = t(prob$constraint_matrix)
}
index = which(prob$direction_of_constraints == '==')
if(len(index)>0) constraints = add.constraints(prob$constraint_matrix[,index], type = '=', b = prob$right_hand_side[index], constraints)
index = which(prob$direction_of_constraints == '<=')
if(len(index)>0) constraints = add.constraints(prob$constraint_matrix[,index], type = '<=', b = prob$right_hand_side[index], constraints)
index = which(prob$direction_of_constraints == '>=')
if(len(index)>0) constraints = add.constraints(prob$constraint_matrix[,index], type = '>=', b = prob$right_hand_side[index], constraints)
f.obj = prob$objective_coefficients
dir = ifelse(prob$maximize, 'max', 'min')
temp = matrix(spl(gsub(']','', prob$objective_vars_names),'\\['), nr=2)
prob$names = temp[1,]
prob$tickers = temp[2,]
return(list(constraints=constraints, f.obj=f.obj, dir=dir, prob=prob))
}
min.var.portfolio.gmpl <- function(ia, constraints)
{
load.packages('quadprog,corpcor')
cov.temp = ia$cov
n0 = ia$n
n = nrow(constraints$A)
if( n != nrow(cov.temp) ) {
temp =  matrix(0, n, n)
temp[1:n0, 1:n0] = cov.temp[1:n0, 1:n0]
cov.temp = temp
}
if(!is.positive.definite(cov.temp)) {
cov.temp <- make.positive.definite(cov.temp, 0.000000001)
}
binary.vec = 0
if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index
sol = solve.QP.bounds(Dmat = cov.temp, dvec = rep(0, nrow(cov.temp)) ,
Amat=constraints$A, bvec=constraints$b, constraints$meq,
lb = constraints$lb, ub = constraints$ub, binary.vec = binary.vec)
if(binary.vec[1] != 0) cat(sol$counter,'QP calls made to solve problem with', len(binary.vec), 'binary variables using Branch&Bound', '\n')
x = sol$solution[1:ia$n]
names(x) = ia$symbols
return(x)
}
portopt.mathprog.test <- function( )
{
load.packages('quantmod,quadprog,corpcor')
tickers = dow.jones.components()
ia = aa.test.create.ia.custom(tickers, dates = '2000::2010')
n = ia$n
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
x = min.var.portfolio.gmpl(ia, constraints)
png(filename = 'plot1.png', width = 600, height = 400, units = 'px', pointsize = 12, bg = 'white')
barplot(100*x, las=2, main='Minimum Variance Portfolio')
dev.off()
load.packages('Rglpk')
model.file = 'model1.mod'
cat('
set SYMBOLS ;
var weight{i in SYMBOLS} >= 0, <= 1 ;
minimize alpha : sum{i in SYMBOLS} weight[i] ;
subject to fully_invested : sum{i in SYMBOLS} weight[i] = 1 ;
data;
set SYMBOLS := ', ia$symbols, ';
', file = model.file, append = FALSE)
model = Rglpk.read.model(model.file,type = 'MathProg')
constraints = Rglpk.create.constraints(model)$constraints
x = min.var.portfolio.gmpl(ia, constraints)
png(filename = 'plot2.png', width = 600, height = 400, units = 'px', pointsize = 12, bg = 'white')
barplot(100*x, las=2, main='Minimum Variance Portfolio using GNU MathProg model')
dev.off()
model.file = 'model2.mod'
cat('
set SYMBOLS ;
var weight{i in SYMBOLS} >= 0, <= 1 ;
var held{SYMBOLS} binary;
minimize alpha : sum{i in SYMBOLS} weight[i] ;
subject to fully_invested : sum{i in SYMBOLS} weight[i] = 1 ;
subject to MinWgt {i in SYMBOLS} : weight[i] >= 0.025 * held[i];
subject to MaxWgt {i in SYMBOLS} : weight[i] <= .20 * held[i] ;
subject to MaxAssetsLB : 0 <= sum {i in SYMBOLS} held[i] ;
subject to MaxAssetsUB : sum {i in SYMBOLS} held[i] <= 6 ;
data;
set SYMBOLS := ', ia$symbols, ';
', file = model.file, append = FALSE)
model = Rglpk.read.model(model.file,type = 'MathProg')
constraints = Rglpk.create.constraints(model)$constraints
x = min.var.portfolio.gmpl(ia, constraints)
png(filename = 'plot3.png', width = 600, height = 400, units = 'px', pointsize = 12, bg = 'white')
barplot(100*x, las=2, main='Minimum Variance Portfolio using GNU MathProg model \n with Minimum Investment and Number of Assets Constraints')
dev.off()
model.file = 'model3.mod'
cat('
set SYMBOLS ;
var long {i in SYMBOLS} >= 0, <= 0.8 ;
var short{i in SYMBOLS} >= 0, <= 0.5 ;
var islong{SYMBOLS} binary;
minimize alpha : sum{i in SYMBOLS} long[i] ;
subject to fully_invested : sum{i in SYMBOLS} (long[i] - short[i]) = 1 ;
subject to leverage : sum{i in SYMBOLS} (long[i] + short[i]) = 1.6 ;
subject to long_flag  {i in SYMBOLS} : long[i]  <= islong[i] ;
subject to short_flag {i in SYMBOLS} : short[i] <= (1 - islong[i]) ;
data;
set SYMBOLS := ', ia$symbols, ';
', file = model.file, append = FALSE)
model = Rglpk.read.model(model.file,type = 'MathProg')
constraints = Rglpk.create.constraints(model)$constraints
x = min.var.portfolio.gmpl(aa.test.ia.add.short(ia), constraints)
x = x[1:ia$n] - x[-c(1:ia$n)]
png(filename = 'plot4.png', width = 600, height = 400, units = 'px', pointsize = 12, bg = 'white')
barplot(100*x, las=2, main='Minimum Variance Portfolio using GNU MathProg model \n with 130:30 Constraints')
dev.off()
ia = aa.test.create.ia.custom(tickers[1:15], dates = '2000::2010')
model.file = 'model4.mod'
param = ia$cov[,1,drop=F]
colnames(param) = 'CurWgt'
param[,'CurWgt'] = 1/ia$n
cat('
set SYMBOLS ;
param CurWgt{SYMBOLS} ;
var weight{i in SYMBOLS} >= 0, <= 1 ;
var TradePos{i in SYMBOLS} >= 0 ;
var TradeNeg{i in SYMBOLS} >= 0 ;
var TradeFlag{SYMBOLS} binary;
var trade{SYMBOLS} binary;
minimize alpha : sum{i in SYMBOLS} weight[i] ;
subject to fully_invested : sum{i in SYMBOLS} weight[i] = 1 ;
subject to TradeRange {i in SYMBOLS} : (CurWgt[i] - weight[i]) = (TradePos[i] - TradeNeg[i]) ;
subject to TradeFlagPos {i in SYMBOLS} : TradePos[i] <= 100 * TradeFlag[i];
subject to TradeFlagNeg {i in SYMBOLS} : TradeNeg[i] <= 100 * (1 - TradeFlag[i]);
subject to MinTradeSize {i in SYMBOLS} : (TradePos[i] + TradeNeg[i]) >= 0.01 * trade[i];
subject to MaxTradeSize {i in SYMBOLS} : (TradePos[i] + TradeNeg[i]) <= .90 * trade[i] ;
subject to MaxTrade : sum {i in SYMBOLS} trade[i] <= 48 ;
data;
set SYMBOLS := ', ia$symbols, ';
param : CurWgt:=
', file = model.file, append = FALSE)
write.table(param, sep='\t', quote = F, col.names = F, file = model.file, append = TRUE)
cat(';
', file = model.file, append = TRUE)
model = Rglpk.read.model(model.file,type = 'MathProg')
constraints = Rglpk.create.constraints(model)$constraints
x = min.var.portfolio.gmpl(ia, constraints)
sqrt(x %*% ia$cov %*% x)
png(filename = 'plot5.png', width = 600, height = 400, units = 'px', pointsize = 12, bg = 'white')
barplot(100*x, las=2, main='Minimum Variance Portfolio using GNU MathProg model \n with Turnover Constraints')
dev.off()
ia = aa.test.create.ia.custom(tickers[1:10], dates = '2000::2010')
model.file = 'model4.mod'
param = ia$cov[,1,drop=F]
colnames(param) = 'CurWgt'
param[,'CurWgt'] = 1/ia$n
cat('
set SYMBOLS ;
param CurWgt{SYMBOLS} ;
var weight{i in SYMBOLS} >= 0, <= 1 ;
var TradePos{i in SYMBOLS} >= 0 ;
var TradeNeg{i in SYMBOLS} >= 0 ;
var TradeFlag{SYMBOLS} binary;
var trade{SYMBOLS} binary;
minimize alpha : sum{i in SYMBOLS} weight[i] ;
subject to fully_invested : sum{i in SYMBOLS} weight[i] = 1 ;
subject to TradeRange {i in SYMBOLS} : (CurWgt[i] - weight[i]) = (TradePos[i] - TradeNeg[i]) ;
subject to TradeFlagPos {i in SYMBOLS} : TradePos[i] <= 100 * TradeFlag[i];
subject to TradeFlagNeg {i in SYMBOLS} : TradeNeg[i] <= 100 * (1 - TradeFlag[i]);
subject to MinTradeSize {i in SYMBOLS} : (TradePos[i] + TradeNeg[i]) >= 0.05 * trade[i];
subject to MaxTradeSize {i in SYMBOLS} : (TradePos[i] + TradeNeg[i]) <= .20 * trade[i] ;
subject to MaxTrade : sum {i in SYMBOLS} trade[i] <= 8 ;
data;
set SYMBOLS := ', ia$symbols, ';
param : CurWgt:=
', file = model.file, append = FALSE)
write.table(param, sep='\t', quote = F, col.names = F, file = model.file, append = TRUE)
cat(';
', file = model.file, append = TRUE)
model = Rglpk.read.model(model.file,type = 'MathProg')
constraints = Rglpk.create.constraints(model)$constraints
x = min.var.portfolio.gmpl(ia, constraints)
sqrt(x %*% ia$cov %*% x)
png(filename = 'plot6.png', width = 600, height = 400, units = 'px', pointsize = 12, bg = 'white')
barplot(100*x, las=2, main='Minimum Variance Portfolio using GNU MathProg model \n with Turnover Constraints')
dev.off()
}
add.constraint.omega <- function
(
ia,
value,
type = c('=', '>=', '<='),
constraints
)
{
if(is.null(ia$parameters.omega)) omega = 0 else omega = ia$parameters.omega
n0 = ncol(ia$hist.returns)
n = nrow(constraints$A)
nt = nrow(ia$hist.returns)
constraints = add.variables(2*nt + 1, constraints, lb = c(rep(0,2*nt),-Inf))
constraints$A[n + 2*nt + 1, ] = -constraints$b
constraints$b[] = 0
index = which( constraints$ub[1:n] < +Inf )
if( len(index) > 0 ) {
a = rbind( diag(n), matrix(0, 2*nt, n), -constraints$ub[1:n])
constraints = add.constraints(a[, index], rep(0, len(index)), '<=', constraints)
}
index = which( constraints$lb[1:n] > -Inf )
if( len(index) > 0 ) {
a = rbind( diag(n), matrix(0, 2*nt, n), -constraints$lb[1:n])
constraints = add.constraints(a[, index], rep(0, len(index)), '>=', constraints)
}
constraints$lb[1:n] = -Inf
constraints$ub[1:n] = Inf
a = rbind( matrix(0, n, nt), -diag(nt), diag(nt), -omega)
a[1 : n0, ] = t(ia$hist.returns)
constraints = add.constraints(a, rep(0, nt), '=', constraints)
constraints = add.constraints(c( rep(0,n), rep(0,nt), (1/nt) * rep(1,nt), 0), 1, '=', constraints)
constraints = add.constraints(c(rep(0, n), (1/nt) * rep(1, nt), rep(0, nt), 0), value, type[1], constraints)
return( constraints )
}
portfolio.omega <- function
(
weight,
ia
)
{
weight = weight[, 1:ia$n]
if(is.null(ia$parameters.omega)) omega = 0 else omega = ia$parameters.omega
portfolio.returns = weight %*% t(ia$hist.returns)
return( apply(portfolio.returns, 1, function(x) mean(pmax(x - omega,0)) / mean(pmax(omega - x,0)) ) )
}
max.omega.portfolio <- function
(
ia,
constraints
)
{
n = nrow(constraints$A)
nt = nrow(ia$hist.returns)
sol = optimize.portfolio(ia, constraints, add.constraint.omega, portfolio.omega, 'max', T)
if(!inherits(sol, 'try-error')) {
x0 = sol$solution[1:n]
u = sol$solution[(1+n):(n+nt)]
d = sol$solution[(n+nt+1):(n+2*nt)]
t = sol$solution[(n+2*nt+1):(n+2*nt+1)]
x = x0/t
} else {
x = NA
}
if( any( u*d != 0 ) || sol$status !=0 ) {
if(is.null(ia$parameters.omega)) omega = 0 else omega = ia$parameters.omega
fn <- function(x){
portfolio.returns = x %*% t(ia$hist.returns)
mean(pmax(portfolio.returns - omega,0)) / mean(pmax(omega - portfolio.returns,0))
}
x = optimize.portfolio.nlp(ia, constraints, fn, direction = 'max')
}
return( x )
}
portopt.omega <- function
(
ia,
constraints = NULL,
nportfolios = 50,
name = 'Omega'
)
{
out = list(weight = matrix(NA, nportfolios, nrow(constraints$A)))
colnames(out$weight) = rep('', ncol(out$weight))
colnames(out$weight)[1:ia$n] = ia$symbols
ef.risk = portopt(ia, constraints, 2)
out$weight[nportfolios, ] = ef.risk$weight[2,]
out$weight[1, ] = ef.risk$weight[1,]
constraints$x0 = out$weight[1, ]
out$return = portfolio.return(out$weight, ia)
target = seq(out$return[1], out$return[nportfolios], length.out = nportfolios)
constraints = add.constraints(c(ia$expected.return, rep(0, nrow(constraints$A) - ia$n)),
target[1], type = '<=', constraints)
for(i in 1:nportfolios ) {
cat('i =', i, '\n')
constraints$b[ len(constraints$b) ] = -target[i]
out$weight[i, ] = max.omega.portfolio(ia, constraints)
constraints$x0 = out$weight[i, ]
}
out$return = portfolio.return(out$weight, ia)
out$risk = portfolio.risk(out$weight, ia)
out$name = name
return(out)
}
plot.omega <- function
(
weight,
ia
)
{
omegafn = function(x,L) { mean(pmax(x-L,0)) / mean(pmax(L-x,0)) }
if(is.null(ia$parameters.omega)) omega = 0 else omega = ia$parameters.omega
weight = weight[, 1:ia$n, drop=F]
portfolio.returns = weight %*% t(ia$hist.returns)
threshhold = quantile(portfolio.returns, probs = c(0.05, 0.95))
threshhold = seq(threshhold[1], threshhold[2], length.out = 100)
par(mar = c(4,4,2,1), cex = 0.8)
for(i in 1:nrow(weight)) {
data = sapply(threshhold, function(L) omegafn(portfolio.returns[i, ], L))
if(i==1) plot(threshhold,log(data), type='l', col=i,
xlab='Threshhold', ylab='Log(Omega)', main='Portfolio Omega')
lines(threshhold, log(data), col=i)
}
abline(v = omega, col='orange')
grid()
plota.legend(rownames(weight) ,1:nrow(weight), x = 'bottomleft')
}
new.constraints <- function
(
n,
A = NULL,
b = NULL,
type = c('=', '>=', '<='),
lb = NA,
ub = NA
)
{
meq = 0
if ( is.null(A) || is.na(A) || is.null(b) || is.na(b) ) {
A = matrix(0, n, 0)
b = c()
} else {
if ( is.null(dim(A)) ) dim(A) = c(len(A), 1)
if ( type[1] == '=' ) meq = len(b)
if ( type[1] == '<=' ) {
A = -A
b = -b
}
}
if ( is.null(lb) || is.na(lb) ) lb = rep(NA, n)
if ( len(lb) != n ) lb = rep(lb[1], n)
if ( is.null(ub) || is.na(ub) ) ub = rep(NA, n)
if ( len(ub) != n ) ub = rep(ub[1], n)
return( list(n = n, A = A, b = b, meq = meq, lb = lb, ub = ub) )
}
add.constraints <- function
(
A,
b,
type = c('=', '>=', '<='),
constraints
)
{
if(is.null(constraints)) constraints = new.constraints(n = nrow(A))
if(is.null(dim(A))) A = matrix(A)
if(len(b) == 1) b = rep(b, ncol(A))
if ( type[1] == '=' ) {
constraints$A = cbind( A, constraints$A )
constraints$b = c( b, constraints$b )
constraints$meq = constraints$meq + len(b)
}
if ( type[1] == '>=' ) {
constraints$A = cbind( constraints$A, A )
constraints$b = c( constraints$b, b )
}
if ( type[1] == '<=' ) {
constraints$A = cbind( constraints$A, -A )
constraints$b = c( constraints$b, -b )
}
return( constraints )
}
add.variables <- function
(
n,
constraints,
lb = NA,
ub = NA
)
{
constraints$A = rbind( constraints$A, matrix(0, n, len(constraints$b)) )
if ( is.null(lb) || is.na(lb) ) lb = rep(NA, n)
if ( len(lb) != n ) lb = rep(lb[1], n)
if ( is.null(ub) || is.na(ub) ) ub = rep(NA, n)
if ( len(ub) != n ) ub = rep(ub[1], n)
constraints$lb = c(constraints$lb, lb)
constraints$ub = c(constraints$ub, ub)
constraints$n = constraints$n + n
return( constraints )
}
delete.constraints <- function
(
delete.index,
constraints
)
{
constraints$A = constraints$A[, -delete.index, drop=F]
constraints$b = constraints$b[ -delete.index]
constraints$meq = constraints$meq - len(intersect((1:constraints$meq), delete.index))
return( constraints )
}
type.constraints <- function(constraints)
{
c(rep('=', constraints$meq), rep('>=', len(constraints$b) - constraints$meq))
}
min.portfolio <- function
(
ia,
constraints,
add.constraint.fn,
min.risk.fn
)
{
optimize.portfolio(ia, constraints, add.constraint.fn, min.risk.fn)
}
optimize.portfolio <- function
(
ia,
constraints,
add.constraint.fn,
min.risk.fn,
direction = 'min',
full.solution = F
)
{
n = nrow(constraints$A)
nt = nrow(ia$hist.returns)
constraints = match.fun(add.constraint.fn)(ia, 0, '>=', constraints)
f.obj = constraints$A[, ncol(constraints$A)]
constraints = delete.constraints( ncol(constraints$A), constraints)
f.con = constraints$A
f.dir = c(rep('=', constraints$meq), rep('>=', len(constraints$b) - constraints$meq))
f.rhs = constraints$b
x = NA
binary.vec = 0
if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index
sol = try(solve.LP.bounds(direction, f.obj, t(f.con), f.dir, f.rhs,
lb = constraints$lb, ub = constraints$ub, binary.vec = binary.vec,
default.lb = -100), TRUE)
if(!inherits(sol, 'try-error')) {
x = sol$solution[1:n]
if( F ) {
f.obj %*% sol$solution  - match.fun(min.risk.fn)(t(x), ia)
}
}
if( full.solution ) x = sol
return( x )
}
optimize.portfolio.nlp <- function
(
ia,
constraints,
fn,
nl.constraints = NULL,
direction = 'min',
full.solution = F
)
{
load.packages('Rdonlp2', repos ='http://R-Forge.R-project.org')
if( direction == 'min' ) fnscale = 1 else fnscale = -1
if( as.numeric( sessionInfo()$R.version$minor ) < 9 ) {
cntl <- donlp2.control(silent = T, fnscale = fnscale, iterma =10000, nstep = 100, epsx = 1e-10)
} else {
cntl <- donlp2Control()
cntl$silent = T
cntl$fnscale = fnscale
cntl$iterma =10000
cntl$nstep = 100
cntl$epsx = 1e-10
}
par.l = constraints$lb
par.u = constraints$ub
p = rep(1, nrow(constraints$A))
if(!is.null(constraints$x0)) {
if( sum(is.na(constraints$x0)) == 0) p = constraints$x0
}
A = t(constraints$A)
lin.l = constraints$b
lin.u = constraints$b
lin.u[ -c(1:constraints$meq) ] = +Inf
x = NA
if( !is.null(nl.constraints) ) {
sol = donlp2(p, fn,
par.lower=par.l, par.upper=par.u,
A=A, lin.u=lin.u, lin.l=lin.l,
control=cntl,
nlin=nl.constraints$constraints,
nlin.upper=nl.constraints$upper, nlin.lower=nl.constraints$lower
)
} else {
sol = donlp2(p, fn,
par.lower=par.l, par.upper=par.u,
A=A, lin.u=lin.u, lin.l=lin.l,
control=cntl)
}
if(!inherits(sol, 'try-error')) {
x = sol$par
}
if( full.solution ) x = sol
return( x )
}
add.constraint.maxloss <- function
(
ia,
value,
type = c('=', '>=', '<='),
constraints
)
{
n0 = ncol(ia$hist.returns)
n = nrow(constraints$A)
nt = nrow(ia$hist.returns)
constraints = add.variables(1, constraints)
a = rbind( matrix(0, n, nt), 1)
a[1 : n0, ] = t(ia$hist.returns)
constraints = add.constraints(a, rep(0, nt), '>=', constraints)
constraints = add.constraints(c(rep(0, n), 1), value, type[1], constraints)
return( constraints )
}
portfolio.maxloss <- function
(
weight,
ia
)
{
weight = weight[, 1:ia$n, drop=F]
portfolio.returns = weight %*% t(ia$hist.returns)
return( -apply(portfolio.returns, 1, min) )
}
min.maxloss.portfolio <- function
(
ia,
constraints
)
{
min.portfolio(ia, constraints, add.constraint.maxloss, portfolio.maxloss)
}
add.constraint.mad <- function
(
ia,
value,
type = c('=', '>=', '<='),
constraints
)
{
n0 = ncol(ia$hist.returns)
n = nrow(constraints$A)
nt = nrow(ia$hist.returns)
constraints = add.variables(2 * nt, constraints, lb = 0)
a = rbind( matrix(0, n, nt), -diag(nt), diag(nt))
a[1 : n0, ] = t(ia$hist.returns) - repmat(colMeans(ia$hist.returns), 1, nt)
constraints = add.constraints(a, rep(0, nt), '=', constraints)
constraints = add.constraints(c(rep(0, n), (1/nt) * rep(1, 2 * nt)), value, type[1], constraints)
return( constraints )
}
portfolio.mad <- function
(
weight,
ia
)
{
if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
weight = weight[, 1:ia$n, drop=F]
portfolio.returns = weight %*% t(ia$hist.returns)
return( apply(portfolio.returns, 1, function(x) mean(abs(x - mean(x))) ) )
}
min.mad.portfolio <- function
(
ia,
constraints
)
{
min.portfolio(ia, constraints, add.constraint.mad, portfolio.mad)
}
add.constraint.cvar <- function
(
ia,
value,
type = c('=', '>=', '<='),
constraints
)
{
if(is.null(ia$parameters.alpha)) alpha = 0.95 else alpha = ia$parameters.alpha
n0 = ncol(ia$hist.returns)
n = nrow(constraints$A)
nt = nrow(ia$hist.returns)
constraints = add.variables(nt + 1, constraints, lb = c(rep(0,nt),-Inf))
a = rbind( matrix(0, n, nt), diag(nt), 1)
a[1 : n0, ] = t(ia$hist.returns)
constraints = add.constraints(a, rep(0, nt), '>=', constraints)
constraints = add.constraints(c(rep(0, n), (1/(1-alpha))* (1/nt) * rep(1, nt), 1), value, type[1], constraints)
return( constraints )
}
portfolio.cvar <- function
(
weight,
ia
)
{
weight = weight[, 1:ia$n, drop=F]
if(is.null(ia$parameters.alpha)) alpha = 0.95 else alpha = ia$parameters.alpha
portfolio.returns = weight %*% t(ia$hist.returns)
return( apply(portfolio.returns, 1, function(x) -compute.cvar(x, 1-alpha) ) )
}
min.cvar.portfolio <- function
(
ia,
constraints
)
{
min.portfolio(ia, constraints, add.constraint.cvar, portfolio.cvar)
}
portfolio.var <- function
(
weight,
ia
)
{
weight = weight[, 1:ia$n, drop=F]
if(is.null(ia$parameters.alpha)) alpha = 0.95 else alpha = ia$parameters.alpha
portfolio.returns = weight %*% t(ia$hist.returns)
return( apply(portfolio.returns, 1, function(x) -compute.var(x, 1-alpha) ) )
}
add.constraint.cdar <- function
(
ia,
value,
type = c('=', '>=', '<='),
constraints
)
{
if(is.null(ia$parameters.alpha)) alpha = 0.95 else alpha = ia$parameters.alpha
n0 = ncol(ia$hist.returns)
n = nrow(constraints$A)
nt = nrow(ia$hist.returns)
constraints = add.variables(2*nt + 1, constraints, lb = c(rep(0,nt), rep(-Inf,nt+1)))
a = rbind( matrix(0, n, nt), diag(nt), 1, -diag(nt))
a[1 : n0, ] = t(apply( t(ia$hist.returns), 1, cumsum))
constraints = add.constraints(a, rep(0, nt), '>=', constraints)
a = rbind( matrix(0, n, nt), 0*diag(nt), 0, diag(nt))
a[1 : n0, ] = -t(apply( t(ia$hist.returns), 1, cumsum))
constraints = add.constraints(a, rep(0, nt), '>=', constraints)
temp = diag(nt);
temp[-nt,-1]=-diag((nt-1))
diag(temp) = 1
a = rbind( matrix(0, n, nt), 0*diag(nt), 0, temp)
a = a[,-1]
constraints = add.constraints(a, rep(0, (nt-1)), '>=', constraints)
constraints = add.constraints(c(rep(0, n), (1/(1-alpha))* (1/nt) * rep(1, nt), 1, rep(0, nt)), value, type[1], constraints)
return( constraints )
}
portfolio.cdar <- function
(
weight,
ia
)
{
weight = weight[, 1:ia$n, drop=F]
if(is.null(ia$parameters.alpha)) alpha = 0.95 else alpha = ia$parameters.alpha
portfolio.returns = weight %*% t(ia$hist.returns)
return( apply(portfolio.returns, 1,
function(x) {
x = cumsum(x)
x = x - cummax(x)
-compute.cvar(x, 1-alpha)
}
))
}
min.cdar.portfolio <- function
(
ia,
constraints
)
{
min.portfolio(ia, constraints, add.constraint.cdar, portfolio.cdar)
}
portfolio.cdar.real <- function
(
weight,
ia
)
{
weight = weight[, 1:ia$n, drop=F]
if(is.null(ia$parameters.alpha)) alpha = 0.95 else alpha = ia$parameters.alpha
portfolio.returns = weight %*% t(ia$hist.returns)
out = rep(0, nrow(weight))
for( i in 1:nrow(weight) ) {
portfolio.equity = cumprod(1 + portfolio.returns[i,])
x = compute.drawdowns(portfolio.equity)
out[i] = compute.cvar(x, alpha)
}
return( out )
}
compute.drawdowns <- function( portfolio.equity, make.plot = FALSE )
{
temp = portfolio.equity / cummax(portfolio.equity) - 1
temp = c(temp, 0)
drawdown.start = which( temp == 0 & mlag(temp, -1) != 0 )
drawdown.end = which( temp == 0 & mlag(temp, 1) != 0 )
if(make.plot) {
plot((1:len(temp)), temp, type='l')
points((1:len(temp))[drawdown.start] , temp[drawdown.start], col='red')
points((1:len(temp))[drawdown.end] , temp[drawdown.end], col='blue')
}
return( apply(cbind(drawdown.start, drawdown.end), 1,
function(x){ min(temp[ x[1]:x[2] ], na.rm=T)} )
)
}
min.avgcor.portfolio <- function
(
ia,
constraints
)
{
cov = ia$cov[1:ia$n, 1:ia$n]
s = sqrt(diag(cov))
fn <- function(x){
sd_x = sqrt( t(x) %*% cov %*% x )
mean( ( x %*% cov ) / ( s * sd_x ) )
}
x = optimize.portfolio.nlp(ia, constraints, fn)
return( x )
}
portfolio.avgcor <- function
(
weight,
ia
)
{
weight = weight[, 1:ia$n, drop=F]
cov = ia$cov[1:ia$n, 1:ia$n]
s = sqrt(diag(cov))
return( apply(weight, 1, function(x) {
sd_x = sqrt( t(x) %*% cov %*% x )
mean( ( x %*% cov ) / ( s * sd_x ) )
})	)
}
min.cor.insteadof.cov.portfolio <- function
(
ia,
constraints
)
{
if(is.null(ia$cov.temp)) ia$cov.temp = ia$cov
sol = solve.QP.bounds(Dmat = ia$correlation, dvec = rep(0, nrow(ia$cov.temp)) ,
Amat=constraints$A, bvec=constraints$b, constraints$meq,
lb = constraints$lb, ub = constraints$ub)
return( sol$solution )
}
portfolio.avgcor.real <- function
(
weight,
ia
)
{
weight = weight[, 1:ia$n, drop=F]
portfolio.returns = weight %*% t(ia$hist.returns)
return( apply(portfolio.returns, 1, function(x) mean(cor(ia$hist.returns, x)) ) )
}
add.constraint.mad.downside <- function
(
ia,
value,
type = c('=', '>=', '<='),
constraints
)
{
n0 = ncol(ia$hist.returns)
n = nrow(constraints$A)
nt = nrow(ia$hist.returns)
constraints = add.variables(nt, constraints, lb = 0)
a = rbind( matrix(0, n, nt), diag(nt))
if(is.null(ia$parameters.mar) || is.na(ia$parameters.mar)) {
a[1 : n0, ] = t(ia$hist.returns) - repmat(colMeans(ia$hist.returns), 1, nt)
constraints = add.constraints(a, rep(0, nt), '>=', constraints)
} else {
a[1 : n0, ] = t(ia$hist.returns)
constraints = add.constraints(a, rep(ia$parameters.mar, nt), '>=', constraints)
}
constraints = add.constraints(c(rep(0, n), (1/nt) * rep(1, nt)), value, type[1], constraints)
return( constraints )
}
portfolio.mad.downside <- function
(
weight,
ia
)
{
weight = weight[, 1:ia$n, drop=F]
portfolio.returns = weight %*% t(ia$hist.returns)
if(is.null(ia$parameters.mar) || is.na(ia$parameters.mar)) {
return( apply(portfolio.returns, 1, function(x) mean(pmax(mean(x) - x, 0)) ) )
} else {
return( apply(portfolio.returns, 1, function(x) mean(pmax(ia$parameters.mar - x, 0)) ) )
}
}
min.mad.downside.portfolio <- function
(
ia,
constraints
)
{
min.portfolio(ia, constraints, add.constraint.mad.downside, portfolio.mad.downside)
}
portfolio.risk.downside <- function
(
weight,
ia
)
{
weight = weight[, 1:ia$n, drop=F]
portfolio.returns = weight %*% t(ia$hist.returns)
if(is.null(ia$parameters.mar) || is.na(ia$parameters.mar)) {
return( apply(portfolio.returns, 1, function(x) sqrt(mean(pmax(mean(x) - x, 0)^2)) ) )
} else {
return( apply(portfolio.returns, 1, function(x) sqrt(mean(pmax(ia$parameters.mar - x, 0)^2)) ) )
}
}
min.risk.downside.portfolio <- function
(
ia,
constraints
)
{
n = nrow(constraints$A)
nt = nrow(ia$hist.returns)
constraints = add.constraint.mad.downside(ia, 0, '>=', constraints)
f.obj = constraints$A[, ncol(constraints$A)]
constraints = delete.constraints( ncol(constraints$A), constraints)
Dmat = diag( len(f.obj) )
diag(Dmat) = f.obj
if(!is.positive.definite(Dmat)) {
Dmat <- make.positive.definite(Dmat)
}
x = NA
binary.vec = 0
if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index
sol = try(solve.QP.bounds(Dmat = Dmat, dvec = rep(0, nrow(Dmat)) ,
Amat=constraints$A, bvec=constraints$b, constraints$meq,
lb = constraints$lb, ub = constraints$ub, binary.vec = binary.vec),TRUE)
if(!inherits(sol, 'try-error')) {
x = sol$solution[1:n]
if( F ) {
sol$solution %*% Dmat %*% (sol$solution) - portfolio.risk.downside(t(x), ia)^2
}
}
return( x )
}
add.constraint.gini <- function
(
ia,
value,
type = c('=', '>=', '<='),
constraints
)
{
n0 = ncol(ia$hist.returns)
n = nrow(constraints$A)
nt = nrow(ia$hist.returns)
constraints = add.variables(nt*(nt-1), constraints, lb=0)
a = matrix(0, n0 + nt*(nt-1), nt*(nt-1)/2)
diag(a[(n0+1) : (n0 + nt*(nt-1)/2), ]) = -1
diag(a[(n0+1+nt*(nt-1)/2) : (n0 + nt*(nt-1)), ]) = 1
hist.returns = as.matrix(ia$hist.returns)
i.start = 0
for(t in 1:(nt-1)) {
index = (i.start+1) : (i.start + nt -t)
for(i in 1:n0) {
a[i, index] = ( hist.returns[t,i] - hist.returns[,i] ) [ (t+1) : nt ]
}
i.start = i.start + nt -t
}
constraints = add.constraints(a, 0, '=', constraints)
constraints = add.constraints(c(rep(0, n), rep(1, nt*(nt-1))), value, type[1], constraints)
return( constraints )
}
min.gini.portfolio <- function
(
ia,
constraints
)
{
min.portfolio(ia, constraints, add.constraint.gini, portfolio.gini.coefficient)
}
portfolio.gini.coefficient <- function
(
weight,
ia
)
{
if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
weight = weight[, 1:ia$n, drop=F]
portfolio.returns = weight %*% t(ia$hist.returns)
n = ncol(portfolio.returns)
one.to.n = 1:n
out = weight[,1] * NA
out[] = apply( portfolio.returns, 1, function(x) {
temp = sort(x, decreasing = F)
sum( (2*one.to.n - n - 1) * temp )
} )
out = 2 * out /(n*(n-1))
return(out)
}
lp.obj.portfolio <- function
(
ia,
constraints,
f.obj = c(ia$expected.return, rep(0, nrow(constraints$A) - ia$n)),
direction = 'min'
)
{
x = NA
binary.vec = 0
if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index
sol = try(solve.LP.bounds(direction, f.obj,
t(constraints$A),
c(rep('=', constraints$meq), rep('>=', len(constraints$b) - constraints$meq)),
constraints$b, lb = constraints$lb, ub = constraints$ub, binary.vec = binary.vec), TRUE)
if(!inherits(sol, 'try-error')) {
x = sol$solution
}
return( x )
}
max.return.portfolio <- function
(
ia,
constraints
)
{
lp.obj.portfolio(ia, constraints, direction = 'max')
}
portfolio.return <- function
(
weight,
ia
)
{
if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
weight = weight[, 1:ia$n, drop=F]
portfolio.return = weight %*% ia$expected.return
return( portfolio.return )
}
portfolio.geometric.return <- function
(
weight,
ia
)
{
weight = weight[, 1:ia$n, drop=F]
portfolio.returns = weight %*% t(ia$hist.returns)
return( apply(portfolio.returns, 1, function(x) (prod(1+x)^(1/len(x)))^ia$annual.factor - 1 ) )
}
max.geometric.return.portfolio <- function
(
ia,
constraints,
min.risk,
max.risk
)
{
fn <- function(x){
portfolio.returns = x %*% t(ia$hist.returns)
prod(1 + portfolio.returns)
}
nlcon1 <- function(x){
sqrt(t(x) %*% ia$cov %*% x)
}
nl.constraints = list()
nl.constraints$constraints = list(nlcon1)
nl.constraints$upper = c(max.risk)
nl.constraints$lower = c(min.risk)
x = optimize.portfolio.nlp(ia, constraints, fn, nl.constraints, direction = 'max')
return( x )
}
portfolio.unrebalanced.return <- function
(
weight,
ia
)
{
weight = weight[, 1:ia$n, drop=F]
total.return = apply(1+ia$hist.returns,2,prod)
total.portfolio.return = weight %*% total.return / rowSums(weight)
total.portfolio.return = (total.portfolio.return^(1/nrow(ia$hist.returns)))^ia$annual.factor - 1
return( total.portfolio.return )
}
geom2aritm <- function(G, V, a, b)
{
(2*G + a*V^2) / (1 - b*G + sqrt((1+b*G)^2 + 2*a*b*V^2))
}
aritm2geom <- function(R, V, a, b)
{
R - a*V^2 / (2*(1 + b*R))
}
geom2aritm4 <- function(G, V)
{
(1+G)*sqrt(1/2 + 1/2*sqrt(1 + 4*V^2/(1+G)^2)) - 1
}
aritm2geom4 <- function(R, V)
{
(1+R)/(sqrt(1 + V^2/(1+R)^2)) - 1
}
target.return.portfolio.helper <- function
(
ia,
constraints,
target.return
)
{
constraints.target = add.constraints(ia$expected.return, type='>=', b=target.return, constraints)
sol = try(min.var.portfolio(ia, constraints.target), silent = TRUE)
if(inherits(sol, 'try-error'))
sol = max.return.portfolio(ia, constraints)
sol
}
target.return.portfolio <- function
(
target.return,
annual.factor = 252
)
{
target.return = as.double(target.return[1])
if(target.return > 1) target.return = target.return / 100
target.return = target.return / annual.factor
function
(
ia,
constraints
)
{
target.return.portfolio.helper(ia, constraints, target.return)
}
}
target.risk.portfolio.helper <- function
(
ia,
constraints,
target.risk,
silent = T,
min.w = NA,
max.w = NA
)
{
if( is.na(max.w) ) max.w = max.return.portfolio(ia, constraints)
if( is.na(min.w) ) min.w = min.var.portfolio(ia, constraints)
max.r = portfolio.return(max.w, ia)
min.r = portfolio.return(min.w, ia)
max.s = portfolio.risk(max.w, ia)
min.s = portfolio.risk(min.w, ia)
if( target.risk >= min.s & target.risk <= max.s ) {
f <- function (x, ia, constraints, target.risk) {
portfolio.risk(target.return.portfolio.helper(ia, constraints, x), ia) - target.risk
}
f.lower = min.s - target.risk
f.upper = max.s - target.risk
sol = uniroot(f, c(min.r, max.r), f.lower=f.lower, f.upper=f.upper, tol = 0.0001,
ia=ia, constraints=constraints, target.risk=target.risk)
if(!silent) cat('Found solution in', sol$iter, 'itterations', '\n')
return( target.return.portfolio.helper(ia, constraints, sol$root) )
} else if( target.risk < min.s ) {
return( min.w )
} else {
return( max.w )
}
stop(paste('target.risk =', target.risk, 'is not possible, max risk =', max.s, ', min risk =', min.s))
}
target.risk.portfolio <- function
(
target.risk,
annual.factor = 252
)
{
target.risk = as.double(target.risk[1])
if(target.risk > 1) target.risk = target.risk / 100
target.risk = target.risk / sqrt(annual.factor)
function
(
ia,
constraints
)
{
target.risk.portfolio.helper(ia, constraints, target.risk)
}
}
min.risk.portfolio <- function
(
ia,
constraints
)
{
x = NA
binary.vec = 0
if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index
if(is.null(ia$cov.temp)) ia$cov.temp = ia$cov
sol = try(solve.QP.bounds(Dmat = ia$cov.temp, dvec = rep(0, nrow(ia$cov.temp)) ,
Amat=constraints$A, bvec=constraints$b, constraints$meq,
lb = constraints$lb, ub = constraints$ub, binary.vec = binary.vec),TRUE)
if(!inherits(sol, 'try-error')) {
if(binary.vec[1] != 0) cat(sol$counter,'QP calls made to solve problem with', len(constraints$binary.index), 'binary variables using Branch&Bound', '\n')
x = sol$solution;
}
return( x )
}
portfolio.risk <- function
(
weight,
ia
)
{
if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
weight = weight[, 1:ia$n, drop=F]
cov = ia$cov[1:ia$n, 1:ia$n]
return( apply(weight, 1, function(x) sqrt(t(x) %*% cov %*% x)) )
}
find.erc.portfolio <- function
(
ia,
constraints
)
{
cov = ia$cov[1:ia$n, 1:ia$n]
fn <- function(x){
risk.contribution = (x * (cov %*% x))
sum( abs(risk.contribution - mean(risk.contribution)) )
}
x = optimize.portfolio.nlp(ia, constraints, fn)
return( x )
}
find.erc.portfolio.simple <- function
(
ia,
constraints
)
{
cov = ia$cov[1:ia$n, 1:ia$n]
fn <- function(x){
if (sum(x) == 0) x = x + 1e-2
x  = x / sum(x)
risk.contribution = (x * (cov %*% x))
var(as.double(risk.contribution))
}
x0 = 1/sqrt(diag(cov))
x0 = x0 / sum(x0)
x = nlminb(start = x0, objective = fn, lower = constraints$lb, upper = constraints$ub)
x$par = x$par / sum(x$par)
return(x$par)
}
find.erc.portfolio.test <- function() {
ia = aa.test.create.ia.rebal()
n = ia$n
x0 = 1/sqrt(diag(ia$cov))
temp = x0 / sum(x0)
rc.temp = portfolio.risk.contribution(temp, ia)
rc.temp = abs(as.vector(rc.temp))
plot(rc.temp,ylim=c(0,0.4))
diff(range(rc.temp))
sd(rc.temp)
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
temp = find.erc.portfolio(ia, constraints)
rc.temp = portfolio.risk.contribution(temp, ia)
rc.temp = abs(as.vector(rc.temp))
plot(rc.temp,ylim=c(0,0.4))
diff(range(rc.temp))
sd(rc.temp)
temp = find.erc.portfolio.simple(ia, constraints)
temp = temp / sum(temp)
rc.temp = portfolio.risk.contribution(temp, ia)
rc.temp = abs(as.vector(rc.temp))
plot(rc.temp,ylim=c(0,0.4))
diff(range(rc.temp))
sd(rc.temp)
temp = equal.risk.contribution.portfolio(ia, constraints)
temp = temp / sum(temp)
rc.temp = portfolio.risk.contribution(temp, ia)
rc.temp = abs(as.vector(rc.temp))
plot(rc.temp,ylim=c(0,0.4))
diff(range(rc.temp))
sd(rc.temp)
}
portfolio.risk.contribution <- function
(
weight,
ia
)
{
if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
weight = weight[, 1:ia$n, drop=F]
cov = ia$cov[1:ia$n, 1:ia$n]
out = weight
out[] = t(apply( weight, 1, function(x) (x * (cov %*% x)) / (t(x) %*% cov %*% x)[1] ))
return(out)
}
portopt <- function
(
ia,
constraints = NULL,
nportfolios = 50,
name = 'Risk',
min.risk.fn = min.risk.portfolio,
equally.spaced.risk = F
)
{
load.packages('quadprog,corpcor,lpSolve,kernlab')
if( is.null(constraints) ) {
constraints = new.constraints(rep(0, ia$n), 0, type = '>=')
}
ia$risk = iif(ia$risk == 0, 0.000001, ia$risk)
if( is.null(ia$cov) ) ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
ia$cov.temp = ia$cov
n0 = ia$n
n = nrow(constraints$A)
if( n != nrow(ia$cov.temp) ) {
temp =  matrix(0, n, n)
temp[1:n0, 1:n0] = ia$cov.temp[1:n0, 1:n0]
ia$cov.temp = temp
}
if(!is.positive.definite(ia$cov.temp, method = 'chol')) {
ia$cov.temp <- make.positive.definite(ia$cov.temp, 0.000000001)
}
if(nportfolios<2) nportfolios = 2
out = list(weight = matrix(NA, nportfolios, nrow(constraints$A)))
colnames(out$weight) = rep('', ncol(out$weight))
colnames(out$weight)[1:ia$n] = ia$symbols
out$weight[nportfolios, ] = max.return.portfolio(ia, constraints)
out$weight[1, ] = match.fun(min.risk.fn)(ia, constraints)
constraints$x0 = out$weight[1, ]
if(nportfolios > 2) {
out$return = portfolio.return(out$weight, ia)
target = seq(out$return[1], out$return[nportfolios], length.out = nportfolios)
constraints = add.constraints(c(ia$expected.return, rep(0, nrow(constraints$A) - ia$n)),
target[1], type = '>=', constraints)
for(i in 2:(nportfolios - 1) ) {
constraints$b[ len(constraints$b) ] = target[i]
out$weight[i, ] = match.fun(min.risk.fn)(ia, constraints)
constraints$x0 = out$weight[i, ]
}
if( equally.spaced.risk ) {
out$risk = portfolio.risk(out$weight, ia)
temp = diff(out$risk)
index = which(temp >= median(temp) + mad(temp))
if( len(index) > 0 ) {
index = min(index)
proper.spacing = ceiling((out$risk[nportfolios] - out$risk[index])/temp[(index-1)])-1
nportfolios1 = proper.spacing + 2
if(nportfolios1 > 2) {
out$return = portfolio.return(out$weight, ia)
out$risk = portfolio.risk(out$weight, ia)
temp = spline(out$risk, out$return, n = nportfolios, method = 'natural')
target = temp$y[ which(temp$y > out$return[index] & temp$y < out$return[nportfolios] &
temp$x > out$risk[index] & temp$x < out$risk[nportfolios])]
target = c(out$return[index], target, out$return[nportfolios])
nportfolios1 = len(target)
out1 = list(weight = matrix(NA, nportfolios1, nrow(constraints$A)))
out1$weight[1, ] = out$weight[index, ]
out1$weight[nportfolios1, ] = out$weight[nportfolios, ]
constraints$x0 = out1$weight[1, ]
for(i in 2:(nportfolios1 - 1) ) {
constraints$b[ len(constraints$b) ] = target[i]
out1$weight[i, ] = match.fun(min.risk.fn)(ia, constraints)
constraints$x0 = out1$weight[i, ]
}
out$weight = rbind(out$weight[-c(index:nportfolios),], out1$weight)
}
}
}
}
rm.index = is.na(rowSums(out$weight))
if(any(rm.index)) out$weight = out$weight[!rm.index,]
out$return = portfolio.return(out$weight, ia)
out$risk = portfolio.risk(out$weight, ia)
out$name = name
return(out)
}
plot.ia <- function
(
ia,
layout = NULL
)
{
if( is.null(layout) ) layout(1:2)
temp = cbind(ia$expected.return, ia$risk)
temp[] = plota.format(100 * temp[], 1, '', '%')
temp = cbind(ia$symbol.names, temp)
colnames(temp) = spl('Name,Return,Risk')
plot.table(temp, 'Symbol')
temp = ia$correlation
temp[lower.tri(temp, TRUE)] = NA
temp = temp[-ia$n, -1]
temp[] = plota.format(100 * temp[], 1, '', '%')
plot.table(temp, highlight = TRUE, colorbar = TRUE)
}
plot.ef <- function
(
ia,
efs,
portfolio.risk.fn = portfolio.risk,
transition.map = TRUE,
layout = NULL
)
{
risk.label = as.character(substitute(portfolio.risk.fn))
n = ia$n
x = match.fun(portfolio.risk.fn)(diag(n), ia)
y = ia$expected.return
xlim = range(c(0, x,
max( sapply(efs, function(x) max(match.fun(portfolio.risk.fn)(x$weight,ia))) )
), na.rm = T)
ylim = range(c(0, y,
min( sapply(efs, function(x) min(portfolio.return(x$weight,ia))) ),
max( sapply(efs, function(x) max(portfolio.return(x$weight,ia))) )
), na.rm = T)
x = 100 * x
y = 100 * y
xlim = 100 * xlim
ylim = 100 * ylim
if( !transition.map ) layout = T
if( is.null(layout) ) layout(1:2)
par(mar = c(4,3,2,1), cex = 0.8)
plot(x, y, xlim = xlim, ylim = ylim,
xlab='', ylab='', main=paste(risk.label, 'vs Return'), col='black')
mtext('Return', side = 2,line = 2, cex = par('cex'))
mtext(risk.label, side = 1,line = 2, cex = par('cex'))
grid();
text(x, y, ia$symbols,	col = 'blue', adj = c(1,1), cex = 0.8)
for(i in len(efs):1) {
ef = efs[[ i ]]
x = 100 * match.fun(portfolio.risk.fn)(ef$weight, ia)
y = 100 * ef$return
lines(x, y, col=i)
}
plota.legend(sapply(efs, function(x) x$name), 1:len(efs))
if(transition.map) {
plot.transition.map(efs[[i]]$weight, x, risk.label, efs[[i]]$name)
}
}
plot.transitopn.map <- function(x,y,xlab = 'Risk',name = '',type=c('s','l')) {
plot.transition.map(x,y,xlab,name,type)
}
plot.transition.map <- function
(
y,
x,
xlab = 'Risk',
name = '',
type=c('s','l'),
col = NA
)
{
if( is.list(y) ) {
name = y$name
x = 100 * y$risk
y = y$weight
}
y[is.na(y)] = 0
par(mar = c(4,3,2,1), cex = 0.8)
plota.stacked(x, y, xlab = xlab, main = paste('Transition Map for', name),
type=type[1], col=ifna(col, plota.colors(ncol(y))) )
}
portfolio.turnover <- function
(
weight
)
{
if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
out = weight[,1] * NA
out[] = rowSums( abs(weight - mlag(weight)) ) / 2
return(out)
}
portfolio.concentration.herfindahl.index <- function
(
weight
)
{
if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
one.over.n = 1/ rowSums(!is.na(weight))
out = weight[,1] * NA
out[] = (rowSums(weight^2, na.rm=T) - one.over.n) / (1 - one.over.n)
return(out)
one.over.n = 1/ncol(weight)
out = weight[,1] * NA
out[] = (rowSums(weight^2) - one.over.n) / (1 - one.over.n)
return(out)
}
portfolio.concentration.gini.coefficient <- function
(
weight
)
{
if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
n = ncol(weight)
one.to.n = 1:n
out = weight[,1] * NA
for(i in 1:nrow(weight)) {
x = coredata(weight[i,])
index = !is.na(x)
n1 = sum(index)
if( n1 > 0 ) {
temp = sort(x[index], decreasing = T)
if(temp[n1] < 0) temp = temp - temp[n1]
out[i] = (n1+1)/(n1-1) - 2 * sum(temp * one.to.n[1:n1]) /(n1*(n1-1)* sum(temp) / n1)
}
}
return(out)
out[] = apply( weight, 1, function(x) {
temp = sort(x, decreasing = T)
sum(temp * one.to.n)
} )
out = (n+1)/(n-1) - 2 * out /(n*(n-1)* apply(weight, 1, mean))
return(out)
}
aa.test <- function()
{
ia = aa.test.create.ia()
png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot.ia(ia)
dev.off()
png(filename = 'plot2.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1)
par(mar = c(4,4,2,1), cex = 0.8)
x = 100 * ia$risk
y = 100 * ia$expected.return
plot(x, y, xlim = range(c(0, x)), ylim = range(c(0, y)),
xlab='Risk', ylab='Return', main='Risk vs Return', col='black')
grid();
text(x, y, ia$symbols,	col = 'blue', adj = c(1,1), cex = 0.8)
dev.off()
n = ia$n
constraints = new.constraints(n, lb = 0, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ef = portopt(ia, constraints, 50, 'Efficient Frontier')
png(filename = 'plot3.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot.ef(ia, list(ef))
dev.off()
ef.risk = portopt(ia, constraints, 50, 'Risk')
ef.maxloss = portopt(ia, constraints, 50, 'Max Loss', min.maxloss.portfolio)
ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio.maxloss, F)
plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad), portfolio.mad, F)
dev.off()
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.transition.map(ef.risk)
plot.transition.map(ef.maxloss)
plot.transition.map(ef.mad)
dev.off()
}
aa.long.short.test <- function()
{
ia = aa.test.create.ia()
n = ia$n
constraints = new.constraints(n, lb = -0.5, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ef.risk = portopt(ia, constraints, 50, 'Risk')
ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.mad)
dev.off()
constraints = new.constraints(n, lb = -0.5, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
constraints = add.variables(n, constraints)
constraints = add.constraints(rbind(diag(n), diag(n)), rep(0, n), type = '>=', constraints)
constraints = add.constraints(rbind(diag(n), -diag(n)), rep(0, n), type = '<=', constraints)
constraints = add.constraints(c(rep(0, n), rep(1, n)), 1.6, type = '=', constraints)
ef.risk = portopt(ia, constraints, 50, 'Risk')
ef.risk$weight = ef.risk$weight[,(1:n)]
ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
ef.mad$weight = ef.mad$weight[,(1:n)]
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.mad)
dev.off()
ia.ls = aa.test.ia.add.short(ia)
constraints = new.constraints(2*n, lb = 0, ub = c(rep(0.8,n),rep(0.5,n)))
constraints = add.constraints(c(rep(1,n), -rep(1,n)), 1, type = '=', constraints)
constraints = add.constraints(c(rep(1,n), rep(1,n)), 1.6, type = '=', constraints)
ef.risk = portopt(ia.ls, constraints, 50, 'Risk')
ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, (n+1):(2*n)]
ef.mad = portopt(ia.ls, constraints, 50, 'MAD', min.mad.portfolio)
ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n+1):(2*n)]
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.mad)
dev.off()
constraints = new.constraints(2*n, lb = 0, ub = c(rep(0.8,n),rep(0.5,n)))
constraints = add.constraints(c(rep(1,n), -rep(1,n)), 1, type = '=', constraints)
constraints = add.constraints(c(rep(1,n), rep(1,n)), 3, type = '=', constraints)
ef.risk = portopt(ia.ls, constraints, 50, 'Risk')
ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, (n+1):(2*n)]
ef.mad = portopt(ia.ls, constraints, 50, 'MAD', min.mad.portfolio)
ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n+1):(2*n)]
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.mad)
dev.off()
constraints = new.constraints(2*n, lb = 0, ub = c(rep(0.8,n),rep(0.5,n)))
constraints = add.constraints(c(rep(1,n), -rep(1,n)), 1, type = '=', constraints)
constraints = add.constraints(c(rep(1,n), rep(1,n)), 3, type = '=', constraints)
constraints = add.variables(n, constraints)
constraints$binary.index = (2*n+1):(3*n)
constraints = add.constraints(rbind(diag(n), 0*diag(n), -diag(n)), rep(0, n), type = '<=', constraints)
constraints = add.constraints(rbind(0*diag(n), diag(n), diag(n)), rep(1, n), type = '<=', constraints)
ef.risk = portopt(ia.ls, constraints, 50, 'Risk')
ef.risk$weight = ef.risk$weight[, 1:n] - ef.risk$weight[, (n+1):(2*n)]
ef.mad = portopt(ia.ls, constraints, 50, 'MAD', min.mad.portfolio)
ef.mad$weight = ef.mad$weight[, 1:n] - ef.mad$weight[, (n+1):(2*n)]
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.mad)
dev.off()
}
aa.cardinality.test <- function()
{
ia = aa.test.create.ia()
n = ia$n
constraints = new.constraints(n, lb = 0, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ef.risk = portopt(ia, constraints, 50, 'Risk')
ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
aa.plot.ef.summary.test <- function(ef)
{
layout(1:2)
par(mar = c(4,4,2,1), cex = 0.8)
y = iif(ef$weight > 0.000001, ef$weight, NA)
plot(as.vector(sort(100 * y)), pch=20, xaxt='n', ylim = c(0, 80),
xlab='', ylab='Weight', main='Portfolio Weights')
abline(h=0, col = 'red')
abline(h=10, col = 'red')
plot(100* ef$risk, rowSums(!is.na(y), na.rm = T), pch=20, type='b',
xlab='Risk', ylab='Number of Assets', main='Number of Assets')
}
aa.plot.ef.summary.test(ef.risk)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.mad)
dev.off()
constraints = new.constraints(n,rep(1, n), 1, type = '=')
constraints = add.variables(n, constraints)
constraints$binary.index = (n+1):(2*n)
constraints = add.constraints(rbind(diag(n), -0.1 * diag(n)), rep(0, n), type = '>=', constraints)
constraints = add.constraints(rbind(diag(n), -0.8 * diag(n)), rep(0, n), type = '<=', constraints)
ef.risk = portopt(ia, constraints, 50, 'Risk')
ef.risk$weight = ef.risk$weight[, 1:n]
ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
ef.mad$weight = ef.mad$weight[, 1:n]
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
aa.plot.ef.summary.test(ef.risk)
dev.off()
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.mad)
dev.off()
constraints = new.constraints(n, rep(1, n), 1, type = '=')
constraints = add.variables(n, constraints)
constraints$binary.index = (n+1):(2*n)
constraints = add.constraints(rbind(diag(n), -0.00001 * diag(n)), rep(0, n), type = '>=', constraints)
constraints = add.constraints(rbind(diag(n), -0.8 * diag(n)), rep(0, n), type = '<=', constraints)
constraints = add.constraints(c(rep(0,n), rep(1,n)), 3, type = '=', constraints)
ef.risk = portopt(ia, constraints, 50, 'Risk')
ef.risk$weight = ef.risk$weight[, 1:n]
ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
ef.mad$weight = ef.mad$weight[, 1:n]
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
aa.plot.ef.summary.test(ef.risk)
dev.off()
png(filename = 'plot6.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.mad), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.mad)
dev.off()
}
aa.avg.cor.test <- function()
{
ia = aa.test.create.ia()
n = ia$n
constraints = new.constraints(n, lb = 0, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ef.risk = portopt(ia, constraints, 50, 'Risk')
ef.cor.insteadof.cov = portopt(ia, constraints, 50, 'Cor instead of Cov', min.cor.insteadof.cov.portfolio)
ef.avgcor = portopt(ia, constraints, 50, 'AvgCor', min.avgcor.portfolio)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
plot.ef(ia, list(ef.risk, ef.avgcor, ef.cor.insteadof.cov), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.avgcor, ef.cor.insteadof.cov), portfolio.avgcor, F)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.transition.map(ef.risk)
plot.transition.map(ef.avgcor)
plot.transition.map(ef.cor.insteadof.cov)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot.ia(ia)
dev.off()
ef.random = list()
ef.random$name = 'Random'
ef.random$weight = randfixedsum(1000000, n, 1, 0, 0.8)
ef.random$risk = portfolio.avgcor(ef.random$weight, ia)
ef.random$return = portfolio.return(ef.random$weight, ia)
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1)
plot(100*ef.random$risk, 100*ef.random$return, type='p', pch=20,
xlim = 100*range(0, ef.random$risk, ef.avgcor$risk),
ylim = 100*range(0, ef.random$return, ef.avgcor$return),
main = 'Average Correlation Efficient Frontier vs Random Portfolios',
xlab = 'portfolio.avgcor',
ylab = 'Return'
)
lines(100*portfolio.avgcor(ef.avgcor$weight, ia), 100*ef.avgcor$return, type='l', lwd=2,col = 'red')
dev.off()
}
aa.erc.test <- function()
{
ia = aa.test.create.ia()
n = ia$n
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ef.risk = portopt(ia, constraints, 50, 'Risk')
layout( 1:3 )
plot.ef(ia, list(ef.risk), portfolio.risk, F)
plot.transition.map(ef.risk)
plot.transition.map(portfolio.risk.contribution(ef.risk$weight, ia),
ef.risk$risk, name='Risk Contribution')
x = rep(1/ia$n,ia$n)
round(100*portfolio.risk.contribution(x, ia),1)
x = find.erc.portfolio(ia, constraints)
round(100*portfolio.risk.contribution(x, ia),1)
s = (c(1,2,3,4)/10)
cor = 0.5 + 0*diag(4)
diag(cor) = 1
cov = cor * (s %*% t(s))
weight = rep(1/4,4)
weight = c(100,0,0,0)/100
weight = c(48,24,16,12)/100
ia$n = 4
ia$cov=cov
round(100*portfolio.risk(weight, ia),1)
round(100*portfolio.risk.contribution(weight, ia),1)
s = c(12,10,11,13,12)/100
cor = 0.6 + 0*diag(5)
diag(cor) = 1
cov = cor * (s %*% t(s))
weight = c(23.96,6.43,16.92,28.73,23.96)/100
weight = c(19.2,23,20.8,17.7,19.2)/100
ia$n = 5
ia$cov=cov
round(100*portfolio.risk(weight, ia),1)
round(100*portfolio.risk.contribution(weight, ia),1)
}
aa.gini.test <- function()
{
ia = aa.test.create.ia.rebal()
ia$risk = apply(coredata(ia$hist.returns),2,sd)
ia$correlation = cor(coredata(ia$hist.returns), use='complete.obs',method='pearson')
ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
n = ia$n
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ef.risk = portopt(ia, constraints, 50, 'Risk')
ef.gini = portopt(ia, constraints, 50, 'GINI', min.gini.portfolio)
png(filename = 'plot1g.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.gini), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.gini), portfolio.gini.coefficient, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.gini)
dev.off()
ia = list()
ia$n = 3
ia$hist.returns = matrix(0,3,3)
ia$hist.returns[1,] = c(10,9,6)/100
ia$hist.returns[2,] = c(15,8,12)/100
ia$hist.returns[3,] = c(12,7,15)/100
}
aa.cvar.test <- function()
{
ia = aa.test.create.ia()
n = ia$n
constraints = new.constraints(n, lb = 0, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ia$parameters.alpha = 0.95
ef.risk = 		portopt(ia, constraints, 50, 'Risk')
ef.maxloss = 	portopt(ia, constraints, 50, 'MaxLoss',	min.maxloss.portfolio)
ef.mad = 		portopt(ia, constraints, 50, 'MAD', 	min.mad.portfolio)
ef.cvar = 		portopt(ia, constraints, 50, 'CVaR', 	min.cvar.portfolio)
ef.cdar = 		portopt(ia, constraints, 50, 'CDaR', 	min.cdar.portfolio)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio.cvar, F)
plot.ef(ia, list(ef.risk, ef.cvar, ef.cdar), portfolio.cdar, F)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.transition.map(ef.risk)
plot.transition.map(ef.cvar)
plot.transition.map(ef.cdar)
dev.off()
return()
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), portfolio.maxloss, F)
plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), portfolio.cvar, F)
plot.ef(ia, list(ef.risk, ef.maxloss, ef.mad, ef.cvar, ef.cdar), portfolio.cdar, F)
layout( matrix(1:4, nrow = 2) )
plot.transition.map(ef.maxloss)
plot.transition.map(ef.mad)
plot.transition.map(ef.cvar)
plot.transition.map(ef.cdar)
}
aa.omega.test <- function()
{
ia = aa.test.create.ia()
n = ia$n
constraints = new.constraints(n, lb = 0, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ia$parameters.omega = 13/100
ia$parameters.omega = 12/100
ia$parameters.omega = ia$parameters.omega / 12
ef.risk = portopt(ia, constraints, 50, 'Risk')
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2, byrow=T) )
rownames(ef.risk$weight) = paste('Risk','weight',1:50,sep='_')
plot.omega(ef.risk$weight[c(1,10,40,50), ], ia)
temp = diag(n)
rownames(temp) = ia$symbols
plot.omega(temp, ia)
plot.ef(ia, list(ef.risk), portfolio.omega, T, T)
dev.off()
ef.omega = portopt.omega(ia, constraints, 50, 'Omega')
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2, byrow=T) )
plot.omega(ef.risk$weight[c(1,10,40,50), ], ia)
rownames(ef.omega$weight) = paste('Omega','weight',1:50,sep='_')
plot.omega(ef.omega$weight[c(1,10,40,50), ], ia)
plot.ef(ia, list(ef.omega, ef.risk), portfolio.omega, T, T)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk,ef.omega), portfolio.risk, F)
plot.ef(ia, list(ef.risk,ef.omega), portfolio.omega, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.omega)
dev.off()
}
aa.downside.test <- function()
{
ia = aa.test.create.ia()
n = ia$n
constraints = new.constraints(n, lb = 0, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ia$parameters.mar = 0/100
ia$parameters.mar = ia$parameters.mar / 12
ef.mad = portopt(ia, constraints, 50, 'MAD', min.mad.portfolio)
ef.mad.downside = portopt(ia, constraints, 50, 'S-MAD', min.mad.downside.portfolio)
ef.risk = portopt(ia, constraints, 50, 'Risk')
ef.risk.downside = portopt(ia, constraints, 50, 'S-Risk', min.risk.downside.portfolio)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.mad.downside, ef.mad), portfolio.mad, F)
plot.ef(ia, list(ef.mad.downside, ef.mad), portfolio.mad.downside, F)
plot.transition.map(ef.mad)
plot.transition.map(ef.mad.downside)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk.downside, ef.risk), portfolio.risk, F)
plot.ef(ia, list(ef.risk.downside, ef.risk), portfolio.risk.downside, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.risk.downside)
dev.off()
}
aa.multiple.risk.measures.test <- function()
{
ia = aa.test.create.ia()
n = ia$n
constraints = new.constraints(n, lb = 0, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ef.risk = 		portopt(ia, constraints, 50, 'Risk')
ef.maxloss = 	portopt(ia, constraints, 50, 'MaxLoss',	min.maxloss.portfolio)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.maxloss), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.maxloss), portfolio.maxloss, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.maxloss)
dev.off()
constraints = add.constraint.maxloss(ia, 12/100, '<=', constraints)
ef.risk.maxloss = 		portopt(ia, constraints, 50, 'Risk+MaxLoss')
ef.risk.maxloss$weight = ef.risk.maxloss$weight[, 1:n]
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk.maxloss, ef.risk, ef.maxloss), portfolio.risk, F)
plot.ef(ia, list(ef.risk.maxloss, ef.risk, ef.maxloss), portfolio.maxloss, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.risk.maxloss)
dev.off()
return()
constraints = new.constraints(n, lb = 0, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ia$parameters.alpha = 0.95
ef.risk = 		portopt(ia, constraints, 50, 'Risk')
ef.maxloss = 	portopt(ia, constraints, 50, 'MaxLoss',	min.maxloss.portfolio)
ef.mad = 		portopt(ia, constraints, 50, 'MAD', 	min.mad.portfolio)
ef.cvar = 		portopt(ia, constraints, 50, 'CVaR', 	min.cvar.portfolio)
ef.cdar = 		portopt(ia, constraints, 50, 'CDaR', 	min.cdar.portfolio)
layout(1)
plot.ef(ia, list(ef.risk, ef.maxloss), portfolio.maxloss, F)
constraints = new.constraints(n, lb = 0, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
constraints = add.constraint.maxloss(ia, 15/100, '<=', constraints)
ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
ef.risk.new$weight = ef.risk.new$weight[, 1:n]
layout(1:2)
plot.ef(ia, list(ef.risk), portfolio.maxloss, F)
plot.ef(ia, list(ef.risk.new), portfolio.maxloss, F)
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk.new, ef.risk,ef.maxloss), portfolio.maxloss, F)
plot.ef(ia, list(ef.risk.new, ef.risk, ef.maxloss), portfolio.risk, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.risk.new)
layout(1)
plot.ef(ia, list(ef.risk, ef.mad), portfolio.mad, F)
constraints = new.constraints(n, lb = 0, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
constraints = add.constraint.mad(ia, 2.9/100, '<=', constraints)
ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
ef.risk.new$weight = ef.risk.new$weight[, 1:n]
layout(1:2)
plot.ef(ia, list(ef.risk), portfolio.mad, F)
plot.ef(ia, list(ef.risk.new), portfolio.mad, F)
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk.new, ef.risk,ef.mad), portfolio.mad, F)
plot.ef(ia, list(ef.risk.new, ef.risk, ef.mad), portfolio.risk, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.risk.new)
layout(1)
plot.ef(ia, list(ef.risk, ef.cvar), portfolio.cvar, F)
constraints = new.constraints(n, lb = 0, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
constraints = add.constraint.cvar(ia, 8/100, '<=', constraints)
ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
ef.risk.new$weight = ef.risk.new$weight[, 1:n]
layout(1:2)
plot.ef(ia, list(ef.risk), portfolio.cvar, F)
plot.ef(ia, list(ef.risk.new), portfolio.cvar, F)
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk.new, ef.risk,ef.cvar), portfolio.cvar, F)
plot.ef(ia, list(ef.risk.new, ef.risk, ef.cvar), portfolio.risk, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.risk.new)
layout(1)
plot.ef(ia, list(ef.risk, ef.cdar), portfolio.cdar, F)
constraints = new.constraints(n, lb = 0, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
constraints = add.constraint.cdar(ia, 15/100, '<=', constraints)
ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
ef.risk.new$weight = ef.risk.new$weight[, 1:n]
layout(1:2)
plot.ef(ia, list(ef.risk), portfolio.cdar, F)
plot.ef(ia, list(ef.risk.new), portfolio.cdar, F)
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk.new, ef.risk,ef.cdar), portfolio.cdar, F)
plot.ef(ia, list(ef.risk.new, ef.risk, ef.cdar), portfolio.risk, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.risk.new)
layout(1:2)
plot.ef(ia, list(ef.risk, ef.maxloss), portfolio.maxloss, F)
plot.ef(ia, list(ef.risk, ef.cdar), portfolio.cdar, F)
constraints = new.constraints(n, lb = 0, ub = 0.8)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
constraints = add.constraint.maxloss(ia, 15/100, '<=', constraints)
constraints = add.constraint.cdar(ia, 15/100, '<=', constraints)
ef.risk.new = portopt(ia, constraints, 50, 'Risk+')
ef.risk.new$weight = ef.risk.new$weight[, 1:n]
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk), portfolio.maxloss, F)
plot.ef(ia, list(ef.risk.new), portfolio.maxloss, F)
plot.ef(ia, list(ef.risk), portfolio.cdar, F)
plot.ef(ia, list(ef.risk.new), portfolio.cdar, F)
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk.new, ef.risk, ef.maxloss, ef.cdar), portfolio.maxloss, F)
plot.ef(ia, list(ef.risk.new, ef.risk, ef.maxloss, ef.cdar), portfolio.cdar, F)
plot.ef(ia, list(ef.risk.new, ef.risk, ef.maxloss, ef.cdar), portfolio.risk, F)
layout( matrix(1:4, nrow = 2) )
plot.transition.map(ef.risk)
plot.transition.map(ef.risk.new)
plot.transition.map(ef.maxloss)
plot.transition.map(ef.cdar)
}
aa.control.risk.return.test <- function()
{
tickers = spl('EEM,EFA,GLD,IWM,IYR,QQQ,SPY,TLT')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='2012:12::')
prices = data$prices
n=ncol(prices)
ret = na.omit(prices/mlag(prices)-1)
ia = create.historical.ia(ret,252)
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(diag(n), type='>=', b=0, constraints)
constraints = add.constraints(diag(n), type='<=', b=1, constraints)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ef = portopt(ia, constraints, 50, 'Efficient Frontier')
risk.fn = portfolio.risk
plot.ef(ia, list(ef), risk.fn, transition.map=F)
weight = min.var.portfolio(ia,constraints)
points(100 * risk.fn(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='red')
weight = max.sharpe.portfolio()(ia,constraints)
points(100 * risk.fn(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='orange')
weight = max.return.portfolio(ia,constraints)
points(100 * risk.fn(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='green')
weight = risk.parity.portfolio()(ia,constraints)
points(100 * risk.fn(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='green')
target.return = 24/100
constraints1 = add.constraints(ia$expected.return,type='>=', b=target.return, constraints)
weight = min.var.portfolio(ia,constraints1)
points(100 * risk.fn(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='orange')
target.risk = 12/100
target.mad = approx(portfolio.risk(ef$weight,ia), portfolio.mad(ef$weight,ia), target.risk, method='linear')$y
constraints1 = add.constraint.mad(ia, type='<=', value=target.mad, constraints)
weight = max.return.portfolio(ia,constraints1)
points(100 * risk.fn(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='orange')
target.return = 24/100
target.risk = 12/100
target.mad = approx(portfolio.risk(ef$weight,ia), portfolio.mad(ef$weight,ia), target.risk, method='linear')$y
target.mad = target.mad
constraints1 = add.constraints(ia$expected.return,type='>=', b=target.return, constraints)
constraints1 = add.constraint.mad(ia, type='>=', value=target.mad, constraints1)
f.obj.return = c(ia$expected.return, rep(0, nrow(constraints1$A) - ia$n))
f.obj.mad = constraints1$A[, ncol(constraints1$A)]
weight = lp.obj.portfolio(ia, constraints1, f.obj.return + f.obj.mad )
points(100 * risk.fn(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='orange')
100 * portfolio.mad(weight, ia)
100 * target.mad
100 * portfolio.risk(weight, ia)
100 * portfolio.return(weight, ia)
}
aa.solutions2instability.test <- function()
{
ia = aa.test.create.ia.rebal()
n = ia$n
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ef.risk = portopt(ia, constraints, 50, 'Risk', equally.spaced.risk = T)
ef.risk.resampled = portopt.resampled(ia, constraints, 50, 'Risk Resampled',
nsamples = 200, sample.len= 10)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(c(1,1,2,3), nrow = 2, byrow=T) )
plot.ef(ia, list(ef.risk, ef.risk.resampled), portfolio.risk, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.risk.resampled)
dev.off()
load.packages('tawny')
ia.original = ia
ia$cov = tawny::cov.shrink(ia$hist.returns)
ef.risk.cov.shrink = portopt(ia, constraints, 50, 'Risk Ledoit-Wolf', equally.spaced.risk = T)
ia = ia.original
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(c(1,1,2,3), nrow = 2, byrow=T) )
plot.ef(ia, list(ef.risk, ef.risk.cov.shrink), portfolio.risk, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.risk.cov.shrink)
dev.off()
ef.risk.resampled.shrink = portopt.resampled(ia, constraints, 50, 'Risk Ledoit-Wolf+Resampled',
nsamples = 200, sample.len= 10, shrinkage.fn=tawny::cov.shrink)
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(c(1:4), nrow = 2, byrow=T) )
plot.ef(ia, list(ef.risk, ef.risk.resampled, ef.risk.resampled.shrink), portfolio.risk, F)
plot.transition.map(ef.risk)
plot.transition.map(ef.risk.resampled)
plot.transition.map(ef.risk.resampled.shrink)
dev.off()
}
aa.arithmetic.geometric.test <- function()
{
ia = aa.test.create.ia.rebal()
n = ia$n
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ef.risk = portopt(ia, constraints, 50, 'Arithmetic', equally.spaced.risk = T)
ef.risk.geometric = ef.risk
ef.risk.geometric$name = 'Geometric'
ef.risk.geometric$return = portfolio.geometric.return(ef.risk$weight, ia)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot.ef(ia, list(ef.risk, ef.risk.geometric), portfolio.risk, T)
dev.off()
ef.risk.A10 = ef.risk
ef.risk.A10$name = 'A(1;0)'
ef.risk.A10$return = apply( cbind(ef.risk$return, ef.risk$risk), 1,
function(x) aritm2geom(x[1], x[2], 1, 0) )
ef.risk.A11 = ef.risk
ef.risk.A11$name = 'A(1;1)'
ef.risk.A11$return = apply( cbind(ef.risk$return, ef.risk$risk), 1,
function(x) aritm2geom(x[1], x[2], 1, 1) )
ia.G = ia
ia.G$expected.return = apply( cbind(ia$geometric.return, ia$risk), 1,
function(x) geom2aritm(x[1], x[2], 1, 0) )
ef.risk.G10 = portopt(ia.G, constraints, 50, 'G(1;0)',equally.spaced.risk = T)
ef.risk.G10$return = apply( cbind(ef.risk.G10$return, ef.risk.G10$risk), 1,
function(x) aritm2geom(x[1], x[2], 1, 0) )
ia.G$expected.return = apply( cbind(ia$geometric.return, ia$risk), 1,
function(x) geom2aritm(x[1], x[2], 1, 1) )
ef.risk.G11 = portopt(ia.G, constraints, 50, 'G(1;1)',equally.spaced.risk = T)
ef.risk.G11$return = apply( cbind(ef.risk.G11$return, ef.risk.G11$risk), 1,
function(x) aritm2geom(x[1], x[2], 1, 1) )
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.A10), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.A11), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.G10), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.G11), portfolio.risk, F)
dev.off()
ef.risk.A4 = ef.risk
ef.risk.A4$name = 'Risk A4'
ef.risk.A4$return = apply( cbind(ef.risk$return, ef.risk$risk), 1,
function(x) aritm2geom4(x[1], x[2]) )
ia.G = ia
ia.G$expected.return = apply( cbind(ia$geometric.return, ia$risk), 1,
function(x) geom2aritm4(x[1], x[2]) )
ef.risk.G4 = portopt(ia.G, constraints, 50, 'Risk G4',equally.spaced.risk = T)
ef.risk.G4$return = apply( cbind(ef.risk.G4$return, ef.risk.G4$risk), 1,
function(x) aritm2geom4(x[1], x[2]) )
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:2, nrow = 2) )
plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.A4), portfolio.risk, F)
plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.G4), portfolio.risk, F)
dev.off()
ef.true.geometric = ef.risk
ef.true.geometric$name = 'True Geometric'
constraints$x0 = ef.risk$weight[1,]
for(i in 1:len(ef.risk$risk)) {
cat('i =', i, '\n')
ef.true.geometric$weight[i,] = max.geometric.return.portfolio(ia, constraints, ef.risk$risk[i], ef.risk$risk[i])
constraints$x0 = ef.true.geometric$weight[i,]
}
ef.true.geometric$return = portfolio.geometric.return(ef.true.geometric$weight, ia)
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk.geometric, ef.risk, ef.true.geometric), portfolio.risk, T, T)
plot.ef(ia, list(ef.true.geometric, ef.risk, ef.risk.geometric), portfolio.risk, T, T)
dev.off()
ef.random = list()
ef.random$name = 'Random'
ef.random$weight = randfixedsum(100000, n, 1, 0, 1)
ef.random$risk = portfolio.risk(ef.random$weight, ia)
ef.random$return = portfolio.geometric.return(ef.random$weight, ia)
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1)
plot(100*ef.random$risk, 100*ef.random$return, type='p', pch=20,
xlim = 100*range(ef.random$risk, ef.true.geometric$risk),
ylim = 100*range(ef.random$return, ef.true.geometric$return),
main = 'True Geometric Efficient Frontier vs Random Portfolios',
xlab = 'portfolio.risk',
ylab = 'Return'
)
lines(100*ef.true.geometric$risk, 100*ef.true.geometric$return, type='l', lwd=2,col = 'red')
dev.off()
return()
ef.risk.unrebalanced = ef.risk
ef.risk.unrebalanced$name = 'Unrebalanced'
ef.risk.unrebalanced$return = portfolio.unrebalanced.return(ef.risk$weight, ia)
plot.ef(ia, list(ef.risk, ef.risk.geometric, ef.risk.unrebalanced), portfolio.risk, T)
ia.G = ia
ia.G$expected.return = ia$geometric.return
ef.risk.geometric1 = portopt(ia.G, constraints, 50, 'Geometric1',equally.spaced.risk = T)
plot.ef(ia, list(ef.risk, ef.risk.geometric,ef.risk.geometric1), portfolio.risk, T)
x=max.geometric.return.portfolio(ia, constraints, 0, 1)
lines( portfolio.risk(t(x), ia), portfolio.geometric.return(t(x), ia), type='p', pch=20, col = 'blue')
}
aa.periodic.table.test <- function()
{
ia = aa.test.create.ia.country('1990::')
hist.returns = ia$hist.returns
hist.prices = cumprod(1 + hist.returns)
period.ends = endpoints(hist.prices, 'years')
hist.prices = hist.prices[period.ends, ]
hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )
hist.returns = hist.returns['2000::']
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot.periodic.table1(hist.returns)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot.periodic.table2(hist.returns)
dev.off()
}
aa.style.summary.plot <- function(name, style.weights, style.r.squared, window.len)
{
layout( matrix(c(1,2,2,3,3,3), nrow=2, byrow=T) )
weight = last(style.weights)
plot.table(t(round(100*weight)))
plota(100*style.r.squared, type='l', LeftMargin = 3, main=paste(window.len, 'months window Linear Least Squares Regression R^2'))
plot.transition.map(style.weights, index(style.weights), xlab='', name=name)
}
aa.style.test <- function()
{
load.packages('quantmod')
symbols = spl('FMILX,EWA,EWC,EWQ,EWG,EWJ,EWU,SPY')
symbols = spl('FWWFX,EWA,EWC,EWQ,EWG,EWJ,EWU,SPY')
symbol.names = spl('Fund,Australia,Canada,France,Germany,Japan,UK,USA')
getSymbols(symbols, from = '1980-01-01', auto.assign = TRUE)
hist.prices = merge(FWWFX,EWA,EWC,EWQ,EWG,EWJ,EWU,SPY)
period.ends = endpoints(hist.prices, 'months')
hist.prices = Ad(hist.prices)[period.ends, ]
index(hist.prices) = as.Date(paste('1/', format(index(hist.prices), '%m/%Y'), sep=''), '%d/%m/%Y')
colnames(hist.prices) = symbol.names
hist.prices = na.omit(hist.prices['1990::2010'])
hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )
TB3M = quantmod::getSymbols('TB3MS', src='FRED', auto.assign = FALSE)
TB3M = processTBill(TB3M, timetomaturity = 1/4)
index(TB3M) = as.Date(paste('1/', format(index(TB3M), '%m/%Y'), sep=''), '%d/%m/%Y')
TB3M = ROC(Ad(TB3M), type = 'discrete')
colnames(TB3M) = 'Cash'
hist.returns = na.omit( merge(hist.returns, TB3M) )
ndates = nrow(hist.returns)
n = ncol(hist.returns)-1
window.len = 36
style.weights = hist.returns[, -1]
style.weights[] = NA
style.r.squared = hist.returns[, 1]
style.r.squared[] = NA
for( i in window.len:ndates ) {
window.index = (i - window.len + 1) : i
fit = lm.constraint( hist.returns[window.index, -1], hist.returns[window.index, 1] )
style.weights[i,] = fit$coefficients
style.r.squared[i,] = fit$r.squared
}
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
aa.style.summary.plot('Style UnConstrained', style.weights, style.r.squared, window.len)
dev.off()
load.packages('quadprog')
style.weights[] = NA
style.r.squared[] = NA
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
for( i in window.len:ndates ) {
window.index = (i - window.len + 1) : i
fit = lm.constraint( hist.returns[window.index, -1], hist.returns[window.index, 1], constraints )
style.weights[i,] = fit$coefficients
style.r.squared[i,] = fit$r.squared
}
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
aa.style.summary.plot('Style Constrained', style.weights, style.r.squared, window.len)
dev.off()
style.weights[] = NA
style.r.squared[] = NA
temp = rep(0, n)
names(temp) = colnames(hist.returns)[-1]
lb = temp
ub = temp
ub[] = 1
lb['Australia'] = 0
ub['Australia'] = 5
lb['Canada'] = 0
ub['Canada'] = 5
lb['France'] = 0
ub['France'] = 15
lb['Germany'] = 0
ub['Germany'] = 15
lb['Japan'] = 0
ub['Japan'] = 15
lb['UK'] = 0
ub['UK'] = 25
lb['USA'] = 30
ub['USA'] = 100
lb['Cash'] = 2
ub['Cash'] = 15
constraints = new.constraints(n, lb = lb/100, ub = ub/100)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
for( i in window.len:ndates ) {
window.index = (i - window.len + 1) : i
fit = lm.constraint( hist.returns[window.index, -1], hist.returns[window.index, 1], constraints )
style.weights[i,] = fit$coefficients
style.r.squared[i,] = fit$r.squared
}
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
aa.style.summary.plot('Style Constrained+Limits', style.weights, style.r.squared, window.len)
dev.off()
manager.returns = hist.returns[, 1]
manager.returns = manager.returns[window.len:ndates,]
implied.returns = as.xts( rowSums(style.weights * hist.returns[, -1]), index(hist.returns))
implied.returns = implied.returns[window.len:ndates,]
tracking.error = manager.returns - implied.returns
alpha = 12*mean(tracking.error)
covar.alpha = 12* cov(tracking.error)
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
plota(cumprod(1+manager.returns), type='l')
plota.lines(cumprod(1+implied.returns), col='red')
plota.legend('Fund,Style', 'black,red')
par(mar = c(4,4,2,1))
hist(100*tracking.error, xlab='Monthly Tracking Error',
main= paste('Annualized Alpha =', round(100*alpha,1), 'Std Dev =', round(100*sqrt(covar.alpha),1))
)
dev.off()
}
aa.black.litterman.test <- function()
{
hist.caps = aa.test.hist.capitalization()
hist.caps.weight = hist.caps/rowSums(hist.caps)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot.transition.map(hist.caps.weight, index(hist.caps.weight), xlab='', name='Market Capitalization Weight History')
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:9, nrow = 3, byrow=T) )
col = plota.colors(ncol(hist.caps))
for(i in 1:ncol(hist.caps)) {
plota(hist.caps[,i], type='l', lwd=5, col=col[i], main=colnames(hist.caps)[i])
}
dev.off()
ia = aa.test.create.ia.country()
ir = get.fedfunds.rate()
period = join( format(range(index(ia$hist.returns)), '%Y:%m'), '::')
risk.aversion = bl.compute.risk.aversion( ia$hist.returns$USA, ir[period]/ia$annual.factor )
risk.aversion = bl.compute.risk.aversion( ia$hist.returns$USA )
cap.weight = last(hist.caps.weight)
ia.bl = ia
ia.bl$expected.return = bl.compute.eqret( risk.aversion, ia$cov, cap.weight, last(ir[period]) )
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(c(1,1,2,3), nrow=2, byrow=T) )
pie(coredata(cap.weight), paste(colnames(cap.weight), round(100*cap.weight), '%'),
main = paste('Country Market Capitalization Weights for', format(last(index(ia$hist.returns)),'%b %Y'))
, col=plota.colors(ia$n))
plot.ia(ia.bl, T)
dev.off()
n = ia$n
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ef.risk = portopt(ia, constraints, 50, 'Historical', equally.spaced.risk = T)
ef.risk.bl = portopt(ia.bl, constraints, 50, 'Black-Litterman', equally.spaced.risk = T)
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia, list(ef.risk), portfolio.risk, T, T)
plot.ef(ia.bl, list(ef.risk.bl), portfolio.risk, T, T)
dev.off()
temp = matrix(rep(0, n), nrow = 1)
colnames(temp) = ia$symbols
temp[,'Japan'] = 1
temp[,'UK'] = -1
pmat = temp
qmat = c(0.02)
temp[] = 0
temp[,'Australia'] = 1
pmat = rbind(pmat, temp)
qmat = c(qmat, 0.12)
post = bl.compute.posterior(ia.bl$expected.return, ia$cov, pmat, qmat, tau = 0.025 )
ia.bl.view = ia.bl
ia.bl.view$expected.return = post$expected.return
ia.bl.view$cov = post$cov
ia.bl.view$risk = sqrt(diag(ia.bl.view$cov))
ef.risk.bl.view = portopt(ia.bl.view, constraints, 50, 'Black-Litterman + View(s)', equally.spaced.risk = T)
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout( matrix(1:4, nrow = 2) )
plot.ef(ia.bl, list(ef.risk.bl), portfolio.risk, T, T)
plot.ef(ia.bl.view, list(ef.risk.bl.view), portfolio.risk, T, T)
dev.off()
}
aa.test.hist.capitalization <- function()
{
symbols = spl('Australia	Canada	France	Germany	Japan	United Kingdom	United States', '\t')
data =
'1988	138.0	242.0	245.0	252.0	3910.0	771.0	2790.0
1989	141.0	291.0	365.0	365.0	4390.0	827.0	3510.0
1990	109.0	242.0	314.0	355.0	2920.0	849.0	3060.0
1991	149.0	267.0	348.0	393.0	3130.0	988.0	4090.0
1992	145.0	243.0	351.0	348.0	2400.0	927.0	4490.0
1993	204.9	326.5	456.1	463.5	2999.8	1151.6	5136.2
1994	218.9	315.0	451.3	470.5	3719.9	1210.2	5067.0
1995	245.2	366.3	522.1	577.4	3667.3	1407.7	6857.6
1996	312.0	486.3	591.1	671.0	3088.9	1740.2	8484.4
1997	295.8	567.6	674.4	825.2	2216.7	1996.2	11308.8
1998	328.9	543.4	991.5	1094.0	2495.8	2374.3	13451.4
1999	427.7	800.9	1475.5	1432.2	4546.9	2933.3	16635.1
2000	372.8	841.4	1446.6	1270.2	3157.2	2577.0	15104.0
2001	375.1	700.8	1174.4	1071.7	2251.8	2164.7	13854.6
2002	378.8	575.3	967.0	691.1	2126.1	1864.3	11098.1
2003	585.5	894.0	1355.9	1079.0	3040.7	2460.1	14266.3
2004	776.4	1177.5	1559.1	1194.5	3678.3	2815.9	16323.7
2005	804.1	1480.9	1758.7	1221.3	4736.5	3058.2	16970.9
2006	1095.9	1700.7	2428.6	1637.8	4726.3	3794.3	19425.9
2007	1298.4	2186.6	2771.2	2105.5	4453.5	3858.5	19947.3
2008	675.6	1002.2	1492.3	1108.0	3220.5	1852.0	11737.6
2009	1258.5	1681.0	1972.0	1297.6	3377.9	2796.4	15077.3
2010	1454.5	2160.2	1926.5	1429.7	4099.6	3107.0	17139.0'
hist.caps = matrix( as.double(spl( gsub('\n', '\t', data), '\t')),
nrow = len(spl(data, '\n')), byrow=TRUE)
load.packages('quantmod')
symbol.names = symbols
hist.caps = as.xts( hist.caps[,-1] ,
as.Date(paste('1/1/', hist.caps[,1], sep=''), '%d/%m/%Y')
)
colnames(hist.caps) = symbols
return(hist.caps)
}
get.fedfunds.rate <- function()
{
url = 'http://www.federalreserve.gov/datadownload/Output.aspx?rel=H15&series=40afb80a445c5903ca2c4888e40f3f1f&lastObs=&from=&to=&filetype=csv&label=include&layout=seriescolumn'
txt = readLines(url)
txt = txt[-c(1 : grep('Time Period', txt))]
hist.returns = matrix( spl(txt), nrow = len(txt), byrow=TRUE)
load.packages('quantmod')
hist.returns = as.xts( as.double(hist.returns[,-1]) / 100,
as.Date(paste(hist.returns[,1], '-1', sep=''), '%Y-%m-%d')
)
return(hist.returns)
}
aa.test.create.ia.country <- function(dates = '1990::2010')
{
load.packages('quantmod,quadprog')
symbols = spl('EWA,EWC,EWQ,EWG,EWJ,EWU,SPY')
symbol.names = spl('Australia,Canada,France,Germany,Japan,UK,USA')
getSymbols(symbols, from = '1980-01-01', auto.assign = TRUE)
hist.prices = merge(EWA,EWC,EWQ,EWG,EWJ,EWU,SPY)
period.ends = endpoints(hist.prices, 'months')
hist.prices = Ad(hist.prices)[period.ends, ]
colnames(hist.prices) = symbol.names
annual.factor = 12
hist.prices = na.omit(hist.prices[dates])
hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )
ia = create.historical.ia(hist.returns, annual.factor, symbol.names, symbol.names)
return(ia)
}
aa.test.create.ia.rebal <- function()
{
symbols = spl('SP500	SmallUS   	Europe	Pacific	Japan  	Gold   	20Y_Treas	5Y_Treas	TBills', '\t')
symbols = trim(symbols)
data =
'1970	0.0403	-0.1743	-0.0935	-0.13	-0.156	0.0871	0.121	0.1685	0.0652
1971	0.1432	0.165	0.2803	0.1082	0.6107	-0.0373	0.1324	0.0874	0.0439
1972	0.1898	0.0443	0.1582	0.6678	1.1447	0.602	0.0567	0.0517	0.0384
1973	-0.1466	-0.309	-0.0773	-0.2392	-0.1595	0.9184	-0.011	0.0461	0.0693
1974	-0.2647	-0.1995	-0.2277	-0.4059	-0.1392	0.1094	0.0435	0.0568	0.0801
1975	0.372	0.5282	0.439	0.6342	0.1723	-0.2407	0.0919	0.0782	0.058
1976	0.2384	0.5738	-0.0637	0.0572	0.2637	-0.3258	0.1676	0.1288	0.0508
1977	-0.0718	0.2538	0.2392	0.0334	0.1722	0.3549	-0.0065	0.014	0.0513
1978	0.0656	0.2346	0.243	0.2397	0.5182	0.0934	-0.0118	0.0349	0.072
1979	0.1844	0.4346	0.1467	0.5216	-0.1461	1.6133	-0.0121	0.041	0.1038
1980	0.3242	0.3988	0.1452	0.6149	0.2939	0.6427	-0.0396	0.039	0.1126
1981	-0.0491	0.1388	-0.1045	-0.1547	0.1041	-0.2514	0.0186	0.0944	0.1472
1982	0.2141	0.2801	0.0569	-0.2818	-0.0023	0.4786	0.4037	0.291	0.1053
1983	0.2251	0.3967	0.2238	0.3421	0.2779	0.0259	0.0069	0.0741	0.088
1984	0.0623	-0.0667	0.0126	-0.0724	0.1701	0.2922	0.1554	0.1403	0.0978
1985	0.3216	0.2466	0.7979	0.1729	0.4413	-0.0887	0.3096	0.2034	0.0773
1986	0.1847	0.0685	0.4446	0.4839	0.9185	0.3593	0.2445	0.1513	0.0615
1987	0.0523	-0.093	0.041	0.042	0.4187	0.3753	-0.027	0.029	0.0546
1988	0.1681	0.2287	0.1635	0.3056	0.3534	-0.1846	0.0968	0.0609	0.0636
1989	0.3149	0.1018	0.2906	0.1585	0.0217	0.2538	0.181	0.1327	0.0838
1990	-0.0317	-0.2156	-0.0337	-0.1015	-0.3618	-0.2373	0.062	0.0974	0.0782
1991	0.3055	0.4463	0.1366	0.3661	0.0882	-0.042	0.1926	0.1531	0.056
1992	0.0766	0.2335	-0.0425	0.0701	-0.2111	-0.1598	0.0941	0.072	0.0351
1993	0.099	0.21	0.2979	0.8035	0.2505	0.8287	0.1824	0.1124	0.029
1994	0.012	0.031	0.0266	-0.141	0.2217	-0.1193	-0.0778	-0.0513	0.0391
1995	0.3753	0.3448	0.2213	0.1295	0.0069	0.0191	0.3069	0.1905	0.0551
1996	0.2295	0.1765	0.2895	0.2054	-0.155	0.0706	-0.0127	0.0661	0.0502'
hist.returns = matrix( as.double(spl( gsub('\n', '\t', data), '\t')),
nrow = len(spl(data, '\n')), byrow=TRUE)
load.packages('quantmod')
hist.returns = as.xts( hist.returns[,-1] ,
as.Date(paste('1/1/', hist.returns[,1], sep=''), '%d/%m/%Y')
)
colnames(hist.returns) = symbols
ia = create.historical.ia(hist.returns, 1, symbols)
return(ia)
}
aa.test.create.ia <- function()
{
load.packages('quantmod,quadprog')
symbols = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
symbol.names = spl('S&P 500,Nasdaq 100,Emerging Markets,Russell 2000,EAFE,20 Year Treasury,U.S. Real Estate,Gold')
getSymbols(symbols, from = '1980-01-01', auto.assign = TRUE)
hist.prices = merge(SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD)
month.ends = endpoints(hist.prices, 'months')
hist.prices = Ad(hist.prices)[month.ends, ]
colnames(hist.prices) = symbols
hist.prices = na.omit(hist.prices['1995::2010'])
hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )
ia = create.historical.ia(hist.returns, 12, symbols, symbol.names)
return(ia)
}
aa.test.create.ia.custom <- function(symbols, symbol.names = symbols, dates = NULL)
{
load.packages('quantmod,quadprog')
data <- new.env()
getSymbols(symbols, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates=dates)
hist.prices = data$prices
period.ends = endpoints(hist.prices, 'months')
hist.prices = hist.prices[period.ends, ]
colnames(hist.prices) = symbol.names
annual.factor = 12
hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )
ia = create.historical.ia(hist.returns, annual.factor, symbol.names, symbol.names)
return(ia)
}
create.historical.ia <- function
(
hist.returns,
annual.factor,
symbols = colnames(hist.returns),
symbol.names = symbols)
{
ia = list()
ia$n = len(symbols)
ia$annual.factor = annual.factor
ia$symbols = symbols
ia$symbol.names = symbol.names
ia$hist.returns = hist.returns
ia$arithmetic.return = apply(hist.returns, 2, mean, na.rm = T)
ia$geometric.return = apply(hist.returns, 2, function(x) prod(1+x)^(1/len(x))-1 )
ia$risk = apply(hist.returns, 2, sd, na.rm = T)
ia$correlation = cor(hist.returns, use = 'complete.obs', method = 'pearson')
ia$arithmetic.return = (1 + ia$arithmetic.return)^ia$annual.factor - 1
ia$geometric.return = (1 + ia$geometric.return)^ia$annual.factor - 1
ia$risk = sqrt(ia$annual.factor) * ia$risk
ia$risk = iif(ia$risk == 0, 0.000001, ia$risk)
ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
ia$expected.return = ia$arithmetic.return
return(ia)
}
aa.test.ia.add.short <- function(ia)
{
ia$symbols = c(ia$symbols,ia$symbols)
ia$n = 2*ia$n
ia$hist.returns = cbind(ia$hist.returns, -ia$hist.returns)
ia$expected.return = c(ia$expected.return, -ia$expected.return)
ia$risk = c(ia$risk, ia$risk)
ia$correlation = cbind( rbind(ia$correlation, -ia$correlation), rbind(-ia$correlation, ia$correlation) )
ia$cov = cbind( rbind(ia$cov, -ia$cov), rbind(-ia$cov, ia$cov) )
return(ia)
}
binary_branch_bound <- function
(
index_binvar,
bbb_data,
bbb_solve,
control = bbb_control()
)
{
fbest = Inf
xbest = 0 * bbb_data$x0
counter = 0
nbinvar = length(index_binvar)
flag = 7
stack = new.env()
stack$data = list()
stack$cost = c()
stack$pointer = c()
stack$data[[1]] = list(lb = bbb_data$lb,
ub = bbb_data$ub,
var = 1:nbinvar,
path = rep(0,nbinvar),
level = 0,
fval = Inf)
stack$cost = 0
stack$pointer = 1
control$proborder.selected = control$proborder
if(F) {
lb = bbb_data$lb
ub = bbb_data$ub
for( i in 0:1 ) {
lb[] = i
ub[] = i
sol = match.fun(bbb_solve)(bbb_data, lb, ub)
if( sol$ok ) {
x = sol$x
fval = sol$fval
xi = x[index_binvar]
if ( max(abs( round(xi,0) - xi )) < control$bineps ) {
fbest = fval
xbest = x
flag = 1
if( !control$silent ) cat('FOUND SOLUTION =', fbest, '\n');
}
}
}
}
while ( length(stack$data) > 0 ) {
subprob = bbb_pop(stack)
if( !control$silent ) {
cat('-----------------------------------------------------', '\n')
if( max(subprob$path) > 0 ) {
temp.index = order(-subprob$path)[1 : sum(subprob$path > 0)]
cat('\t',
paste('b', temp.index, ' = ', subprob$lb[temp.index],sep='')
, '\n')
} else {
cat(counter, '\t', 'FIRST NODE', '\n')
}
cat(counter, '\t', subprob$lb, '\t', subprob$var, '\t', subprob$fval, '\t', fbest, '\n')
cat('\t', subprob$ub, '\n')
cat('stack size =', len(stack$pointer), '\n')
}
if( is.finite( subprob$fval ) & is.finite( fbest ) & fbest <= subprob$fval ) {
if( !control$silent ) cat('SKIP this problem because a solution with lower FVAL already found\n')
} else {
counter = counter + 1
sol = match.fun(bbb_solve)(bbb_data, subprob$lb, subprob$ub)
if( !sol$ok ) {
if( !control$silent ) cat('NO SOLUTION EXISTS\n\n');
} else {
x = sol$x
fval = sol$fval
if( !control$silent ) {
cat('SOLUTION OK', '\t', sol$fval, '\n')
cat('\t', round(x[index_binvar[subprob$var]],3), '\n\n')
}
if ( flag !=1 ) flag=5
if ( fval <= fbest ) {
if ( length(subprob$var ) == 0 ) {
fbest = fval
xbest = x
flag = 1
if( !control$silent ) cat('FOUND SOLUTION =', fbest, '\n');
} else {
xi = x[index_binvar[subprob$var]]
if ( max(abs( round(xi,0) - xi )) < control$bineps ) {
fbest = fval
xbest = x
flag = 1
if( !control$silent ) cat('FOUND SOLUTION =', fbest, '\n');
} else {
branchvar = bbb_decision(xi,control)
probs = bbb_separate(subprob, branchvar, fval)
p0 = probs$p0
p1 = probs$p1
if( !control$silent ) cat('Branch on =', subprob$var[branchvar], '\n');
if( control$searchdir == 0 ) {
cost=1/(subprob$level+1)
} else if( control$searchdir == 1 ) {
cost=subprob$level+1
} else if( control$searchdir == 2 ) {
cost=fval
} else if( control$searchdir == 3 ) {
cost=fval/(subprob$level+1)
}
if( control$proborder == 2 ) {
control$proborder.selected = round(xi[branchvar],0)
}
if( control$proborder.selected == 0 ) {
bbb_push(stack, p1, p0, cost)
} else {
bbb_push(stack, p0, p1, cost)
}
}
}
}
}
if( F ) {
cat('counter =', counter, '\n')
cat('fbest     =', fbest, '\n')
cat('stack$pointer =', stack$pointer, '\n')
cat('\n')
}
}
}
rm(list=ls(stack,all=TRUE), envir=stack)
return(list(xmin = xbest, fmin = fbest, counter = counter, flag = flag))
}
bbb_decision <- function
(
xi,
control
)
{
if( control$branchvar == 0 ) {
branchvar = 1
} else if( control$branchvar == 1 ) {
branchvar = which.max( abs(xi-round(xi,0)) )
} else if( control$branchvar == 2 ) {
branchvar = which.min( abs(xi-round(xi,0)) )
} else {
branchvar = 1
}
return(branchvar)
}
bbb_pop <- function(stack)
{
i = stack$pointer[ length(stack$data) ]
subprob   = stack$data[[i]]
stack$pointer[ stack$pointer > i ] = stack$pointer[ stack$pointer > i ] - 1
stack$data[[i]] = NULL
length(stack$cost) = length(stack$data)
length(stack$pointer) = length(stack$data)
return(subprob)
}
bbb_push <- function
(
stack,
element1,
element2,
cost
)
{
n = length(stack$data)
i = match(TRUE, stack$cost <= cost)
if( is.na(i) ) i = n else i = i - 1
stack$data[[ (n+1) ]] = element1
stack$data[[ (n+2) ]] = element2
if(i == 0) {
stack$pointer=c((n+1),(n+2), stack$pointer)
stack$cost=c(cost,cost, stack$cost)
} else {
stack$pointer=c(stack$pointer[1:i], (n+1),(n+2), stack$pointer[-c(1:i)])
stack$cost=c(stack$cost[1:i], cost, cost, stack$cost[-c(1:i)])
}
}
bbb_separate <- function
(
prob,
branchvar,
fval
)
{
if(length(prob$var) >= 1) {
p0 = prob
p0$fval = fval
p0$level = prob$level + 1
p0$var = prob$var[-branchvar]
p0$path[ prob$var[branchvar] ] = 1 + max(p0$path)
p1 = p0
p0$lb[ prob$var[branchvar] ] = 0
p0$ub[ prob$var[branchvar] ] = 0
p1$lb[ prob$var[branchvar] ] = 1
p1$ub[ prob$var[branchvar] ] = 1
} else {
stop('no more integer variables to branch on')
}
return( list(p0 = p0, p1 = p1) )
}
bt.merge <- function
(
b,
align = c('keep.all', 'remove.na'),
dates = NULL
)
{
align = align[1]
symbolnames = b$symbolnames
nsymbols = len(symbolnames)
ncount = sapply(symbolnames, function(i) nrow(b[[i]]))
all.dates = double(sum(ncount))
itemp = 1
for( i in 1:nsymbols ) {
all.dates[itemp : (itemp + ncount[i] -1)] = attr(b[[ symbolnames[i] ]], 'index')
itemp = itemp + ncount[i]
}
temp = sort(all.dates)
unique.dates = c(temp[1], temp[-1][diff(temp)!=0])
if(!is.null(dates)) {
class(unique.dates) = c('POSIXct', 'POSIXt')
temp = make.xts(integer(len(unique.dates)), unique.dates)
unique.dates = attr(temp[dates], 'index')
}
date.map = matrix(NA, nr = len(unique.dates), nsymbols)
itemp = 1
for( i in 1:nsymbols ) {
index = match(all.dates[itemp : (itemp + ncount[i] -1)], unique.dates)
sub.index = which(!is.na(index))
date.map[ index[sub.index], i] = sub.index
itemp = itemp + ncount[i]
}
if( align == 'remove.na' ) {
index = which(count(date.map, side=1) < nsymbols )
} else {
index = which(count(date.map, side=1) < max(1, 0.1 * nsymbols) )
}
if(len(index) > 0) {
date.map = date.map[-index,, drop = FALSE]
unique.dates = unique.dates[-index]
}
class(unique.dates) = c('POSIXct', 'POSIXt')
return( list(all.dates = unique.dates, date.map = date.map))
}
find.names <- function(find.names, all.names)
{
as.list(sapply(spl(find.names), function(x) {
loc = grep(x, all.names, ignore.case = TRUE)
iif(len(loc) > 0, loc, NA)
}))
}
bt.prep <- function
(
b,
align = c('keep.all', 'remove.na'),
dates = NULL,
fill.gaps = F
)
{
if( !exists('symbolnames', b, inherits = F) ) b$symbolnames = ls(b)
symbolnames = b$symbolnames
nsymbols = len(symbolnames)
if( nsymbols > 1 ) {
out = bt.merge(b, align, dates)
for( i in 1:nsymbols ) {
b[[ symbolnames[i] ]] =
make.xts( coredata( b[[ symbolnames[i] ]] )[ out$date.map[,i],, drop = FALSE], out$all.dates)
map.col = find.names('Close,Volume,Open,High,Low,Adjusted', colnames(b[[ symbolnames[i] ]]))
if(fill.gaps & !is.na(map.col$Close)) {
close = coredata(b[[ symbolnames[i] ]][,map.col$Close])
n = len(close)
last.n = max(which(!is.na(close)))
close = ifna.prev(close)
if(last.n + 5 < n) close[last.n : n] = NA
b[[ symbolnames[i] ]][, map.col$Close] = close
index = !is.na(close)
if(!is.na(map.col$Volume)) {
index1 = is.na(b[[ symbolnames[i] ]][, map.col$Volume]) & index
b[[ symbolnames[i] ]][index1, map.col$Volume] = 0
}
for(field in spl('Open,High,Low,Adjusted')) {
j = map.col[[field]]
if(!is.na(j)) {
index1 = is.na(b[[ symbolnames[i] ]][,j]) & index
b[[ symbolnames[i] ]][index1, j] = close[index1]
}}
}
}
} else {
if(!is.null(dates)) b[[ symbolnames[1] ]] = b[[ symbolnames[1] ]][dates,]
out = list(all.dates = index.xts(b[[ symbolnames[1] ]]) )
}
b$dates = out$all.dates
dummy.mat = matrix(double(), len(out$all.dates), nsymbols)
colnames(dummy.mat) = symbolnames
dummy.mat = make.xts(dummy.mat, out$all.dates)
b$weight = dummy.mat
b$execution.price = dummy.mat
for( i in 1:nsymbols ) {
if( has.Cl( b[[ symbolnames[i] ]] ) ) {
dummy.mat[,i] = Cl( b[[ symbolnames[i] ]] );
}
}
b$prices = dummy.mat
}
bt.prep.matrix <- function
(
b,
align = c('keep.all', 'remove.na'),
dates = NULL
)
{
align = align[1]
nsymbols = len(b$symbolnames)
if(!is.null(dates)) {
temp = make.xts(1:len(b$dates), b$dates)
temp = temp[dates]
index = as.vector(temp)
for(i in b$fields) b[[ i ]] = b[[ i ]][index,, drop = FALSE]
b$dates = b$dates[index]
}
if( align == 'remove.na' ) {
index = which(count(b$Cl, side=1) < nsymbols )
} else {
index = which(count(b$Cl,side=1) < max(1,0.1 * nsymbols) )
}
if(len(index) > 0) {
for(i in b$fields) b[[ i ]] = b[[ i ]][-index,, drop = FALSE]
b$dates = b$dates[-index]
}
dummy.mat = make.xts(b$Cl, b$dates)
b$weight = NA * dummy.mat
b$execution.price = NA * dummy.mat
b$prices = dummy.mat
}
bt.prep.matrix.test <- function() {
load.packages('quantmod')
returns = read.xts('Example.csv', date.fn=function(x) paste('1',x), format='%d %b-%y')
prices = bt.apply.matrix(1 + returns, cumprod)
data <- new.env()
data$symbolnames = colnames(prices)
data$dates = index(prices)
data$fields = 'Cl'
data$Cl = prices
bt.prep.matrix(data)
data$weight[] = NA
data$weight[] = 1
buy.hold = bt.run.share(data)
plotbt(buy.hold, plotX = T, log = 'y', LeftMargin = 3)
mtext('Cumulative Performance', side = 2, line = 1)
}
bt.prep.remove.symbols.min.history <- function
(
b,
min.history = 1000
)
{
bt.prep.remove.symbols(b, which( count(b$prices, side=2) < min.history ))
}
bt.prep.remove.symbols <- function
(
b,
index
)
{
if( len(index) > 0 ) {
if( is.character(index) ) index = match(index, b$symbolnames)
b$prices = b$prices[, -index]
b$weight = b$weight[, -index]
b$execution.price = b$execution.price[, -index]
rm(list = b$symbolnames[index], envir = b)
b$symbolnames = b$symbolnames[ -index]
}
}
bt.prep.trim <- function
(
b,
dates = NULL
)
{
if(is.null(dates)) return(b)
data.copy <- new.env()
for(s in b$symbolnames) data.copy[[s]] = b[[s]][dates,,drop=F]
data.copy$symbolnames = b$symbolnames
data.copy$prices = b$prices[dates,,drop=F]
data.copy$weight = b$weight[dates,,drop=F]
data.copy$execution.price = b$execution.price[dates,,drop=F]
return(data.copy)
}
bt.run.share <- function
(
b,
prices = b$prices,
clean.signal = T,
trade.summary = F,
do.lag = 1,
do.CarryLastObservationForwardIfNA = TRUE,
silent = F,
capital = 100000,
commission = 0,
weight = b$weight,
dates = 1:nrow(b$prices)
)
{
prices[] = bt.apply.matrix(coredata(prices), ifna.prev)
weight = mlag(weight, do.lag - 1)
do.lag = 1
if(clean.signal) {
weight[] = (capital / prices) * bt.exrem(weight)
} else {
weight[] = (capital / prices) * weight
}
bt.run(b,
trade.summary = trade.summary,
do.lag = do.lag,
do.CarryLastObservationForwardIfNA = do.CarryLastObservationForwardIfNA,
type='share',
silent = silent,
capital = capital,
commission = commission,
weight = weight,
dates = dates)
}
bt.run <- function
(
b,
trade.summary = F,
do.lag = 1,
do.CarryLastObservationForwardIfNA = TRUE,
type = c('weight', 'share'),
silent = F,
capital = 100000,
commission = 0,
weight = b$weight,
dates = 1:nrow(b$prices)
)
{
dates.index = dates2index(b$prices, dates)
type = type[1]
if( !silent ) {
cat('Latest weights :\n')
print( last(weight) )
cat('\n')
}
weight[] = ifna(weight, NA)
if(do.lag > 0) {
weight = mlag(weight, do.lag)
}
if(do.CarryLastObservationForwardIfNA) {
weight[] = apply(coredata(weight), 2, ifna.prev)
}
weight[is.na(weight)] = 0
weight1 = mlag(weight, -1)
tstart = weight != weight1 & weight1 != 0
tend = weight != 0 & weight != weight1
trade = ifna(tstart | tend, FALSE)
prices = b$prices
if( sum(trade) > 0 ) {
execution.price = coredata(b$execution.price)
prices1 = coredata(b$prices)
prices1[trade] = iif( is.na(execution.price[trade]), prices1[trade], execution.price[trade] )
prices[] = prices1
}
if( type == 'weight') {
ret = prices / mlag(prices) - 1
ret[] = ifna(ret, NA)
ret[is.na(ret)] = 0
} else {
ret = prices
}
temp = b$weight
temp[] = weight
weight = temp
bt = bt.summary(weight, ret, type, b$prices, capital, commission, dates.index)
bt$dates.index = dates.index
if( trade.summary ) bt$trade.summary = bt.trade.summary(b, bt)
if( !silent ) {
cat('Performance summary :\n')
cat('', spl('CAGR,Best,Worst'), '\n', sep = '\t')
cat('', sapply(cbind(bt$cagr, bt$best, bt$worst), function(x) round(100*x,1)), '\n', sep = '\t')
cat('\n')
}
return(bt)
}
bt.summary <- function
(
weight,
ret,
type = c('weight', 'share'),
close.prices,
capital = 100000,
commission = 0,
dates.index = 1:nrow(weight)
)
{
if( !is.list(commission) ) {
if( type == 'weight')
commission = list(cps = 0.0, fixed = 0.0, percentage = commission)
else
commission = list(cps = commission, fixed = 0.0, percentage = 0.0)
}
if(len(dates.index) != nrow(weight)) {
weight = weight[dates.index,,drop=F]
ret = ret[dates.index,,drop=F]
close.prices = close.prices[dates.index,,drop=F]
}
type = type[1]
n = nrow(ret)
bt = list()
bt$weight = weight
bt$type = type
com.weight = mlag(weight,-1)
if( type == 'weight') {
temp = ret[,1]
temp[] = rowSums(ret * weight) -
rowSums(abs(com.weight - mlag(com.weight)) * commission$percentage, na.rm=T)
- rowSums(sign(abs(com.weight - mlag(com.weight))) * commission$fixed, na.rm=T)
bt$ret = temp
} else {
bt$share = weight
bt$capital = capital
prices = ret
prices[] = bt.apply.matrix(coredata(prices), ifna.prev)
close.prices[] = bt.apply.matrix(coredata(close.prices), ifna.prev)
cash = capital - rowSums(bt$share * mlag(close.prices), na.rm=T)
share.nextday = mlag(bt$share, -1)
tstart = bt$share != share.nextday & share.nextday != 0
tend = bt$share != 0 & bt$share != share.nextday
trade = ifna(tstart | tend, FALSE)
tstart = trade
index = mlag(apply(tstart, 1, any))
index = ifna(index, FALSE)
index[1] = T
totalcash = NA * cash
totalcash[index] = cash[index]
totalcash = ifna.prev(totalcash)
totalcash = ifna(totalcash,0)
portfolio.ret = (totalcash  + rowSums(bt$share * prices, na.rm=T)
- rowSums(abs(com.weight - mlag(com.weight)) * commission$cps, na.rm=T)
- rowSums(sign(abs(com.weight - mlag(com.weight))) * commission$fixed, na.rm=T)
- rowSums(prices * abs(com.weight - mlag(com.weight)) * commission$percentage, na.rm=T)
) / (totalcash + rowSums(bt$share * mlag(prices), na.rm=T) ) - 1
bt$weight = bt$share * mlag(prices) / (totalcash + rowSums(bt$share * mlag(prices), na.rm=T) )
bt$weight[is.na(bt$weight)] = 0
temp = ret[,1]
temp[] = ifna(portfolio.ret,0)
temp[1] = 0
bt$ret = temp
}
bt$best = max(bt$ret)
bt$worst = min(bt$ret)
bankrupt = which(bt$ret <= -1)
if(len(bankrupt) > 0) bt$ret[bankrupt[1]:n] = -1
bt$equity = cumprod(1 + bt$ret)
bt$cagr = compute.cagr(bt$equity)
return(bt)
}
bt.summary.test <- function() {
load.packages('quantmod')
data <- new.env()
getSymbols('EEM', src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
bt.prep(data, align='keep.all', dates='2013:08::2013:08:10')
buy.date = '2013:08:05'
sell.date = '2013:08:06'
coredata(data$prices) <-c(10,20,40,60,20,160,60)
prices = data$prices
data$weight[] = NA
data$weight[buy.date] = 1
data$weight[sell.date] = 0
commission = list(cps = 0.0, fixed = 0.0, percentage = 1/100)
model3 = bt.run(data, commission = commission, silent = T)
model3$ret
data$weight[] = NA
data$weight[buy.date] = 1
data$weight[sell.date] = 0
commission = list(cps = 0.0, fixed = 0.0, percentage = 1/100)
model3 = bt.run.share(data, commission = commission, capital = 100000, silent = T)
model3$ret
}
bt.trim <- function
(
...
)
{
models = variable.number.arguments( ... )
for( i in 1:len(models) ) {
bt = models[[i]]
n = len(bt$equity)
first = which.max(!is.na(bt$equity) & bt$equity != 1)
if (first < n) {
index = first:n
bt$dates.index = bt$dates.index[index]
bt$equity = bt$equity[index]
bt$ret = bt$ret[index]
bt$weight = bt$weight[index,,drop=F]
if (!is.null(bt$share)) bt$share = bt$share[index,,drop=F]
bt$best = max(bt$ret)
bt$worst = min(bt$ret)
bt$cagr = compute.cagr(bt$equity)
}
models[[i]] = bt
}
return (models)
}
bt.trim.test <- function() {
load.packages('quantmod')
data <- new.env()
getSymbols(spl('SPY,GLD'), src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all')
models = list()
data$weight[] = NA
data$weight$SPY[] = 1
models$SPY = bt.run.share(data, clean.signal=F)
data$weight[] = NA
data$weight$GLD[] = 1
models$GLD = bt.run.share(data, clean.signal=F)
strategy.performance.snapshoot(bt.trim(models), T)
}
bt.run.weight.fast <- function
(
b,
do.lag = 1,
do.CarryLastObservationForwardIfNA = TRUE
)
{
weight = ifna(coredata(b$weight), NA)
if(do.lag > 0) weight = mlag(weight, do.lag)
if(do.CarryLastObservationForwardIfNA) weight[] = apply(coredata(weight), 2, ifna.prev)
weight[is.na(weight)] = 0
prices = coredata(b$prices)
ret = prices / mlag(prices) - 1
ret[] = ifna(ret, 0)
ret = rowSums(ret * weight)
list(weight = weight, ret = ret, equity = cumprod(1 + ret))
}
compute.turnover <- function
(
bt,
b
)
{
year.ends =  unique(c(endpoints(bt$weight, 'years'), nrow(bt$weight)))
year.ends = year.ends[year.ends>0]
nr = len(year.ends)
period.index = c(1, year.ends)
if( bt$type == 'weight') {
portfolio.value = rowSums(abs(bt$weight), na.rm=T)
portfolio.turnover = rowSums( abs(bt$weight - mlag(bt$weight)), na.rm=T)
portfolio.turnover[ rowSums( !is.na(bt$weight) & !is.na(mlag(bt$weight)) ) == 0 ] = NA
} else {
prices = mlag(b$prices[bt$dates.index,,drop=F])
cash = bt$capital - rowSums(bt$share * prices, na.rm=T)
share.nextday = mlag(bt$share, -1)
tstart = bt$share != share.nextday & share.nextday != 0
index = mlag(apply(tstart, 1, any))
index = ifna(index, FALSE)
totalcash = NA * cash
totalcash[index] = cash[index]
totalcash = ifna.prev(totalcash)
portfolio.value = totalcash + rowSums(bt$share * prices, na.rm=T)
portfolio.turnover = rowSums( prices * abs(bt$share - mlag(bt$share)), na.rm=T)
portfolio.turnover[ rowSums( !is.na(bt$share) & !is.na(mlag(bt$share)) & !is.na(prices) ) == 0 ] = NA
}
portfolio.turnover[1:2] = 0
temp = NA * period.index
for(iyear in 2:len(period.index)) {
temp[iyear] = sum( portfolio.turnover[ period.index[(iyear-1)] : period.index[iyear] ], na.rm=T) /
mean( portfolio.value[ period.index[(iyear-1)] : period.index[iyear] ], na.rm=T)
}
return( ifna(mean(temp, na.rm=T),0) )
}
compute.max.deviation <- function
(
bt,
target.allocation
)
{
weight = bt$weight[-1,]
max(abs(weight - repmat(target.allocation, nrow(weight), 1)))
}
bt.trade.summary <- function
(
b,
bt
)
{
if( bt$type == 'weight') weight = bt$weight else weight = bt$share
out = NULL
weight1 = mlag(weight, -1)
tstart = weight != weight1 & weight1 != 0
tend = weight != 0 & weight != weight1
tstart[1, weight[1,] != 0] = T
trade = ifna(tstart | tend, FALSE)
prices = b$prices[bt$dates.index,,drop=F]
if( sum(trade) > 0 ) {
execution.price = coredata(b$execution.price[bt$dates.index,,drop=F])
prices1 = coredata(b$prices[bt$dates.index,,drop=F])
prices1[trade] = iif( is.na(execution.price[trade]), prices1[trade], execution.price[trade] )
prices1[is.na(prices1)] = ifna(mlag(prices1), NA)[is.na(prices1)]
prices[] = prices1
weight = bt$weight
symbolnames = b$symbolnames
nsymbols = len(symbolnames)
trades = c()
for( i in 1:nsymbols ) {
tstarti = which(tstart[,i])
tendi = which(tend[,i])
if( len(tstarti) > 0 ) {
if( len(tendi) < len(tstarti) ) tendi = c(tendi, nrow(weight))
trades = rbind(trades,
cbind(i, weight[(tstarti+1), i],
tstarti, tendi,
as.vector(prices[tstarti, i]), as.vector(prices[tendi,i])
)
)
}
}
colnames(trades) = spl('symbol,weight,entry.date,exit.date,entry.price,exit.price')
out = list()
out$stats = cbind(
bt.trade.summary.helper(trades),
bt.trade.summary.helper(trades[trades[, 'weight'] >= 0, ]),
bt.trade.summary.helper(trades[trades[, 'weight'] <0, ])
)
colnames(out$stats) = spl('All,Long,Short')
temp.x = index.xts(weight)
trades = data.frame(coredata(trades))
trades$symbol = symbolnames[trades$symbol]
trades$entry.date = temp.x[trades$entry.date]
trades$exit.date = temp.x[trades$exit.date]
trades$return = round(100*(trades$weight) * (trades$exit.price/trades$entry.price - 1),2)
trades$entry.price = round(trades$entry.price, 2)
trades$exit.price = round(trades$exit.price, 2)
trades$weight = round(100*(trades$weight),1)
out$trades = as.matrix(trades)
}
return(out)
}
bt.trade.summary.helper <- function(trades)
{
if(nrow(trades) <= 0) return(NA)
out = list()
tpnl = trades[, 'weight'] * (trades[, 'exit.price'] / trades[,'entry.price'] - 1)
tlen = trades[, 'exit.date'] - trades[, 'entry.date']
out$ntrades = nrow(trades)
out$avg.pnl = mean(tpnl)
out$len = mean(tlen)
out$win.prob = len(which( tpnl > 0 )) / out$ntrades
out$win.avg.pnl = mean( tpnl[ tpnl > 0 ])
out$win.len = mean( tlen[ tpnl > 0 ])
out$loss.prob = 1 - out$win.prob
out$loss.avg.pnl = mean( tpnl[ tpnl < 0 ])
out$loss.len = mean( tlen[ tpnl < 0 ])
out$expectancy = (out$win.prob * out$win.avg.pnl + out$loss.prob * out$loss.avg.pnl)/100
out$profitfactor = -(out$win.prob * out$win.avg.pnl) / (out$loss.prob * out$loss.avg.pnl)
return(as.matrix(unlist(out)))
}
bt.apply <- function
(
b,
xfun=Cl,
...
)
{
out = b$weight
out[] = NA
symbolnames = b$symbolnames
nsymbols = length(symbolnames)
for( i in 1:nsymbols ) {
msg = try( match.fun(xfun)( coredata(b[[ symbolnames[i] ]]),... ) , silent=TRUE)
if (class(msg)[1] != 'try-error') {
out[,i] = msg
} else {
cat(i, msg, '\n')
}
}
return(out)
}
bt.apply.matrix <- function
(
b,
xfun=Cl,
...
)
{
out = b
out[] = NA
nsymbols = ncol(b)
for( i in 1:nsymbols ) {
msg = try( match.fun(xfun)( coredata(b[,i]),... ) , silent=TRUE);
if (class(msg)[1] != 'try-error') {
out[,i] = msg
} else {
cat(i, msg, '\n')
}
}
return(out)
}
exrem <- function(x) {
temp = c(0, ifna(ifna.prev(x),0))
itemp = which(temp != mlag(temp))
x[] = NA
x[(itemp-1)] = temp[itemp]
return(x)
}
exrem.test <- function() {
exrem(c(NA,1,1,0,1,1,NA,0))
}
bt.exrem <- function(weight)
{
bt.apply.matrix(weight, exrem)
}
bt.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
bt.prep(data, align='keep.all', dates='1970::2011')
prices = data$prices
data$weight[] = 1
buy.hold = bt.run(data)
sma = bt.apply(data, function(x) { SMA(Cl(x), 200) } )
data$weight[] = NA
data$weight[] = iif(prices >= sma, 1, 0)
sma.cross = bt.run(data, trade.summary=T)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1( sma.cross, buy.hold)
dev.off()
png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2( sma.cross, buy.hold)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part3( sma.cross, buy.hold)
dev.off()
pdf(file = 'report.pdf', width=8.5, height=11)
plotbt.custom.report(sma.cross, buy.hold, trade.summary=T)
dev.off()
data$weight[] = NA
data$weight$SPY = 1
temp = bt.run(data)
data$weight[] = NA
data$weight$SPY = 2
temp = bt.run(data)
data$weight[] = NA
data$weight$SPY = 1
capital = 100000
data$weight[] = (capital / prices) * data$weight
temp = bt.run(data, type='share', capital=capital)
data$weight[] = NA
data$weight$SPY = 2
capital = 100000
data$weight[] = (capital / prices) * data$weight
temp = bt.run(data, type='share', capital=capital)
}
compute.cagr <- function(equity)
{
as.double( last(equity,1)^(1/compute.nyears(equity)) - 1 )
}
compute.nyears <- function(x)
{
as.double(diff(as.Date(range(index.xts(x)))))/365
}
compute.raw.annual.factor = function(x) {
round( nrow(x) / compute.nyears(x) )
}
compute.annual.factor = function(x) {
possible.values = c(252,52,26,13,12,6,4,3,2,1)
index = which.min(abs( compute.raw.annual.factor(x) - possible.values ))
round( possible.values[index] )
}
compute.sharpe <- function(x)
{
temp = compute.annual.factor(x)
x = as.vector(coredata(x))
return(sqrt(temp) * mean(x)/sd(x) )
}
compute.calmar <- function(x)
{
compute.cagr(x) / compute.max.drawdown(x)
}
compute.R2 <- function(equity)
{
x = as.double(index.xts(equity))
y = equity
return( cor(y,x)^2 )
}
compute.DVR <- function(bt)
{
return( compute.sharpe(bt$ret) * compute.R2(bt$equity) )
}
compute.risk <- function(x)
{
temp = compute.annual.factor(x)
x = as.vector(coredata(x))
return( sqrt(temp)*sd(x) )
}
compute.drawdown <- function(x)
{
return(x / cummax(c(1,x))[-1] - 1)
}
compute.max.drawdown <- function(x)
{
as.double( min(compute.drawdown(x)) )
}
compute.avg.drawdown <- function(x)
{
drawdown = c( 0, compute.drawdown(coredata(x)), 0 )
dstart = which( drawdown == 0 & mlag(drawdown, -1) != 0 )
dend = which(drawdown == 0 & mlag(drawdown, 1) != 0 )
drawdowns = apply( cbind(dstart, dend), 1, function(x) min(drawdown[ x[1]:x[2] ], na.rm=T) )
mean(drawdowns)
}
compute.cdar <- function(x, probs=0.05)
{
drawdown = c( 0, compute.drawdown(coredata(x)), 0 )
dstart = which( drawdown == 0 & mlag(drawdown, -1) != 0 )
dend = which(drawdown == 0 & mlag(drawdown, 1) != 0 )
drawdowns = apply( cbind(dstart, dend), 1, function(x) min(drawdown[ x[1]:x[2] ], na.rm=T) )
if(len(drawdowns)>2)
mean( drawdowns[ drawdowns < quantile(drawdowns, probs=probs) ] )
else
min(drawdowns)
}
compute.exposure <- function(weight)
{
sum( apply(weight, 1, function(x) sum(x != 0) ) != 0 ) / nrow(weight)
}
compute.var <- function(x, probs=0.05)
{
quantile( coredata(x), probs=probs)
}
compute.cvar <- function(x, probs=0.05)
{
x = coredata(x)
mean( x[ x < quantile(x, probs=probs) ] )
}
compute.stats <- function(data, fns, do.na.omit = T)
{
out = matrix(double(), len(fns), len(data))
colnames(out) = names(data)
rownames(out) = names(fns)
if( do.na.omit )
for(c in 1:len(data)) {
for(r in 1:len(fns)) {
out[r,c] = match.fun(fns[[r]])( fast.na.omit(data[[c]]) )
}
}
else
for(c in 1:len(data)) {
for(r in 1:len(fns)) {
out[r,c] = match.fun(fns[[r]])( data[[c]] )
}
}
return(out)
}
bt.simple <- function(data, signal)
{
signal = Lag(signal, 1)
signal = na.locf(signal, na.rm = FALSE)
signal[is.na(signal)] = 0
ret = ROC(Cl(data), type='discrete')
ret[1] = 0
n = nrow(ret)
bt <- list()
bt$ret = ret * signal
bt$best = max(bt$ret)
bt$worst = min(bt$ret)
bt$equity = cumprod(1 + bt$ret)
bt$cagr = bt$equity[n] ^ (1/nyears(data)) - 1
cat('', spl('CAGR,Best,Worst'), '\n', sep = '\t')
cat('', sapply(cbind(bt$cagr, bt$best, bt$worst), function(x) round(100*x,1)), '\n', sep = '\t')
return(bt)
}
bt.simple.test <- function()
{
load.packages('quantmod')
data = getSymbols('SPY', src = 'yahoo', from = '1980-01-01', auto.assign = F)
signal = rep(1, nrow(data))
buy.hold = bt.simple(data, signal)
sma = SMA(Cl(data),200)
signal = ifelse(Cl(data) > sma, 1, 0)
sma.cross = bt.simple(data, signal)
dates = '2000::2009'
buy.hold.equity <- buy.hold$equity[dates] / as.double(buy.hold$equity[dates][1])
sma.cross.equity <- sma.cross$equity[dates] / as.double(sma.cross$equity[dates][1])
chartSeries(buy.hold.equity, TA=c(addTA(sma.cross.equity, on=1, col='red')),
theme ='white', yrange = range(buy.hold.equity, sma.cross.equity) )
}
bt.apply.min.weight <- function
(
weight,
long.min.weight = 0.1,
short.min.weight = long.min.weight
)
{
if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
pos = apply(weight, 1, function(row) sum(row[row > 0]))
neg = rowSums(weight) - pos
pos.mat = iif(weight >= long.min.weight, weight, 0)
neg.mat = iif(weight <= -short.min.weight, weight, 0)
pos.mat = pos.mat * ifna(pos / rowSums(pos.mat), 1)
neg.mat = neg.mat * ifna(neg / rowSums(neg.mat), 1)
return(pos.mat + neg.mat)
}
test.bt.apply.min.weight <- function()
{
data = c(0.1, 0.6, 0.2, 0.1, 0, -0.1, -0.6, -0.2, -0.1, 0)
mm = matrix(data=data, nrow=2, byrow=TRUE)
print(bt.apply.min.weight(mm, 0.1))
print(bt.apply.min.weight(mm, 0.2))
data = c(0.1, 0.6, 0.2, 0.1, 0, -0.1, -0.6, -0.2, -0.1, 0)
mm = matrix(data=data, nrow=1, byrow=TRUE)
print(bt.apply.min.weight(mm, 0.1))
print(bt.apply.min.weight(mm, 0.2))
data = c(0.1, 0.6, 0.2, 0.1, 0, -0.2, -0.5, -0.3, -0.1, 0)
mm = matrix(data=data, nrow=1, byrow=TRUE)
print(bt.apply.min.weight(mm, 0.1))
print(bt.apply.min.weight(mm, 0.2))
}
bt.apply.round.weight <- function
(
weight,
long.round.weight = 5/100,
short.round.weight = long.round.weight
)
{
if(is.null(dim(weight))) dim(weight) = c(1, len(weight))
pos = apply(weight, 1, function(row) sum(row[row > 0]))
neg = rowSums(weight) - pos
pos.mat = iif(weight >= 0, round(weight / long.round.weight) * long.round.weight, 0)
neg.mat = iif(weight <= 0, round(weight / short.round.weight) * short.round.weight, 0)
pos.mat = pos.mat * ifna(pos / rowSums(pos.mat), 1)
neg.mat = neg.mat * ifna(neg / rowSums(neg.mat), 1)
return(pos.mat + neg.mat)
}
bt.start.dates <- function
(
b
)
{
temp = lapply(b, function(x) index(x[1]) )
temp$dates = NULL
temp$prices = NULL
temp$weight = NULL
temp$execution.price = NULL
temp$symbolnames = NULL
temp = temp[order( sapply(temp, function(x) x) )]
t(t( sapply(temp, function(x) as.character(x)) ))
}
bt.exrem.time.exit <- function(signal, nlen, create.weight = T) {
signal[is.na(signal)] = FALSE
signal.index = which(signal)
nsignal.index = len(signal.index)
nperiods = len(signal)
signal.index.exit = iif(signal.index + nlen - 1 > nperiods, nperiods, signal.index + nlen)
if(!create.weight) {
for(i in 1:nsignal.index) {
if( signal[ signal.index[i] ] ) {
signal[ (signal.index[i]+1) : signal.index.exit[i] ] = FALSE
}
}
return(signal)
} else {
temp = signal * NA
for(i in 1:nsignal.index) {
if( signal[ signal.index[i] ] ) {
signal[ (signal.index[i]+1) : signal.index.exit[i] ] = FALSE
temp[ signal.index.exit[i] ] = 0
}
}
temp[signal] = 1
return(temp)
}
}
bt.min.holding.period <- function(x, nlen) {
x = coredata(x)
enter = x != 0
enter[is.na(enter)] = FALSE
enter.index = which(enter)
for(t in enter.index)
if( enter[ t ] ) {
index = t + nlen
enter[ t : index ] = FALSE
x[ t : index ] = x[t]
}
return(x)
}
bt.time.stop <- function(weight, nlen)
{
bt.apply.matrix(weight, bt.ts.time.stop, nlen)
}
bt.price.stop <- function(b, price, pstop)
{
out = b
out[] = NA
nsymbols = ncol(b)
if(is.null(dim(pstop))) pstop = rep.row(pstop, nrow(b))
for( i in 1:nsymbols )
out[,i] = bt.ts.price.stop(coredata(b[,i]), coredata(price[,i]), coredata(pstop[,i]))
return(out)
}
bt.time.price.stop <- function(b, nlen, price, pstop)
{
out = b
out[] = NA
nsymbols = ncol(b)
if(is.null(dim(pstop))) pstop = rep.row(pstop, nrow(b))
for( i in 1:nsymbols )
out[,i] = bt.ts.time.price.stop(coredata(b[,i]), nlen, coredata(price[,i]), coredata(pstop[,i]))
return(out)
}
bt.ts.trade.index <- function(x)
{
enter = x != 0
enter[is.na(enter)] = FALSE
enter[length(x)] = FALSE
enter.index = which(enter)
temp = ifna.prev(x)
temp0 = mlag(temp)
exit = temp0 != 0 & temp != temp0
exit[ !exit ] = NA
exit = ifna.prevx.rev(exit)
list(enter = enter, enter.index = enter.index, exit = exit)
}
bt.ts.enter.state <- function(x)
{
enter = x != 0
enter[is.na(enter)] = FALSE
enter
}
bt.ts.time.stop <- function(x, nlen)
{
temp = bt.ts.trade.index(x)
enter = temp$enter
enter.index = temp$enter.index
exit = temp$exit
for(t in enter.index)
if( enter[ t ] )
if( exit[ t ] < t + nlen )
enter[ t : exit[ t ] ] = FALSE
else {
enter[ t : (t + nlen) ] = FALSE
x[ (t + nlen) ] = 0
}
return(x)
}
time.stop.test <- function() {
bt.ts.time.stop(c(1,1,1,0,1,1,NA,1,0),2)
bt.ts.time.stop(c(1,0,1,1,1,1,1,1,1),3)
}
bt.ts.price.stop <- function(x, price, pstop)
{
price = coredata(price)
pstop = coredata(pstop)
if(length(pstop) == 1) pstop = fast.rep(pstop, len(x))
dummy = 1:length(x)
temp = bt.ts.trade.index(x)
enter = temp$enter
enter.index = temp$enter.index
exit = temp$exit
for(t in enter.index)
if( enter[ t ] ) {
if( x[ t ] > 0 )
temp = price[ t : exit[ t ] ] < price[ t ] - pstop[ t ]
else
temp = price[ t : exit[ t ] ] > price[ t ] + pstop[ t ]
if( any(temp, na.rm=T) ) {
iexit = t - 1 + dummy[temp][1]
enter[ t : iexit ] = FALSE
x[ iexit ] = 0
} else
enter[ t : exit[ t ] ] = FALSE
}
return(x)
}
price.stop.test <- function() {
bt.ts.price.stop(c(1,1,1,1,1,1,NA,1,0),
c(1,1,0.9,0.7,1,1,1,1,0),
0.2
)
bt.ts.price.stop(-c(1,1,1,1,1,1,NA,1,0),
c(1,1,0.9,1.7,1,1,1,1,0),
0.2
)
}
bt.ts.time.price.stop <- function(x, nlen, price, pstop)
{
price = coredata(price)
pstop = coredata(pstop)
if(length(pstop) == 1) pstop = fast.rep(pstop, len(x))
dummy = 1:length(x)
temp = bt.ts.trade.index(x)
enter = temp$enter
enter.index = temp$enter.index
exit = temp$exit
for(t in enter.index)
if( enter[ t ] ) {
if( x[ t ] > 0 )
temp = price[ t : exit[ t ] ] < price[ t ] - pstop[ t ]
else
temp = price[ t : exit[ t ] ] > price[ t ] + pstop[ t ]
if( any(temp, na.rm=T) ) {
iexit = t - 1 + dummy[temp][1]
if( iexit < t + nlen ) {
enter[ t : iexit ] = FALSE
x[ iexit ] = 0
} else {
enter[ t : (t + nlen) ] = FALSE
x[ (t + nlen) ] = 0
}
} else
if( exit[ t ] < t + nlen )
enter[ t : exit[ t ] ] = FALSE
else {
enter[ t : (t + nlen) ] = FALSE
x[ (t + nlen) ] = 0
}
}
return(x)
}
time.price.stop.test <- function() {
bt.ts.time.price.stop(c(1,1,1,1,1,1,NA,1,0),
4,
c(1,1,0.9,0.7,1,1,1,1,0),
0.2
)
bt.ts.time.price.stop(-c(1,1,1,1,1,1,NA,1,0),
4,
c(1,1,0.9,1.7,1,1,1,1,0),
0.2
)
}
custom.stop.fn <- function(x, price, stop.fn, ...)
{
price = coredata(price)
if(is.character(stop.fn)) stop.fn = match.fun(stop.fn)
dummy = 1:length(x)
temp = bt.ts.trade.index(x)
enter = temp$enter
enter.index = temp$enter.index
exit = temp$exit
for(t in enter.index)
if( enter[ t ] ) {
temp = stop.fn(x[ t ], price, t, exit[ (t + 1) ], ...)
if( any(temp, na.rm=T) ) {
iexit = t - 1 + dummy[temp][1]
enter[ t : iexit ] = FALSE
x[ iexit ] = 0
} else
enter[ t : exit[ t ] ] = FALSE
}
return(x)
}
custom.stop.fn.list <- function(x, price, stop.fn, ...)
{
price = coredata(price)
if(is.character(stop.fn)) stop.fn = match.fun(stop.fn)
dummy = 1:length(x)
temp = bt.ts.trade.index(x)
enter = temp$enter
enter.index = temp$enter.index
exit = temp$exit
for(t in enter.index)
if( enter[ t ] ) {
out = stop.fn(x[ t ], price, t, exit[ (t + 1) ], ...)
temp = out$state
if( any(temp, na.rm=T) ) {
iexit = t - 1 + dummy[temp][1]
if(out$clean.signal) enter[ t : iexit ] = FALSE
x[ iexit ] = out$value
} else
enter[ t : exit[ t ] ] = FALSE
}
return(x)
}
custom.stop.fn.full <- function(x, price, stop.fn, ...)
{
price = coredata(price)
if(is.character(stop.fn)) stop.fn = match.fun(stop.fn)
temp = bt.ts.trade.index(x)
enter = temp$enter
enter.index = temp$enter.index
exit = temp$exit
for(t in enter.index)
if( enter[ t ] ) {
out = stop.fn(x, price, t, exit[ (t + 1) ], ...)
x = out$x
if(out$clean.signal) enter[ t : out$tlast ] = FALSE
}
return(x)
}
custom.trailing.stop.test <- function(weight, price, tstart, tend, sma, nstop) {
index = tstart : tend
if(weight > 0) {
temp = price[ index ] < cummax(0.9 * sma[ index ])
temp = temp | price[ index ] > cummax(1.1 * sma[ index ])
} else {
temp = price[ index ] > cummax(1.1 * sma[ index ])
}
if( tend - tstart > nstop ) temp[ (nstop + 1) ] = T
return( temp )
}
custom.stop.fn.test <- function() {
signal = c(1,1,1,1,1,1,NA,1,0)
price = c(1,1,0.9,0.7,1,1,1,1,0)
custom.stop.fn(signal, price,
custom.trailing.stop.test,
sma = ifna(SMA(price, 2), price),
nstop = 20
)
signal = -c(1,1,1,1,1,1,NA,1,0)
price = c(1,1,0.9,1.7,1,1,1,1,0)
custom.stop.fn(signal, price,
custom.trailing.stop.test,
sma = ifna(SMA(price, 2), price),
nstop = 4
)
}
bt.stop.strategy.plot <- function(
data,
model,
dates = '::',
main = NULL,
layout = NULL,
extra.plot.fn = NULL,
...
) {
weight = model$weight[dates]
col = iif(weight > 0, 'green', iif(weight < 0, 'gray', 'white'))
plota.control$col.x.highlight = col.add.alpha(col, 100)
highlight = T
if(is.null(layout)) layout(1)
plota(data$prices[dates], type='l', x.highlight = highlight, ...)
if(!is.null(extra.plot.fn)) match.fun(extra.plot.fn)()
plota.legend('Long,Short,Not Invested','green,gray,white')
if(!is.null(main))
legend('top', legend=main, bty='n')
}
bt.stop.kaeppels.40w.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
tickers = spl('^DJI')
tickers = spl('DIA')
tickers = spl('^GSPC')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1967-01-01', env = data, auto.assign = T)
bt.prep(data, align='keep.all', dates='1967:04:21::')
prices = data$prices
nperiods = nrow(prices)
models = list()
data$weight[] = NA
data$weight[] = 1
models$buy.hold = bt.run.share(data, clean.signal=T)
start.cycle = as.Date("1967-04-21")
diff = data$dates - start.cycle
diff.cyc = diff / 280
diff.int = as.integer(diff.cyc)
signal=iif((diff.cyc-diff.int) < 0.5, 1, 0)
signal = exrem(signal)
data$weight[] = NA
data$weight[] = signal
models$cycle = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
fixed.stop <- function(weight, price, tstart, tend, pstop) {
index = tstart : tend
if(weight > 0)
price[ index ] < (1 - pstop) * price[ tstart ]
else
price[ index ] > (1 + pstop) * price[ tstart ]
}
trailing.stop <- function(weight, price, tstart, tend, pstop) {
index = tstart : tend
if(weight > 0) {
temp = price[ index ] < (1 - pstop) * cummax(price[ index ])
} else {
temp = price[ index ] > (1 + pstop) * cummin(price[ index ])
}
return( temp )
}
pstop = 8.5 / 100
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(signal), coredata(prices), fixed.stop, pstop = pstop)
models$cycle.fixed.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(signal), coredata(prices), trailing.stop, pstop = pstop)
models$cycle.trailing.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
plotbt.custom.report.part1(models)
plotbt.custom.report.part2(models$cycle)
plotbt.strategy.sidebyside(models)
plotbt.custom.report.part3(models$cycle, trade.summary = TRUE)
strategy.performance.snapshoot(models, T)
plotbt.custom.report.part2(models$cycle.trailing.stop)
plotbt.custom.report.part3(models$cycle.trailing.stop, trade.summary = TRUE)
dates = '2009:04::'
layout(1:3)
bt.stop.strategy.plot(data, models$cycle, dates = dates, layout=T, main = '40 week cycle', plotX = F)
bt.stop.strategy.plot(data, models$cycle.fixed.stop, dates = dates, layout=T, main = '40 week cycle fixed stop', plotX = F)
bt.stop.strategy.plot(data, models$cycle.trailing.stop, dates = dates, layout=T, main = '40 week cycle trailing stop')
}
bt.stop.ma.cross.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1999::')
prices = data$prices
models = list()
data$weight[] = NA
data$weight[] = 1
models$buy.hold = bt.run.share(data, clean.signal=T)
sma.fast = SMA(prices, 20)
sma.slow = SMA(prices, 50)
buy.signal = iif(cross.up(sma.fast, sma.slow), 1, NA)
data$weight[] = NA
data$weight[] = iif(cross.up(sma.fast, sma.slow), 1, iif(cross.dn(sma.fast, sma.slow), 0, NA))
models$ma.cross = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
fixed.stop <- function(weight, price, tstart, tend, pstop) {
index = tstart : tend
if(weight > 0)
price[ index ] < (1 - pstop) * price[ tstart ]
else
price[ index ] > (1 + pstop) * price[ tstart ]
}
trailing.stop <- function(weight, price, tstart, tend, pstop) {
index = tstart : tend
if(weight > 0) {
temp = price[ index ] < (1 - pstop) * cummax(price[ index ])
} else {
temp = price[ index ] > (1 + pstop) * cummin(price[ index ])
}
return( temp )
}
trailing.stop.profit.target <- function(weight, price, tstart, tend, pstop, pprofit) {
index = tstart : tend
if(weight > 0) {
temp = price[ index ] < (1 - pstop) * cummax(price[ index ])
temp = temp | price[ index ] > (1 + pprofit) * price[ tstart ]
} else {
temp = price[ index ] > (1 + pstop) * cummin(price[ index ])
temp = temp | price[ index ] < (1 - pprofit) * price[ tstart ]
}
return( temp )
}
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), fixed.stop,
pstop = 1/100)
models$ma.cross.fixed.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), trailing.stop,
pstop = 1/100)
models$ma.cross.trailing.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(buy.signal), coredata(prices), trailing.stop.profit.target,
pstop = 1/100, pprofit = 1.5/100)
models$ma.cross.trailing.stop.profit.target = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
jpeg(filename = 'plot1.jpg', width = 500, height = 500, units = 'px', pointsize = 12)
strategy.performance.snapshoot(models, T)
dev.off()
dates = '2010::2010'
extra.plot.fn <- function() {
plota.lines(sma.fast, col='red')
plota.lines(sma.slow, col='blue')
}
jpeg(filename = 'plot2.jpg', width = 500, height = 500, units = 'px', pointsize = 12)
layout(1:4)
bt.stop.strategy.plot(data, models$ma.cross, dates = dates, layout=T, main = 'MA Cross', extra.plot.fn = extra.plot.fn, plotX = F)
bt.stop.strategy.plot(data, models$ma.cross.fixed.stop, dates = dates, layout=T, main = 'Fixed Stop', plotX = F)
bt.stop.strategy.plot(data, models$ma.cross.trailing.stop, dates = dates, layout=T, main = 'Trailing Stop', plotX = F)
bt.stop.strategy.plot(data, models$ma.cross.trailing.stop.profit.target, dates = dates, layout=T, main = 'Trailing Stop and Profit Target')
dev.off()
}
bt.stop.ma.cross.pullback.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
tickers = spl('^GSPC')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
bt.prep(data, align='keep.all', dates='1999::')
prices = data$prices
models = list()
data$weight[] = NA
data$weight[] = 1
models$buy.hold = bt.run.share(data, clean.signal=T)
sma.fast = SMA(prices, 50)
sma.slow = SMA(prices, 200)
data$weight[] = NA
data$weight[] = iif(cross.up(prices, sma.slow), 1, iif(cross.dn(prices, sma.slow), 0, NA))
models$ma.crossover = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
data$weight[] = NA
data$weight[] = iif(cross.up(prices, sma.slow), 1, iif(cross.dn(prices, 0.95 * sma.slow), 0, NA))
models$ma.crossover.pullback = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
fixed.stop <- function(weight, price, tstart, tend, pstop) {
index = tstart : tend
if(weight > 0)
price[ index ] < (1 - pstop) * price[ tstart ]
else
price[ index ] > (1 + pstop) * price[ tstart ]
}
trailing.stop <- function(weight, price, tstart, tend, pstop) {
index = tstart : tend
if(weight > 0) {
temp = price[ index ] < (1 - pstop) * cummax(price[ index ])
} else {
temp = price[ index ] > (1 + pstop) * cummin(price[ index ])
}
return( temp )
}
signal = iif(cross.up(prices, sma.slow), 1, iif(cross.dn(prices, sma.slow), 0, NA))
signal = iif(cross.up(prices, sma.slow), 1, NA)
signal = iif(cross.up(prices, sma.slow), 1, iif(cross.dn(prices, 0.95 * sma.slow), 0, NA))
pstop = 8.5 / 100
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(signal), coredata(prices), fixed.stop, pstop = pstop)
models$cycle.fixed.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
data$weight[] = NA
data$weight[] = custom.stop.fn(coredata(signal), coredata(prices), trailing.stop, pstop = pstop)
models$cycle.trailing.stop = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
strategy.performance.snapshoot(models, T)
dates = '::'
extra.plot.fn <- function() {
plota.lines(sma.slow, col='blue')
plota.lines(0.95 * sma.slow, col='red')
}
layout(1:4)
bt.stop.strategy.plot(data, models$ma.crossover, dates = dates, layout=T, main = 'base', plotX = F)
bt.stop.strategy.plot(data, models$ma.crossover.pullback, dates = dates, layout=T, main = 'base + 5% pull-back', extra.plot.fn = extra.plot.fn, plotX = F)
bt.stop.strategy.plot(data, models$cycle.fixed.stop, dates = dates, layout=T, main = 'fixed stop', plotX = F)
bt.stop.strategy.plot(data, models$cycle.trailing.stop, dates = dates, layout=T, main = 'trailing stop')
}
bt.stop.dual.ma.strategy <- function
(
data,
short.ma.len = 50,
long.ma.len = 200,
n.atr = NA,
atr.len = 39
)
{
atr.trailing.stop <- function(weight, price, tstart, tend, atr) {
index = tstart : tend
if(weight > 0)
price[ index ] < cummax( price[ index ] ) - atr[ index ]
else
price[ index ] > cummin( price[ index ] ) + atr[ index ]
}
prices = data$prices
short.ma = SMA(prices, short.ma.len)
long.ma = SMA(prices, long.ma.len)
signal = iif(cross.up(short.ma, long.ma), 1, iif(cross.dn(short.ma, long.ma), 0, NA))
if( !is.na(n.atr) ) {
atr = bt.apply(data, function(x) ATR(HLC(x), atr.len)[,'atr'])
signal = custom.stop.fn(coredata(signal), coredata(prices),
atr.trailing.stop, atr = coredata(n.atr * atr))
}
data$weight[] = NA
data$weight[] = signal
bt.run.share(data, clean.signal=T)
}
bt.stop.dual.ma.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
bt.prep(data, align='keep.all')
models = list()
data$weight[] = NA
data$weight[] = 1
models$buy.hold = bt.run.share(data, clean.signal=T)
models$ma = bt.stop.dual.ma.strategy(data, 50, 200)
models$ma.stop = bt.stop.dual.ma.strategy(data, 50, 200, 5, 39)
strategy.performance.snapshoot(models, T)
dates = '2010::'
extra.plot.fn <- function() {
short.ma.len = 50
long.ma.len = 200
n.atr = 5
atr.len = 39
short.ma = SMA(data$prices, short.ma.len)
long.ma = SMA(data$prices, long.ma.len)
atr = bt.apply(data, function(x) ATR(HLC(x), atr.len)[,'atr'])
plota.lines(short.ma, col='red')
plota.lines(long.ma, col='blue')
plota.lines(data$prices - n.atr * atr, col='orange')
}
layout(1:2)
bt.stop.strategy.plot(data, models$ma, dates = dates, layout=T, main = 'MA base', extra.plot.fn=extra.plot.fn, plotX = F)
bt.stop.strategy.plot(data, models$ma.stop, dates = dates, layout=T, main = 'base + 5% pull-back', extra.plot.fn=extra.plot.fn)
}
plotbt.custom.report <- function
(
...,
dates = NULL,
main = '',
trade.summary = FALSE,
x.highlight = NULL
)
{
ilayout =
'1,1
1,1
2,2
3,3
4,6
4,6
5,7
5,8'
plota.layout(ilayout)
models = variable.number.arguments( ... )
plotbt(models, dates = dates, main = main, plotX = F, log = 'y', LeftMargin = 3, x.highlight = x.highlight)
mtext('Cumulative Performance', side = 2, line = 1)
plotbt(models[1], plottype = '12M', dates = dates, plotX = F, LeftMargin = 3, x.highlight = x.highlight)
mtext('12 Month Rolling', side = 2, line = 1)
plotbt(models[1], dates = dates, xfun = function(x) { 100 * compute.drawdown(x$equity) }, LeftMargin = 3, x.highlight = x.highlight)
mtext('Drawdown', side = 2, line = 1)
model = models[[1]]
plotbt.transition.map(model$weight, x.highlight = x.highlight)
temp = plotbt.monthly.table(model$equity)
plotbt.holdings.time(model$weight)
if ( !is.null(model$trade.summary) ) {
plot.table( list2matrix(bt.detail.summary(model, model$trade.summary)), keep_all.same.cex = TRUE)
} else {
plot.table( list2matrix(bt.detail.summary(model)), keep_all.same.cex = TRUE)
}
if( len(models) > 1 ) plotbt.strategy.sidebyside(models)
if ( trade.summary & !is.null(model$trade.summary)) {
ntrades = min(20, nrow(model$trade.summary$trades))
temp = last(model$trade.summary$trades, ntrades)
if( ntrades == 1 ) temp = model$trade.summary$trades
print( temp )
print( model$trade.summary$stats )
layout(c(1,rep(2,10)))
make.table(1,1)
a = matrix(names(models)[1],1,1)
cex = plot.table.helper.auto.adjust.cex(a)
draw.cell(a[1],1,1, text.cex=cex,frame.cell=F)
plot.table( temp )
}
}
plotbt.custom.report.part1 <- function
(
...,
dates = NULL,
main = '',
trade.summary = FALSE,
x.highlight = NULL
)
{
layout(1:3)
models = variable.number.arguments( ... )
model = models[[1]]
plotbt(models, dates = dates, main = main, plotX = F, log = 'y', LeftMargin = 3, x.highlight = x.highlight)
mtext('Cumulative Performance', side = 2, line = 1)
plotbt(models[1], plottype = '12M', dates = dates, plotX = F, LeftMargin = 3, x.highlight = x.highlight)
mtext('12 Month Rolling', side = 2, line = 1)
plotbt(models[1], dates = dates, xfun = function(x) { 100 * compute.drawdown(x$equity) }, LeftMargin = 3, x.highlight = x.highlight)
mtext('Drawdown', side = 2, line = 1)
}
plotbt.custom.report.part2 <- function
(
...,
dates = NULL,
main = '',
trade.summary = FALSE,
x.highlight = NULL
)
{
models = variable.number.arguments( ... )
model = models[[1]]
ilayout =
'1,3
2,4
2,5'
plota.layout(ilayout)
plotbt.transition.map(model$weight, x.highlight = x.highlight)
temp = plotbt.monthly.table(model$equity)
plotbt.holdings.time(model$weight)
if ( !is.null(model$trade.summary) ) {
plot.table( list2matrix(bt.detail.summary(model, model$trade.summary)), keep_all.same.cex = TRUE)
} else {
plot.table( list2matrix(bt.detail.summary(model)), keep_all.same.cex = TRUE)
}
if( len(models) > 1 ) plotbt.strategy.sidebyside(models)
}
plotbt.custom.report.part3 <- function
(
...,
dates = NULL,
main = '',
trade.summary = FALSE
)
{
models = variable.number.arguments( ... )
model = models[[1]]
if ( trade.summary & !is.null(model$trade.summary)) {
ntrades = min(20, nrow(model$trade.summary$trades))
temp = last(model$trade.summary$trades, ntrades)
if( ntrades == 1 ) temp = model$trade.summary$trades
print( temp )
print( model$trade.summary$stats )
layout(c(1,rep(2,10)))
make.table(1,1)
a = matrix(names(models)[1],1,1)
cex = plot.table.helper.auto.adjust.cex(a)
draw.cell(a[1],1,1, text.cex=cex,frame.cell=F)
plot.table( temp )
}
}
bt.detail.summary <- function
(
bt,
trade.summary = NULL
)
{
out.all = list()
out = list()
out$Period = join( format( range(index.xts(bt$equity)), '%b%Y'), ' - ')
out$Cagr = compute.cagr(bt$equity)
out$Sharpe = compute.sharpe(bt$ret) / 100
out$DVR = compute.DVR(bt) / 100
out$Volatility = compute.risk(bt$ret)
out$MaxDD = compute.max.drawdown(bt$equity)
out$AvgDD = compute.avg.drawdown(bt$equity)
if( !is.null(trade.summary) ) {
out$Profit.Factor = trade.summary$stats['profitfactor', 'All']
}
out$VaR = compute.var(bt$ret)
out$CVaR = compute.cvar(bt$ret)
out$Exposure = compute.exposure(bt$weight)
out.all$System = lapply(out, function(x) if(is.double(x)) round(100*x,2) else x)
if( !is.null(bt$trade.summary) ) trade.summary = bt$trade.summary
out = list()
if( !is.null(trade.summary) ) {
out$Win.Percent = trade.summary$stats['win.prob', 'All']
out$Avg.Trade = trade.summary$stats['avg.pnl', 'All']
out$Avg.Win = trade.summary$stats['win.avg.pnl', 'All']
out$Avg.Loss = trade.summary$stats['loss.avg.pnl', 'All']
out = lapply(out, function(x) if(is.double(x)) round(100*x,1) else x)
out$Best.Trade = max(as.double(trade.summary$trades[, 'return']))
out$Worst.Trade = min(as.double(trade.summary$trades[, 'return']))
out$WinLoss.Ratio = round( -trade.summary$stats['win.avg.pnl', 'All']/trade.summary$stats['loss.avg.pnl', 'All'] , 2)
out$Avg.Len = round(trade.summary$stats['len', 'All'],2)
out$Num.Trades = trade.summary$stats['ntrades', 'All']
}
out.all$Trade = out
out = list()
out$Win.Percent.Day = sum(bt$ret > 0, na.rm = T) / len(bt$ret)
out$Best.Day = bt$best
out$Worst.Day = bt$worst
month.ends = endpoints(bt$equity, 'months')
mret = ROC(bt$equity[month.ends,], type = 'discrete')
out$Win.Percent.Month = sum(mret > 0, na.rm = T) / len(mret)
out$Best.Month = max(mret, na.rm = T)
out$Worst.Month = min(mret, na.rm = T)
year.ends = endpoints(bt$equity, 'years')
mret = ROC(bt$equity[year.ends,], type = 'discrete')
out$Win.Percent.Year = sum(mret > 0, na.rm = T) / len(mret)
out$Best.Year = max(mret, na.rm = T)
out$Worst.Year = min(mret, na.rm = T)
out.all$Period = lapply(out, function(x) if(is.double(x)) round(100*x,1) else x)
return(out.all)
}
plotbt.strategy.sidebyside <- function
(
... ,
perfromance.metric = spl('System,Trade,Period'),
perfromance.fn = 'bt.detail.summary',
return.table = FALSE,
make.plot = TRUE
)
{
models = variable.number.arguments( ... )
out = list()
for( i in 1:len(models) ) {
out[[ names(models)[i] ]] = match.fun(perfromance.fn)(models[[ i ]])[[ perfromance.metric[1] ]]
}
temp = list2matrix(out, keep.names=F)
if(make.plot) plot.table( temp, smain = perfromance.metric[1] )
if(return.table) return(temp)
}
plotbt <- function
(
...,
dates = NULL,
plottype = spl('line,12M'),
xfun=function(x) { x$equity },
main = NULL,
plotX = T,
log = '',
x.highlight = NULL,
LeftMargin = 0
)
{
models = variable.number.arguments( ... )
plottype = plottype[1]
n = length(models)
temp = list()
for( i in 1:n ) {
msg = try( match.fun(xfun)( models[[i]] ) , silent = TRUE)
if (class(msg)[1] != 'try-error') {
temp[[i]] = msg
}
}
nlag = max( 1, compute.annual.factor(temp[[1]]) )
yrange=c();
for( i in 1:n ) {
itemp = temp[[i]]
if(!is.null(dates)) {
itemp = itemp[dates]
if(itemp[1] != 0) itemp = itemp / as.double(itemp[1])
}
if( plottype == '12M' ) {
itemp = 100 * (itemp / mlag(itemp, nlag ) - 1)
}
temp[[i]] = itemp
yrange = range(yrange, itemp ,na.rm = T)
}
plota(temp[[1]], main = main, plotX = plotX, type = 'l', col = 1,
ylim = yrange,log = log, LeftMargin = LeftMargin, x.highlight = x.highlight)
if( n > 1 ) {
for( i in 2:n ) plota.lines(temp[[i]], col = i)
}
if( plottype == '12M' ) legend('topright', legend = '12 Month Rolling', bty = 'n')
plota.legend(names(models), paste('', 1:n, sep=''), temp)
}
plotbt.transition.map <- function
(
weight,
name = '',
col = rainbow(ncol(weight), start=0, end=.9),
x.highlight = NULL
)
{
par(mar=c(2, 4, 1, 1), cex = 0.8, cex.main=0.8,cex.sub=0.8,cex.axis=0.8,cex.lab=0.8)
weight[is.na(weight)] = 0
plota.stacked(index.xts(weight), weight, col = col, type='s', main = iif(nchar(name) > 0, paste('Transition Map for', name), ''), x.highlight = x.highlight)
}
plotbt.holdings <- function
(
weight,
smain = format(index.xts(last(weight)), '%d-%b-%Y')
)
{
par(mar=c(2, 2, 2, 2), cex = 0.8, cex.main=0.8,cex.sub=0.8,cex.axis=0.8,cex.lab=0.8)
icols=rainbow(ncol(weight), start=0, end=.9)
temp = 100 * as.vector(last(weight))
atemp = abs(temp)
if(sum(atemp)>0) {
pie(atemp, labels = paste(round(temp,0), '% ', colnames(weight), sep=''),
col = icols, cex =0.8,
main = paste('Allocation for ', smain, sep='')
)
}
}
plotbt.holdings.time <- function(weight)
{
weight = as.matrix( apply(abs(weight), 2, sum, na.rm = T) )
if( sum(abs(weight)) > 0 ) plotbt.holdings( t(weight) / sum(abs(weight), na.rm = T), smain = 'in time')
}
plotbt.monthly.table <- function(equity, make.plot = TRUE, smain = '')
{
equity = map2monthly(equity)
dates = index.xts(equity)
equity = coredata(equity)
if(T) {
month.ends = date.month.ends(dates)
year.ends =  date.year.ends(dates[month.ends])
year.ends = month.ends[year.ends]
nr = len(year.ends) + 1
} else {
month.ends = unique(c(endpoints(dates, 'months'), len(dates)))
month.ends = month.ends[month.ends>0]
year.ends =  unique(c(endpoints(dates[month.ends], 'years'), len(month.ends)))
year.ends = year.ends[year.ends>0]
year.ends = month.ends[year.ends]
nr = len(year.ends) + 1
}
temp = matrix( double(), nr, 12 + 2)
rownames(temp) = c(date.year(dates[year.ends]), 'Avg')
colnames(temp) = spl('Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec,Year,MaxDD')
index = c(1, year.ends)
for(iyear in 2:len(index)) {
iequity = equity[ index[(iyear-1)] : index[iyear] ]
iequity = ifna( ifna.prev(iequity), 0)
temp[(iyear-1), 'Year'] = last(iequity, 1) / iequity[1] -1
temp[(iyear-1), 'MaxDD'] = min(iequity / cummax(iequity) - 1, na.rm = T)
}
index = month.ends
monthly.returns = c(NA, diff(equity[index]) / equity[index[-len(index)]])
index = date.month(range(dates[index]))
monthly.returns = c( rep(NA, index[1]-1), monthly.returns, rep(NA, 12-index[2]) )
temp[1:(nr - 1), 1:12] = matrix(monthly.returns, ncol=12, byrow = T)
temp = ifna(temp, NA)
temp[nr,] = apply(temp[-nr,], 2, mean, na.rm = T)
if(make.plot) {
highlight = temp
highlight[] = iif(temp > 0, 'lightgreen', iif(temp < 0, 'red', 'white'))
highlight[nr,] = iif(temp[nr,] > 0, 'green', iif(temp[nr,] < 0, 'orange', 'white'))
highlight[,13] = iif(temp[,13] > 0, 'green', iif(temp[,13] < 0, 'orange', 'white'))
highlight[,14] = 'yellow'
}
temp[] = plota.format(100 * temp, 1, '', '')
if(make.plot) plot.table(temp, highlight = highlight, smain = smain)
return(temp)
}
variable.number.arguments <- function( ... )
{
out = list( ... )
if( is.list(out[[1]][[1]]) ) return( out[[1]] )
names( out ) = as.character(substitute(c(...))[-1])
return ( out )
}
list2matrix <- function
(
ilist,
keep.names = TRUE
)
{
if ( is.list( ilist[[1]] ) ) {
inc = 1
if( keep.names ) inc = 2
out = matrix('', nr = max(unlist(lapply(ilist, len))), nc = inc * len(ilist) )
colnames(out) = rep('', inc * len(ilist))
for( i in 1:len(ilist) ) {
nr = len(ilist[[i]])
colnames(out)[inc * i] = names(ilist)[i]
if(nr > 0){
if( keep.names ) {
out[1:nr,(2*i-1)] = names(ilist[[i]])
} else {
rownames(out) = names(ilist[[i]])
}
out[1:nr,inc*i] = unlist(ilist[[i]])
}
}
return(out)
} else {
return( as.matrix(unlist(ilist)) )
}
}
bt.empty.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1970::2011')
prices = data$prices
data$weight[] = 0
buy.hold = bt.run(data, trade.summary=T)
plotbt.custom.report.part1( buy.hold, trade.summary =T)
plotbt.custom.report.part2( buy.hold, trade.summary =T)
plotbt.custom.report.part3( buy.hold, trade.summary =T)
}
bt.execution.price.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1970::')
prices = data$prices
nperiods = nrow(prices)
models = list()
data$weight[] = 0
data$execution.price[] = NA
data$weight[] = 1
models$buy.hold = bt.run.share(data, clean.signal=T)
sma.fast = SMA(prices, 50)
sma.slow = SMA(prices, 200)
signal = iif(sma.fast >= sma.slow, 1, -1)
data$weight[] = NA
data$execution.price[] = NA
data$weight[] = signal
models$ma.crossover = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
data$weight[] = NA
data$execution.price[] = NA
data$weight[] = signal
models$ma.crossover.com = bt.run.share(data, commission = 0.1, clean.signal=T)
popen = bt.apply(data, Op)
signal.new = signal
trade.start	 = which(signal != mlag(signal) & signal != 0)
signal.new[trade.start] = 0
trade.start = trade.start + 1
data$weight[] = NA
data$execution.price[] = NA
data$execution.price[trade.start,] = popen[trade.start,]
data$weight[] = signal.new
models$ma.crossover.enter.next.open = bt.run.share(data, clean.signal=T, trade.summary = TRUE)
models = rev(models)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)
mtext('Cumulative Performance', side = 2, line = 1)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part3(models$ma.crossover, trade.summary = TRUE)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part3(models$ma.crossover.enter.next.open, trade.summary = TRUE)
dev.off()
commission = 4
data$weight[] = NA
data$execution.price[] = NA
data$weight[201,] = 1
data$weight[316,] = 0
data$execution.price[201,] = prices[201,] + commission
data$execution.price[316,] = prices[316,] - commission
models$test.com = bt.run.share(data, clean.signal=T, trade.summary=T)
data$weight[] = NA
data$execution.price[] = NA
data$weight[201,] = 1
data$weight[316,] = 0
models$test.com.new = bt.run.share(data, commission=commission, trade.summary=T, clean.signal=T)
cbind(last(models$test.com$equity), last(models$test.com.new$equity),
as.double(prices[316] - commission)/as.double(prices[201] + commission))
as.double(prices[202]) / as.double(prices[201] + commission)-1
models$test.com$equity[202]-1
as.double(prices[202] - commission) / as.double(prices[201])-1
models$test.com.new$equity[202]-1
commission = 0.1
sma.fast = SMA(prices, 50)
sma.slow = SMA(prices, 200)
weight = iif(sma.fast >= sma.slow, 1, -1)
weight[] = bt.exrem(weight)
index = which(!is.na(weight))
trade.start = index+1
trade.end = c(index[-1],nperiods)
trade.direction = sign(weight[index])
data$weight[] = NA
data$execution.price[] = NA
data$weight[] = weight
models$test.com.new = bt.run.share(data, commission=commission, trade.summary=T, clean.signal=T)
data$weight[] = NA
data$execution.price[] = NA
index = which(trade.direction > 0)
data$execution.price[trade.start[index],] = prices[trade.start[index],] + commission
data$execution.price[trade.end[index],] = prices[trade.end[index],] - commission
index = which(trade.direction < 0)
data$execution.price[trade.start[index],] = prices[trade.start[index],] - commission
data$execution.price[trade.end[index],] = prices[trade.end[index],] + commission
data$weight[trade.start,] = trade.direction
data$weight[trade.end,] = 0
models$test.com = bt.run.share(data, clean.signal=T, trade.summary=T)
}
bt.commission.test <- function()
{
load.packages('quantmod')
tickers = spl('EEM')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='2013:08::2013:09')
buy.date = '2013:08:14'
sell.date = '2013:08:15'
day.after.sell.date = '2013:08:16'
capital = 100000
prices = data$prices
share = as.double(capital / prices[buy.date])
comp.ret <- function(sell.trade.cost, buy.trade.cost) { round(100 * (as.double(sell.trade.cost) / as.double(buy.trade.cost) - 1), 2) }
data$weight[] = NA
data$weight[buy.date] = 1
data$weight[sell.date] = 0
commission = 0.0
model = bt.run.share(data, commission = commission, capital = capital, silent = T)
comp.ret( share * prices[sell.date], share * prices[buy.date] )
comp.ret( model$equity[day.after.sell.date], model$equity[buy.date] )
data$weight[] = NA
data$weight[buy.date] = 1
data$weight[sell.date] = 0
commission = 0.1
model = bt.run.share(data, commission = commission, capital = capital, silent = T)
comp.ret( share * (prices[sell.date] - commission), share * (prices[buy.date] + commission) )
comp.ret( model$equity[day.after.sell.date], model$equity[buy.date] )
data$weight[] = NA
data$weight[buy.date] = 1
data$weight[sell.date] = 0
commission = list(cps = 0.0, fixed = 5.0, percentage = 0.0)
model = bt.run.share(data, commission = commission, capital = capital, silent = T)
comp.ret( share * prices[sell.date] - commission$fixed, share * prices[buy.date] + commission$fixed )
comp.ret( model$equity[day.after.sell.date], model$equity[buy.date] )
data$weight[] = NA
data$weight[buy.date] = 1
data$weight[sell.date] = 0
commission = list(cps = 0.0, fixed = 0.0, percentage = 1/100)
model = bt.run.share(data, commission = commission, capital = capital, silent = T)
comp.ret( share * prices[sell.date] * (1 - commission$percentage), share * prices[buy.date] * (1 + commission$percentage) )
comp.ret( model$equity[day.after.sell.date], model$equity[buy.date] )
return
obj = portfolio.allocation.helper(data$prices,
periodicity = 'months', lookback.len = 60,
min.risk.fns = list(EW=equal.weight.portfolio)
)
commission = list(cps = 0.0, fixed = 0.0, percentage = 0/100)
models = create.strategies(obj, data, capital = capital, commission = commission )$models
ret = models$EW$ret
commission = list(cps = 0.0, fixed = 0.0, percentage = 4/100)
models = create.strategies(obj, data, capital = capital, commission = commission )$models
ret = cbind(ret, models$EW$ret)
round(100 * cbind(ret, ret[,1] - ret[,2]),2)
write.xts(cbind(ret, ret[,1] - ret[,2]), 'diff.csv')
tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='1990::')
obj = portfolio.allocation.helper(data$prices,
periodicity = 'months', lookback.len = 60,
min.risk.fns = list(
EW=equal.weight.portfolio
)
)
capital = 100000
commission = list(cps = 0.0, fixed = 0.0, percentage = 0/100)
models = create.strategies(obj, data, capital = capital, commission = commission )$models
strategy.performance.snapshoot(models, T)
}
bt.timelyportfolio.visualization.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='2000::2011')
prices = data$prices
data$weight[] = 1
buy.hold = bt.run(data)
ma10 = bt.apply.matrix(prices, EMA, 10)
ma50 = bt.apply.matrix(prices, EMA, 50)
ma200 = bt.apply.matrix(prices, EMA, 200)
data$weight[] = NA;
data$weight[] = iif(ma10 > ma50 & ma50 > ma200, 1,
iif(ma10 < ma50 & ma50 < ma200, -1, 0))
strategy = bt.run.share(data, clean.signal=F)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1)
plota(strategy$eq, type='l', ylim=range(buy.hold$eq,strategy$eq))
col = iif(strategy$weight > 0, 'green', iif(strategy$weight < 0, 'red', 'gray'))
plota.lines(buy.hold$eq, type='l', col=col)
plota.legend('strategy,Long,Short,Not Invested','black,green,red,gray')
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
drawdowns = compute.drawdown(strategy$eq)
highlight = drawdowns < -0.1
plota.control$col.x.highlight = iif(drawdowns < -0.15, 'orange', iif(drawdowns < -0.1, 'yellow', 0))
plota(strategy$eq, type='l', plotX=F, x.highlight = highlight, ylim=range(buy.hold$eq,strategy$eq))
plota.legend('strategy,10% Drawdown,15%  Drawdown','black,yellow,orange')
plota(100*drawdowns, type='l', x.highlight = highlight)
plota.legend('drawdown', 'black', x='bottomleft')
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plota.control$col.x.highlight = iif(drawdowns < -0.15, 'orange', iif(drawdowns < -0.1, 'yellow', 0))
highlight = drawdowns < -0.1
plotbt.custom.report.part1(strategy, buy.hold, x.highlight = highlight)
dev.off()
}
bt.improving.trend.following.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1970::2011')
prices = data$prices
data$weight[] = 1
buy.hold = bt.run(data)
sma = bt.apply(data, function(x) { SMA(Cl(x), 10) } )
data$weight[] = NA
data$weight[] = iif(prices >= sma, 1, 0)
trend.following = bt.run(data, trade.summary=T)
dv = bt.apply(data, function(x) { DV(HLC(x), 1, TRUE) } )
data$weight[] = NA
data$weight[] = iif(prices > sma & dv < 0.25, 1, data$weight)
data$weight[] = iif(prices < sma & dv > 0.75, 0, data$weight)
trend.following.dv1 = bt.run(data, trade.summary=T)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(trend.following.dv1, trend.following, buy.hold)
dev.off()
png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(trend.following.dv1, trend.following, buy.hold)
dev.off()
ma.lens = seq(10, 100, by = 10)
dv.lens = seq(1, 5, by = 1)
mas = matrix(double(), nrow(prices), len(ma.lens))
dvs = matrix(double(), nrow(prices), len(dv.lens))
for(i in 1:len(ma.lens)) {
ma.len = ma.lens[i]
mas[, i] = bt.apply(data, function(x) { SMA(Cl(x), ma.len) } )
}
for(i in 1:len(dv.lens)) {
dv.len = dv.lens[i]
dvs[,i] = bt.apply(data, function(x) { DV(HLC(x), dv.len, TRUE) } )
}
dummy = matrix(double(), len(ma.lens), 1+len(dv.lens))
rownames(dummy) = paste('SMA', ma.lens)
colnames(dummy) = c('NO', paste('DV', dv.lens))
out = list()
out$Cagr = dummy
out$Sharpe = dummy
out$DVR = dummy
out$MaxDD = dummy
for(ima in 1:len(ma.lens)) {
sma = mas[, ima]
cat('SMA =', ma.lens[ima], '\n')
for(idv in 0:len(dv.lens)) {
if( idv == 0 ) {
data$weight[] = NA
data$weight[] = iif(prices > sma, 1, 0)
} else {
dv = dvs[, idv]
data$weight[] = NA
data$weight[] = iif(prices > sma & dv < 0.25, 1, data$weight)
data$weight[] = iif(prices < sma & dv > 0.75, 0, data$weight)
}
strategy = bt.run(data, silent=T)
idv = idv + 1
out$Cagr[ima, idv] = compute.cagr(strategy$equity)
out$Sharpe[ima, idv] = compute.sharpe(strategy$ret)
out$DVR[ima, idv] = compute.DVR(strategy)
out$MaxDD[ima, idv] = compute.max.drawdown(strategy$equity)
}
}
png(filename = 'plot3.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')
layout(matrix(1:4,nrow=2))
for(i in names(out)) {
temp = out[[i]]
temp[] = plota.format( 100 * temp, 1, '', '' )
plot.table(temp, smain = i, highlight = T, colorbar = F)
}
dev.off()
}
bt.roc.cross.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1970::2011')
prices = data$prices
data$weight[] = 1
buy.hold = bt.run(data)
roc5 = prices / mlag(prices,5)
roc252 = prices / mlag(prices,252)
roc5.1 = mlag(roc5,1)
roc5.2 = mlag(roc5,2)
roc252.1 = mlag(roc252,1)
roc252.2 = mlag(roc252,2)
data$weight[] = NA
data$weight$SPY[] = iif(roc252.2 < roc5.2 & roc252.1 > roc5.1 & roc252 > roc5, 1, data$weight$SPY)
data$weight$SPY[] = iif(roc252.2 > roc5.2 & roc252.1 < roc5.1 & roc252 < roc5, -1, data$weight$SPY)
roc.cross = bt.run(data, trade.summary=T)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(roc.cross, buy.hold, trade.summary=T)
dev.off()
png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(roc.cross, buy.hold, trade.summary=T)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part3(roc.cross, buy.hold, trade.summary=T)
dev.off()
data$weight[] = NA
data$weight$SPY[] = iif(roc252.2 < roc5.2 & roc252.1 > roc5.1 & roc252 > roc5, 1, data$weight$SPY)
data$weight$SPY[] = iif(roc252.2 > roc5.2 & roc252.1 < roc5.1 & roc252 < roc5, -1, data$weight$SPY)
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)
roc.cross.share = bt.run(data, type='share', trade.summary=T, capital=capital)
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(roc.cross.share, roc.cross, buy.hold, trade.summary=T)
dev.off()
png(filename = 'plot5.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(roc.cross.share, roc.cross, buy.hold, trade.summary=T)
dev.off()
}
bt.rotational.trading.test <- function()
{
load.packages('quantmod')
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1970::')
prices = data$prices
n = len(tickers)
month.ends = endpoints(prices, 'months')
month.ends = month.ends[month.ends > 0]
models = list()
dates = '2001::'
data$weight[] = NA
data$weight[month.ends,] = ntop(prices, n)[month.ends,]
models$equal.weight = bt.run.share(data, clean.signal=F, dates=dates)
position.score = prices / mlag(prices, 126)
data$weight[] = NA
data$weight[month.ends,] = ntop(position.score[month.ends,], 2)
models$top2 = bt.run.share(data, trade.summary=T, dates=dates)
data$weight[] = NA
data$weight[month.ends,] = ntop.keep(position.score[month.ends,], 2, 6)
models$top2.keep6 = bt.run.share(data, trade.summary=T, dates=dates)
strategy.performance.snapshoot(models, T)
layout(1)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
pdf(file = 'report.pdf', width=8.5, height=11)
plotbt.custom.report(models, trade.summary=T)
dev.off()
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(models)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part3(models, trade.summary=T)
dev.off()
}
bt.timing.model.test <- function()
{
load.packages('quantmod')
tickers = spl('VTI,VEU,IEF,VNQ,DBC')
tickers = spl('VTI,EFA,IEF,ICF,DBC,SHY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
for(i in ls(data)) cat( i, format(index(data[[i]][1,]), '%d%b%y'), '\n')
CRB = get.CRB()
index = max(which( index(CRB) < index(data$DBC[1,]) ))
scale = as.vector(Cl(data$DBC[1,])) / as.vector(Cl(CRB[(index + 1),]))
temp = CRB[1 : (index + 1),] * repmat(scale, index + 1, 6)
data$DBC = rbind( temp[1:index,], data$DBC )
bt.prep(data, align='remove.na', dates='1970::2011')
prices = data$prices
n = len(tickers)
position.score = prices
position.score$SHY = NA
month.ends = date.month.ends(index(prices))
data$weight[] = NA
data$weight[month.ends,] = ntop(position.score[month.ends,], n)
capital = 100000
data$weight[] = (capital / prices) * data$weight
equal.weight = bt.run(data, type='share', capital=capital)
sma = bt.apply.matrix(prices, SMA, 200)
buy.rule = prices > sma
buy.rule = ifna(buy.rule, F)
weight = ntop(position.score[month.ends,], n)
weight[!buy.rule[month.ends,]] = 0
weight$SHY = 1 - rowSums(weight)
data$weight[] = NA
data$weight[month.ends,] = weight
capital = 100000
data$weight[] = (capital / prices) * data$weight
timing = bt.run(data, type='share', trade.summary=T, capital=capital)
weight = ntop(position.score, n)
weight[!buy.rule] = 0
weight$SHY = 1 - rowSums(weight)
data$weight[] = NA
data$weight[] = weight
capital = 100000
data$weight[] = (capital / prices) * data$weight
timing.d = bt.run(data, type='share', trade.summary=T, capital=capital)
pdf(file = 'report.pdf', width=8.5, height=11)
plotbt.custom.report(timing, timing.d, equal.weight, trade.summary=T)
dev.off()
dv = bt.apply(data, function(x) { DV(HLC(x), 1, TRUE) } )
data$weight[] = NA
data$weight[] = iif(prices > sma & dv < 0.25, 0.2, data$weight)
data$weight[] = iif(prices < sma & dv > 0.75, 0, data$weight)
data$weight$SHY = 0
data$weight = bt.apply.matrix(data$weight, ifna.prev)
data$weight$SHY = 1 - rowSums(data$weight)
data$weight = bt.exrem(data$weight)
capital = 100000
data$weight[] = (capital / prices) * data$weight
timing.d1 = bt.run(data, type='share', trade.summary=T, capital=capital)
models = variable.number.arguments(timing.d1, timing.d, timing, equal.weight)
sapply(models, compute.turnover, data)
plotbt.custom.report.part1(timing.d1, timing.d, timing, equal.weight)
}
bt.meom.test <- function()
{
load.packages('quantmod')
tickers = spl('DIA,EEM,EFA,EWH,EWJ,EWT,EWZ,FXI,GLD,GSG,IEF,ILF,IWM,IYR,QQQ,SPY,VNQ,XLB,XLE,XLF,XLI,XLP,XLU,XLV,XLY,XLK')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1995-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1995::')
prices = data$prices
n = ncol(prices)
nperiods = nrow(prices)
data$weight[] = ntop(prices, n)
equal.weight = bt.run(data)
month.ends = endpoints(prices, 'months')
month.ends = month.ends[month.ends > 0]
month.ends2 = iif(month.ends + 2 > nperiods, nperiods, month.ends + 2)
data$weight[] = NA
data$weight[month.ends,] = ntop(prices, n)[month.ends,]
data$weight[month.ends2,] = 0
capital = 100000
data$weight[] = (capital / prices) * data$weight
meom.equal.weight = bt.run(data, type='share', capital=capital)
buy.rule = prices > bt.apply.matrix(prices, function(x) { WMA(x, 89) } )
buy.rule = ifna(buy.rule, F)
ret2 = ifna(prices / mlag(prices, 2), 0)
position.score = bt.apply.matrix(ret2, SMA, 5) * bt.apply.matrix(ret2, SMA, 40)
position.score[!buy.rule] = NA
data$weight[] = NA;
data$weight[month.ends,] = ntop(position.score[month.ends,], 2)
data$weight[month.ends2,] = 0
capital = 100000
data$weight[] = (capital / prices) * data$weight
meom.top2.rank1 = bt.run(data, type='share', trade.summary=T, capital=capital)
position.score = bt.apply.matrix(ret2, SMA, 5) * mlag( bt.apply.matrix(ret2, SMA, 10), 5)
position.score[!buy.rule] = NA
data$weight[] = NA
data$weight[month.ends,] = ntop(position.score[month.ends,], 2)
data$weight[month.ends2,] = 0
capital = 100000
data$weight[] = (capital / prices) * data$weight
meom.top2.rank2 = bt.run(data, type='share', trade.summary=T, capital=capital)
pdf(file = 'report.pdf', width=8.5, height=11)
plotbt.custom.report(meom.top2.rank2, meom.top2.rank1, meom.equal.weight, equal.weight, trade.summary=T)
dev.off()
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(meom.top2.rank2, meom.top2.rank1, meom.equal.weight, equal.weight, trade.summary=T)
dev.off()
png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(meom.top2.rank2, meom.top2.rank1, meom.equal.weight, equal.weight, trade.summary=T)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part3(meom.top2.rank2, meom.top2.rank1, meom.equal.weight, equal.weight, trade.summary=T)
dev.off()
month.ends1 = iif(month.ends + 1 > nperiods, nperiods, month.ends + 1)
data$weight[] = NA
data$weight[month.ends,] = ntop(position.score[month.ends,], 2)
data$weight[month.ends2,] = 0
popen = bt.apply(data, Op)
data$weight[month.ends1,] = iif((prices > popen)[month.ends1,], 0, NA)
capital = 100000
data$weight[] = (capital / prices) * data$weight
meom.top2.rank2.hold12 = bt.run(data, type='share', trade.summary=T, capital=capital)
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(meom.top2.rank2.hold12, meom.top2.rank2, meom.top2.rank1, meom.equal.weight, equal.weight, trade.summary=T)
dev.off()
png(filename = 'plot5.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(meom.top2.rank2.hold12, meom.top2.rank2, meom.top2.rank1, meom.equal.weight, equal.weight, trade.summary=T)
dev.off()
}
bt.intraday.test <- function()
{
load.packages('quantmod')
EURUSD = getSymbols.fxhistoricaldata('EURUSD', 'hour', auto.assign = F, download=F)
SPY = getSymbols('SPY', src = 'yahoo', from = '1980-01-01', auto.assign = F)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plota(EURUSD['2012:03:06 10::2012:03:06 21'], type='candle', main='EURUSD on 2012:03:06 from 10 to 21')
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
dates= '2012:01:01::2012:01:11'
y = SPY[dates]
plota(y, type = 'candle', LeftMargin=3)
y = EURUSD[dates]
plota2Y(y, ylim = range(OHLC(y), na.rm=T), las=1, col='red', col.axis = 'red')
plota.ohlc(y, col=plota.candle.col(y))
plota.legend('SPY(rhs),EURUSD(lhs)', 'black,red', list(SPY[dates],EURUSD[dates]))
dev.off()
tickers = spl('EURUSD,USDJPY,GBPUSD,AUDUSD,USDCHF,USDCAD')
data <- new.env()
getSymbols.fxhistoricaldata(tickers, 'day', data, download=F)
bt.prep(data, align='remove.na', dates='1990::')
prices = data$prices
n = len(tickers)
models = list()
data$weight[] = NA
data$weight[] = ntop(prices, n)
models$equal.weight = bt.run.share(data, clean.signal=F)
sma = bt.apply.matrix(prices, SMA, 200)
data$weight[] = NA
data$weight[] = ntop(prices, n) * (prices > sma)
models$timing = bt.run.share(data, clean.signal=F)
models = rev(models)
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot4.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(models)
dev.off()
data <- new.env()
getSymbols.fxhistoricaldata(tickers, 'hour', data, download=F)
bt.prep(data, align='remove.na', dates='1990::')
prices = data$prices
n = len(tickers)
models = list()
data$weight[] = NA
data$weight[] = ntop(prices, n)
models$equal.weight = bt.run.share(data, clean.signal=F)
sma = bt.apply.matrix(prices, SMA, 200)
data$weight[] = NA
data$weight[] = ntop(prices, n) * (prices > sma)
models$timing = bt.run.share(data, clean.signal=F)
models = rev(models)
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot6.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(models)
dev.off()
}
bt.min.var.test <- function()
{
load.packages('quantmod,quadprog,lpSolve')
tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
data.weekly <- new.env()
for(i in tickers) data.weekly[[i]] = to.weekly(data[[i]], indexAt='endof')
bt.prep(data, align='remove.na', dates='1990::')
bt.prep(data.weekly, align='remove.na', dates='1990::')
prices = data$prices
n = ncol(prices)
week.ends = endpoints(prices, 'weeks')
week.ends = week.ends[week.ends > 0]
data$weight[] = NA
data$weight[week.ends,] = ntop(prices[week.ends,], n)
capital = 100000
data$weight[] = (capital / prices) * data$weight
equal.weight = bt.run(data, type='share', capital=capital)
constraints = new.constraints(n, lb = -Inf, ub = +Inf)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ret = prices / mlag(prices) - 1
weight = coredata(prices)
weight[] = NA
for( i in week.ends[week.ends >= (63 + 1)] ) {
hist = ret[ (i- 63 +1):i, ]
ia = create.ia(hist)
s0 = apply(coredata(hist),2,sd)
ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
weight[i,] = min.risk.portfolio(ia, constraints)
}
data$weight[] = weight
capital = 100000
data$weight[] = (capital / prices) * data$weight
min.var.daily = bt.run(data, type='share', capital=capital)
retw = data.weekly$prices / mlag(data.weekly$prices) - 1
weightw = coredata(prices)
weightw[] = NA
for( i in week.ends[week.ends >= (63 + 1)] ) {
j = which(index(ret[i,]) == index(retw))
hist = retw[ (j- 13 +1):j, ]
ia = create.ia(hist)
s0 = apply(coredata(hist),2,sd)
ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
weightw[i,] = min.risk.portfolio(ia, constraints)
}
data$weight[] = weightw
capital = 100000
data$weight[] = (capital / prices) * data$weight
min.var.weekly = bt.run(data, type='share', capital=capital, trade.summary = T)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(min.var.weekly, min.var.daily, equal.weight)
dev.off()
png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(min.var.weekly, min.var.daily, equal.weight)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
plotbt.transition.map(min.var.daily$weight)
legend('topright', legend = 'min.var.daily', bty = 'n')
plotbt.transition.map(min.var.weekly$weight)
legend('topright', legend = 'min.var.weekly', bty = 'n')
dev.off()
}
bt.aa.test <- function()
{
load.packages('quantmod,quadprog,corpcor,lpSolve')
tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='1990::2011')
prices = data$prices
n = ncol(prices)
period.ends = endpoints(prices, 'weeks')
period.annual.factor = 52
period.ends = period.ends[period.ends > 0]
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ret = prices / mlag(prices) - 1
start.i = which(period.ends >= (63 + 1))[1]
min.risk.fns = spl('min.risk.portfolio,min.maxloss.portfolio')
weight = NA * prices[period.ends,]
weights = list()
weights$equal.weight = weight
weights$equal.weight[] = ntop(prices[period.ends,], n)
weights$equal.weight[1:start.i,] = NA
for(f in min.risk.fns) weights[[ gsub('\\.portfolio', '', f) ]] = weight
risk.contributions = list()
for(f in names(weights)) risk.contributions[[ f ]] = weight
for( j in start.i:len(period.ends) ) {
i = period.ends[j]
hist = ret[ (i- 63 +1):i, ]
include.index = rep(TRUE, n)
ia = create.ia(hist)
s0 = apply(coredata(hist),2,sd)
ia$correlation = cor(coredata(hist), use='complete.obs',method='pearson')
ia$cov = ia$correlation * (s0 %*% t(s0))
for(f in min.risk.fns) {
constraints$x0 = weights[[ gsub('\\.portfolio', '', f) ]][(j-1), include.index]
weights[[ gsub('\\.portfolio', '', f) ]][j, include.index] = match.fun(f)(ia, constraints)
}
for(f in names(weights)) {
risk.contributions[[ f ]][j, include.index] = portfolio.risk.contribution(weights[[ f ]][j, include.index], ia)
}
if( j %% 10 == 0) cat(j, '\n')
}
models = list()
for(i in names(weights)) {
data$weight[] = NA
data$weight[period.ends,] = weights[[i]]
models[[i]] = bt.run.share(data, clean.signal = F)
}
models = rev(models)
weights = rev(weights)
risk.contributions = rev(risk.contributions)
png(filename = 'plot1.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)
mtext('Cumulative Performance', side = 2, line = 1)
dev.off()
png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(models)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
dev.off()
png(filename = 'plot4.png', width = 600, height = 1600, units = 'px', pointsize = 12, bg = 'white')
layout(1:len(models))
for(m in names(models)) {
plotbt.transition.map(models[[m]]$weight, name=m)
legend('topright', legend = m, bty = 'n')
}
dev.off()
png(filename = 'plot5.png', width = 600, height = 1600, units = 'px', pointsize = 12, bg = 'white')
layout(1:len(risk.contributions))
for(m in names(risk.contributions)) {
plotbt.transition.map(risk.contributions[[m]], name=paste('Risk Contributions',m))
legend('topright', legend = m, bty = 'n')
}
dev.off()
png(filename = 'plot6.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
plota.matplot(lapply(weights, portfolio.concentration.gini.coefficient), main='Gini Coefficient')
plota.matplot(lapply(weights, portfolio.concentration.herfindahl.index), main='Herfindahl Index')
dev.off()
png(filename = 'plot7.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
out = compute.stats(weights,
list(Gini=function(w) mean(portfolio.concentration.gini.coefficient(w), na.rm=T),
Herfindahl=function(w) mean(portfolio.concentration.herfindahl.index(w), na.rm=T),
Turnover=function(w) period.annual.factor * mean(portfolio.turnover(w), na.rm=T)
)
)
out[] = plota.format(100 * out, 1, '', '%')
plot.table(t(out))
dev.off()
png(filename = 'plot8.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1)
barplot.with.labels(sapply(weights, function(w) period.annual.factor * mean(portfolio.turnover(w), na.rm=T)), 'Average Annual Portfolio Turnover')
dev.off()
}
bt.aa.test.new <- function()
{
load.packages('quantmod,quadprog,corpcor,lpSolve')
tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='1990::')
cluster.group = cluster.group.kmeans.90
obj = portfolio.allocation.helper(data$prices,
periodicity = 'months', lookback.len = 60,
min.risk.fns = list(
EW=equal.weight.portfolio,
RP=risk.parity.portfolio(),
MD=max.div.portfolio,
MV=min.var.portfolio,
MVE=min.var.excel.portfolio,
MV2=min.var2.portfolio,
MC=min.corr.portfolio,
MCE=min.corr.excel.portfolio,
MC2=min.corr2.portfolio,
MS=max.sharpe.portfolio(),
ERC = equal.risk.contribution.portfolio,
TRET.12 = target.return.portfolio(12/100),
TRISK.10 = target.risk.portfolio(10/100),
C.EW = distribute.weights(equal.weight.portfolio, cluster.group),
C.RP = distribute.weights(risk.parity.portfolio(), cluster.group),
RSO.RP.5 = rso.portfolio(risk.parity.portfolio(), 5, 500),
MMaxLoss = min.maxloss.portfolio,
MMad = min.mad.portfolio,
MCVaR = min.cvar.portfolio,
MCDaR = min.cdar.portfolio,
MMadDown = min.mad.downside.portfolio,
MRiskDown = min.risk.downside.portfolio,
MCorCov = min.cor.insteadof.cov.portfolio
)
)
models = create.strategies(obj, data)$models
png(filename = 'plot1.png', width = 1800, height = 1800, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T, 'Backtesting Asset Allocation portfolios')
dev.off()
}
bt.rebalancing.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY,TLT')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='1900::2011')
prices = data$prices
nperiods = nrow(prices)
target.allocation = matrix(c(0.5, 0.5), nrow=1)
data$weight[] = NA
data$weight[1,] = target.allocation
capital = 100000
data$weight[] = (capital / prices) * data$weight
buy.hold = bt.run(data, type='share', capital=capital)
models = list()
for(period in spl('months,quarters,years')) {
data$weight[] = NA
data$weight[1,] = target.allocation
period.ends = endpoints(prices, period)
period.ends = period.ends[period.ends > 0]
data$weight[period.ends,] = repmat(target.allocation, len(period.ends), 1)
capital = 100000
data$weight[] = (capital / prices) * data$weight
models[[period]] = bt.run(data, type='share', capital=capital)
}
models$buy.hold = buy.hold
compute.turnover(models$years, data)
compute.max.deviation(models$years, target.allocation)
pdf(file = 'report.pdf', width=8.5, height=11)
plotbt.custom.report(models)
dev.off()
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
plotbt.transition.map(models$buy.hold$weight, 'buy.hold', spl('red,orange'))
abline(h=50)
plotbt.transition.map(models$months$weight, 'months', spl('red,orange'))
abline(h=50)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
dev.off()
models$smart5.all = bt.max.deviation.rebalancing(data, buy.hold, target.allocation, 5/100, 0)
models$smart5.half = bt.max.deviation.rebalancing(data, buy.hold, target.allocation, 5/100, 0.5)
png(filename = 'plot4.png', width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')
layout(1:4)
plotbt.transition.map(models$buy.hold$weight, 'buy.hold', spl('red,orange'))
abline(h=50)
plotbt.transition.map(models$smart5.all$weight, 'Max Deviation 5%, All the way', spl('red,orange'))
abline(h=50)
plotbt.transition.map(models$smart5.half$weight, 'Max Deviation 5%, Half the way', spl('red,orange'))
abline(h=50)
plotbt.transition.map(models$years$weight, 'years', spl('red,orange'))
abline(h=50)
dev.off()
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover', F)
barplot.with.labels(sapply(models, compute.max.deviation, target.allocation), 'Maximum Deviation from Target Mix')
dev.off()
png(filename = 'plot6.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(models)
dev.off()
months = spl('Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec')
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]
models = list()
for(i in 1:12) {
index = which( date.month(index(prices)[period.ends]) == i )
data$weight[] = NA
data$weight[1,] = target.allocation
data$weight[period.ends[index],] = repmat(target.allocation, len(index), 1)
capital = 100000
data$weight[] = (capital / prices) * data$weight
models[[ months[i] ]] = bt.run(data, type='share', capital=capital)
}
png(filename = 'plot7.png', width = 1200, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(models)
dev.off()
layout(1)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
}
bt.max.deviation.rebalancing <- function
(
data,
model,
target.allocation,
max.deviation = 3/100,
rebalancing.ratio = 0,
start.index = 1,
period.ends = 1:nrow(model$weight)
)
{
nperiods = nrow(model$weight)
action.index = rep(F, nperiods)
start.index = period.ends[start.index]
start.index0 = start.index
while(T) {
weight = model$weight
index = apply(abs(weight - rep.row(target.allocation, nperiods)), 1, max) > max.deviation
index = which( index[period.ends] )
if( len(index) > 0 ) {
index = period.ends[index]
index = index[ index > start.index ]
if( len(index) > 0 ) {
action.index[index[1]] = T
data$weight[] = NA
data$weight[start.index0,] = target.allocation
temp = rep.row(target.allocation, sum(action.index))
data$weight[action.index,] = temp +
rebalancing.ratio * (weight[action.index,] - temp)
model = bt.run.share(data, clean.signal=F, silent=T)
start.index = index[1]
} else break
} else break
}
return(model)
}
bt.rebalancing1.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY,TLT,GLD,FXE,USO,SHY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='1900::2011')
prices = data$prices
nperiods = nrow(prices)
target.allocation = matrix(rep(1/6,6), nrow=1)
data$weight[] = NA
data$weight[1,] = target.allocation
capital = 100000
data$weight[] = (capital / prices) * data$weight
buy.hold = bt.run(data, type='share', capital=capital)
models = list()
for(period in spl('months,quarters,years')) {
data$weight[] = NA
data$weight[1,] = target.allocation
period.ends = endpoints(prices, period)
period.ends = period.ends[period.ends > 0]
data$weight[period.ends,] = repmat(target.allocation, len(period.ends), 1)
capital = 100000
data$weight[] = (capital / prices) * data$weight
models[[period]] = bt.run(data, type='share', capital=capital)
}
models$buy.hold = buy.hold
models$smart3.all = bt.max.deviation.rebalancing(data, buy.hold, target.allocation, 3/100, 0)
models$smart3.half = bt.max.deviation.rebalancing(data, buy.hold, target.allocation, 3/100, 0.5)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover', F)
barplot.with.labels(sapply(models, compute.max.deviation, target.allocation), 'Maximum Deviation from Target Mix')
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(models)
dev.off()
}
engineering.returns.kpi <- function
(
bt,
trade.summary = NULL
)
{
if( !is.null(bt$trade.summary) ) trade.summary = bt$trade.summary
out = list()
out$Period = join( format( range(index(bt$equity)), '%b%Y'), ' - ')
out$Cagr = compute.cagr(bt$equity)
out$DVR = compute.DVR(bt)
out$Sharpe = compute.sharpe(bt$ret)
out$R2 = compute.R2(bt$equity)
if( !is.null(trade.summary) ) {
out$Win.Percent = trade.summary$stats['win.prob', 'All']
out$Avg.Trade = trade.summary$stats['avg.pnl', 'All']
}
out$MaxDD = compute.max.drawdown(bt$equity)
out = lapply(out, function(x) if(is.double(x)) round(100*x,1) else x)
if( !is.null(trade.summary) ) out$Num.Trades = trade.summary$stats['ntrades', 'All']
return( list(System=out))
}
bt.rotational.trading.trades.test <- function()
{
load.packages('quantmod')
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='1970::2011')
prices = data$prices
n = len(tickers)
week.ends = endpoints(prices, 'weeks')
week.ends = week.ends[week.ends > 0]
position.score = prices / mlag(prices, 200)
position.score.ma = position.score
buy.rule = T
data$weight[] = NA
data$weight[] = ntop(position.score, 2)
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)
top2.d = bt.run(data, type='share', trade.summary=T, capital=capital)
data$weight[] = NA
data$weight[week.ends,] = ntop(position.score[week.ends,], 2)
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)
top2.w = bt.run(data, type='share', trade.summary=T, capital=capital)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(top2.d, top2.w, perfromance.fn = 'engineering.returns.kpi')
dev.off()
data$weight[] = NA
data$weight[] = ntop.keep(position.score, 2, 4)
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)
top2.d.keep4 = bt.run(data, type='share', trade.summary=T, capital=capital)
data$weight[] = NA
data$weight[] = ntop.keep(position.score, 2, 6)
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)
top2.d.keep6 = bt.run(data, type='share', trade.summary=T, capital=capital)
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(top2.d, top2.d.keep4, top2.d.keep6, perfromance.fn = 'engineering.returns.kpi')
dev.off()
models = list()
models$Bench = top2.d
for( avg in spl('SMA,EMA') ) {
for( i in c(3,5,10,20) ) {
position.score.smooth = bt.apply.matrix(position.score.ma, avg, i)
position.score.smooth[!buy.rule,] = NA
data$weight[] = NA
data$weight[] = ntop(position.score.smooth, 2)
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)
models[[ paste(avg,i) ]] = bt.run(data, type='share', trade.summary=T, capital=capital)
}
}
png(filename = 'plot3.png', width = 1200, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(models, perfromance.fn = 'engineering.returns.kpi')
dev.off()
position.score.smooth = bt.apply.matrix(position.score.ma, 'EMA', 10)
position.score.smooth[!buy.rule,] = NA
data$weight[] = NA
data$weight[] = ntop.keep(position.score.smooth, 2, 6)
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)
top2.d.keep6.EMA10 = bt.run(data, type='share', trade.summary=T, capital=capital)
data$weight[] = NA
data$weight[week.ends,] = ntop.keep(position.score[week.ends,], 2, 6)
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)
top2.w.keep6 = bt.run(data, type='share', trade.summary=T, capital=capital)
position.score.smooth[] = NA
position.score.smooth[week.ends,] = bt.apply.matrix(position.score.ma[week.ends,], 'EMA', 10)
position.score.smooth[!buy.rule,] = NA
data$weight[] = NA
data$weight[week.ends,] = ntop.keep(position.score.smooth[week.ends,], 2, 6)
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)
top2.w.keep6.EMA10 = bt.run(data, type='share', trade.summary=T, capital=capital)
png(filename = 'plot4.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(top2.d, top2.d.keep6, top2.d.keep6.EMA10, top2.w, top2.w.keep6, top2.w.keep6.EMA10, perfromance.fn = 'engineering.returns.kpi')
dev.off()
data$weight[] = ntop(prices, n)
ew = bt.run(data)
buy.rule = (ew$equity > SMA(ew$equity,200)) | (ew$equity > SMA(ew$equity,30))
buy.rule = (ew$equity > SMA(ew$equity,200))
buy.rule = ifna(buy.rule, F)
position.score = bt.apply(data, function(x) TSI(HLC(x)) )
position.score.ma = position.score
position.score[!buy.rule,] = NA
}
bt.december.trading.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='1970::2011')
prices = data$prices
n = len(tickers)
ret = prices / mlag(prices) - 1
dates = index(prices)
years = date.year(dates)
index = which(date.month(dates) == 12)
trading.days = sapply(tapply(ret[index,], years[index], function(x) coredata(x)), function(x) x[1:22])
avg.trading.days = apply(trading.days[, -ncol(trading.days)], 1, mean, na.rm=T)
current.year = trading.days[, ncol(trading.days)]
avg.trading.days = 100 * ( cumprod(1 + avg.trading.days) - 1 )
current.year = 100 * ( cumprod(1 + current.year) - 1 )
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
par(mar=c(4,4,1,1))
plot(avg.trading.days, type='b', col=1,
ylim=range(avg.trading.days,current.year,na.rm=T),
xlab = 'Number of Trading Days in December',
ylab = 'Avg % Profit/Loss'
)
lines(current.year, type='b', col=2)
grid()
plota.legend('Avg SPY,SPY Dec 2011', 1:2)
dev.off()
data$weight[] = 1
capital = 100000
data$weight[] = (capital / prices) * data$weight
buy.hold = bt.run(data, type='share', capital=capital)
index = which(date.month(dates) == 11)
last.day.november = match(tapply(dates[index], years[index], function(x) tail(x,1)), dates)
index = which(date.month(dates) == 12)
last.day.december = match(tapply(dates[index], years[index], function(x) tail(x,1)), dates)
data$weight[] = NA
data$weight[last.day.november,] = 1
data$weight[last.day.december,] = 0
capital = 100000
data$weight[] = (capital / prices) * data$weight
december = bt.run(data, type='share', capital=capital, trade.summary=T)
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(december, buy.hold, trade.summary=T)
dev.off()
png(filename = 'plot3.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(december, buy.hold, trade.summary=T)
dev.off()
}
bt.seasonality.test <- function()
{
load.packages('quantmod')
tickers = dow.jones.components()
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1970::2011')
prices = data$prices
n = ncol(prices)
month.ends = endpoints(prices, 'months')
prices = prices[month.ends,]
ret = prices / mlag(prices) - 1
ret = ret[date.month(index(ret)) == 1, ]
ret = last(ret,20)
stats = matrix(rep(NA,2*n), nc=n)
colnames(stats) = colnames(prices)
rownames(stats) = spl('N,Positive')
for(i in 1:n) {
stats['N',i] = sum(!is.na(ret[,i]))
stats['Positive',i] = sum(ret[,i]>0, na.rm=T)
}
sort(stats['Positive',], decreasing =T)
png(filename = 'plot1.png', width = 600, height = 200, units = 'px', pointsize = 12, bg = 'white')
plot.table(stats[, order(stats['Positive',], decreasing =T)[1:10]])
dev.off()
}
bt.test.garch.speed <- function()
{
load.packages('tseries,fGarch,rbenchmark')
temp = garchSim(n=252)
test1 <- function() {
fit1=garch(temp, order = c(1, 1), control = garch.control(trace = F))
}
test2 <- function() {
fit2=garchFit(~ garch(1,1), data = temp, include.mean=FALSE, trace=F)
}
benchmark(
test1(),
test2(),
columns=spl('test,replications,elapsed,relative'),
order='relative',
replications=100
)
}
garch.predict.one.day <- function(fit, r.last)
{
h.last = tail( fitted(fit)[,1] ,1)
sqrt(sum( coef(fit) * c(1,  r.last^2, h.last^2) ))
}
garchFit.predict.one.day <- function(fit, r.last)
{
h.last = tail(sqrt(fit@h.t), 1)
sqrt(sum( fit@fit$matcoef[,1] * c(1,  r.last^2, h.last^2) ))
}
bt.forecast.garch.volatility <- function(ret.log, est.period = 252)
{
nperiods = nrow(ret.log)
garch.vol = NA * ret.log
for( i in (est.period + 1) : nperiods ) {
temp = as.vector(ret.log[ (i - est.period + 1) : i, ])
r.last =  tail( temp, 1 )
fit = tryCatch( garch(temp, order = c(1, 1), control = garch.control(trace = F)),
error=function( err ) FALSE, warning=function( warn ) FALSE )
if( !is.logical( fit ) ) {
if( i == est.period + 1 ) garch.vol[1:est.period] = fitted(fit)[,1]
garch.vol[i] = garch.predict.one.day(fit, r.last)
} else {
fit = tryCatch( garchFit(~ garch(1,1), data = temp, include.mean=FALSE, trace=F),
error=function( err ) FALSE, warning=function( warn ) FALSE )
if( !is.logical( fit ) ) {
if( i == est.period + 1 ) garch.vol[1:est.period] = sqrt(fit@h.t)
garch.vol[i] = garchFit.predict.one.day(fit, r.last)
}
}
if( i %% 100 == 0) cat(i, '\n')
}
garch.vol[] = ifna.prev(coredata(garch.vol))
return(garch.vol)
}
bt.volatility.garch <- function()
{
load.packages('quantmod')
tickers = 'SPY'
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='2000::2012')
prices = data$prices
n = len(tickers)
nperiods = nrow(prices)
data$weight[] = 1
buy.hold = bt.run(data)
rsi2 = bt.apply.matrix(prices, RSI, 2)
data$weight[] = NA
data$weight[] = iif(rsi2 < 50, 1, -1)
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)
mr = bt.run(data, type='share', capital=capital, trade.summary=T)
sma.short = bt.apply.matrix(prices, SMA, 50)
sma.long = bt.apply.matrix(prices, SMA, 200)
data$weight[] = NA
data$weight[] = iif(sma.short > sma.long, 1, -1)
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)
tf = bt.run(data, type='share', capital=capital, trade.summary=T)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(mr, tf, buy.hold, trade.summary=T)
dev.off()
ret.log = bt.apply.matrix(prices, ROC, type='continuous')
hist.vol = bt.apply.matrix(ret.log, runSD, n = 21)
vol.rank = percent.rank(SMA(percent.rank(hist.vol, 252), 21), 250)
data$weight[] = NA
data$weight[] = iif(vol.rank > 0.5,
iif(rsi2 < 50, 1, -1),
iif(sma.short > sma.long, 1, -1)
)
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)
regime.switching = bt.run(data, type='share', capital=capital, trade.summary=T)
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(regime.switching, mr, tf, buy.hold, trade.summary=T)
dev.off()
load.packages('tseries,fGarch')
garch.vol = bt.forecast.garch.volatility(ret.log, 252)
vol.rank = percent.rank(SMA(percent.rank(garch.vol, 252), 21), 250)
data$weight[] = NA
data$weight[] = iif(vol.rank > 0.5,
iif(rsi2 < 50, 1, -1),
iif(sma.short > sma.long, 1, -1)
)
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)
regime.switching.garch = bt.run(data, type='share', capital=capital, trade.summary=T)
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(regime.switching.garch, regime.switching, buy.hold, trade.summary=T)
dev.off()
}
bt.matching.test <- function()
{
load.packages('quantmod')
tickers = 'SPY'
data = getSymbols(tickers, src = 'yahoo', from = '1950-01-01', auto.assign = F)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
obj = bt.matching.find(Cl(data), plot=T)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
matches = bt.matching.overlay(obj, plot=T)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
bt.matching.overlay.table(obj, matches, plot=T)
dev.off()
data = last(data, 252*10)
reference = coredata(Cl(data))
n = len(reference)
query = reference[(n-90+1):n]
reference = reference[1:(n-90)]
n.query = len(query)
n.reference = len(reference)
dist = rep(NA, n.reference)
query.normalized = (query - mean(query)) / sd(query)
for( i in n.query : n.reference ) {
window = reference[ (i - n.query + 1) : i]
window.normalized = (window - mean(window)) / sd(window)
dist[i] = stats:::dist(rbind(query.normalized, window.normalized))
}
min.index = c()
n.match = 10
temp = dist
temp[ temp > mean(dist, na.rm=T) ] = NA
for(i in 1:n.match) {
if(any(!is.na(temp))) {
index = which.min(temp)
min.index[i] = index
temp[max(0,index - 2*n.query) : min(n.reference,(index + n.query))] = NA
}
}
n.match = len(min.index)
dates = index(data)[1:len(dist)]
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
par(mar=c(2, 4, 2, 2))
plot(dates, dist, type='l',col='gray', main='Top Matches', ylab='Euclidean Distance', xlab='')
abline(h = mean(dist, na.rm=T), col='darkgray', lwd=2)
points(dates[min.index], dist[min.index], pch=22, col='red', bg='red')
text(dates[min.index], dist[min.index], 1:n.match, adj=c(1,1), col='black',xpd=TRUE)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plota(data, type='l', col='gray', main=tickers)
plota.lines(last(data,90), col='blue')
for(i in 1:n.match) {
plota.lines(data[(min.index[i]-n.query + 1):min.index[i]], col='red')
}
text(index4xts(data)[min.index - n.query/2], reference[min.index - n.query/2], 1:n.match,
adj=c(1,-1), col='black',xpd=TRUE)
plota.legend('Pattern,Match Number','blue,red')
dev.off()
matches = matrix(NA, nr=(n.match+1), nc=3*n.query)
temp = c(rep(NA, n.query), reference, query)
for(i in 1:n.match) {
matches[i,] = temp[ (min.index[i] - n.query + 1):(min.index[i] + 2*n.query) ]
}
matches[(n.match+1),] = temp[ (len(temp) - 2*n.query + 1):(len(temp) + n.query) ]
for(i in 1:(n.match+1)) {
matches[i,] = matches[i,] / matches[i,n.query]
}
temp = 100 * ( t(matches[,-c(1:n.query)]) - 1)
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
par(mar=c(2, 4, 2, 2))
matplot(temp, type='l',col='gray',lwd=2, lty='dotted', xlim=c(1,2.5*n.query),
main = paste('Pattern Prediction with', n.match, 'neighbours'),ylab='Normalized', xlab='')
lines(temp[,(n.match+1)], col='black',lwd=4)
points(rep(2*n.query,n.match), temp[2*n.query,1:n.match], pch=21, lwd=2, col='gray', bg='gray')
bt.plot.dot.label <- function(x, data, xfun, col='red') {
for(j in 1:len(xfun)) {
y = match.fun(xfun[[j]])(data)
points(x, y, pch=21, lwd=4, col=col, bg=col)
text(x, y, paste(names(xfun)[j], ':', round(y,1),'%'),
adj=c(-0.1,0), cex = 0.8, col=col,xpd=TRUE)
}
}
bt.plot.dot.label(2*n.query, temp[2*n.query,1:n.match],
list(Min=min,Max=max,Median=median,'Bot 25%'=function(x) quantile(x,0.25),'Top 75%'=function(x) quantile(x,0.75)))
bt.plot.dot.label(n.query, temp[n.query,(n.match+1)], list(Current=min))
dev.off()
temp = matrix( double(), nr=(n.match+4), 6)
rownames(temp) = c(1:n.match, spl('Current,Min,Average,Max'))
colnames(temp) = spl('Start,End,Return,Week,Month,Quarter')
temp[1:(n.match+1),'Return'] = matches[,2*n.query]/ matches[,n.query]
temp[1:(n.match+1),'Week'] = matches[,(2*n.query+5)]/ matches[,2*n.query]
temp[1:(n.match+1),'Month'] = matches[,(2*n.query+20)]/ matches[,2*n.query]
temp[1:(n.match+1),'Quarter'] = matches[,(2*n.query+60)]/ matches[,2*n.query]
index = spl('Return,Week,Month,Quarter')
temp['Min', index] = apply(temp[1:(n.match+1),index],2,min,na.rm=T)
temp['Average', index] = apply(temp[1:(n.match+1),index],2,mean,na.rm=T)
temp['Max', index] = apply(temp[1:(n.match+1),index],2,max,na.rm=T)
temp[] = plota.format(100*(temp-1),1,'','%')
temp['Current', 'Start'] = format(index(last(data,90)[1]), '%d %b %Y')
temp['Current', 'End'] = format(index(last(data,1)[1]), '%d %b %Y')
for(i in 1:n.match) {
temp[i, 'Start'] = format(index(data[min.index[i] - n.query + 1]), '%d %b %Y')
temp[i, 'End'] = format(index(data[min.index[i]]), '%d %b %Y')
}
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot.table(temp, smain='Match Number')
dev.off()
}
bt.matching.backtest.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY,^GSPC')
data <- new.env()
quantmod:::getSymbols(tickers, src = 'yahoo', from = '1950-01-01', env = data, auto.assign = T)
bt.prep(data, align='keep.all')
scale = as.double( data$prices$SPY['1993:01:29'] / data$prices$GSPC['1993:01:29'] )
hist = c(scale * data$prices$GSPC['::1993:01:28'], data$prices$SPY['1993:01:29::'])
month.ends = endpoints(hist, 'months')
month.ends = month.ends[month.ends > 0]
start.index = which(date.year(index(hist[month.ends])) == 1994)[1]
weight = hist * NA
for( i in start.index : len(month.ends) ) {
obj = bt.matching.find(hist[1:month.ends[i],], normalize.fn = normalize.first)
matches = bt.matching.overlay(obj)
n.match = len(obj$min.index)
n.query = len(obj$query)
month.ahead.forecast = matches[,(2*n.query+22)]/ matches[,2*n.query] - 1
weights = rep(1/n.match, n.match)
avg.direction = weighted.mean(month.ahead.forecast[1:n.match], w=weights)
temp = round(100*(obj$dist / obj$dist[1] - 1))
n.weight = max(temp) + 1
weights = (n.weight - temp) / ( n.weight * (n.weight+1) / 2)
weights = weights / sum(weights)
avg.direction = weighted.mean(month.ahead.forecast[1:n.match], w=weights)
weight[month.ends[i]] = 0
if( avg.direction > 0 ) weight[month.ends[i]] = 1
if( i %% 10 == 0) cat(i, '\n')
}
tickers = 'SPY'
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1950-01-01', env = data, auto.assign = T)
bt.prep(data, align='keep.all')
prices = data$prices
data$weight[] = 1
buy.hold = bt.run(data)
data$weight[] = NA
data$weight[] = weight['1993:01:29::']
capital = 100000
data$weight[] = (capital / prices) * bt.exrem(data$weight)
test = bt.run(data, type='share', capital=capital, trade.summary=T)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(test, buy.hold, trade.summary=T)
dev.off()
}
normalize.mean <- function(x) { x - mean(x) }
normalize.mean.sd <- function(x) { (x - mean(x)) / sd(x) }
normalize.first <- function(x) { x/as.double(x[1]) }
dist.euclidean <- function(x) { stats:::dist(x) }
bt.matching.find <- function
(
data,
n.query=90,
n.reference=252*10,
n.match=10,
normalize.fn = normalize.mean.sd,
dist.fn = dist.euclidean,
plot=FALSE,
plot.dist=FALSE,
layout = NULL,
main = NULL
)
{
data = last(data, n.reference)
reference = coredata(data)
n = len(reference)
query = reference[(n - n.query + 1):n]
reference = reference[1:(n - n.query)]
main = paste(main, join(format(range(index(data)[(n - n.query + 1):n]), '%d%b%Y'), ' - '))
n.query = len(query)
n.reference = len(reference)
dist.fn.name = ''
if(is.character(dist.fn)) {
dist.fn.name = paste('with',dist.fn)
dist.fn = get(dist.fn)
}
dist = rep(NA, n.reference)
query.normalized = match.fun(normalize.fn)(query)
for( i in n.query : n.reference ) {
window = reference[ (i - n.query + 1) : i]
window.normalized = match.fun(normalize.fn)(window)
dist[i] = match.fun(dist.fn)(rbind(query.normalized, window.normalized))
if( i %% 100 == 0) cat(i, '\n')
}
min.index = c()
temp = dist
temp[ temp > mean(dist, na.rm=T) ] = NA
for(i in 1:n.match) {
if(any(!is.na(temp))) {
index = which.min(temp)
min.index[i] = index
temp[max(0,index - 2*n.query) : min(n.reference,(index + n.query))] = NA
}
}
n.match = len(min.index)
if(plot) {
dates = index(data)[1:len(dist)]
if(is.null(layout)) {
if(plot.dist) layout(1:2) else layout(1)
}
par(mar=c(2, 4, 2, 2))
if(plot.dist) {
plot(dates, dist, type='l',col='gray', main=paste('Top Historical Matches for', main, dist.fn.name), ylab='Distance', xlab='')
abline(h = mean(dist, na.rm=T), col='darkgray', lwd=2)
points(dates[min.index], dist[min.index], pch=22, col='red', bg='red')
text(dates[min.index], dist[min.index], 1:n.match, adj=c(1,1), col='black',xpd=TRUE)
}
plota(data, type='l', col='gray', LeftMargin = 1,
main=iif(!plot.dist, paste('Top Historical Matches for', main), NULL)
)
plota.lines(last(data,90), col='blue')
for(i in 1:n.match) {
plota.lines(data[(min.index[i]-n.query + 1):min.index[i]], col='red')
}
text(index4xts(data)[min.index - n.query/2], reference[min.index - n.query/2], 1:n.match,
adj=c(1,-1), col='black',xpd=TRUE)
plota.legend(paste('Pattern: ', main, ',Match Number'),'blue,red')
}
return(list(min.index=min.index, dist=dist[min.index], query=query, reference=reference, dates = index(data), main = main))
}
bt.plot.dot.label <- function(x, data, xfun, col='red') {
for(j in 1:len(xfun)) {
y = match.fun(xfun[[j]])(data)
points(x, y, pch=21, lwd=4, col=col, bg=col)
text(x, y, paste(names(xfun)[j], ':', round(y,1),'%'),
adj=c(-0.1,0), cex = 0.8, col=col,xpd=TRUE)
}
}
bt.matching.overlay <- function
(
obj,
future=NA,
plot=FALSE,
plot.index=NA,
layout = NULL
)
{
min.index = obj$min.index
query = obj$query
reference = obj$reference
n.match = len(min.index)
n.query = len(query)
n.reference = len(reference)
matches = matrix(NA, nr=(n.match+1), nc=3*n.query)
temp = c(rep(NA, n.query), reference, query, future)
for(i in 1:n.match) {
matches[i,] = temp[ (min.index[i] - n.query + 1):(min.index[i] + 2*n.query) ]
}
matches[(n.match+1),] = temp[ (n.reference + 1):(n.reference + 3*n.query) ]
for(i in 1:(n.match+1)) {
matches[i,] = matches[i,] / iif(!is.na(matches[i,n.query]), matches[i,n.query], matches[i,(n.query+1)])
}
if(plot) {
temp = 100 * ( t(matches[,-c(1:n.query)]) - 1)
if(!is.na(plot.index[1])) temp=temp[plot.index,]
n = nrow(temp)
if(is.null(layout)) layout(1)
par(mar=c(4, 2, 2, 2))
matplot(temp, type='n',col='gray',lwd=2, lty='dotted', xlim=c(1, n + 0.15*n),
main = paste(obj$main,'Historical Pattern Prediction with', n.match, 'neighbours'),ylab='Normalized', xlab = 'Trading Days')
col=adjustcolor('yellow', 0.5)
rect(0, par('usr')[3],n.query, par('usr')[4], col=col, border=col)
box()
matlines(temp, col='gray',lwd=2, lty='dotted')
lines(temp[,(n.match+1)], col='black',lwd=4)
points(rep(n, n.match), temp[n, 1:n.match], pch=21, lwd=2, col='gray', bg='gray')
bt.plot.dot.label(n, temp[n, 1:n.match],
list(Min=min,Max=max,Median=median,'Bot 25%'=function(x) quantile(x,0.25),'Top 75%'=function(x) quantile(x,0.75)))
bt.plot.dot.label(n.query, temp[n.query,(n.match+1)], list(Current=min))
}
return(matches)
}
bt.matching.overlay.table <- function
(
obj,
matches,
weights=NA,
plot=FALSE,
layout = NULL
)
{
min.index = obj$min.index
query = obj$query
reference = obj$reference
dates = obj$dates
n.match = len(min.index)
n.query = len(query)
n.reference = len(reference)
if(is.na(weights)) weights = rep(1/n.match, n.match)
temp = matrix( double(), nr=(n.match + 4), 6)
rownames(temp) = c(1:n.match, spl('Current,Min,Average,Max'))
colnames(temp) = spl('Start,End,Return,Week,Month,Quarter')
temp[1:(n.match+1),'Return'] = matches[,2*n.query]/ matches[,n.query]
temp[1:(n.match+1),'Week'] = matches[,(2*n.query+5)]/ matches[,2*n.query]
temp[1:(n.match+1),'Month'] = matches[,(2*n.query+20)]/ matches[,2*n.query]
temp[1:(n.match+1),'Quarter'] = matches[,(2*n.query+60)]/ matches[,2*n.query]
index = spl('Return,Week,Month,Quarter')
temp['Min', index] = apply(temp[1:(n.match+0),index],2,min,na.rm=T)
temp['Average', index] = apply(temp[1:(n.match+0),index],2,weighted.mean,w=weights,na.rm=T)
temp['Max', index] = apply(temp[1:(n.match+0),index],2,max,na.rm=T)
temp[] = plota.format(100*(temp-1),1,'','%')
temp['Current', 'Start'] = format(dates[(n.reference+1)], '%d %b %Y')
temp['Current', 'End'] = format(dates[len(dates)], '%d %b %Y')
for(i in 1:n.match) {
temp[i, 'Start'] = format(dates[min.index[i] - n.query + 1], '%d %b %Y')
temp[i, 'End'] = format(dates[min.index[i]], '%d %b %Y')
}
if(plot) {
if(is.null(layout)) layout(1)
plot.table(temp, smain='Match Number')
}
return(temp)
}
dist.MOdist <- function(x) { MOdist(t(x)) }
dist.DTW <- function(x) { dtw(x[1,], x[2,])$distance }
bt.matching.dtw.test <- function()
{
load.packages('dtw')
idx = seq(0,6.28,len=100)
query = sin(idx)+runif(100)/10
reference = cos(idx)
alignment<-dtw(query, reference, keep=TRUE)
alignment$index1 = 1:100
alignment$index2 = 1:100
png(filename = 'plot0.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot(alignment,main='Example of 1 to 1 mapping', type='two',off=3)
dev.off()
alignment<-dtw(query, reference, keep=TRUE)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot(alignment,main='Example of 1 to many mapping (DTW)', type='two',off=3)
dev.off()
load.packages('quantmod')
tickers = 'SPY'
data = getSymbols(tickers, src = 'yahoo', from = '1950-01-01', auto.assign = F)
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
obj = bt.matching.find(Cl(data), normalize.fn = normalize.mean, dist.fn = 'dist.euclidean', plot=T)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
matches = bt.matching.overlay(obj, plot.index=1:90, plot=T)
dev.off()
png(filename = 'plot4.png', width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
matches = bt.matching.overlay(obj, plot=T, layout=T)
bt.matching.overlay.table(obj, matches, plot=T, layout=T)
dev.off()
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
obj = bt.matching.find(Cl(data), normalize.fn = normalize.mean, dist.fn = 'dist.DTW', plot=T)
dev.off()
png(filename = 'plot6.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
matches = bt.matching.overlay(obj, plot.index=1:90, plot=T)
dev.off()
png(filename = 'plot7.png', width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
matches = bt.matching.overlay(obj, plot=T, layout=T)
bt.matching.overlay.table(obj, matches, plot=T, layout=T)
dev.off()
png(filename = 'plot8.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
obj = bt.matching.find(Cl(data), normalize.fn = normalize.mean, dist.fn = 'dist.DTW1', plot=T)
dev.off()
png(filename = 'plot9.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
matches = bt.matching.overlay(obj, plot.index=1:90, plot=T)
dev.off()
png(filename = 'plot10.png', width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
matches = bt.matching.overlay(obj, plot=T, layout=T)
bt.matching.overlay.table(obj, matches, plot=T, layout=T)
dev.off()
png(filename = 'plot11.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
obj = bt.matching.find(Cl(data), normalize.fn = normalize.mean, dist.fn = 'dist.DDTW', plot=T)
dev.off()
png(filename = 'plot12.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
matches = bt.matching.overlay(obj, plot.index=1:90, plot=T)
dev.off()
png(filename = 'plot13.png', width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
matches = bt.matching.overlay(obj, plot=T, layout=T)
bt.matching.overlay.table(obj, matches, plot=T, layout=T)
dev.off()
}
derivative.est <- function(x) {
x = as.vector(x)
n = len(x)
d = (( x - mlag(x) ) + ( mlag(x,-1)- mlag(x) ) / 2) / 2
d[1] = d[2]
d[n] = d[(n-1)]
d
}
dist.DDTW <- function(x) {
y = x
x[1,] = derivative.est(x[1,])
x[2,] = derivative.est(x[2,])
alignment = dtw(x[1,], x[2,])
stats:::dist(rbind(y[1,alignment$index1],y[2,alignment$index2]))
}
dist.DTW1 <- function(x) {
alignment = dtw(x[1,], x[2,])
stats:::dist(rbind(x[1,alignment$index1],x[2,alignment$index2]))
}
bt.ddtw.test <- function()
{
load.packages('quantmod')
tickers = 'SPY'
data = getSymbols(tickers, src = 'yahoo', from = '1950-01-01', auto.assign = F)
load.packages('dtw')
query = as.vector(coredata(last(Cl(data['2011::2011']), 60)))
reference = as.vector(coredata(last(Cl(data['2010::2010']), 60)))
alignment = dtw(query, reference, keep=TRUE)
png(filename = 'plot1.ddtw.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot(alignment,main='DTW Alignment', type='two',off=20)
dev.off()
derivative.est <- function(x) {
x = as.vector(x)
n = len(x)
d = (( x - mlag(x) ) + ( mlag(x,-1)- mlag(x) ) / 2) / 2
d[1] = d[2]
d[n] = d[(n-1)]
d
}
alignment0 = dtw(derivative.est(query), derivative.est(reference), keep=TRUE)
alignment$index1 = alignment0$index1
alignment$index2 = alignment0$index2
png(filename = 'plot2.ddtw.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot(alignment,main='Derivative DTW Alignment', type='two',off=20)
dev.off()
}
bt.position.sizing.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1970::')
prices = data$prices
nperiods = nrow(prices)
models = list()
data$weight[] = 0
data$weight[] = 1
models$buy.hold = bt.run.share(data, clean.signal=T)
atr = bt.apply(data, function(x) ATR(HLC(x),20)[,'atr'])
data$weight[] = NA
capital = 100000
data$weight[] = (capital * 2/100) / (2 * atr)
max.allocation = capital / prices
data$weight[] = iif(data$weight > max.allocation, max.allocation,data$weight)
models$buy.hold.2atr = bt.run(data, type='share', capital=capital)
models = rev(models)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(models)
dev.off()
}
bt.volatility.position.sizing.test <- function()
{
load.packages('quantmod')
tickers = 'SPY'
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1994::')
models = list()
prices = data$prices
data$weight[] = 1
models$buy.hold = bt.run.share(data, clean.signal=T)
ret.log = bt.apply.matrix(prices, ROC, type='continuous')
hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 60)
data$weight[] = 0.1 / hist.vol
models$buy.hold.volatility.weighted = bt.run.share(data, clean.signal=T)
data$weight[] = 0.1 / hist.vol
rs = rowSums(data$weight)
data$weight[] = data$weight / iif(rs > 1, rs, 1)
models$buy.hold.volatility.weighted.100 = bt.run.share(data, clean.signal=T)
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]
data$weight[] = NA
data$weight[period.ends,] = 0.1 / hist.vol[period.ends,]
rs = rowSums(data$weight[period.ends,])
data$weight[period.ends,] = data$weight[period.ends,] / iif(rs > 1, rs, 1)
models$buy.hold.volatility.weighted.100.monthly = bt.run.share(data, clean.signal=T)
png(filename = 'plot1.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)
mtext('Cumulative Performance', side = 2, line = 1)
dev.off()
png(filename = 'plot2.png', width = 1600, height = 1000, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(rev(models))
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover', plotX = F, label='both')
dev.off()
models = models[c('buy.hold' ,'buy.hold.volatility.weighted.100.monthly')]
calc = c("close", "garman.klass", "parkinson", "rogers.satchell", "gk.yz", "yang.zhang")
ohlc = OHLC(data$SPY)
for(icalc in calc) {
vol = volatility(ohlc, calc = icalc, n = 60, N = 252)
data$weight[] = NA
data$weight[period.ends,] = 0.1 / vol[period.ends,]
rs = rowSums(data$weight[period.ends,])
data$weight[period.ends,] = data$weight[period.ends,] / iif(rs > 1, rs, 1)
models[[icalc]] = bt.run.share(data, clean.signal=T)
}
png(filename = 'plot4.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)
mtext('Cumulative Performance', side = 2, line = 1)
dev.off()
png(filename = 'plot5.png', width = 1600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(models)
dev.off()
models = list()
sma.fast = SMA(prices, 50)
sma.slow = SMA(prices, 200)
weight = iif(sma.fast >= sma.slow, 1, -1)
data$weight[] = weight
models$ma.crossover = bt.run.share(data, clean.signal=T)
ret.log = bt.apply.matrix(models$ma.crossover$equity, ROC, type='continuous')
hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 60)
data$weight[] = NA
data$weight[period.ends,] = (0.1 / hist.vol[period.ends,]) * weight[period.ends,]
rs = rowSums(data$weight[period.ends,])
data$weight[period.ends,] = data$weight[period.ends,] / iif(abs(rs) > 1, abs(rs), 1)
models$ma.crossover.volatility.weighted.100.monthly = bt.run.share(data, clean.signal=T)
png(filename = 'plot6.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)
mtext('Cumulative Performance', side = 2, line = 1)
dev.off()
png(filename = 'plot7.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(rev(models))
dev.off()
tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='1994::')
prices = data$prices
n = ncol(prices)
models = list()
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]
data$weight[] = NA
data$weight[period.ends,] = ntop(prices[period.ends,], n)
data$weight[1:200,] = NA
models$equal.weight = bt.run.share(data, clean.signal=F)
sma = bt.apply.matrix(prices, SMA, 200)
weight = ntop(prices, n) * (prices > sma)
data$weight[] = NA
data$weight[period.ends,] = weight[period.ends,]
models$timing = bt.run.share(data, clean.signal=F)
ret.log = bt.apply.matrix(models$timing$equity, ROC, type='continuous')
hist.vol = bt.apply.matrix(ret.log, runSD, n = 60)
hist.vol = sqrt(252) * as.vector(hist.vol)
data$weight[] = NA
data$weight[period.ends,] = (0.1 / hist.vol[period.ends]) * weight[period.ends,]
rs = rowSums(data$weight)
data$weight[] = data$weight / iif(rs > 1, rs, 1)
data$weight[1:200,] = NA
models$timing.volatility.weighted.100.monthly = bt.run.share(data, clean.signal=T)
png(filename = 'plot8.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)
mtext('Cumulative Performance', side = 2, line = 1)
dev.off()
png(filename = 'plot9.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(rev(models))
dev.off()
}
bt.rolling.cor.test <- function()
{
load.packages('quantmod')
tickers = sp500.components()$tickers
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1970::')
spy = getSymbols('SPY', src = 'yahoo', from = '1970-01-01', auto.assign = F)
ret.spy = coredata( Cl(spy) / mlag(Cl(spy))-1 )
prices = data$prices['1993:01:29::']
nperiods = nrow(prices)
ret = prices / mlag(prices) - 1
ret = coredata(ret)
index = which((count(t(prices)) > 100 ))
index = index[-c(1:252)]
avg.cor = NA * prices[,1]
avg.cor.spy = NA * prices[,1]
for(i in index) {
hist = ret[ (i- 252 +1):i, ]
hist = hist[ , count(hist)==252, drop=F]
nleft = ncol(hist)
correlation = cor(hist, use='complete.obs',method='pearson')
avg.cor[i,] = (sum(correlation) - nleft) / (nleft*(nleft-1))
avg.cor.spy[i,] = sum(cor(ret.spy[ (i- 252 +1):i, ], hist, use='complete.obs',method='pearson')) / nleft
if( i %% 100 == 0) cat(i, 'out of', nperiods, '\n')
}
png(filename = 'plot.sp500.cor.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
sma50 = SMA(Cl(spy), 50)
sma200 = SMA(Cl(spy), 200)
cols = col.add.alpha(spl('green,red'),50)
plota.control$col.x.highlight = iif(sma50 > sma200, cols[1], cols[2])
highlight = sma50 > sma200 | sma50 < sma200
plota(avg.cor, type='l', ylim=range(avg.cor, avg.cor.spy, na.rm=T), x.highlight = highlight,
main='Average 252 day Pairwise Correlation for stocks in SP500')
plota.lines(avg.cor.spy, type='l', col='blue')
plota.legend('Pairwise Correlation,Correlation with SPY,SPY 50-day SMA > 200-day SMA,SPY 50-day SMA < 200-day SMA',
c('black,blue',cols))
dev.off()
}
bt.volatility.quantiles.test <- function()
{
load.packages('quantmod')
tickers = sp500.components()$tickers
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
rm.index = which( sapply(ls(data), function(x) nrow(data[[x]])) < 1000 )
rm(list=names(rm.index), envir=data)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1994::')
data.spy <- new.env()
getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data.spy, auto.assign = T)
bt.prep(data.spy, align='keep.all', dates='1994::')
prices = data$prices
nperiods = nrow(prices)
n = ncol(prices)
models = list()
data.spy$weight[] = NA
data.spy$weight[] = 1
models$spy = bt.run(data.spy)
data$weight[] = NA
data$weight[] = ntop(prices, 500)
models$equal.weight = bt.run(data)
period.ends = endpoints(prices, 'weeks')
period.ends = period.ends[period.ends > 0]
p = bt.apply.matrix(coredata(prices), ifna.prev)
ret = p / mlag(p) - 1
sd252 = bt.apply.matrix(ret, runSD, 252)
n.quantiles=5
start.t = which(period.ends >= (252+2))[1]
quantiles = weights = p * NA
for( t in start.t:len(period.ends) ) {
i = period.ends[t]
factor = sd252[i,]
ranking = ceiling(n.quantiles * rank(factor, na.last = 'keep','first') / count(factor))
quantiles[i,] = ranking
weights[i,] = 1/tapply(rep(1,n), ranking, sum)[ranking]
}
quantiles = ifna(quantiles,0)
for( i in 1:n.quantiles) {
temp = weights * NA
temp[period.ends,] = 0
temp[quantiles == i] = weights[quantiles == i]
data$weight[] = NA
data$weight[] = temp
models[[ paste('Q',i,sep='_') ]] = bt.run(data, silent = T)
}
rowSums(models$Q_2$weight,na.rm=T)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(models)
dev.off()
}
bt.fa.value.quantiles.test <- function()
{
load.packages('quantmod')
tickers = sp500.components()$tickers
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
rm.index = which( sapply(ls(data), function(x) nrow(data[[x]])) < 1000 )
rm(list=names(rm.index), envir=data)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1994::')
tickers = data$symbolnames
data.spy <- new.env()
getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data.spy, auto.assign = T)
bt.prep(data.spy, align='keep.all', dates='1994::')
prices = data$prices
nperiods = nrow(prices)
n = ncol(prices)
models = list()
data.spy$weight[] = NA
data.spy$weight[] = 1
models$spy = bt.run(data.spy)
data$weight[] = NA
data$weight[] = ntop(prices, n)
models$equal.weight = bt.run(data)
periodicity = 'weeks'
factors = get.fama.french.data('F-F_Research_Data_Factors', periodicity = periodicity,download = F, clean = F)
period.ends = endpoints(data$prices, periodicity)
period.ends = period.ends[period.ends > 0]
data.fa <- new.env()
for(i in tickers) data.fa[[i]] = data[[i]][period.ends,]
data.fa$factors = factors$data / 100
bt.prep(data.fa, align='remove.na')
index = match( index(data.fa$prices), index(data$prices) )
measure = data$prices[ index, ]
for(i in tickers) {
cat(i, '\n')
obj = factor.rolling.regression(data.fa, i, 36, silent=T)
measure[,i] = coredata(obj$fl$estimate$HML)
}
n.quantiles=5
start.t = 1+36
quantiles = weights = coredata(measure) * NA
for( t in start.t:nrow(weights) ) {
factor = as.vector(coredata(measure[t,]))
ranking = ceiling(n.quantiles * rank(factor, na.last = 'keep','first') / count(factor))
quantiles[t,] = ranking
weights[t,] = 1/tapply(rep(1,n), ranking, sum)[ranking]
}
quantiles = ifna(quantiles,0)
for( i in 1:n.quantiles) {
temp = weights * NA
temp[] = 0
temp[quantiles == i] = weights[quantiles == i]
data$weight[] = NA
data$weight[index,] = temp
models[[ paste('Q',i,sep='_') ]] = bt.run(data, silent = T)
}
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(models)
dev.off()
}
factor.rolling.regression <- function(
data,
ticker = data$symbolnames[-grep('factor', data$symbolnames)],
window.len = 36,
silent = F,
custom.stats.fn = NULL
)
{
ticker = ticker[1]
prices = data$prices
nperiods = nrow(prices)
dates = index(data$prices)
hist.returns = ROC(prices[,ticker], type = 'discrete')
hist.returns = hist.returns - data$factors$RF
yout = hist.returns
y = coredata(yout)
xout = data$factors[, -which(names(data$factors) == 'RF')]
x = coredata(xout)
ok.index = !(is.na(y) | (rowSums(is.na(x)) > 0))
fit = ols(cbind(1,x[ok.index,]),y[ok.index], T)
est = fit$coefficients
std.err = fit$seb
r2 = fit$r.squared
fl.all = list()
fl.all$estimate = c(est, r2)
fl.all$std.error = c(std.err, NA)
colnames = c('alpha', colnames(x), 'R2')
estimate = make.xts(matrix(NA, nr = nperiods, len(colnames)), dates)
colnames(estimate) = colnames
fl = list()
fl$estimate = estimate
fl$std.error = estimate
if( !is.null(custom.stats.fn) ) {
temp = match.fun(custom.stats.fn)(cbind(1,x), y, fit)
fl$custom = make.xts(matrix(NA, nr = nperiods, len(temp)), dates)
}
for( i in window.len:nperiods ) {
window.index = (i - window.len + 1) : i
if(all(!is.na(y[window.index]))) {
xtemp = cbind(1,x[window.index,])
ytemp = y[window.index]
fit = ols(xtemp, ytemp, T)
est = fit$coefficients
std.err = fit$seb
r2 = fit$r.squared
fl$estimate[i,] = c(est, r2)
fl$std.error[i,] = c(std.err, NA)
if( !is.null(custom.stats.fn) )
fl$custom[i,] = match.fun(custom.stats.fn)(xtemp, ytemp, fit)
}
if( i %% 10 == 0) if(!silent) cat(i, '\n')
}
return(list(fl.all = fl.all, fl = fl, window.len=window.len,
y=yout, x=xout, RF=data$factors$RF))
}
factor.rolling.regression.detail.plot <- function(obj) {
n = ncol(obj$fl$estimate)
dates = index(obj$fl$estimate)
layout(matrix(1:(2*n), nc=2, byrow=T))
for(i in 1:n) {
est = obj$fl$estimate[,i]
est.std.error = ifna(obj$fl$std.error[,i], 0)
plota(est,
ylim = range( c(
range(est + est.std.error, na.rm=T),
range(est - est.std.error, na.rm=T)
)))
polygon(c(dates,rev(dates)),
c(coredata(est + est.std.error),
rev(coredata(est - est.std.error))),
border=NA, col=col.add.alpha('red',50))
est = obj$fl.all$estimate[i]
est.std.error = obj$fl.all$std.error[i]
polygon(c(range(dates),rev(range(dates))),
c(rep(est + est.std.error,2),
rep(est - est.std.error,2)),
border=NA, col=col.add.alpha('blue',50))
abline(h=0, col='blue', lty='dashed')
abline(h=est, col='blue')
plota.lines(obj$fl$estimate[,i], type='l', col='red')
par(mar = c(4,3,2,1))
hist(obj$fl$estimate[,i], col='red', border='gray', las=1,
xlab='', ylab='', main=colnames(obj$fl$estimate)[i])
abline(v=obj$fl.all$estimate[i], col='blue', lwd=2)
}
}
factor.rolling.regression.style.plot <- function(obj,
xfactor='HML', yfactor='SMB',
xlim = c(-1.5, 1.5), ylim = c(-0.5, 1.5)
) {
i = which(colnames(obj$fl$estimate) == xfactor)
x = coredata(obj$fl$estimate[,i])
x.e = ifna(coredata(obj$fl$std.error[,i]), 0)
x.all = obj$fl.all$estimate[i]
x.all.e = obj$fl.all$std.error[i]
xlab = colnames(obj$fl$estimate)[i]
i = which(colnames(obj$fl$estimate) == yfactor)
y = coredata(obj$fl$estimate[,i])
y.e = ifna(coredata(obj$fl$std.error[,i]), 0)
y.all = obj$fl.all$estimate[i]
y.all.e = obj$fl.all$std.error[i]
ylab = colnames(obj$fl$estimate)[i]
layout(1)
plot(x,y, xlab=xlab, ylab = ylab, type='n', las=1,
xlim = range(c(x + x.e, x - x.e, xlim), na.rm=T),
ylim = range(c(y + y.e, y - y.e, ylim), na.rm=T),
main = paste('Style, last =', ylab, round(last(y),2), xlab, round(last(x),2))
)
grid()
abline(h=0)
abline(v=0)
col = col.add.alpha('pink',250)
rect(x - x.e, y - y.e, x + x.e, y + y.e, col=col, border=NA)
points(x,y, col='red', pch=20)
points(last(x),last(y), col='black', pch=3)
points(x.all,y.all, col='blue', pch=15)
legend('topleft', spl('Estimates,Last estimate,Overall estimate'),
pch = c(20,3,15),
col = spl('red,black,blue'),
pt.bg = spl('red,black,blue'),
bty='n'
)
}
factor.rolling.regression.bt.plot <- function(obj) {
ticker = colnames(obj$y)
n = ncol(obj$fl$estimate)-1
nperiods = nrow(obj$fl$estimate)
ret = cbind(obj$RF, obj$y, 1, obj$x)
colnames(ret)[1:3] = spl('RF,fund,alpha')
prices = bt.apply.matrix(1+ifna(ret,0),cumprod)
data <- new.env()
data$symbolnames = colnames(prices)
for(i in colnames(prices)) {
data[[i]] = prices[,i]
colnames(data[[i]]) = 'Close'
}
bt.prep(data, align='keep.all')
models = list()
data$weight[] = NA
data$weight$fund = 1
data$weight$RF = 1
data$weight[1:obj$window.len,] = NA
models[[ticker]] = bt.run.share(data, clean.signal = F)
data$weight[] = NA
data$weight[,3:(n+2)] = t(repmat(obj$fl.all$estimate[1:n], 1, nperiods))
data$weight$RF = 1
data$weight[1:obj$window.len,] = NA
models$all.alpha = bt.run.share(data, clean.signal = F)
data$weight[] = NA
data$weight[,3:(n+2)] = t(repmat(obj$fl.all$estimate[1:n], 1, nperiods))
data$weight$RF = 1
data$weight$alpha = NA
data$weight[1:obj$window.len,] = NA
models$all = bt.run.share(data, clean.signal = F)
data$weight[] = NA
data$weight[,3:(n+2)] = obj$fl$estimate[,1:n]
data$weight$RF = 1
data$weight[1:obj$window.len,] = NA
models$est.alpha = bt.run.share(data, clean.signal = F)
data$weight[] = NA
data$weight[,3:(n+2)] = obj$fl$estimate[,1:n]
data$weight$RF = 1
data$weight$alpha = NA
data$weight[1:obj$window.len,] = NA
models$est = bt.run.share(data, clean.signal = F)
layout(1)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)
mtext('Cumulative Performance', side = 2, line = 1)
}
three.factor.rolling.regression <- function() {
load.packages('quantmod')
tickers = 'VISVX'
periodicity = 'weeks'
periodicity = 'months'
data <- new.env()
quantmod::getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) {
temp = adjustOHLC(data[[i]], use.Adjusted=T)
period.ends = endpoints(temp, periodicity)
period.ends = period.ends[period.ends > 0]
if(periodicity == 'months') {
monthly.dates = as.Date(paste(format(index(temp)[period.ends], '%Y%m'),'01',sep=''), '%Y%m%d')
data[[i]] = make.xts(coredata(temp[period.ends,]), monthly.dates)
} else
data[[i]] = temp[period.ends,]
}
data.fund = data[[tickers]]
factors = get.fama.french.data('F-F_Research_Data_Factors', periodicity = periodicity,download = T, clean = F)
data <- new.env()
data[[tickers]] = data.fund
data$factors = factors$data / 100
bt.prep(data, align='remove.na', dates='1994::')
obj = factor.rolling.regression(data, tickers, 36)
png(filename = 'plot1.png', width = 600, height = 1200, units = 'px', pointsize = 12, bg = 'white')
factor.rolling.regression.detail.plot(obj)
dev.off()
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
factor.rolling.regression.style.plot(obj)
dev.off()
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
factor.rolling.regression.bt.plot(obj)
dev.off()
factors = get.fama.french.data('F-F_Research_Data_Factors', periodicity = periodicity,download = F, clean = F)
factors.extra = get.fama.french.data('F-F_Momentum_Factor', periodicity = periodicity,download = T, clean = F)
factors$data = merge(factors$data, factors.extra$data)
data <- new.env()
data[[tickers]] = data.fund
data$factors = factors$data / 100
bt.prep(data, align='remove.na', dates='1994::')
obj = factor.rolling.regression(data, tickers, 36)
png(filename = 'plot4.png', width = 600, height = 1200, units = 'px', pointsize = 12, bg = 'white')
factor.rolling.regression.detail.plot(obj)
dev.off()
png(filename = 'plot5.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
factor.rolling.regression.style.plot(obj)
dev.off()
png(filename = 'plot6.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
factor.rolling.regression.style.plot(obj, xfactor='HML', yfactor='Mom')
dev.off()
png(filename = 'plot7.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
factor.rolling.regression.bt.plot(obj)
dev.off()
}
your.own.factor.rolling.regression <- function() {
load.packages('quantmod')
tickers = spl('EEM,SPY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')
prices = data$prices
periodicity = 'weeks'
period.ends = endpoints(prices, periodicity)
period.ends = period.ends[period.ends > 0]
hist.returns = ROC(prices[period.ends,], type = 'discrete')
hist.returns = na.omit(hist.returns)
EEM_SPY = hist.returns$EEM - hist.returns$SPY
colnames(EEM_SPY) = 'EEM_SPY'
write.xts(EEM_SPY, 'EEM_SPY.csv')
load.packages('quantmod')
tickers = 'VISVX'
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) {
temp = adjustOHLC(data[[i]], use.Adjusted=T)
period.ends = endpoints(temp, periodicity)
period.ends = period.ends[period.ends > 0]
data[[i]] = temp[period.ends,]
}
data.fund = data[[tickers]]
factors = get.fama.french.data('F-F_Research_Data_Factors', periodicity = periodicity,download = F, clean = F)
factors.extra = 100 * read.xts('EEM_SPY.csv')
factors$data = merge(factors$data, factors.extra, join='inner')
data <- new.env()
data[[tickers]] = data.fund
data$factors = factors$data / 100
bt.prep(data, align='remove.na')
load.packages('psych')
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
pairs.panels(coredata(data$factors))
dev.off()
obj = factor.rolling.regression(data, tickers, 36)
png(filename = 'plot2.png', width = 600, height = 1200, units = 'px', pointsize = 12, bg = 'white')
factor.rolling.regression.detail.plot(obj)
dev.off()
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
factor.rolling.regression.style.plot(obj)
dev.off()
png(filename = 'plot4.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
factor.rolling.regression.style.plot(obj, xfactor='HML', yfactor='EEM_SPY')
dev.off()
png(filename = 'plot5.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
factor.rolling.regression.bt.plot(obj)
dev.off()
}
bt.one.month.test <- function()
{
load.packages('quantmod')
tickers = sp500.components()$tickers
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
rm.index = which( sapply(ls(data), function(x) nrow(data[[x]])) < 1000 )
rm(list=names(rm.index), envir=data)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1994::')
tickers = data$symbolnames
data.spy <- new.env()
getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data.spy, auto.assign = T)
bt.prep(data.spy, align='keep.all', dates='1994::')
prices = data$prices
n = ncol(prices)
periodicity = 'months'
period.ends = endpoints(data$prices, periodicity)
period.ends = period.ends[period.ends > 0]
prices = prices[period.ends, ]
models = list()
n.skip = 36
n.skip = 2
data.spy$weight[] = NA
data.spy$weight[] = 1
data.spy$weight[1:period.ends[n.skip],] = NA
models$spy = bt.run(data.spy)
data$weight[] = NA
data$weight[period.ends,] = ntop(prices, n)
data$weight[1:period.ends[n.skip],] = NA
models$equal.weight = bt.run(data)
one.month = coredata(prices / mlag(prices))
models = c(models,
bt.make.quintiles(one.month, data, period.ends, start.t=1 + n.skip, prefix='M1_'))
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models[spl('spy,equal.weight,spread')])
dev.off()
}
factor.rolling.regression.custom.stats <- function(x,y,fit) {
n = len(y)
e = y - x %*% fit$coefficients
se = sd(e)
return(c(e[n], e[n]/se))
}
bt.make.quintiles <- function(
position.score,
data,
period.ends,
n.quantiles = 5,
start.t = 2,
prefix = ''
)
{
n = ncol(position.score)
position.score = coredata(position.score)
quantiles = weights = position.score * NA
for( t in start.t:nrow(weights) ) {
factor = as.vector(position.score[t,])
ranking = ceiling(n.quantiles * rank(factor, na.last = 'keep','first') / count(factor))
quantiles[t,] = ranking
weights[t,] = 1/tapply(rep(1,n), ranking, sum)[ranking]
}
quantiles = ifna(quantiles,0)
temp = weights * NA
models = list()
for( i in 1:n.quantiles) {
temp[] = 0
temp[quantiles == i] = weights[quantiles == i]
data$weight[] = NA
data$weight[period.ends,] = temp
models[[ paste(prefix,'Q',i,sep='') ]] = bt.run(data, silent = T)
}
temp[] = 0
temp[quantiles == 1] = weights[quantiles == 1]
temp[quantiles == n.quantiles] = -weights[quantiles == n.quantiles]
data$weight[] = NA
data$weight[period.ends,] = temp
models$spread = bt.run(data, silent = T)
return(models)
}
bt.fa.one.month.test <- function()
{
load.packages('quantmod')
info = sp500.components()
tickers = info$tickers
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
rm.index = which( sapply(ls(data), function(x) nrow(data[[x]])) < 1000 )
rm(list=names(rm.index), envir=data)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1994::')
tickers = data$symbolnames
sector = info$sector[match(tickers, info$tickers)]
data.spy <- new.env()
getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data.spy, auto.assign = T)
bt.prep(data.spy, align='keep.all', dates='1994::')
prices = data$prices
n = ncol(prices)
periodicity = 'months'
period.ends = endpoints(data$prices, periodicity)
period.ends = period.ends[period.ends > 0]
prices = prices[period.ends, ]
models = list()
n.skip = 36
data.spy$weight[] = NA
data.spy$weight[] = 1
data.spy$weight[1:period.ends[n.skip],] = NA
models$spy = bt.run(data.spy)
data$weight[] = NA
data$weight[period.ends,] = ntop(prices, n)
data$weight[1:period.ends[n.skip],] = NA
models$equal.weight = bt.run(data)
factors = get.fama.french.data('F-F_Research_Data_Factors', periodicity = periodicity,download = F, clean = F)
map = match(format(index(factors$data), '%Y%m'), format(index(prices), '%Y%m'))
dates = index(factors$data)
dates[!is.na(map)] = index(prices)[na.omit(map)]
index(factors$data) = as.Date(dates)
data.fa <- new.env()
for(i in tickers) data.fa[[i]] = data[[i]][period.ends, ]
data.fa$factors = factors$data / 100
bt.prep(data.fa, align='remove.na')
index = match( index(data.fa$prices), index(data$prices) )
prices = data$prices[index, ]
temp = NA * prices
factors	= list()
factors$last.e = temp
factors$last.e_s = temp
for(i in tickers) {
cat(i, '\n')
obj = factor.rolling.regression(data.fa, i, 36, silent=T,
factor.rolling.regression.custom.stats)
for(j in 1:len(factors))
factors[[j]][,i] = obj$fl$custom[,j]
}
factors$one.month = coredata(prices / mlag(prices))
load(file='data.ff.factors.Rdata')
quantiles = list()
for(name in names(factors)) {
cat(name, '\n')
quantiles[[name]] = bt.make.quintiles(factors[[name]], data, index, start.t =  1+36, prefix=paste(name,'_',sep=''))
}
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(quantiles$one.month$spread,quantiles$last.e$spread,quantiles$last.e_s$spread)
dev.off()
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(quantiles$one.month$spread,quantiles$last.e$spread,quantiles$last.e_s$spread)
dev.off()
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(	quantiles$last.e )
dev.off()
png(filename = 'plot4.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(	quantiles$last.e_s )
dev.off()
}
bt.fa.sector.one.month.test <- function()
{
load.packages('quantmod')
info = sp500.components()
tickers = info$tickers
data <- new.env()
for(i in tickers) try(getSymbols(i, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T), TRUE)
rm.index = which( sapply(ls(data), function(x) nrow(data[[x]])) < 1000 )
rm(list=names(rm.index), envir=data)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1994::')
tickers = data$symbolnames
sector = info$sector[match(tickers, info$tickers)]
data.spy <- new.env()
getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data.spy, auto.assign = T)
bt.prep(data.spy, align='keep.all', dates='1994::')
save(data, data.spy, tickers, sector, file='data.sp500.components.Rdata')
prices = data$prices
n = ncol(prices)
periodicity = 'months'
period.ends = endpoints(data$prices, periodicity)
period.ends = period.ends[period.ends > 0]
prices = prices[period.ends, ]
models = list()
n.skip = 36
data.spy$weight[] = NA
data.spy$weight[] = 1
data.spy$weight[1:period.ends[n.skip],] = NA
models$spy = bt.run(data.spy)
data$weight[] = NA
data$weight[period.ends,] = ntop(prices, n)
data$weight[1:period.ends[n.skip],] = NA
models$equal.weight = bt.run(data)
factors = get.fama.french.data('F-F_Research_Data_Factors', periodicity = periodicity,download = T, clean = F)
if(periodicity == 'months') {
map = match(format(index(factors$data), '%Y%m'), format(index(prices), '%Y%m'))
dates = index(factors$data)
dates[!is.na(map)] = index(prices)[na.omit(map)]
index(factors$data) = as.Date(dates)
}
data.fa <- new.env()
for(i in tickers) data.fa[[i]] = data[[i]][period.ends, ]
data.fa$factors = factors$data / 100
bt.prep(data.fa, align='remove.na')
index = match( index(data.fa$prices), index(data$prices) )
prices = data$prices[index, ]
temp = NA * prices
factors	= list()
factors$last.e = temp
factors$last.e_s = temp
for(i in tickers) {
cat(i, '\n')
obj = factor.rolling.regression(data.fa, i, 36, silent=T,
factor.rolling.regression.custom.stats)
for(j in 1:len(factors))
factors[[j]][,i] = obj$fl$custom[,j]
}
nlag = iif(periodicity == 'months', 1, 4)
factors$one.month = coredata(prices / mlag(prices, nlag))
save(factors, file='data.ff.factors.Rdata')
quantiles = list()
for(name in names(factors)) {
cat(name, '\n')
quantiles[[name]] = bt.make.quintiles(factors[[name]], data, index, start.t =  1+36, prefix=paste(name,'_',sep=''))
}
quantiles.sn = list()
for(name in names(factors)) {
cat(name, '\n')
quantiles.sn[[name]] = bt.make.quintiles.sector(sector, factors[[name]], data, index, start.t =  1+36, prefix=paste(name,'_',sep=''))
}
save(quantiles, quantiles.sn, file='model.quantiles.Rdata')
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(quantiles$one.month$spread,
quantiles$last.e$spread, quantiles$last.e_s$spread,
quantiles.sn$one.month$spread.sn,
quantiles.sn$last.e$spread.sn, quantiles.sn$last.e_s$spread.sn)
dev.off()
png(filename = 'plot2.png', width = 800, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(quantiles$one.month$spread,
quantiles$last.e$spread, quantiles$last.e_s$spread,
quantiles.sn$one.month$spread.sn,
quantiles.sn$last.e$spread.sn, quantiles.sn$last.e_s$spread.sn)
dev.off()
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(	quantiles.sn$one.month )
dev.off()
png(filename = 'plot4.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(	quantiles.sn$last.e_s )
dev.off()
png(filename = 'plot1a.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(c(models,quantiles$one.month))
dev.off()
png(filename = 'plot2a.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(c(models,quantiles$one.month$spread))
dev.off()
png(filename = 'plot1b.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(quantiles$one.month$spread,quantiles$last.e$spread,quantiles$last.e_s$spread)
dev.off()
png(filename = 'plot2b.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(quantiles$one.month$spread,quantiles$last.e$spread,quantiles$last.e_s$spread)
dev.off()
png(filename = 'plot3b.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(	quantiles$last.e )
dev.off()
png(filename = 'plot4b.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(	quantiles$last.e_s )
dev.off()
}
bt.make.quintiles.sector <- function(
sector,
position.score,
data,
period.ends,
n.quantiles = 5,
start.t = 2,
prefix = ''
)
{
temp = factor(sector)
sector.names = levels(temp)
n.sectors = len(sector.names)
sectors = matrix(unclass(temp),nr=nrow(position.score),nc=ncol(position.score),byrow=T)
position.score = coredata(position.score)
quantiles = weights = position.score * NA
for( s in 1:n.sectors) {
for( t in start.t:nrow(weights) ) {
index = sectors[t,] == s
n = sum(index)
if(n > 3*n.quantiles) {
factor = as.vector(position.score[t, index])
ranking = ceiling(n.quantiles * rank(factor, na.last = 'keep','first') / count(factor))
quantiles[t, index] = ranking
weights[t, index] = 1/tapply(rep(1,n), ranking, sum)[ranking]
}
}
}
quantiles = ifna(quantiles,0)
long = weights * NA
short = weights * NA
models = list()
for( s in 1:n.sectors) {
long[] = 0
long[quantiles == 1 & sectors == s] = weights[quantiles == 1 & sectors == s]
long = long / rowSums(long,na.rm=T)
short[] = 0
short[quantiles == n.quantiles & sectors == s] = weights[quantiles == n.quantiles & sectors == s]
short = short / rowSums(short,na.rm=T)
data$weight[] = NA
data$weight[period.ends,] = long - short
models[[ paste(prefix,'spread.',sector.names[s], sep='') ]]	= bt.run(data, silent = T)
}
if(F) {
load.packages('abind')
model.prices = abind(lapply(models, function(m) m$equity), along=2)
model.prices = model.prices[period.ends,]
model.returns = model.prices / mlag(model.prices)-1
model.score = bt.apply.matrix(model.returns, SMA, 6)
model.vol = bt.apply.matrix(model.returns, runSD, 6)
top = ntop(model.score, 3)
top = top / model.vol
top = top / rowSums(top, na.rm=T)
top = ifna(top,0)
n = ncol(position.score)
nperiods = nrow(position.score)
long[] = 0
short[] = 0
for( s in 1:n.sectors) {
score = matrix(top[,s], nr = nperiods, n)
long[quantiles == 1 & sectors == s] = (weights * score)[quantiles == 1 & sectors == s]
short[quantiles == n.quantiles & sectors == s] = (weights * score)[quantiles == n.quantiles & sectors == s]
}
long = long / rowSums(long,na.rm=T)
short = short / rowSums(short,na.rm=T)
data$weight[] = NA
data$weight[period.ends,] = long - short
models$spread.sn.top3 = bt.run(data, silent = T)
}
long[] = 0
long[quantiles == 1] = weights[quantiles == 1]
long = long / rowSums(long,na.rm=T)
short[] = 0
short[quantiles == n.quantiles] = weights[quantiles == n.quantiles]
short = short / rowSums(short,na.rm=T)
data$weight[] = NA
data$weight[period.ends,] = long - short
models$spread.sn = bt.run(data, silent = T)
return(models)
}
forecast.helper <- function(fit, h=10, level = c(80,95)) {
out = try( forecast(fit, h=h, level=level), silent=TRUE)
if (class(out)[1] != 'try-error') {
out = data.frame(out)
} else {
temp = data.frame(predict(fit, n.ahead=h, doplot=F))
pred = temp[,1]
se = temp[,2]
qq = qnorm(0.5 * (1 + level/100))
out = matrix(NA, nr=h, nc=1+2*len(qq))
out[,1] = pred
for(i in 1:len(qq))
out[,(2*i):(2*i+1)] = c(pred - qq[i] * se, pred + qq[i] * se)
colnames(out) = c('Point.Forecast', matrix(c(paste('Lo', level, sep='.'), paste('Hi', level, sep='.')), nr=2, byrow=T))
out = data.frame(out)
}
return(out)
}
forecast2xts <- function(data, forecast) {
h = nrow(forecast)
dates = as.Date(index(data))
new.dates = seq(last(dates)+1, last(dates) + 2*365, by='day')
rm.index = date.dayofweek(new.dates) == 6 | date.dayofweek(new.dates) == 0
new.dates = new.dates[!rm.index]
new.dates = new.dates[1:h]
return(make.xts(forecast, new.dates))
}
forecast.plot <- function(data, forecast, ...) {
out = forecast2xts(data, forecast)
plota(c(data, out[,1]*NA), type='l',
ylim = range(data,out,na.rm=T), ...)
new.dates = index4xts(out)
temp = coredata(out)
n = (ncol(out) %/% 2)
for(i in n : 1) {
polygon(c(new.dates,rev(new.dates)),
c(temp[,(2*i)], rev(temp[,(2*i+1)])),
border=NA, col=col.add.alpha(i+2,150))
}
plota.lines(out[,1], col='red')
labels = c('Data,Forecast', paste(gsub('Lo.', '', colnames(out)[2*(1:n)]), '%', sep=''))
plota.legend(labels, fill = c('black,red',col.add.alpha((1:n)+2, 150)))
}
bt.forecast.dashboard <- function() {
load.packages('quantmod')
tickers = spl('SPY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1990-01-01', env = data, auto.assign = T)
bt.prep(data, align='remove.na')
load.packages('forecast,fGarch,fArma')
sample = last(data$prices$SPY, 200)
ts.sample = ts(sample, frequency = 12)
models = list(
garch = garchFit(~arma(1,15)+garch(1,1), data=sample, trace=F),
arima = armaFit(~ arima(1, 1, 15), data=ts.sample),
arma = Arima(ts.sample, c(1,0,1)),
arfima = arfima(ts.sample),
auto.arima = auto.arima(ts.sample),
bats = bats(ts.sample),
HoltWinters = HoltWinters(ts.sample),
naive = Arima(ts.sample, c(0,1,0))
)
png(filename = 'plot1.png', width = 800, height = 800, units = 'px', pointsize = 12, bg = 'white')
layout(matrix(1:9,nr=3))
for(i in 1:len(models)) {
out = forecast.helper(models[[i]], 30, level = c(80,95))
forecast.plot(sample, out, main = names(models)[i])
}
dev.off()
png(filename = 'plot2.png', width = 800, height = 800, units = 'px', pointsize = 12, bg = 'white')
layout(matrix(1:9,nr=3))
for(i in 1:len(models)) {
out = forecast.helper(models[[i]], 30, level = c(75,85,95,97,99))
forecast.plot(sample, out, main = names(models)[i])
}
dev.off()
}
bt.new.60.40.test <- function()
{
load.packages('quantmod')
tickers = spl('SHY,IEF,TLT,SPY')
data.all <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1990-01-01', env = data.all, auto.assign = T)
for(i in ls(data.all)) data.all[[i]] = adjustOHLC(data.all[[i]], use.Adjusted=T)
bt.prep(data.all, align='remove.na')
prices = data.all$prices
n = ncol(prices)
nperiods = nrow(prices)
prices = prices/ matrix(first(prices), nr=nperiods, nc=n, byrow=T)
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plota.matplot(prices)
dev.off()
data <- new.env()
data$stock = data.all$SPY
data$bond = data.all$TLT
bt.prep(data, align='remove.na')
prices = data$prices
n = ncol(prices)
nperiods = nrow(prices)
models = list()
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]
weight.dollar = matrix(c(0.4, 0.6), nr=nperiods, nc=n, byrow=T)
data$weight[] = NA
data$weight[period.ends,] = weight.dollar[period.ends,]
models$dollar.w.60.40 = bt.run.share(data, clean.signal=F)
ret.log = bt.apply.matrix(prices, ROC, type='continuous')
hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 21)
weight.risk = weight.dollar / hist.vol
weight.risk = weight.risk / rowSums(weight.risk)
data$weight[] = NA
data$weight[period.ends,] = weight.risk[period.ends,]
models$risk.w.60.40 = bt.run.share(data, clean.signal=F)
data$weight[] = NA
data$weight[period.ends,] = target.vol.strategy(models$risk.w.60.40,
weight.risk, 6/100, 21, 100/100)[period.ends,]
models$risk.w.60.40.target6 = bt.run.share(data, clean.signal=T)
weight = target.vol.strategy(models$risk.w.60.40,
weight.risk, 6/100, 21, 100/100)
data.all$weight[] = NA
data.all$weight$SPY[period.ends,] = weight$stock[period.ends,]
data.all$weight$TLT[period.ends,] = weight$bond[period.ends,]
cash = 1-rowSums(weight)
data.all$weight$SHY[period.ends,] = cash[period.ends]
models$risk.w.60.40.target6.cash = bt.run.share(data.all, clean.signal=T)
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(models)
dev.off()
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot4.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(models$risk.w.60.40.target6)
dev.off()
png(filename = 'plot5.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(models$risk.w.60.40.target6.cash)
dev.off()
}
bt.aaa.combo <- function
(
data,
period.ends,
n.top = 5,
n.top.keep = n.top,
n.mom = 6*22,
n.vol = 1*22
)
{
prices = coredata(data$prices)
ret.log = bt.apply.matrix(prices, ROC, type='continuous')
hist.vol = bt.apply.matrix(ret.log, runSD, n = n.vol)
adj.vol = 1/hist.vol[period.ends,]
momentum = prices / mlag(prices, n.mom)
weight = ntop.keep(momentum[period.ends,], n.top, n.top.keep) * adj.vol
n.skip = max(n.mom, n.vol)
data$weight[] = NA
data$weight[period.ends,] = weight / rowSums(weight, na.rm=T)
data$weight[1 : n.skip,] = NA
bt.run.share(data, clean.signal=F, silent=T)
}
bt.aaa.minrisk <- function
(
data,
period.ends,
n.top = 5,
n.mom = 6*22,
n.vol = 1*22
)
{
prices = coredata(data$prices)
ret.log = bt.apply.matrix(prices, ROC, type='continuous')
momentum = prices / mlag(prices, n.mom)
weight = NA * prices
weight[period.ends,] = ntop(momentum[period.ends,], n.top)
n.skip = max(n.mom, n.vol)
for( i in period.ends[period.ends >= n.skip] ) {
hist = ret.log[ (i - n.vol + 1):i, ]
include.index = count(hist)== n.vol
index = ( weight[i,] > 0 ) & include.index
n = sum(index)
if(n > 0) {
hist = hist[ , index]
ia = create.ia(hist)
s0 = apply(coredata(hist),2,sd)
ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
weight[i,] = 0
weight[i,index] = min.risk.portfolio(ia, constraints)
}
}
data$weight[] = NA
data$weight[period.ends,] = weight[period.ends,]
bt.run.share(data, clean.signal=F, silent=T)
}
bt.aaa.sensitivity.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='2004:12::')
prices = data$prices
n = ncol(prices)
models = list()
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]
models = list()
models$combo = bt.aaa.combo(data, period.ends, n.top = 5,
n.mom = 180, n.vol = 20)
models$aaa = bt.aaa.minrisk(data, period.ends, n.top = 5,
n.mom = 180, n.vol = 20)
plotbt.custom.report.part1(models)
mom.lens = ( 1 : 12 ) * 20
vol.lens = ( 1 : 12 ) * 20
models = list()
for(n.mom in mom.lens) {
cat('MOM =', n.mom, '\n')
for(n.vol in vol.lens) {
cat('\tVOL =', n.vol, '\n')
models[[ paste('M', n.mom, 'V', n.vol) ]] =
bt.aaa.combo(data, period.ends, n.top = 5,
n.mom = n.mom, n.vol = n.vol)
}
}
out = plotbt.strategy.sidebyside(models, return.table=T, make.plot = F)
dummy = matrix('', len(vol.lens), len(mom.lens))
colnames(dummy) = paste('M', mom.lens)
rownames(dummy) = paste('V', vol.lens)
names = spl('Sharpe,Cagr,DVR,MaxDD')
png(filename = 'plot1.png', width = 1000, height = 1000, units = 'px', pointsize = 12, bg = 'white')
layout(matrix(1:4,nrow=2))
for(i in names) {
dummy[] = ''
for(n.mom in mom.lens)
for(n.vol in vol.lens)
dummy[paste('V', n.vol), paste('M', n.mom)] =
out[i, paste('M', n.mom, 'V', n.vol) ]
plot.table(dummy, smain = i, highlight = T, colorbar = F)
}
dev.off()
for(n.mom in mom.lens) {
cat('MOM =', n.mom, '\n')
for(n.vol in vol.lens) {
cat('\tVOL =', n.vol, '\n')
models[[ paste('M', n.mom, 'V', n.vol) ]] =
bt.aaa.minrisk(data, period.ends, n.top = 5,
n.mom = n.mom, n.vol = n.vol)
}
}
out = plotbt.strategy.sidebyside(models, return.table=T, make.plot = F)
png(filename = 'plot2.png', width = 1000, height = 1000, units = 'px', pointsize = 12, bg = 'white')
layout(matrix(1:4,nrow=2))
for(i in names) {
dummy[] = ''
for(n.mom in mom.lens)
for(n.vol in vol.lens)
dummy[paste('V', n.vol), paste('M', n.mom)] =
out[i, paste('M', n.mom, 'V', n.vol) ]
plot.table(dummy, smain = i, highlight = T, colorbar = F)
}
dev.off()
}
bt.aaa.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
data.price <- new.env()
for(i in ls(data)) data.price[[i]] = adjustOHLC(data[[i]], symbol.name=i, adjust='split', use.Adjusted=F)
bt.prep(data.price, align='keep.all', dates='2004:12::')
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='2004:12::')
use.total = FALSE
if(F) {
y = data$prices$TLT
y.price = data.price$prices$TLT
y = y / as.double(y[1])
y.price = y.price / as.double(y.price[1])
plota(y, type='l', ylim=range(y, y.price, na.rm=T))
plota.lines(y.price, col='red')
plota.legend('Total,Price', 'black,red')
}
prices = data$prices
n = ncol(prices)
prices4mom = iif(use.total, data$prices, data.price$prices)
prices4vol = iif(use.total, data$prices, data.price$prices)
models = list()
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]
n.top = 5
n.mom = 6*22
n.vol = 1*22
data$weight[] = NA
data$weight[period.ends,] = ntop(prices[period.ends,], n)
models$equal.weight = bt.run.share(data, clean.signal=F)
ret.log = bt.apply.matrix(prices4vol, ROC, type='continuous')
hist.vol = bt.apply.matrix(ret.log, runSD, n = n.vol)
adj.vol = 1/hist.vol[period.ends,]
data$weight[] = NA
data$weight[period.ends,] = adj.vol / rowSums(adj.vol, na.rm=T)
models$volatility.weighted = bt.run.share(data, clean.signal=F)
momentum = prices4mom / mlag(prices4mom, n.mom)
data$weight[] = NA
data$weight[period.ends,] = ntop(momentum[period.ends,], n.top)
models$momentum = bt.run.share(data, clean.signal=F)
weight = ntop(momentum[period.ends,], n.top) * adj.vol
data$weight[] = NA
data$weight[period.ends,] = weight / rowSums(weight, na.rm=T)
models$combo = bt.run.share(data, clean.signal=F,trade.summary = TRUE)
weight = NA * prices
weight[period.ends,] = ntop(momentum[period.ends,], n.top)
for( i in period.ends[period.ends >= n.mom] ) {
hist = ret.log[ (i - n.vol + 1):i, ]
include.index = count(hist)== n.vol
index = ( weight[i,] > 0 ) & include.index
n = sum(index)
if(n > 0) {
hist = hist[ , index]
ia = create.ia(hist)
s0 = apply(coredata(hist),2,sd)
ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
weight[i,] = 0
weight[i,index] = min.risk.portfolio(ia, constraints)
}
}
data$weight[] = NA
data$weight[period.ends,] = weight[period.ends,]
models$aaa = bt.run.share(data, clean.signal=F,trade.summary = TRUE)
models = rev(models)
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(models)
dev.off()
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part3(models$combo, trade.summary = TRUE)
dev.off()
png(filename = 'plot4.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part3(models$aaa, trade.summary = TRUE)
dev.off()
}
bt.aaa.test.new <- function()
{
load.packages('quantmod')
tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='2004:12::')
prices = data$prices
n = ncol(prices)
models = list()
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]
n.mom = 180
n.vol = 60
n.top = 4
momentum = prices / mlag(prices, n.mom)
models$combo = bt.aaa.combo(data, period.ends, n.top = n.top,
n.mom = n.mom, n.vol = n.vol)
models$aaa = bt.aaa.minrisk(data, period.ends, n.top = n.top,
n.mom = n.mom, n.vol = n.vol)
obj = portfolio.allocation.helper(data$prices, period.ends=period.ends,
lookback.len = n.vol, universe = ntop(momentum[period.ends,], n.top) > 0,
min.risk.fns = list(EW=equal.weight.portfolio,
RP=risk.parity.portfolio(),
MV=min.var.portfolio,
MD=max.div.portfolio,
MC=min.corr.portfolio,
MC2=min.corr2.portfolio,
MCE=min.corr.excel.portfolio,
RSO.2 = rso.portfolio(equal.weight.portfolio, 2, 100),
MS=max.sharpe.portfolio())
)
models = create.strategies(obj, data)$models
png(filename = 'plot2.png', width = 800, height = 800, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T)
dev.off()
png(filename = 'plot3.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(models$MS)
dev.off()
png(filename = 'plot4.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
dev.off()
}
bt.rso.portfolio.test <- function()
{
load.packages('quantmod,quadprog,corpcor,lpSolve')
tickers = spl('SPY,EEM,EFA,TLT,IWM,QQQ,GLD')
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1998::')
obj = portfolio.allocation.helper(data$prices,
periodicity = 'months', lookback.len = 120,
min.risk.fns = list(
EW = equal.weight.portfolio,
MS = max.sharpe.portfolio(),
RSO.MS.2 = rso.portfolio(max.sharpe.portfolio(), 2, 100),
RSO.MS.3 = rso.portfolio(max.sharpe.portfolio(), 3, 100),
RSO.MS.4 = rso.portfolio(max.sharpe.portfolio(), 4, 100),
RSO.MS.5 = rso.portfolio(max.sharpe.portfolio(), 5, 100),
RSO.MS.6 = rso.portfolio(max.sharpe.portfolio(), 6, 100),
RSO.MS.7 = rso.portfolio(max.sharpe.portfolio(), 7, 100)
)
)
models = create.strategies(obj, data)$models
strategy.performance.snapshoot(models,T)
}
bt.current.quote.test <- function()
{
load.packages('quantmod')
tickers = spl('VTI,EFA,SHY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data)
last(data$prices, 2)
load.packages('quantmod')
tickers = spl('VTI,EFA,SHY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
quotes = getQuote(tickers)
for(i in ls(data))
if( last(index(data[[i]])) < as.Date(quotes[i, 'Trade Time']) ) {
data[[i]] = rbind( data[[i]], make.xts(quotes[i, spl('Open,High,Low,Last,Volume,Last')],
as.Date(quotes[i, 'Trade Time'])))
}
bt.prep(data)
last(data$prices, 2)
}
bt.extend.DBC.test <- function()
{
load.packages('quantmod')
CRB = get.CRB()
tickers = spl('GSG,DBC')
getSymbols(tickers, src = 'yahoo', from = '1970-01-01')
out = na.omit(merge(Ad(CRB), Ad(GSG), Ad(DBC)))
colnames(out) = spl('CRB,GSG,DBC')
temp = out / t(repmat(as.vector(out[1,]),1,nrow(out)))
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
layout(1:2, heights=c(4,1))
plota(temp, ylim=range(temp))
plota.lines(temp[,1],col=1)
plota.lines(temp[,2],col=2)
plota.lines(temp[,3],col=3)
plota.legend(colnames(temp),1:3)
temp = cor(temp / mlag(temp)- 1, use='complete.obs', method='pearson')
temp[] = plota.format(100 * temp, 0, '', '%')
plot.table(temp)
dev.off()
tickers = spl('GLD,DBC,TLT')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
data$GLD = extend.GLD(data$GLD)
data$DBC = extend.data(data$DBC, get.CRB(), scale=T)
bt.prep(data, align='remove.na')
prices = data$prices
n = ncol(prices)
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]
models = list()
data$weight[] = NA
data$weight[period.ends,] = ntop(prices[period.ends,], n)
models$equal.weight = bt.run.share(data, clean.signal=F)
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot3.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(models)
dev.off()
}
bt.extend.DBC.update.test <- function()
{
load.packages('quantmod')
tickers = spl('GSG,DBC')
data = new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
temp = extract.table.from.webpage( join(readLines("TRJ_CRB")), 'EODValue' )
temp = join( apply(temp, 1, join, ','), '\n' )
data$CRB_1 = make.stock.xts( read.xts(temp, format='%m/%d/%y' ) )
data$CRB_2 = make.stock.xts( read.xts("prfmdata.csv", format='%m/%d/%Y' ) )
bt.prep(data, align='remove.na')
png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
plota.matplot(scale.one(data$prices))
dev.off()
}
bt.extend.GLD.test <- function()
{
load.packages('quantmod')
GLD = getSymbols('GLD', src = 'yahoo', from = '1970-01-01', auto.assign = F)
GLD = adjustOHLC(GLD, use.Adjusted=T)
temp = read.csv('http://wikiposit.org/w?action=dl&dltypes=comma%20separated&sp=daily&uid=KITCO',skip=4,header=TRUE, stringsAsFactors=F)
Gold.PM = make.xts(as.double(temp$Gold.PM) / 10, as.Date(temp$Date, '%d-%b-%Y'))
Gold.PM = Gold.PM[ !is.na(Gold.PM), ]
data <- new.env()
data$GLD = GLD
data$Gold.PM = Gold.PM
bt.prep(data, align='remove.na')
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
plota(data$GLD, type='l', col='black', plotX=F)
plota.lines(data$Gold.PM, col='blue')
plota.legend('GLD,Gold.PM', 'black,blue', list(data$GLD, data$Gold.PM))
spread = 100 * (Cl(data$GLD) - data$Gold.PM) / data$Gold.PM
plota(spread , type='l', col='black')
plota.legend('GLD vs Gold.PM % spread', 'black', spread)
dev.off()
SLV = getSymbols('SLV', src = 'yahoo', from = '1970-01-01', auto.assign = F)
SLV = adjustOHLC(SLV, use.Adjusted=T)
Silver = make.xts(as.double(temp$Silver), as.Date(temp$Date, '%d-%b-%Y'))
Silver = Silver[ !is.na(Silver), ]
data <- new.env()
data$SLV = SLV
data$Silver = Silver
bt.prep(data, align='remove.na')
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
plota(data$SLV, type='l', col='black')
plota.lines(data$Silver, col='blue')
plota.legend('SLV,Silver', 'black,blue', list(data$SLV, data$Silver))
spread = 100*(Cl(data$SLV) - data$Silver) / data$Silver
plota(spread , type='l', col='black')
plota.legend('SLV vs Silver % spread', 'black', spread)
dev.off()
tickers = spl('GLD,TLT')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
data$GLD = extend.GLD(data$GLD)
bt.prep(data, align='remove.na')
prices = data$prices
n = ncol(prices)
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]
models = list()
data$weight[] = NA
data$weight[period.ends,] = ntop(prices[period.ends,], n)
models$equal.weight = bt.run.share(data, clean.signal=F)
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot4.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(models)
dev.off()
}
bt.permanent.portfolio.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY,TLT,GLD,SHY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
data$GLD = extend.GLD(data$GLD)
bt.prep(data, align='remove.na')
prices = data$prices
n = ncol(prices)
nperiods = nrow(prices)
period.ends = endpoints(prices, 'years')
period.ends = period.ends[period.ends > 0]
period.ends.y = c(1, period.ends)
period.ends = endpoints(prices, 'quarters')
period.ends = period.ends[period.ends > 0]
period.ends.q = c(1, period.ends)
models = list()
target.allocation = matrix(rep(1/n,n), nrow=1)
data$weight[] = NA
data$weight[period.ends.y[1],] = target.allocation
models$buy.hold = bt.run.share(data, clean.signal=F)
data$weight[] = NA
data$weight[period.ends.y,] = ntop(prices[period.ends.y,], n)
models$equal.weight.y = bt.run.share(data, clean.signal=F)
models$threshold.y = bt.max.deviation.rebalancing(data, models$buy.hold, target.allocation, 10/100, 0, period.ends = period.ends.y)
data$weight[] = NA
data$weight[period.ends.q,] = ntop(prices[period.ends.q,], n)
models$equal.weight.q = bt.run.share(data, clean.signal=F)
models$threshold.q = bt.max.deviation.rebalancing(data, models$buy.hold, target.allocation, 10/100, 0, period.ends = period.ends.q)
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(models)
dev.off()
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover', F)
barplot.with.labels(sapply(models, compute.max.deviation, target.allocation), 'Maximum Deviation from Target Mix')
dev.off()
}
bt.permanent.portfolio2.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY,TLT,GLD,SHY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
data$GLD = extend.GLD(data$GLD)
bt.prep(data, align='remove.na')
prices = data$prices
n = ncol(prices)
period.ends = endpoints(prices, 'quarters')
period.ends = period.ends[period.ends > 0]
period.ends = c(1, period.ends)
models = list()
target.allocation = matrix(rep(1/n,n), nrow=1)
weight.dollar = ntop(prices, n)
data$weight[] = NA
data$weight[period.ends,] = weight.dollar[period.ends,]
models$dollar = bt.run.share(data, clean.signal=F)
data$weight[] = NA
data$weight[period.ends,] = target.vol.strategy(models$dollar,
weight.dollar, 7/100, 21, 100/100)[period.ends,]
models$dollar.target7 = bt.run.share(data, clean.signal=F)
ret.log = bt.apply.matrix(prices, ROC, type='continuous')
hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 21)
weight.risk = weight.dollar / hist.vol
weight.risk = weight.risk / rowSums(weight.risk)
data$weight[] = NA
data$weight[period.ends,] = weight.risk[period.ends,]
models$risk = bt.run.share(data, clean.signal=F)
if(F) {
data$weight[] = NA
data$weight[period.ends,] = target.vol.strategy(models$risk,
weight.risk, 7/100, 21, 100/100)[period.ends,]
models$risk.target7 = bt.run.share(data, clean.signal=F)
data$weight[] = NA
data$weight[period.ends,] = target.vol.strategy(models$risk,
weight.risk, 5/100, 21, 100/100)[period.ends,]
models$risk.target5 = bt.run.share(data, clean.signal=F)
}
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]
period.ends = c(1, period.ends)
sma = bt.apply.matrix(prices, SMA, 200)
weight.dollar.tactical = weight.dollar * (prices > sma)
data$weight[] = NA
data$weight[period.ends,] = weight.dollar.tactical[period.ends,]
models$dollar.tactical = bt.run.share(data, clean.signal=F)
data$weight[] = NA
data$weight[period.ends,] = target.vol.strategy(models$dollar.tactical,
weight.dollar.tactical, 7/100, 21, 100/100)[period.ends,]
models$dollar.tactical.target7 = bt.run.share(data, clean.signal=F)
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(models)
dev.off()
}
bt.permanent.portfolio3.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY,TLT,GLD,SHY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
data$GLD = extend.GLD(data$GLD)
bt.prep(data, align='remove.na')
prices = data$prices
n = ncol(prices)
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]
period.ends = c(1, period.ends)
models = list()
commission = 0.1
target.allocation = matrix(rep(1/n,n), nrow=1)
weight.dollar = ntop(prices, n)
data$weight[] = NA
data$weight[period.ends,] = weight.dollar[period.ends,]
models$dollar = bt.run.share(data, commission=commission, clean.signal=F)
data$weight[] = NA
data$weight[period.ends,] = target.vol.strategy(models$dollar,
weight.dollar, 7/100, 21, 100/100)[period.ends,]
models$dollar.target7 = bt.run.share(data, commission=commission, clean.signal=F)
ret.log = bt.apply.matrix(prices, ROC, type='continuous')
hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = 21)
weight.risk = weight.dollar / hist.vol
weight.risk$SHY = 0
weight.risk = weight.risk / rowSums(weight.risk)
data$weight[] = NA
data$weight[period.ends,] = weight.risk[period.ends,]
models$risk = bt.run.share(data, commission=commission, clean.signal=F)
data$weight[] = NA
data$weight[period.ends,] = target.vol.strategy(models$risk,
weight.risk, 7/100, 21, 100/100)[period.ends,]
models$risk.target7 = bt.run.share(data, commission=commission, clean.signal=F)
data$weight[] = NA
data$weight[period.ends,] = target.vol.strategy(models$risk,
weight.risk, 7/100, 21, 100/100)[period.ends,]
cash = 1-rowSums(data$weight)
data$weight$SHY[period.ends,] = cash[period.ends]
models$risk.target7.shy = bt.run.share(data, commission=commission, clean.signal=F)
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.strategy.sidebyside(models)
dev.off()
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
layout(1)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
dev.off()
sma = bt.apply.matrix(prices, SMA, 200)
weight.dollar.tactical = weight.dollar * (prices > sma)
data$weight[] = NA
data$weight[period.ends,] = weight.dollar.tactical[period.ends,]
models$dollar.tactical = bt.run.share(data, commission=commission, clean.signal=F)
data$weight[] = NA
data$weight[period.ends,] = target.vol.strategy(models$dollar.tactical,
weight.dollar.tactical, 7/100, 21, 100/100)[period.ends,]
models$dollar.tactical.target7 = bt.run.share(data, commission=commission, clean.signal=F)
weight.risk.tactical = weight.risk * (prices > sma)
data$weight[] = NA
data$weight[period.ends,] = weight.risk.tactical[period.ends,]
models$risk.tactical = bt.run.share(data, commission=commission, clean.signal=F)
data$weight[] = NA
data$weight[period.ends,] = target.vol.strategy(models$risk.tactical,
weight.risk.tactical, 7/100, 21, 100/100)[period.ends,]
cash = 1-rowSums(data$weight)
data$weight$SHY[period.ends,] = cash[period.ends]
models$risk.tactical.target7.shy = bt.run.share(data, commission=commission, clean.signal=F)
}
bt.mca.test <- function()
{
load.packages('quantmod,quadprog')
tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='2002:08::')
obj = portfolio.allocation.helper(data$prices, periodicity = 'weeks',
min.risk.fns = list(EW=equal.weight.portfolio,
RP=risk.parity.portfolio(),
MV=min.var.portfolio,
MD=max.div.portfolio,
MC=min.corr.portfolio,
MC2=min.corr2.portfolio,
MCE=min.corr.excel.portfolio),
custom.stats.fn = 'portfolio.allocation.custom.stats'
)
models = create.strategies(obj, data)$models
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)
mtext('Cumulative Performance', side = 2, line = 1)
out = plotbt.strategy.sidebyside(models, return.table=T)
dev.off()
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
cdi = custom.composite.diversification.indicator(obj,plot.table = F)
out = rbind(colMeans(cdi, na.rm=T), out)
rownames(out)[1] = 'Composite Diversification Indicator(CDI)'
dev.off()
y = 100 * sapply(models, compute.turnover, data)
out = rbind(y, out)
rownames(out)[1] = 'Portfolio Turnover'
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD', c(T,T,T,T))
dev.off()
png(filename = 'plot4.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
performance.barchart.helper(out, 'Volatility,Portfolio Turnover,Composite Diversification Indicator(CDI)', c(F,F,T))
dev.off()
png(filename = 'plot5.png', width = 600, height = 1000, units = 'px', pointsize = 12, bg = 'white')
layout(1:len(models))
for(m in names(models)) {
plotbt.transition.map(models[[m]]$weight, name=m)
legend('topright', legend = m, bty = 'n')
}
dev.off()
png(filename = 'plot6.png', width = 600, height = 1000, units = 'px', pointsize = 12, bg = 'white')
dates = index(data$prices)[obj$period.ends]
layout(1:len(models))
for(m in names(models)) {
plotbt.transition.map(make.xts(obj$risk.contributions[[m]], dates),
name=paste('Risk Contributions',m))
legend('topright', legend = m, bty = 'n')
}
dev.off()
plot.table(  sapply(models, function(m) round(100*last(m$weight),1))  )
}
bt.mca.speed.test <- function()
{
load.packages('quadprog,corpcor')
n = 100
hist = matrix(rnorm(1000*n), nc=n)
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(diag(n), type='>=', b=0, constraints)
constraints = add.constraints(diag(n), type='<=', b=1, constraints)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
ia = list()
ia$n = n
ia$risk = apply(hist, 2, sd)
ia$correlation = cor(hist, use='complete.obs', method='pearson')
ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
ia$cov = make.positive.definite(ia$cov, 0.000000001)
ia$correlation = make.positive.definite(ia$correlation, 0.000000001)
load.packages('rbenchmark')
benchmark(
min.var.portfolio(ia, constraints),
min.corr.portfolio(ia, constraints),
min.corr2.portfolio(ia, constraints),
columns=c("test", "replications", "elapsed", "relative"),
order="relative",
replications=100
)
Rprof()
for(i in 1:10)
min.corr.portfolio(ia, constraints)
Rprof(NULL)
summaryRprof()
tic(12)
for(icount in 1:10) {
}
toc(12)
Rprof()
for(icount in 1:10) {
}
Rprof(NULL)
summaryRprof()
}
bt.crp.test <- function()
{
load.packages('FNN')
load.packages('logopt', 'http://R-Forge.R-project.org')
load.packages('quantmod')
data(nyse.cover.1962.1984)
x = nyse.cover.1962.1984
x = x[,spl('iroqu,kinar')]
data <- new.env()
for(i in names(x)) {
data[[i]] = cumprod(x[,i])
colnames(data[[i]]) = 'Close'
}
bt.prep(data, align='remove.na')
prices = data$prices
n = ncol(prices)
plota(prices$iroqu, col='blue', type='l',   ylim=range(prices), main = '"iroqu" and "kinar"', ylab='')
plota.lines(prices$kinar, col='red')
grid()
plota.legend('iroqu,kinar', 'blue,red')
universal = prices[,1] * 0
alphas = seq(0,1,by=0.05)
crps = alphas
for (i in 1:length(crps)) {
data$weight[] = NA
data$weight[] = c(alphas[i], 1-alphas[i])
equity = bt.run(data, silent=T)$equity
universal = universal + equity
crps[i] = last(equity)
}
universal = universal/length(alphas)
plot(alphas, crps, col="blue", type="l", ylab="",
main='20 Year Return vs. mix of "iroqu" and "kinar"',
xlab='Fraction of "iroqu" in Portfolio')
points(alphas, crps, pch=19, cex=0.5, col="red")
abline(h=mean(crps), col="green")
text(0.5,mean(crps)*1.05,labels="Return from Universal Portfolio")
grid()
plota(prices$iroqu, col='blue', type='l',   ylim=range(prices, universal),
main = 'Universal Portfolios with "iroqu" and "kinar"', ylab="")
plota.lines(prices$kinar, col='red')
plota.lines(universal, col='green')
grid()
plota.legend('iroqu,kinar,universal', 'blue,red,green')
crp.portfolio <- function
(
ia,
constraints
)
{
bcrp.optim(1 + ia$hist.returns, fast.only = TRUE )
}
load.packages('quantmod,quadprog')
tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='2002:08::')
obj = portfolio.allocation.helper(data$prices, periodicity = 'weeks', lookback.len = 460,
min.risk.fns = list(CRP=crp.portfolio)
)
models = create.strategies(obj, data)$models
plotbt.custom.report.part2( models$CRP )
obj = portfolio.allocation.helper(data$prices, periodicity = 'weeks',
min.risk.fns = list(EW=equal.weight.portfolio,
RP=risk.parity.portfolio(),
MC=min.corr.portfolio,
MC2=min.corr2.portfolio)
)
models = c(models, create.strategies(obj, data)$models)
layout(1:2)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)
mtext('Cumulative Performance', side = 2, line = 1)
out = plotbt.strategy.sidebyside(models, return.table=T)
}
bt.october.gold.test <- function()
{
load.packages('quantmod')
ticker = 'GLD'
data = getSymbols(ticker, src = 'yahoo', from = '1970-01-01', auto.assign = F)
data = adjustOHLC(data, use.Adjusted=T)
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
month.year.seasonality(data, ticker)
dev.off()
data = bundes.bank.data.gold()
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
month.year.seasonality(data, 'GOLD', lookback.len = nrow(data))
dev.off()
GLD = getSymbols(ticker, src = 'yahoo', from = '1970-01-01', auto.assign = F)
GLD = adjustOHLC(GLD, use.Adjusted=T)
write.xts(extend.data(GLD, data / 10), 'GOLD.csv')
}
couch.potato.strategy <- function
(
data.all,
tickers = 'XIC.TO,XSP.TO,XBB.TO',
weights = c( 1/3, 1/3, 1/3 ),
periodicity = 'years',
dates = '1900::',
commission = 0.1
)
{
tickers = spl(tickers)
names(weights) = tickers
data <- new.env()
for(s in tickers) data[[ s ]] = data.all[[ s ]]
bt.prep(data, align='remove.na', dates=dates)
prices = data$prices
n = ncol(prices)
nperiods = nrow(prices)
period.ends = endpoints(data$prices, periodicity)
period.ends = c(1, period.ends[period.ends > 0])
data$weight[] = NA
for(s in tickers) data$weight[period.ends, s] = weights[s]
model = bt.run.share(data, clean.signal=F, commission=commission)
return(model)
}
bt.couch.potato.test <- function()
{
load.packages('quantmod')
map = list()
map$can.eq = 'XIC.TO'
map$can.div = 'XDV.TO'
map$us.eq = 'XSP.TO'
map$us.div = 'DVY'
map$int.eq = 'XIN.TO'
map$can.bond = 'XBB.TO'
map$can.real.bond = 'XRB.TO'
map$can.re = 'XRE.TO'
map$can.it = 'XTR.TO'
map$can.gold = 'XGD.TO'
data <- new.env()
for(s in names(map)) {
data[[ s ]] = getSymbols(map[[ s ]], src = 'yahoo', from = '1995-01-01', env = data, auto.assign = F)
data[[ s ]] = adjustOHLC(data[[ s ]], use.Adjusted=T)
}
models = list()
periodicity = 'years'
dates = '2006::'
models$classic = couch.potato.strategy(data, 'can.eq,us.eq,can.bond', rep(1/3,3), periodicity, dates)
models$global = couch.potato.strategy(data, 'can.eq,us.eq,int.eq,can.bond', c(0.2, 0.2, 0.2, 0.4), periodicity, dates)
models$yield = couch.potato.strategy(data, 'can.div,can.it,us.div,can.bond', c(0.25, 0.25, 0.25, 0.25), periodicity, dates)
models$growth = couch.potato.strategy(data, 'can.eq,us.eq,int.eq,can.bond', c(0.25, 0.25, 0.25, 0.25), periodicity, dates)
models$complete = couch.potato.strategy(data, 'can.eq,us.eq,int.eq,can.re,can.real.bond,can.bond', c(0.2, 0.15, 0.15, 0.1, 0.1, 0.3), periodicity, dates)
models$permanent = couch.potato.strategy(data, 'can.eq,can.gold,can.bond', c(0.25,0.25,0.5), periodicity, dates)
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)
mtext('Cumulative Performance', side = 2, line = 1)
out = plotbt.strategy.sidebyside(models, return.table=T)
dev.off()
tickers = spl('VIPSX,VTSMX,VGTSX,SPY,TLT,GLD,SHY')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1995-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
data$GLD = extend.GLD(data$GLD)
models = list()
periodicity = 'years'
dates = '2003::'
models$classic = couch.potato.strategy(data, 'VIPSX,VTSMX', rep(1/2,2), periodicity, dates)
models$margarita = couch.potato.strategy(data, 'VIPSX,VTSMX,VGTSX', rep(1/3,3), periodicity, dates)
models$permanent = couch.potato.strategy(data, 'SPY,TLT,GLD,SHY', rep(1/4,4), periodicity, dates)
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)
mtext('Cumulative Performance', side = 2, line = 1)
out = plotbt.strategy.sidebyside(models, return.table=T)
dev.off()
}
bt.regime.detection.test <- function()
{
bull1 = rnorm( 100, 0.10, 0.15 )
bear  = rnorm( 100, -0.01, 0.20 )
bull2 = rnorm( 100, 0.10, 0.15 )
true.states = c(rep(1,100),rep(2,100),rep(1,100))
returns = c( bull1, bear,  bull2 )
load.packages('RHmm')
y=returns
ResFit = HMMFit(y, nStates=2)
VitPath = viterbi(ResFit, y)
fb = forwardBackward(ResFit, y)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
plot(VitPath$states, type='s', main='Implied States', xlab='', ylab='State')
matplot(fb$Gamma, type='l', main='Smoothed Probabilities', ylab='Probability')
legend(x='topright', c('State1','State2'),  fill=1:2, bty='n')
dev.off()
bear2  = rnorm( 100, -0.01, 0.20 )
bull3 = rnorm( 100, 0.10, 0.10 )
bear3  = rnorm( 100, -0.01, 0.25 )
y = c( bull1, bear,  bull2, bear2, bull3, bear3 )
VitPath = viterbi(ResFit, y)$states
map = rank(sqrt(ResFit$HMM$distribution$var) - ResFit$HMM$distribution$mean)
VitPath = map[VitPath]
load.packages('quantmod')
data = xts(y, as.Date(1:len(y)))
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:3)
plota.control$col.x.highlight = col.add.alpha(true.states+1, 150)
plota(data, type='h', plotX=F, x.highlight=T)
plota.legend('Returns + True Regimes')
plota(cumprod(1+data/100), type='l', plotX=F, x.highlight=T)
plota.legend('Equity + True Regimes')
plota.control$col.x.highlight = col.add.alpha(VitPath+1, 150)
plota(data, type='h', x.highlight=T)
plota.legend('Returns + Detected Regimes')
dev.off()
}
bt.regime.detection.pitfalls.test <- function()
{
load.packages('quantmod')
data <- new.env()
getSymbols('SPY', src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
data$SPY = adjustOHLC(data$SPY, use.Adjusted=T)
bt.prep(data)
nperiods = nrow(data$prices)
models = list()
rets = ROC(Ad(data$SPY))
rets[1] = 0
in.sample.index = '1993::2002'
out.sample.index = '2003::'
in.sample = rets[in.sample.index]
out.sample = rets[out.sample.index]
out.sample.first.date = nrow(in.sample) + 1
load.packages('RHmm')
fit = HMMFit(in.sample, nStates=2)
states.all = rets * NA
states.all[] = viterbi(fit, rets)$states
data$weight[] = NA
data$weight[] = iif(states.all == 1, 0, 1)
data$weight[in.sample.index] = NA
models$states.all = bt.run.share(data)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
states.win1 = states.all * NA
for(i in out.sample.first.date:nperiods) {
states.win1[i] = last(viterbi(fit, rets[1:i])$states)
if( i %% 100 == 0) cat(i, 'out of', nperiods, '\n')
}
initPoint = fit$HMM
states.win2 = states.all * NA
for(i in out.sample.first.date:nperiods) {
fit2 = HMMFit(rets[2:i], nStates=2, control=list(init='USER', initPoint = initPoint))
initPoint = fit2$HMM
states.win2[i] = last(viterbi(fit2, rets[2:i])$states)
if( i %% 100 == 0) cat(i, 'out of', nperiods, '\n')
}
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:3)
col = col.add.alpha('white',210)
plota(states.all[out.sample.index], type='s', plotX=F)
plota.legend('Implied States based on all data', x='center', bty='o', bg=col, box.col=col,border=col,fill=col,cex=2)
plota(states.win1[out.sample.index], type='s')
plota.legend('Implied States based on rolling window', x='center', bty='o', bg=col, box.col=col,border=col,fill=col,cex=2)
plota(states.win2[out.sample.index], type='s')
plota.legend('Implied States based on rolling window(re-fit)', x='center', bty='o', bg=col, box.col=col,border=col,fill=col,cex=2)
dev.off()
data$weight[] = NA
data$weight[] = iif(states.win1 == 1, 0, 1)
data$weight[in.sample.index] = NA
models$states.win1 = bt.run.share(data)
data$weight[] = NA
data$weight[] = iif(states.win2 == 1, 0, 1)
data$weight[in.sample.index] = NA
models$states.win2 = bt.run.share(data)
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
}
bt.financial.turbulence.test <- function()
{
load.packages('quantmod')
fx = get.G10()
nperiods = nrow(fx)
turbulence = fx[,1] * NA
ret = coredata(fx / mlag(fx) - 1)
look.back = 252
for( i in (look.back+1) : nperiods ) {
temp = ret[(i - look.back + 1):(i-1), ]
turbulence[i] = mahalanobis(ret[i,], colMeans(temp), cov(temp))
if( i %% 200 == 0) cat(i, 'out of', nperiods, '\n')
}
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plota(EMA( turbulence, 30), type='l',
main='30 day average of the Financial Turbulence for G10 Currencies')
dev.off()
}
bt.pca.test <- function()
{
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
tickers.desc = spl('ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities')
sector.map = c()
for(i in 1:len(tickers)) {
sector.map = rbind(sector.map,
cbind(sector.spdr.components(tickers[i]), tickers.desc[i])
)
}
colnames(sector.map) = spl('ticker,sector')
load.packages('quantmod')
tickers = dow.jones.components()
sectors = factor(sector.map[ match(tickers, sector.map[,'ticker']), 'sector'])
names(sectors) = tickers
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '2000-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='2012')
sectors = sectors[data$symbolnames]
save(data, tickers, sectors, file='bt.pca.test.Rdata')
prices = data$prices
ret = prices / mlag(prices) - 1
p = princomp(na.omit(ret))
loadings = p$loadings[]
p.variance.explained = p$sdev^2 / sum(p$sdev^2)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
barplot(100*p.variance.explained, las=2, xlab='', ylab='% Variance Explained')
dev.off()
x = loadings[,1]
y = loadings[,2]
z = loadings[,3]
cols = as.double(sectors)
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot(x, y, type='p', pch=20, col=cols, xlab='Comp.1', ylab='Comp.2')
text(x, y, data$symbolnames, col=cols, cex=.8, pos=4)
legend('topright', cex=.8,  legend = levels(sectors), fill = 1:nlevels(sectors), merge = F, bty = 'n')
dev.off()
load.packages('scatterplot3d')
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
s3d = scatterplot3d(x, y, z, xlab='Comp.1', ylab='Comp.2', zlab='Comp.3', color=cols, pch = 20)
s3d.coords = s3d$xyz.convert(x, y, z)
text(s3d.coords$x, s3d.coords$y, labels=data$symbolnames, col=cols, cex=.8, pos=4)
legend('topleft', cex=.8,  legend = levels(sectors), fill = 1:nlevels(sectors), merge = F, bty = 'n')
dev.off()
}
bt.clustering.test <- function()
{
load.packages('quantmod')
load(file='bt.pca.test.Rdata')
prices = data$prices
ret = prices / mlag(prices) - 1
p = princomp(na.omit(ret))
loadings = p$loadings[]
x = loadings[,1]
y = loadings[,2]
z = loadings[,3]
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
hc = hclust(dist(cbind(x,y)), method = 'ward')
plot(hc, axes=F,xlab='', ylab='',sub ='', main='Comp 1/2')
rect.hclust(hc, k=3, border='red')
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
hc = hclust(dist(cbind(x,y,z)), method = 'ward')
plot(hc, axes=F,xlab='', ylab='',sub ='', main='Comp 1/2/3')
rect.hclust(hc, k=3, border='red')
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
hc = hclust(as.dist(1-cor(na.omit(ret))), method = 'ward')
plot(hc, axes=F,xlab='', ylab='',sub ='', main='Correlation')
rect.hclust(hc, k=3, border='red')
dev.off()
}
bt.pca.trading.test <- function()
{
load.packages('quantmod')
tickers = dow.jones.components()
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '2009-01-01', env = data, auto.assign = T)
bt.prep(data, align='remove.na')
prices = last(data$prices, 1000)
n = len(tickers)
ret = prices / mlag(prices) - 1
p = princomp(na.omit(ret[1:250,]))
loadings = p$loadings[]
components = loadings[,1:4]
components = components / rep.row(colSums(abs(components)),len(tickers))
market = ret[1:250,] %*% rep(1/n,n)
temp = cbind(market, -ret[1:250,] %*% components)
colnames(temp)[1] = 'Market'
round(cor(temp, use='complete.obs',method='pearson'),1)
round(100*sd(temp,na.rm=T),1)
library(tseries)
layout(1:2)
temp = rnorm(100)
plot(temp, type='b', main=adf.test(temp)$p.value)
plot(cumsum(temp), type='b', main=adf.test(cumsum(temp))$p.value)
library(tseries)
equity = bt.apply.matrix(1 + ifna(-ret %*% components,0), cumprod)
equity = make.xts(equity, index(prices))
adf.test(as.numeric(equity[,1]))$p.value
adf.test(as.numeric(equity[,2]))$p.value
adf.test(as.numeric(equity[,3]))$p.value
adf.test(as.numeric(equity[,4]))$p.value
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
i.comp = 4
bbands1 = BBands(rep.col(equity[,i.comp],3), n=200, sd=1)
bbands2 = BBands(rep.col(equity[,i.comp],3), n=200, sd=2)
temp = cbind(equity[,i.comp], bbands1[,'up'], bbands1[,'dn'], bbands1[,'mavg'],
bbands2[,'up'], bbands2[,'dn'])
colnames(temp) = spl('Comp. 4,1SD Up,1SD Down,200 SMA,2SD Up,2SD Down')
plota.matplot(temp, main=paste(i.comp, 'Principal component'))
barplot.with.labels(sort(components[,i.comp]), 'weights')
dev.off()
ts.sample = ts(as.numeric(equity[,i.comp]), frequency = 252)
fit.stl = stl(ts.sample, s.window="periodic")
plot(fit.stl)
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
plota.matplot(prices, plotX=F)
plota.matplot(equity)
dev.off()
}
bt.cluster.visual.test <- function()
{
load.packages('quantmod')
tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')
ret = data$prices / mlag(data$prices) - 1
ret = na.omit(ret)
dates = '2012::2012'
method = 'pearson'
correlation = cor(ret[dates], method = method)
dissimilarity = 1 - (correlation)
distance = as.dist(dissimilarity)
xy = cmdscale(distance)
fit = kmeans(xy, 4, iter.max=100, nstart=100)
fit$cluster
load.packages('cluster')
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F,
main = paste('Major Market Clusters over', dates), sub='')
dev.off()
png(filename = 'plot2.png', width = 800, height = 800, units = 'px', pointsize = 12, bg = 'white')
layout(matrix(1:8,nc=2))
par( mar = c(2, 2, 2, 2) )
for(icluster in 2:8)
clusplot(xy, kmeans(xy, icluster, iter.max=100, nstart=100)$cluster, color=TRUE, shade=F,
labels=3, lines=0, plotchar=F, main=icluster, sub='')
dev.off()
}
bt.cluster.optimal.number.test <- function()
{
load.packages('quantmod')
tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')
ret = data$prices / mlag(data$prices) - 1
ret = na.omit(ret)
dates = '2012::2012'
method = 'pearson'
correlation = cor(ret[dates], method = method)
dissimilarity = 1 - (correlation)
distance = as.dist(dissimilarity)
xy = cmdscale(distance)
n = ncol(data$prices)
n1 = ceiling(n*2/3)
p.exp = rep(0,n1)
min.cor = matrix(1,n1,n1)
for (i in 2:n1) {
fit = kmeans(xy, centers=i, iter.max=100, nstart=100)
p.exp[i] = 1- fit$tot.withinss / fit$totss
for (j in 1:i) {
index = fit$cluster == j
min.cor[i,j] = min(correlation[index,index])
}
}
min(which(p.exp > 0.9))
min(which(apply(min.cor[-1,],1,min,na.rm=T) > 0.4)) + 1
find.maximum.distance.point(p.exp[-1]) + 1
load.packages('cluster')
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
fit = kmeans(xy, 4, iter.max=100, nstart=100)
clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F,
main = paste('Major Market Clusters over', dates, ', 4 Clusters'), sub='')
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
fit = kmeans(xy, 5, iter.max=100, nstart=100)
clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F,
main = paste('Major Market Clusters over', dates, ', 5 Clusters'), sub='')
dev.off()
load.packages('mclust')
fitBIC = mclustBIC(xy)
plot(fitBIC, legendArgs = list(x = "topleft"))
fit <- summary(fitBIC, data = xy)
mclust2Dplot(data = xy, what = "density", identify = TRUE, parameters = fit$parameters, z = fit$z)
}
bt.cluster.optimal.number.historical.test <- function()
{
load.packages('quantmod')
tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')
dates='2007:03::'
tickers = dow.jones.components()
dates='1970::'
tickers = sp500.components()$tickers
dates='1994::'
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates=dates)
portfolio.allocation.custom.stats.clusters <- function(x,ia) {
return(list(
ncluster.90 = max(cluster.group.kmeans.90(ia)),
ncluster.elbow = max(cluster.group.kmeans.elbow(ia)),
ncluster.hclust = max(cluster.group.hclust(ia))
))
}
periodicity = 'weeks'
lookback.len = 250
obj = portfolio.allocation.helper(data$prices,
periodicity = periodicity, lookback.len = lookback.len,
min.risk.fns = list(EW=equal.weight.portfolio),
custom.stats.fn = portfolio.allocation.custom.stats.clusters
)
temp = list(ncluster.90 = 'Kmeans 90% variance',
ncluster.elbow = 'Kmeans Elbow',
ncluster.hclust = 'Hierarchical clustering at 1/3 height')
for(i in 1:len(temp)) {
hist.cluster = obj[[ names(temp)[i] ]]
title = temp[[ i ]]
png(filename = paste('plot',i,'.png',sep=''), width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plota(hist.cluster, type='l', col='gray', main=title)
plota.lines(SMA(hist.cluster,10), type='l', col='red',lwd=5)
plota.legend('Number of Clusters,10 period moving average', 'gray,red', x = 'bottomleft')
dev.off()
}
}
bt.seasonality.january.test <- function()
{
load.packages('quantmod')
price = getSymbols('^GSPC', src = 'yahoo', from = '1900-01-01', auto.assign = F)
price = Cl(to.monthly(price, indexAt='endof'))
ret = price / mlag(price) - 1
index =  which( date.month(index(ret)) == 1 & ret > 4/100 )
temp = c(coredata(ret),rep(0,12))
out = cbind(ret[index], sapply(index, function(i) prod(1 + temp[i:(i+11)])-1))
colnames(out) = spl('January,Year')
png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
col=col.add.alpha(spl('black,gray'),200)
pos = barplot(100*out, border=NA, beside=T, axisnames = F, axes = FALSE,
col=col, main='Annual Return When S&P500 Rises More than 4% in January')
axis(1, at = colMeans(pos), labels = date.year(index(out)), las=2)
axis(2, las=1)
grid(NA, NULL)
abline(h= 100*mean(out$Year), col='red', lwd=2)
plota.legend(spl('January,Annual,Average'),  c(col,'red'))
dev.off()
png(filename = 'plot2.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot.table(round(100*as.matrix(out),1))
dev.off()
}
bt.cluster.portfolio.allocation.test <- function()
{
load.packages('quantmod')
tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')
ret = data$prices / mlag(data$prices) - 1
dates = '2012::2012'
ret = ret[dates]
fn.name = 'risk.parity.portfolio'
fn.name = 'equal.weight.portfolio'
names = c('risk.parity.portfolio', 'equal.weight.portfolio')
for(fn.name in names) {
fn = match.fun(fn.name)
ia = create.ia(ret)
weight = fn(ia)
group = cluster.group.kmeans.90(ia)
ngroups = max(group)
weight0 = rep(NA, ia$n)
hist.g = NA * ia$hist.returns[,1:ngroups]
for(g in 1:ngroups) {
if( sum(group == g) == 1 ) {
weight0[group == g] = 1
hist.g[,g] = ia$hist.returns[, group == g, drop=F]
} else {
ia.temp = create.ia(ia$hist.returns[, group == g, drop=F])
w0 = fn(ia.temp)
weight0[group == g] = w0
hist.g[,g] = ia.temp$hist.returns %*% w0
}
}
ia.g = create.ia(hist.g)
group.weights = fn(ia.g)
for(g in 1:ngroups)
weight0[group == g] = weight0[group == g] * group.weights[g]
load.packages('RColorBrewer')
col = colorRampPalette(brewer.pal(9,'Set1'))(ia$n)
png(filename = paste(fn.name,'.plot.png',sep=''), width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')
layout(matrix(1:2,nr=2,nc=1))
par(mar = c(0,0,2,0))
index = order(group)
pie(weight[index], labels = paste(colnames(ret), round(100*weight,1),'%')[index], col=col, main=fn.name)
pie(weight0[index], labels = paste(colnames(ret), round(100*weight0,1),'%')[index], col=col, main=paste('Cluster',fn.name))
dev.off()
}
}
bt.cluster.portfolio.allocation.test1 <- function()
{
load.packages('quantmod')
tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')
periodicity = 'months'
lookback.len = 250
cluster.group = cluster.group.kmeans.90
obj = portfolio.allocation.helper(data$prices,
periodicity = periodicity, lookback.len = lookback.len,
min.risk.fns = list(
EW=equal.weight.portfolio,
RP=risk.parity.portfolio(),
C.EW = distribute.weights(equal.weight.portfolio, cluster.group),
C.RP=distribute.weights(risk.parity.portfolio(), cluster.group)
)
)
models = create.strategies(obj, data)$models
png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T)
dev.off()
}
load.hist.stock.data <- function()
{
load.packages('quantmod')
stock.folder = 'c:\\Stocks\\Data\\'
tickers = spl('UUP,EMB,HYG')
data <- new.env()
data.load.method = 'basic'
if(data.load.method == 'basic') {
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
}else if(data.load.method == 'basic.local') {
getSymbols.sit(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T, stock.folder = stock.folder)
}else if(data.load.method == 'custom.local') {
for(n in tickers) {
data[[n]] = read.xts(paste(stock.folder, n, '.csv', sep=''), format='%m/%d/%Y')
}
}else if(data.load.method == 'custom.one.file') {
filename = 'hex.csv'
all.data = read.xts(paste(stock.folder, filename, sep=''), format='%m/%d/%Y')
for(n in names(all.data)) {
data[[n]] = all.data[,n]
colnames(data[[n]]) = 'Close'
data[[n]]$Adjusted = data[[n]]$Open = data[[n]]$High = data[[n]]$Low = data[[n]]$Close
}
}
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')
prices = data$prices
n = ncol(prices)
models = list()
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]
obj = portfolio.allocation.helper(data$prices, period.ends=period.ends, lookback.len = 250,
min.risk.fns = list(EW=equal.weight.portfolio,
RP=risk.parity.portfolio(),
MV=min.var.portfolio,
MC=min.corr.portfolio)
)
models = create.strategies(obj, data)$models
strategy.performance.snapshoot(models, T)
}
john.ehlers.custom.strategy.plot <- function(
data,
models,
name,
main = name,
dates = '::',
layout = NULL
) {
stoch = roofing.stochastic.indicator(data$prices)
weight = models[[name]]$weight[dates]
col = iif(weight > 0, 'green', iif(weight < 0, 'red', 'white'))
plota.control$col.x.highlight = col.add.alpha(col, 100)
highlight = T
if(is.null(layout)) layout(1:2)
plota(data$prices[dates], type='l', x.highlight = highlight, plotX = F, main=main)
plota.legend('Long,Short,Not Invested','green,red,white')
plota(stoch[dates], type='l', x.highlight = highlight, plotX = F, ylim=c(0,1))
col = col.add.alpha('red', 100)
abline(h = 0.2, col=col, lwd=3)
abline(h = 0.8, col=col, lwd=3)
plota.legend('John Ehlers Stochastic')
}
john.ehlers.filter.test <- function() {
load.packages('quantmod')
tickers = spl('DG')
data = new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data)
prices = data$prices
models = list()
stoch = roofing.stochastic.indicator(prices)
stoch14 = bt.apply(data, function(x) stoch(HLC(x),14)[,'slowD'])
dates = '2011:10::2012:9'
jpeg(filename = 'plot1.jpg', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:3)
plota(prices[dates], type='l', plotX=F)
plota.legend('DG')
plota(stoch[dates], type='l', plotX=F)
abline(h = 0.2, col='red')
abline(h = 0.8, col='red')
plota.legend('John Ehlers Stochastic')
plota(stoch14[dates], type='l')
abline(h = 0.2, col='red')
abline(h = 0.8, col='red')
plota.legend('Stochastic')
dev.off()
data$weight[] = NA
data$weight[] = iif(cross.up(stoch, 0.2), 1, iif(cross.dn(stoch, 0.8), -1, NA))
models$post = bt.run.share(data, clean.signal=T, trade.summary=T)
data$weight[] = NA
data$weight[] = iif(cross.up(stoch, 0.2), 1, iif(cross.dn(stoch, 0.8), 0, NA))
models$post.L = bt.run.share(data, clean.signal=T, trade.summary=T)
data$weight[] = NA
data$weight[] = iif(cross.up(stoch, 0.2), 0, iif(cross.dn(stoch, 0.8), -1, NA))
models$post.S = bt.run.share(data, clean.signal=T, trade.summary=T)
data$weight[] = NA
data$weight[] = iif(cross.dn(stoch, 0.2), 1, iif(cross.up(stoch, 0.8), -1, NA))
models$pre = bt.run.share(data, clean.signal=T, trade.summary=T)
data$weight[] = NA
data$weight[] = iif(cross.dn(stoch, 0.2), 1, iif(cross.up(stoch, 0.8), 0, NA))
models$pre.L = bt.run.share(data, clean.signal=T, trade.summary=T)
data$weight[] = NA
data$weight[] = iif(cross.dn(stoch, 0.2), 0, iif(cross.up(stoch, 0.8), -1, NA))
models$pre.S = bt.run.share(data, clean.signal=T, trade.summary=T)
jpeg(filename = 'plot2.jpg', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T)
dev.off()
jpeg(filename = 'plot3.jpg', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:4, heights=c(2,1,2,1))
john.ehlers.custom.strategy.plot(data, models, 'post.L', dates = '2013::', layout=T,
main = 'post.L: Buy When the Indicator Crosses Above 20% and Sell when the Indicator Crosses Below 80%')
john.ehlers.custom.strategy.plot(data, models, 'pre.L', dates = '2013::', layout=T,
main = 'pre.L: Buy When the Indicator Crosses Below 20% and Sell when the Indicator Crosses Above 80%')
dev.off()
return
x = Cl(data$DG)
dates = '2013'
layout(1:2)
plota(x[dates], type='l')
plota(my.stochastic.indicator(x)[dates], type='l')
abline(h = 0.2, col='red')
abline(h = 0.8, col='red')
trades = last(models$pre.L$trade.summary$trades,10)
position = sign(as.double(trades[,'weight']))
d = index4xts(prices[dates2index(prices, trades[,'entry.date'])])
col = col.add.alpha('green', 50)
segments(d, rep(0.2,len(d)), d, rep(0.25,len(d)), col=col, lwd=5)
points(d, rep(0.25,len(d)), pch=24, col=col, bg=col, lwd=5)
d = index4xts(prices[dates2index(prices, trades[,'exit.date'])])
col = col.add.alpha('red', 50)
segments(d, rep(0.8,len(d)), d, rep(0.75,len(d)), col=col, lwd=5)
points(d, rep(0.75,len(d)), pch=25, col=col, bg=col, lwd=5)
last(models$post$trade.summary$trades,10)
}
bt.calendar.based.sector.strategy.test <- function()
{
load.packages('quantmod')
tickers = spl('FSPTX,FSENX,FSAGX,VFINX,BIL')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
TB3M = quantmod::getSymbols('DTB3', src='FRED', auto.assign = FALSE)
TB3M[] = ifna.prev(TB3M)
TB3M = processTBill(TB3M, timetomaturity = 1/4, 261)
data$BIL = extend.data(data$BIL, TB3M, scale=T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')
prices = data$prices
dates = data$dates
models = list()
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]
months = date.month(dates[period.ends])
dates = '::'
do.lag = 0
data$weight[] = NA
data$weight$VFINX[] = 1
models$VFINX  = bt.run.share(data, clean.signal=F, dates=dates, do.lag=do.lag)
data$weight[] = NA
data$weight$VFINX[period.ends] = iif( months >= 10 | months <= 5, 1, 0)
data$weight$BIL[period.ends] = iif( !(months >= 10 | months <= 5), 1, 0)
models$VFINX_Cash  = bt.run.share(data, clean.signal=F, dates=dates, do.lag=do.lag)
data$weight[] = NA
data$weight$FSPTX[period.ends] = iif( months >= 10 | months < 1, 1, 0)
data$weight$FSENX[period.ends] = iif( months >= 1 & months < 5, 1, 0)
data$weight$FSAGX[period.ends] = iif( months >= 8 & months < 9, 1, 0)
data$weight$BIL[period.ends] = 1 - rowSums(data$weight[period.ends], na.rm = T)
models$Sector  = bt.run.share(data, clean.signal=F, dates=dates, do.lag=do.lag)
jpeg(filename = 'plot1.jpg', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T)
dev.off()
plotbt.custom.report.part2(models$sector, trade.summary=T)
return
data$weight[] = NA
data$weight$FSPTX[period.ends] = iif( months >= 10 | months < 1, 1, 0)
data$weight$FSENX[period.ends] = iif( months >= 1 & months < 5, 1, 0)
data$weight$BIL[period.ends] = iif( months >= 5 & months < 8, 1, data$weight$BIL[period.ends])
data$weight$FSAGX[period.ends] = iif( months >= 8 & months < 9, 1, 0)
data$weight$BIL[period.ends] = iif( months >= 9 & months < 10, 1, data$weight$BIL[period.ends])
data$weight$BIL[period.ends] = ifna(data$weight$BIL[period.ends], 0)
models$sector1  = bt.run.share(data, clean.signal=F)
}
bt.7twelve.strategy.test <- function()
{
load.packages('quantmod')
tickers = spl('VFINX,VIMSX,NAESX,VDMIX,VEIEX,VGSIX,FNARX,QRAAX,VBMFX,VIPSX,OIBAX,BIL')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
TB3M = quantmod::getSymbols('DTB3', src='FRED', auto.assign = FALSE)
TB3M[] = ifna.prev(TB3M)
TB3M = processTBill(TB3M, timetomaturity = 1/4, 261)
data$BIL = extend.data(data$BIL, TB3M, scale=T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')
models = list()
data$weight[] = NA
data$weight$VFINX[] = 1
models$VFINX  = bt.run.share(data, clean.signal=F)
obj = portfolio.allocation.helper(data$prices, periodicity = 'years',
min.risk.fns = list(EW=equal.weight.portfolio)
)
models$year = create.strategies(obj, data)$models$EW
obj = portfolio.allocation.helper(data$prices, periodicity = 'quarters',
min.risk.fns = list(EW=equal.weight.portfolio)
)
models$quarter = create.strategies(obj, data)$models$EW
obj = portfolio.allocation.helper(data$prices, periodicity = 'months',
min.risk.fns = list(EW=equal.weight.portfolio)
)
models$month = create.strategies(obj, data)$models$EW
jpeg(filename = 'plot1.jpg', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T)
dev.off()
return
map = list(
us.eq = list(
us.large = list('VFINX', 'VTI'),
us.mid = list('VIMSX', 'VO'),
us.small = list('NAESX', 'VB')
),
non.us.eq = list(
devel.eq = list('VDMIX', 'EFA'),
em.eq = list('VEIEX', 'EEM')
),
re = list(
re = list('VGSIX', 'RWX')
),
res = list(
nat.res = list('FNARX', 'GLD'),
com = list('QRAAX', 'DBC')
),
us.bond = list(
us.bond = list('VBMFX', 'AGG'),
tips = list('VIPSX', 'TIP')
),
non.bond = list(
int.bond = list('OIBAX', 'BWX')
),
cash = list(
cash = list('BIL', 'BIL')
)
)
funds = unlist(lapply(map, function(x) lapply(x, function(y) y[[1]])))
etfs = unlist(lapply(map, function(x) lapply(x, function(y) y[[2]])))
paste(funds, collapse=',')
}
bt.mebanefaber.modified.mn.test <- function()
{
load.packages('quantmod')
data = new.env()
temp = get.fama.french.data('F-F_Research_Data_Factors', periodicity = '',download = T, clean = T)
ret = temp[[1]]$Mkt.RF + temp[[1]]$RF
price = bt.apply.matrix(ret / 100, function(x) cumprod(1 + x))
data$SPY = make.stock.xts( price )
temp = get.fama.french.data('10_Portfolios_Prior_12_2', periodicity = '',download = T, clean = T)
ret = temp[[1]]
price = bt.apply.matrix(ret / 100, function(x) cumprod(1 + x))
data$HI.MO = make.stock.xts( price$High )
data$LO.MO = make.stock.xts( price$Low )
bt.prep(data, align='remove.na')
models = list()
data$weight[] = NA
data$weight$SPY[] = 1
models$SPY = bt.run.share(data, clean.signal=T)
data$weight[] = NA
data$weight$HI.MO[] = 1
models$HI.MO = bt.run.share(data, clean.signal=T)
data$weight[] = NA
data$weight$LO.MO[] = 1
models$LO.MO = bt.run.share(data, clean.signal=T)
data$weight[] = NA
data$weight$HI.MO[] = 1
data$weight$LO.MO[] = -1
models$MKT.NEUTRAL = bt.run.share(data, clean.signal=F)
market.drawdown = -100 * compute.drawdown(data$prices$SPY)
market.drawdown.10.step = 10 * floor(market.drawdown / 10)
short.allocation = 100 - market.drawdown.10.step * 2
short.allocation[ short.allocation < 0 ] = 0
data$weight[] = NA
data$weight$HI.MO[] = 1
data$weight$LO.MO[] = -1 * short.allocation / 100
models$Modified.MN = bt.run.share(data, clean.signal=F)
jpeg(filename = 'plot1.jpg', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T)
dev.off()
}
bt.mebanefaber.f.squared.test <- function()
{
load.packages('quantmod')
data = new.env()
download = T
temp = get.fama.french.data('F-F_Research_Data_Factors', periodicity = '',download = download, clean = T)
ret = cbind(temp[[1]]$Mkt.RF + temp[[1]]$RF, temp[[1]]$RF)
price = bt.apply.matrix(ret / 100, function(x) cumprod(1 + x))
data$SPY = make.stock.xts( price$Mkt.RF )
data$SHY = make.stock.xts( price$RF )
temp = get.fama.french.data('10_Industry_Portfolios', periodicity = '',download = download, clean = T)
ret = temp[[1]]
price = bt.apply.matrix(ret[,1:9] / 100, function(x) cumprod(1 + x))
for(n in names(price)) data[[n]] = make.stock.xts( price[,n] )
data$symbolnames = c(names(price), 'SHY', 'SPY')
bt.prep(data, align='remove.na', dates='2000::')
bt.dates = '2001:04::'
prices = data$prices
n = ncol(data$prices)
models = list()
data$weight[] = NA
data$weight$SPY[1] = 1
models$SPY = bt.run.share(data, clean.signal=F, dates=bt.dates)
weight = prices
weight$SPY = NA
weight$SHY = NA
data$weight[] = NA
data$weight[] = ntop(weight[], n)
models$EW = bt.run.share(data, clean.signal=F, dates=bt.dates)
sma = bt.apply.matrix(prices, SMA, 10)
position.score = sma
position.score[ prices < sma ] = NA
position.score$SHY = NA
position.score$SPY = NA
weight = ntop(position.score[], n)
n.selected = rowSums(weight != 0)
weight$SHY[n.selected == 0,] = 1
weight[n.selected == 1,] = 0.25 * weight[n.selected == 1,]
weight$SHY[n.selected == 1,] = 0.75
weight[n.selected == 2,] = 0.5 * weight[n.selected == 2,]
weight$SHY[n.selected == 2,] = 0.5
weight[n.selected == 3,] = 0.75 * weight[n.selected == 3,]
weight$SHY[n.selected == 3,] = 0.25
data$weight[] = NA
data$weight[] = weight
models$strategy1 = bt.run.share(data, clean.signal=F, dates=bt.dates)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, one.page = T)
dev.off()
}
bt.averaged.test <- function()
{
load.packages('quantmod')
tickers = spl('Us.Eq = VTI + VTSMX,
Eurpoe.Eq = IEV + FIEUX,
Japan.Eq = EWJ + FJPNX,
Emer.Eq = EEM + VEIEX,
Re = RWX + VNQ + VGSIX,
Com = DBC + QRAAX,
Gold = GLD + SCGDX,
Long.Tr = TLT + VUSTX,
Mid.Tr = IEF + VFITX,
Short.Tr = SHY + VFISX')
start.date = 1998
dates = paste(start.date,'::',sep='')
data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, set.symbolnames = T, auto.assign = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates=paste(start.date-2,':12::',sep=''), fill.gaps = T)
prices = data$prices
n = ncol(prices)
nperiods = nrow(prices)
periodicity = 'quarters'
periodicity = 'months'
period.ends = endpoints(prices, periodicity)
period.ends = period.ends[period.ends > 0]
max.product.exposure = 0.6
lookback.len = 40
create.ia.fn = create.ia
ia.array = c(20,40,60)
avg.create.ia.fn = create.ia.averaged(ia.array, 1)
universe = prices>0
mom.lookback.len = 120
momentum = prices / mlag(prices, mom.lookback.len) - 1
mom.universe = ifna(momentum > 0, F)
mom.array = c(20,60,120,250)
avg.momentum = momentum.averaged(prices, mom.array, 3)
avgmom.universe = ifna(avg.momentum > 0, F)
min.risk.fns = list(
EW = equal.weight.portfolio,
MV = min.var.portfolio,
MCE = min.corr.excel.portfolio,
MV.RSO = rso.portfolio(min.var.portfolio, 3, 100, const.ub = max.product.exposure),
MCE.RSO = rso.portfolio(min.corr.excel.portfolio, 3, 100, const.ub = max.product.exposure)
)
make.strategy.custom <- function(name, create.ia.fn, lookback.len, universe, env) {
obj = portfolio.allocation.helper(data$prices,
periodicity = periodicity,
universe = universe,
lookback.len = lookback.len,
create.ia.fn = create.ia.fn,
const.ub = max.product.exposure,
min.risk.fns = min.risk.fns,
adjust2positive.definite = F
)
env[[name]] = create.strategies(obj, data, prefix=paste(name,'.',sep=''))$models
}
models <- new.env()
make.strategy.custom('ia.none'        , create.ia.fn    , lookback.len, universe       , models)
make.strategy.custom('ia.mom'         , create.ia.fn    , lookback.len, mom.universe   , models)
make.strategy.custom('ia.avg_mom'     , create.ia.fn    , lookback.len, avgmom.universe, models)
make.strategy.custom('avg_ia.none'    , avg.create.ia.fn, 252         , universe       , models)
make.strategy.custom('avg_ia.mom'     , avg.create.ia.fn, 252         , mom.universe   , models)
make.strategy.custom('avg_ia.avg_mom' , avg.create.ia.fn, 252         , avgmom.universe, models)
strategy.snapshot.custom <-	function(models, n = 0, title = NULL) {
if (n > 0)
models = models[ as.vector(matrix(1:len(models),ncol=n, byrow=T)) ]
layout(1:3)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title)
mtext('Cumulative Performance', side = 2, line = 1)
plotbt.strategy.sidebyside(models)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover', T)
}
png(filename = 'plot1.png', width = 900, height = 900, units = 'px', pointsize = 12, bg = 'white')
models.final = c(models$ia.none, models$ia.mom)
strategy.snapshot.custom(models.final, len(min.risk.fns), 'Momentum Filter')
dev.off()
png(filename = 'plot2.png', width = 900, height = 900, units = 'px', pointsize = 12, bg = 'white')
models.final = c(models$ia.none, models$avg_ia.none)
strategy.snapshot.custom(models.final, len(min.risk.fns), 'Averaged Input Assumptions')
dev.off()
png(filename = 'plot3.png', width = 900, height = 900, units = 'px', pointsize = 12, bg = 'white')
models.final = c(models$ia.mom, models$ia.avg_mom)
strategy.snapshot.custom(models.final, len(min.risk.fns), 'Averaged Momentum')
dev.off()
png(filename = 'plot4.png', width = 900, height = 900, units = 'px', pointsize = 12, bg = 'white')
models.final = c(models$ia.mom, models$avg_ia.avg_mom)
strategy.snapshot.custom(models.final, len(min.risk.fns), 'Averaged vs Base')
dev.off()
}
bt.probabilistic.momentum.test <- function()
{
load.packages('quantmod')
tickers = spl('EQ=QQQ,FI=SHY')
tickers = spl('EQ=SPY,FI=TLT')
data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='::')
lookback.len = 120
lookback.len = 60
confidence.level = 60/100
prices = data$prices
ret = prices / mlag(prices) - 1
models = list()
momentum = prices / mlag(prices, lookback.len)
data$weight[] = NA
data$weight$EQ[] = momentum$EQ > momentum$FI
data$weight$FI[] = momentum$EQ <= momentum$FI
models$Simple  = bt.run.share(data, clean.signal=T)
ir = sqrt(lookback.len) * runMean(ret$EQ - ret$FI, lookback.len) / runSD(ret$EQ - ret$FI, lookback.len)
momentum.p = pt(ir, lookback.len - 1)
data$weight[] = NA
data$weight$EQ[] = iif(cross.up(momentum.p, confidence.level), 1, iif(cross.dn(momentum.p, (1 - confidence.level)), 0,NA))
data$weight$FI[] = iif(cross.dn(momentum.p, (1 - confidence.level)), 1, iif(cross.up(momentum.p, confidence.level), 0,NA))
models$Probabilistic  = bt.run.share(data, clean.signal=T)
data$weight[] = NA
data$weight$EQ[] = iif(cross.up(momentum.p, confidence.level), 1, iif(cross.up(momentum.p, (1 - confidence.level)), 0,NA))
data$weight$FI[] = iif(cross.dn(momentum.p, (1 - confidence.level)), 1, iif(cross.up(momentum.p, confidence.level), 0,NA))
models$Probabilistic.Leverage = bt.run.share(data, clean.signal=T)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T)
dev.off()
cols = spl('steelblue,steelblue1')
prices = scale.one(data$prices)
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:3)
plota(prices$EQ, type='l', ylim=range(prices), plotX=F, col=cols[1], lwd=2)
plota.lines(prices$FI, type='l', plotX=F, col=cols[2], lwd=2)
plota.legend('EQ,FI',cols,as.list(prices))
highlight = models$Probabilistic$weight$EQ > 0
plota.control$col.x.highlight = iif(highlight, cols[1], cols[2])
plota(models$Probabilistic$equity, type='l', plotX=F, x.highlight = highlight | T)
plota.legend('Probabilistic,EQ,FI',c('black',cols))
highlight = models$Simple$weight$EQ > 0
plota.control$col.x.highlight = iif(highlight, cols[1], cols[2])
plota(models$Simple$equity, type='l', plotX=T, x.highlight = highlight | T)
plota.legend('Simple,EQ,FI',c('black',cols))
dev.off()
pdf(file = 'Probabilistic.Momentum.Report.pdf', width=8.5, height=11)
strategy.performance.snapshoot(bt.trim(models), data = data)
dev.off()
momentum = prices / mlag(prices, lookback.len)
signal = momentum$EQ > momentum$FI
data$weight[] = NA
data$weight$EQ[] = iif(signal, 60, 40) / 100
data$weight$FI[] = iif(signal, 40, 60) / 100
models$Simple  = bt.run.share(data, clean.signal=T)
ir = sqrt(lookback.len) * runMean(ret$EQ - ret$FI, lookback.len) / runSD(ret$EQ - ret$FI, lookback.len)
momentum.p = pt(ir, lookback.len - 1)
signal = iif(cross.up(momentum.p, confidence.level), 1, iif(cross.dn(momentum.p, (1 - confidence.level)), 0,NA))
signal = ifna.prev(signal) == 1
data$weight[] = NA
data$weight$EQ[] = iif(signal, 60, 40) / 100
data$weight$FI[] = iif(signal, 40, 60) / 100
models$Probabilistic  = bt.run.share(data, clean.signal=T)
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T)
dev.off()
}
bt.load.thebonnotgang.data <- function(Symbols, folder, silent=F, clean=T)
{
load.packages('quantmod')
Sys.localeconv()["decimal_point"]
Sys.setlocale("LC_NUMERIC", "French_France.1252")
data <- new.env()
for(s in spl(Symbols))
data[[s]] = read.xts(paste0(folder,s,'_1m.csv'),
sep = ';', date.column = 3, format='%Y-%m-%d %H:%M:%S', index.class = c("POSIXlt", "POSIXt"))
if(!clean)return(data)
for(i in ls(data)) {
dates = index(data[[i]])
dates.number = as.double(dates)
factor = format(dates, '%Y%m%d')
gap = tapply(dates.number, factor, function(x) max(diff(x)))
ok.index = names(gap[gap <= 4*60])
data[[i]] = data[[i]][ !is.na(match(factor, ok.index)) ]
if(!silent)cat(i, 'removing due to gaps:', setdiff(factor,ok.index), '\n\n')
dates = index(data[[i]])
dates.number = as.double(dates)
factor = format(dates, '%Y%m%d')
nperiods = len(dates)
day.change = which(diff(dates.number) > 5 * 60)
day.start = c(1, day.change + 1)
day.end = c(day.change, nperiods)
ok.index = which(dates.number[day.end] - dates.number[day.start] < 7*60*60 &
dates.number[day.end] - dates.number[day.start] > 2*60*60)
ok.index = factor[day.start][ok.index]
data[[i]] = data[[i]][ !is.na(match(factor, ok.index)) ]
if(!silent)cat(i, 'removing due to trading hours:', setdiff(factor,ok.index), '\n\n')
dates = index(data[[i]])
dates.number = as.double(dates)
factor = format(dates, '%Y%m%d')
nperiods = len(dates)
day.change = which(diff(dates.number) > 5 * 60)
day.start = c(1, day.change + 1)
day.end = c(day.change, nperiods)
add.hours = as.double(format(dates[day.start], '%H')) - 9
for(h in which(add.hours != 0))
dates[day.start[h]:day.end[h]] = dates[day.start[h]:day.end[h]] - add.hours[h]*60*60
index(data[[i]]) = dates
}
ok.index = unique(format(index(data[[ls(data)[1]]]), '%Y%m%d'))
for(i in ls(data)) {
dates = index(data[[i]])
factor = format(dates, '%Y%m%d')
ok.index = intersect(ok.index, unique(factor))
}
for(i in ls(data)) {
dates = index(data[[i]])
factor = format(dates, '%Y%m%d')
data[[i]] = data[[i]][ !is.na(match(factor, ok.index)) ]
if(!silent)cat(i, 'removing due to not being common:', setdiff(factor,ok.index), '\n\n')
}
for(i in ls(data))
index(data[[i]]) = as.POSIXct(format(index(data[[i]]) + 60, '%Y-%m-%d %H:%M'), tz = Sys.getenv('TZ'), format = '%Y-%m-%d %H:%M')
data
}
bt.intraday.day <- function(dates)
{
dates.number = as.double(dates)
nperiods = len(dates)
day.change = which(diff(dates.number) > 5 * 60)
list(
day.start = c(1, day.change + 1),
day.end = c(day.change, nperiods)
)
}
bt.intraday.thebonnotgang.test <- function()
{
load.packages('quantmod')
spath = 'c:/Desktop/'
Sys.localeconv()["decimal_point"]
Sys.setlocale("LC_NUMERIC", "French_France.1252")
data <- new.env()
data$SPY = read.xts(paste0(spath,'SPY_1m.csv'),
sep = ';', date.column = 3, format='%Y-%m-%d %H:%M:%S', index.class = c("POSIXlt", "POSIXt"))
data$GLD = read.xts(paste0(spath,'GLD_1m.csv'),
sep = ';', date.column = 3, format='%Y-%m-%d %H:%M:%S', index.class = c("POSIXlt", "POSIXt"))
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(c(1,1,2))
plota(data$SPY['2012:11:01'], type='candle', main='SPY on Nov 1st, 2012', plotX = F)
plota(plota.scale.volume(data$SPY['2012:11:01']), type = 'volume')
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(c(1,1,2))
plota(data$SPY['2013:11:01'], type='candle', main='SPY on Nov 1st, 2013', plotX = F)
plota(plota.scale.volume(data$SPY['2013:11:01']), type = 'volume')
dev.off()
i = 'GLD'
dates = index(data[[i]])
factor = format(dates, '%Y%m%d')
gap = tapply(dates, factor, function(x) max(diff(x)))
gap[names(gap[gap > 4*60])]
data[[i]]['2013:02:19']
i = 'SPY'
dates = index(data[[i]])
factor = format(dates, '%Y%m%d')
gap = tapply(dates, factor, function(x) max(diff(x)))
gap[names(gap[gap > 4*60])]
data[[i]]['2013:02:19']
data.daily <- new.env()
quantmod::getSymbols(spl('SPY,GLD'), src = 'yahoo', from = '1970-01-01', env = data.daily, auto.assign = T)
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1)
plota(data$GLD, type='l', col='blue', main='GLD')
plota.lines(data.daily$GLD, type='l', col='red')
plota.legend('Intraday,Daily', 'blue,red')
dev.off()
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plota(data$SPY, type='l', col='blue', main='SPY')
plota.lines(data.daily$SPY, type='l', col='red')
plota.legend('Intraday,Daily', 'blue,red')
dev.off()
GLD.sample = data$GLD['2012:07:10::2012:07:10 09:35']
SPY.sample= data$SPY['2012:07:10::2012:07:10 09:35']
merge( Cl(GLD.sample), Cl(SPY.sample) )
index(GLD.sample) = as.POSIXct(format(index(GLD.sample) + 60, '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
index(SPY.sample) = as.POSIXct(format(index(SPY.sample) + 60, '%Y-%m-%d %H:%M'), format = '%Y-%m-%d %H:%M')
merge( Cl(GLD.sample), Cl(SPY.sample) )
data = bt.load.thebonnotgang.data('SPY,GLD', spath)
bt.prep(data, align='keep.all', fill.gaps = T)
prices = data$prices
dates = data$dates
nperiods = nrow(prices)
models = list()
data$weight[] = NA
data$weight$SPY = 1
models$SPY = bt.run.share(data, clean.signal=F)
data$weight[] = NA
data$weight$GLD = 1
models$GLD = bt.run.share(data, clean.signal=F)
data$weight[] = NA
data$weight$SPY = 0.5
data$weight$GLD = 0.5
models$EW = bt.run.share(data, clean.signal=F)
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T)
dev.off()
}
bt.strategy.intraday.thebonnotgang.test <- function()
{
load.packages('quantmod')
spath = 'c:/Desktop/'
spath = 'c:/Documents and Settings/mkapler/Desktop/'
spath = 'c:/Desktop/1car/1shaun/'
data = bt.load.thebonnotgang.data('SPY,GLD', spath)
data1 <- new.env()
data1$FI = data$GLD
data1$EQ = data$SPY
data = data1
bt.prep(data, align='keep.all', fill.gaps = T)
lookback.len = 120
confidence.level = 60/100
prices = data$prices
ret = prices / mlag(prices) - 1
models = list()
momentum = prices / mlag(prices, lookback.len)
data$weight[] = NA
data$weight$EQ[] = momentum$EQ > momentum$FI
data$weight$FI[] = momentum$EQ <= momentum$FI
models$Simple  = bt.run.share(data, clean.signal=T)
ir = sqrt(lookback.len) * runMean(ret$EQ - ret$FI, lookback.len) / runSD(ret$EQ - ret$FI, lookback.len)
momentum.p = pt(ir, lookback.len - 1)
data$weight[] = NA
data$weight$EQ[] = iif(cross.up(momentum.p, confidence.level), 1, iif(cross.dn(momentum.p, (1 - confidence.level)), 0,NA))
data$weight$FI[] = iif(cross.dn(momentum.p, (1 - confidence.level)), 1, iif(cross.up(momentum.p, confidence.level), 0,NA))
models$Probabilistic  = bt.run.share(data, clean.signal=T)
data$weight[] = NA
data$weight$EQ[] = iif(cross.up(momentum.p, confidence.level), 1, iif(cross.up(momentum.p, (1 - confidence.level)), 0,NA))
data$weight$FI[] = iif(cross.dn(momentum.p, (1 - confidence.level)), 1, iif(cross.up(momentum.p, confidence.level), 0,NA))
models$Probabilistic.Leverage = bt.run.share(data, clean.signal=T)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T)
dev.off()
strategy.name = 'Probabilistic.Leverage'
ret = models[[strategy.name]]$ret
ret.number = 100*as.double(ret)
dates = index(ret)
factor = format(dates, '%H')
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
par(mar=c(4,4,1,1))
boxplot(tapply(ret.number, factor, function(x) x),outline=T, main=paste(strategy.name, 'Distribution of Returns'), las=1)
barplot(tapply(ret.number, factor, function(x) sum(x)), main=paste(strategy.name, 'P&L by Hour'), las=1)
dev.off()
day.stat = bt.intraday.day(dates)
ret.number[day.stat$day.start] = 0
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
par(mar=c(4,4,1,1))
boxplot(tapply(ret.number, factor, function(x) x),outline=T, main=paste(strategy.name, 'Distribution of Returns'), las=1)
barplot(tapply(ret.number, factor, function(x) sum(x)), main=paste(strategy.name, 'P&L by Hour'), las=1)
dev.off()
}
bt.pair.strategy.intraday.thebonnotgang.test <- function()
{
load.packages('quantmod')
spath = 'c:/Desktop/'
spath = 'c:/Documents and Settings/mkapler/Desktop/'
data = bt.load.thebonnotgang.data('USO,GLD', spath)
bt.prep(data, align='keep.all', fill.gaps = T)
prices = data$prices
nperiods = nrow(prices)
dates = data$dates
day.stat = bt.intraday.day(dates)
models = list()
lookback = 120
stoch = (prices - bt.apply.matrix(prices, runMin, lookback)) / (bt.apply.matrix(prices, runMax, lookback) - bt.apply.matrix(prices, runMin, lookback))
stoch = bt.apply.matrix(stoch, ifna.prev)
stat = stoch$USO - stoch$GLD
stat = (stat - runMean(stat,20))/runSD(stat,20)
data$weight[] = NA
data$weight$USO = iif(stat >= 2, -1, iif(stat <= -2, 1, 0))
data$weight$GLD = iif(stat <= -2, -1, iif(stat >= 2, 1, 0))
data$weight[day.stat$day.end,] = 0
data$weight[as.vector(0:(lookback-1) + rep.row(day.stat$day.start,lookback)),] = 0
models$P = bt.run.share(data, clean.signal=T, do.lag = 1)
beta = NA * prices[,1]
temp = coredata(prices[,spl('USO,GLD')])
for(i in lookback : nperiods) {
dummy = temp[(i- lookback +1):i,]
beta[i] = ols(dummy[, 1], dummy[, 2])$coefficients
if( i %% 1000 == 0) cat(i, nperiods, round(100*i/nperiods), '\n')
}
stat = temp[,2] - beta * temp[,1]
stat = -(stat - runMean(stat,20))/runSD(stat,20)
data$weight[] = NA
data$weight$USO = iif(stat >= 2, -1, iif(stat <= -2, 1, 0))
data$weight$GLD = iif(stat <= -2, -1, iif(stat >= 2, 1, 0))
data$weight[day.stat$day.end,] = 0
data$weight[as.vector(0:(lookback-1) + rep.row(day.stat$day.start,lookback)),] = 0
models$P1 = bt.run.share(data, clean.signal=T, do.lag = 1)
png(filename = 'plot1a.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T)
dev.off()
}
bt.calendar.strategy.month.end.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, set.symbolnames = T, auto.assign = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', fill.gaps = T)
prices = data$prices
n = ncol(prices)
models = list()
universe = prices > 0
key.date.index = date.month.ends(data$dates, F)
key.date = NA * prices
key.date[key.date.index,] = T
data$weight[] = NA
data$weight[] = ifna(universe & key.date, F)
models$T0 = bt.run.share(data, do.lag = 0, trade.summary=T, clean.signal=T)
calendar.strategy <- function(data, signal, universe = data$prices > 0) {
data$weight[] = NA
data$weight[] = ifna(universe & signal, F)
bt.run.share(data, do.lag = 0, trade.summary=T, clean.signal=T)
}
calendar.signal <- function(key.date, offsets = 0) {
signal = mlag(key.date, offsets[1])
for(i in offsets) signal = signal | mlag(key.date, i)
signal
}
models$T0 = calendar.strategy(data, key.date)
models$N1 = calendar.strategy(data, mlag(key.date,1))
models$N2 = calendar.strategy(data, mlag(key.date,2))
models$P1 = calendar.strategy(data, mlag(key.date,-1))
models$P2 = calendar.strategy(data, mlag(key.date,-2))
signal = key.date | mlag(key.date,-1) | mlag(key.date,-2) | mlag(key.date,1) | mlag(key.date,2)
models$P2N2 = calendar.strategy(data, signal)
models$P2N2 = calendar.strategy(data, calendar.signal(key.date, -2:2))
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, control=list(comparison=T), sort.performance=F)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
last.trades(models$P2)
dev.off()
signals = calendar.signal(key.date, T0=0, N1=1, N2=2, P1=-1, P2=-2, P2N2=-2:2)
models = calendar.strategy(data, signals, universe = universe)
strategy.performance.snapshoot(models, control=list(main=T))
strategy.performance.snapshoot(models, control=list(comparison=T), sort.performance=F)
strategy.performance.snapshoot(models["P2N2"], control=list(monthly=T))
strategy.performance.snapshoot(models, control=list(transition=T))
last.trades(models$P2)
}
bt.calendar.strategy.option.expiry.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, set.symbolnames = T, auto.assign = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', fill.gaps = T)
prices = data$prices
n = ncol(prices)
dates = data$dates
models = list()
universe = prices > 0
years = date.year(range(dates))
second.friday = third.friday.month(years[1]:years[2], 12) - 7
key.date.index = na.omit(match(second.friday, dates))
key.date = NA * prices
key.date[key.date.index,] = T
signals = list(T0=0)
for(i in 1:15) signals[[paste0('N',i)]] = 0:i
signals = calendar.signal(key.date, signals)
models = calendar.strategy(data, signals, universe = universe)
names(models)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T, sort.performance=F)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
out = sapply(models, function(x) list(
CAGR = 100*compute.cagr(x$equity),
MD = 100*compute.max.drawdown(x$equity),
Win = x$trade.summary$stats['win.prob', 'All'],
Profit = x$trade.summary$stats['profitfactor', 'All']
))
performance.barchart.helper(out, sort.performance = F)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models$N15, control=list(main=T))
dev.off()
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
last.trades(models$N15)
dev.off()
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
trades = models$N15$trade.summary$trades
trades = make.xts(parse.number(trades[,'return']), as.Date(trades[,'entry.date']))
layout(1:2)
par(mar = c(4,3,3,1), cex = 0.8)
barplot(trades, main='Trades', las=1)
plot(cumprod(1+trades/100), type='b', main='Trades', las=1)
dev.off()
}
bt.calendar.strategy.fed.days.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY')
data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, set.symbolnames = T, auto.assign = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', fill.gaps = T)
prices = data$prices
n = ncol(prices)
dates = data$dates
models = list()
universe = prices > 0
universe = universe & prices > SMA(prices,100)
info = get.FOMC.dates(F)
key.date.index = na.omit(match(info$day, dates))
key.date = NA * prices
key.date[key.date.index,] = T
signals = list(T0=0)
for(i in 1:15) signals[[paste0('N',i)]] = 0:i
signals = calendar.signal(key.date, signals)
models = calendar.strategy(data, signals, universe = universe)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T, sort.performance=F)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
out = sapply(models, function(x) list(
CAGR = 100*compute.cagr(x$equity),
MD = 100*compute.max.drawdown(x$equity),
Win = x$trade.summary$stats['win.prob', 'All'],
Profit = x$trade.summary$stats['profitfactor', 'All']
))
performance.barchart.helper(out, sort.performance = F)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models$N15, control=list(main=T))
dev.off()
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
last.trades(models$N15)
dev.off()
png(filename = 'plot5.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
trades = models$N15$trade.summary$trades
trades = make.xts(parse.number(trades[,'return']), as.Date(trades[,'entry.date']))
layout(1:2)
par(mar = c(4,3,3,1), cex = 0.8)
barplot(trades, main='N15 Trades', las=1)
plot(cumprod(1+trades/100), type='b', main='N15 Trades', las=1)
dev.off()
}
bt.adjusted.momentum.test <- function()
{
load.packages('quantmod')
tickers = spl('SPY,^VIX')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', fill.gaps = T)
VIX = Cl(data$VIX)
bt.prep.remove.symbols(data, 'VIX')
prices = data$prices
models = list()
data$weight[] = NA
data$weight[] = iif(prices > SMA(prices, 200), 1, 0)
models$ma200 = bt.run.share(data, clean.signal=T)
roc = prices / mlag(prices) - 1
data$weight[] = NA
data$weight[] = iif(SMA(roc, 200) > 0, 1, 0)
models$roc200 = bt.run.share(data, clean.signal=T)
data$weight[] = NA
data$weight[] = iif(SMA(roc/VIX, 200) > 0, 1, 0)
models$vix.mom = bt.run.share(data, clean.signal=T)
forecast = SMA(roc,10)
error = roc - mlag(forecast)
mae = SMA(abs(error), 10)
data$weight[] = NA
data$weight[] = iif(SMA(roc/mae, 200) > 0, 1, 0)
models$er.mom = bt.run.share(data, clean.signal=T)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T)
dev.off()
}
scale.one <- function
(
x,
overlay = F,
main.index = which(!is.na(x[1,]))[1]
)
{
index = 1:nrow(x)
if( overlay )
x / rep.row(apply(x, 2,
function(v) {
i = index[!is.na(v)][1]
v[i] / as.double(x[i,main.index])
}
), nrow(x))
else
x / rep.row(apply(x, 2, function(v) v[index[!is.na(v)][1]]), nrow(x))
}
make.stock.xts <- function(out, column=1, ...) {
out = out[,column]
colnames(out) = 'Close'
out$Adjusted = out$Open = out$High = out$Low = out$Close
out$Volume = 0
return(out[,spl('Open,High,Low,Close,Volume,Adjusted')])
}
compute.cor <- function
(
data,
method = c("pearson", "kendall", "spearman")
)
{
cor(data, use='complete.obs',method=method[1])
}
proxy.test <- function(data.all, names = ls(data.all), price.fn=Ad)
{
data = new.env()
data$symbolnames = names
for(n in data$symbolnames)
data[[n]] = make.stock.xts( price.fn( data.all[[n]] ) )
bt.prep(data, align='remove.na')
prices = data$prices
layout(1:2, heights=c(4,1))
plota.matplot(scale.one(prices))
rets = (prices/mlag(prices)-1)[-1,]
temp = cor(rets, use='complete.obs', method='pearson')
diag(temp) = NA
temp[lower.tri(temp)] = NA
temp = temp[-nrow(temp),,drop=F]
temp[] = plota.format(100 * temp, 0, '', '%')
out = temp
temp = compute.stats( as.list(rets),
list(
Mean=function(x) 252*mean(x,na.rm=T),
StDev=function(x) sqrt(252)*sd(x,na.rm=T)
)
)
temp[] = plota.format(100 * temp, 1, '', '%')
out = rbind(out,NA,temp)
plot.table(out)
}
proxy.overlay.plot <- function(data.all, names = ls(data.all), price.fn=Ad)
{
data = new.env()
data$symbolnames = names
for(n in data$symbolnames)
data[[n]] = make.stock.xts( price.fn( data.all[[n]] ) )
bt.prep(data, align='keep.all')
prices = data$prices
prices = scale.one(prices, T)
layout(1)
plota.matplot(prices)
}
proxy.prices <- function(data, names = ls(data)) {
n.names = len(names)
temp = list()
layout(1:(n.names+1))
for(n in names) {
plota.matplot(cbind(Cl(data[[n]]),Ad(data[[n]])),main=n)
temp[[ paste(n, 'Price') ]] = Cl(data[[n]])
temp[[ paste(n, 'Total') ]] = Ad(data[[n]])
}
temp = compute.stats( lapply(temp, function(x) ifna(x/mlag(x) -1,NA)),
list(
Mean=function(x) 252*mean(x,na.rm=T),
StDev=function(x) sqrt(252)*sd(x,na.rm=T)
)
)
temp[] = plota.format(100 * temp, 1, '', '%')
plot.table(temp)
}
proxy.example.test <- function() {
load.packages('quantmod')
tickers = spl('GSG,DBC')
data = new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
temp = extract.table.from.webpage( join(readLines("TRJ_CRB")), 'EODValue' )
temp = join( apply(temp, 1, join, ','), '\n' )
data$CRB_1 = make.stock.xts( read.xts(temp, format='%m/%d/%y' ) )
data$CRB_2 = make.stock.xts( read.xts("prfmdata.csv", format='%m/%d/%Y' ) )
jpeg(filename = 'plot1.jpg', width = 500, height = 500, units = 'px', pointsize = 12)
proxy.test(data)
dev.off()
jpeg(filename = 'plot2.jpg', width = 500, height = 500, units = 'px', pointsize = 12)
proxy.overlay.plot(data)
dev.off()
load.packages('quantmod')
tickers = spl('IYR,VGSIX,RWO')
data = new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
jpeg(filename = 'plot3.jpg', width = 500, height = 500, units = 'px', pointsize = 12)
proxy.test(data)
dev.off()
jpeg(filename = 'plot4.jpg', width = 500, height = 500, units = 'px', pointsize = 12)
proxy.overlay.plot(data)
dev.off()
}
find.tokens <- function
(
txt,
marker,
pos = 1,
pos.start = T
)
{
marker = spl(marker)
for(i in 1:len(marker)) {
if( pos < 2 )
pos1 = regexpr(marker[i], txt)
else
pos1 = regexpr(marker[i], substr(txt, pos, nchar(txt)))
if( pos1 < 0 )
return(pos1)
else {
if( pos < 2 ) pos = pos1
else pos = pos1 + pos - 1
}
if( !pos.start ) pos = pos + attr(pos1, 'match.length')
}
return(pos)
}
extract.token <- function
(
txt,
smarker,
emarker,
pos = 1,
keep.marker = F
)
{
pos1 = 1
if (nchar(smarker) > 0)
pos1 = find.tokens(txt, smarker, pos, pos.start = keep.marker)
if( pos1 < 0 ) return("")
pos2 = nchar(txt)
if (nchar(emarker) > 0)
pos2 = find.tokens(txt, emarker, pos1, pos.start = !keep.marker) - 1
if( pos2 < 0 ) return("")
return(substr(txt,pos1,pos2))
}
remove.tags <- function
(
temp
)
{
temp = gsub(pattern = '<.*?>', replacement = '', temp, perl = TRUE)
temp = gsub(pattern = '\r', replacement = '', temp, perl = TRUE)
temp = gsub(pattern = '\n', replacement = '', temp, perl = TRUE)
temp = gsub(pattern = '\t', replacement = '', temp, perl = TRUE)
temp = gsub(pattern = '&nbsp;', replacement = '', temp, perl = TRUE)
temp = gsub(pattern = '&amp;', replacement = '', temp, perl = TRUE)
temp = gsub(pattern = '&raquo;', replacement = '', temp, perl = TRUE)
temp = gsub(pattern = '&#37;', replacement = '%', temp, perl = TRUE)
return(temp)
}
extract.table.from.webpage <- function
(
txt,
marker,
hasHeader=T
)
{
tryCatch({
marker = spl(marker)
pos1=1
for(i in 1:len(marker)) {
pos1 = regexpr(marker[i], substr(txt, pos1, nchar(txt))) + pos1
}
pos0 = tail(gregexpr('<table', substr(txt, 1, pos1))[[1]], 1)
pos2 = head(gregexpr('</table', substr(txt, pos1, nchar(txt)))[[1]], 1)
temp =  substr(txt, pos0, pos1 + pos2 - 2)
temp = gsub(pattern = '<br>', replacement = '', temp, perl = TRUE)
temp = gsub(pattern = '</tr>', replacement = ';row;', temp, perl = TRUE)
temp = gsub(pattern = '</td>', replacement = ';col;', temp, perl = TRUE)
temp = gsub(pattern = '</th>', replacement = ';col;', temp, perl = TRUE)
temp = gsub(pattern = '<.*?>', replacement = '', temp, perl = TRUE)
temp = gsub(pattern = '\r', replacement = '', temp, perl = TRUE)
temp = gsub(pattern = '\n', replacement = '', temp, perl = TRUE)
temp = gsub(pattern = '\t', replacement = '', temp, perl = TRUE)
temp = gsub(pattern = '&nbsp;', replacement = '', temp, perl = TRUE)
temp = gsub(pattern = '&amp;', replacement = '', temp, perl = TRUE)
temp = gsub(pattern = '&raquo;', replacement = '', temp, perl = TRUE)
temp = lapply( strsplit(temp, ';row;'), strsplit, ';col;')
n = max( sapply(temp[[1]], function(x) len(x)) )
temp = t( sapply(temp[[1]], function(x) x[1:n]) )
if(hasHeader) {
colnames(temp) = temp[(hasHeader + 0), ]
temp = temp[-c(1:(hasHeader + 0)), ,drop=F]
}
}, error = function(ex) {
temp <<- txt
}, finally = {
return(temp)
})
}
extract.table.from.webpage.test <- function()
{
load.packages('quantmod')
Symbol = 'IBM'
url = paste('http://finance.yahoo.com/q/ks?s=', Symbol, sep = '')
txt = join(readLines(url))
temp = extract.table.from.webpage(txt, 'Market Cap', hasHeader = F)
temp = rbind(c('', Symbol), temp)
data = getSymbols(Symbol, from = '1980-01-01', auto.assign = FALSE)
y = data['2010::2011']
sma50 = SMA(Cl(y), 50)
png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(c(1,1,2,3,3))
plota(y, type = 'candle', main = Symbol, plotX = F)
plota.lines(sma50, col='blue')
plota.legend(c(Symbol,'SMA 50'), 'green,blue', list(y,sma50))
y = plota.scale.volume(y)
plota(y, type = 'volume')
plot.table(temp)
dev.off()
}
PricingZeroCouponBond <- function
(
yield,
timetomaturity,
parvalue = 100
)
{
parvalue / ( 1 + yield ) ^ timetomaturity
}
processTBill <- function
(
yields,
timetomaturity = 1/4,
frequency = 365
)
{
yield = coredata(yields) / 100
pr = sapply( yield, function(x) PricingZeroCouponBond(x, timetomaturity) )
pr = ROC(pr, type='discrete')
pr[1] = 0
ir = (1+mlag(yield, nlag=1))^(1 / frequency)-1
ir[1] = 0
tr = pr + ir
close.price = cumprod(1 + pr)
adjusted.price = cumprod(1 + tr)
out = as.xts( cbind(close.price, adjusted.price), index(yields) )
colnames(out) = spl('Close,Adjusted')
return(out)
}
processTBill.test <- function()
{
quantmod::getSymbols("GS1", src = "FRED")
ir = (1 + mlag(GS1) / 100) ^ (1/12) - 1
ir[1] = 0
out = processTBill(GS1, timetomaturity = 1,12)
plota(cumprod(1 + ir), type='l', log = 'y')
plota.lines(Ad(out), type='l', col='red')
SHY = getSymbols('SHY', src='yahoo', auto.assign = FALSE)
tbill.m = quantmod::getSymbols('GS3', src='FRED', auto.assign = FALSE)
tbill.d = quantmod::getSymbols('DGS3', src='FRED', auto.assign = FALSE)
timetomaturity = 3
compute.raw.annual.factor(tbill.d)
compute.raw.annual.factor(tbill.m)
tbill.m = processTBill(tbill.m, timetomaturity = timetomaturity, 12)
tbill.d[] = ifna.prev(tbill.d)
tbill.d = processTBill(tbill.d, timetomaturity = timetomaturity,261)
dates = '2003::'
tbill.m = tbill.m[dates,2]
tbill.m = tbill.m / as.double(tbill.m[1])
tbill.d = tbill.d[dates,2]
tbill.d = tbill.d / as.double(tbill.d[1])
SHY = Ad(SHY[dates,])
SHY = SHY / as.double(SHY[1])
plota(tbill.d, type='l')
plota.lines(tbill.m, type='s', col='blue')
plota.lines(SHY, type='l', col='red')
plota.legend('Daily 3YR T-Bills,Monthly 3YR T-Bills,SHY','black,blue,red')
}
get.CRB <- function(...)
{
load.packages('gtools,gdata')
url = paste('http://www.jefferies.com/html/ProductsServices/SalesTrading/Commodities/scripts/genExcel.pl?Index=RJCRB_Total&StartDate=19940101&EndDate=', format(Sys.Date(), '%Y%m%d'), sep='')
temp = read.xls(url, ...)
temp = as.matrix(temp[-c(1:7),])
out = repmat(as.double(temp[,2]), 1, 6)
colnames(out) = spl('Open,High,Low,Close,Volume,Adjusted')
out[, 'Volume'] = 0
out = make.xts( out, as.POSIXct(temp[,1], tz = Sys.getenv('TZ'), format='%m/%d/%y'))
indexClass(out) = 'Date'
return(out)
}
get.CRB.test <- function()
{
CRB = get.CRB()
load.packages('quantmod')
tickers = spl('GSG,DBC')
getSymbols(tickers, src = 'yahoo', from = '1970-01-01')
out = na.omit(merge(Cl(CRB), Cl(GSG), Cl(DBC)))
colnames(out) = spl('CRB,GSG,DBC')
temp = out / t(repmat(as.vector(out[1,]),1,nrow(out)))
layout(1:2)
plota(temp, ylim=range(temp))
plota.lines(temp[,1],col=1)
plota.lines(temp[,2],col=2)
plota.lines(temp[,3],col=3)
plota.legend(colnames(temp),1:3)
temp = cor(temp / mlag(temp)- 1, use='complete.obs', method='pearson')
temp[] = plota.format(100 * temp, 0, '', '%')
plot.table(temp)
layout(1:3)
plota.matplot(CRB[,c('Close','Adjusted')])
plota.matplot(DBC[,c('DBC.Close','DBC.Adjusted')])
plota.matplot(GSG[,c('GSG.Close','GSG.Adjusted')])
layout(1)
comm = extend.data(DBC, CRB, scale=T)
plota(comm, type='l', col=1)
plota.lines(CRB*0.078, type='l', lwd=5, col=col.add.alpha(2,150))
plota.lines(DBC, type='l', lwd=5, col=col.add.alpha(3,150))
plota.lines(comm, type='l', col=1)
plota.legend('comm,CRB,DBC', 1:3, list(comm,CRB,DBC))
}
dow.jones.components <- function()
{
url = 'http://finance.yahoo.com/q/cp?s=^DJI+Components'
txt = join(readLines(url))
temp = extract.table.from.webpage(txt, 'Volume', hasHeader = T)
tickers = temp[, 'Symbol']
return(tickers)
}
nasdaq.100.components <- function()
{
url = 'http://www.nasdaq.com/markets/indices/nasdaq-100.aspx'
txt = join(readLines(url))
temp = extract.table.from.webpage(txt, 'Symbol', hasHeader = 2)
tickers = temp[, 'Symbol']
return(tickers)
}
sector.spdr.components <- function(sector.etf = 'XLE')
{
url = paste('http://www.sectorspdr.com/sectorspdr/IDCO.Client.Spdrs.Holdings/Export/ExportCsv?symbol=', sector.etf, sep='')
temp = read.csv(url, skip=1, header=TRUE, stringsAsFactors=F)
tickers = temp[, 'Symbol']
return(tickers)
}
sp500.components <- function()
{
url = 'http://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
txt = join(readLines(url))
temp = extract.table.from.webpage(txt, 'Ticker', hasHeader = T)
tickers = temp[, 'Ticker symbol']
sector = temp[, 'GICS Sector']
return(list(tickers=tickers, sector=sector))
}
sp100.components <- function()
{
url = 'http://www.barchart.com/stocks/sp100.php'
txt = join(readLines(url))
temp = extract.table.from.webpage(txt, 'Components', hasHeader = T)
i.start = grep('Name', temp[,2])
tickers = trim(temp[-c(1:i.start), 1])
return(tickers)
}
ftse100.components <- function()
{
url = 'http://uk.ishares.com/en/rc/products/ISF/all-holdings/'
txt = join(readLines(url))
txt = gsub('&#37;','%',txt)
temp = extract.table.from.webpage(txt, 'Security', hasHeader = T)
temp = trim(temp)
colnames(temp) = temp[1,]
temp = temp[-1,]
holdings = temp
page.label = ''
ticker2ISIN = c()
for(i in 1:100) {
cat(i,'\n')
url = paste('http://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/constituents-indices.html?index=UKX&page=', i, sep='')
txt = join(readLines(url))
pos = regexpr('Page [0-9]+ of [0-9]+', txt, ignore.case = T)
page.label.new = substr(txt, pos, pos + attr(pos, 'match.length')-1)
if(page.label == page.label.new) break
page.label = page.label.new
temp.table = extract.table.from.webpage(txt, 'Price', hasHeader = T)
colnames(temp.table)[1] = 'tickers'
temp = gsub(pattern = '<a', replacement = '<td>', txt, perl = TRUE)
temp = gsub(pattern = '</a>', replacement = '</td>', temp, perl = TRUE)
temp = extract.table.from.webpage(temp, 'Price', hasHeader = T)
pos = regexpr('fourWayKey=', temp[,2])
ISIN = as.vector(sapply(1:nrow(temp), function(j)
substr(temp[j,2], pos[j] + attr(pos, 'match.length')[j], pos[j] + attr(pos, 'match.length')[j] + 12 - 1)
))
ticker2ISIN = rbind(ticker2ISIN, cbind(temp.table[,spl('ticker,Name,Price'), drop=F], ISIN))
}
ISIN = intersect(holdings[,'ISIN'],ticker2ISIN[,'ISIN'])
holdings = cbind(holdings[match(ISIN, holdings[,'ISIN']), ],
ticker2ISIN[match(ISIN, ticker2ISIN[,'ISIN']), spl('ticker,Name,Price')])
return(apply(holdings, 2, list))
}
us.ishares.components <- function(Symbol = 'DVY', date = NULL, debug = F)
{
url = paste('http://us.ishares.com/product_info/fund/holdings/', Symbol, '.htm?periodCd=d', sep='')
if( !is.null(date) )
url = paste('http://us.ishares.com/product_info/fund/holdings/', Symbol, '.htm?asofDt=', date.end(date), '&periodCd=m', sep='')
txt = join(readLines(url))
temp = remove.tags(extract.token(txt, 'Holdings Detail', 'Holdings subject to change'))
date = as.Date(spl(trim(temp),' ')[3], '%m/%d/%Y')
temp = extract.table.from.webpage(txt, 'Symbol', hasHeader = T)
colnames(temp) = trim(colnames(temp))
temp = trim(temp)
tickers = temp[, 'Symbol']
keep.index = nchar(tickers)>1
weights = as.double(temp[keep.index, '% Net Assets']) / 100
tickers = tickers[keep.index]
out = list(tickers = tickers, weights = weights, date = date)
if(debug) out$txt = txt
out
}
google.search <- function
(
query
)
{
url = paste("http://google.com/search?ie=utf-8&oe=utf-8&q=", URLencode(query), "&num=10&gws_rd=cr", sep='')
txt = join(readLines(url))
tokens = spl(txt, '<li class="g">')
if(len(tokens) < 2) return(NULL)
records = matrix('', nrow=len(tokens)-1,nc=2)
colnames(records) = c('label','url')
for(i in 2:len(tokens)) {
token = tokens[i]
token = extract.token(token, '<a href=', '</a>', keep.marker = T)
url = extract.token(token, 'url\\?q=', '&amp;sa=U&amp;')
label = remove.tags(token)
records[i-1,] = c(label,url)
}
return(records)
}
getQuote.google <- function(tickers) {
url = paste('http://finance.google.com/finance/info?client=ig&q=', join(tickers,','), sep='')
txt = join(readLines(url))
temp = gsub(':', ',', txt)
temp = scan(text = temp, what='', sep=',', quiet=T)
temp = matrix(trim(temp), nr=len(temp)/len(tickers), byrow=F)
index = match(spl('t,l,lt'), tolower(temp[,1]))+1
names(index) = spl('ticker,last,date')
last = as.double(temp[index['last'],])
date = strptime(temp[index['date'],],format=' %b %d, %H,%M')
out = data.frame(last,date)
rownames(out) = temp[index['ticker'],]
out
}
getQuote.google.xml <- function(tickers) {
url = paste('http://www.google.com/ig/api?', paste('stock=',tickers, '&', sep='', collapse=''), sep='')
txt = join(readLines(url))
temp = txt
temp = gsub('<finance.*?>', '', temp, perl = TRUE)
temp = gsub('</finance>', '', temp, perl = TRUE)
temp = gsub('<xml.*?>', '', temp, perl = TRUE)
temp = gsub('</xml.*?>', '', temp, perl = TRUE)
temp = gsub('<\\?xml.*?>', '', temp, perl = TRUE)
temp = gsub('data=', '', temp, perl = TRUE)
temp = gsub('/><', ' ', temp)
temp = gsub('>', '', temp)
temp = gsub('<', '', temp)
temp = scan(text = temp, what='', sep=' ', quiet=T)
temp = matrix(trim(temp), nr=len(temp)/len(tickers), byrow=F)
cnames = spl('trade_date_utc,trade_time_utc,symbol,last,high,low,volume,open,avg_volume,market_cap,y_close')
index = match(cnames, tolower(temp[,1]))+1
names(index) = cnames
date = strptime(paste(temp[index['trade_date_utc'],], temp[index['trade_time_utc'],]), format='%Y%m%d %H%M%S',tz='UTC')
date = as.POSIXct(date, tz = Sys.getenv('TZ'))
out = data.frame(t(temp[index[-c(1:3)],]))
colnames(out) = cnames[-c(1:3)]
rownames(out) = temp[index['symbol'],]
out
}
extend.GLD <- function(GLD) {
extend.data(GLD, bundes.bank.data.gold(), scale=T)
}
extend.SLV <- function(SLV) {
extend.data(SLV, KITCO.data('Silver'))
}
KITCO.data <- function
(
symbol = spl('Gold.AM,Gold.PM,Silver,Platinum.AM,Platinum.PM,Palladium.AM,Palladium.PM')
)
{
url = 'http://wikiposit.org/w?action=dl&dltypes=comma%20separated&sp=daily&uid=KITCO'
temp = read.csv(url, skip=4, header=TRUE, stringsAsFactors=F)
hist = make.xts(as.double(temp[,symbol]), as.POSIXct(temp[,1], tz = Sys.getenv('TZ'), format='%d-%b-%Y'))
indexClass(hist) = 'Date'
colnames(hist)='Close'
return( hist[!is.na(hist)] )
}
extend.data <- function
(
current,
hist,
scale = F
)
{
colnames(current) = sapply(colnames(current), function(x) last(spl(x,'\\.')))
colnames(hist) = sapply(colnames(hist), function(x) last(spl(x,'\\.')))
close.index = find.names('Close', colnames(hist))$Close
if(is.na(close.index)) close.index = 1
adjusted.index = find.names('Adjusted', colnames(hist))$Adjusted
if(is.na(adjusted.index)) adjusted.index = close.index
if(scale) {
common = merge(Cl(current), hist[,close.index], join='inner')
scale = as.numeric(common[1,1]) / as.numeric(common[1,2])
if( close.index == adjusted.index )
hist = hist * scale
else {
hist[,-adjusted.index] = hist[,-adjusted.index] * scale
common = merge(Ad(current), hist[,adjusted.index], join='inner')
scale = as.numeric(common[1,1]) / as.numeric(common[1,2])
hist[,adjusted.index] = hist[,adjusted.index] * scale
}
}
hist = hist[format(index(current[1])-1,'::%Y:%m:%d'),,drop=F]
if( ncol(hist) != ncol(current) )
hist = make.xts( rep.col(hist[,close.index], ncol(current)), index(hist))
else
hist = hist[, colnames(current)]
colnames(hist) = colnames(current)
rbind( hist, current )
}
bundes.bank.data <- function(symbol) {
url = paste('http://www.bundesbank.de/cae/servlet/CsvDownload?tsId=', symbol, '&its_csvFormat=en&mode=its', sep='')
temp = read.csv(url, skip=5, header=F, stringsAsFactors=F)
hist = make.xts(as.double(temp[,2]), as.POSIXct(temp[,1], tz = Sys.getenv('TZ'), format='%Y-%m-%d'))
indexClass(hist) = 'Date'
colnames(hist)='Close'
return( hist[!is.na(hist)] )
}
bundes.bank.data.gold <- function() {
bundes.bank.data('BBEX3.D.XAU.USD.EA.AC.C05')
}
fx.sauder.data <- function(start.year, end.year, base.cur, target.curs) {
url = paste('http://fx.sauder.ubc.ca/cgi/fxdata?b=', base.cur, join(paste('&c=', spl(target.curs), sep='')), '&rd=&fd=1&fm=1&fy=', start.year, '&ld=31&lm=12&ly=', end.year, '&y=daily&q=volume&f=csv&o=', sep='')
temp = read.csv(url, skip=1, header=T, stringsAsFactors=F)
hist = make.xts(as.matrix(temp[,-c(1:3)]), as.POSIXct(temp[,2], tz = Sys.getenv('TZ'), format='%Y/%m/%d'))
indexClass(hist) = 'Date'
colnames(hist) = gsub(paste('.', base.cur, sep=''), '', colnames(hist))
return( hist[!is.na(hist[,1]),] )
}
getSymbols.PI <- function
(
Symbols,
env = .GlobalEnv,
auto.assign = TRUE,
download = TRUE
)
{
temp.folder = paste(getwd(), 'temp', sep='/')
dir.create(temp.folder, F)
for (i in 1:len(Symbols)) {
if(download) {
url = paste('http://pitrading.com/free_eod_data/', Symbols[i], '.zip', sep='')
filename = paste(temp.folder, '/', Symbols[i], '.zip', sep='')
download.file(url, filename,  mode = 'wb')
unzip(filename, exdir=temp.folder)
}
filename = paste(temp.folder, '/', Symbols[i], '.txt', sep='')
temp = read.delim(filename, header=TRUE, sep=',')
out = make.xts(temp[,-1], as.POSIXct(temp[,1], tz = Sys.getenv('TZ'), format='%m/%d/%Y'))
indexClass(out) = 'Date'
out$Adjusted = out$Close
cat(i, 'out of', len(Symbols), 'Reading', Symbols[i], '\n', sep='\t')
if (auto.assign) {
assign(paste(gsub('\\^', '', Symbols[i]), sep='_'), out, env)
}
}
if (!auto.assign) {
return(out)
} else {
return(env)
}
}
getSymbols.fxhistoricaldata <- function
(
Symbols,
type = spl('hour,day'),
env = .GlobalEnv,
auto.assign = TRUE,
download = FALSE
)
{
type = type[1]
temp.folder = paste(getwd(), 'temp', sep='/')
dir.create(temp.folder, F)
for (i in 1:len(Symbols)) {
if(download) {
url = paste('http://www.fxhistoricaldata.com/download/', Symbols[i], '?t=', type, sep='')
filename = paste(temp.folder, '/', Symbols[i], '_', type, '.zip', sep='')
download.file(url, filename,  mode = 'wb')
unzip(filename, exdir=temp.folder)
}
filename = paste(temp.folder, '/', Symbols[i], '_', type, '.csv', sep='')
temp = read.delim(filename, header=TRUE, sep=',')
colnames(temp) = gsub('[X\\.|\\.]', '', colnames(temp))
out = make.xts(temp[,spl('OPEN,LOW,HIGH,CLOSE')],
strptime(paste(temp$DATE, temp$TIME), format='%Y%m%d %H:%M:%S'))
cat(i, 'out of', len(Symbols), 'Reading', Symbols[i], '\n', sep='\t')
if (auto.assign) {
assign(paste(gsub('\\^', '', Symbols[i]), type, sep='_'), out, env)
}
}
if (!auto.assign) {
return(out)
} else {
return(env)
}
}
get.G10 <- function
(
type = spl('currency')
)
{
if( type[1] != 'currency') {
cat('Warning:', type[1], 'is not yet implemented in getG10 function\n')
return()
}
map = '
FX          FX.NAME
DEXUSAL     U.S./Australia
DEXUSUK     U.S./U.K.
DEXCAUS     Canada/U.S.
DEXNOUS     Norway/U.S.
DEXUSEU     U.S./Euro
DEXJPUS     Japan/U.S.
DEXUSNZ     U.S./NewZealand
DEXSDUS     Sweden/U.S.
DEXSZUS     Switzerland/U.S.
'
map = matrix(scan(text = map, what='', quiet=T), nc=2, byrow=T)
colnames(map) = map[1,]
map = data.frame(map[-1,], stringsAsFactors=F)
convert.index = grep('DEXUS',map$FX, value=T)
load.packages('quantmod')
data.fx <- new.env()
quantmod::getSymbols(map$FX, src = 'FRED', from = '1970-01-01', env = data.fx, auto.assign = T)
for(i in convert.index) data.fx[[i]] = 1 / data.fx[[i]]
bt.prep(data.fx, align='remove.na')
fx = bt.apply(data.fx, '[')
return(fx)
}
getSymbols.TB <- function(
env = .GlobalEnv,
auto.assign = TRUE,
download = FALSE,
type = c('Both', 'Futures', 'Forex'),
rm.index =  'PB',
clean = FALSE
)
{
if(download) {
download.file('http://www.tradingblox.com/Data/DataOnly.zip', 'DataOnly.zip')
}
temp.folder = paste(getwd(), 'temp', sep='/')
dir.create(temp.folder, F)
temp.folder = paste(getwd(), '/', 'temp', sep='')
if(clean) shell('del /F /S /Q temp\\*.*', wait = TRUE)
files = unzip('DataOnly.zip', exdir=temp.folder)
def1 = try(read.csv('http://www.tradingblox.com/tradingblox/CSIUA/FuturesInfo.txt',skip=1,header=FALSE, stringsAsFactors=F),TRUE)
if(inherits(def1, 'try-error')) def1 = read.csv('FuturesInfo.txt',skip=1,header=FALSE, stringsAsFactors=F)
def1 = def1[-match(rm.index, def1[,1]),]
def1[,3] = 'Futures'
def2 = try(read.csv('http://www.tradingblox.com/tradingblox/CSIUA/ForexInfo.txt',skip=1,header=FALSE, stringsAsFactors=F),TRUE)
if(inherits(def2, 'try-error')) def2 = read.csv('ForexInfo.txt',skip=1,header=FALSE, stringsAsFactors=F)
def2[,3] = 'Forex'
def = rbind(def1[,1:4], def2[,1:4])
if(type[1] == 'Futures') def = def1[,1:4]
if(type[1] == 'Forex') def = def2[,1:4]
for( i in 1:nrow(def) ) {
symbol = def[i,1]
filename = paste(temp.folder, '/', def[i,3], '/', def[i,4], sep='')
if(file.exists(filename)) {
fr <- read.csv(filename, header = FALSE)
fr <- make.xts(fr[,-1], as.Date(as.character(fr[,1]),'%Y%m%d'))
colnames(fr) <- spl('Open,High,Low,Close,Volume,OpenInterest,DeliveryMonth,Unadjusted')[1:ncol(fr)]
fr$Adjusted = fr$Close
if (auto.assign) assign(symbol, fr, env)
cat(i, 'out of', nrow(def), 'Reading', symbol, format(index.xts(fr)[1],'%Y%m%d'), format(index.xts(fr)[nrow(fr)],'%Y%m%d'), '\n', sep='\t')
} else {
cat('\t\t\t Missing data for ', symbol, '\n');
}
}
index = match(ls(env)[ na.omit(match(def[,1], ls(env))) ], def[,1])
temp = def[index,1]
names(temp) = def[index,1]
env$symbolnames = temp
temp = def[index,2]
names(temp) = def[index,1]
env$symbol.descriptions = temp
temp = def[index,3]
names(temp) = def[index,1]
env$symbol.groups = temp
names = trim(gsub(pattern = '\\(.*?\\)', replacement = '', env$symbol.descriptions, perl = TRUE))
names = trim(gsub('-NYMEX','',names,ignore.case =T))
names = trim(gsub('-COMEX','',names,ignore.case =T))
names = trim(gsub('-CBT','',names,ignore.case =T))
names = trim(gsub('-CME-','',names,ignore.case =T))
names = trim(gsub('-CME','',names,ignore.case =T))
names = trim(gsub('-NYCE','',names,ignore.case =T))
names = trim(gsub('-Globex','',names,ignore.case =T))
names = trim(gsub('-FINEX','',names,ignore.case =T))
names = trim(gsub('-CSCE','',names,ignore.case =T))
names = trim(gsub(' w/Prj A','',names,ignore.case =T))
env$symbol.descriptions.print = names
data = env
for(i in data$symbolnames[data$symbol.groups != 'Forex']) {
spot = as.vector(data[[i]]$Unadjusted)
dspot = spot - mlag(spot)
futures = as.vector(data[[i]]$Adjusted)
dfutures = futures - mlag(futures)
index = which(round(dspot - dfutures,4) != 0 )
spot.adjust.roll = spot
spot.adjust.roll[(index-1)] = spot.adjust.roll[index] - dfutures[index]
reta = (mlag(spot.adjust.roll) + futures - mlag(futures)) / mlag(spot.adjust.roll)
reta[1] = 1
n = len(spot)
new.series = cumprod(reta)
data[[i]]$Close = spot[n] * new.series / new.series[n]
data[[i]]$Adjusted	= data[[i]]$Close
}
if (!auto.assign) {
return(fr)
} else {
return(env)
}
}
get.fama.french.data <- function(
name = c('F-F_Research_Data_Factors', 'F-F_Research_Data_Factors'),
periodicity = c('days','weeks', 'months'),
download = FALSE,
clean = FALSE
)
{
map = c('_daily', '_weekly', '')
names(map) = c('days','weeks', 'months')
period = ifna(map[periodicity[1]], periodicity[1])
filename.zip = paste(name[1], period, '.zip', sep='')
filename.txt = paste(name[1], period, '.txt', sep='')
url = paste('http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/', filename.zip, sep='')
if(download) {
download.file(url, filename.zip)
}
temp.folder = paste(getwd(), 'temp', sep='/')
dir.create(temp.folder, F)
temp.folder = paste(getwd(), '/', 'temp', sep='')
if(clean) shell('del /F /S /Q temp\\*.*', wait = TRUE)
files = unzip(filename.zip, exdir=temp.folder)
filename = paste(temp.folder, '/', filename.txt, sep='')
out = readLines(filename)
index = which(nchar(out) == 0)
data.index = grep('^[ 0-9\\.\\+-]+$', out)
temp.index = which(diff(data.index) > 1)
data.index = matrix(data.index[sort(c(1, temp.index, temp.index+1, len(data.index)))], nc=2, byrow=T)
data = list()
for(i in 1:nrow(data.index)) {
start.index = index[which( index > data.index[i,1] ) - 1][1] + 1
if(is.na(start.index)) start.index = index[len(index)] + 1
end.index = data.index[i,1] - 1
n.index = end.index - start.index + 1
name = 'data'
colnames = scan(text = out[start.index], what='', quiet=T)
if(n.index == 2) {
name = trim(out[start.index])
colnames = scan(text = out[end.index], what='', quiet=T)
} else if(n.index > 2) {
name = trim(out[start.index])
colnames0 = scan(text = out[(end.index-1)], what='', quiet=T)
colnames1 = scan(text = out[end.index], what='', quiet=T)
colnames = paste(rep(colnames0, each = len(colnames1) / len(colnames0)), colnames1, sep='.')
}
colnames = gsub('-', '.', colnames)
temp =  matrix(scan(filename, what = double(), quiet=T,
skip = (data.index[i,1]-1),
nlines = (data.index[i,2] - data.index[i,1]+1))
, nc=len(colnames)+1, byrow=T)
date.format = '%Y%m%d'
date.format.add = ''
date.format.n = nchar(paste(temp[1,1]))
if( date.format.n == 6 ) {
date.format.add = '01'
} else if( date.format.n == 4 ) {
date.format.add = '0101'
}
data[[name]] = make.xts(temp[,-1], as.Date(paste(temp[,1], date.format.add, sep=''),date.format))
colnames(data[[name]]) = colnames
}
return( data )
}
get.FOMC.dates <- function
(
download = TRUE,
fomc.filename = 'FOMC.Rdata'
)
{
if(!download && file.exists(fomc.filename)) {
load(file=fomc.filename)
return(FOMC)
}
url = 'http://www.federalreserve.gov/monetarypolicy/fomccalendars.htm'
txt = join(readLines(url))
data = c()
for(year in 2009:(1 + date.year(Sys.Date()))) {
temp = extract.table.from.webpage(txt, paste(year,'FOMC Meetings'))
if(nrow(temp) == 0) next
temp = tolower(trim(temp[,1:2]))
temp = temp[nchar(temp[,1]) > 0,]
month = temp[,1]
day = gsub('\\(','',gsub('\\)','',temp[,2]))
day = trim(day)
status = rep('', len(day))
index = grep('\\*',day)
if(any(index)) status[index] = '*'
index = grep(' ',day)
if(any(index)) for(j in index) status[j] = spl(day[j],' ')[2]
day = gsub('\\*','', sapply(day,function(x) spl(x,' ')[1]))
temp = apply(cbind(day, month, status), 1, function(x) paste(year, spl(x[2],'/'), spl(x[1],'-'), '|', x[3])	)
data = cbind(data, trim(sapply(unlist(temp),spl,'\\|')))
}
recent.days = as.Date(data[1,],'%Y %B %d')
status = as.vector(data[2,])
data = c()
for(year in 1936:2008) {
cat(year,'\n')
url = paste0('http://www.federalreserve.gov/monetarypolicy/fomchistorical', year, '.htm')
txt = join(readLines(url))
tokens = spl(txt,'<div id="historical">')
days = c()
for(token in tokens[-1])
days = c(days,colnames(extract.table.from.webpage(token, 'table'))[1])
data = rbind(data, cbind(year, days))
}
day = tolower(data[,2])
day = gsub(',', '-', gsub('and', '', gsub('conference call', '', gsub('meeting','',day))))
day = unlist(lapply(day, function(x) join(rev(rev(spl(x,' '))[-1]),' ')))
temp = unlist(apply(cbind(day,data[,1]),1, function(x) paste(trim(spl(x[1],'-')),x[2]) ))
temp = sapply(lapply(temp,spl,' '), function(x) iif(len(x)==3,x,c(NA,x)))
temp[1,] = ifna.prev(temp[1,])
days = as.Date(apply(temp,2,join,' '),'%B %d %Y ')
FOMC = list(day = c(days, recent.days), status=c(rep('',len(days)), status))
save(FOMC,file=fomc.filename)
FOMC
}
getSymbol.intraday.google <- function
(
Symbol,
Exchange,
interval = 60,
period = '1d'
)
{
url = paste('http://www.google.com/finance/getprices?q=', Symbol,
'&x=', Exchange,
'&i=', interval,
'&p=', period,
'&f=', 'd,o,h,l,c,v', sep='')
load.packages('data.table')
out = fread(url, stringsAsFactors=F)
if(ncol(out) < 5) {
cat('Error getting data from', url, '\n')
return(NULL)
}
setnames(out, spl('Date,Open,High,Low,Close,Volume'))
date = out$Date
date.index = substr(out$Date,1,1) == 'a'
date = as.double(gsub('a','',date))
temp = NA * date
temp[date.index] = date[date.index]
temp = ifna.prev(temp)
date = temp + date * interval
date[date.index] = temp[date.index]
class(date) = c("POSIXt", "POSIXct")
date = date - (as.double(format(date[1],'%H')) - 9)*60*60
make.xts(out[, eval(expression(list( Open,High,Low,Close,Volume )))], date)
}
data.clean <- function
(
data,
min.ratio = 2.5,
min.obs = 3*252
)
{
index = names(which(lapply(data,function(x) count(x$Close)) < min.obs))
if (len(index) > 0) {
cat('Removing', index, 'have less than', min.obs, 'observations','\n')
rm(list=index, envir=data)
}
for(ticker in ls(data)) {
ticker.data = data[[ticker]]
ticker.data = ticker.data[ticker.data$Close > 0 & ticker.data$Adjusted > 0]
nperiods = nrow(ticker.data)
price = ticker.data$Adjusted
ratio = as.vector((price)/mlag(price))
index = which(ratio > min.ratio)
if(len(index) > 0)
for(i in index) {
cat('Abnormal price found for', ticker, format(index(ticker.data)[i],'%d-%b-%Y'),'Ratio :', round(ratio[i],1),'\n')
for(name in spl('Open,Close,High,Low,Adjusted'))
ticker.data[i:nperiods,name] = ticker.data[i:nperiods,name] / ratio[i]
}
ratio = as.vector(mlag(price)/(price))
index = which(ratio > min.ratio)
if(len(index) > 0)
for(i in index) {
cat('Abnormal price found for', ticker, format(index(ticker.data)[i],'%d-%b-%Y'),'Inverse Ratio :', round(ratio[i],1),'\n')
for(name in spl('Open,Close,High,Low,Adjusted'))
ticker.data[i:nperiods,name] = ticker.data[i:nperiods,name] * ratio[i]
}
data[[ticker]] = ticker.data
}
}
consecutive.changes <- function
(
data,
positive=T
)
{
if(positive) dir = diff(data) > 0 else dir = diff(data) < 0
temp = cumsum(iif(dir, 1, 0))
temp - ifna.prev(iif(dir, NA, coredata(temp)))
}
factor.avgcor <- function(data, next.month.ret, name) {
load.packages('abind')
temp = abind(data, along = 3)
temp = abind(next.month.ret, temp, along = 3)
dimnames(temp)[[3]][1] = 'Ret'
temp = t(compute.avgcor(temp, 'spearman')[,-1])
temp[] = plota.format(100 * temp, 0, '', '%')
plot.table(temp, smain=paste(name,'Correlation',sep=' \n '))
}
compute.avgcor <- function
(
data,
method = c('pearson', 'kendall', 'spearman')
)
{
nr = dim(data)[1]
nc = dim(data)[3]
corm = matrix(NA,nc,nc)
colnames(corm) = rownames(corm) = dimnames(data)[[3]]
for( i in 1:(nc-1) ) {
for( j in (i+1):nc ) {
corm[i,j] = mean( as.double( sapply(1:nr, function(t)
try(cor(data[t,,i], data[t,,j], use = 'complete.obs', method[1]),TRUE)
)), na.rm=T)
}
}
return(corm)
}
cap.weighted.mean <- function
(
data,
capitalization
)
{
capitalization = capitalization * (!is.na(data))
weight = capitalization / rowSums(capitalization,na.rm=T)
rowSums(data * weight,na.rm=T)
}
sector.mean <- function
(
data,
sectors
)
{
out = data * NA
for(sector in levels(sectors)) {
index = (sector == sectors)
out[,index] = ifna(apply(data[,index, drop=F], 1, mean, na.rm=T),NA)
}
return(out)
}
compute.quantiles <- function
(
data,
next.month.ret,
smain='',
n.quantiles=5,
plot=T
)
{
n = ncol(data)
nperiods = nrow(data)
data = coredata(ifna(data,NA))
next.month.ret = coredata(ifna(next.month.ret,NA))
temp = matrix(NA, nperiods, n.quantiles)
hist.factor.quantiles = hist.ret.quantiles = temp
temp = matrix(NA, nperiods, n)
quantiles = weights = ranking = temp
index = which(rowSums(!is.na(data)) >= n.quantiles)
for(t in index) {
factor = data[t,]
ret = next.month.ret[t,]
ranking[t,] = rank(factor, na.last = 'keep','first')
t.ranking = ceiling(n.quantiles * ranking[t,] / count(factor))
quantiles[t,] = t.ranking
weights[t,] = 1/tapply(rep(1,n), t.ranking, sum)[t.ranking]
hist.factor.quantiles[t,] = tapply(factor, t.ranking, mean)
hist.ret.quantiles[t,] = tapply(ret, t.ranking, mean)
}
if(plot) {
par(mar=c(4,4,2,1))
temp = 100*apply(hist.ret.quantiles,2,mean,na.rm=T)
barplot(temp, names.arg=paste(1:n.quantiles), ylab='%',
main=paste(smain, ', spread =',round(temp[n.quantiles]-temp[1],2), '%'))
}
return(list(quantiles=quantiles, weights=weights, ranking=ranking,
hist.factor.quantiles = hist.factor.quantiles, hist.ret.quantiles = hist.ret.quantiles))
}
add.avg.factor <- function
(
data
)
{
temp = abind(data, along = 3)
data$AVG = data[[1]]
data$AVG[] = ifna(apply(temp, c(1,2), mean, na.rm=T),NA)
return(data)
}
normalize.mkval <- function
(
data,
MKVAL
)
{
for(i in names(data)) {
data[[i]] = (data[[i]] - cap.weighted.mean(data[[i]], MKVAL)) /
apply(data[[i]], 1, sd, na.rm=T)
}
return(data)
}
normal.transform <- function(data)
{
rk=rank(data, na.last='keep', ties.method = 'first')
n = count(data)
x = qnorm((1:n) / (n+1))
return(x[rk])
}
normalize.normal <- function
(
data
)
{
for(i in names(data)) {
data[[i]][] = t(apply(data[[i]], 1, normal.transform))
}
return(data)
}
plot.quantiles <- function
(
data,
next.month.ret,
smain=''
)
{
layout(matrix(1:(2*ceiling(len(data)/2)), nc=2))
sapply(1:len(data), function(i)
compute.quantiles(data[[i]], next.month.ret, paste(names(data)[i],smain))
)
}
plot.bt.quantiles <- function
(
factors,
next.month.ret,
smain='',
data
)
{
out = compute.quantiles(factors, next.month.ret, plot=F)
prices = data$prices
prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
month.ends = endpoints(prices, 'months')
models = list()
for(i in 1:5) {
data$weight[] = NA
data$weight[month.ends,] = iif(out$quantiles == i, out$weights, 0)
capital = 100000
data$weight[] = (capital / prices) * (data$weight)
models[[paste('Q',i,sep='')]] = bt.run(data, type='share', capital=capital)
}
data$weight[] = NA
data$weight[month.ends,] = iif(out$quantiles == 5, out$weights,
iif(out$quantiles == 1, -out$weights, 0))
capital = 100000
data$weight[] = (capital / prices) * (data$weight)
models$Q5_Q1 = bt.run(data, type='share', capital=capital)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main=smain)
mtext('Cumulative Performance', side = 2, line = 1)
}
plot.factors <- function
(
data,
name,
next.month.ret
)
{
x = as.vector(t(data))
y = as.vector(t(next.month.ret))
x = ifna(x,NA)
y = ifna(y,NA)
index = !is.na(x) & !is.na(y)
x = x[index]
y = y[index]
cor.p = round(100*cor(x, y, use = 'complete.obs', method = 'pearson'),1)
cor.s = round(100*cor(x, y, use = 'complete.obs', method = 'spearman'),1)
layout(1:2)
plot(x, pch=20)
par(mar=c(4,4,2,1))
plot(x, y, pch=20, xlab=name, ylab='Next Month Return')
abline(lm(y ~ x), col='blue', lwd=2)
plota.legend(paste('Pearson =',cor.p,',Spearman =', cor.s))
}
fm.fund.data.test <- function()
{
symbol = 'WMT'
symbol = paste(iif( nchar(symbol) <= 3, 'NYSE:', 'NASDAQ:'), symbol, sep='')
fund = fund.data(symbol, 80)
fund.date = date.fund.data(fund)
total.capitalization = get.fund.data('total capitalization', fund, fund.date)
barplot(total.capitalization)
EPS.Q = as.double(fund['Diluted EPS from Total Operations',])
EPS.Q = as.xts(EPS.Q, fund.date)
EPS = runSum(EPS.Q, 4)
EPS.Q = get.fund.data('Diluted EPS from Total Operations', fund, fund.date)
EPS = get.fund.data('Diluted EPS from Total Operations', fund, fund.date, is.12m.rolling=T)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
par(mar=c(2,2,2,1))
x = barplot(EPS.Q, main='Wal-Mart Quarterly Earnings per share', border=NA)
text(x, EPS.Q, fund['quarterly indicator',], adj=c(0.5,-0.3), cex=0.8, xpd = TRUE)
barplot(EPS, main='Wal-Mart Rolling Annual Earnings per share', border=NA)
dev.off()
load.packages('quantmod')
tickers = 'WMT'
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
data$WMT = merge(data$WMT, EPS)
data$WMT$EPS = ifna.prev(coredata(data$WMT$EPS))
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
y = data$WMT['1990::']
plota(Cl(y), type = 'l', LeftMargin=3)
plota2Y(y$EPS, type='l', las=1, col='red', col.axis = 'red')
plota.legend('WMT(rhs),WMT.EPS(lhs)', 'blue,red', list(Cl(y),y$EPS))
dev.off()
load.packages('quantmod')
tickers = dow.jones.components()
data.fund <- new.env()
temp = paste(iif( nchar(tickers) <= 3, 'NYSE:', 'NASDAQ:'), tickers, sep='')
for(i in 1:len(tickers)) data.fund[[tickers[i]]] = fund.data(temp[i], 80)
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
for(i in tickers) {
fund = data.fund[[i]]
fund.date = date.fund.data(fund)
EPS.Q = as.double(fund['Diluted EPS from Total Operations',])
EPS.Q = as.xts(EPS.Q, fund.date)
EPS = runSum(EPS.Q, 4)
data[[i]] = merge(data[[i]], EPS)
}
bt.prep(data, align='keep.all', dates='1995::2011')
prices = data$prices
prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
factors = list()
EPS = bt.apply(data, function(x) ifna.prev(x[, 'EPS']))
factors$EP = EPS / prices
volume = bt.apply(data, function(x) ifna.prev(Vo(x)))
factors$VOMO = (prices / mlag(prices,10) - 1) * bt.apply.matrix(volume, runMean, 22) / bt.apply.matrix(volume, runMean, 66)
month.ends = endpoints(prices, 'months')
prices = prices[month.ends,]
n = ncol(prices)
nperiods = nrow(prices)
ret = prices / mlag(prices) - 1
next.month.ret = mlag(ret, -1)
factors$EP = factors$EP[month.ends,]
factors$VOMO = factors$VOMO[month.ends,]
x = as.vector(factors$EP)
y = as.vector(next.month.ret)
cor.test(x, y, use = 'complete.obs', method = 'pearson')
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
par(mar=c(4,4,2,1))
plot(x, y, pch=20, main='Correlation Analysis for EP factor', xlab='EP', ylab='Next Month Return')
abline(lm(y ~ x), col='blue', lwd=2)
dev.off()
}
fm.fund.factor.test <- function()
{
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
tickers.desc = spl('ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities')
sector.map = c()
for(i in 1:len(tickers)) {
sector.map = rbind(sector.map,
cbind(sector.spdr.components(tickers[i]), tickers.desc[i])
)
}
colnames(sector.map) = spl('ticker,sector')
load.packages('quantmod,abind')
tickers = dow.jones.components()
sectors = factor(sector.map[ match(tickers, sector.map[,'ticker']), 'sector'])
names(sectors) = tickers
if(FALSE) {
data.fund <- new.env()
temp = paste(iif( nchar(tickers) <= 3, 'NYSE:', 'NASDAQ:'), tickers, sep='')
for(i in 1:len(tickers)) data.fund[[tickers[i]]] = fund.data(temp[i], 80)
save(data.fund, file='data.fund.Rdata')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
save(data, file='data.Rdata')
} else {
load(file='data.fund.Rdata')
load(file='data.Rdata')
}
for(i in tickers) {
fund = data.fund[[i]]
fund.date = date.fund.data(fund)
EPS = get.fund.data('Diluted EPS from Total Operations', fund, fund.date, is.12m.rolling=T)
SALE = get.fund.data('total revenue', fund, fund.date, is.12m.rolling=T)
CSHO = get.fund.data('total common shares out', fund, fund.date)
CEQ = get.fund.data('total equity', fund, fund.date)
DV.PS = get.fund.data('dividends paid per share', fund, fund.date, is.12m.rolling=T)
CFL = get.fund.data('net cash from operating activities', fund, fund.date, cash.flow=T, is.12m.rolling=T)
data[[i]] = merge(data[[i]], EPS, SALE, CSHO, CEQ, DV.PS, CFL)
}
bt.prep(data, align='keep.all', dates='1995::2011')
prices = data$prices
prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
sectors	= sectors[colnames(prices)]
factors = list()
factors$TV = list()
CSHO =  bt.apply(data, function(x) ifna.prev(x[, 'CSHO']))
MKVAL = prices * CSHO
EPS = bt.apply(data, function(x) ifna.prev(x[, 'EPS']))
factors$TV$EP = EPS / prices
SALE = bt.apply(data, function(x) ifna.prev(x[, 'SALE']))
factors$TV$SP = SALE / MKVAL
CFL = bt.apply(data, function(x) ifna.prev(x[, 'CFL']))
factors$TV$CFP = CFL / MKVAL
DV.PS = bt.apply(data, function(x) ifna.prev(x[, 'DV.PS']))
factors$TV$DY = DV.PS / prices
CEQ = bt.apply(data, function(x) ifna.prev(x[, 'CEQ']))
factors$TV$BP = CEQ	/ MKVAL
factors$TV$SP[, sectors == 'Financials'] = NA
factors$TV$CFP[, sectors == 'Financials'] = NA
month.ends = endpoints(prices, 'months')
prices = prices[month.ends,]
n = ncol(prices)
nperiods = nrow(prices)
ret = prices / mlag(prices) - 1
next.month.ret = mlag(ret, -1)
MKVAL = MKVAL[month.ends,]
for(j in 1:len(factors)) {
for(i in 1:len(factors[[j]])) {
factors[[j]][[i]] = factors[[j]][[i]][month.ends,]
}
}
sapply(factors$TV, count)
for(i in names(factors$TV)) {
factors$TV[[i]] = (factors$TV[[i]] - cap.weighted.mean(factors$TV[[i]], MKVAL)) /
apply(factors$TV[[i]], 1, sd, na.rm=T)
}
load.packages('abind')
temp = abind(factors$TV, along = 3)
factors$TV$AVG = factors$TV[[1]]
factors$TV$AVG[] = apply(temp, c(1,2), mean, na.rm=T)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(matrix(1:6,nc=2))
sapply(1:len(factors$TV), function(i)
compute.quantiles(factors$TV[[i]], next.month.ret, paste(names(factors$TV)[i], 'Traditional Value'))
)
dev.off()
out = compute.quantiles(factors$TV$AVG, next.month.ret, plot=F)
prices = data$prices
prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
models = list()
for(i in 1:5) {
data$weight[] = NA
data$weight[month.ends,] = iif(out$quantiles == i, out$weights, 0)
capital = 100000
data$weight[] = (capital / prices) * (data$weight)
models[[paste('Q',i,sep='')]] = bt.run(data, type='share', capital=capital)
}
data$weight[] = NA
data$weight[month.ends,] = iif(out$quantiles == 5, out$weights,
iif(out$quantiles == 1, -out$weights, 0))
capital = 100000
data$weight[] = (capital / prices) * (data$weight)
models$Q5_Q1 = bt.run(data, type='share', capital=capital)
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)
mtext('Cumulative Performance', side = 2, line = 1)
dev.off()
}
fm.all.factor.test <- function()
{
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
tickers.desc = spl('ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities')
sector.map = c()
for(i in 1:len(tickers)) {
sector.map = rbind(sector.map,
cbind(sector.spdr.components(tickers[i]), tickers.desc[i])
)
}
colnames(sector.map) = spl('ticker,sector')
load.packages('quantmod,abind')
tickers = dow.jones.components()
sectors = factor(sector.map[ match(tickers, sector.map[,'ticker']), 'sector'])
names(sectors) = tickers
if(FALSE) {
data.fund <- new.env()
temp = paste(iif( nchar(tickers) <= 3, 'NYSE:', 'NASDAQ:'), tickers, sep='')
for(i in 1:len(tickers)) data.fund[[tickers[i]]] = fund.data(temp[i], 80)
save(data.fund, file='data.fund.Rdata')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
save(data, file='data.Rdata')
} else {
load(file='data.fund.Rdata')
load(file='data.Rdata')
}
data.clean(data, min.ratio=3)
tickers = ls(data)
for(i in tickers) {
fund = data.fund[[i]]
fund.date = date.fund.data(fund)
nperiods = ncol(fund)
D = list()
D$EPS = get.fund.data('Diluted EPS from Total Operations', fund, fund.date, is.12m.rolling=T)
D$SALE = get.fund.data('total revenue', fund, fund.date, is.12m.rolling=T)
D$CSHO = get.fund.data('total common shares out', fund, fund.date)
D$CEQ = get.fund.data('total equity', fund, fund.date)
D$DV.PS = get.fund.data('dividends paid per share', fund, fund.date, is.12m.rolling=T)
D$CFL = get.fund.data('net cash from operating activities', fund, fund.date, cash.flow=T, is.12m.rolling=T)
D$CFL.CON.CHG = consecutive.changes(D$CFL)
D$EPS.CON.CHG = consecutive.changes(D$EPS)
temp = get.fund.data('net cash from operating activities', fund, fund.date, cash.flow=T)
D$CFL.CHG = temp / mlag(temp,4)
D$SALE.3YR.GR = D$SALE
if(!all(is.na(D$SALE))) D$SALE.3YR.GR = SMA(ifna(D$SALE / mlag(D$SALE,4) - 1,NA), 3*4)
D$EPS.3YR.GR = SMA(D$EPS / mlag(D$EPS,4) - 1, 3*4)
D$EPS.TREND = D$EPS * NA
D$EPS.TREND[12:nperiods] = sapply(12:nperiods, function(i) beta.degree(ols(cbind(1,1:12), D$EPS[(i-12+1):i])$coefficients[2]))
D$CFL.TREND = D$CFL * NA
D$CFL.TREND[4:nperiods] = sapply(4:nperiods, function(i) beta.degree(ols(cbind(1,1:4), D$CFL[(i-4+1):i])$coefficients[2]))
RECT = get.fund.data('receivables', fund, fund.date)
INVT = get.fund.data('inventories', fund, fund.date)
D$AT = get.fund.data('total assets', fund, fund.date)
XSGA = get.fund.data('Selling, General & Administrative (SG&A) Expense', fund, fund.date, is.12m.rolling=T)
D$RS.CON.CHG = consecutive.changes((RECT + INVT) / D$SALE, F)
D$CS.CON.CHG = consecutive.changes(D$CFL/D$SALE)
D$OS.CON.CHG = consecutive.changes(XSGA/D$SALE, F)
D$RS = (RECT + INVT) / D$SALE
D$SA = D$SALE / D$AT
D$OS = XSGA / D$SALE
D$ES = D$EPS / D$SALE
temp = abind(D,along=2)
colnames(temp) = names(D)
data[[i]] = merge(adjustOHLC(data[[i]], use.Adjusted=T), as.xts(temp, fund.date))
}
bt.prep(data, align='keep.all', fill.gaps = T, dates='1995::')
prices = data$prices
prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
sectors	= sectors[colnames(prices)]
factors = list()
factor.names = list()
factors$TV = list()
factor.names$TV = 'Traditional Value'
CSHO =  bt.apply(data, function(x) ifna.prev(x[, 'CSHO']))
MKVAL = prices * CSHO
EPS = bt.apply(data, function(x) ifna.prev(x[, 'EPS']))
factors$TV$EP = EPS / prices
SALE = bt.apply(data, function(x) ifna.prev(x[, 'SALE']))
factors$TV$SP = SALE / MKVAL
CFL = bt.apply(data, function(x) ifna.prev(x[, 'CFL']))
factors$TV$CFP = CFL / MKVAL
DV.PS = bt.apply(data, function(x) ifna.prev(x[, 'DV.PS']))
factors$TV$DY = DV.PS / prices
CEQ = bt.apply(data, function(x) ifna.prev(x[, 'CEQ']))
factors$TV$BP = CEQ	/ MKVAL
factors$TV$SP[, sectors == 'Financials'] = NA
factors$TV$CFP[, sectors == 'Financials'] = NA
factors$HG = list()
factor.names$HG = 'Historical Growth'
for(i in spl('CFL.CON.CHG,EPS.CON.CHG,CFL.CHG,SALE.3YR.GR,EPS.3YR.GR,EPS.TREND,CFL.TREND')) {
factors$HG[[i]] = bt.apply(data, function(x) ifna.prev(x[, i]))
}
factors$PT = list()
factor.names$PT = 'Profit Trends'
for(i in spl('RS.CON.CHG,CS.CON.CHG,OS.CON.CHG,RS,SA,OS,ES')) {
factors$PT[[i]] = bt.apply(data, function(x) ifna.prev(x[, i]))
}
factors$PM = list()
factor.names$PM = 'Price Momentum'
week.ends = endpoints(prices, 'weeks')
week.prices = prices[week.ends,]
week.nperiods = nrow(week.prices)
factors$PM$S52W.TREND = bt.apply.matrix(week.prices, function(x) {
c(rep(NA,51),
sapply(52:week.nperiods, function(i) beta.degree(ols(cbind(1,1:52), x[(i - 52 + 1):i])$coefficients[2]))
)})
factors$PM$PP04.52W = bt.apply.matrix(week.prices, EMA, 4) / bt.apply.matrix(week.prices, EMA, 52)
factors$PM$R39W = week.prices / mlag(week.prices,39)
temp = bt.apply(data, function(x) cumsum(ifna(Vo(x),0)))
temp = temp[week.ends,]
week.volume = temp - mlag(temp)
temp = (week.prices - mlag(week.prices)) * week.volume
factors$PM$VPT51W = bt.apply.matrix(temp, runSum, 51)
for(i in 1:len(factors$PM)) {
temp = prices * NA
temp[week.ends,] = factors$PM[[i]]
factors$PM[[i]] = bt.apply.matrix(temp, function(x) ifna.prev(x))
}
factors$PM$P260LOW = prices / bt.apply.matrix(prices, runMin, 260)
for(i in names(factors$PM)) factors$PM[[i]] = -factors$PM[[i]]
factors$PR = list()
factor.names$PR = 'Price Reversal'
factors$PR$r5DR = prices/mlag(prices,5)
factors$PR$r5DR = factors$PR$r5DR / sector.mean(factors$PR$r5DR, sectors)
factors$PR$MFV = bt.apply(data, function(x) {
MFI(cbind(ifna.prev(Hi(x)),ifna.prev(Lo(x)),ifna.prev(Cl(x))),5) / ifna.prev(Vo(x))
})
factors$PR$MACD = bt.apply.matrix(prices, function(x) {
temp=MACD(x,10)
temp[,'macd'] - temp[,'signal']
})
factors$PR$RSI = bt.apply.matrix(prices, RSI, 14)
factors$PR$STOCH = bt.apply(data, function(x) {
stoch(cbind(ifna.prev(Hi(x)),ifna.prev(Lo(x)),ifna.prev(Cl(x))),14)[,'slowD']
})
factors$PR$rR4W = week.prices / mlag(week.prices,4)
factors$PR$rR4W = factors$PR$rR4W / sector.mean(factors$PR$rR4W, sectors)
temp = prices * NA
temp[week.ends,] = factors$PR$rR4W
factors$PR$rR4W = bt.apply.matrix(temp, function(x) ifna.prev(x))
volume = bt.apply(data, function(x) ifna.prev(Vo(x)))
factors$PR$VOMO = (prices / mlag(prices,10) - 1) * bt.apply.matrix(volume, runMean, 22) / bt.apply.matrix(volume, runMean, 66)
for(i in names(factors$PR)) factors$PR[[i]] = -factors$PR[[i]]
factors$SS = list()
factor.names$SS = 'Small Size'
factors$SS$MC = log(MKVAL)
factors$SS$MC3 = log(MKVAL)^3
factors$SS$P = log(prices)
factors$SS$AT = log(bt.apply(data, function(x) ifna.prev(x[, 'AT'])))
factors$SS$SALE = log(bt.apply(data, function(x) ifna.prev(x[, 'SALE'])))
for(i in names(factors$SS)) factors$SS[[i]] = -factors$SS[[i]]
month.ends = endpoints(prices, 'months')
prices = prices[month.ends,]
n = ncol(prices)
nperiods = nrow(prices)
ret = prices / mlag(prices) - 1
next.month.ret = mlag(ret, -1)
MKVAL = MKVAL[month.ends,]
for(j in 1:len(factors)) {
for(i in 1:len(factors[[j]])) {
factors[[j]][[i]] = factors[[j]][[i]][month.ends,]
factors[[j]][[i]][] = ifna(factors[[j]][[i]],NA)
}
}
factors$RV = list()
factor.names$RV = 'Relative Value'
for(i in spl('EP,SP,CFP')) {
factors$RV[[paste('r',i,sep='')]] = factors$TV[[i]] / sector.mean(factors$TV[[i]], sectors)
}
for(i in spl('rEP,rSP,rCFP')) {
factors$RV[[paste('s',i,sep='')]] = factors$RV[[i]] -
bt.apply.matrix(factors$RV[[i]], function(x) if(all(is.na(x))) x else SMA(x,60)[1:len(x)])
}
for(i in spl('RS,SA')) {
factors$PT[[paste('r',i,sep='')]] = factors$PT[[i]] / sector.mean(factors$PT[[i]], sectors)
}
for(j in 1:len(factors)) {
for(i in 1:len(factors[[j]])) {
factors[[j]][[i]][] = ifna(factors[[j]][[i]],NA)
}
}
for(j in names(factors)) {
factors[[j]] = normalize.normal(factors[[j]])
factors[[j]] = add.avg.factor(factors[[j]])
}
factors.avg = list()
for(j in names(factors)) factors.avg[[j]] = factors[[j]]$AVG
factors.avg = add.avg.factor(factors.avg)
png(filename = 'plot1.png', width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')
plot.quantiles(factors.avg, next.month.ret, 'Average')
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot.bt.quantiles(factors.avg$AVG, next.month.ret, 'Composite Average', data)
dev.off()
save(data, sectors, factors.avg, next.month.ret, file='data.factors.Rdata')
factors.avg = factors.avg[which(names(factors.avg) != 'AVG')]
nperiods = nrow(next.month.ret)
n = ncol(next.month.ret)
factors.matrix = abind(factors.avg, along = 3)
all.data = factors.matrix
beta = all.data[,1,] * NA
all.data = abind(next.month.ret, all.data, along = 3)
dimnames(all.data)[[3]][1] = 'Ret'
for(t in 12:(nperiods-1)) {
temp = all.data[t:t,,]
x = temp[,-1]
y = temp[,1]
beta[(t+1),] = lm(y~x-1)$coefficients
}
alpha = next.month.ret * NA
for(t in 18:(nperiods-1)) {
coef = colMeans(beta[(t-5):t,],na.rm=T)
coef = iif(coef > 0, coef, 0)
alpha[t,] = rowSums(all.data[t,,-1] * t(repmat(coef, 1,n)), na.rm=T)
}
png(filename = 'plot4.png', width = 600, height = 800, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
temp = compute.quantiles(alpha, next.month.ret, plot=T)
plot.bt.quantiles(alpha, next.month.ret, 'Alpha', data)
dev.off()
}
fm.risk.model.test <- function()
{
load.packages('quantmod,abind')
load(file='data.factors.Rdata')
factors.avg = factors.avg[which(names(factors.avg) != 'AVG')]
nperiods = nrow(next.month.ret)
n = ncol(next.month.ret)
nsectors = len(levels(sectors))
sectors.matrix = array(double(), c(nperiods, n, nsectors))
dimnames(sectors.matrix)[[3]] = levels(sectors)
for(j in levels(sectors)) {
sectors.matrix[,,j] = matrix(sectors == j,  nr=nperiods, nc=n, byrow=T)
}
factors.matrix = abind(factors.avg, along = 3)
all.data = abind(sectors.matrix, factors.matrix)
beta = all.data[,1,] * NA
specific.return = next.month.ret * NA
nfactors = ncol(beta)
all.data = abind(next.month.ret, all.data, along = 3)
dimnames(all.data)[[3]][1] = 'Ret'
for(t in 12:(nperiods-1)) {
temp = all.data[t:t,,]
x = temp[,-c(1:2)]
y = temp[,1]
b = lm(y~x)$coefficients
b[2:nsectors] = b[1] + b[2:nsectors]
beta[(t+1),] = b
specific.return[(t+1),] = y - rowSums(temp[,-1] * matrix(b, n, nfactors, byrow=T), na.rm=T)
}
fm.hist.plot <- function(temp, smain=NULL) {
ntemp = ncol(temp)
cols = plota.colors(ntemp)
plota(temp, ylim = range(temp), log='y', main=smain)
for(i in 1:ntemp) plota.lines(temp[,i], col=cols[i])
plota.legend(colnames(temp), cols, as.list(temp))
}
temp = make.xts(beta, index(next.month.ret))
temp = temp['2000::',]
temp[] = apply(coredata(temp), 2, function(x) cumprod(1 + ifna(x,0)))
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
fm.hist.plot(temp[,-c(1:nsectors)], 'Factor Returns')
dev.off()
load.packages('BurStFin')
factor.covariance = array(double(), c(nperiods, nfactors, nfactors))
dimnames(factor.covariance)[[2]] = colnames(beta)
dimnames(factor.covariance)[[3]] = colnames(beta)
for(t in 36:nperiods) {
factor.covariance[t,,] = var.shrink.eqcor(beta[(t-23):t,])
}
load.packages('tseries,fGarch')
specific.variance = next.month.ret * NA
for(i in 1:n) specific.variance[,i] = bt.forecast.garch.volatility(specific.return[,i], 24)
hist.specific.variance = next.month.ret * NA
for(i in 1:n) hist.specific.variance[,i] = runSD(specific.return[,i], 24)
specific.variance[] = ifna(coredata(specific.variance), coredata(hist.specific.variance))
save(all.data, factor.covariance, specific.variance, file='risk.model.Rdata')
portfolio = rep(1/n, n)
portfolio = matrix(portfolio, n, nfactors)
portfolio.risk = next.month.ret[,1] * NA
for(t in 36:(nperiods-1)) {
portfolio.exposure = colSums(portfolio * all.data[t,,-1], na.rm=T)
portfolio.risk[t] = sqrt(
portfolio.exposure %*% factor.covariance[t,,] %*% (portfolio.exposure) +
sum(specific.variance[t,]^2 * portfolio[,1]^2, na.rm=T)
)
}
portfolio = rep(1/n, n)
portfolio = t(matrix(portfolio, n, nperiods))
portfolio.returns = next.month.ret[,1] * NA
portfolio.returns[] = rowSums(mlag(next.month.ret) * portfolio, na.rm=T)
hist.portfolio.risk = runSD(portfolio.returns, 24)
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plota(portfolio.risk['2000::',], type='l')
plota.lines(hist.portfolio.risk, col='blue')
plota.legend('Risk,Historical Risk', 'black,blue')
dev.off()
}
fm.risk.model.optimization.test <- function()
{
load.packages('quantmod')
load(file='data.factors.Rdata')
nperiods = nrow(next.month.ret)
n = ncol(next.month.ret)
tickers = colnames(next.month.ret)
load(file='risk.model.Rdata')
load.packages('quadprog,corpcor')
temp = last(mlag(next.month.ret),24)
cov.temp = cov(temp, use='complete.obs', method='pearson')
hist.cov = cov.temp
if(!is.positive.definite(cov.temp)) {
cov.temp <- make.positive.definite(cov.temp, 0.000000001)
}
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(rep(1,n), 1, type = '=', constraints)
sol = solve.QP.bounds(Dmat = cov.temp, dvec = rep(0, nrow(cov.temp)) ,
Amat=constraints$A, bvec=constraints$b, constraints$meq,
lb = constraints$lb, ub = constraints$ub)
print(sqrt(sol$value))
x = round(sol$solution,4)
x = iif(x < 0, 0, x)
names(x) = colnames(next.month.ret)
hist.min.var.portfolio = x
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
barplot(100*x,las = 2,
main = 'Minimum variance portfolio weights (sample covariance matrix)')
dev.off()
t = nperiods
factor.exposures = all.data[t,,-1]
nfactors = ncol(factor.exposures)
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(rep(1,n), 1, type = '=', constraints)
constraints = add.variables(nfactors, constraints)
constraints = add.constraints(rbind(factor.exposures, -diag(nfactors)), rep(0, nfactors), type = '=', constraints)
temp = diag(n)
diag(temp) = specific.variance[t,]^2
cov.temp = diag(n + nfactors)
cov.temp[1:n,1:n] = temp
cov.temp[(n+1):(n+nfactors),(n+1):(n+nfactors)] = factor.covariance[t,,]
load.packages('quadprog')
sol = solve.QP.bounds(Dmat = cov.temp, dvec = rep(0, nrow(cov.temp)) ,
Amat=constraints$A, bvec=constraints$b, constraints$meq,
lb = constraints$lb, ub = constraints$ub)
print(sqrt(sol$value))
x = round(sol$solution,4)[1:n]
x = iif(x < 0, 0, x)
names(x) = colnames(next.month.ret)
risk.model.min.var.portfolio = x
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
barplot(100*x,las = 2,
main = 'Minimum variance portfolio weights (multiple factor risk model)')
dev.off()
fm.risk.cov(hist.min.var.portfolio, hist.cov)
fm.risk.cov(risk.model.min.var.portfolio, hist.cov)
fm.risk.model(hist.min.var.portfolio, factor.exposures, factor.covariance[t,,], specific.variance[t,])
fm.risk.model(risk.model.min.var.portfolio, factor.exposures, factor.covariance[t,,], specific.variance[t,])
}
fm.risk.cov <- function(portfolio, cov) {
sqrt(portfolio %*% cov %*% portfolio)
}
fm.risk.model <- function
(
portfolio,
factor.exposures,
factor.covariance,
specific.variance
)
{
factor.exposures = ifna(factor.exposures, 0)
specific.variance = ifna(specific.variance, mean(coredata(specific.variance), na.rm=T))
portfolio.exposure = portfolio %*% factor.exposures
sqrt(
portfolio.exposure %*% factor.covariance %*% t(portfolio.exposure) +
sum(specific.variance^2 * portfolio^2)
)[1]
}
fm.long.short.test <- function()
{
load.packages('quantmod')
load(file='data.factors.Rdata')
nperiods = nrow(next.month.ret)
tickers = colnames(next.month.ret)
n = len(tickers)
load(file='risk.model.Rdata')
factor.exposures = all.data[,,-1]
factor.names = dimnames(factor.exposures)[[3]]
nfactors = len(factor.names)
ret = mlag(next.month.ret)
beta = ret * NA
benchmark = ntop(ret, n)
benchmark.ret = rowSums(benchmark * ret, na.rm=T)
for(t in 24:nperiods) {
t.index = (t-23):t
benchmark.var = var( benchmark.ret[t.index], na.rm=T )
t.count = count(ret[t.index, ])
t.cov = cov( ifna(ret[t.index,], 0), benchmark.ret[t.index], use='complete.obs' )
beta[t,] = iif(t.count > 20, t.cov/benchmark.var, NA)
}
load.packages('quadprog,corpcor,kernlab')
weight = NA * next.month.ret
weights = list()
weights$benchmark = ntop(beta, n)
weights$long.alpha = weight
for(t in 36:nperiods) {
constraints = new.constraints(n, lb = 0, ub = 10/100)
constraints = add.constraints(rep(1,n), type = '=', b = 1, constraints)
constraints = add.constraints(ifna(as.vector(beta[t,]),0), type = '=', b = 1, constraints)
constraints = add.variables(nfactors, constraints)
constraints = add.constraints(rbind(ifna(factor.exposures[t,,], 0), -diag(nfactors)), rep(0, nfactors), type = '=', constraints)
temp = diag(n)
diag(temp) = ifna(specific.variance[t,], mean(coredata(specific.variance[t,]), na.rm=T))^2
cov.temp = diag(n + nfactors)
cov.temp[1:n,1:n] = temp
cov.temp[(n+1):(n+nfactors),(n+1):(n+nfactors)] = factor.covariance[t,,]
risk.aversion = 0.0075
alpha = factors.avg$AVG[t,] / 5
expected.return = c(ifna(coredata(alpha),0), rep(0, nfactors))
index = which(is.na(beta[t,]))
if( len(index) > 0) {
constraints$ub[index] = 0
constraints$lb[index] = 0
}
sol = solve.QP.bounds(Dmat = 2* risk.aversion * cov.temp, dvec = expected.return,
Amat = constraints$A, bvec = constraints$b,
meq = constraints$meq, lb = constraints$lb, ub = constraints$ub)
weights$long.alpha[t,] = sol$solution[1:n]
cat(t, '\n')
}
weights$long.short.alpha = weight
for(t in 36:nperiods) {
constraints = new.constraints(n, lb = -10/100, ub = 10/100)
constraints = add.constraints(rep(1,n), type = '=', b = 1, constraints)
constraints = add.constraints(ifna(as.vector(beta[t,]),0), type = '=', b = 1, constraints)
constraints = add.variables(nfactors, constraints)
constraints = add.constraints(rbind(ifna(factor.exposures[t,,], 0), -diag(nfactors)), rep(0, nfactors), type = '=', constraints)
constraints = add.variables(n, constraints)
constraints = add.constraints(rbind(diag(n), matrix(0,nfactors,n)  ,diag(n)), rep(0, n), type = '>=', constraints)
constraints = add.constraints(rbind(diag(n), matrix(0,nfactors,n), -diag(n)), rep(0, n), type = '<=', constraints)
constraints = add.constraints(c(rep(0, n), rep(0, nfactors), rep(1, n)), 1.6, type = '=', constraints)
temp = diag(n)
diag(temp) = ifna(specific.variance[t,], mean(coredata(specific.variance[t,]), na.rm=T))^2
cov.temp = 0*diag(n + nfactors + n)
cov.temp[1:n,1:n] = temp
cov.temp[(n+1):(n+nfactors),(n+1):(n+nfactors)] = factor.covariance[t,,]
risk.aversion = 0.0075
alpha = factors.avg$AVG[t,] / 5
expected.return = c(ifna(coredata(alpha),0), rep(0, nfactors), rep(0, n))
index = which(is.na(beta[t,]))
if( len(index) > 0) {
constraints$ub[index] = 0
constraints$lb[index] = 0
}
sol = solve.QP.bounds(Dmat = 2* risk.aversion * cov.temp, dvec = expected.return,
Amat = constraints$A, bvec = constraints$b,
meq = constraints$meq, lb = constraints$lb, ub = constraints$ub)
weights$long.short.alpha[t,] = sol$solution[1:n]
cat(t, '\n')
}
weights$long.min.var.alpha = weight
for(t in 36:nperiods) {
constraints = new.constraints(n, lb = 0, ub = 10/100)
constraints = add.constraints(rep(1,n), type = '=', b = 1, constraints)
constraints = add.variables(nfactors, constraints)
constraints = add.constraints(rbind(ifna(factor.exposures[t,,], 0), -diag(nfactors)), rep(0, nfactors), type = '=', constraints)
temp = diag(n)
diag(temp) = ifna(specific.variance[t,], mean(coredata(specific.variance[t,]), na.rm=T))^2
cov.temp = diag(n + nfactors)
cov.temp[1:n,1:n] = temp
cov.temp[(n+1):(n+nfactors),(n+1):(n+nfactors)] = factor.covariance[t,,]
alpha = factors.avg$AVG[t,] / 5
expected.return = c(ifna(coredata(alpha),0), rep(0, nfactors))
index = which(is.na(beta[t,]))
if( len(index) > 0) {
constraints$ub[index] = 0
constraints$lb[index] = 0
}
sol = solve.QP.bounds(Dmat = cov.temp, dvec = 0 * expected.return,
Amat = constraints$A, bvec = constraints$b,
meq = constraints$meq, lb = constraints$lb, ub = constraints$ub)
weights$long.min.var.alpha[t,] = sol$solution[1:n]
cat(t, '\n')
}
weights$market.neutral.alpha = weight
for(t in 36:nperiods) {
constraints = new.constraints(2*n, lb = 0, ub = c(rep(0.1,n), rep(0.1,n)))
constraints = add.constraints(c(rep(1,n), -rep(1,n)), 0, type = '=', constraints)
constraints = add.constraints(c(rep(1,n), rep(1,n)), 2, type = '=', constraints)
temp = ifna(as.vector(beta[t,]),0)
constraints = add.constraints(c(temp, -temp), type = '=', b = 0, constraints)
constraints = add.variables(nfactors, constraints)
temp = ifna(factor.exposures[t,,], 0)
constraints = add.constraints(rbind(temp, -temp, -diag(nfactors)), rep(0, nfactors), type = '=', constraints)
constraints = add.variables(n, constraints)
constraints$binary.index = (2*n + nfactors + 1):(3*n + nfactors)
constraints = add.constraints(rbind(diag(n), 0*diag(n), matrix(0,nfactors,n), -diag(n)), rep(0, n), type = '<=', constraints)
constraints = add.constraints(rbind(0*diag(n), diag(n), matrix(0,nfactors,n), diag(n)), rep(1, n), type = '<=', constraints)
alpha = factors.avg$AVG[t,] / 5
temp = ifna(coredata(alpha),0)
expected.return = c(temp, -temp, rep(0, nfactors), rep(0, n))
temp = diag(n)
diag(temp) = ifna(specific.variance[t,], mean(coredata(specific.variance[t,]), na.rm=T))^2
temp = cbind( rbind(temp, -temp), rbind(-temp, temp) )
cov.temp = 0*diag(2*n + nfactors + n)
cov.temp[1:(2*n),1:(2*n)] = temp
cov.temp[(2*n+1):(2*n+nfactors),(2*n+1):(2*n+nfactors)] = factor.covariance[t,,]
if(!is.positive.definite(cov.temp)) {
cov.temp <- make.positive.definite(cov.temp, 0.000000001)
}
risk.aversion = 0.0075
index = which(is.na(beta[t,]))
if( len(index) > 0) {
constraints$ub[index] = 0
constraints$lb[index] = 0
constraints$ub[2*index] = 0
constraints$lb[2*index] = 0
}
sol = solve.QP.bounds(Dmat = 2* risk.aversion * cov.temp, dvec = expected.return,
Amat = constraints$A, bvec = constraints$b,
meq = constraints$meq, lb = constraints$lb, ub = constraints$ub,
binary.vec = constraints$binary.index)
if(constraints$binary.index[1] != 0) cat(sol$counter,'QP calls made to solve problem with', len(constraints$binary.index), 'binary variables using Branch&Bound', '\n')
x = sol$solution[1:n] - sol$solution[(n+1):(2*n)]
weights$market.neutral.alpha[t,] = x
cat(t, '\n')
}
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
layout(1:5)
for(i in names(weights)) plotbt.transition.map(weights[[i]], i)
dev.off()
prices = data$prices
prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
month.ends = endpoints(prices, 'months')
models = list()
for(i in names(weights)) {
data$weight[] = NA
data$weight[month.ends,] = weights[[i]]
capital = 100000
data$weight[] = (capital / prices) * (data$weight)
models[[i]] = bt.run(data, type='share', capital=capital)
}
models = rev(models)
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models, dates='1998::')
dev.off()
png(filename = 'plot3.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(models)
dev.off()
png(filename = 'plot4.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
dev.off()
}
fm.hist.dow.jones.components <- function()
{
tickers = spl('MMM	AA	MO	AXP	T	BAC	CAT	CVX	CSCO	C	KO	DD	XOM	GE	HPQ	HD	HON	INTC	IBM	IP	JNJ	JPM	KFT	MCD	MRK	MSFT	PFE	PG	BA	TRV	UTX	VZ	WMT	DIS', '\t')
data =
'8-Jun-09	1	1	0	1	1	1	1	1	1	0	1	1	1	1	1	1	0	1	1	0	1	1	1	1	1	1	1	1	1	1	1	1	1	1
22-Sep-08	1	1	0	1	1	1	1	1	0	1	1	1	1	1	1	1	0	1	1	0	1	1	1	1	1	1	1	1	1	0	1	1	1	1
19-Feb-08	1	1	0	1	1	1	1	1	0	1	1	1	1	1	1	1	0	1	1	0	1	1	0	1	1	1	1	1	1	0	1	1	1	1
21-Nov-05	1	1	1	1	1	0	1	0	0	1	1	1	1	1	1	1	1	1	1	0	1	1	0	1	1	1	1	1	1	0	1	1	1	1
8-Apr-04	1	1	1	1	1	0	1	0	0	1	1	1	1	1	1	1	1	1	1	0	1	1	0	1	1	1	1	1	1	0	1	1	1	1
27-Jan-03	1	1	1	1	1	0	1	0	0	1	1	1	1	1	1	1	1	1	1	1	1	1	0	1	1	1	0	1	1	0	1	0	1	1
1-Nov-99	1	1	1	1	1	0	1	0	0	1	1	1	1	1	1	1	1	0	1	1	1	1	0	1	1	1	0	1	1	0	1	0	1	1
17-Mar-97	1	1	1	1	1	0	1	1	0	1	1	1	1	1	1	0	0	0	1	1	1	1	0	1	1	0	0	1	1	0	1	0	1	1
6-May-91	1	1	1	1	1	0	1	1	0	1	1	1	1	1	1	0	0	0	1	1	1	1	0	1	1	0	0	1	1	0	1	0	1	1'
hist = matrix( spl( gsub('\n', '\t', data), '\t'), nrow = len(spl(data, '\n')), byrow=TRUE)
hist = as.xts( matrix(as.double(hist[,-1]),nr=nrow(hist)), as.Date(hist[,1], '%d-%b-%y'))
colnames(hist) = tickers
return(hist)
}
fund.data <- function
(
Symbol,
n=10,
mode=c('quarterly','annual'),
max.attempts=5
)
{
all.data = c()
option.value = -1
start_date = spl('istart_date,start_date')
names(start_date) = spl('quarterly,annual')
repeat {
if(option.value >= 0) {
url = paste('http://uk.advfn.com/p.php?pid=financials&symbol=', Symbol, '&btn=', mode[1], '_reports&', start_date[mode[1]], '=', option.value, sep = '')
} else {
url = paste('http://uk.advfn.com/p.php?pid=financials&symbol=', Symbol, '&btn=', mode[1], '_reports', sep = '')
}
cat('Downloading', url, '\n')
for(iattempt in 1:max.attempts) {
flag = T
tryCatch({
txt = join(readLines(url))
}, interrupt = function(ex) {
flag <<-  F
Sys.sleep(0.1)
}, error = function(ex) {
flag <<-  F
Sys.sleep(0.1)
}, finally = {
if(flag) break
})
}
if( len(grep('INDICATORS', txt, ignore.case = T)) == 0 ) {
cat('No Data Found for', Symbol, '\n')
return(all.data)
}
pos = regexpr(pattern = '<title>(.*?)</title>', txt, ignore.case = TRUE, perl = TRUE)
if(len(pos) == 1)
title = substr(txt, attr(pos, 'capture.start'), attr(pos, 'capture.start') + attr(pos, 'capture.length') - 1)
data = extract.table.from.webpage(txt, 'INDICATORS', hasHeader = T)
colnames(data) = data[1,]
rownames(data) = data[,1]
data = data[,-1,drop=F]
add.index = which( is.na(match( colnames(data), colnames(all.data) )) )
all.data = cbind(data[,add.index,drop=F], all.data)
if(ncol(all.data) >= n) break
if(option.value == 0)  break
temp = gsub(pattern = '<option', replacement = '<tr>', txt, perl = TRUE)
temp = gsub(pattern = '</option>', replacement = '</tr>', temp, perl = TRUE)
temp = extract.table.from.webpage(temp, 'All amounts', hasHeader = T)
temp = apply(temp,1,join)
index.selected = grep('selected', temp)
option.value = 0
if(	len(index.selected) )
option.value = as.double( gsub('.*value=\'([0-9]*).*', '\\1', temp[index.selected]) )
if(option.value > 0) {
option.value = option.value - 5
option.value = max(0, option.value)
} else {
break
}
}
all.data = all.data[, colSums(nchar(trim(all.data))) > 0, drop=F]
all.data = rbind(all.data, title)
rownames(all.data)[nrow(all.data)] = 'HTMLTITLEtext'
if( ncol(all.data) > n ) {
return(all.data[,(ncol(all.data)-n+1):ncol(all.data), drop=F])
} else {
return(all.data)
}
}
date.fund.data <- function(data)
{
quarter.end.date = as.Date(paste(data[1,], '/1', sep=''), '%Y/%m/%d')
quarterly.indicator = data['quarterly indicator',]
date.preliminary.data.loaded = as.Date(data['date preliminary data loaded',], '%Y-%m-%d') + 1
months = seq(quarter.end.date[1], tail(quarter.end.date,1)+365, by='1 month')
index = match(quarter.end.date, months)
quarter.end.date = months[ iif(quarterly.indicator == '4', index+3, index+2) + 1 ] - 1
fund.date = date.preliminary.data.loaded
fund.date[is.na(fund.date)] = quarter.end.date[is.na(fund.date)]
return(fund.date)
}
get.fund.data.index <- function
(
label,
fund,
silent = T
)
{
names = rownames(fund)
index = grep(label, names, ignore.case = T)
if( len(index) == 0 ) {
labels = spl(label,' ')
n = len(labels)
temp.count = rep(0,nrow(fund))
for(ilabel in labels) {
index = grep(ilabel, rownames(fund), ignore.case = T)
if(len(index)>0) temp.count[index] = temp.count[index]+1
}
index = which(temp.count == n)
if( !silent ) cat('Exact label not found, trying partial match\n')
}
if( len(index) > 0 ) {
if( len(index) > 1 ) {
if( !silent ) cat('Possible Matches', rownames(fund)[index], '\n', sep=' | ')
index = index[ which.min(nchar(names[index]) - nchar(label)) ]
}
if( !silent ) cat('Match =', rownames(fund)[index], '\n')
index[1]
} else {
if( !silent ) cat('No Match Found for', label, '\n')
c()
}
}
get.fund.data <- function
(
label,
fund,
fund.date,
is.12m.rolling=F,
cash.flow=F
)
{
index = get.fund.data.index(label, fund)
if( len(index) == 0 ) return(as.xts(rep(NA,len(fund.date)), fund.date))
temp.q = as.double(gsub(',', '', fund[index,]))
temp.q = ifna(temp.q, 0)
if(cash.flow) {
quarterly.indicator = fund['quarterly indicator',]
temp.q = iif(quarterly.indicator == '1', temp.q, temp.q - mlag(temp.q))
}
temp.q = as.xts(temp.q, fund.date)
iif(is.12m.rolling, runSum(temp.q, 4), temp.q)
}
fundamental.fb.test <- function()
{
load.packages('quantmod')
tickers = spl('FB,LNKD,GRPN,AAPL,GOOG')
tickers.temp = spl('NASDAQ:FB,NYSE:LNKD,NASDAQ:GRPN,NASDAQ:AAPL,NASDAQ:GOOG')
data.fund <- new.env()
for(i in 1:len(tickers)) {
if(is.null(data.fund[[tickers[i]]])) {
cat(tickers[i],'\n')
data.fund[[tickers[i]]] = fund.data(tickers.temp[i], 80)
}
}
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
for(i in tickers) {
fund = data.fund[[i]]
fund.date = date.fund.data(fund)
EPS = 4 * get.fund.data('Diluted EPS from Total Operations', fund, fund.date)
if(nrow(EPS) > 3)
EPS = rbind(EPS[1:3], get.fund.data('Diluted EPS from Total Operations', fund, fund.date, is.12m.rolling=T)[-c(1:3)])
data[[i]] = merge(data[[i]], EPS)
}
bt.prep(data, align='keep.all', dates='1995::')
prices = data$prices
prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
EPS =  bt.apply(data, function(x) ifna.prev(x[, 'EPS']))
PE = ifna(prices / EPS, NA)
PE[ abs(EPS) < 0.001 ] = NA
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plota.matplot(PE)
dev.off()
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plota.matplot(PE, type='b',pch=20, dates='2012::')
dev.off()
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plota.matplot(EPS)
dev.off()
png(filename = 'plot4.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plota.matplot(prices)
dev.off()
}
fundamental.dcf.test <- function()
{
load.packages('quantmod')
tickers = spl('AAPL')
tickers.temp = paste(iif( nchar(tickers) <= 3, 'NYSE:', 'NASDAQ:'), tickers, sep='')
data.fund <- new.env()
for(i in 1:len(tickers))
data.fund[[tickers[i]]] = fund.data(tickers.temp[i], 80, 'annual')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
fund = data.fund[[tickers[1]]]
fund.date = date.fund.data(fund)
price = Cl(data[[tickers[1]]]['1995::'])
FCF = get.fund.data('free cash flow', fund, fund.date)
IC = get.fund.data('invested capital', fund, fund.date)
SALE = get.fund.data('total revenue', fund, fund.date)
CEQ = get.fund.data('total equity', fund, fund.date)
CSHO = get.fund.data('total common shares out', fund, fund.date)
CROIC = FCF/IC
g = runMean(CROIC, 5)
cash = runMean(FCF, 5)
compute.DCF.IV <- function(cash, eqity, shares, g, R) {
if( cash <= 0 ) return(NA)
if( len(R) == 1 ) R = rep(R, len(g))
value = eqity + sum(cash * cumprod(1 + g) / cumprod(1 + R))
return( value / shares )
}
dcf.price = NA * g
i.start = which(!is.na(g))[1]
for(i in i.start : nrow(g)) {
g.scenario = c(rep(g[i],3), rep(g[i],4)*0.8, rep(g[i],3)*0.8*0.8, rep(3/100,10))
dcf.price[i] = compute.DCF.IV(cash[i], CEQ[i], CSHO[i], g.scenario, 9/100)
}
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plota(price, type='l', log = 'y', col='blue', main=tickers[1],
ylim=range(price,dcf.price,na.rm=T))
plota.lines(dcf.price, type='s', col='red', lwd=2)
plota.legend('Close,Intrinsic Value', 'blue,red', list(price, dcf.price))
dev.off()
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plota(g, type='b', col='blue', pch=0, main='Growth Rate')
dev.off()
png(filename = 'plot3.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
plota(cash, type='b', col='blue', pch=0, main='Free Cash Flows')
dev.off()
}
min.corr.paper.examples <- function()
{
load.packages('quantmod')
data <- new.env()
getSymbols.TB(env = data, auto.assign = T, download = T)
bt.prep(data, align='remove.na', dates='1990::')
save(data,file='FuturesForex.Rdata')
tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='2002:08::')
save(data,file='ETF.Rdata')
load.packages('quantmod,quadprog')
tickers = spl('AA,AXP,BA,CAT,DD,DIS,GE,IBM,IP,JNJ,JPM,KO,MCD,MMM,MO,MRK,MSFT')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1980::')
prices = coredata(data$prices)
prices[is.na(prices)] = mlag(prices)[is.na(prices)]
prices[is.na(prices)] = mlag(prices)[is.na(prices)]
data$prices[] = prices
save(data,file='Dow.Engle.Rdata')
load.packages('quantmod,quadprog')
tickers = spl('VTI,IEV,EEM,EWJ,AGG,GSG,GLD,ICF')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='2003:10::')
save(data,file='ETF2.Rdata')
load.packages('quantmod,quadprog')
tickers = spl('ATVI,ADBE,ALTR,AMZN,AMGN,APOL,AAPL,AMAT,ADSK,ADP,BBBY,BIIB,BMC,BRCM,CHRW,CA,CELG,CERN,CHKP,CTAS,CSCO,CTXS,CTSH,CMCSA,COST,DELL,XRAY,DISH,EBAY,EA,EXPD,ESRX,FAST,FISV,FLEX,FLIR,FWLT,GILD,HSIC,HOLX,INFY,INTC,INTU,JBHT,KLAC,LRCX,LIFE,LLTC,LOGI,MAT,MXIM,MCHP,MSFT,MYL,NTAP,NWSA,NVDA,ORLY,ORCL,PCAR,PDCO,PAYX,PCLN,QGEN,QCOM,BBRY,ROST,SNDK,SIAL,SPLS,SBUX,SRCL,SYMC,TEVA,URBN,VRSN,VRTX,VOD,XLNX,YHOO')
data <- new.env()
for(i in tickers) {
try(getSymbols(i, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T), TRUE)
data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
}
bt.prep(data, align='keep.all', dates='1995::')
prices = coredata(data$prices)
prices[is.na(prices)] = mlag(prices)[is.na(prices)]
prices[is.na(prices)] = mlag(prices)[is.na(prices)]
data$prices[] = prices
save(data,file='nasdaq.100.Rdata')
names = spl('ETF,FuturesForex,Dow.Engle,ETF2,nasdaq.100')
lookback.len = 60
periodicitys = spl('weeks,months')
periodicity = periodicitys[1]
prefix = paste(substr(periodicity,1,1), '.', sep='')
for(name in names) {
load(file = paste(name, '.Rdata', sep=''))
obj = portfolio.allocation.helper(data$prices, periodicity, lookback.len = lookback.len, prefix = prefix,
min.risk.fns = 'min.corr.portfolio,min.corr2.portfolio,max.div.portfolio,min.var.portfolio,risk.parity.portfolio(),equal.weight.portfolio',
custom.stats.fn = 'portfolio.allocation.custom.stats')
save(obj, file=paste(name, lookback.len, periodicity, '.bt', '.Rdata', sep=''))
}
for(name in names) {
load(file=paste(name, '.Rdata', sep=''))
custom.input.report.helper(paste('report.', name, sep=''), data)
load(file=paste(name, lookback.len, periodicity, '.bt', '.Rdata', sep=''))
custom.report.helper(paste('report.', name, lookback.len, periodicity, sep=''),
create.strategies(obj, data))
}
names = spl('FuturesForex')
for(name in names) {
load(file=paste(name, '.Rdata', sep=''))
load(file=paste(name, lookback.len, periodicity, '.bt', '.Rdata', sep=''))
leverage = c(5, 4, 15, 20, 3, 1)
custom.report.helper(paste('report.leverage.', name, lookback.len, periodicity, sep=''),
create.strategies(obj, data, leverage))
}
}
custom.input.report.helper <- function(filename, data) {
filename.pdf = paste(filename, '.pdf', sep='')
filename.csv = paste(filename, '.csv', sep='')
pdf(file = filename.pdf, width=8.5, height=11)
layout(1:2)
asset.models = list()
for(i in data$symbolnames) {
data$weight[] = NA
data$weight[,i] = 1
asset.models[[i]] = bt.run(data, silent=T)
}
asset.summary = plotbt.strategy.sidebyside(asset.models, return.table=T)
ret.log = bt.apply.matrix(data$prices, ROC, type='continuous')
temp = cor(ret.log, use='complete.obs', method='pearson')
temp[] = plota.format(100 * temp, 0, '', '%')
plot.table(temp, smain='Correlation', highlight = TRUE, colorbar = TRUE)
layout(matrix(1:4,2,2))
if( is.null(data$symbol.groups) ) {
index = order(data$symbolnames)
for(i in data$symbolnames[index])
plota(data[[i]], type='l', cex.main=0.7,main= i)
} else {
index = order(data$symbol.groups)
for(i in data$symbolnames[index])
plota(data[[i]], type='l', cex.main=0.7, main= paste(i, data$symbol.groups[i], data$symbol.descriptions.print[i], sep=' / ') )
asset.summary = rbind(data$symbol.groups, data$symbol.descriptions.print, asset.summary)
}
dev.off()
load.packages('abind')
write.csv(asset.summary, filename.csv)
cat('\n\n', file=filename.csv, append=TRUE)
write.table(temp, sep=',',  row.names = , col.names = NA,
file=filename.csv, append=TRUE)
}
custom.report.helper <- function(filename, obj) {
filename.pdf = paste(filename, '.pdf', sep='')
filename.csv = paste(filename, '.csv', sep='')
models = obj$models
pdf(file = filename.pdf, width=8.5, height=11)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)
mtext('Cumulative Performance', side = 2, line = 1)
out = plotbt.strategy.sidebyside(models, perfromance.fn = 'custom.returns.kpi', return.table=T)
cdi = custom.composite.diversification.indicator(obj)
out = rbind(colMeans(cdi, na.rm=T), out)
rownames(out)[1] = 'Composite Diversification Indicator(CDI)'
y = 100 * sapply(models, compute.turnover, data)
out = rbind(y, out)
rownames(out)[1] = 'Portfolio Turnover'
performance.barchart.helper(out, 'Sharpe,Cagr,RC Gini,RC Herfindahl,Volatility,Portfolio Turnover,Composite Diversification Indicator(CDI)', c(T,T,F,F,F,F,T))
custom.summary.positions(obj$weights)
custom.period.chart(models)
layout(1:len(models))
for(m in names(models)) {
plotbt.transition.map(models[[m]]$weight, name=m)
legend('topright', legend = m, bty = 'n')
}
dates = index(models[[1]]$weight)[obj$period.ends]
layout(1:len(models))
for(m in names(models)) {
plotbt.transition.map(make.xts(obj$risk.contributions[[m]], dates),
name=paste('Risk Contributions',m))
legend('topright', legend = m, bty = 'n')
}
dev.off()
load.packages('abind')
write.csv(out, filename.csv)
cat('\n\n', file=filename.csv, append=TRUE)
out = abind(lapply(models, function(m) m$equity))
colnames(out) = names(models)
write.xts(make.xts(out, index(models[[1]]$equity)), filename.csv, append=TRUE)
}
custom.composite.diversification.indicator <- function
(
obj,
avg.flag = T,
avg.len = 10,
plot.main = T,
plot.table = T
)
{
cdi = 0.5 * obj$risk.gini + 0.5 * obj$degree.diversification
if(avg.flag) cdi = bt.apply.matrix(cdi, EMA, avg.len)
if(plot.main) {
avg.name = iif(avg.flag, paste(avg.len,'period EMA') , '')
layout(1:3)
out = obj$degree.diversification
if(avg.flag) out = bt.apply.matrix(out, EMA, avg.len)
plota.matplot(out, cex.main = 1,
main=paste('D = 1 - Portfolio Risk/Weighted Average of asset vols in the portfolio', avg.name))
out = obj$risk.gini
if(avg.flag) out = bt.apply.matrix(out, EMA, avg.len)
plota.matplot(out, cex.main = 1,
main=paste('1 - Gini(Risk Contributions)', avg.name))
plota.matplot(cdi, cex.main = 1,
main=paste('Composite Diversification Indicator (CDI) = 50/50 Gini/D', avg.name))
}
if(plot.table) {
weights = seq(0,1,0.1)
temp = matrix(NA, nc=len(weights), nr=ncol(cdi))
colnames(temp) = paste(weights)
rownames(temp) = colnames(cdi)
for(j in 1:len(weights)) {
i = weights[j]
temp[,j] = rank(-colMeans((1-i) * obj$risk.gini + i * obj$degree.diversification, na.rm=T))
}
temp = cbind(temp, round(rowMeans(temp),1))
colnames(temp)[ncol(temp)] = 'AVG'
highlight = apply(temp,2, function(x) plot.table.helper.color(t(x)) )
layout(1)
plot.table(temp, smain = 'CDI Rank\nAlgo vs %D',highlight = highlight, colorbar = TRUE)
}
return(cdi)
}
custom.summary.positions <- function(weights) {
layout(1:len(weights))
for(w in names(weights)) {
tickers = colnames(weights[[w]])
n = len(tickers)
temp = matrix(NA, nr=4, nc=n)
colnames(temp) = tickers
rownames(temp) = spl('Avg Pos,Max Pos,Min Pos,# Periods')
temp['Avg Pos',] = 100 * apply(weights[[w]],2,mean,na.rm=T)
temp['Max Pos',] = 100 * apply(weights[[w]],2,max,na.rm=T)
temp['Min Pos',] = 100 * apply(weights[[w]],2,min,na.rm=T)
temp['# Periods',] = apply(weights[[w]] > 1/1000,2,sum,na.rm=T)
temp[] = plota.format(temp, 0, '', '')
plot.table(temp, smain=w)
}
}
custom.profit.chart <- function(data, main, cols) {
par(mar=c(4, 3, 2, 2),cex.main=2,cex.sub=1, cex.axis=1.5,cex.lab=1.5)
barplot(data, names.arg = names(data),
col=iif(data > 0, cols[1], cols[2]),
main=main,
cex.names = 1.5, border = 'darkgray',las=2)
grid(NA,NULL)
abline(h=0,col='black')
abline(h=mean(data),col='gray',lty='dashed',lwd=3)
}
custom.period.chart <- function(models) {
for(imodel in 1:len(models)) {
equity = models[[imodel]]$equity
period.ends = endpoints(equity, 'months')
period.ends = unique(c(1, period.ends[period.ends > 0]))
ret = equity[period.ends,] / mlag(equity[period.ends,]) - 1
ret = ret[-1]
ret.by.month = create.monthly.table(ret)
ret.by.month = 100 * apply(ret.by.month, 2, mean, na.rm=T)
period.ends = endpoints(equity, 'years')
period.ends = unique(c(1, period.ends[period.ends > 0]))
ret = equity[period.ends,] / mlag(equity[period.ends,]) - 1
ret.by.year = ret[-1]
ilayout =
'1,1
2,2
2,2
2,2
2,2
2,2
3,4
3,4
3,4
3,4
3,4
3,4'
plota.layout(ilayout)
make.table(1,1)
a = matrix(names(models)[imodel],1,1)
cex = plot.table.helper.auto.adjust.cex(a)
draw.cell(a[1],1,1, text.cex=cex,frame.cell=F)
temp = plotbt.monthly.table(equity)
cols = spl('green,red')
custom.profit.chart(ret.by.month, 'Average Monthly Returns', cols)
ret = 100*as.vector(ret.by.year)
names(ret) = date.year(index(ret.by.year))
custom.profit.chart(ret, 'Annual Returns', cols)
}
}
custom.returns.kpi <- function
(
bt,
trade.summary = NULL
)
{
out = list()
w = bt$period.weight
rc = bt$risk.contribution
out[[ 'Avg #' ]] =  mean(rowSums(w > 1/1000)) / 100
out[[ 'W Gini' ]] = mean(portfolio.concentration.gini.coefficient(w), na.rm=T)
out[[ 'W Herfindahl' ]] = mean(portfolio.concentration.herfindahl.index(w), na.rm=T)
out[[ 'RC Gini' ]] = mean(portfolio.concentration.gini.coefficient(rc), na.rm=T)
out[[ 'RC Herfindahl' ]] = mean(portfolio.concentration.herfindahl.index(rc), na.rm=T)
out = lapply(out, function(x) if(is.double(x)) round(100*x,1) else x)
out = c(bt.detail.summary(bt)$System, out)
return( list(System=out))
}
min.corr.paper.numerical.examples <- function()
{
n = 3
ia = list()
ia$n = 3
ia$risk = c(14, 18, 22) / 100;
ia$correlation = matrix(
c(1, 0.90, 0.85,
0.90, 1, 0.70,
0.85, 0.70, 1), nr=3, byrow=T)
ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
constraints = new.constraints(n)
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(diag(n), type='>=', b=0, constraints)
constraints = add.constraints(diag(n), type='<=', b=1, constraints)
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)
x = min.var.portfolio(ia, constraints)
sol = solve.QP(Dmat=ia$cov, dvec=rep(0, ia$n),
Amat=constraints$A, bvec=constraints$b, meq=constraints$meq)
x = sol$solution
round(x,4)
sqrt(x %*% ia$cov %*% x)
x %*% ia$cov
sol = solve.QP(Dmat=ia$correlation, dvec=rep(0, ia$n),
Amat=constraints$A, bvec=constraints$b, meq=constraints$meq)
x = sol$solution
round(x,4)
x %*% ia$correlation
x = x / sqrt( diag(ia$cov) )
x = x / sum(x)
round(x,4)
sqrt(x %*% ia$cov %*% x)
upper.index = upper.tri(ia$correlation)
cor.m = ia$correlation[upper.index]
cor.mu = mean(cor.m)
cor.sd = sd(cor.m)
norm.dist.m = 0 * ia$correlation
diag(norm.dist.m) = NA
norm.dist.m[upper.index] = sapply(cor.m, function(x) 1-pnorm(x, cor.mu, cor.sd))
norm.dist.m = (norm.dist.m + t(norm.dist.m))
norm.dist.avg = apply(norm.dist.m, 1, mean, na.rm=T)
norm.dist.rank = rank(-norm.dist.avg)
adjust.factor = 1
adjusted.norm.dist.rank = norm.dist.rank ^ adjust.factor
norm.dist.weight = adjusted.norm.dist.rank / sum(adjusted.norm.dist.rank)
weighted.norm.dist.average = norm.dist.weight %*% ifna(norm.dist.m,0)
final.weight = weighted.norm.dist.average / sum(weighted.norm.dist.average)
x = final.weight
x = x / sqrt( diag(ia$cov) )
x = x / sum(x)
round(x,4)
x = as.vector(x)
sqrt(x %*% ia$cov %*% x)
cor.m = ia$correlation
diag(cor.m) = 0
avg = rowMeans(cor.m)
cor.mu = mean(avg)
cor.sd = sd(avg)
norm.dist.avg = 1-pnorm(avg, cor.mu, cor.sd)
norm.dist.rank = rank(-norm.dist.avg)
adjust.factor = 1
adjusted.norm.dist.rank = norm.dist.rank ^ adjust.factor
norm.dist.weight = adjusted.norm.dist.rank / sum(adjusted.norm.dist.rank)
weighted.norm.dist.average = norm.dist.weight %*% (1-cor.m)
final.weight = weighted.norm.dist.average / sum(weighted.norm.dist.average)
x = final.weight
x = x / sqrt( diag(ia$cov) )
x = x / sum(x)
round(x,4)
x = as.vector(x)
sqrt(x %*% ia$cov %*% x)
}
solve.LP.bounds <- function
(
direction,
objective.in,
const.mat,
const.dir,
const.rhs,
binary.vec = 0,
lb = 0,
ub = +Inf,
default.lb = -100
)
{
n = len(objective.in)
if( len(lb) == 1 ) lb = rep(lb, n)
if( len(ub) == 1 ) ub = rep(ub, n)
lb = ifna(lb, default.lb)
ub = ifna(ub, +Inf)
lb[ lb < default.lb ] = default.lb
dvec = lb
index = which( ub < +Inf )
if( len(index) > 0 ) {
const.rhs = c(const.rhs, ub[index])
const.dir = c(const.dir, rep('<=', len(index)))
const.mat = rbind(const.mat, diag(n)[index, ])
}
if ( binary.vec[1] == 0 ) {
sol = lp( direction, objective.in, const.mat, const.dir,
const.rhs - const.mat %*% dvec )
} else {
dvec[binary.vec] = 0
sol = lp( direction, objective.in, const.mat, const.dir,
const.rhs - const.mat %*% dvec, binary.vec = binary.vec )
}
sol$solution = sol$solution + dvec
sol$value = objective.in %*% sol$solution
return( sol )
}
solve.QP.bounds <- function
(
Dmat,
dvec,
Amat,
bvec,
meq=0,
factorized=FALSE,
binary.vec = 0,
lb = -Inf,
ub = +Inf
)
{
Amat1 = Amat
bvec1 = bvec
n = len(dvec)
if( len(lb) == 1 ) lb = rep(lb, n)
if( len(ub) == 1 ) ub = rep(ub, n)
lb = ifna(lb, -Inf)
ub = ifna(ub, +Inf)
index = which( ub < +Inf )
if( len(index) > 0 ) {
bvec = c(bvec, -ub[index])
Amat = cbind(Amat, -diag(n)[, index])
}
index = which( lb > -Inf )
if( len(index) > 0 ) {
bvec = c(bvec, lb[index])
Amat = cbind(Amat, diag(n)[, index])
}
if ( binary.vec[1] == 0 ) {
qp.data.final = solve.QP.remove.equality.constraints(Dmat, dvec, Amat, bvec, meq)
Dmat = qp.data.final$Dmat
dvec = qp.data.final$dvec
Amat = qp.data.final$Amat
bvec = qp.data.final$bvec
meq = qp.data.final$meq
sol = try(solve.QP(Dmat, dvec, Amat, bvec, meq, factorized),TRUE)
if(inherits(sol, 'try-error')) {
ok = F
sol = list()
} else {
tol = 1e-3
ok = T
check = sol$solution %*% Amat - bvec
if(meq > 0) ok = ok & all(abs(check[1:meq]) <= tol)
ok = ok & all(check[-c(1:meq)] > -tol)
}
if(!ok) {
require(kernlab)
index.constant.variables = which(!is.na(qp.data.final$solution))
if( len(index.constant.variables) > 0 ) {
Amat1 = Amat[,1:ncol(Amat1)]
bvec1 = bvec[1:ncol(Amat1)]
lb = lb[-index.constant.variables]
ub = ub[-index.constant.variables]
}
sv = ipop(c = matrix(-dvec), H = Dmat, A = t(Amat1),
b = bvec1, l = ifna(lb,-100), u = ifna(ub,100),
r = c(rep(0,meq), rep(100, len(bvec1) - meq))
)
sol$solution = primal(sv)
}
x = qp.data.final$solution
x[qp.data.final$var.index] = sol$solution
sol$solution = x
} else {
qp_data = qp_new(binary.vec, Dmat = Dmat, dvec = dvec,
Amat=Amat, bvec=bvec, meq=meq, factorized=factorized)
sol = binary_branch_bound(binary.vec, qp_data, qp_solve,
control = bbb_control(silent=T, branchvar='max', searchdir='best' ))
qp_delete(qp_data)
sol$value = sol$fmin
sol$solution = sol$xmin
}
return(sol)
}
bbb_control <- function
(
itermax = 200,
depthmax = Inf,
bineps = 1e-4,
precisioneps = 0,
silent = T,
branchvar = c('first', 'max','min'),
proborder = c('0', '1', 'mindiff'),
searchdir = c('depth', 'breadth', 'best', 'normbest')
)
{
branchvar = switch(branchvar[1],
'first' = 0,
'max' = 1,
'min' = 2,
0)
branchvar = iif( is.null(branchvar),0, branchvar)
proborder = switch(proborder[1],
'0' = 0,
'1' = 1,
'mindiff' = 2,
0)
proborder = iif( is.null(proborder),0, proborder)
searchdir = switch(searchdir[1],
'depth' = 0,
'breadth' = 1,
'best' = 2,
'normbest' = 2,
0)
searchdir = iif( is.null(searchdir),0, searchdir)
control = list(itermax = itermax, depthmax = depthmax, bineps = bineps, precisioneps = precisioneps, silent = silent,
branchvar = branchvar,
proborder = proborder,
searchdir = searchdir)
return(control)
}
qp_new <- function
(
index_binvar,
Dmat,
dvec,
Amat,
bvec,
meq = 0,
factorized = FALSE
)
{
nbinvar = length(index_binvar)
nx = nrow(Dmat)
nbvec = length(bvec)
Amat = cbind( Amat, diag(nx)[,index_binvar], -diag(nx)[,index_binvar] )
bvec = c(bvec, rep(0,nx)[index_binvar], rep(1,nx)[index_binvar] )
lb_bin_index = (1:nbinvar) + nbvec
ub_bin_index = (1:nbinvar) + nbvec + nbinvar
qp_data = new.env()
qp_data$Dmat = Dmat
qp_data$dvec = dvec
qp_data$Amat = Amat
qp_data$bvec = bvec
qp_data$meq = meq
qp_data$factorized = factorized
qp_data$x0 = rep(0,nx)
qp_data$lb_bin_index = lb_bin_index
qp_data$ub_bin_index = ub_bin_index
qp_data$lb = bvec[lb_bin_index]
qp_data$ub = bvec[ub_bin_index]
return(qp_data)
}
qp_delete <- function(qp_data)
{
rm(list = ls(qp_data,all=TRUE), envir = qp_data)
}
qp_solve <- function
(
qp_data,
lb,
ub
)
{
bvec = qp_data$bvec
bvec[qp_data$lb_bin_index] = lb
bvec[qp_data$ub_bin_index] = -ub
qp.data.final = solve.QP.remove.equality.constraints(qp_data$Dmat, qp_data$dvec,
qp_data$Amat, bvec, qp_data$meq)
sol = tryCatch( solve.QP(Dmat=qp.data.final$Dmat, dvec=qp.data.final$dvec, Amat=qp.data.final$Amat,
bvec=qp.data.final$bvec, meq=qp.data.final$meq, factorized=qp_data$factorized),
error=function( err ) FALSE,
warning=function( warn ) FALSE )
if( !is.logical( sol ) ) {
x = qp.data.final$solution
x[qp.data.final$var.index] = sol$solution
return(list( ok = TRUE, x = x, fval = sol$value ))
} else {
return(list( ok = FALSE ))
}
}
mbqp.test <- function()
{
load.packages('quadprog')
Q = diag(4)
b	= c( 2, -3, -2, -3)
C	= matrix(c(-1,  -1,  -1,  -1,
10,	5,   3,	4,
-1,	0,   0,	0),
3,4,byrow = TRUE)
d = c(-2, 10,  0)
vlb  = c(-1e10, 0, 0, 0);
vub  = c( 1e10, 1, 1, 1);
index_binvar = c(2, 3, 4);
Dmat = Q
dvec = -b
Amat = -t(C)
bvec = -d
n = nrow(Dmat)
Amat = cbind( Amat, diag(n), -diag(n) )
bvec = c( bvec, vlb, -vub )
sol = solve.QP(Dmat=Dmat, dvec=dvec, Amat=Amat, bvec=bvec, meq=0)
xsol = sol$solution
fsol = sol$value
cat('QP.solve fsol =', fsol, 'xsol =', xsol, '\n')
sol = solve.QP.bounds(Dmat=Dmat, dvec=dvec, Amat=Amat, bvec=bvec, meq=0, binary.vec = index_binvar)
xsol = sol$solution
fsol = sol$value
cat('QP.solve Binary Branch and Bound fsol =', fsol, 'xsol =', xsol, '\n')
}
solve.QP.remove.equality.constraints <- function
(
Dmat,
dvec,
Amat,
bvec,
meq=0
)
{
qp.data = list()
qp.data$Amat = Amat
qp.data$bvec = bvec
qp.data$Dmat = Dmat
qp.data$dvec = dvec
qp.data$meq = meq
Amat1 = t(qp.data$Amat)
bvec1 = qp.data$bvec
Dmat1 = qp.data$Dmat
dvec1 = qp.data$dvec
meq1 = qp.data$meq
qp.data$solution = rep(NA, ncol(Amat1))
qp.data$var.index = 1:ncol(Amat1)
while(T) {
one.non.zero.index = which( rowSums(Amat1!=0) == 1 )
if( len(one.non.zero.index) == 0 ) break
temp0 = rowSums(Amat1[one.non.zero.index,])
temp = abs( temp0 )
bvec1[one.non.zero.index] = bvec1[one.non.zero.index] / temp
Amat1[one.non.zero.index,] = Amat1[one.non.zero.index,] / temp
temp0.index = matrix(1:ncol(Amat1), nr=ncol(Amat1), nc=len(one.non.zero.index))[t(Amat1[one.non.zero.index,]!=0)]
equality.constraints = rep(NA, ncol(Amat1))
lb = ub = rep(NA, ncol(Amat1))
index = temp0 > 0
temp = order(bvec1[one.non.zero.index[index]], decreasing = FALSE)
lb[temp0.index[index][temp]] = bvec1[one.non.zero.index[index]][temp]
index = temp0 < 0
temp = order(-bvec1[one.non.zero.index[index]], decreasing = TRUE)
ub[temp0.index[index][temp]] = -bvec1[one.non.zero.index[index]][temp]
remove.index = which(lb == ub)
if( len(remove.index) > 0 ) {
equality.constraints[remove.index] = lb[remove.index]
Dmat1 = Dmat1[-remove.index, -remove.index,drop=F]
dvec1 = dvec1[-remove.index]
bvec1 = bvec1 - Amat1[,remove.index,drop=F] %*% equality.constraints[remove.index]
Amat1 = Amat1[,-remove.index,drop=F]
qp.data$solution[ qp.data$var.index[remove.index] ] = lb[remove.index]
qp.data$var.index = which(is.na(qp.data$solution))
if( ncol(Amat1) > 0 ) {
remove.index = which( rowSums(Amat1!=0) == 0 & bvec1 == 0 )
if(len(remove.index)>0) {
bvec1 = bvec1[-remove.index]
Amat1 = Amat1[-remove.index,,drop=F]
if( meq1 > 0 ) meq1 = meq1 - len(intersect((1:meq1), remove.index))
}
} else break
} else break
}
qp.data$Amat = t(Amat1)
qp.data$bvec = bvec1
qp.data$Dmat = Dmat1
qp.data$dvec = dvec1
qp.data$meq = meq1
return(qp.data)
}
solve.QP.remove.equality.constraints12 <- function
(
Dmat,
dvec,
Amat,
bvec,
meq=0
)
{
qp.data.temp = list()
qp.data.temp$Amat = Amat
qp.data.temp$bvec = bvec
qp.data.temp$Dmat = Dmat
qp.data.temp$dvec = dvec
qp.data.temp$meq = meq
qp.data.temp = remove.equality.constraints.old(qp.data.temp)
if( len(qp.data.temp$var.index) == len(qp.data.temp$solution) ) {
qp.data.final = qp.data.temp
} else {
qp.data.final = remove.equality.constraints.old(qp.data.temp)
qp.data.temp$solution[qp.data.temp$var.index] = qp.data.final$solution
qp.data.final$solution = qp.data.temp$solution
qp.data.final$var.index = qp.data.temp$var.index[qp.data.final$var.index]
}
return(qp.data.final)
}
remove.equality.constraints.old <- function(qp.data)
{
Amat1 = qp.data$Amat
bvec1 = qp.data$bvec
Dmat1 = qp.data$Dmat
dvec1 = qp.data$dvec
meq1 = qp.data$meq
one.non.zero.index = which( colSums(Amat1!=0) == 1 )
if( len(one.non.zero.index) == 0 ) {
equality.constraints = rep(NA, nrow(Amat1))
qp.data$solution = equality.constraints
qp.data$var.index = which(is.na(equality.constraints))
return(qp.data)
}
bvec1[one.non.zero.index] = bvec1[one.non.zero.index] / abs( colSums(Amat1[,one.non.zero.index]) )
Amat1[,one.non.zero.index] = Amat1[,one.non.zero.index] / abs( Amat1[,one.non.zero.index] )
Amat1[is.na(Amat1)] = 0
equality.constraints = rep(NA, nrow(Amat1))
remove.constraints = rep(NA, ncol(Amat1))
for( r in which(rowSums(Amat1[,one.non.zero.index]!=0) > 1) ) {
temp.index = which( Amat1[,one.non.zero.index][r,] != 0 )
for( r1 in temp.index[-1] ) {
temp.index1 = colSums(abs(
rbind(Amat1,bvec1)[,one.non.zero.index[temp.index]] +
rbind(Amat1,bvec1)[,one.non.zero.index[r1]])
)
if( any(temp.index1 == 0 ) ) {
equality.constraints[r] =
bvec1[one.non.zero.index[r1]] / Amat1[r,one.non.zero.index[r1]]
remove.constraints[ one.non.zero.index[r1] ] = 1
remove.constraints[ one.non.zero.index[ temp.index[ temp.index1 == 0]]] = 1
break;
}
}
}
remove.index = which(!is.na(equality.constraints))
if(len(remove.index)>0) {
Dmat1 = Dmat1[-remove.index, -remove.index,drop=F]
dvec1 = dvec1[-remove.index]
bvec1 = bvec1 - equality.constraints[remove.index] %*% Amat1[remove.index,,drop=F]
Amat1 = Amat1[-remove.index,,drop=F]
remove.index1 = which(remove.constraints==1)
if(len(remove.index1)>0) {
bvec1 = bvec1[-remove.index1]
Amat1 = Amat1[,-remove.index1,drop=F]
if( meq1 > 0 ) meq1 = meq1 - len(intersect((1:meq1), remove.index1))
}
}
qp.data$Amat = Amat1
qp.data$bvec = bvec1
qp.data$Dmat = Dmat1
qp.data$dvec = dvec1
qp.data$meq = meq1
qp.data$solution = equality.constraints
qp.data$var.index = which(is.na(equality.constraints))
return(qp.data)
}
lm.constraint <- function
(
x,
y,
constraints = NULL
)
{
if( is.null(constraints) ) {
fit = lm.fit(x, y)
return( ols.summary(x, y, fit$coefficients) )
} else {
temp = cov(cbind(y, x))
Dmat = temp[-1,-1]
dvec = temp[-1,1]
sol = solve.QP.bounds(Dmat = Dmat, dvec = dvec ,
Amat=constraints$A, bvec=constraints$b, constraints$meq,
lb = constraints$lb, ub = constraints$ub)
return( ols.summary(x, y, sol$solution) )
}
}
ols <- function
(
x,
y,
computeSummary=F
)
{
xx = t(x) %*% x
if(is.null(ncol(xx))) { xinv = inv1(xx)
} else if(ncol(xx) == 1) { xinv = inv1(xx)
} else if(ncol(xx) == 2) { xinv = inv2(xx)
} else if(ncol(xx) == 3) { xinv = inv3(xx)
} else { xinv = inv(xx) }
coefficients = xinv %*% t(x) %*% y
if(computeSummary) {
return( ols.summary(x, y, coefficients, xinv) )
} else {
return(list(coefficients = coefficients))
}
}
ols.summary <- function
(
x,
y,
coefficients,
xinv = NULL
)
{
n = length(y)
p = length(coefficients)
rdf = n-p
e = y - x %*% coefficients
ess=sum(e^2)
mss = sum((y - sum(y)/n)^2)
r.squared = 1 - ess/mss
if( !is.null(xinv) ) {
s2=ess/(rdf)
seb=sqrt(diag(s2*xinv))
tratio=coefficients/seb
return(list(coefficients = coefficients, seb = seb,tratio = tratio, r.squared = r.squared))
} else {
return(list(coefficients = coefficients, r.squared = r.squared))
}
}
ols.test <- function() {
x = matrix( rnorm(4*10), ncol=4)
y = rnorm(10)
summary(lm(y ~ x+0))
ols(x, y, T)
}
inv <- function(x) { solve(x) }
inv1 <- function(x) { 1/x }
inv2 <- function(x)
{
matrix(c(x[2,2],-x[1,2],-x[2,1],x[1,1]),nrow=2,byrow=T) / (x[1,1]*x[2,2] - x[1,2]*x[2,1])
}
inv3 <- function(x)
{
matrix(c(x[3,3]*x[2,2]-x[3,2]*x[2,3],-(x[3,3]*x[1,2]-x[3,2]*x[1,3]),x[2,3]*x[1,2]-x[2,2]*x[1,3],
-(x[3,3]*x[2,1]-x[3,1]*x[2,3]),x[3,3]*x[1,1]-x[3,1]*x[1,3],-(x[2,3]*x[1,1]-x[2,1]*x[1,3]),
x[3,2]*x[2,1]-x[3,1]*x[2,2],-(x[3,2]*x[1,1]-x[3,1]*x[1,2]),x[2,2]*x[1,1]-x[2,1]*x[1,2]),nrow=3,byrow=T) /
(x[1,1]*(x[3,3]*x[2,2]-x[3,2]*x[2,3])-x[2,1]*(x[3,3]*x[1,2]-x[3,2]*x[1,3])+x[3,1]*(x[2,3]*x[1,2]-x[2,2]*x[1,3]))
}
inv.test <- function() {
m=matrix(c(4,3,3,2),nrow=2,byrow=T)
inv2(m) %*% m
inv(m) %*% m
m = matrix(c(1,2,3,4,5,6,7,8,8),ncol=3,byrow=T)
inv3(m) %*% m
m %*% inv3(m)
inv(m) %*% m
}
find.maximum.distance.point <- function
(
y,
x=1:len(y)
)
{
allCoord = rbind(y, x)
firstPoint = allCoord[,1]
lineVec = allCoord[,len(y)] - firstPoint
lineVecN = lineVec / sqrt(sum(lineVec^2))
vecFromFirst = allCoord - firstPoint
scalarProduct = lineVecN %*% vecFromFirst
vecFromFirstParallel = t(scalarProduct) %*% lineVecN
vecToLine = t(vecFromFirst) - vecFromFirstParallel
distToLine = sqrt(rowSums(vecToLine^2,2))
which.max(distToLine)
}
make.table <- function
(
nr,
nc
)
{
savepar = par(mar = rep(1, 4))
plot(c(0.5, nc*2 + 0.5), c(-0.5, -(nr + 0.5)), xaxs = 'i', yaxs = 'i',
type = 'n', xlab = '', ylab = '', axes = FALSE)
savepar
}
draw.cell <- function
(
title,
r,
c,
text.cex = 1,
bg.col = 'white',
frame.cell = T
)
{
if(!frame.cell) bcol = bg.col else bcol = 'black'
rect((2*(c - 1) + .5), -(r - .5), (2*c + .5), -(r + .5), col = bg.col, border = bcol)
if( c == 1) {
text((2*(c - 1) + .5), -r, title, adj = 0, cex = text.cex)
} else if( r == 1 ) {
text((2*(c - 1) + .5), -r, title, adj = 0, cex = text.cex)
} else {
text((2*c + .5), -r, title, adj = 1, cex = text.cex)
}
}
plot.table.helper.auto.adjust.cex <- function
(
temp.table,
keep.all.same.cex = FALSE
)
{
nr = nrow(temp.table)
nc = ncol(temp.table)
all.xrange = diff(par()$usr[1:2]) / nc
xrange = matrix( strwidth(paste('  ', temp.table), units = 'user', cex = 1), nc = nc)
all.yrange = diff(par()$usr[3:4]) / nr
yrange = matrix( 5/3 * strheight(temp.table, units = 'user', cex = 1), nc = nc)
plot.matrix.cex = pmin( round(all.yrange / yrange, 2) , round(all.xrange / xrange, 2) )
header.col.cex = min(plot.matrix.cex[1,-1])
header.row.cex = min(plot.matrix.cex[-1,1])
title.cex = plot.matrix.cex[1, 1]
data.cex = min(plot.matrix.cex[-1, -1])
if ( keep.all.same.cex ) {
plot.matrix.cex[] = min(plot.matrix.cex)
} else {
plot.matrix.cex[1,-1] = min(c(header.col.cex, header.row.cex))
plot.matrix.cex[-1,1] = min(c(header.col.cex, header.row.cex))
plot.matrix.cex[-1,-1]= min(c(header.col.cex, header.row.cex, data.cex))
plot.matrix.cex[1,1]= min(c(header.col.cex, header.row.cex, data.cex, title.cex))
plot.matrix.cex[1,-1] = min(c(header.col.cex))
plot.matrix.cex[-1,1] = min(c(header.row.cex))
plot.matrix.cex[-1,-1]= min(c(data.cex))
plot.matrix.cex[1,1]= min(c(title.cex))
}
return(plot.matrix.cex)
}
plot.table.param <- function
(
plot.matrix,
smain = '',
plot.matrix.cex,
plot.matrix_bg.col,
frame.cell = T,
keep.all.same.cex = FALSE
)
{
n = nrow(plot.matrix)
pages = unique(c(seq(0, n, by = 120), n))
for(p in 1:(len(pages)-1)) {
rindex = (pages[p]+1) : pages[p+1]
temp.table = matrix('', nr = len(rindex)+1, nc = ncol(plot.matrix)+1)
temp.table[-1, -1] = plot.matrix[rindex,]
temp.table[1, -1] = colnames(plot.matrix)
temp.table[-1, 1] = rownames(plot.matrix)[rindex]
temp.table[1, 1] = smain
nr = nrow(temp.table)
nc = ncol(temp.table)
par(mar = c(0, 0, 0, 0), cex = 0.5)
oldpar = make.table(nr, nc)
text.cex = plot.matrix.cex[c(1, 1 + rindex), ]
text.cex = plot.table.helper.auto.adjust.cex(temp.table, keep.all.same.cex)
bg.col = plot.matrix_bg.col[c(1, 1 + rindex), ]
for(r in 1:nr) {
for(c in 1:nc) {
draw.cell( paste('', temp.table[r,c], '', sep=' '), r, c,
text.cex = text.cex[r,c], bg.col = bg.col[r,c], frame.cell = frame.cell)
}
}
}
}
plot.table.helper.color <- function
(
temp
){
temp = matrix(as.double(gsub('[%,$]', '', temp)), nrow(temp), ncol(temp))
highlight = as.vector(temp)
cols = rep(NA, len(highlight))
ncols = len(highlight[!is.na(highlight)])
cols[1:ncols] = rainbow(ncols, start = 0, end = 0.3)
o = sort.list(highlight, na.last = TRUE, decreasing = FALSE)
o1 = sort.list(o, na.last = TRUE, decreasing = FALSE)
highlight = matrix(cols[o1], nrow = nrow(temp))
highlight[is.na(temp)] = NA
return(highlight)
}
plot.table.helper.colorbar <- function
(
plot.matrix
)
{
nr = nrow(plot.matrix) + 1
nc = ncol(plot.matrix) + 1
c = nc
r1 = 1
r2 = nr
rect((2*(c - 1) + .5), -(r1 - .5), (2*c + .5), -(r2 + .5), col='white', border='white')
rect((2*(c - 1) + .5), -(r1 - .5), (2*(c - 1) + .5), -(r2 + .5), col='black', border='black')
y1= c( -(r2) : -(r1) )
graphics::image(x = c(  (2*(c - 1) + 1.5) : (2*c + 0.5) ),
y   = y1,
z   = t(matrix(  y1  , ncol = 1)),
col = t(matrix( rainbow(len( y1  ), start = 0, end = 0.3) , ncol = 1)),
add = T)
}
plot.table <- function
(
plot.matrix,
smain = '',
text.cex = 1,
frame.cell = T,
highlight = F,
colorbar = FALSE,
keep_all.same.cex = FALSE
)
{
if( is.null(rownames(plot.matrix)) & is.null(colnames(plot.matrix)) ) {
temp.matrix = plot.matrix
if( nrow(temp.matrix) == 1 ) temp.matrix = rbind('', temp.matrix)
if( ncol(temp.matrix) == 1 ) temp.matrix = cbind('', temp.matrix)
plot.matrix = temp.matrix[-1, -1, drop = FALSE]
colnames(plot.matrix) = temp.matrix[1, -1]
rownames(plot.matrix) = temp.matrix[-1, 1]
smain = temp.matrix[1, 1]
} else if( is.null(rownames(plot.matrix)) ) {
temp.matrix = plot.matrix
if( ncol(plot.matrix) == 1 ) temp.matrix = cbind('', temp.matrix)
plot.matrix = temp.matrix[, -1, drop = FALSE]
colnames(plot.matrix) = colnames(temp.matrix)[-1]
rownames(plot.matrix) = temp.matrix[,1]
smain = colnames(temp.matrix)[1]
} else if( is.null(colnames(plot.matrix)) ) {
temp.matrix = plot.matrix
if( nrow(temp.matrix) == 1 ) temp.matrix = rbind('', temp.matrix)
plot.matrix = temp.matrix[-1, , drop = FALSE]
rownames(plot.matrix) = rownames(temp.matrix)[-1]
colnames(plot.matrix) = temp.matrix[1, ]
smain = rownames(temp.matrix)[1]
}
plot.matrix[which(trim(plot.matrix) == 'NA')] = ''
plot.matrix[which(trim(plot.matrix) == 'NA%')] = ''
plot.matrix[which(is.na(plot.matrix))] = ''
if(colorbar) {
plot.matrix = cbind(plot.matrix, '')
if(!is.null(highlight)) if(!is.logical(highlight)) { highlight = cbind(highlight, NA) }
}
nr = nrow(plot.matrix) + 1
nc = ncol(plot.matrix) + 1
is_highlight = T
if(is.logical(highlight)) {
is_highlight = highlight
if(highlight) highlight = plot.table.helper.color(plot.matrix)
}
if(!is_highlight) {
plot.matrix.cex = matrix(1, nr = nr, nc = nc )
plot.matrix_bg.col = matrix('white', nr = nr, nc = nc )
plot.matrix_bg.col[seq(1, nr, 2), ] = 'yellow'
plot.matrix_bg.col[1,] = 'gray';
plot.table.param( plot.matrix, smain, plot.matrix.cex, plot.matrix_bg.col,
frame.cell, keep_all.same.cex)
} else {
plot.matrix.cex = matrix(1, nr = nr, nc = nc )
plot.matrix_bg.col = matrix('white', nr = nr, nc = nc )
plot.matrix_bg.col[1,] = 'gray'
plot.matrix_bg.col[2:nr,2:nc] = highlight
plot.table.param(plot.matrix, smain, plot.matrix.cex, plot.matrix_bg.col,
frame.cell, keep_all.same.cex)
}
if(colorbar) plot.table.helper.colorbar(plot.matrix);
}
plot.table.test <- function()
{
mrownames = spl('row one,row two,row 3')
mcolnames = spl('col 1,col 2,col 3,col 4')
temp = matrix(NA, len(mrownames), len(mcolnames))
rownames(temp) = mrownames
colnames(temp) = mcolnames
temp[,] = matrix(1:12,3,4)
png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot.table(temp, format(as.Date(Sys.time()), '%d %b %Y'))
dev.off()
data =  matrix(rnorm(1000), nc=10)
colnames(data) = paste('data', 1:10, sep='')
temp = cor(data, use='complete.obs', method='pearson')
temp[] = plota.format(100 * temp, 0, '', '%')
png(filename = 'plot2.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot.table(temp, smain='Correlation', highlight = TRUE, colorbar = TRUE)
dev.off()
}
plot.periodic.table1 <- function(hist.returns)
{
n = ncol(hist.returns)
temp = t(coredata(hist.returns))
colnames(temp) = format(index.xts(hist.returns), '%Y')
rownames(temp) = 1:n
rownames(temp)[1] = ' Best '
rownames(temp)[n] = ' Worst '
col = plota.colors(n)
highlight = apply(temp,2, function(x) col[order(x, decreasing = T)] )
temp[] = apply(temp,2, sort, decreasing = T)
temp[] = plota.format(100 * temp, 0, '', '%')
plot.table(temp, highlight = highlight)
plota.legend(colnames(hist.returns), col)
}
plot.periodic.table2 <- function(hist.returns)
{
temp = t(coredata(hist.returns))
colnames(temp) = format(index.xts(hist.returns), '%Y')
temp[] = plota.format(100 * temp, 0, '', '%')
highlight = apply(temp,2, function(x) plot.table.helper.color(t(x)) )
plot.table(temp, highlight = highlight, colorbar = TRUE)
}
plota.theme <- function
(
col.border = 'black',
col.up = 'green',
col.dn = 'red',
col.x.highlight = 'orange',
col.y.highlight = 'orange',
alpha=NA
)
{
col = c(col.border, col.up, col.dn, col.x.highlight, col.y.highlight)
if(!is.na(alpha)) col = col.add.alpha(col, alpha)
plota.control$col.border = col[1]
plota.control$col.up = col[2]
plota.control$col.dn = col[3]
plota.control$col.x.highlight = col[4]
plota.control$col.y.highlight = col[5]
}
plota.theme.blue.red <- function(alpha=NA)
{
plota.theme(
col.border = 'black',
col.up = 'blue',
col.dn = 'red',
alpha = alpha
)
}
plota.theme.green.orange <- function(alpha=NA)
{
plota.theme(
col.border = rgb(68,68,68, maxColorValue=255),
col.up = rgb(0,204,0, maxColorValue=255),
col.dn = rgb(255,119,0, maxColorValue=255),
alpha = alpha
)
}
plota.theme.gray.orange <- function(alpha=NA)
{
plota.theme(
col.border = '#444444',
col.up = '#BEBEBE',
col.dn = '#FF7700',
alpha = alpha
)
}
plota.control = new.env()
plota.control$col.border = 'black'
plota.control$col.up = 'green'
plota.control$col.dn = 'red'
plota.control$col.x.highlight = 'orange'
plota.control$col.y.highlight = 'orange'
plota.control$xaxis.ticks = c()
plota.theme.green.orange();
col.add.alpha <- function
(
col,
alpha=150
)
{
rgb(t(col2rgb(col)), alpha=alpha, maxColorValue = 255)
}
plota <- function
(
y,
main = NULL,
plotX = TRUE,
LeftMargin = 0,
x.highlight = NULL,
y.highlight = NULL,
las = 1,
type = 'n',
xlab = '',
ylab = '',
ylim = NULL,
log = '',
...
)
{
hasTitle = !is.null(main);
par( mar = c(iif(plotX,2,0), LeftMargin , iif(hasTitle,2,0), 3) )
if(has.Cl(y)) y1 = Cl(y) else y1 = y[,1]
if( is.null(ylim) ) {
ylim = range(y1, na.rm = T)
switch(type,
'ohlc' = ,
'hl' = ,
'candle' = { ylim = range(OHLC(y), na.rm = T) },
'volume' = { y1 = Vo(y); ylim = range(Vo(y), na.rm = T) }
)
}
temp.x = attr(y, 'index')
plot( temp.x, y1, xlab = xlab, ylab = ylab, main = main,
type = 'n', yaxt = 'n', xaxt = 'n', ylim = ylim, log = log, ... )
axis(4, las = las)
class(temp.x) = c('POSIXct', 'POSIXt')
plota.control$xaxis.ticks = axis.POSIXct(1, temp.x,labels = plotX, tick = plotX)
if( !is.null(x.highlight) ) plota.x.highlight(y, x.highlight);
if( !is.null(y.highlight) ) plota.y.highlight(y, y.highlight);
plota.grid()
switch(type,
'candle' = plota.candle(y, ...),
'hl' = plota.hl(y, ...),
'ohlc' = plota.ohlc(y, ...),
'volume' = plota.volume(y, ...),
{  lines(temp.x, y1, type=type, ...) }
)
box();
}
plota2Y <- function(
y,
las = 1,
type = 'n',
...
)
{
xlim = par('usr')[1:2]
class(xlim) = c('POSIXct', 'POSIXt')
y1 = y[paste(format(xlim, '%Y:%m:%d %H:%M:%S'), sep = '', collapse = '::')]
par(new = TRUE)
xlim = par('usr')[1:2]
plot( attr(y1, 'index') , y1[,1], xlim = xlim, xaxs = 'i', type = type,
yaxt = 'n', xaxt = 'n', xlab = '', ylab = '', axes = F, ... )
axis(2, las = las, ...)
}
plota.grid <- function()
{
abline( h = axTicks(2), col = 'lightgray', lty = 'dotted')
abline( v = plota.control$xaxis.ticks, col = 'lightgray', lty = 'dotted')
}
plota.lines <- function(
y,
type = 'l',
col = par('col'),
...
)
{
if(has.Cl(y)) y1 = Cl(y) else y1 = y[,1]
temp.x = attr(y, 'index')
if( type == 'l' & len(col) > 1 ) {
for( icol in unique(col) ) {
lines(temp.x, iif(col == icol, y1, NA), type = type, col = icol, ...)
}
} else {
lines(temp.x, y1, type = type, col = col, ...)
}
}
plota.format <- function(
temp,
nround = 2,
sprefix = '',
eprefix = ''
)
{
return( paste(sprefix,
format(round(as.numeric(temp), nround), big.mark = ',', scientific=FALSE),
eprefix ,sep='') )
}
plota.legend <- function
(
labels,
fill = NULL,
lastobs = NULL,
x = 'topleft',
merge = F,
bty = 'n',
yformat = plota.format,
...
)
{
if( !is.null(fill) ) fill = spl( as.character(fill) )
labels = spl( as.character(labels) )
if( !is.null(lastobs) ) {
if( is.list(lastobs) ) {
labels1 = sapply(lastobs, function(x) unclass(last(x))[1])
} else {
labels1 = unclass(last(lastobs))[1];
}
labels = paste(labels, match.fun(yformat)( labels1 ))
}
legend(x, legend = labels, fill = fill, merge = merge, bty = bty, ...)
}
plota.layout <- function(
ilayout,
delim = ','
)
{
ilayout = matrix( as.double(spl( gsub('\n', delim, ilayout), delim)),
nrow = len(spl(ilayout, '\n')), byrow=TRUE)
layout(mat = ilayout)
}
plota.dx <- function
(
y
)
{
xlim = par('usr')[1:2]
class(xlim) = c('POSIXct', 'POSIXt')
y1 = y[paste(format(xlim, '%Y:%m:%d %H:%M:%S'), sep = '', collapse = '::')]
xlim = par('usr')[1:2]
xportion = min(1, diff(unclass(range(attr(y1, 'index'))))*1.08 / diff(xlim) )
return( xportion * diff(xlim) / ( 2* nrow(y1)  ) )
}
plota.x.highlight <- function
(
y,
highlight,
col = plota.control$col.x.highlight
)
{
if(len(col)==1) {
plota.x.highlight.helper(y, highlight, col = col)
} else {
for( icol in unique(col[highlight]) ) {
plota.x.highlight.helper(y, iif(col == icol, highlight, FALSE), col = icol)
}
}
}
plota.x.highlight.helper <- function
(
y,
highlight,
col = plota.control$col.x.highlight
)
{
dx = plota.dx(y);
hl_index = highlight;
if( is.logical(highlight) ) hl_index = which(highlight);
if( identical(unique(highlight) , c(0, 1)) ) hl_index = which(as.logical(highlight));
hl_index1 = which(diff(hl_index) > 1 )
hl_index = hl_index[ sort(c(1, len(hl_index), hl_index1, (hl_index1+1))) ]
temp.y = par('usr')[3:4]
if(par('ylog')) temp.y = 10^temp.y
temp.x = attr(y, 'index')
for( i in seq(1,len(hl_index),2) ) {
rect(temp.x[hl_index[i]] - dx/2, temp.y[1],
temp.x[hl_index[(i + 1)]] + dx/2, temp.y[2],
col = col, border = col )
}
box();
}
plota.y.highlight <- function
(
y,
highlight,
col = plota.control$col.y.highlight
)
{
temp.y = par('usr')[3:4]
if(par('ylog')) temp.y = 10^temp.y
temp.x = par('usr')[1:2]
if(par('xlog')) temp.x = 10^temp.x
highlight[highlight == Inf] = temp.y[2]
highlight[highlight == -Inf] = temp.y[1]
for( i in seq(1,len(highlight),by=2) ) {
rect(temp.x[1], highlight[i],
temp.x[2], highlight[(i + 1)],
col = col, border = col )
}
box();
}
plota.candle.col <- function(	y ) {
return( iif( Cl(y)>Op(y), plota.control$col.up, plota.control$col.dn) )
}
plota.volume.col <- function( y ) {
return( iif( Cl(y)>mlag(Cl(y)), plota.control$col.up, plota.control$col.dn) )
}
plota.candle <- function
(
y,
col = plota.candle.col(y)
)
{
dx = plota.dx(y)
dxi0 = ( dx / xinch() ) * 96
if( dxi0 < 1 ) {
plota.hl.lwd(y, col = col, lwd = 1)
} else if ( dxi0 < 1.75 ) {
plota.ohlc.lwd(y, col = col, lwd = 1)
} else {
temp.x = attr(y, 'index')
rect(temp.x - dx/10, Lo(y), temp.x + dx/10, Hi(y),
col = plota.control$col.border, border = plota.control$col.border)
rect(temp.x - dx/2, Op(y), temp.x + dx/2, Cl(y),
col = col, border = plota.control$col.border)
}
}
plota.ohlc <- function
(
y,
col = plota.control$col.border
)
{
dx = plota.dx(y)
dxi0 = ( dx / xinch() ) * 96
if( dxi0 < 1 ) {
plota.hl.lwd(y, col = col, lwd = 1)
} else if ( dxi0 < 1.75 ) {
plota.ohlc.lwd(y, col = col, lwd = 1)
} else {
temp.x = attr(y, 'index')
rect(temp.x - dx/8, Lo(y), temp.x + dx/8, Hi(y), col = col, border = col)
segments(temp.x - dx/2, Op(y), temp.x, Op(y), col = col)
segments(temp.x + dx/2, Cl(y), temp.x, Cl(y), col = col)
}
}
plota.hl <- function
(
y,
col = plota.volume.col(y),
border = plota.control$col.border
)
{
dx = plota.dx(y)
dxi0 = ( dx / xinch() ) * 96
if( dxi0 < 1.75 ) {
plota.hl.lwd(y, col = col, lwd = 1)
} else {
temp.x = attr(y, 'index')
rect(temp.x - dx/2, Lo(y), temp.x + dx/2, Hi(y),
col = col, border = border)
}
}
plota.ohlc.lwd <- function
(
y,
lwd=1,
...
)
{
dx = plota.dx(y)
temp.x = attr(y, 'index')
segments(temp.x, Lo(y), temp.x, Hi(y), lwd = lwd, lend = 2,  ...)
segments(temp.x - dx/2, Op(y), temp.x, Op(y), lwd = lwd, lend = 2, ...)
segments(temp.x + dx/2, Cl(y), temp.x, Cl(y), lwd = lwd, lend = 2, ...)
}
plota.hl.lwd <- function
(
y,
lwd=1,
...
)
{
temp.x = attr(y, 'index')
segments(temp.x, Lo(y), temp.x, Hi(y), lwd = lwd, lend = 2, ...)
}
plota.volume <- function
(
y,
col = plota.volume.col(y),
border = plota.control$col.border
)
{
dx = plota.dx(y)
dxi0 = ( dx / xinch() ) * 96
temp.x = attr(y, 'index')
if( dxi0 < 1.75 ) {
segments(temp.x, 0, temp.x, Vo(y), col = col, lwd = 1, lend = 2)
} else {
rect(temp.x - dx/2, 0, temp.x + dx/2, Vo(y),
col = col, border = border)
}
idv = grep('Volume', colnames(y))
temp = spl(colnames(y)[idv], ';')
if( len(temp) > 1 ) legend('topright',legend = temp[len(temp)], bty='n');
}
plota.scale.volume <- function(y)
{
Volumes = Vo(y)
max.vol = max(Volumes, na.rm = T)
vol.scale = list(100, '100s')
if (max.vol > 10000)
vol.scale = list(1000, '1000s')
if (max.vol > 1e+05)
vol.scale = list(10000, '10,000s')
if (max.vol > 1e+06)
vol.scale = list(1e+05, '100,000s')
if (max.vol > 1e+07)
vol.scale = list(1e+06, 'millions')
idv = grep('Volume', colnames(y))
y[, idv] = Volumes/vol.scale[[1]]
colnames(y)[idv] = paste( colnames(y)[idv], vol.scale[[2]], sep=';' )
return(y)
}
plota.test <- function() {
load.packages('quantmod')
data.spy = getSymbols('SPY', from = '1980-01-01', auto.assign = FALSE)
data.ibm = getSymbols('IBM', from = '1980-01-01', auto.assign = FALSE)
y = data.spy['2011:01:01::2011:02:01']
highlight = which(Cl(y) < 127)
png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(c(1,1,2))
plota(y, type = 'candle', main = 'SPY', plotX = F, x.highlight = highlight)
y = plota.scale.volume(y)
plota(y, type = 'volume', x.highlight = highlight)
dev.off()
y = data.spy['2010:01:01::2011:02:01']
png(filename = 'plot2.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(c(1,1,2,3))
plota(y, type = 'candle', plotX = F)
plota.legend('SPY', 'blue', y)
y = plota.scale.volume(y)
plota(y, type = 'volume', plotX = F)
plota.legend('Volume', 'blue', Vo(y))
rsi = RSI(Cl(y),2)
plota(rsi, type = 'l', y.highlight = c(c(Inf,80),c(20,-Inf)))
abline(h = 20, col = 'red')
abline(h = 80, col = 'red')
plota.legend('RSI(2)', 'black', rsi)
dev.off()
y = data.spy['2010:01:01::2011:02:01']
png(filename = 'plot3.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
plota(y, type = 'ohlc', LeftMargin=3)
y0 = y;
y = data.ibm['2010:10:15::2011:02:01']
plota2Y(y, ylim = range(OHLC(y)),las=1, col='red', col.axis = 'red')
plota.ohlc(y, col = 'red')
plota.legend('SPY(rhs),IBM(lhs)', 'blue,red', list(y0,y))
dev.off()
y = data.spy['2010:01:01::2011:02:01']
png(filename = 'plot4.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
plota(y, type = 'candle')
y1 = to.monthly(y)
index(y1) = as.Date(index(y1))
plota.ohlc(y1, col = 'pink')
plota.candle(y)
plota.legend('Daily,Monthly', 'red,pink')
dev.off()
y = data.spy['2010:01:01::2011']
png(filename = 'plot5.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(c(1,2,3))
plota(y, type = 'candle', plotX = F)
plota.legend('Daily', 'blue', y)
plota(y, ylim = range(OHLC(y)), plotX = F)
y1 = to.weekly(y)
index(y1) = as.Date(index(y1))
plota.candle(y1)
plota.legend('Weekly', 'blue', y1)
plota(y, ylim = range(OHLC(y)))
y1 = to.monthly(y)
index(y1) = as.Date(index(y1))
plota.candle(y1)
plota.legend('Monthly', 'blue', y1)
dev.off()
}
plota.colors <- function(N) {
col = rev(c('yellow','cyan','magenta','red','gray','green','blue'))
temp = list()
for(j in 1:length(col)) {
temp[[j]] = colors()[grep(col[j],colors())]
temp[[j]] = temp[[j]][grep('^[^0-9]*$',temp[[j]])]
temp[[j]] = temp[[j]][order(nchar(temp[[j]]))]
index = which( colSums(col2rgb(temp[[j]])) < 100 )
if( length(index) > 0 ) temp[[j]] = temp[[j]][-index]
index = which( colSums(255 - col2rgb(temp[[j]])) < 100 )
if( length(index) > 0 ) temp[[j]] = temp[[j]][-index]
}
index = 1
col = rep('', N)
for(i in 1:10) {
for(j in 1:length(temp)) {
if(length(temp[[j]]) >= i) {
col[index] = temp[[j]][i]
index = index + 1
if(index > N) break
}
}
if(index > N) break
}
return(col)
}
plota.stacked <- function
(
x,
y,
xlab='',
col = plota.colors(ncol(y)),
type=c('l','s'),
...
)
{
y = 100 * y
y1 = list()
y1$positive = y
y1$positive[ y1$positive < 0 ] = 0
y1$negative = y
y1$negative[ y1$negative > 0 ] = 0
ylim = c(min(rowSums(y1$negative, na.rm = T)), max(1, rowSums(y1$positive, na.rm = T)))
if( class(x)[1] != 'Date' & class(x)[1] != 'POSIXct') {
plot(x, rep(0, len(x)), ylim = ylim, t = 'n', xlab = '', ylab = '', cex = par('cex'), ...)
grid()
} else {
plota(make.xts(y[,1], x), ylim = ylim, cex = par('cex'), LeftMargin = 4, ...)
axis(2, las = 1)
x = unclass(as.POSIXct(x))
}
mtext('Allocation %', side = 2,line = 3, cex = par('cex'))
mtext(xlab, side = 1,line = 2, cex = par('cex'))
if( type[1] == 'l' ) {
prep.x = c(x[1], x, x[len(x)])
for( y in y1 ) {
for (i in ncol(y) : 1) {
prep.y = c(0, rowSums(y[, 1 : i, drop = FALSE]), 0)
polygon(prep.x, prep.y, col = col[i], border = NA, angle = 90)
}
}
} else {
dx = mean(diff(x))
prep.x = c(rep(x,each=2), x[len(x)] + dx, x[len(x)] + dx)
for( y in y1 ) {
for (i in ncol(y) : 1) {
prep.y = c(0, rep(rowSums(y[, 1 : i, drop = FALSE]),each=2), 0)
polygon(prep.x, prep.y, col = col[i], border = NA, angle = 90)
}
}
}
plota.legend(colnames(y), col, cex = par('cex'))
}
plota.matplot <- function
(
y,
dates = NULL,
ylim = NULL,
type = 'l',
...
)
{
if( is.list(y) ) {
if(!is.null(dates)) y[[1]] = y[[1]][dates]
if(is.null(ylim)) {
ylim = c()
n = len(y)
for( i in 1:n ) {
if(!is.null(dates)) y[[i]] = y[[i]][dates]
ylim = range(ylim, y[[i]], na.rm = T)
}
}
plota(y[[1]], ylim = ylim, col = 1, type = type, ...)
if( n > 1 ) {
for( i in 2:n ) plota.lines(y[[i]], col = i, type = type, ...)
}
plota.legend(names(y), paste(1:n), y)
} else {
n = ncol(y)
if(!is.null(dates)) y = y[dates]
if(is.null(ylim)) ylim = range(y, na.rm = T)
plota(y[,1], ylim = ylim, col = 1, type = type, ...)
if( n > 1 ) {
for( i in 2:n ) plota.lines(y[,i], col = i, type = type, ...)
}
plota.legend(names(y), paste(1:n), as.list(y))
}
}
plota.add.copyright <- function(copyright = 'Systematic Investor') {
mtext(paste(copyright,'\uA9'), side = 1,line = -1, outer = T, adj = 1, font = 1, cex = 0.7, col='blue')
}
plota.recession <- function
(
col = col.add.alpha('gray', 50),	# Set alpha = 50 so it's relatively transparent
ylim = par('usr')[3:4],
...
)
{
recessions.df = read.table(textConnection(
"Peak, Trough
1857-06-01, 1858-12-01
1860-10-01, 1861-06-01
1865-04-01, 1867-12-01
1869-06-01, 1870-12-01
1873-10-01, 1879-03-01
1882-03-01, 1885-05-01
1887-03-01, 1888-04-01
1890-07-01, 1891-05-01
1893-01-01, 1894-06-01
1895-12-01, 1897-06-01
1899-06-01, 1900-12-01
1902-09-01, 1904-08-01
1907-05-01, 1908-06-01
1910-01-01, 1912-01-01
1913-01-01, 1914-12-01
1918-08-01, 1919-03-01
1920-01-01, 1921-07-01
1923-05-01, 1924-07-01
1926-10-01, 1927-11-01
1929-08-01, 1933-03-01
1937-05-01, 1938-06-01
1945-02-01, 1945-10-01
1948-11-01, 1949-10-01
1953-07-01, 1954-05-01
1957-08-01, 1958-04-01
1960-04-01, 1961-02-01
1969-12-01, 1970-11-01
1973-11-01, 1975-03-01
1980-01-01, 1980-07-01
1981-07-01, 1982-11-01
1990-07-01, 1991-03-01
2001-03-01, 2001-11-01
2007-12-01, 2009-06-01"),
sep=',', colClasses=c('Date', 'Date'), header=TRUE)
chart.max <- as.numeric(max(ylim))
chart.min <- as.numeric(min(ylim))
recessions.trim = subset(recessions.df, Peak >= as.Date(min(plota.control$xaxis.ticks)) )
if(len(recessions.trim) < 1) return
for(i in 1:length(recessions.trim$Peak)){
rect(xleft = as.POSIXct(recessions.trim$Peak[[i]]),
xright = as.POSIXct(recessions.trim$Trough[[i]]),
ybottom = chart.min, ytop = chart.max, col=col, border=col, ...)
}
}
randsphere <- function
(
m,
n,
r
)
{
x = matrix(rnorm( m * n ), nrow = m, ncol = n);
s2 = apply(x^2, 1, sum)
return( x * repmat(r*(runif(m)^(1/n))/sqrt(s2),1,n) )
}
randsphere.test <- function()
{
load.packages('car,scatterplot3d')
png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
x = randsphere(1000, 2, 1)
y = x[, 2]
x = x[, 1]
par(mar = c(5,4,1,1))
plot(x,y, pch = 20)
ellipse(c(0, 0), matrix(c(1, 0, 0, 1), nrow = 2), radius = 1)
dev.off()
png(filename = 'plot2.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(matrix(1:4,nrow=2))
x = randsphere(10000, 3, 1)
z = x[, 3]
y = x[, 2]
x = x[, 1]
scatterplot3d(x, y, z, highlight.3d = TRUE, pch = 20)
par(mar = c(5,4,1,1))
plot(x, y, pch = 20)
ellipse(c(0, 0), matrix(c(1, 0, 0, 1), nrow = 2), radius = 1)
plot(x, z, pch = 20)
ellipse(c(0, 0), matrix(c(1, 0, 0, 1), nrow = 2), radius = 1)
plot(y, z, pch = 20)
ellipse(c(0, 0), matrix(c(1, 0, 0, 1), nrow = 2), radius = 1)
dev.off()
}
randfixedsum <- function
(
m,
n,
s,
a,
b
)
{
if( (s<n*a) | (s>n*b) | (a>=b) )
stop('Inequalities n*a <= s <= n*b and a < b must hold.\n')
s = (s - n * a) / (b - a)
k = max( min( floor(s), n - 1), 0)
s = max( min( s, k + 1), k)
s1 = s - (k : (k - n + 1))
s2 = ((k + n) : (k+1)) - s
w = matrix(0, n, (n + 1))
realmax = 10^300
w[1, 2] = realmax
t = matrix(0, (n-1), n)
tiny = 2^(-1074)
for( i in 2:n ) {
tmp1 = w[(i - 1), 2 : (i + 1)] * s1[1 : i] / i
tmp2 = w[(i - 1), 1 : i] * s2[(n - i + 1) : n] / i
w[i, 2 : (i + 1)] = tmp1 + tmp2
tmp3 = w[i, 2 : (i+1)] + tiny
tmp4 = (s2[(n - i + 1) : n] > s1[1:i])
t[(i - 1), 1 : i] = (tmp2 / tmp3) * tmp4 + (1 - tmp1 / tmp3) * (!tmp4)
}
v = n^(3/2) * (w[n, (k + 2)] / realmax) * (b - a)^(n - 1)
x = matrix(0, n, m)
rt = matrix( runif((n-1) * m), (n-1), m)
rs = matrix( runif((n-1) * m), (n-1), m)
s = repmat(s, 1, m)
j = repmat((k + 1), 1, m)
sm = matrix(0, 1, m)
pr = matrix(1, 1, m)
for( i in (n - 1):1) {
e = (rt[(n - i), ] <= t[i, j])
sx = rs[(n - i), ]^(1/i)
sm = sm + (1 - sx) * pr * s / (i+1)
pr = sx * pr
x[(n - i), ] = sm + pr * e
s = s - e
j = j - e
}
x[n, ] = sm + pr * s
rp = matrix( runif(n * m), n, m)
p = apply(rp, 2, order)
x = (b - a) * x[p + repmat(t(seq(0, n * (m - 1), by = n)), n, 1)] + a
x = matrix(x, n, m)
return(t(x))
}
randfixedsum.test <- function()
{
load.packages('car,scatterplot3d')
png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
x = randfixedsum(100, 2, 1, 0.2, 0.8)
y = x[, 2]
x = x[, 1]
par(mar = c(5,4,1,1))
plot(x,y, pch = 20)
dev.off()
png(filename = 'plot2.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(matrix(1:4,nrow=2))
x = randfixedsum(1000, 3, 1, 0.2, 0.8)
z = x[, 3]
y = x[, 2]
x = x[, 1]
scatterplot3d(x, y, z, highlight.3d = TRUE, pch = 20, angle=190)
par(mar = c(5,4,1,1))
plot(x, y, pch = 20)
plot(x, z, pch = 20)
plot(y, z, pch = 20)
dev.off()
}
asset.paths <- function(s0, mu, sigma,
nsims = 10000,
periods = c(0, 1)
)
{
s0 = as.vector(s0)
nsteps = len(periods)
dt = c(periods[1], diff(periods))
if( len(s0) == 1 ) {
drift = mu - 0.5 * sigma^2
if( nsteps == 1 ) {
s0 * exp(drift * dt + sigma * sqrt(dt) * rnorm(nsims))
} else {
temp = matrix(exp(drift * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims)
for(i in 2:nsteps) temp[i,] = temp[i,] * temp[(i-1),]
s0 * temp
}
} else {
require(MASS)
drift = mu - 0.5 * diag(sigma)
n = len(mu)
if( nsteps == 1 ) {
s0 * exp(drift * dt + sqrt(dt) * t(mvrnorm(nsims, rep(0, n), sigma)))
} else {
temp = array(exp(as.vector(drift %*% t(dt)) + t(sqrt(dt) * mvrnorm(nsteps * nsims, rep(0, n), sigma))), c(n, nsteps, nsims))
for(i in 2:nsteps) temp[,i,] = temp[,i,] * temp[,(i-1),]
s0 * temp
}
}
}
asset.paths.test <- function()
{
S = c(100,105)
X = 98
Time = 0.5
r = 0.05
sigma = c(0.11,0.16)
rho = 0.63
N = 10000
periods = 0:10
prices = asset.paths(S[1], r, sigma[1], N, periods = periods)
png(filename = 'plot1.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
matplot(prices[,1:100], type='l', xlab='Years', ylab='Prices',
main='Selected Price Paths')
dev.off()
periods = 0:10
cov.matrix = sigma%*%t(sigma) * matrix(c(1,rho,rho,1),2,2)
prices = asset.paths(S, c(r,r), cov.matrix, N, periods = periods)
png(filename = 'plot2.png', width = 600, height = 600, units = 'px', pointsize = 12, bg = 'white')
layout(1:2)
matplot(prices[1,,1:100], type='l', xlab='Years', ylab='Prices',
main='Selected Price Paths for Asset 1')
matplot(prices[2,,1:100], type='l', xlab='Years', ylab='Prices',
main='Selected Price Paths for Asset 2')
dev.off()
cor(as.vector(prices[1,,] / mlag(prices[1,,])),
as.vector(prices[2,,] / mlag(prices[2,,])),
use='complete.obs', method='pearson')
load.packages('fOptions')
GBSOption(TypeFlag = "c", S = S[1], X = X, Time = Time, r = r, b = r, sigma = sigma[1])
N = 1000000
prices = asset.paths(S[1], r, sigma[1], N, periods = Time)
future.payoff = pmax(0, prices - X)
discounted.payoff = future.payoff * exp(-r * Time)
mean(discounted.payoff)
load.packages('fExoticOptions')
Time = 1/12
TurnbullWakemanAsianApproxOption(TypeFlag = "c", S = S[1], SA = S[1],
X = X, Time = Time, time = Time, tau = 0 , r = r, b = r, sigma = sigma[1])
N = 100000
periods = seq(0,Time,1/360)
n = len(periods)
prices = asset.paths(S[1], r, sigma[1], N, periods = periods)
future.payoff = pmax(0, colSums(prices)/n - X)
discounted.payoff = future.payoff * exp(-r * Time)
mean(discounted.payoff)
Time = 0.5
TwoRiskyAssetsOption(TypeFlag = "cmax", S1 = S[1], S2 = S[2],
X = X, Time = Time, r = r, b1 = r, b2 = r,
sigma1 = sigma[1], sigma2 = sigma[2], rho = rho)
N = 100000
cov.matrix = sigma%*%t(sigma) * matrix(c(1,rho,rho,1),2,2)
prices = asset.paths(S, c(r,r), sigma = cov.matrix, N, periods = Time)
future.payoff = pmax(0, apply(prices,2,max) - X)
discounted.payoff = future.payoff * exp(-r * Time)
mean(discounted.payoff)
Time = 1/12
N = 10000
periods = seq(0,Time,1/360)
n = len(periods)
prices = asset.paths(S, c(r,r), sigma = cov.matrix, N, periods = periods)
future.payoff = pmax(0, colSums(apply(prices,c(2,3),max))/n - X)
discounted.payoff = future.payoff * exp(-r * Time)
mean(discounted.payoff)
}
all.permutations <- function(n = 1) {
m = matrix(F,2^n,n)
m[2,1] = T
if (n == 1) return(m)
istart = 2
for(i in 2:n) {
index = (istart+1):(2*istart)
m[index, ] = m[1:istart,]
m[index, i] = T
istart = istart * 2
}
return(m)
}
seasonality.test <- function()
{
load.packages('quantmod')
ticker = 'WMT'
data = getSymbols(ticker, src = 'yahoo', from = '1970-01-01', auto.assign = F)
data = adjustOHLC(data, use.Adjusted=T)
data = data['1980::2012:04:07']
png(filename = 'plot.month.year.seasonality.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
month.year.seasonality(data, ticker)
dev.off()
month.ends = endpoints(data, 'months')
month.ends = month.ends[month.ends > 0 & month.ends < nrow(data)]
index = which(format(index(data), '%b')[month.ends] == 'Apr')
png(filename = 'plot.time.seasonality.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1)
time.seasonality(data, 1 + month.ends[index], 20, ticker)
dev.off()
}
pattern.test <- function()
{
load.packages('quantmod')
ticker = 'SPY'
data = getSymbols(ticker, src = 'yahoo', from = '1970-01-01', auto.assign = F)
data = adjustOHLC(data, use.Adjusted=T)
data = data['::2012:04:07']
png(filename = 'plot1.time.series.pattern.matching.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
matches = bt.matching.find(Cl(data), main = ticker,	n.query=90, plot=TRUE)
dev.off()
png(filename = 'plot2.time.series.pattern.matching.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
out = bt.matching.overlay(matches, plot=TRUE)
dev.off()
png(filename = 'plot.pattern.matching.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot.patterns(data, 190, ticker)
dev.off()
}
month.year.seasonality <- function
(
data,
ticker,
lookback.len = 20*252
)
{
data = last(data, lookback.len)
nperiods = nrow(data)
month.ends = endpoints(data, 'months')
month.ends = month.ends[month.ends > 0]
prices = Cl(data)[month.ends]
ret = prices / mlag(prices) - 1
ret = ret[-1]
ret.by.month = create.monthly.table(ret)
data_list = lapply(apply(ret.by.month, 2, list), '[[', 1)
group.seasonality(data_list, paste(ticker, 'Monthly', join(format(range(index(data)), '%d-%b-%Y'), ' to\n')))
}
group.seasonality <- function
(
data_list,
smain,
...
)
{
out = compute.stats( data_list,
list(Sharpe=function(x) mean(x,na.rm=T)/sd(x,na.rm=T),
'% Positive'=function(x) sum(x > 0,na.rm=T)/sum(!is.na(x)),
Min=function(x) min(x,na.rm=T),
Max=function(x) max(x,na.rm=T),
Avg=function(x) mean(x,na.rm=T),
Med=function(x) median(x,na.rm=T),
StDev=function(x) sd(x,na.rm=T)
)
)
layout(mat=matrix(1:4, 2, 2, byrow=FALSE))
par(mar=c(4, 3, 2, 2))
col = spl('lightgray,red')
stats.names = spl('Sharpe,% Positive,Min,Avg')
for(i in stats.names) {
barplot(100*out[i,], names.arg = colnames(out),
col=iif(out[i,] > 0, col[1], col[2]),
main=iif(i == stats.names[1], paste(smain,' ', i, sep=''), i),
border = 'darkgray',las=2, ...)
grid(NA,NULL)
abline(h=0, col='black')
}
}
time.seasonality <- function
(
data,
period.starts,
period.len,
ticker
)
{
nperiods = len(period.starts)
dates = index(data)
ndates = len(dates)
prices = Cl(data)
ret = prices / mlag(prices) - 1
ret = c( as.double(ret), rep(NA, period.len))
trading.days = sapply(period.starts, function(i) ret[i : (i + period.len - 1)])
periods.index = 1:nperiods
temp = count(trading.days)
periods.index = periods.index[temp > 0.9 * median(temp)]
last.period = trading.days[, nperiods]
last.period = 100 * ( cumprod(1 + last.period) - 1 )
avg.period = apply(trading.days[, periods.index], 1, mean, na.rm=T)
avg.period = 100 * ( cumprod(1 + avg.period) - 1 )
temp = 100*(apply(1 + trading.days[, periods.index], 2, cumprod) - 1)
quantiles = apply(temp, 1, quantile, probs = c(75, 25)/100, na.rm=T)
cols = spl('blue,red,gray')
par(mar=c(4,4,2,1))
plot(avg.period, type='n', xaxt = 'n', xlim=c(1,period.len),
ylim=range(avg.period, last.period, quantiles, na.rm=T),
main = ticker, xlab = 'Trading Days', ylab = 'Avg % Profit/Loss')
grid()
axis(1, 1:period.len)
lines(quantiles[1,], type='l', lwd=2, col=cols[3])
lines(quantiles[2,], type='l', lwd=2, col=cols[3])
lines(last.period, type='b', lwd=2, col=cols[2], bg=cols[2], pch=24)
lines(avg.period, type='b', lwd=2, col=cols[1], bg=cols[1], pch=22)
first.year = format(dates[period.starts][periods.index[1]], '%Y')
last.year = format(dates[period.starts][last(periods.index)], '%Y')
last.period.start.date = format(dates[period.starts[nperiods]], '%d %b %Y')
last.period.end.date = format(dates[ndates], '%d %b %Y')
if( (period.starts[nperiods] + period.len - 1) < ndates ) {
last.period.end.date = format(dates[period.starts[nperiods] + period.len - 1], '%d %b %Y')
}
plota.legend(c(paste('Avgerage for', first.year, '-', last.year),
paste(last.period.start.date, '-', last.period.end.date),
'Top 25% / Bot 25%'), cols)
}
plot.patterns <- function
(
data,
n,
ticker,
patterns = pattern.db()
)
{
load.packages('sm')
sample = last(data, n)
obj = find.extrema( Cl(sample) )
mhat = obj$mhat
mhat.extrema.loc = obj$mhat.extrema.loc
data.extrema.loc = obj$data.extrema.loc
n.index = len(data.extrema.loc)
plota.control$col.border = 'gray'
plota(sample, type='hl',col='gray')
plota.lines(mhat, col='magenta', lwd=2)
plota.lines(sample, col='blue')
if(n.index > 0) {
plota.lines(sample[data.extrema.loc], type='p', col='blue', lwd=3, pch=19)
out = find.patterns(obj, patterns = patterns, silent=F, plot=T)
}
plota.legend(c(paste(ticker, join(format(range(index(sample)), '%d%b%Y'), ' - ')),
'Close,Kernel,Pattern(s)'),
'gray,blue,magenta,orange')
}
find.extrema <- function(
x
)
{
if(is.xts(x)) {
y = as.vector( Cl(x) )
} else {
y = x
}
n = len(y)
t = 1:n
h = h.select(t, y, method = 'cv')
temp = sm.regression(t, y, h=h, display = 'none')
mhat = approx(temp$eval.points, temp$estimate, t, method='linear')$y
temp = diff(sign(diff(mhat)))
loc = which( temp != 0 ) + 1
loc.dir = -sign(temp[(loc - 1)])
temp = c( y[1], y, y[n] )
temp = cbind(temp[loc], temp[(loc + 1)], temp[(loc + 2)])
max.index = loc + apply(temp, 1, which.max) - 2
min.index = loc + apply(temp, 1, which.min) - 2
data.loc = iif(loc.dir > 0, max.index, min.index)
data.loc = iif(data.loc < 1, 1, iif(data.loc > n, n, data.loc))
if(is.xts(x)) mhat = make.xts(mhat, index(x))
return(list(data = y, mhat = mhat, extrema.dir = loc.dir,
mhat.extrema.loc = loc, data.extrema.loc = data.loc))
}
find.patterns <- function
(
obj,
patterns = pattern.db(),
silent=T,
plot=T
)
{
data = obj$data
mhat = obj$mhat
extrema.dir = obj$extrema.dir
data.extrema.loc = obj$data.extrema.loc
n.index = len(data.extrema.loc)
if(is.xts(mhat)) {
dates = index4xts(obj$mhat)
} else {
dates = 1:len(data)
}
col = col.add.alpha('orange', alpha=150)
out = out.rownames = c()
for(i in 1:n.index) {
for(pattern in patterns) {
if( pattern$start * extrema.dir[i] > 0 ) {
if( i + pattern$len - 1 <= n.index ) {
envir.data = c(data[data.extrema.loc][i:(i + pattern$len - 1)],
data.extrema.loc[i:(i + pattern$len - 1)])
names(envir.data) = c(paste('E', 1:pattern$len, sep=''),
paste('t', 1:pattern$len, sep=''))
envir.data = as.list(envir.data)
envir.data$E = data[data.extrema.loc][-c(1:i)]
envir.data$t = data.extrema.loc[-c(1:i)]
if( eval(pattern$formula, envir = envir.data) ) {
if(!silent) cat('Found', pattern$name, 'at', i, '\n')
if(plot & !is.null(pattern$plot)) {
temp = dates[data.extrema.loc[i:(i + pattern$len - 1)]]
names(temp) = paste('d', 1:pattern$len, sep='')
envir.data = c( envir.data, temp )
envir.data$d = dates[data.extrema.loc[-c(1:i)]]
envir.data$col = col
eval(pattern$plot, envir = envir.data)
}
out.rownames = c(out.rownames, pattern$name)
out = rbind(out, c(data.extrema.loc[i],
iif(is.null(pattern$last.point),
data.extrema.loc[(i + pattern$len - 1)],
eval(pattern$last.point, envir = envir.data)
)))
}
}
}
}
}
if(len(out)>0) {
colnames(out) = spl('start,end')
rownames(out) = out.rownames
}
return(out)
}
find.all.patterns.window <- function()
{
load.packages('quantmod')
ticker = 'SPY'
data = getSymbols(ticker, src = 'yahoo', from = '1970-01-01', auto.assign = F)
data = adjustOHLC(data, use.Adjusted=T)
data = data['2010::']
load.packages('sm')
history = as.vector(coredata(Cl(data)))
window.len = 90
patterns = pattern.db()
found.patterns = c()
for(t in window.len : (len(history)-1)) {
sample = history[(t - window.len + 1):t]
obj = find.extrema( sample )
if(len(obj$data.extrema.loc) > 0) {
out =  find.patterns(obj, patterns = patterns, silent=F, plot=F)
if(len(out)>0) found.patterns = rbind(found.patterns,cbind(t,out,t-window.len+out))
}
if( t %% 10 == 0) cat(t, 'out of', len(history), '\n')
}
colnames(found.patterns) = spl('t,start,end,tstart,tend')
setdiff(unique(names(patterns)), unique(rownames(found.patterns)))
frequency = tapply(rep(1,nrow(found.patterns)), rownames(found.patterns), sum)
barplot(frequency)
index = which(rownames(found.patterns)=='HS')
found.patterns[ index[1:10],]
pattern.index = index[1]
t = found.patterns[pattern.index, 't'];
plot.patterns(data[1:t,], window.len, ticker, patterns)
sample = data[(t - window.len + 1):t]
start.pattern = found.patterns[pattern.index, 'start']
end.pattern = found.patterns[pattern.index, 'end']
abline(v = index(sample)[start.pattern]);
abline(v = index(sample)[end.pattern]);
index(sample)[start.pattern]
index(data[(t - window.len + start.pattern),])
}
bt.patterns.test <- function()
{
load.packages('quantmod')
ticker = 'SPY'
data = getSymbols(ticker, src = 'yahoo', from = '1970-01-01', auto.assign = F)
data = adjustOHLC(data, use.Adjusted=T)
load.packages('sm')
history = as.vector(coredata(Cl(data)))
window.L = 35
window.d = 3
window.len = window.L + window.d
patterns = pattern.db()
found.patterns = c()
for(t in window.len : (len(history)-1)) {
ret = history[(t+1)]/history[t]-1
sample = history[(t - window.len + 1):t]
obj = find.extrema( sample )
if(len(obj$data.extrema.loc) > 0) {
out =  find.patterns(obj, patterns = patterns, silent=F, plot=F)
if(len(out)>0) found.patterns = rbind(found.patterns,cbind(t,out,t-window.len+out, ret))
}
if( t %% 10 == 0) cat(t, 'out of', len(history), '\n')
}
colnames(found.patterns) = spl('t,start,end,tstart,tend,ret')
found.patterns = found.patterns[found.patterns[,'end'] <= window.L,]
pattern.names = unique(rownames(found.patterns))
all.patterns = c()
for(name in pattern.names) {
index = which(rownames(found.patterns) == name)
temp = NA * found.patterns[index,]
i.count = 0
i.start = 1
while(i.start < len(index)) {
i.count = i.count + 1
temp[i.count,] = found.patterns[index[i.start],]
subindex = which(found.patterns[index,'tstart'] > temp[i.count,'tend'])
if(len(subindex) > 0) {
i.start = subindex[1]
} else break
}
all.patterns = rbind(all.patterns, temp[1:i.count,])
}
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
frequency = tapply(rep(1,nrow(all.patterns)), rownames(all.patterns), sum)
layout(1)
barplot.with.labels(frequency/100, 'Frequency for each Pattern')
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
all.patterns[,'ret'] = history[(all.patterns[,'t']+20)] / history[all.patterns[,'t']] - 1
data_list = tapply(all.patterns[,'ret'], rownames(all.patterns), list)
group.seasonality(data_list, '20 days after Pattern')
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
layout(1)
name = 'BBOT'
index = which(rownames(all.patterns) == name)
time.seasonality(data, all.patterns[index,'t'], 20, name)
dev.off()
}
pattern.db <- function()
{
patterns = list()
pattern = list()
pattern$len = 5
pattern$start = 'max'
pattern$formula = expression(
E3 > E1 &
E3 > E5 &
abs(E1 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
abs(E5 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
abs(E2 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2 &
abs(E4 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2
)
pattern$plot = expression({
lines(c(d1,d2,d3,d4,d5), c(E1,E2,E3,E4,E5), lwd=10, col=col)
text(d3, E3, 'HS', adj=c(0.5,-0.5), xpd=TRUE)
})
patterns$HS = pattern
pattern = list()
pattern$len = 5
pattern$start = 'min'
pattern$formula = expression(
E3 < E1 &
E3 < E5 &
abs(E1 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
abs(E5 - (E1+E5)/2) < 1.5/100 * (E1+E5)/2 &
abs(E2 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2 &
abs(E4 - (E2+E4)/2) < 1.5/100 * (E2+E4)/2
)
pattern$plot = expression({
lines(c(d1,d2,d3,d4,d5), c(E1,E2,E3,E4,E5), lwd=10, col=col)
text(d3, E3, 'IHS', adj=c(0.5,1), xpd=TRUE)
})
patterns$IHS = pattern
pattern = list()
pattern$len = 5
pattern$start = 'max'
pattern$formula = expression(
E1 < E3 &
E3 < E5 &
E2 > E4
)
pattern$plot = expression({
beta = ols(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients
lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=10, col=col)
lines(c(d2,d4), c(E2,E4), lwd=10, col=col)
text(d3, min(E2,E4), 'BTOP', adj=c(0.5,1), xpd=TRUE)
})
patterns$BTOP = pattern
pattern = list()
pattern$len = 5
pattern$start = 'min'
pattern$formula = expression(
E1 > E3 &
E3 > E5 &
E2 < E4
)
pattern$plot = expression({
beta = ols(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients
lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=10, col=col)
lines(c(d2,d4), c(E2,E4), lwd=10, col=col)
text(d3, max(E2,E4), 'BBOT', adj=c(0.5,0), xpd=TRUE)
})
patterns$BBOT = pattern
pattern = list()
pattern$len = 5
pattern$start = 'max'
pattern$formula = expression(
E1 > E3 &
E3 > E5 &
E2 < E4
)
pattern$plot = expression({
beta = ols(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients
lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=10, col=col)
lines(c(d2,d4), c(E2,E4), lwd=10, col=col)
text(d3, min(E2,E4), 'TTOP', adj=c(0.5,1), xpd=TRUE)
})
patterns$TTOP = pattern
pattern = list()
pattern$len = 5
pattern$start = 'min'
pattern$formula = expression(
E1 < E3 &
E3 < E5 &
E2 > E4
)
pattern$plot = expression({
beta = ols(cbind(1,c(t1,t3,t5)),c(E1,E3,E5))$coefficients
lines(c(d1,d3,d5), beta[1] + beta[2]*c(t1,t3,t5), lwd=10, col=col)
lines(c(d2,d4), c(E2,E4), lwd=10, col=col)
text(d3, max(E2,E4), 'TBOT', adj=c(0.5,0), xpd=TRUE)
})
patterns$TBOT = pattern
pattern = list()
pattern$len = 5
pattern$start = 'max'
pattern$formula = expression({
avg.top = (E1+E3+E5)/3
avg.bop = (E2+E4)/2
abs(E1 - avg.top) < 0.75/100 * avg.top &
abs(E3 - avg.top) < 0.75/100 * avg.top &
abs(E5 - avg.top) < 0.75/100 * avg.top &
abs(E2 - avg.bop) < 0.75/100 * avg.bop &
abs(E4 - avg.bop) < 0.75/100 * avg.bop &
min(E1,E3,E5) > max(E2,E4)
})
pattern$plot = expression({
avg.top = (E1+E3+E5)/3
avg.bop = (E2+E4)/2
lines(c(d1,d3,d5), rep(avg.top,3), lwd=10, col=col)
lines(c(d2,d4), rep(avg.bop,2), lwd=10, col=col)
text(d3, min(E2,E4), 'RTOP', adj=c(0.5,-0.5), xpd=TRUE)
})
patterns$RTOP = pattern
pattern = list()
pattern$len = 5
pattern$start = 'min'
pattern$formula = expression({
avg.top = (E2+E4)/2
avg.bop = (E1+E3+E5)/3
abs(E2 - avg.top) < 0.75/100 * avg.top &
abs(E4 - avg.top) < 0.75/100 * avg.top &
abs(E1 - avg.bop) < 0.75/100 * avg.bop &
abs(E3 - avg.bop) < 0.75/100 * avg.bop &
abs(E5 - avg.bop) < 0.75/100 * avg.bop &
min(E2,E4) > max(E1,E3,E5)
})
pattern$plot = expression({
avg.top = (E2+E4)/2
avg.bop = (E1+E3+E5)/3
lines(c(d1,d3,d5), rep(avg.bop,3), lwd=10, col=col)
lines(c(d2,d4), rep(avg.top,2), lwd=10, col=col)
text(d3, max(E2,E4), 'RBOT', adj=c(0.5,0), xpd=TRUE)
})
patterns$RBOT = pattern
pattern = list()
pattern$len = 3
pattern$start = 'max'
pattern$formula = expression({
second.top = max(E)
second.top.t = t[which.max(E)]
avg = (E1 + second.top)/2
abs(E1         - avg) < 1.5/100 * avg &
abs(second.top - avg) < 1.5/100 * avg &
second.top.t - t1 > 22
})
pattern$plot = expression({
second.top = max(E)
second.top.d = d[which.max(E)]
avg = (E1 + second.top)/2
points(c(d1, second.top.d), c(E1, second.top), pch=2, lwd=2)
lines(c(d1, second.top.d), rep(avg, 2), lwd=10, col=col)
text(d2, avg, 'DTOP', adj=c(0.5,-0.5), xpd=TRUE)
})
pattern$last.point = expression(t[which.max(E)])
patterns$DTOP = pattern
pattern = list()
pattern$len = 3
pattern$start = 'min'
pattern$formula = expression(
abs(E1 -         (E1+min(E))/2) < 1.5/100 * (E1+min(E))/2 &
abs(max(E[-1]) - (E1+min(E))/2) < 1.5/100 * (E1+min(E))/2 &
t[which.min(E)] - t1 > 22
)
pattern$plot = expression({
second.bot = min(E)
second.bot.d = d[which.min(E)]
avg = (E1 + second.bot)/2
points(c(d1, second.bot.d), c(E1, second.bot), pch=2, lwd=2)
lines(c(d1, second.bot.d), rep(avg, 2), lwd=10, col=col)
text(d2, avg, 'DBOT', adj=c(0.5,1), xpd=TRUE)
})
pattern$last.point = expression(t[which.min(E)])
patterns$DBOT = pattern
for(i in 1:len(patterns)) {
patterns[[i]]$name = names(patterns)[i]
patterns[[i]]$start = iif(patterns[[i]]$start == 'max', 1, -1)
}
return(patterns)
}
bt.cluster.risk.parity.weights.test <- function()
{
load.packages('quantmod')
tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')
map = spl('Gold GLD,US Dollar UUP,S&P500 SPY,Nasdaq QQQ,Small Cap IWM,EmergingM EEM,InternationalM EFA,Real Estate IYR,Oil USO,Treasurys TLT')
names(map) = tickers
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
for(i in ls(data)) data[[ map[i] ]] = data[[i]]
rm(list=tickers, envir=data)
bt.prep(data, align='remove.na', dates = '2011:12::2012')
periodicity = 'months'
lookback.len = 250
cluster.group <<- cluster.group.kmeans.90
obj = portfolio.allocation.helper(data$prices,
periodicity = periodicity, lookback.len = lookback.len,
min.risk.fns = list(
EW=equal.weight.portfolio,
RP=risk.parity.portfolio(),
ERC=equal.risk.contribution.portfolio,
G2.EW = distribute.weights(equal.weight.portfolio, cluster.group),
G2.RP=distribute.weights(risk.parity.portfolio(), cluster.group),
G2.MV=distribute.weights(min.var.portfolio, cluster.group),
G2.ERC=distribute.weights(equal.risk.contribution.portfolio, cluster.group)
),
adjust2positive.definite = F,
custom.stats.fn = portfolio.allocation.custom.stats.clusters
)
clusters = coredata(obj$clusters$EW)[13,]
temp = clusters
temp[] = 0
temp[clusters == clusters[names(clusters) == 'Treasurys TLT']] = 1
temp[clusters == clusters[names(clusters) == 'US Dollar UUP']] = 2
temp[clusters == clusters[names(clusters) == 'EmergingM EEM']] = 3
temp[clusters == clusters[names(clusters) == 'Gold GLD']] = 4
clusters = temp
png(filename = 'plot1.png', width = 1200, height = 600, units = 'px', pointsize = 12, bg = 'white')
layout(matrix(1:2,nc=2))
plot.cluster.weights(coredata(obj$weights$ERC)[13,], clusters,
main='ERC Weights')
plot.cluster.weights(coredata(obj$risk.contributions$ERC)[13,], clusters,
main='ERC Risk Contributions')
dev.off()
png(filename = 'plot2.png', width = 1200, height = 600, units = 'px', pointsize = 12, bg = 'white')
layout(matrix(1:2,nc=2))
plot.cluster.weights(coredata(obj$weights$G2.ERC)[13,], clusters,
main='Cluster ERC Weights')
plot.cluster.weights(coredata(obj$risk.contributions$G2.ERC)[13,], clusters,
main='Cluster ERC Risk Contributions')
dev.off()
}
bt.cluster.risk.parity.10.major.assets <- function()
{
tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD')
dates='2004:12::'
name = 'ETFs AAA'
load.packages('quantmod')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '2000-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates=dates, fill.gaps=T)
periodicity = 'weeks'
lookback.len = 250
cluster.group <<- cluster.group.kmeans.90
obj = portfolio.allocation.helper(data$prices,
periodicity = periodicity, lookback.len = lookback.len,
min.risk.fns = list(
EW=equal.weight.portfolio,
RP=risk.parity.portfolio(),
ERC=equal.risk.contribution.portfolio,
Dynamic.EW = distribute.weights(equal.weight.portfolio, cluster.group),
Dynamic.RP=distribute.weights(risk.parity.portfolio(), cluster.group),
Dynamic.ERC=distribute.weights(equal.risk.contribution.portfolio, cluster.group)
),
adjust2positive.definite = F,
custom.stats.fn = portfolio.allocation.custom.stats
)
models = create.strategies(obj, data, dates=(lookback.len):nrow(data$prices))$models
title = paste(name, '(' ,periodicity, ',' , lookback.len, 'days )')
stats = bt.summary.report(models, title, data, obj,
control = list(
plot.weight.transition.maps = F,
plot.risk.contribution.transition.maps = F)
)
}
bt.cluster.risk.parity.dow.30 <- function()
{
load.packages('quantmod')
dates='1995::'
name = 'Dow Jones 30'
data = load.dow.jones(align='keep.all', dates=dates)
sectors = data$sectors
tickers = data$symbolnames
periodicity = 'weeks'
lookback.len = 250
cluster.group <<- cluster.group.kmeans.90
obj = portfolio.allocation.helper(data$prices,
periodicity = periodicity, lookback.len = lookback.len,
min.risk.fns = list(EW=equal.weight.portfolio,
RP=risk.parity.portfolio(),
ERC=equal.risk.contribution.portfolio,
Static.EW = distribute.weights(equal.weight.portfolio, static.group(as.numeric(sectors))),
Static.RP=distribute.weights(risk.parity.portfolio(), static.group(sectors)),
Static.ERC=distribute.weights(equal.risk.contribution.portfolio, static.group(sectors)),
Dynamic.EW = distribute.weights(equal.weight.portfolio, cluster.group),
Dynamic.RP=distribute.weights(risk.parity.portfolio(), cluster.group),
Dynamic.ERC=distribute.weights(equal.risk.contribution.portfolio, cluster.group)
),
adjust2positive.definite = F,
custom.stats.fn = portfolio.allocation.custom.stats
)
models = create.strategies(obj, data, dates=(lookback.len):nrow(data$prices))$models
title = paste(name, '(' ,periodicity, ',' , lookback.len, 'days )')
stats = bt.summary.report(models, title, data, obj,
control = list(
plot.weight.transition.maps = F,
plot.risk.contribution.transition.maps = F)
)
}
load.dow.jones <- function(align='remove.na', dates = NULL) {
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
tickers.desc = spl('ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities')
sector.map = c()
for(i in 1:len(tickers)) {
sector.map = rbind(sector.map,
cbind(sector.spdr.components(tickers[i]), tickers.desc[i])
)
}
colnames(sector.map) = spl('ticker,sector')
load.packages('quantmod')
tickers = dow.jones.components()
sectors = factor(sector.map[ match(tickers, sector.map[,'ticker']), 'sector'])
names(sectors) = tickers
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align=align,dates=dates)
data$sectors = sectors[data$symbolnames]
return(data)
}
portfolio.allocation.custom.stats.clusters <- function(x,ia) {
risk.contributions = portfolio.risk.contribution(x, ia)
clusters = cluster.group(ia)
return(list(
risk.contributions = risk.contributions,
clusters = clusters,
ncluster = max(clusters)
))
}
bt.summary.report <- function(models, title, data, obj=NULL,
control = list(
plot.weight.transition.maps = F,
plot.risk.contribution.transition.maps = !is.null(obj)
)
) {
if(is.null(control$plot.weight.transition.maps)) control$plot.weight.transition.maps = F
if(is.null(control$plot.risk.contribution.transition.maps)) control$plot.risk.contribution.transition.maps = obj!=NULL
filename = title
filename.pdf = paste(filename, '.pdf', sep='')
filename.csv = paste(filename, '.csv', sep='')
pdf(file = filename.pdf, width=8.5, height=11)
layout(1:2)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title)
mtext('Cumulative Performance', side = 2, line = 1)
out = plotbt.strategy.sidebyside(models, return.table=T)
cdi = custom.composite.diversification.indicator(obj, plot.main = F, plot.table = F)
out = rbind(colMeans(cdi, na.rm=T), out)
rownames(out)[1] = 'Composite Diversification Indicator(CDI)'
y = 100 * sapply(models, compute.turnover, data)
out = rbind(y, out)
rownames(out)[1] = 'Portfolio Turnover'
performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD,Volatility,Portfolio Turnover,Composite Diversification Indicator(CDI)', c(T,T,T,T,F,F,T))
if(control$plot.weight.transition.maps) {
layout(1:4)
for(m in names(models)) {
plotbt.transition.map(models[[m]]$weight, name=m)
legend('topright', legend = m, bty = 'n')
}
}
if(control$plot.risk.contribution.transition.maps) {
dates = index(data$prices)[obj$period.ends]
layout(1:4)
for(m in names(models)) {
plotbt.transition.map(make.xts(obj$risk.contributions[[m]], dates),
name=paste('Risk Contributions',m))
legend('topright', legend = m, bty = 'n')
}
}
dev.off()
load.packages('abind')
append=FALSE
cat(title, '\n', file=filename.csv, append=append)
write.table(out, sep=',',  row.names = , col.names = NA, file=filename.csv, append=TRUE)
cat('\n\n', file=filename.csv, append=TRUE)
if(F) {
out = abind(lapply(models, function(m) m$equity))
colnames(out) = names(models)
write.xts(make.xts(out, index(models[[1]]$equity)), filename.csv, append=TRUE)
}
return(out)
}
pie.labels.fix <- function (x, y, angles, labels, radius = 1, ...) {
par(xpd = TRUE)
xylim <- par("usr")
plotdim <- par("pin")
yradius <- radius * (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
xc <- cos(angles) * radius + x
yc <- sin(angles) * yradius + y
text(xc, yc, labels, ...)
par(xpd = FALSE)
}
plot.cluster.weights <- function(weight, clusters, main='') {
load.packages('RColorBrewer,plotrix')
clusters = sort(clusters)
weight = weight[names(clusters)]
weight.cluster = tapply(weight,clusters,sum)
counts = tapply(names(clusters),clusters,len)
ncluster = len(counts)
require(RColorBrewer)
colors = colorRampPalette(brewer.pal(iif(ncluster>9,9,ncluster),'Set1'))(ncluster)
cols = c()
for(i in 1:ncluster) cols = c(cols, col.add.alpha(colors[i], seq(200,100,length.out = counts[i])))
if(F) {
plot(-1:1 ,-1:1, main=main, type='n', yaxt = 'n', xaxt = 'n', xlab = '', ylab = '', axes = F)
bisect.angles = floating.pie(0,0,weight, col=cols, border='white', radius=0.9, cex=0.8)
pie.labels(0,0,bisect.angles,names(weight),radius=1,bg=0,border=F, srt=bisect.angles)
}
par(mar = c(2,2,2,2))
pie(weight, col=cols, border='white', radius=0.9, main=main)
require(plotrix)
bisect.angles = floating.pie(0,0,weight.cluster,radius=0.5,col=colors,border='white')
pie.labels.fix(0,0,bisect.angles,paste(round(100*weight.cluster,0),'%',sep=''),radius=0.2)
}
adaptive.shrinkage.paper.backtests <- function()
{
stats = list()
for(i in 0:8) {
temp = bt.run.data.set(i)
stats[[temp$title]] = temp$stats
}
save(stats, file='stats.Rdata')
if(F) {
load(file='stats.Rdata')
names = c("Portfolio Turnover", "Composite Diversification Indicator(CDI)", "Cagr", "Sharpe", "Volatility", "MaxDD")
custom.order=c(F,T,T,T,F,F)
names = spl('Portfolio Turnover,Sharpe,Volatility,Composite Diversification Indicator(CDI)')
custom.order = c(T,F,T,F)
out = c()
for(n in names) {
temp = sapply(stats, function(x) x[n,])
temp = apply(temp, 1, as.double)
out = rbind(out, rowMeans(apply(temp, 1, rank)))
}
rownames(out) = names
performance.barchart.helper(out, custom.order = !custom.order,
custom.col=c('MV.S_SA_A'='red', 'best.sharpe'='green'))
}
bt.summary.stats(stats)
}
adaptive.shrinkage.paper.backtests.leverage <- function()
{
temp = bt.run.data.set(0)
vol = as.double(temp$stats['Volatility',])
leverage = vol[1] / vol
temp1 = bt.run.data.set(0, leverage = leverage, title = paste(temp$title, 'leverage'))
}
cssa.average.shrinkage <- function(hist) {
n = ncol(hist)
correlation = cor(hist, use='complete.obs', method='pearson')
s0 = apply(hist, 2, sd, na.rm=T)
index = s0==0
if(sum(index) > 0) {
correlation[index,] = 0
correlation[,index] = 0
}
column.avg = (colSums(correlation) - 1) / (n - 1)
new.matrix = outer(column.avg, column.avg, '+') / 2
diag(new.matrix) = 1
new.matrix * (s0 %*% t(s0))
}
shrink.average.cssa <- function(s=NULL) { s=s; function(x, a) cov.shrink(x, cssa.average.shrinkage, s, 1)$sigma }
bt.load.data.set <- function(data.set = 1)
{
if(data.set == 0) {
data <- new.env()
getSymbols.TB(env = data, auto.assign = T, download = F)
bt.prep(data, align='remove.na', dates='1990::')
return(list(data = data, title = 'Futures Forex', tickers = data$symbolnames))
}
data.sets = list()
data.sets[['Selected ETFs']] = list(
tickers = spl('SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD'),
align='keep.all', dates='2002:08::')
data.sets[['Selected ETFs 1']] = list(
tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT'),
align='keep.all', dates='2003:04::')
data.sets[['Selected ETFs 2']] = list(
tickers = spl('VTI,IEV,EEM,EWJ,AGG,GSG,GLD,ICF'),
align='keep.all', dates='2003:10::')
data.sets[['Dow stock (Engle)']] = list(
tickers = spl('AA,AXP,BA,CAT,DD,DIS,GE,IBM,IP,JNJ,JPM,KO,MCD,MMM,MO,MRK,MSFT'),
align='keep.all', dates='1980::')
data.sets[['Nasdaq 100']] = list(
tickers = spl('ATVI,ADBE,ALTR,AMZN,AMGN,APOL,AAPL,AMAT,ADSK,ADP,BBBY,BIIB,BRCM,CHRW,CA,CELG,CEPH,CERN,CHKP,CTAS,CSCO,CTXS,CTSH,CMCSA,COST,DELL,XRAY,DISH,EBAY,EA,EXPD,ESRX,FAST,FISV,FLEX,FLIR,FWLT,GILD,HSIC,HOLX,INFY,INTC,INTU,JBHT,KLAC,LRCX,LIFE,LLTC,LOGI,MAT,MXIM,MCHP,MSFT,MYL,NTAP,NWSA,NVDA,ORLY,ORCL,PCAR,PDCO,PAYX,PCLN,QGEN,QCOM,BBRY,ROST,SNDK,SIAL,SPLS,SBUX,SRCL,SYMC,TEVA,URBN,VRSN,VRTX,VOD,XLNX,YHOO'),
align='keep.all', dates='1995::')
data.sets[['Selected ETFs 10 Major Markets']] = list(
tickers = spl('SPY,EFA,EWJ,EEM,IYR,RWX,IEF,TLT,DBC,GLD'),
align='keep.all', dates='2002:08::')
data.sets[['Sector ETFs']] = list(
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU'),
align='keep.all', dates='1998:12::')
data.sets[['MSCI Country']] = list(
tickers = spl('SPY,TUR,EIRL,THD,EWL,EWK,EWA,EWD,EWW,EWN,EWJ,EWQ,EWO,EWP,EWH,EWG,MES,EWS,EIDO,EWU,EPOL,EWM,EIS,AFK,EWI,EWT,EWC,FXI,EZA,EWY,VNM,ECH,RSX,EWZ')	,
align='keep.all', dates='2000:08::')
load.packages('quantmod')
title = names(data.sets)[data.set]
info = data.sets[[data.set]]
data <- new.env()
getSymbols(info$tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
data.clean(data, min.ratio=3)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align = info$align, dates = info$dates)
prices = coredata(data$prices)
prices[is.na(prices)] = mlag(prices)[is.na(prices)]
prices[is.na(prices)] = mlag(prices)[is.na(prices)]
data$prices[] = prices
return(list(data = data, title = title, tickers = info$tickers))
}
bt.run.data.set <- function(i = 1, dates = '::',
leverage = 1,
title = ''
)
{
data.set = bt.load.data.set(i)
data = data.set$data
if(nchar(title) == 0) title = data.set$title
obj = portfolio.allocation.helper(data$prices,
periodicity = 'months', lookback.len = 60,
min.risk.fns = list(MV=min.var.portfolio),
shrinkage.fns = list(
S=sample.shrinkage,
SA=sample.anchored.shrinkage,
D=shrink.diag(1),
CC=shrink.const.cor(1),
SI=shrink.single.index(1),
A=shrink.average.cssa(1),
AVG=function(h,a) 1/4*(
shrink.diag(1)(h,a) + shrink.const.cor(1)(h,a) +
shrink.single.index(1)(h,a) + shrink.average.cssa(1)(h,a)
),
S_SA_A=function(h,a) 1/3*(shrink.average.cssa(1)(h,a)
+ sample.shrinkage(h,a) + sample.anchored.shrinkage(h,a)),
D_S=shrink.diag(),
CC_S=shrink.const.cor(),
SI_S=shrink.single.index(),
A_S=shrink.average.cssa(),
AVG_S=function(h,a) cov.shrink(h, 1/4 *(
shrink.diag(1)(h,a) + shrink.const.cor(1)(h,a) +
shrink.single.index(1)(h,a) + shrink.average.cssa(1)(h,a)
))$sigma,
S_SA_A_S=function(h,a) cov.shrink(h, 1/3 *(
shrink.average.cssa(1)(h,a) + sample.shrinkage(h,a) + sample.anchored.shrinkage(h,a)
))$sigma
),
adjust2positive.definite = F,
custom.stats.fn = 'portfolio.allocation.custom.stats'
)
obj = bt.shrinkage.best.sharpe('S,SA,A', 252, obj, data)
models = create.strategies(obj, data, dates = dates, leverage = leverage)$models
list(title = title, stats = bt.summary.report(models, title, data, obj,
control = list(plot.weight.transition.maps = F,
plot.risk.contribution.transition.maps = F)
)
)
}
bt.shrinkage.best.sharpe <- function(methods, sharpe.lookback.len, obj, data)
{
models = create.strategies(obj, data)$models
methods = spl(methods)
methods = paste('MV',methods,sep='.')
global.data <- new.env()
for(i in methods) {
temp = models[[i]]$equity
temp[1:min(which(temp != 1))] = NA
global.data[[i]] = make.stock.xts(temp)
}
bt.prep(global.data, align='keep.all')
rets = global.data$prices / mlag(global.data$prices) - 1
sharpe = bt.apply.matrix(rets, runMean, sharpe.lookback.len) / bt.apply.matrix(rets, runSD, sharpe.lookback.len)
sharpe = ifna(sharpe,-Inf)
index.best = unlist(apply(sharpe[obj$period.ends,],1,which.max))
obj$weights$best.sharpe = obj$weights[[1]]
for(m in 1:len(methods))
obj$weights$best.sharpe[ index.best == m, ] = obj$weights[[ methods[m] ]][ index.best == m, ]
n = 1/len(methods)
obj$weights$simple.avg = obj$weights[[1]] * 0
for(m in 1:len(methods))
obj$weights$simple.avg[] = obj$weights$simple.avg[] + n * obj$weights[[ methods[m] ]]
return(obj);
}
bt.summary.report <- function(models, title, data, obj=NULL,
control = list(plot.weight.transition.maps = F,
plot.risk.contribution.transition.maps = !is.null(obj)
)
) {
if(is.null(control$plot.weight.transition.maps)) control$plot.weight.transition.maps = F
if(is.null(control$plot.risk.contribution.transition.maps)) control$plot.risk.contribution.transition.maps = obj!=NULL
filename = title
filename.pdf = paste(filename, '.pdf', sep='')
filename.csv = paste(filename, '.csv', sep='')
pdf(file = filename.pdf, width=8.5, height=11)
layout(1:2)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title)
mtext('Cumulative Performance', side = 2, line = 1)
out = plotbt.strategy.sidebyside(models, return.table=T)
cdi = custom.composite.diversification.indicator(obj, plot.main = F, plot.table = F)
out = rbind(colMeans(cdi, na.rm=T), out)
rownames(out)[1] = 'Composite Diversification Indicator(CDI)'
y = 100 * sapply(models, compute.turnover, data)
out = rbind(y, out)
rownames(out)[1] = 'Portfolio Turnover'
performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD,Volatility,Portfolio Turnover,Composite Diversification Indicator(CDI)', c(T,T,T,T,F,F,T))
if(control$plot.weight.transition.maps) {
layout(1:len(models))
for(m in names(models)) {
plotbt.transition.map(models[[m]]$weight, name=m)
legend('topright', legend = m, bty = 'n')
}
}
if(control$plot.risk.contribution.transition.maps) {
dates = index(data$prices)[obj$period.ends]
layout(1:len(models))
layout(1:4)
for(m in names(models)) {
plotbt.transition.map(make.xts(obj$risk.contributions[[m]], dates),
name=paste('Risk Contributions',m))
legend('topright', legend = m, bty = 'n')
}
}
dev.off()
load.packages('abind')
append=FALSE
cat(title, '\n', file=filename.csv, append=append)
write.table(out, sep=',',  row.names = , col.names = NA, file=filename.csv, append=TRUE)
cat('\n\n', file=filename.csv, append=TRUE)
if(F) {
out = abind(lapply(models, function(m) m$equity))
colnames(out) = names(models)
write.xts(make.xts(out, index(models[[1]]$equity)), filename.csv, append=TRUE)
}
return(out)
}
bt.summary.stats <- function(stats)
{
names = spl('Portfolio Turnover,Sharpe,Volatility,Composite Diversification Indicator(CDI)')
custom.order = c(T,F,T,F)
out = stats[[1]][names,]
for(i in 1:len(names)) {
temp = apply(sapply(stats, function(x) x[names[i],]), 1, function(x) mean(as.double(x)))
temp = pnorm(temp, mean(temp), sd(temp))
if(custom.order[i]) temp = 1 - temp
out[i,] = temp
}
score = apply(out[1:3,], 2, function(x) mean(as.double(x)))
all.out1 = rbind(out, score)
rownames(all.out1)[5] = 'Composite Standardized Score (higher is better)'
out = stats[[1]][names,]
for(i in 1:len(names)) {
temp = apply(sapply(stats, function(x) x[names[i],]), 2, function(x) rank(iif(custom.order[i],1,-1)*as.double(x)))
out[i,] = rowMeans(temp)
}
score = apply(out[1:3,], 2, function(x) mean(as.double(x)))
all.out2 = rbind(out, score)
rownames(all.out2)[5] = 'Composite Rank Score (lower is better)'
write.table(rbind(all.out1, '', all.out2), sep=',',  row.names = , col.names = NA, file='summary.csv')
out = read.csv('summary.csv', stringsAsFactors=F)
temp = as.matrix(out[,-1,drop=F])
colnames(temp) = colnames(out)[-1]
rownames(temp) = out[,1]
pdf(file = 'summary.pdf', width=8.5, height=11)
performance.barchart.helper(temp, 'Composite Standardized Score (higher is better),Composite Rank Score (lower is better)', c(T,F), nc.plot=1,
custom.col=c('MV.S_SA_A'='red', 'best.sharpe'='green'))
dev.off()
png(filename = 'plot2.png', width = 600, height = 400, units = 'px', pointsize = 12, bg = 'white')
performance.barchart.helper(temp, 'Composite Rank Score (lower is better)', c(F), nc.plot=1,
custom.col=c('MV.S_SA_A'='red', 'best.sharpe'='green'))
dev.off()
}
tableColor <- function(data, header.col='LightGray', row.col='yellow', negative.col='red', ...) {
if (is.null(data))
return("")
add.to.row= NULL
if( nrow(data) > 1) {
temp = as.list(seq(1,nrow(data)-1,by=2))
add.to.row=list(temp, rep("XXX", len(temp)))
}
temp = renderTable(data, add.to.row=add.to.row, ...)
temp = temp()
if(!is.na(negative.col) && !is.null(negative.col))
temp = gsub("(-\\d*\\.\\d*)", paste("<font color=", negative.col, ">\\1</font>"), temp)
if(!is.na(row.col) && !is.null(row.col))
temp = gsub("XXX<TR>", paste("<TR bgcolor=", row.col, ">"), temp)
temp = gsub("XXX", "", temp)
if(!is.na(header.col) && !is.null(header.col))
temp = gsub("<TR>\\s*<TH>\\s*</TH>",paste("<TR bgcolor=", header.col, "><TH></TH>"), temp)
return(temp)
}
createNonReactiveTextInput <- function(id, label, value, button.label='') {
if(button.label != '')
list(
tagList(tags$label(label),
tags$input(id = id, type = "text", value = value, style="display:none;"),
tags$input(id = paste(id,"Temp",sep=''), type = "text", value = value, style="display:inline;",
onkeypress = paste("{if (event.keyCode==13) $('#", id, "TempChange').click()}",sep=''))
),
div(tags$button(id = paste(id, "TempChange",sep=''), type = "button", class = "btn btn-primary",
onclick = paste("$('#",id,"').val($('#", id, "Temp').val()).change();",sep=''),
button.label))
)
else
list(
tagList(tags$label(label),
tags$input(id = id, type = "text", value = value, style="display:none;"),
tags$input(id = paste(id,"Temp",sep=''), type = "text", value = value, style="display:inline;",
onkeypress = paste("{if (event.keyCode==13) $('#",id,"').val($('#", id, "Temp').val()).change()}",sep=''))
)
)
}
createNonReactiveTextInputCustom <- function(id, label, tag.label = 'input', button.label='', enableEnter=TRUE,
opts) {
onkeypress = ''
if(button.label != '') {
if(enableEnter)
onkeypress = paste("{if (event.keyCode==13) $('#", id, "TempChange').click()}",sep='')
list(
tagList(tags$label(label),
tag(tag.label, c(id = id, style="display:none;", opts)),
tag(tag.label, c(id = paste(id,"Temp",sep=''), style="display:inline;",
onkeypress = onkeypress,
opts))
),
div(tags$button(id = paste(id, "TempChange",sep=''), type = "button", class = "btn btn-primary",
onclick = paste("$('#",id,"').val($('#", id, "Temp').val()).change();",sep=''),
button.label))
)
} else {
if(enableEnter)
onkeypress = paste("{if (event.keyCode==13) $('#",id,"').val($('#", id, "Temp').val()).change()}",sep='')
list(
tagList(tags$label(label),
tag(tag.label, c(id = id, style="display:none;", opts)),
tag(tag.label, c(id = paste(id,"Temp",sep=''), style="display:inline;",
onkeypress = onkeypress,
opts))
)
)
}
}
strategy.load.historical.data <- function
(
tickers = spl('DIA,SPY'),
dates = '1900::',
align = 'keep.all',
fill.gaps = T,
adjust = T,
current = F
)
{
load.packages('quantmod')
tickers = spl(tickers)
data <- new.env()
for(i in 1:len(tickers)) {
ticker0 = gsub('\\^', '', tickers[i])
temp = try(getSymbols(tickers[i], src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T), TRUE)
if(inherits(temp, 'try-error'))
cat(i, 'out of', len(tickers), 'Error Reading', tickers[i], '\n', sep='\t')
else if(is.null(data[[ tickers[i] ]]))
if( is.null(data[[ ticker0 ]]) )
cat(i, 'out of', len(tickers), 'Error Reading', tickers[i], '\n', sep='\t')
else
cat(i, 'out of', len(tickers), 'Reading', ticker0, format(range(index(data[[ ticker0 ]])), '%d-%b-%Y'), '\n', sep='\t')
else
cat(i, 'out of', len(tickers), 'Reading', tickers[i], format(range(index(data[[ tickers[i] ]])), '%d-%b-%Y'), '\n', sep='\t')
}
if(adjust) for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
if(current) {
quotes = getQuote(tickers)
for(i in ls(data))
if( last(index(data[[i]])) < as.Date(quotes[i, 'Trade Time']) ) {
data[[i]] = rbind( data[[i]], make.xts(quotes[i, spl('Open,High,Low,Last,Volume,Last')],
as.Date(quotes[i, 'Trade Time'])))
}
}
bt.prep(data, align=align, dates=dates, fill.gaps=fill.gaps)
return(data)
}
performance.barchart.helper <- function(out,
names = rownames(out),
custom.order=rep(T,len(spl(names))),
nplots.page = len(spl(names)),
nc.plot = 2,
sort.performance = T,
custom.col = NULL
)
{
layout(mat=matrix(1:(nplots.page + nplots.page %% nc.plot), nc=nc.plot, byrow=FALSE))
par(mar=c(4, 3, 2, 2))
col = spl('ivory2,red')
names = spl(names)
names(custom.order) = names
for(i in names) {
y = as.double(out[i,])
index = iif(sort.performance, order(y, decreasing = custom.order[i]), 1:len(y))
cols = iif(y[index] > 0, col[1], col[2])
names(cols) = colnames(out)[index]
if(!is.null(custom.col))
cols[names(custom.col)] = custom.col
x = barplot(y[index], names.arg = '',
col=cols,
main=i,
border = 'darkgray',las=2)
grid(NA,NULL)
abline(h=0, col='black')
if(y[1] > 0)
text(x, 0 * x, colnames(out)[index], adj=c(-0.1,1), srt=90, xpd = TRUE)
else
text(x, 0 * x, colnames(out)[index], adj=c(1.1,1), srt=90, xpd = TRUE)
if(sort.performance) {
mtext('worst', side = 1,line = 0, outer = F, adj = 1, font = 1, cex = 1)
mtext('best', side = 1,line = 0, outer = F, adj = 0, font = 1, cex = 1)
}
}
}
barplot.with.labels <- function(data, main, plotX = TRUE, label=c('level','name','both')) {
par(mar=c( iif(plotX, 6, 2), 4, 2, 2))
x = barplot(100 * data, main = main, las = 2, names.arg = iif(plotX, names(data), ''))
if(label[1] == 'level') text(x, 100 * data, round(100 * data,1), adj=c(0.5,1), xpd = TRUE)
if(label[1] == 'name') text(x, 0 * data, names(data), adj=c(-0.1,1), srt=90, xpd = TRUE)
if(label[1] == 'both')
text(x, 0 * data, paste(round(100 * data), '% ', names(data), sep=''), adj=c(-0.1,1), srt=90, xpd = TRUE)
}
strategy.performance.snapshoot <- function(models, one.page = F, title = NULL, data = NULL,
control = list(main = T, comparison = T, transition = T, monthly = T),
sort.performance = T
) {
for(n in spl('main,comparison,transition,monthly'))
if(is.null(control[[n]])) control[[n]] = F
out = NULL
if(control$main) {
layout(1:2)
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = title)
mtext('Cumulative Performance', side = 2, line = 1)
out = plotbt.strategy.sidebyside(models, return.table=T)
}
if(one.page) return()
if(control$comparison) {
if(is.null(out))
out = plotbt.strategy.sidebyside(models, return.table=T, make.plot = F)
if(!is.null(data)) {
y = 100 * sapply(models, compute.turnover, data)
out = rbind(y, out)
rownames(out)[1] = 'Turnover'
performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD,Volatility,Turnover', c(T,T,T,T,F,F), sort.performance = sort.performance)
} else
performance.barchart.helper(out, 'Sharpe,Cagr,DVR,MaxDD', c(T,T,T,T), sort.performance = sort.performance)
}
if(control$transition) {
layout(1:min(4,len(models)))
for(m in names(models)) {
plotbt.transition.map(models[[m]]$weight, name=m)
legend('topright', legend = m, bty = 'n')
}
}
if(control$monthly) {
layout(1:min(4,len(models)))
for(n in names(models))
plotbt.monthly.table(models[[n]]$equity, smain=n)
}
}
meom.strategy <- function
(
tickers = spl('DIA,SPY'),
dates = '1900::'
)
{
data = strategy.load.historical.data(tickers, dates, fill.gaps=T)
prices = data$prices
n = ncol(prices)
nperiods = nrow(prices)
month.ends = endpoints(prices, 'months')
month.ends = month.ends[month.ends > 0]
month.ends2 = iif(month.ends + 2 > nperiods, nperiods, month.ends + 2)
month.ends1 = iif(month.ends + 1 > nperiods, nperiods, month.ends + 1)
models = list()
data$weight[] = NA
data$weight[] = ntop(prices, n)
models$equal.weight = bt.run.share(data, clean.signal=F)
data$weight[] = NA
data$weight[month.ends,] = ntop(prices, n)[month.ends,]
data$weight[month.ends2,] = 0
models$meom.equal.weight = bt.run.share(data, clean.signal=F)
buy.rule = prices > bt.apply.matrix(prices, function(x) { WMA(x, 89) } )
buy.rule = ifna(buy.rule, F)
ret2 = ifna(prices / mlag(prices, 2), 0)
position.score = bt.apply.matrix(ret2, SMA, 5) * bt.apply.matrix(ret2, SMA, 40)
position.score[!buy.rule] = NA
data$weight[] = NA;
data$weight[month.ends,] = ntop(position.score[month.ends,], 2)
data$weight[month.ends2,] = 0
models$meom.top2.rank1 = bt.run.share(data, clean.signal=F, trade.summary=T)
position.score = bt.apply.matrix(ret2, SMA, 5) * mlag( bt.apply.matrix(ret2, SMA, 10), 5)
position.score[!buy.rule] = NA
data$weight[] = NA
data$weight[month.ends,] = ntop(position.score[month.ends,], 2)
data$weight[month.ends2,] = 0
models$meom.top2.rank2 = bt.run.share(data, clean.signal=F, trade.summary=T)
data$weight[] = NA
data$weight[month.ends,] = ntop(position.score[month.ends,], 2)
data$weight[month.ends2,] = 0
popen = bt.apply(data, Op)
data$weight[month.ends1,] = iif((prices > popen)[month.ends1,], 0, NA)
models$meom.top2.rank2.hold12 = bt.run.share(data, clean.signal=F, trade.summary=T)
return(rev(models))
}
meom.strategy.test <- function()
{
models = meom.strategy(
tickers = 'DIA,EEM,EFA,EWH,EWJ,EWT,EWZ,FXI,GLD,GSG,IEF,ILF,IWM,IYR,QQQ,SPY,VNQ,XLB,XLE,XLF,XLI,XLP,XLU,XLV,XLY,XLK',
dates='1995::'
)
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part1(models)
dev.off()
png(filename = 'plot2.png', width = 1200, height = 800, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part2(models)
dev.off()
png(filename = 'plot3.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
plotbt.custom.report.part3(models$meom.top2.rank2, trade.summary=T)
dev.off()
}
timing.strategy <- function
(
tickers = spl('DIA,SPY,SHY'),
dates = '1900::',
periodicity = 'months',
ma.len = 200,
cash = 'SHY'
)
{
data = strategy.load.historical.data(tickers, dates)
prices = data$prices
n = ncol(prices)
nperiods = nrow(prices)
period.ends = endpoints(data$prices, periodicity)
period.ends = period.ends[period.ends > 0]
models = list()
position.score = prices
position.score$SHY = NA
weight = ntop(position.score[period.ends,], n)
data$weight[] = NA
data$weight[period.ends,] = weight
models$equal.weight = bt.run.share(data, clean.signal=F)
sma = bt.apply.matrix(prices, SMA, ma.len)
buy.rule = prices > sma
buy.rule = ifna(buy.rule, F)
weight = ntop(position.score[period.ends,], n)
weight[!buy.rule[period.ends,]] = 0
weight$SHY = 1 - rowSums(weight)
data$weight[] = NA
data$weight[period.ends,]  = weight
models$timing = bt.run.share(data, clean.signal=F, trade.summary=T)
return(rev(models))
}
timing.strategy.test <- function()
{
models = timing.strategy(
tickers = 'VTI,EFA,IEF,ICF,DBC,SHY',
dates='2002:08::'
)
plotbt.custom.report.part1(models)
plotbt.custom.report.part2(models)
}
rotation.strategy <- function
(
tickers = spl('DIA,SPY,SHY'),
dates = '1900::',
periodicity = 'months',
top.n = 2,
keep.n = 6
)
{
data = strategy.load.historical.data(tickers, dates)
prices = data$prices
n = ncol(prices)
nperiods = nrow(prices)
period.ends = endpoints(data$prices, periodicity)
period.ends = period.ends[period.ends > 0]
models = list()
data$weight[] = NA
data$weight[period.ends,] = ntop(prices[period.ends,], n)
models$equal.weight = bt.run.share(data, clean.signal=F)
position.score = prices / mlag(prices, 126)
data$weight[] = NA
data$weight[period.ends,] = ntop(position.score[period.ends,], top.n)
models$top = bt.run.share(data, clean.signal=T, trade.summary=T)
data$weight[] = NA
data$weight[period.ends,] = ntop.keep(position.score[period.ends,], top.n, keep.n)
models$top.keep = bt.run.share(data, clean.signal=T, trade.summary=T)
return(rev(models))
}
rotation.strategy.test <- function()
{
models = rotation.strategy(
tickers = 'XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ',
dates='1970::'
)
plotbt.custom.report.part1(models)
plotbt.custom.report.part2(models)
}
create.ia <- function(hist.returns, index=1:ncol(hist.returns), nperiod=nrow(hist.returns))
{
ia = list()
ia$hist.returns = hist.returns
ia$nperiod = nperiod
ia$index = index
ia$n = ncol(ia$hist.returns)
ia$symbols = colnames(ia$hist.returns)
ia$risk = apply(ia$hist.returns, 2, sd, na.rm = T)
ia$correlation = cor(ia$hist.returns, use='complete.obs',method='pearson')
ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
ia$expected.return = apply(ia$hist.returns, 2, mean, na.rm = T)
return(ia)
}
update.ia <- function(ia, name, cov.shrink)
{
if(name != 'sample') {
ia$cov = cov.shrink
s0 = 1 / sqrt(diag(ia$cov))
ia$correlation = ia$cov * (s0 %*% t(s0))
}
ia
}
create.ia.subset <- function(ia.base, index=1:ia.base$n)
{
ia = list()
ia$hist.returns = ia.base$hist.returns[,index,drop=F]
ia$nperiod = ia.base$nperiod
ia$index = ia.base$index[index]
ia$n = ncol(ia$hist.returns)
ia$symbols = colnames(ia$hist.returns)
ia$risk = ia.base$risk[index]
ia$correlation = ia.base$correlation[index,index,drop=F]
ia$cov = ia.base$cov[index,index,drop=F]
ia$expected.return = ia.base$expected.return[index]
return(ia)
}
create.ia.period <- function
(
prices,
periodicity = 'weeks',
period.ends = endpoints(prices, periodicity)
)
{
prices = prices[period.ends,,drop=F]
ret = coredata(prices / mlag(prices) - 1)
function(hist.returns, index, nperiod)
{
i = nperiod
create.ia(ret[which(
period.ends <= i &
period.ends >= (i - nrow(hist.returns) + 1)
), index, drop=F],
index,
nperiod)
}
}
ia.build.hist <- function(hist.returns, lookbacks, n.lag)
{
nperiods = nrow(hist.returns)
temp = c()
for (n.lookback in lookbacks)
temp = rbind(temp, hist.returns[(nperiods - n.lookback - n.lag + 1):(nperiods - n.lag), , drop=F])
return(temp)
}
momentum.averaged <- function(prices,
lookbacks = c(20,60,120,250) ,
n.lag = 3
) {
momentum = 0 * prices
for (n.lookback in lookbacks) {
part.mom = mlag(prices, n.lag) / mlag(prices, n.lookback + n.lag) - 1
momentum = momentum + 252 / n.lookback * part.mom
}
momentum / len(lookbacks)
}
create.ia.averaged <- function(lookbacks, n.lag)
{
lookbacks = lookbacks
n.lag = n.lag
function(hist.returns, index, nperiod)
{
temp = ia.build.hist(hist.returns, lookbacks, n.lag)
create.ia(temp, index, nperiod)
}
}
create.basic.constraints <- function(
n,
const.lb = 0,
const.ub = 1,
const.sum = 1
)
{
if(len(const.lb) == 1) const.lb = rep(const.lb, n)
if(len(const.ub) == 1) const.ub = rep(const.ub, n)
constraints = new.constraints(n, lb = const.lb, ub = const.ub)
constraints = add.constraints(diag(n), type='>=', b=const.lb, constraints)
constraints = add.constraints(diag(n), type='<=', b=const.ub, constraints)
if(!is.na(const.sum))
constraints = add.constraints(rep(1, n), type = '=', b=const.sum, constraints)
return(constraints)
}
static.weight.portfolio <- function(static.allocation)
{
static.allocation = static.allocation
function
(
ia,
constraints
)
{
return(static.allocation[ia$index])
}
}
equal.weight.portfolio <- function
(
ia,
constraints
)
{
rep(1/ia$n, ia$n)
}
get.risky.asset.index <- function(ia) {
if(is.null(ia$risk)) ia$risk = sqrt(diag(ia$cov))
(ia$risk > 0) & !is.na(ia$risk)
}
set.risky.asset <- function(x, risk.index) {
out = rep(0, len(risk.index))
out[risk.index] = x
return(out)
}
risk.parity.portfolio.basic <- function
(
ia,
constraints
)
{
if(is.null(ia$risk)) ia$risk = sqrt(diag(ia$cov))
risk.index = get.risky.asset.index(ia)
x = 1 / ia$risk[risk.index]
set.risky.asset(x / sum(x), risk.index)
}
risk.parity.portfolio <- function(
risk.fn = function(ia) ia$risk
)
{
algo.map = list(
'cvar' = function(ia) -apply(ia$hist.returns, 2, compute.cvar),
'md' = function(ia) -apply(apply(1+ia$hist.returns, 2, cumprod), 2, compute.max.drawdown),
'cdar' = function(ia) -apply(apply(1+ia$hist.returns, 2, cumprod), 2, compute.cdar)
)
fn = try( match.fun(risk.fn) , silent = TRUE)
if(class(fn)[1] == 'try-error' && is.character(risk.fn) && any(names(algo.map) == tolower(risk.fn)))
fn = algo.map[[ tolower(risk.fn) ]]
if(class(fn)[1] == 'try-error') stop(paste('risk.parity.portfolio', fn))
function
(
ia,
constraints
)
{
if(is.null(ia$risk)) ia$risk = sqrt(diag(ia$cov))
risk.index = get.risky.asset.index(ia)
x = 1 / fn(ia)[risk.index]
set.risky.asset(x / sum(x), risk.index)
}
}
min.var.portfolio <- function
(
ia,
constraints,
cov.matrix = ia$cov,
dvec = rep(0, ia$n)
)
{
risk.index = get.risky.asset.index(ia)
Dmat = cov.matrix[risk.index, risk.index]
sol = try(solve.QP(Dmat=Dmat,
dvec=dvec[risk.index],
Amat=constraints$A[risk.index,,drop=F],
bvec=constraints$b,
meq=constraints$meq), silent = TRUE)
if(inherits(sol, 'try-error'))
sol = try(solve.QP(Dmat=make.positive.definite(Dmat, 0.000000001),
dvec=dvec[risk.index],
Amat=constraints$A[risk.index,,drop=F],
bvec=constraints$b,
meq=constraints$meq), silent = TRUE)
if(inherits(sol, 'try-error')) {
gia <<- ia
stop(sol)
}
set.risky.asset(sol$solution, risk.index)
}
max.div.portfolio <- function
(
ia,
constraints
)
{
risk.index = get.risky.asset.index(ia)
x = min.var.portfolio(ia, constraints, ia$correlation)
x = x[risk.index] / ia$risk[risk.index]
set.risky.asset(x / sum(x), risk.index)
}
equal.risk.contribution.portfolio <- function
(
ia,
constraints
)
{
risk.index = get.risky.asset.index(ia)
cov = ia$cov[risk.index, risk.index]
fn <- function(x){
if (sum(x) == 0) x = x + 1e-2
x  = x / sum(x)
risk.contribution = (x * (cov %*% x))
var(as.double(risk.contribution))
}
x0 = 1/sqrt(diag(cov))
x0 = x0 / sum(x0)
if(!is.null(constraints$x0))
if(all(!is.na(constraints$x0)))
if( sum(constraints$x0) == 1 )
if( fn(x0) > fn(constraints$x0[risk.index]) )
x0 = constraints$x0[risk.index]
load.packages('nloptr')
x = nloptr( x0=x0,eval_f=fn,lb = constraints$lb[risk.index],ub = constraints$ub[risk.index],
opts = list('algorithm'='NLOPT_LN_BOBYQA','xtol_rel'=1.0e-10))
set.risky.asset(x$solution / sum(x$solution), risk.index)
}
ef.portfolio <- function(percent = 0.5)
{
percent = as.double(percent[1])
if(percent > 1) percent = percent / 100
function
(
ia,
constraints
)
{
max.w = max.return.portfolio(ia, constraints)
min.w = min.var.portfolio(ia, constraints)
max.r = sum(max.w * ia$expected.return)
min.r = sum(min.w * ia$expected.return)
target = (max.r - min.r) * percent + min.r
constraints = add.constraints(ia$expected.return,
type='>=', b=target, constraints)
return(min.var.portfolio(ia, constraints))
}
}
min.te.portfolio.test <- function()
{
min.te.portfolio <- function(ia, constraints, index.weight)
min.var.portfolio(ia, constraints, dvec = index.weight %*% ia$cov)
load.packages('quantmod')
tickers = spl('SPY,TLT,XLP')
data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, set.symbolnames = T, auto.assign = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', fill.gaps = T)
prices = data$prices
n = ncol(prices)
nperiods = nrow(prices)
ret = prices / mlag(prices) - 1
lookback.len = 120
hist = ret[(nperiods - lookback.len) : nperiods, , drop=F]
ia = create.ia(hist, nperiod=nperiods)
index.weight = coredata(last(prices))
index.weight = index.weight / sum(index.weight)
load.packages('quadprog,corpcor,kernlab')
constraints = create.basic.constraints(n, 0, 1, 1)
x = min.te.portfolio(ia, constraints, index.weight)
x - index.weight
252 * portfolio.return(x, ia)
constraints = create.basic.constraints(n, 0, 0.4, 1)
x = min.te.portfolio(ia, constraints, index.weight)
x - index.weight
252 * portfolio.return(x, ia)
constraints = create.basic.constraints(n, 0, 1, 1)
constraints = add.constraints(ia$expected.return, type = '>=', b=0.95 * max(ia$expected.return), constraints)
x = min.te.portfolio(ia, constraints, index.weight)
x - index.weight
252 * portfolio.return(x, ia)
}
max.sharpe.portfolio.helper <- function
(
ia,
const = spl('long-only,long-short,market-neutral'),
const.sum = 1,
rf = 0
)
{
const = const[1]
n = ia$n
constraints = new.constraints(n)
if( const == 'long-only' )
constraints = add.constraints(diag(n), type='>=', b=0, constraints)
excess.return = ia$expected.return - rf
if( all(excess.return <= 0) )
constraints = add.constraints(excess.return, -1 , type = '=', constraints)
else
constraints = add.constraints(excess.return, 1 , type = '=', constraints)
if( const == 'market-neutral' )
constraints = add.constraints(rep(1,n), 0 , type = '=', constraints)
weight = min.var.portfolio(ia,constraints)
if( const == 'market-neutral' )
return(2*const.sum * weight / sum(abs(weight)))
else
return(const.sum * weight / sum(weight))
}
max.sharpe.portfolio <- function
(
const = spl('long-only,long-short,market-neutral'),
const.sum = 1
)
{
const = const[1]
const.sum = const.sum
function(ia, constraints) { max.sharpe.portfolio.helper(ia, const, const.sum) }
}
max.sharpe.portfolio.test <- function()
{
ia = aa.test.create.ia()
n = ia$n
constraints = create.basic.constraints(n, 0, 1, 1)
ef = portopt(ia, constraints, 50, 'Efficient Frontier')
png(filename = 'plot1.png', width = 500, height = 500, units = 'px', pointsize = 12, bg = 'white')
plot.ef(ia, list(ef), transition.map=F)
max(portfolio.return(ef$weight,ia) /  portfolio.risk(ef$weight,ia))
weight = min.var.portfolio(ia,constraints)
points(100 * portfolio.risk(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='red')
portfolio.return(weight,ia) /  portfolio.risk(weight,ia)
weight = max.sharpe.portfolio()(ia,constraints)
points(100 * portfolio.risk(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='orange')
portfolio.return(weight,ia) /  portfolio.risk(weight,ia)
plota.legend('Minimum Variance,Maximum Sharpe','red,orange', x='topright')
dev.off()
weight = max.sharpe.portfolio('long-only')(ia,constraints)
round(weight,2)
round(c(sum(weight[weight<0]), sum(weight[weight>0])),2)
weight = max.sharpe.portfolio('long-short')(ia,constraints)
round(weight,2)
round(c(sum(weight[weight<0]), sum(weight[weight>0])),2)
weight = max.sharpe.portfolio('long-short', -1)(ia,constraints)
round(weight,2)
round(c(sum(weight[weight<0]), sum(weight[weight>0])),2)
weight = max.sharpe.portfolio('market-neutral')(ia,constraints)
round(weight,2)
round(c(sum(weight[weight<0]), sum(weight[weight>0])),2)
}
find.portfolio.given.risk.test <- function()
{
ia = aa.test.create.ia()
n = ia$n
constraints = create.basic.constraints(n, 0, 1, 1)
load.packages('quadprog,corpcor,lpSolve,kernlab')
weight = max.return.portfolio(ia, constraints)
weight
cat(round(100*c(portfolio.return(weight,ia), portfolio.risk(weight,ia), portfolio.return(weight,ia) /  portfolio.risk(weight,ia)),2), '\n')
weight = min.var.portfolio(ia,constraints)
weight
cat(round(100*c(portfolio.return(weight,ia), portfolio.risk(weight,ia), portfolio.return(weight,ia) /  portfolio.risk(weight,ia)),2), '\n')
weight = target.return.portfolio.helper(ia,constraints, 12/100)
weight
cat(round(100*c(portfolio.return(weight,ia), portfolio.risk(weight,ia), portfolio.return(weight,ia) /  portfolio.risk(weight,ia)),2), '\n')
weight = target.risk.portfolio.helper(ia,constraints, 10/100, silent=F)
weight
cat(round(100*c(portfolio.return(weight,ia), portfolio.risk(weight,ia), portfolio.return(weight,ia) /  portfolio.risk(weight,ia)),2), '\n')
}
min.corr.excel <- function(power.function = 1, final.scale = 'risk')
{
power.function = as.numeric(power.function)
final.scale = final.scale
function
(
ia,
constraints
)
{
risk.index = get.risky.asset.index(ia)
n = sum(risk.index)
x = min.corr.special.case(ia$risk[risk.index])
if(!is.null(x)) return( set.risky.asset(x / sum(x), risk.index) )
cor.matrix = ia$correlation[risk.index, risk.index]
upper.index = upper.tri(cor.matrix)
cor.m = cor.matrix[upper.index]
cor.mu = mean(cor.m)
cor.sd = sd(cor.m)
avg.corr.contribution = (rowSums(cor.matrix) - 1) / (n - 1)
avg.rank = rank(avg.corr.contribution)
avg.rank = avg.rank ^ power.function
rr.adjustment = avg.rank / sum(avg.rank)
norm.dist.m = 0 * cor.matrix
diag(norm.dist.m) = 0
norm.dist.m[upper.index] = 1-pnorm(cor.m, cor.mu, cor.sd)
norm.dist.m = (norm.dist.m + t(norm.dist.m))
rr.norm.dist.m = rep.col(rr.adjustment,n) * norm.dist.m
rr.norm.dist = colSums(rr.norm.dist.m)
rr.weighted = rr.norm.dist / sum(rr.norm.dist)
vol.scale = ia$risk[risk.index]
if(final.scale == 'vol') vol.scale = diag(ia$cov[risk.index, risk.index])
inverse.volatility.weight = (1 / vol.scale) / sum(1 / vol.scale)
x = rr.weighted * inverse.volatility.weight / sum(rr.weighted * inverse.volatility.weight)
set.risky.asset(x / sum(x), risk.index)
}
}
min.corr.excel.portfolio.test <- function()
{
ia = list()
ia$n = 3
ia$risk = c(14, 18, 22) / 100;
ia$correlation = matrix(
c(1, 0.90, 0.85,
0.90, 1, 0.70,
0.85, 0.70, 1), nr=3, byrow=T)
ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
constraints = create.basic.constraints(ia$n, 0, 1, 1)
min.corr.excel.portfolio(ia,constraints)
}
min.corr.special.case <- function(risk) {
n = len(risk)
if(n == 1) 1
else if(n == 2) 1/risk
else NULL
}
min.corr <- function(power.function = 1)
{
power.function = as.numeric(power.function)
function
(
ia,
constraints
)
{
risk.index = get.risky.asset.index(ia)
x = min.corr.special.case(ia$risk[risk.index])
if(!is.null(x)) return( set.risky.asset(x / sum(x), risk.index) )
cor.matrix = ia$correlation[risk.index, risk.index]
upper.index = upper.tri(cor.matrix)
cor.m = cor.matrix[upper.index]
cor.mu = mean(cor.m)
cor.sd = sd(cor.m)
norm.dist.m = 0 * cor.matrix
diag(norm.dist.m) = NA
norm.dist.m[upper.index] = 1-pnorm(cor.m, cor.mu, cor.sd)
norm.dist.m = (norm.dist.m + t(norm.dist.m))
norm.dist.avg = rowMeans(norm.dist.m, na.rm=T)
norm.dist.rank = rank(-norm.dist.avg)
norm.dist.rank = norm.dist.rank ^ power.function
norm.dist.weight = norm.dist.rank / sum(norm.dist.rank)
diag(norm.dist.m) = 0
weighted.norm.dist.average = norm.dist.weight %*% norm.dist.m
final.weight = weighted.norm.dist.average / sum(weighted.norm.dist.average)
x = final.weight / ia$risk[risk.index]
set.risky.asset(x / sum(x), risk.index)
}
}
min.corr2 <- function(power.function = 1)
{
power.function = as.numeric(power.function)
function
(
ia,
constraints
)
{
risk.index = get.risky.asset.index(ia)
x = min.corr.special.case(ia$risk[risk.index])
if(!is.null(x)) return( set.risky.asset(x / sum(x), risk.index) )
cor.matrix = ia$correlation[risk.index, risk.index]
cor.m = cor.matrix
diag(cor.m) = 0
avg = rowMeans(cor.m)
cor.mu = mean(avg)
cor.sd = sd(avg)
norm.dist.avg = 1-pnorm(avg, cor.mu, cor.sd)
norm.dist.rank = rank(-norm.dist.avg)
norm.dist.rank = norm.dist.rank ^ power.function
norm.dist.weight = norm.dist.rank / sum(norm.dist.rank)
weighted.norm.dist.average = norm.dist.weight %*% (1-cor.m)
final.weight = weighted.norm.dist.average / sum(weighted.norm.dist.average)
x = final.weight / ia$risk[risk.index]
set.risky.asset(x / sum(x), risk.index)
}
}
min.var2111 <- function(power.function = 1)
{
power.function = as.numeric(power.function)
function
(
ia,
constraints
)
{
risk.index = get.risky.asset.index(ia)
x = min.corr.special.case(ia$risk[risk.index])
if(!is.null(x)) return( set.risky.asset(x / sum(x), risk.index) )
data.matrix = ia$cov[risk.index, risk.index]
avg = rowMeans(data.matrix)
data.mu = mean(avg)
data.sd = sd(avg)
norm.dist.avg = 1 - pnorm(avg, data.mu, data.sd)
norm.dist.rank = rank(-norm.dist.avg)
norm.dist.rank = norm.dist.rank ^ power.function
norm.dist.weight = norm.dist.rank / sum(norm.dist.rank)
weighted.norm.dist.average = norm.dist.weight %*% (max(data.matrix) - data.matrix)
final.weight = weighted.norm.dist.average / sum(weighted.norm.dist.average)
x = final.weight / ia$risk[risk.index]
set.risky.asset(x / sum(x), risk.index)
}
}
min.var.excel <- function(power.function = 1)
{
power.function = as.numeric(power.function)
function
(
ia,
constraints
)
{
risk.index = get.risky.asset.index(ia)
x = min.corr.special.case(ia$risk[risk.index])
if(!is.null(x)) return( set.risky.asset(x / sum(x), risk.index) )
data.matrix = ia$cov[risk.index, risk.index]
avg = rowMeans(data.matrix)
data.mu = mean(avg)
data.sd = sd(avg)
norm.dist.avg = 1 - pnorm(avg, data.mu, data.sd)
final.weight = norm.dist.avg / sum(norm.dist.avg)
x = final.weight / diag(data.matrix)
set.risky.asset(x / sum(x), risk.index)
}
}
min.var.excel.portfolio.test <- function()
{
tickers = spl('DBC,EEM,EWJ,GLD')
data = '
-0.004903678  0.005815362  0.006696429  -0.010055275
0.000703977  0.01035895  0.014412417  0.006355806
0.000351741  0.007868383  0.005464481  0.000708299
-0.000351617  -0.002838893  0.006521739  -0.004423735
-0.015124868  -0.015421115  -0.012958963  -0.010782629
-0.004642857  0.009638554  0.014223195  0.003653351
'
ia = create.ia(matrix(scan(text=data), nc = len(tickers)))
data = '
0.000090  0.000044  0.000028  0.000034
0.000044  0.000084  0.000068  0.000039
0.000028  0.000068  0.000101  0.000036
0.000034  0.000039  0.000036  0.000039
'
ia$cov = matrix(scan(text=data), nc = ia$n)
ia$risk = sqrt(diag(ia$cov))
constraints = create.basic.constraints(ia$n, 0, 1, 1)
min.var.excel.portfolio(ia,constraints)
}
min.var2.portfolio <- function(ia,constraints) { min.corr.excel(final.scale = 'vol')(ia,constraints) }
min.var2 <- function(power.function = 1)
{
power.function = as.numeric(power.function)
function(ia,constraints) min.corr.excel(power.function, final.scale = 'vol')(ia,constraints)
}
min.var.excel.portfolio <- function(ia,constraints) { min.var.excel()(ia,constraints) }
min.corr.excel.portfolio <- function(ia,constraints) { min.corr.excel()(ia,constraints) }
min.corr.portfolio <- function(ia,constraints) { min.corr()(ia,constraints) }
min.corr2.portfolio <- function(ia,constraints) { min.corr2()(ia,constraints) }
min.cvar <- function(alpha = 0.95) {
alpha = alpha
function(ia,constraints) {
ia$parameters.alpha = as.numeric(alpha)
min.cvar.portfolio(ia,constraints)
}
}
min.cdar <- function(alpha = 0.95) {
alpha = alpha
function(ia,constraints) {
ia$parameters.alpha = as.numeric(alpha)
min.cdar.portfolio(ia,constraints)
}
}
min.risk.downside <- function(mar = 0) {
mar = mar
function(ia,constraints) {
ia$parameters.mar = as.numeric(mar)
min.risk.downside.portfolio(ia,constraints)
}
}
min.mad.downside <- function(mar = 0) {
mar = mar
function(ia,constraints) {
ia$parameters.mar = as.numeric(mar)
min.mad.downside.portfolio(ia,constraints)
}
}
sample.shrinkage <- function( hist, hist.all ) {
cov(hist, use='complete.obs', method='pearson')
}
sample.anchored.shrinkage <- function( hist, hist.all ) {
cov(hist.all, use='complete.obs', method='pearson')
}
sample.mix.shrinkage <- function( hist, hist.all ) {
0.5 * sample.shrinkage(hist, hist.all) +
0.5 * sample.anchored.shrinkage(hist, hist.all)
}
exp.sample.shrinkage <- function( hist, hist.all ) {
hist = na.omit(hist)
lam = 0.9
i = 0:(nrow(hist)-1)
wt = lam^i
wt = wt/sum(wt)
cov.wt(hist, wt=rev(wt))$cov
}
diagonal.shrinkage <- function( hist, hist.all ) {
n = ncol(hist)
s0 = apply(hist, 2, sd, na.rm=T)
diag(n) * (s0 %*% t(s0))
}
average.shrinkage <- function( hist, hist.all ) {
n = ncol(hist)
correlation = cor(hist, use='complete.obs', method='pearson')
avg.correlation = (sum(correlation) - n) / (n*n - n)
create.cov.matrix(avg.correlation, hist)
}
min.shrinkage <- function( hist, hist.all ) {
correlation = cor(hist, use='complete.obs', method='pearson')
create.cov.matrix(min(correlation), hist)
}
max.shrinkage <- function( hist, hist.all ) {
n = ncol(hist)
correlation = cor(hist, use='complete.obs', method='pearson')
create.cov.matrix(max(correlation-diag(n)), hist)
}
create.cov.matrix <- function( value, hist ) {
n = ncol(hist)
s0 = apply(hist, 2, sd, na.rm=T)
temp = diag(n)
((matrix(1,n,n) - temp) * value + temp) * (s0 %*% t(s0))
}
ledoit.wolf.shrinkage <- function( hist, hist.all ) {
require(BurStFin)
var.shrink.eqcor(hist, 1, compatible = T)
}
factor.model.shrinkage <- function( hist, hist.all ) {
require(BurStFin)
factor.model.stat(hist, 1)
}
cov.shrink <- function(h, prior = NULL, shrinkage = NULL, roff.method = 1) {
require(tawny)
T = nrow(h)
S = cov.sample(h)
if( is.function(prior) ) prior = prior(h)
if( is.null(prior) ) prior = tawny::cov.prior.cc(S)
if( is.null(shrinkage) ) {
p = tawny::shrinkage.p(h, S)
if( roff.method == 0 )
r = sum(p$diags, na.rm=TRUE)
else
r = tawny::shrinkage.r(h, S, p)
c = tawny::shrinkage.c(prior, S)
k = (p$sum - r) / c
shrinkage = max(0, min(k/T, 1))
}
return(list(sigma = shrinkage * prior + (1 - shrinkage) * S, shrinkage = shrinkage))
}
cov.sample <- function(h) {
T = nrow(h)
x = h - rep.row(colMeans(h), T)
(t(x) %*% x) / T
}
cov.const.cor <- function(h) {
sample = cov.sample(h)
n = ncol(h)
var = diag(sample)
cov.mat = sqrt(var) %*% t(sqrt(var))
avg.rho = (sum(sample / iif(cov.mat==0,1,cov.mat))-n)/(n*(n-1))
prior = avg.rho * cov.mat
diag(prior) = var
prior
}
cov.diag <- function(h) {
S = cov.sample(h)
diag(diag(S))
}
cov.market <- function(h) {
T = nrow(h)
x = h - rep.row(colMeans(h), T)
xmkt = rowMeans(x)
n = ncol(h)
sample=cov(cbind(x, xmkt))*(T - 1)/T
covmkt=sample[1:n,n+1]
varmkt=sample[n+1,n+1]
prior=covmkt %*% t(covmkt) / varmkt
diag(prior) = diag(sample[1:n,1:n])
prior
}
cov.2param <- function(h) {
sample = cov.sample(h)
n = ncol(h)
meanvar=mean(diag(sample))
meancov=(sum(sample) -  sum(diag(sample)))/(n*(n-1))
meanvar*diag(n) + meancov*(1-diag(n))
}
shrink.diag <- function(s=NULL) { s=s; function(x, a) { cov.shrink(x, cov.diag, s, 0)$sigma }}
shrink.const.cor <- function(s=NULL) { s=s; function(x, a) { cov.shrink(x, cov.const.cor, s, 1)$sigma }}
shrink.single.index <- function(s=NULL) { s=s; function(x, a) { cov.shrink(x, cov.market, s, 1)$sigma }}
shrink.two.parameter <- function(s=NULL) { s=s; function(x, a) { cov.shrink(x, cov.2param, s, 1)$sigma }}
empty.group <- function
(
ia
)
{
return( rep(1,ia$n) )
}
static.group <- function(group)
{
group = group
function
(
ia
)
{
return(group[ia$index])
}
}
cluster.group.hclust <- function
(
ia
)
{
if(ia$n <= 2) return(c(1,1)[1:ia$n])
dissimilarity = 1 - ia$correlation
distance = as.dist(dissimilarity)
fit =  hclust(distance, method='ward')
minh = min(fit$height)
maxh = max(fit$height)
group = cutree(fit, h=minh + (maxh - minh) /3)
return( group )
}
cluster.group.kmeans.90 <- function
(
ia
)
{
if(ia$n <= 2) return(c(1,1)[1:ia$n])
dissimilarity = 1 - cor(ia$hist.returns, use='complete.obs',method='spearman')
distance = as.dist(dissimilarity)
n = ncol(ia$correlation)
n = ceiling(n*2/3)
xy = cmdscale(distance)
for (i in 2:n) {
fit = kmeans(xy, centers=i, iter.max=100, nstart=100)
p.exp = 1- fit$tot.withinss / fit$totss
if(p.exp > 0.9) break
}
group = fit$cluster
return( group )
}
cluster.group.kmeans.elbow <- function
(
ia
)
{
if(ia$n <= 2) return(c(1,1)[1:ia$n])
dissimilarity = 1 - cor(ia$hist.returns, use='complete.obs',method='spearman')
distance = as.dist(dissimilarity)
n = ncol(ia$correlation)
n = ceiling(n*2/3)
xy = cmdscale(distance)
p.exp = rep(NA, n)
for (i in 2:n) {
fit = kmeans(xy, centers=i, iter.max=100, nstart=100)
p.exp[i] = 1- fit$tot.withinss / fit$totss
}
icluster = find.maximum.distance.point(p.exp[-1]) + 1
fit = kmeans(xy, centers=icluster, iter.max=100, nstart=100)
group = fit$cluster
return( group )
}
cluster.group.FTCA <- function
(
threshold = 0.5
)
{
function
(
ia
)
{
n = ia$n
map.index = 1:n
min.cluster.group = 1
if (threshold >= 1) return(map.index)
group = rep(0, n)
names(group) = names(ia$risk)
index = rep(TRUE, n)
names(index) = names(ia$risk)
while (n > 0) {
if (n == 1) {
group[index] = min.cluster.group
break
} else {
cor.matrix = ia$correlation[index, index]
if (n == 2) {
if (cor.matrix[1,2] > threshold)
group[index] = min.cluster.group
else
group[index] = c(min.cluster.group, min.cluster.group + 1)
break
} else {
avg.corr.contribution = (rowSums(cor.matrix) - 1) / (n - 1)
avg.rank = rank(avg.corr.contribution)
tip = which.min(avg.rank)
top = which.max(avg.rank)
if (cor.matrix[tip,top] > threshold) {
group[index] = min.cluster.group
break
} else {
index.top = map.index[index][cor.matrix[,top] > threshold]
index.tip = map.index[index][cor.matrix[,tip] > threshold]
group[index.tip] = min.cluster.group
group[index.top] = min.cluster.group + 1
index[index.tip] = F
index[index.top] = F
min.cluster.group = min.cluster.group + 2
n = sum(index)
}
}
}
}
return(group)
}
}
cluster.group.FTCA.test <- function() {
load.packages('quantmod')
tickers = spl('GLD,UUP,SPY,QQQ,IWM,EEM,EFA,IYR,USO,TLT')
tickers = spl('GLD,TLT,SPY,IWM,QQQ,EFA,EEM,IYR')
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1900-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all')
portfolio.allocation.custom.stats.clusters <- function(x,ia) {
return(list(
clusters.FTCA = cluster.group.FTCA(0.5)(ia)
))
}
periodicity = 'months'
lookback.len = 252
obj = portfolio.allocation.helper(data$prices,
periodicity = periodicity, lookback.len = lookback.len,
min.risk.fns = list(EW=equal.weight.portfolio),
custom.stats.fn = portfolio.allocation.custom.stats.clusters
)
clusters = obj$clusters.FTCA$EW
clusters['2012:05::']
temp1 = clusters['2011::']
plot.data = coredata(temp1)
rownames(plot.data) = format(index.xts(temp1), '%Y%m')
plot.table(plot.data, highlight = plot.data + 1)
obj = portfolio.allocation.helper(data$prices,
periodicity = periodicity, lookback.len = lookback.len,
min.risk.fns = list(
C.EW.kmeans = distribute.weights(equal.weight.portfolio, cluster.group.kmeans.90),
C.EW.FTCA = distribute.weights(equal.weight.portfolio, cluster.group.FTCA(0.5)),
C.RP.kmeans = distribute.weights(risk.parity.portfolio(), cluster.group.kmeans.90),
C.RP.FTCA = distribute.weights(risk.parity.portfolio(), cluster.group.FTCA(0.5)),
C.MD.kmeans = distribute.weights(max.div.portfolio, cluster.group.kmeans.90),
C.MD.FTCA = distribute.weights(max.div.portfolio, cluster.group.FTCA(0.5)),
C.MV.kmeans = distribute.weights(min.var.portfolio, cluster.group.kmeans.90),
C.MV.FTCA = distribute.weights(min.var.portfolio, cluster.group.FTCA(0.5)),
C.MVE.kmeans = distribute.weights(min.var.excel.portfolio, cluster.group.kmeans.90),
C.MVE.FTCA = distribute.weights(min.var.excel.portfolio, cluster.group.FTCA(0.5)),
C.MCE.kmeans = distribute.weights(min.corr.excel.portfolio, cluster.group.kmeans.90),
C.MCE.FTCA = distribute.weights(min.corr.excel.portfolio, cluster.group.FTCA(0.5)),
C.MS.kmeans = distribute.weights(max.sharpe.portfolio(), cluster.group.kmeans.90),
C.MS.FTCA = distribute.weights(max.sharpe.portfolio(), cluster.group.FTCA(0.5)),
C.ERC.kmeans = distribute.weights(equal.risk.contribution.portfolio, cluster.group.kmeans.90),
C.ERC.FTCA = distribute.weights(equal.risk.contribution.portfolio, cluster.group.FTCA(0.5))
)
)
models = create.strategies(obj, data)$models
png(filename = 'plot1.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
strategy.performance.snapshoot(models, T)
dev.off()
png(filename = 'plot2.png', width = 600, height = 500, units = 'px', pointsize = 12, bg = 'white')
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')
dev.off()
}
distribute.weights <- function
(
fn,
group.fn = NA,
fn.within = NA
)
{
fn = match.fun(fn)
if(!is.function(group.fn)) if(!is.na(group.fn)) group.fn = match.fun(group.fn)
if(!is.function(fn.within)) if(!is.na(fn.within)) fn.within = match.fun(fn.within)
if(!is.function(fn.within)) fn.within = fn
function
(
ia,
constraints
)
{
if(!is.function(group.fn)) return(fn(ia, constraints))
group = as.numeric(group.fn(ia))
ngroups = max(group)
if(ngroups == 1) return(fn(ia, constraints))
weight0 = rep(NA, ia$n)
hist.g = NA * ia$hist.returns[,1:ngroups]
for(g in 1:ngroups) {
index = group == g
if( sum(index) == 1 ) {
weight0[index] = 1
hist.g[,g] = ia$hist.returns[, index, drop=F]
} else {
ia.temp = create.ia.subset(ia, index)
constraints.temp = create.basic.constraints(ia.temp$n, 0, 1, 1)
w0 = match.fun(fn.within)(ia.temp, constraints.temp)
weight0[index] = w0
hist.g[,g] = ia.temp$hist.returns %*% w0
}
}
ia.g = create.ia(hist.g)
constraints.g = create.basic.constraints(ngroups, 0, 1, 1)
group.weights = match.fun(fn)(ia.g, constraints.g)
for(g in 1:ngroups)
weight0[group == g] = weight0[group == g] * group.weights[g]
weight0
}
}
get.algo <- function(algo.name, has.param = F) {
algo.map = list(
'cluster' = distribute.weights,
'max.sharpe' = max.sharpe.portfolio,
'risk.parity' = risk.parity.portfolio
)
if(any(names(algo.map) == algo.name))
if(has.param)
algo.map[[ algo.name ]]
else
algo.map[[ algo.name ]]()
else {
if(has.param) {
fn = try( match.fun(algo.name) , silent = TRUE)
if(class(fn)[1] == 'try-error')	fn = try( match.fun(paste0(algo.name, '.portfolio')) , silent = TRUE)
} else {
fn = try( match.fun(paste0(algo.name, '.portfolio')) , silent = TRUE)
if(class(fn)[1] == 'try-error')	fn = try( match.fun(algo.name) , silent = TRUE)
}
if(class(fn)[1] == 'try-error') stop(paste('get.algo', fn))
fn
}
}
get.group <- function(group.name) {
group.map = list(
'none' = empty.group,
'hclust' = cluster.group.hclust,
'kmeans90' = cluster.group.kmeans.90
)
if(any(names(group.map) == group.name))
group.map[[ group.name ]]
else
stop(paste('Unknown group', group.name))
}
map.min.risk.fns <- function(strategys) {
strategys = spl(strategys,';')
min.risk.fns = list()
strategys = strategys[ nchar(strategys) > 0]
for(i in 1:len(strategys)) {
temp = spl(strategys[i])
f.name = temp[1]
if(nchar(f.name) == 0 || f.name == 'Empty') f.name = paste(temp[-1], collapse='-')
temp = tolower(temp)[-1]
if(len(temp) == 1)
min.risk.fns[[ f.name ]] = get.algo(temp[1])
else {
if(temp[1] == 'cluster') {
params = trim(spl(temp[2], ':'))
min.risk.fns[[ f.name ]] = get.algo(temp[1], T)( get.algo(params[2]), get.group(params[1]) )
} else
min.risk.fns[[ f.name ]] = get.algo(temp[1],T)(temp[-1])
}
}
min.risk.fns
}
rso.portfolio <- function
(
weight.fn,
k,
s,
const.lb = 0,
const.ub = 1,
const.sum = 1
)
{
weight.fn = match.fun(weight.fn)
k = k
s = s
const.lb = const.lb
const.ub = const.ub
const.sum = const.sum
constraints0 = create.basic.constraints(k, const.lb, const.ub, const.sum)
function
(
ia,
constraints
)
{
constraints1 = constraints0
k1 = k
if(k > ia$n) {
k1 = ia$n
constraints1 = create.basic.constraints(k1, const.lb, const.ub, const.sum)
}
space = seq(1:ia$n)
index.samples =t(replicate(s, sample(space, size=k1)))
weight = matrix(NA, nrow = s, ncol = ia$n)
for(i in 1:s){
ia.temp = create.ia.subset(ia, index.samples[i,])
weight[i,index.samples[i,]] = weight.fn(ia.temp, constraints1)
}
final.weight = ifna(colMeans(weight, na.rm=T), 0)
final.weight / sum(final.weight)
}
}
portfolio.allocation.helper.parallel <- function
(
cores = 1,
prices,
periodicity = 'weeks',
period.ends = endpoints(prices, periodicity),
lookback.len = 60,
n.skip = 1,
universe = prices[period.ends,,drop=F]>0,
prefix = '',
min.risk.fns = 'min.var.portfolio',
custom.stats.fn = NULL,
shrinkage.fns = 'sample.shrinkage',
create.ia.fn = create.ia,
update.ia.fn = update.ia,
adjust2positive.definite = T,
silent = F,
log = log.fn(),
log.frequency = 10,
const.lb = 0,
const.ub = 1,
const.sum = 1
)
{
cores = round(cores)
if(cores <= 1)
return(portfolio.allocation.helper(prices, periodicity, period.ends, lookback.len, n.skip,
universe, prefix,
min.risk.fns, custom.stats.fn, shrinkage.fns,
create.ia.fn, update.ia.fn,
adjust2positive.definite, silent, log,
const.lb, const.ub, const.sum))
load.packages('foreach,doParallel')
cl<-makeCluster(cores)
registerDoParallel(cl, cores = cores)
period.ends = period.ends[period.ends > 0]
start.i = which(period.ends >= (lookback.len + n.skip))[1]
chunks = c(1, floor(seq(start.i, len(period.ends)+1, length.out = cores + 1)[-1]))
temp = 1:len(period.ends)
if( nrow(universe) != len(period.ends) ) {
if( nrow(universe) == nrow(prices) )
universe = universe[period.ends,,drop=F]
else
stop("universe incorrect number of rows")
}
universe[is.na(universe)] = F
out <- foreach(i=1:cores, .packages=spl('quantmod,SIT')) %dopar% {
new.universe = universe
new.universe[temp[-c(chunks[i] : (chunks[i+1]-1))],]=F
portfolio.allocation.helper(prices, periodicity, period.ends, lookback.len, n.skip,
universe = new.universe, prefix,
min.risk.fns, custom.stats.fn, shrinkage.fns,
create.ia.fn, update.ia.fn,
adjust2positive.definite, silent, log, log.frequency,
const.lb, const.ub, const.sum)
}
stopCluster(cl)
base.out = out[[1]]
for(i in 2:cores) {
include.index = temp[c(chunks[i] : (chunks[i+1]-1))]
for(v in names(out[[i]])) {
if(is.list(out[[i]][[v]]))
for(n in names(out[[i]][[v]]))
base.out[[v]][[n]][include.index,] = out[[i]][[v]][[n]][include.index,]
if(is.matrix(out[[i]][[v]]))
base.out[[v]][include.index,] = out[[i]][[v]][include.index,]
}
}
base.out
}
portfolio.allocation.helper <- function
(
prices,
periodicity = 'weeks',
period.ends = endpoints(prices, periodicity),
lookback.len = 60,
n.skip = 1,
universe = prices[period.ends,,drop=F]>0,
prefix = '',
min.risk.fns = 'min.var.portfolio',
custom.stats.fn = NULL,
shrinkage.fns = 'sample.shrinkage',
create.ia.fn = create.ia,
update.ia.fn = update.ia,
adjust2positive.definite = T,
silent = F,
log = log.fn(),
log.frequency = 10,
const.lb = 0,
const.ub = 1,
const.sum = 1
)
{
load.packages('quadprog,corpcor')
load.packages('quadprog,corpcor,lpSolve,kernlab')
period.ends = period.ends[period.ends > 0]
if( nrow(universe) != len(period.ends) ) {
if( nrow(universe) == nrow(prices) )
universe = universe[period.ends,,drop=F]
else
stop("universe incorrect number of rows")
}
universe[is.na(universe)] = F
if(len(const.lb) == 1) const.lb = rep(const.lb, ncol(prices))
if(len(const.ub) == 1) const.ub = rep(const.ub, ncol(prices))
if(is.character(min.risk.fns)) {
min.risk.fns = spl(min.risk.fns)
names(min.risk.fns) = min.risk.fns
min.risk.fns = as.list(min.risk.fns)
}
for(i in 1:len(min.risk.fns)) {
f = spl(names(min.risk.fns)[i], '_')
f.name = paste(prefix, gsub('\\.portfolio', '', f[1]),sep='')
if(is.character(min.risk.fns[[i]])) {
if(len(f) == 1) {
min.risk.fns[[ i ]] = match.fun(f[1])
} else {
f.name = paste(f.name, f[-1], sep='_')
min.risk.fns[[ i ]] = match.fun(f[1])(f[-1])
}
}
names(min.risk.fns)[i] = f.name
}
if(is.character(shrinkage.fns)) {
shrinkage.fns = spl(shrinkage.fns)
names(shrinkage.fns) = shrinkage.fns
shrinkage.fns = as.list(shrinkage.fns)
}
for(i in 1:len(shrinkage.fns)) {
f = names(shrinkage.fns)[i]
f.name = gsub('\\.shrinkage', '', f[1])
if(is.character(shrinkage.fns[[ i ]]))
shrinkage.fns[[ i ]] = match.fun(f)
names(shrinkage.fns)[i] = f.name
}
dates = index(prices)[period.ends]
weight = NA * prices[period.ends,,drop=F]
prices = coredata(prices)
ret = prices / mlag(prices) - 1
start.i = which(period.ends >= (lookback.len + n.skip))[1]
weight[] = 0
weights = list()
for(f in names(min.risk.fns))
for(c in names(shrinkage.fns))
weights[[ paste(f,c,sep='.') ]] = weight
custom = list()
if( !is.null(custom.stats.fn) ) {
custom.stats.fn = match.fun(custom.stats.fn)
dummy = matrix(NA, nr=nrow(weight), nc=len(weights))
colnames(dummy) = names(weights)
dummy = make.xts(dummy, dates)
temp = ret
temp[] = rnorm(prod(dim(ret)))
temp = custom.stats.fn(1:ncol(ret), create.ia(temp))
for(ci in names(temp)) {
temp1 = NA * dummy
if(len(temp[[ ci ]]) > 1) {
temp1 = list()
for(w in names(weights))
temp1[[w]] = NA * weights[[w]]
}
custom[[ ci ]] = temp1
}
}
index.map = 1:ncol(ret)
for( j in start.i:len(period.ends) ) {
i = period.ends[j]
hist = ret[ (i- lookback.len +1):i,, drop=F]
include.index = count(hist)== lookback.len
index = universe[j,] & include.index
n = sum(index)
if(n > 0) {
hist = hist[ , index, drop=F]
hist.all = ret[ 1:i, index, drop=F]
if(n > 1) {
constraints = create.basic.constraints(n, const.lb[index], const.ub[index], const.sum)
ia.base = create.ia.fn(hist, index.map[index], i)
for(c in names(shrinkage.fns)) {
cov.shrink = shrinkage.fns[[c]](hist, hist.all)
ia = update.ia.fn(ia.base, c, cov.shrink)
if(adjust2positive.definite) {
temp = try(make.positive.definite(ia$cov, 0.000000001), TRUE)
if(!inherits(temp, 'try-error')) ia$cov = temp
temp = try(make.positive.definite(ia$correlation, 0.000000001), TRUE)
if(!inherits(temp, 'try-error')) ia$correlation = temp
}
for(f in names(min.risk.fns)) {
fname = paste(f,c,sep='.')
if (j > 1) constraints$x0 = as.vector( weights[[ fname ]][(j-1),index] )
weights[[ fname ]][j,index] = min.risk.fns[[f]](ia, constraints)
}
}
} else {
ia = create.ia.fn(hist, index.map[index], i)
for(c in names(shrinkage.fns)) {
for(f in names(min.risk.fns)) {
fname = paste(f,c,sep='.')
weights[[ fname ]][j,index] = 1
}
}
}
if( !is.null(custom.stats.fn) ) {
for(w in names(weights)) {
x = as.vector(weights[[ w ]][j, index])
temp = custom.stats.fn(x, ia)
for(ci in names(temp)) {
if(is.list(custom[[ ci ]]))
custom[[ ci ]][[ w ]][j, index] = temp[[ ci ]]
else
custom[[ ci ]][j, w] = temp[[ ci ]]
}
}
}
}
if( j %% log.frequency == 0) if(!silent) log(j, percent = (j - start.i) / (len(period.ends) - start.i))
}
if( len(shrinkage.fns) == 1 ) {
names(weights) = gsub( paste('\\.', names(shrinkage.fns), '$', sep=''), '', names(weights) )
for(ci in names(custom))
names(custom[[ ci ]]) = gsub( paste('\\.', names(shrinkage.fns), '$', sep=''), '', names(custom[[ ci ]]) )
}
return(c(list(weights = weights, period.ends = period.ends,
periodicity = periodicity, lookback.len = lookback.len), custom))
}
portfolio.allocation.custom.stats <- function(x,ia) {
risk.contributions = portfolio.risk.contribution(x, ia)
return(list(
risk.contributions = risk.contributions,
degree.diversification = 1 - sqrt(x %*% ia$cov %*% x) / (ia$risk %*% x),
risk.gini = 1 - portfolio.concentration.gini.coefficient(risk.contributions)
))
}
portfolio.allocation.helper.basic <- function
(
prices,
periodicity = 'weeks',
period.ends = endpoints(prices, periodicity),
lookback.len = 60,
prefix = '',
universe = prices[period.ends,]>0,
min.risk.fns = 'min.var.portfolio',
silent = F
)
{
load.packages('quadprog,corpcor')
period.ends = period.ends[period.ends > 0]
universe[is.na(universe)] = F
if(is.character(min.risk.fns)) min.risk.fns = spl(min.risk.fns)
dates = index(prices)[period.ends]
prices = coredata(prices)
ret = prices / mlag(prices) - 1
start.i = which(period.ends >= (lookback.len + 1))[1]
weight = NA * prices[period.ends,]
weight[] = 0
weights = list()
for(f in min.risk.fns)
weights[[f]] = weight
for( j in start.i:len(period.ends) ) {
i = period.ends[j]
hist = ret[ (i- lookback.len +1):i, ]
include.index = count(hist)== lookback.len
index = universe[j,] & include.index
n = sum(index)
if(n > 0) {
if(n > 1) {
hist = hist[ , index]
constraints = create.basic.constraints(n, 0, 1, 1)
ia = list()
ia$index = index
ia$n = n
ia$hist.returns = hist
ia$expected.return = apply(hist, 2, mean)
ia$risk = apply(hist, 2, sd)
ia$correlation = cor(hist, use='complete.obs', method='pearson')
ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
for(f in min.risk.fns)
weights[[ f ]][j,index] = match.fun(f)(ia, constraints)
} else {
for(f in min.risk.fns)
weights[[ f ]][j,index] = 1
}
}
if( j %% 10 == 0) if(!silent) cat(j, '\n')
}
return(list(weights = weights, period.ends = period.ends,
periodicity = periodicity, lookback.len = lookback.len))
}
create.strategies <- function
(
obj,
data,
leverage = 1,
min.weight = NA,
round.weight = NA,
execution.price = NA,
close.all.positions.index = NULL,
silent = F,
log = log.fn(),
prefix = '',
suffix = '',
clean.signal = F,
...
)
{
if(len(leverage) > 1 || leverage[1] != 1) {
if(len(leverage) == 1) leverage = rep(leverage, len(obj$weights))
for(i in 1:len(obj$weights)) obj$weights[[i]] = leverage[i] * obj$weights[[i]]
}
if(!is.na(min.weight) && min.weight != 0) for(i in names(obj$weights))
obj$weights[[i]][] = bt.apply.min.weight(coredata(obj$weights[[i]]), min.weight)
if(!is.na(round.weight) && round.weight != 0) for(i in names(obj$weights)) {
obj$weights[[i]][] = bt.apply.round.weight(coredata(obj$weights[[i]]), round.weight)
obj$weights[[i]][] = bt.apply.round.weight(coredata(obj$weights[[i]]), round.weight)
obj$weights[[i]][] = bt.apply.round.weight(coredata(obj$weights[[i]]), round.weight)
}
models = list()
n = len(names(obj$weights))
for(j in 1:n) {
i = names(obj$weights)[j]
i = paste(prefix, i, suffix, sep='')
if(!silent) log(i, percent = j / n)
data$weight[] = NA
data$execution.price[] = execution.price
data$weight[obj$period.ends,] = obj$weights[[j]]
if( !is.null(close.all.positions.index) ) data$weight[close.all.positions.index,] = 0
models[[i]] = bt.run.share(data, clean.signal = clean.signal, ...)
models[[i]]$period.weight = obj$weights[[j]]
}
obj$models = models
return(obj)
}
asset.allocation.strategy.test <- function()
{
tickers = 'SPY,QQQ,EEM,IWM,EFA,TLT,IYR,GLD'
dates='2000::'
data = strategy.load.historical.data(tickers, dates)
obj = portfolio.allocation.helper(data$prices,
periodicity = 'months', lookback.len = 60,
min.risk.fns = list(
EW=equal.weight.portfolio,
RP=risk.parity.portfolio(),
MV=min.var.portfolio
),
custom.stats.fn = 'portfolio.allocation.custom.stats'
)
models = create.strategies(obj, data)$models
plotbt.custom.report.part1(models)
plotbt.custom.report.part2(models)
strategy.performance.snapshoot(models)
}
target.vol.strategy <- function(model, weight,
target = 10/100,
lookback.len = 21,
max.portfolio.leverage = 100/100)
{
ret.log.model = ROC(model$equity, type='continuous')
hist.vol.model = sqrt(252) * runSD(ret.log.model, n = lookback.len)
hist.vol.model = as.vector(hist.vol.model)
weight.target = weight * (target / hist.vol.model)
rs = rowSums(abs(weight.target))
weight.target = weight.target / iif(rs > max.portfolio.leverage, rs/max.portfolio.leverage, 1)
return(weight.target)
}
calendar.signal <- function(key.date, ...) {
offsets = list( ... )
if( is.list(offsets[[1]]) ) offsets = offsets[[1]]
else {
default.names = as.character(substitute(c(...))[-1])
default.names = paste0('L(', default.names, ')')
if(is.null(names(offsets))) names(offsets) = default.names
else names(offsets) = iif(nchar(names(offsets))==0, default.names, names(offsets))
}
signals = list()
for(n in names(offsets)) {
offset = offsets[[n]]
signal = mlag(key.date, offset[1])
for(i in offset) signal = signal | mlag(key.date, i)
signals[[n]] = signal
}
signals
}
calendar.strategy <- function(data, ..., universe = data$prices > 0, do.lag.universe = 1) {
signals = list( ... )
if( is.list(signals[[1]]) ) signals = signals[[1]]
else {
default.names = as.character(substitute(c(...))[-1])
if(is.null(names(signals))) names(signals) = default.names
else names(signals) = iif(nchar(names(signals))==0, default.names, names(signals))
}
universe = mlag(universe, do.lag.universe)
models = list()
nassets = ncol(data$prices)
for(n in names(signals)) {
data$weight[] = NA
temp = ifna(universe & signals[[n]], F)
if(nassets == 1)
data$weight[] = temp
else
data$weight[] = ifna(temp / rowSums(temp),0)
models[[n]] = bt.run.share(data, do.lag = 0, trade.summary=T, clean.signal=T, silent=T)
}
models
}
last.trades <- function(model, n=20, make.plot=T, return.table=F) {
ntrades = min(n, nrow(model$trade.summary$trades))
trades = last(model$trade.summary$trades, ntrades)
if(make.plot) {
layout(1)
plot.table(trades)
}
if(return.table) trades
}
ifna.prev <- function(y)
{
y1 = !is.na(y)
y1[1]=T
return( y[cummax( (1:length(y)) * y1 )]	)
}
ifna.prevx <- function(y) {
y1 = !is.na(y)
y1[1]=T
return( cummax( (1:length(y)) * y1 ) )
}
ifna.prevx.rev <- function(y) {
y1 = !is.na(y)
y1[length(y)] = T
y1[!y1] = Inf
rev(cummin(rev((1:length(y)) * y1)))
}
ifna.prevx.test <- function() {
y = c(NA,1,1,NA,2,2,NA,NA)
y[ifna.prevx(y)]
y[ifna.prevx.rev(y)]
}
cross <- function( array1, array2 ) {
array1 > array2 & iif(len(array1) > 1, mlag(array1), array1) < iif(len(array2) > 1, mlag(array2), array2)
}
cross.up <- function( array1, array2 ) { cross( array1, array2 ) }
cross.dn <- function( array1, array2 ) { cross( array2, array1 ) }
percent.rank <- function
(
data,
n=252
)
{
pctRank <- function(x,i) sum(x[i,1] >= x[(i- (n-1) ):i,])
out = data
data = coredata(data)
if( is.null(dim(data)) ) dim(data) = c(len(data),1)
rng = n:len(data)
out[] = c( rep(NA,(n-1)), sapply(rng, function(i) pctRank(data, i) / n) )
return(out)
}
percent.rankM <- function
(
...,
n = 252
)
{
data = variable.number.arguments( ... )
out = data[[1]]
for(j in 1:len(data)) data[[j]] = coredata(data[[j]])
rank.data = data[[ len(data) ]]
pctRank <- function(x,i) sum(rank.data[i] >= x[(i- (n-1) ):i])
rng = n:len(rank.data)
out[] = 0
for(j in 1:len(data))
out[] = out[] + c( rep(NA,(n-1)), sapply(rng, function(i) pctRank(data[[j]], i) / n) )
return(out/len(data))
}
DV <- function
(
HLC,
n=2,
bounded=FALSE
)
{
hlMean = rowMeans( HLC[,-3] )
res = runMean( HLC[,3] / hlMean, n ) - 1
if(bounded) res = percent.rank(res, 252)
return(res)
}
DVI <- function
(
x,
n=250
)
{
ColumnC = ( x / runMean(x,3) ) - 1
ColumnD = ( runMean( ColumnC , 5 ) + ( runMean( ColumnC , 100 ) / 10 ) ) / 2
ColumnE = runMean( ColumnD , 5 )
ColumnF = iif( x > mlag(x) , 1 , -1 )
ColumnG = ( runSum( ColumnF , 10 ) + ( runSum( ColumnF , 100 ) / 10 ) ) / 2
ColumnH = runMean( ColumnG , 2 )
DVI.magnitude = percent.rank( ColumnE , n )
DVI.stretch = percent.rank( ColumnH, n )
DVI = ( 0.8 * DVI.magnitude ) + ( 0.2 * DVI.stretch )
return(list(DVI=DVI, DVI.magnitude=DVI.magnitude, DVI.stretch=DVI.stretch))
}
TSI <- function
(
HLC,
n=10
)
{
HLC = apply(HLC, 2, ifna.prev)
ratio = ( HLC[,3] - mlag(HLC[,3], n) ) / ATR( HLC , n )[, "atr"]
out = SMA( SMA( ratio , n ), 100 )
return(out)
}
ulcer.index <- function
(
x,
n=14
)
{
sqrt(runSum((100*( x - runMax(x,n) ) / runMax(x,n))^2, n) / n)
}
ev.ratio <- function
(
data,
n = 252
)
{
ret = coredata(data / mlag(data) - 1)
rng = n:len(data)
out = data
out[] = c( rep(NA,(n-1)), sapply(rng,
function(i) {
r = ret[(i- (n-1) ):i]
-sum(r > 0) / n * sum(r[r > 0]) / sum(r[r < 0])
}))
return(out)
}
ntop <- function
(
data,
topn = 1,
dirMaxMin = TRUE
)
{
temp = coredata(data)
if(topn == ncol(data)) {
index = is.na(temp)
temp[index] = 0
temp[!index] = 1
out = data
out[] = ifna(temp / rowSums(temp),0)
return( out )
}
for( i in 1:nrow(data) ) {
x = temp[i,]
o = sort.list(x, na.last = TRUE, decreasing = dirMaxMin)
index = which(!is.na(x))
x[] = NA
if(len(index)>0) {
n = min(topn, len(index))
x[o[1:n]] = 1/n
}
temp[i,] = x
}
temp[is.na(temp)] = 0
out = data
out[] = temp
return( out )
}
ntop.helper <- function
(
x,
n=1,
dirMaxMin = TRUE
)
{
o = sort.list(x, na.last=TRUE, decreasing = dirMaxMin)
index = which(!is.na(x))
x[] = 0
if(len(index)>0) {
n = min(n,len(index))
x[o[1:n]] = 1/n
}
return(x)
}
ntop.speed.test <- function()
{
load.packages('quantmod')
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU,IWB,IWD,IWF,IWM,IWN,IWO,IWP,IWR,IWS,IWV,IWW,IWZ')
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1970::2011')
prices = data$prices
n = len(tickers)
a = coredata(prices)
b = a
c = a
tic(12)
for( i in 1:nrow(b) ) {
b[i,] = ntop.helper(b[i,], n, T)
}
toc(12)
tic(12)
d = prices
for( i in 1:nrow(c) ) {
d[i,] = ntop.helper(c[i,], n, T)
}
toc(12)
range(b-d)
}
ntop.keep <- function
(
data,
topn = 1,
keepn = 1,
dirMaxMin = TRUE
)
{
temp = coredata(data)
for( i in 1:nrow(temp) ) {
x = temp[i,]
o = sort.list(x, na.last = TRUE, decreasing = dirMaxMin)
index = which(!is.na(x))
x[] = NA
if(len(index)>0) {
n = min(topn, len(index))
x[o[1:n]] = 1
if( i>=2 ) {
y = coredata(temp[(i-1),])
n1 = min(keepn,len(index))
y[-o[1:n1]] = NA
index1 = which(!is.na(y))
if(len(index1)>0) {
x[] = NA
x[index1] = 1
for( j in 1:n ) {
if( sum(x,na.rm=T) == topn ) break
x[o[j]] = 1
}
}
}
}
temp[i,] = x/sum(x,na.rm=T)
}
temp[is.na(temp)] = 0
out = data
out[] = temp
return( out )
}
br.rank <- function(x)
{
t(apply(coredata(-x), 1, rank, na.last='keep'))
}
super.smoother.filter <- function(x) {
a1 = exp( -1.414 * pi / 10.0 )
b1 = 2.0 * a1 * cos( (1.414*180.0/10.0) * pi / 180.0 )
c2 = b1
c3 = -a1 * a1
c1 = 1.0 - c2 - c3
x = c1 * (x + mlag(x)) / 2
x[1] = x[2]
out = x * NA
out[] = filter(x, c(c2, c3), method='recursive', init=c(0,0))
out
}
roofing.filter <- function(x) {
alpha1 = (cos((0.707*360 / 48) * pi / 180.0 ) + sin((0.707*360 / 48) * pi / 180.0 ) - 1) / cos((0.707*360 / 48) * pi / 180.0 )
x = (1 - alpha1 / 2)*(1 - alpha1 / 2)*( x - 2*mlag(x) + mlag(x,2))
x[1] = x[2] = x[3]
HP = x * NA
HP[] = filter(x, c(2*(1 - alpha1), - (1 - alpha1)*(1 - alpha1)), method='recursive', init=c(0,0))
super.smoother.filter(HP)
}
roofing.stochastic.indicator  <- function(x, lookback = 20) {
Filt = roofing.filter(x)
HighestC = runMax(Filt, lookback)
HighestC[1:lookback] = as.double(HighestC[lookback])
LowestC = runMin(Filt, lookback)
LowestC[1:lookback] = as.double(LowestC[lookback])
Stoc = (Filt - LowestC) / (HighestC - LowestC)
super.smoother.filter(Stoc)
}
spl <- function
(
s,
delim = ','
)
{
return(unlist(strsplit(s,delim)));
}
join <- function
(
v,
delim = ''
)
{
return(paste(v,collapse=delim));
}
trim <- function
(
s
)
{
s = sub(pattern = '^ +', replacement = '', x = s)
s = sub(pattern = ' +$', replacement = '', x = s)
return(s)
}
len <- function
(
x
)
{
return(length(x))
}
iif <- function
(
cond,
truepart,
falsepart
)
{
if(len(cond) == 1) { if(cond) truepart else falsepart }
else {
if(length(falsepart) == 1) {
temp = falsepart
falsepart = cond
falsepart[] = temp
}
if(length(truepart) == 1)
falsepart[cond] = truepart
else {
cond = ifna(cond,F)
falsepart[cond] = truepart[cond]
}
return(falsepart);
}
}
ifna <- function
(
x,
y
)
{
return(iif(is.na(x) | is.nan(x) | is.infinite(x), y, x))
}
fast.na.omit <- function
(
x
)
{
x[!is.na(x)]
}
ifnull <- function
(
x,
y
) {
return(iif(is.null(x), y, x))
}
fast.rep <- function(x, times) {
length(x) = times
x[] = x[1]
x
}
fast.rep.test.speed <- function() {
test1 <- function() {
rep(101,10000)
}
test2 <- function() {
fast.rep(101,10000)
}
require(rbenchmark)
benchmark(
test1(),
test2(),
columns = c("test", "replications", "elapsed", "relative"),
order = "relative",
replications = 10000
)
}
count <- function(
x,
side = 2
)
{
if( is.null(dim(x)) ) {
sum( !is.na(x) )
} else {
apply(!is.na(x), side, sum)
}
}
run.count <- function
(
x,
window.len
)
{
n    = length(x)
xcount = cumsum( !is.na(x) )
ycount = xcount[-c(1 : (k-1))] - c(0, xcount[-c((n-k+1) : n)])
return( c( xcount[1:(k-1)], ycount))
}
date.dayofweek <- function(dates)
{
return(as.double(format(dates, '%w')))
}
date.day <- function(dates)
{
return(as.double(format(dates, '%d')))
}
date.week <- function(dates)
{
return(as.double(format(dates, '%U')))
}
date.month <- function(dates)
{
return(as.double(format(dates, '%m')))
}
date.quarter <- function(dates)
{
(((date.month(dates))-1) %/% 3)+1
}
date.year <- function(dates)
{
return(as.double(format(dates, '%Y')))
}
date.week.ends <- function(dates, last.date=T)
{
ends = which(diff( 100*date.year(dates) + date.week(dates) ) != 0)
ends.add.last.date(ends, len(dates), last.date)
}
date.month.ends <- function(dates, last.date=T)
{
ends = which(diff( 100*date.year(dates) + date.month(dates) ) != 0)
ends.add.last.date(ends, len(dates), last.date)
}
date.quarter.ends <- function(dates, last.date=T)
{
ends = which(diff( 10*date.year(dates) + date.quarter(dates) ) != 0)
ends.add.last.date(ends, len(dates), last.date)
}
date.year.ends <- function(dates, last.date=T)
{
ends = which(diff( date.year(dates) ) != 0)
ends.add.last.date(ends, len(dates), last.date)
}
ends.add.last.date <- function(ends, last.date, action=T)
{
if(action)
unique(c(ends, last.date))
else
ends
}
date.ends.fn <- function(periodicity) {
switch(periodicity,
'weeks' = date.week.ends,
'months' = date.month.ends,
'quarters' = date.quarter.ends,
'years' = date.year.ends,
date.month.ends)
}
date.ends.index <- function(out, timing) {
if(timing <= 0)
which(out$days.till == (-timing))
else
which(out$days.since == (timing))
}
date.end <- function(date = Sys.Date(), periodicity = 'months', date.format = '%Y-%m-%d') {
date = as.Date(paste(date), date.format)
temp = seq(date, date + 40, 1)
temp[date.ends.fn(periodicity)(temp)[1]]
}
business.days <- function(from, to = as.Date(from) + 31, holidays = NULL) {
from = as.Date(from)
to = as.Date(to)
dates = seq(from, to, by='day')
rm.index = date.dayofweek(dates) == 6 | date.dayofweek(dates) == 0
if(!is.null(holidays)) {
holidays = as.Date(holidays)
rm.index = rm.index | !is.na(match(dates, holidays))
}
dates[!rm.index]
}
business.days.till.end <- function(from, holidays = NULL, fn.ends = date.month.ends) {
from = as.Date(from)
dates = business.days(from - 10, from, holidays)
from = dates[len(dates)]
dates = business.days(from, from + 40, holidays)
index = match.fun(fn.ends)(dates, F)
index[1] - 1
}
business.days.since.end <- function(from, holidays = NULL, fn.ends = date.month.ends) {
from = as.Date(from)
dates = business.days(from - 10, from, holidays)
from = dates[len(dates)]
dates = business.days(from - 40, from + 10, holidays)
index = match.fun(fn.ends)(dates, F)
last.index = index[len(index)]
if( dates[last.index] == from) return(0)
from.index = sum(dates <= from)
if( dates[last.index] < from) return(from.index - last.index)
last.index = index[(len(index) - 1)]
return(from.index - last.index)
}
next.business.day <- function(from, holidays = NULL, offset = 0) {
from = as.Date(from)
dates = business.days(from + offset, from + 10, holidays)
dates[1]
}
last.business.day <- function(from, holidays = NULL, offset = 0) {
from = as.Date(from)
dates = business.days(from - 10, from - offset, holidays)
dates[1]
}
business.days.location.end <- function(dates, calendar = null, fn.ends = date.month.ends) {
dates = as.Date(dates)
n = len(dates)
load.packages('RQuantLib')
holidays = NULL
if(!is.null(calendar)) holidays = getHolidayList(calendar, dates[1] - 60, dates[1] - 1)
before = business.days(dates[1] - 60, dates[1] - 1, holidays)
n.before = len(before)
holidays = NULL
if(!is.null(calendar)) holidays = getHolidayList(calendar, dates[n] + 1, dates[n] + 60)
after = business.days(dates[n] + 1, dates[n] + 60, holidays)
all = c(before, dates, after)
n.all = len(all)
all.index = (n.before + 1) : (n.before + n)
index = match.fun(fn.ends)(all, F)
temp.cum = cumsum(fast.rep(1,n.all))
temp = temp.cum * NA
temp[index] = temp.cum[index]
days.since = temp.cum - ifna.prev(temp)
days.till = temp[ifna.prevx.rev(temp)] - temp.cum
list(days.since = days.since[all.index], days.till = days.till[all.index])
}
map2monthly <- function(equity)
{
if(compute.annual.factor(equity) >= 12) return(equity)
dates = index(equity)
equity = coredata(equity)
temp = as.Date(c('', 10000*date.year(dates) + 100*date.month(dates) + 1), '%Y%m%d')[-1]
new.dates = seq(temp[1], last(temp), by = 'month')
map = match( 100*date.year(dates) + date.month(dates), 100*date.year(new.dates) + date.month(new.dates) )
temp = rep(NA, len(new.dates))
temp[map] = equity
return( make.xts( ifna.prev(temp), new.dates) )
}
create.monthly.table <- function(monthly.data)
{
nperiods = nrow(monthly.data)
years = date.year(index(monthly.data[c(1,nperiods)]))
years = years[1] : years[2]
temp = matrix( double(), len(years), 12)
rownames(temp) = years
colnames(temp) = spl('Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec')
index = date.month(index(monthly.data[c(1,nperiods)]))
temp[] = matrix( c( rep(NA, index[1]-1), monthly.data, rep(NA, 12-index[2]) ), ncol=12, byrow = T)
return(temp)
}
third.friday.month <- function(year, month)
{
day = date.dayofweek( as.Date(c('', 10000*year + 100*month + 1), '%Y%m%d')[-1] )
day = c(20,19,18,17,16,15,21)[1 + day]
return(as.Date(c('', 10000*year + 100*month + day), '%Y%m%d')[-1])
}
dates2index <- function( x, dates = 1:nrow(x) ) {
dates.index = dates
if(!is.numeric(dates)) {
temp = x[,1]
temp[] = 1:nrow(temp)
dates.index = as.numeric(temp[dates])
}
return(dates.index)
}
load.packages <- function
(
packages,
repos = "http://cran.r-project.org",
dependencies = c("Depends", "Imports"),
...
)
{
packages = spl(packages)
for( ipackage in packages ) {
if(!require(ipackage, quietly=TRUE, character.only = TRUE)) {
install.packages(ipackage, repos=repos, dependencies=dependencies, ...)
if(!require(ipackage, quietly=TRUE, character.only = TRUE)) {
stop("package", sQuote(ipackage), 'is needed.  Stopping')
}
}
}
}
tic <- function
(
identifier
)
{
assign(paste('saved.time', identifier, sep=''), proc.time()[3], envir = .GlobalEnv)
}
toc <- function
(
identifier
)
{
if( exists(paste('saved.time', identifier, sep=''), envir = .GlobalEnv) ) {
prevTime = get(paste('saved.time', identifier, sep=''), envir = .GlobalEnv)
diffTimeSecs = proc.time()[3] - prevTime
cat('Elapsed time is', round(diffTimeSecs, 2), 'seconds\n')
} else {
cat('Toc error\n')
}
return (paste('Elapsed time is', round(diffTimeSecs,2), 'seconds', sep=' '))
}
test.tic.toc <- function()
{
tic(10)
for( i in 1 : 100 ) {
temp = runif(100)
}
toc(10)
}
mlag <- function
(
m,
nlag = 1
)
{
if( is.null(dim(m)) ) {
n = len(m)
if(nlag > 0) {
m[(nlag+1):n] = m[1:(n-nlag)]
m[1:nlag] = NA
} else if(nlag < 0) {
m[1:(n+nlag)] = m[(1-nlag):n]
m[(n+nlag+1):n] = NA
}
} else {
n = nrow(m)
if(nlag > 0) {
m[(nlag+1):n,] = m[1:(n-nlag),]
m[1:nlag,] = NA
} else if(nlag < 0) {
m[1:(n+nlag),] = m[(1-nlag):n,]
m[(n+nlag+1):n,] = NA
}
}
return(m);
}
repmat <- function
(
v,
n,
m
)
{
kronecker( matrix(1, n, m), v )
}
rep.row <- function
(
m,
nr
)
{
matrix(m, nr=nr, nc=len(m), byrow=T)
}
rep.col <- function
(
m,
nc
)
{
matrix(m, nr=len(m), nc=nc, byrow=F)
}
lookup.index <- function
(
data,
i,
details = F
)
{
n = nrow(data)
irow = ((i - 1) %% n) + 1
icol = ((i - 1) %/% n) +1
if(details)
list(irow=irow,icol=icol,obs=data[irow,icol],obsr=data[max(0,irow-5):min(nrow(data),irow+5),icol])
else
list(irow=irow,icol=icol)
}
beta.degree <- function(beta)
{
atan(beta)*360/(2*pi)
}
Sys.setenv(TZ = 'GMT')
XTSFunctions <- function() {}
make.xts <- function
(
x,
order.by
)
{
tzone = Sys.getenv('TZ')
orderBy = class(order.by)
index = as.numeric(as.POSIXct(order.by, tz = tzone))
if( is.null(dim(x)) ) {
if( len(order.by) == 1 )
x = t(as.matrix(x))
else
dim(x) = c(len(x), 1)
}
x = as.matrix(x)
x = structure(.Data = x,
index = structure(index, tzone = tzone, tclass = orderBy),
class = c('xts', 'zoo'), .indexCLASS = orderBy, tclass=orderBy, .indexTZ = tzone, tzone=tzone)
return( x )
}
xts2ts = function(x) {
annual.factor = compute.annual.factor(x)
map = c(date.day, date.week, date.month, date.quarter)
names(map) = trim(spl('252, 52, 12, 4'))
date.fn = map[[paste(annual.factor)]]
first.date = index(first(x))
last.date = index(last(x))
start = date.year(first.date)
end = date.year(last.date)
if(	!is.null(date.fn) ) {
start = c(start, date.fn(first.date))
end = c(end, date.fn(last.date))
}
ts(coredata(x[,1]), start = start, end = end, deltat = 1 / annual.factor)
}
flip.xts <- function(x)
{
dates = index(x)
dates.index = nrow(x):1
out = make.xts(coredata(x)[dates.index,], dates[dates.index])
indexClass(out) = indexClass(x)
return( out )
}
write.xts <- function
(
x,
filename,
append = FALSE,
...
)
{
cat('Date', file = filename, append = append)
write.table(x, sep=',',  row.names = format(index(x), ...),
col.names = NA, file = filename, append = T, quote = F)
}
read.xts <- function
(
x,
date.fn = paste,
index.class = 'Date',
decreasing = FALSE,
sep = ',',
date.column = 1,
...
)
{
if (is.matrix(x) || is.data.frame(x) ) {
data = x
dates = as.matrix(data[,1,drop=F])
data  = data[,-1,drop=F]
} else {
filename = x
load.packages('data.table')
out = fread(filename, stringsAsFactors=F, sep=sep)
setnames(out,gsub(' ', '_', trim(colnames(out))))
rest.columns.expr = parse(text = paste('list(', paste(colnames(out)[-(1:date.column)],collapse=','),')'))
dates = as.matrix(out[,date.column,with=FALSE])
data = out[, eval(rest.columns.expr)]
}
dates = as.POSIXct(match.fun(date.fn)(dates), tz = Sys.getenv('TZ'), ...)
dates.index = order(dates, decreasing = decreasing)
out = make.xts(data[dates.index,,drop=F], dates[dates.index])
indexClass(out) = index.class
return( out )
}
read.xts.old <- function
(
filename,
date.fn = paste,
index.class = 'Date',
decreasing = FALSE,
...
)
{
out = read.csv(filename, stringsAsFactors=F)
dates = as.POSIXct(match.fun(date.fn)(out[,1]), tz = Sys.getenv('TZ'), ...)
dates.index = order(dates, decreasing = decreasing)
out = make.xts(out[dates.index,-1,drop=F], dates[dates.index])
indexClass(out) = index.class
return( out )
}
read.xts.yahoo.old <- function
(
filename,
date.fn = paste,
index.class = 'Date',
decreasing = FALSE,
...
)
{
temp = scan(filename, what=list('',double(0), double(0),double(0),double(0),double(0),double(0)), skip=1, sep=',', quiet =T)
dates = as.POSIXct(match.fun(date.fn)(temp[[1]]), tz = Sys.getenv('TZ'), ...)
dates.index = order(dates, decreasing = decreasing)
out = matrix(double(1),len(dates), 6)
colnames(out) = spl('Open,High,Low,Close,Volume,Adjusted')
out[,1] = temp[[2]]
out[,2] = temp[[3]]
out[,3] = temp[[4]]
out[,4] = temp[[5]]
out[,5] = temp[[6]]
out[,6] = temp[[7]]
out = make.xts(out[dates.index,],  dates[dates.index])
indexClass(out) = index.class
return( out )
}
read.xts.test <- function() {
load.packages('rbenchmark')
filename = 'c:/stocks/SPY.csv'
test1 <- function() {
out = read.csv(filename, stringsAsFactors=F)
}
test2 <- function() {
out1 = fread(filename, stringsAsFactors=F)
}
test3 <- function() {
out2 = scan(filename, what=list('',double(0), double(0),double(0),double(0),double(0),double(0)), skip=1, sep=',', quiet =T)
}
library(rbenchmark)
benchmark(
test1(),
test2(),
test3(),
columns = c("test", "replications", "elapsed", "relative"),
order = "relative",
replications = 20
)
test1 <- function() {
out = read.xts(filename, format = '%Y-%m-%d')
}
test2 <- function() {
out1 = read.xts.old(filename, format = '%Y-%m-%d')
}
test3 <- function() {
out2 = read.xts.yahoo.old(filename, format = '%Y-%m-%d')
}
library(rbenchmark)
benchmark(
test1(),
test2(),
test3(),
columns = c("test", "replications", "elapsed", "relative"),
order = "relative",
replications = 20
)
}
index.xts <- function
(
x
)
{
temp = attr(x, 'index')
class(temp) = c('POSIXct', 'POSIXt')
type = attr(x, '.indexCLASS')[1]
if( type == 'Date' || type == 'yearmon' || type == 'yearqtr')
temp = as.Date(temp)
return(temp)
}
index4xts <- function
(
x
)
{
temp = attr(x, 'index')
class(temp)='POSIXct'
return(temp)
}
index2date.time <- function(temp) {
class(temp)='POSIXct'
if( attr(x, '.indexCLASS')[1] == 'Date') {
as.Date(temp)
} else {
as.POSIXct(temp, tz = Sys.getenv('TZ'))
}
}
get.extension <- function(x)
{
trim( tail(spl(x,'\\.'),1) )
}
get.full.filename <- function(x)
{
trim( tail(spl(gsub('\\\\','/',x),'/'),1) )
}
get.filename <- function(x)
{
temp = spl(get.full.filename(x),'\\.')
join(temp[-len(temp)])
}
getSymbols.sit <- function
(
Symbols,
env = .GlobalEnv,
auto.assign = TRUE,
stock.folder = 'c:/temp/Seasonality/stocks',
stock.date.format = '%Y-%m-%d',
...
)
{
require(quantmod)
for(i in 1:len(Symbols)) {
s = Symbols[i]
temp = list()
temp[[ s ]] = list(src='csv', format=stock.date.format, dir=stock.folder)
setSymbolLookup(temp)
temp = quantmod::getSymbols(s, env = env, auto.assign = auto.assign)
if (!auto.assign) {
cat(s, format(range(index(temp)), '%d-%b-%Y'), '\n', sep='\t')
return(temp)
}
if(!is.null(env[[ s ]]))
cat(i, 'out of', len(Symbols), 'Reading', s, format(range(index(env[[ s ]])), '%d-%b-%Y'), '\n', sep='\t')
else
cat(i, 'out of', len(Symbols), 'Missing', s, '\n', sep='\t')
}
}
getSymbols.extra <- function
(
Symbols = NULL,
env = parent.frame(),
getSymbols.fn = getSymbols,
raw.data = new.env(),
set.symbolnames = F,
auto.assign = T,
...
)
{
if(is.character(Symbols)) Symbols = spl(Symbols)
if(len(Symbols) < 1) return(Symbols)
Symbols = toupper(gsub('\n','',Symbols))
map = list()
for(s in Symbols) {
name = iif(len(spl(s, '=')) > 1, spl(s, '=')[1], spl(s, '\\+')[1])
values = spl(iif(len(spl(s, '=')) > 1, spl(s, '=')[2], s), '\\+')
map[[trim(name)]] = trim(values)
}
Symbols = unique(unlist(map))
Symbols = setdiff(Symbols, ls(raw.data))
data <- new.env()
if(len(Symbols) > 0) match.fun(getSymbols.fn)(Symbols, env=data, auto.assign = T, ...)
for(n in ls(raw.data)) data[[n]] = raw.data[[n]]
if (set.symbolnames) env$symbolnames = names(map)
for(s in names(map)) {
env[[ s ]] = data[[ gsub('\\^', '', map[[ s ]][1]) ]]
if( len(map[[ s ]]) > 1)
for(i in 2:len(map[[ s ]]))
env[[ s ]] = extend.data(env[[ s ]], data[[ gsub('\\^', '', map[[ s ]][i]) ]], scale=T)
if (!auto.assign)
return(env[[ s ]])
}
}
getSymbols.extra.test <- function()
{
tickers = spl('REIT=RWX, RWX+VNQ, REIT.LONG=RWX+VNQ+VGSIX')
data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
bt.start.dates(data)
data$symbolnames = spl('REIT.LONG,RWX,REIT')
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', fill.gaps = T)
plota.matplot(data$prices)
raw.data <- new.env()
raw.data$GOLD = bundes.bank.data.gold()
tickers = spl('GLD, GLD.LONG=GLD+GOLD')
data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, raw.data = raw.data, auto.assign = T)
bt.start.dates(data)
data$symbolnames = spl('GLD.LONG,GLD')
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', fill.gaps = T)
plota.matplot(data$prices)
}
log.fn <- function(p.start=0, p.end=1) {
p.start = p.start
p.end = p.end
function(..., percent=NULL) {
cat(..., iif(is.null(percent),'',paste(', percent = ', round(100 * (p.start + percent * (p.end - p.start)), 1), '%', sep='')), '\n')
}
}
log.fn.msg <- function(msg, log = log.fn()) {
log = log
msg = msg
function(..., percent=NULL) { log(paste(msg, ...), percent=percent) }
}
asc <- function(x) { strtoi(charToRaw(x),16L) }
chr <- function(n) { rawToChar(as.raw(n)) }
make.random.string <- function(nbits = 256) { chr( runif(nbits/8, 1, 255) ) }
random.string <- function(lenght = 12) { join(sample(c(0:9, letters, LETTERS),lenght, replace=TRUE)) }
ls.f <- function(env=sys.frame(-1))unlist(lapply(ls(env=env),function(x)if(is.function(get(x)))x))
ls.v <- function(env=sys.frame(-1))unlist(lapply(ls(env=env),function(x)if(!is.function(get(x)))x))
parse.number <- function(x) {
as.numeric(gsub('[^0-9\\+-\\.]', '', x) )
}