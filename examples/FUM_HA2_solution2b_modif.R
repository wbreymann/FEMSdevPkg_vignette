# R-script for homework 2
#-------------------------------------------------------------------------------
# Model of the bank of practice sheet 9 in FEMS.
# This is the basis in order to compute problems 1 to 5 of homework 2
# The script contains comments that should allow you to understand what is done.
# An additional script for the further tasks will follow.

## Preparation --------------
rm(list=ls())
library(FEMS)
options(warnings=-1)

## Initialization -----------
t0 <- "2021-01-01"
# Start date of the contracts. Should already exist at analysis date
tstart <- "2020-12-31" # 
# scale for the amount of capital (in Mio CHF)
scale <- 1000000 # 1 Mio

### Modeling the bank --------

## The structure of the bank -----
bank <- institution("Bank")
# Create additional accounts

# Assets
bank$Assets$AddChild("LT.default")
bank$Assets$AddChild("FixedAssets")

# Liabilities
bank$Liabilities$Debt$AddChild("ShortTerm")
bank$Liabilities$Debt$AddChild("LongTerm")

# Operations
# bank$Operations$AddChild("Revenues")
# Commissions, fees, etc.
bank$Operations$Revenues$AddChild("Commissions")
# Revenue from rent
bank$Operations$Revenues$AddChild("Rent")
# Expenses
# bank$Operations$AddChild("Expenses")
bank$Operations$Expenses$AddChild("Salaries")
# Other expenses
bank$Operations$Expenses$AddChild("Other")
#bank$Operations$AddChild("Profit/loss before taxes")
bank

# risk factor model -------
spot.rates <- c(0.01, 0.02, 0.03, 0.04)
yc <- YieldCurve(label = "YC", 
                 ReferenceDate = t0, 
                 Tenors = c("1M", "2Y", "5Y", "10Y"),
                 Rates = spot.rates)
yc
plot(yc)
rf <- RFConn(yc)

## The contracts: Assets --------

# # Cash
# # a) Bank account
# # Time series of transactions:
# (cashflows <- timeSeries(
#   0, units="CHF",
#   timeSequence(from=t0, by="year", length.out=1)))
# 
# # Construction of the bank account
# 
# # balance.init <- 100*scale
# # (my.account <- bankAccount("2013-12-31", balance=balance.init,
# #                            ext_transactions = cashflows, ir=0.03))


# Liquid assets -------
# We assume that the amount stays constant and model them as a bond.
# But the interest rate is set to market rates periodically.
# We choose a period of 1 year (1 quarter would also be good ("P3ML0"))
# We could also model them as a bank account. 
assets.liquid.value <- 100*scale
assets.liquid <- bond(ContractID="Liquid", start = tstart, maturity = "10 years", 
                      nominal = assets.liquid.value, coupon = yc$Rates[1], role="long", 
                      CycleOfRateReset="P3YL0", MarketObjectCodeOfRateReset="YC") # should be P1YL0"
addContracts(list(assets.liquid), FindNode(bank$Assets, "ShortTerm"))

# events(assets.liquid, t0, rf)

# Long term assets ---------
ir.spread.assets <- 0.03
mortgage.value <- 300*scale # 300 Mio CHF
mtg.mat.short = "2 years"
mtg.mat.med = "5 years"
mtg.mat.long = "10 years"
mortgage.long1 <- bond(start = tstart, maturity = mtg.mat.short, 
                nominal = mortgage.value, coupon = spot.rates[2]+ir.spread.assets, 
                role="long", ContractID="mg1")
mortgage.long2 <- bond(start = tstart, maturity = mtg.mat.med, 
                 nominal = mortgage.value, coupon = spot.rates[3]+ir.spread.assets, 
                 role="long", ContractID="mg2")
mortgage.long3 <- bond(start = tstart, maturity = mtg.mat.long,
                 nominal = mortgage.value, coupon = spot.rates[4]+ir.spread.assets, 
                 role="long", ContractID="mg3")
addContracts(list(mortgage.long1, mortgage.long2, mortgage.long3), 
             FindNode(bank$Assets, "LongTerm"))

plot(mortgage.long1, tstart)
events(mortgage.long1, tstart, rf)

# Modeling of default ----------
# We add contracts with a negative value on the asset side by chosing 'role="short"'
# For each mortgage, we must add every year a contract that has 1% of its value.
# (4% default rate with 75% recovery rate and assumption that recovery happens immediately)
times.default <- timeSequence(from = timeDate(tstart), by = "1 years", length.out = 26)
default.rate <-0.04
recovery <- 0.75
eff.loss.rate <- (1-recovery)*default.rate # effective loss rate
 
mortgage.long1.df1 <- bond(start = as.character(times.default[2]), maturity = "1 years", 
                       nominal = default.rate*mortgage.value, 
                       PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                       coupon = spot.rates[2]+ir.spread.assets, 
                       role="short", ContractID="mg1-df1")
events(mortgage.long1.df1, t0)
mortgage.long1.df2 <- bond(start = "2022-12-30", maturity = "1 day", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[2]+ir.spread.assets, 
                           role="short", ContractID="mg1-df2")
events(mortgage.long1.df2, t0)
mortgage.long2.df1 <- bond(start = as.character(times.default[2]), maturity = "4 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[3]+ir.spread.assets, 
                           role="short", ContractID="mg2-df1")
mortgage.long2.df2 <- bond(start = as.character(times.default[3]), maturity = "3 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[3]+ir.spread.assets, 
                           role="short", ContractID="mg2-df2")
mortgage.long2.df3 <- bond(start = as.character(times.default[4]), maturity = "2 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[3]+ir.spread.assets, 
                           role="short", ContractID="mg2-df3")
mortgage.long2.df4 <- bond(start = as.character(times.default[5]), maturity = "1 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[3]+ir.spread.assets, 
                           role="short", ContractID="mg2-df4")
mortgage.long2.df5 <- bond(start = "2025-12-30", maturity = "1 day", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[3]+ir.spread.assets, 
                           role="short", ContractID="mg2-df5")

mortgage.long3.df1 <- bond(start = as.character(times.default[2]), maturity = "9 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df1")
mortgage.long3.df2 <- bond(start = as.character(times.default[3]), maturity = "8 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df2")
mortgage.long3.df3 <- bond(start = as.character(times.default[4]), maturity = "7 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df3")
mortgage.long3.df4 <- bond(start = as.character(times.default[5]), maturity = "6 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df4")
mortgage.long3.df5 <- bond(start = as.character(times.default[6]), maturity = "5 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df5")
mortgage.long3.df6 <- bond(start = as.character(times.default[7]), maturity = "4 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df6")
mortgage.long3.df7 <- bond(start = as.character(times.default[8]), maturity = "3 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df7")
mortgage.long3.df8 <- bond(start = as.character(times.default[9]), maturity = "2 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df8")
mortgage.long3.df9 <- bond(start = as.character(times.default[10]), maturity = "1 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df9")
mortgage.long3.df10 <- bond(start = "2030-12-30", maturity = "1 day", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df10")

addContracts(list(
  mortgage.long1.df1,mortgage.long1.df2,
  mortgage.long2.df1,mortgage.long2.df2,mortgage.long2.df3,mortgage.long2.df4,mortgage.long2.df5,
  mortgage.long3.df1,mortgage.long3.df2,mortgage.long3.df3,mortgage.long3.df4,mortgage.long3.df5,
  mortgage.long3.df6,mortgage.long3.df7,mortgage.long3.df8,mortgage.long3.df9,mortgage.long3.df10
), FindNode(bank$Assets, "LT.default"))

# events(mortgage.long1.df1, t0)

# Fixed assets -----------
# must be modeled with Investment contract because of depreciation
asset.fixed.value <- 200*scale
# Define time pattern of depreciation
# yearly time steps 50 years into the future
times.re <- timeSequence(from = timeDate(tstart), by = "1 years", length.out = 51)
asset.fixed.func <- function(times) {
  timeSeries(seq(asset.fixed.value, 0, length.out=51), times)
}
asset.fixed.func(times.re)

# Define Investments contract with depreciation
asset.fixed <- Investments(pattern=asset.fixed.func, 
                   args=list(times=times.re
                   ), Currency="CHF", ContractID="RE01")
addContracts(list(asset.fixed), FindNode(bank$Assets, "FixedAssets"))

# The rent must be modeled separately as operational revenue, cf. below.


## Liabilities -----------------------
# 
# Debt ---------------------

# Savings -----------------
savings <- bond(ContractID="svg", start = tstart, maturity = "10 years", 
                nominal = 600*scale, coupon = yc$Rates[1], role="short", 
                CycleOfRateReset="P1YL0", MarketObjectCodeOfRateReset="YC")

addContracts(list(savings), FindNode(bank$Liabilities, "ShortTerm"))
# 
# savings1 <- bond(ContractID="svg-delta", start = "2021-12-31", maturity = "9 years", 
#                  nominal = 50*scale, coupon = yc$Rates[1], role="long", 
#                  CycleOfRateReset="P1YL0", MarketObjectCodeOfRateReset="YC")
# addContracts(list(savings, savings1), FindNode(bank$Liabilities, "ShortTerm"))

# Long term debt ----------
ir.spread.liab <- 0.01
loan.long <- bond(ContractID="dlg1", start = tstart, maturity = "10 years", 
                nominal = 500*scale, coupon = spot.rates[4]+ir.spread.liab, role="short")
addContracts(list(loan.long), FindNode(bank$Liabilities, "LongTerm"))

plot(loan.long, t0)


### Operational cash flows ---------------
# Operations modeled 10 years into the future, 
# with the exception of rent for real estate, which is modeled for the lifetime
# of the real estate investment, i.e. 50 years
times.ops <- timeSequence(tstart, by="1 years", length.out=11)

## Revenues ------------------

# Rent -----------------------
asset.fixed.rent <- 0.05 * asset.fixed.value
rent.yearly <- rep(asset.fixed.rent, 50)
#
rent.fun <- function(rent, times) {
  timeSeries(data=rent, charvec=times)
}
rent.fun(rent.yearly, times.re[-51])
Rent <- OperationalCF(ContractID="RE-Rent", Currency="CHF",  # Define contract
                      pattern=rent.fun, 
                      args=list(rent=rent.yearly, times=times.re[-51]) )
# Test
events(Rent, t0, rf)

addContracts(list(Rent), FindNode(bank$Operations, "Rent"))

# Commissions, Fees, etc. -----------------------
commissions <- 20*scale
# asset.fixed <- bond(start = tstart, maturity = "50 years", 
#                     nominal = 200, coupon = asset.fixed.rent, role="long")
commissions.yearly <- rep(commissions, 10)
#
commissions.fun <- function(commissions, times) {
  timeSeries(data=commissions, charvec=times)
}
Commissions <- OperationalCF(ContractID="Comm", Currency="CHF",  # Define contract
                      pattern=commissions.fun, 
                      args=list(commissions=commissions.yearly, times=times.ops[-1]) )
# Test
events(Commissions, t0, rf)
addContracts(list(Commissions), FindNode(bank$Operations, "Commissions"))


## Expenses ------------------
# Salaries --------------
salaries.yearly <- rep(20*scale, 10)
#
salaries.fun <- function(times, expenses) {
  timeSeries(-expenses, times)
}
Salaries <- OperationalCF(ContractID="Salaries", Currency="CHF",  # Define contract
                          pattern=salaries.fun, args=list(
                          expenses=salaries.yearly,
                          times=times.ops[-1]) )
# Test
# events(Salaries, t0, rf)
addContracts(list(Salaries), FindNode(bank$Operations$Expenses, "Salaries"))

# Other -------------------
other.yearly <- rep(9*scale, 10)
#
other.fun <- function(times, expenses) {
  timeSeries(-expenses, times)
}
Other <- OperationalCF(ContractID="Other", Currency="CHF",  # Define contract
                          pattern=other.fun, args=list(
                            expenses=other.yearly,
                            times=times.ops[-1]) )
# Test
# events(Other, t0, rf)
addContracts(list(Other), FindNode(bank$Operations$Expenses, "Other"))

# Test: Which contracts are in the model? ----------
showContracts(bank)

# Performing the simulation ------
# calculate events
system.time({events(bank, t0, rf, end_date="2031-01-01")})

showEvents(bank$Assets$LT.default)

### Analysis -------------------
# construction of time bucket
# t2 = "2021-01-02"
by <- timeSequence(t0, by="1 years", length.out=6)
(tb <- timeBuckets(by, bucketLabs=2021:2025, 
                  breakLabs=substr(as.character(by),3,10)))


(val.nom <- value(bank, tb, scale=scale, digits=2)[,1:3])
# (val.nom <- value(bank, tb, scale=scale, digits=2)[,1,drop=FALSE])


(liq <- liquidity(bank, tb, scale=scale, digits=2)[,1:2])
(inc.nom <- income(bank, tb, scale=scale, digits=2)[,1:2])

(liq - inc.nom)[,1:2]


## Problem 1: ---------

## Problem 2 ----

# Create the current balance sheet of the bank -> cf.above
# Compute the equity capital ratio (equity / balance sheet total) -> 1/12 = 8.3%


# Equity ratio: 
eq.ratio <- round(100*(-val.nom[12,]/val.nom[2,]),2)
rownames(eq.ratio) <- "eq.ratio"
eq.ratio

# Liquidity Coverage Ratio: 
liq.ratio <- round(100*(-(val.nom[3,]+val.nom[4,])/val.nom[10,]),2)
rownames(liq.ratio) <- "liq.ratio"
liq.ratio

# Compute the ration of the liquid assets over the short term liabilities 
# Compute the same way


## Problem 3: Analysis with shifted interest rates ---------
# Rerun the simulation with the shifted yield curve.

# Define new market environment with shifted rates
spot.rates.shifted <- c(0.01, 0.02, 0.03, 0.04)+0.01
yc.shifted <- YieldCurve(label = "YC",
                 ReferenceDate = t0,
                 Tenors = c("1M", "2Y", "5Y", "10Y"),
                 Rates = spot.rates)
rf1 <- RFConn(yc.shifted)
rf1[["YC"]]

# Change rates for contracts with variable interest rates
bank$Assets$ShortTerm$contracts[[1]][["ContractTerms"]][["NominalInterestRate"]] <- spot.rates.shifted[1]
bank$Liabilities$Debt$ShortTerm$contracts[[1]][["ContractTerms"]][["NominalInterestRate"]] <- spot.rates.shifted[1]
# Rerun simulation 
events(bank, t0, rf1, end_date="2031-01-01")
### Analysis 
# # construction of time bucket
# # t2 = "2021-01-02"
# by <- timeSequence(t0, by="1 years", length.out=6)
# (tb <- timeBuckets(by, bucketLabs=2021:2025, 
#                    breakLabs=substr(as.character(by),3,10)))
(val.nom <- value(bank, tb, scale=scale, digits=2)[,1:2])
# (val.nom <- value(bank, tb, scale=scale, digits=2)[,1,drop=FALSE])
(liq <- liquidity(bank, tb, scale=scale, digits=2)[,1:2])
(inc.nom <- income(bank, tb, scale=scale, digits=2)[,1:2])
(liq - inc.nom)[,1:2]

## Problem 5 ----------------------
# Compute present value, future value, internal rate of return and duration of 
# the cash flows resulting from real estate. 
# Make the simplifying assumption that rent is paid yearly in advance 
# and use the following interest rates for discounting:
# - 2 years spot rates for terms of 1-3 years
# - 5 years spot rates for terms of 4-6 years
# - 10 years spot rates for terms > 6 years
# Here we use the yield curve for discounting

re.rent <- bank$Operations$Revenues$Rent$contracts[[1]]
cfs <- cashFlows(re.rent, from=as.character(t0), to=as.character(times.re[51]))
(pv <- presentValue(cfs, yield=yc, by=times.re[1])/scale)
(futureValue <- pv * 1.04^50)
# To compute the internal rate of return, the initial investment of 200 mio 
# must be added to cfs
cfs[1] <- cfs[1] - 200*scale
# Internal rate of return
irr(cfs)

# duration
duration(re.rent, type="fisher-weil", yield=yc, price=200*scale, from=t0)

## Problem 6 -------------------
# This cannot be solved naturally with FEMS 
# because the capitalization of the net rent (rent - maintenance expenditure) 
# can currently not be modeled with any existing contract

## Problem 7 --------------------
# The interest curve is randomly shifted
# 

# We first set the interest rate of the variable contracts to the original values
bank$Assets$ShortTerm$contracts[[1]][["ContractTerms"]][["NominalInterestRate"]] <- spot.rates[1]
bank$Liabilities$Debt$ShortTerm$contracts[[1]][["ContractTerms"]][["NominalInterestRate"]] <- spot.rates[1]
# Simulation with original values
events(bank, t0, rf, end_date="2023-01-01")
# Analysis
by <- timeSequence(t0, by="1 years", length.out=6)
(tb <- timeBuckets(by, bucketLabs=2021:2025, 
                   breakLabs=substr(as.character(by),3,10)))

# Lists for storing the analytical results
liq <- list()
val.nom <- list()
inc.nom <- list()
val.nom[["0"]] <- value(bank, tb, scale=scale, digits=2)[,1:2]
liq[["0"]] <- liquidity(bank, tb, scale=scale, digits=2)[,1:2]
inc.nom[["0"]] <- income(bank, tb, scale=scale, digits=2)[,1:2]

# Extract particular values of interest
shift <- numeric()
shift["0"] <- 0
profit <- numeric()
profit["0"] <- inc.nom[["0"]][1,1]
eq <- numeric()
eq["0"] <- -val.nom[["0"]][12,2]
liq.assets <- numeric()
liq.assets["0"] <- sum(val.nom[["0"]][3:4,2]) 

# loop over random shift of yield curve
# The simulation is pretty slow
# You can grow the simulated sample by setting ns.new <- nn.old
# If the simulation should be continnued:
ns <- 1
# Load if simulation is a continuation
# load("examples/HA2_7.rda")

## Otherwise execute the following code until dashed line ----------------------
(ns <- length(shift))
(simLen <- 2)
(nn <- ns + simLen - 1)
system.time({
  for (i in ns:nn) {
  i.ch <- as.character(i)
  # create random shift
  shift[i.ch] <- rnorm(1, 0, 0.01)
  spot.rates.shifted <- c(0.01, 0.02, 0.03, 0.04)+shift[i.ch]
  print(c(i, as.numeric(shift[i.ch])))
  yc.shifted <- YieldCurve(label = "YC",
                           ReferenceDate = t0,
                           Tenors = c("1M", "2Y", "5Y", "10Y"),
                           Rates = spot.rates.shifted)
  rf1 <- RFConn(yc.shifted)
  rf1[["YC"]]

  # Change rates for contracts with variable interest rates
  bank$Assets$ShortTerm$contracts[[1]][["ContractTerms"]][["NominalInterestRate"]] <-
    spot.rates.shifted[1]
  bank$Liabilities$Debt$ShortTerm$contracts[[1]][["ContractTerms"]][["NominalInterestRate"]] <-
    spot.rates.shifted[1]
  # Rerun simulation
  events(bank, t0, rf1, end_date="2023-01-01")
  # Store analysis
  liq[[i.ch]] <- liquidity(bank, tb, scale=scale, digits=2)
  inc.nom[[i.ch]] <- income(bank, tb, scale=scale, digits=2)
  val.nom[[i.ch]] <- value(bank, tb, scale=scale, digits=2)
  # Extract particular values
  profit[i.ch] <- inc.nom[[i.ch]][1,1]
  eq[i.ch] <- -val.nom[[i.ch]][12,2]
  liq.assets[i.ch] <- sum(val.nom[[i.ch]][3:4,2])
  # if ( !(i %% 100) )
  # {
  #     print(paste0("Data saved, i = ", i))
  #     save(shift, liq, inc.nom, val.nom, profit, eq, liq.assets, file="HA2_7.rda")
  # }
}
})
# Execute code until here
# --------------------------------------------------------------------------------
# load if don't execute simulation
load("examples/HA2_7.rda")

# --------------------------------------------------------------------------------
length(shift)
length(liq)
length(inc.nom)
length(val.nom)
length(profit)
length(eq)
length(liq.assets)

# save(shift, liq, inc.nom, val.nom, profit, eq, liq.assets, file="HA2_7.rda")
# load("HA2_7.rda")
res7 <- cbind(shift, profit, eq, liq.assets)
head(res7)
res7[order(res7[,1]),]

hist(res7[,"shift"], xlab = "Interest rate shifts")
hist(res7[,"profit"], xlab="Profit")
hist(res7[,"liq.assets"], xlab="Liquid assets")


hist(res7[,"eq"], xlab="Equity")
quantile(res7[,"eq"], probs = c(0.001, 0.01, 0.05), na.rm = FALSE,
         names = TRUE, type = 7, digits = 7)
abline(v=quantile(res7[,"eq"], probs = c(0.001, 0.01, 0.05), na.rm = FALSE),
       col=c("red","blue", "green"), lty=1)
# Expected shortfall
ES <- numeric(3)
ES[1] <- mean(sort(res7[,"eq"])[1:5])
ES[2] <- mean(sort(res7[,"eq"])[1:50])
ES[3] <- mean(sort(res7[,"eq"])[1:250])
ES
abline(v=ES, col=c("red","blue", "green"), lty=2)

# Theoretical:

profit <- 12

fct <- function(x) {
  p <- pnorm(x, mean=profit, sd=5 )
  x*p
}

# VaR 99
qnorm(0.01, mean=profit, sd=5)
# ES 99
integrate(fct, -Inf, qnorm(0.01, mean=profit, sd=5))$value /
  integrate(pnorm, -Inf, qnorm(0.01, mean=profit, sd=5), mean=profit, sd=5)$value
# Prob. of illiquidity
uniroot(qnorm, c(0,0.2), mean=profit, sd=5, tol=1.e-8)


## Problem 8 -------------------
# We model the fluctuation of the savings by a new contract in the
# Liabilities$Debt$ShortTerm account
# The changes occur at the last day of 2021.
# First, the contract is created with nominal 0
bank$Liabilities$Debt$ShortTerm$contracts <- NULL
savings1 <- bond(ContractID="svg-delta", start = "2021-12-31", maturity = "9 years", 
                nominal = 0*scale, coupon = yc$Rates[1], role="long", 
                CycleOfRateReset="P1YL0", MarketObjectCodeOfRateReset="YC")
addContracts(list(savings, savings1), FindNode(bank$Liabilities, "ShortTerm"))

# Now we randomly choose the changes of the savings.
# According to problem 8 in practice sheet 9, the volume should be a normally
# distributed random variable with mean 0 and stdev. 50.
# Since the nominal cannot be negative, we must distinguish two cases:
#  - If the volume decreases we need a long contract 
#  - If the volume increases we need a short contract
nn <- 10
savings.delta <- rnorm(nn, 0, 50)
role <- rep("RPA", nn) # Real Position Asset = "long"
role[savings.delta<0] <- "RPL" # Real Position Liability = "short"
role

# Initialization
{
savings.delta <- numeric()
savings.delta["0"] <- 0
role <- character()
role["0"] <- "RPA"
liq8 <- list()
inc8.nom <- list()
val8.nom <- list()
fcf <- numeric()
current <- numeric()
bank$Liabilities$Debt$ShortTerm$contracts[[2]]$ContractTerms$NotionalPrincipal <- 0
bank$Liabilities$Debt$ShortTerm$contracts[[2]]$ContractTerms$ContractRole <- "RPA"
events(bank, t0, rf, end_date="2022-01-01")

# events(bank$Liabilities$Debt$ShortTerm, t0, rf, end_date="2022-01-01")
liq8[["0"]] <- liquidity(bank, tb, scale=scale, digits=2)
inc8.nom[["0"]] <- income(bank, tb, scale=scale, digits=2)
val8.nom[["0"]] <- value(bank, tb, scale=scale, digits=2)
fcf["0"] <- liq8[["0"]][1,1]
current["0"] <- sum(val8.nom[["0"]][3:4,2])
}

# Loop over random changes of savings
simLen <- 4990
ns <- 1
# If the simulation should be continued:
load("examples/HA2_8.rda")
# ns <- length(savings.delta)
# nn <- ns + simLen - 1
# system.time({
# for (i in ns:nn) {
#   i.ch <- as.character(i)
#   savings.delta[i.ch] <- rnorm(1, 0, 50) # Fehler: muss as.character(i) heissen
#   if ( savings.delta[i.ch]>=0 )
#     role[i.ch] <- "RPA"
#   else 
#     role[i.ch] <- "RPL"
#   print(paste(i, savings.delta[i], role[i]))
#   
#   bank$Liabilities$Debt$ShortTerm$contracts[[2]]$ContractTerms$NotionalPrincipal <- abs(savings.delta[i.ch])*scale
#   bank$Liabilities$Debt$ShortTerm$contracts[[2]]$ContractTerms$ContractRole <- role[i.ch]
#   # events(bank$Liabilities$Debt$ShortTerm, t0, rf, end_date="2022-01-01") # This doesn't work properly
#   events(bank, t0, rf, end_date="2022-01-01")
#   liq8[[i.ch]] <- liquidity(bank, tb, scale=scale, digits=2)
#   inc8.nom[[i.ch]] <- income(bank, tb, scale=scale, digits=2)
#   val8.nom[[i.ch]] <- value(bank, tb, scale=scale, digits=2)
#   fcf[i.ch] <- liq8[[i.ch]][1,1]
#   current[i.ch] <- sum(val8.nom[[i.ch]][3:4,2]) # should be the sum of lines 3 and 4
#   if ( !(i %% 100) ) 
#   {
#     print(paste0("Data saved, i = ", i))
#     save(savings.delta, role, liq8, inc8.nom, val8.nom, fcf, current, file="HA2_8.rda")
#   }
# }
# })
# save(savings.delta, role, liq8, inc8.nom, val8.nom, fcf, current, file="HA2_8.rda")

length(savings.delta)
length(role)
length(liq8)
length(inc8.nom)
length(val8.nom)
length(fcf)
length(current)

# load("HA2_8.rda")
res <- cbind(-round(savings.delta,2), fcf, current)
colnames(res) <- c("d-savings", "free cf", "liq. assets")
res
dim(res)
hist(res[,1], xlab="delta savings") # fluctuation of savings
hist(res[,2], xlab="free cash flow") # free cash flow 
hist(res[,3], xlab="liquid assets") # liquid assets
# Computation of VaR and Expected Shortfall
quantile(res[,3], probs = c(0.001, 0.01, 0.05), na.rm = FALSE,
         names = TRUE, type = 7, digits = 7)
abline(v=quantile(res[,3], probs = c(0.001, 0.01, 0.05), na.rm = FALSE),
       col=c("red","blue", "green"), lty=1)
# Expected shortfall
ES <- numeric(3)
ES[1] <- mean(sort(res[,3])[1:5])
ES[2] <- mean(sort(res[,3])[1:50])
ES[3] <- mean(sort(res[,3])[1:250])
ES
abline(v=ES, col=c("red","blue", "green"), lty=2)

# Theoretical:

# Liquid assets: 
# Without recovery:
liq.assets <- 109
liq.assets <- 122
# With recovery:
liq.assets <- 149

fct <- function(x) {
  p <- pnorm(x, mean=liq.assets, sd=50 )
  x*p
}

# VaR 99
qnorm(0.01, mean=liq.assets, sd=50)
# ES 99
integrate(fct, -Inf, qnorm(0.01, mean=liq.assets, sd=50))$value /
  integrate(pnorm, -Inf, qnorm(0.01, mean=liq.assets, sd=50), mean=liq.assets, sd=50)$value
# Prob. of illiquidity
uniroot(qnorm, c(0,0.2), mean=liq.assets, sd=50, tol=1.e-8)

## Problem 9 ------------------------
# We simulate the default rates as being uniformly distributed between 0 and 8%
# and accordingly adjust the volume of the contracts that simulate the default


# construction of time bucket over 10 years
by <- timeSequence(t0, by="1 years", length.out=11)
(tb <- timeBuckets(by, bucketLabs=2021:2030, 
                   breakLabs=substr(as.character(by),3,10)))

# Computation for 4% default rate of the remaining assets
default.rate <- rep(0.04,10)
default.rate.cum <- default.rate*c(1,cumprod(1-default.rate)[-10])
recovery <- 0.75
eff.loss.rate <- (1-recovery)*default.rate.cum

for(j in 1:2) {
  i <- j
  bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["NotionalPrincipal"]] <- 
    round(default.rate.cum[j]*mortgage.value,0)
  bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["PremiumDiscountAtIED"]] <- 
    -round(eff.loss.rate[j]*mortgage.value,0)
  
  print(paste(i, j, 
              bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["ContractID"]],
              bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["NotionalPrincipal"]],
              bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["PremiumDiscountAtIED"]]
  ))
}

for (j in 1:5) {
  i <- j+2
  bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["NotionalPrincipal"]] <- 
    round(default.rate.cum[j]*mortgage.value,0)
  bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["PremiumDiscountAtIED"]] <- 
    -round(eff.loss.rate[j]*mortgage.value,0)
  print(paste(i, j, 
              bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["ContractID"]],
              bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["NotionalPrincipal"]],
              bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["PremiumDiscountAtIED"]]
  ))
}
for (j in 1:10) {
  i <- j+7
  bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["NotionalPrincipal"]] <- 
    round(default.rate.cum[j]*mortgage.value,0)
  bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["PremiumDiscountAtIED"]] <- 
    -round(eff.loss.rate[j]*mortgage.value,0)
  print(paste(i, j, 
              bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["ContractID"]],
              bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["NotionalPrincipal"]],
              bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["PremiumDiscountAtIED"]]
  ))
}

system.time({events(bank, t0, rf, end_date="2032-01-01")})
# showEvents(bank)

# -------- Initialisation of loop
# Don't execute if simulation is continued
{ 
  liq9 <- list()
  inc9.nom <- list()
  val9.nom <- list()
  liq9[["0"]] <- liquidity(bank, tb, scale=scale, digits=2)
  inc9.nom[["0"]] <- income(bank, tb, scale=scale, digits=2)
  val9.nom[["0"]] <- value(bank, tb, scale=scale, digits=2)
  eq9 <- numeric()
  eq9["0"] <- val9.nom[["0"]][12,10]
  def.rates <- default.rate
  dim(def.rates) <- c(1,10)
  rownames(def.rates) <- "0"
  colnames(def.rates) <- 1:10
}
# End of initialisation
# --------

# loop over random defaults rates 
simLen <- 1000 
# ns <- 1
# If the simulation should be continued:
load("examples/HA2_9.rda")
# (ns <- dim(def.rates)[1])
# (nn <- ns + simLen - 1)
# system.time({
# for (ctr in ns:nn) {
#   ctr.ch <- as.character(ctr)
# 
#   default.rate <- runif(10, 0, 0.08) 
#   default.rate.cum <- default.rate*c(1,cumprod(1-default.rate)[-10])
#   recovery <- 0.75
#   eff.loss.rate <- (1-recovery)*default.rate.cum
#   def.rates <- rbind(def.rates, default.rate)
#   rownames(def.rates)[ctr+1] <- ctr.ch
# 
#   print(paste(ctr, paste(default.rate.cum, collapse=" "), sep=" "))
#     
#   for(j in 1:2) {
#     i <- j
#     bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["NotionalPrincipal"]] <- 
#       round(default.rate.cum[j]*mortgage.value,0)
#     bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["PremiumDiscountAtIED"]] <- 
#       -round(eff.loss.rate[j]*mortgage.value,0)
#   }
#   
#   for (j in 1:5) {
#     i <- j+2
#     bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["NotionalPrincipal"]] <- 
#       round(default.rate.cum[j]*mortgage.value,0)
#     bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["PremiumDiscountAtIED"]] <- 
#       -round(eff.loss.rate[j]*mortgage.value,0)
#   }
#   for (j in 1:10) {
#     i <- j+7
#     bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["NotionalPrincipal"]] <- 
#       round(default.rate.cum[j]*mortgage.value,0)
#     bank$Assets$LT.default$contracts[[i]][["ContractTerms"]][["PremiumDiscountAtIED"]] <- 
#       -round(eff.loss.rate[j]*mortgage.value,0)
#   }
#   
#   system.time({events(bank, t0, rf, end_date="2032-01-01")})
#   
#   liq9[[ctr.ch]] <- liquidity(bank, tb, scale=scale, digits=2)
#   inc9.nom[[ctr.ch]] <- income(bank, tb, scale=scale, digits=2)
#   val9.nom[[ctr.ch]] <- value(bank, tb, scale=scale, digits=2)
#   eq9[ctr.ch] <- val9.nom[[ctr.ch]][12,10]
# 
#   if ( !(ctr %% 100) ) 
#   {
#     print(paste0("Data saved, count = ", ctr))
#     save(def.rates, liq9, inc9.nom, val9.nom, eq9, file="HA2_9.rda")
#   }
#   
# }
# })

dim(def.rates)
length(liq9)
length(inc9.nom)
length(val9.nom)
length(eq9)

def.rates[371,]
def.rates <- def.rates[-371,]

dimnames(def.rates)
length(eq9)
val9.nom[370]
inc9.nom[370]
eq9[370]

# save(def.rates, liq9, inc9.nom, val9.nom, eq9, file="HA2_9.rda")
load("examples/HA2_9.rda")

# Distribution of equity
res <- cbind(
  apply(def.rates, 1,  function(x) 1-prod(1-x)),
  -eq9
)
colnames(res) <- c("total def", "equity")
# The second line shows equity. It should not be negative.
res[order(res[,2]),]

# histogram for equity
hist(res[,2], xlab = "Equity")
# Liquidity cannot really be investigated with this model.
# We would need a rule of how to reinvest the recovered money

# Surviving mortgages
yy <- apply(def.rates, 1, function(x) cumprod(1-x)[10])
hist(yy)
yy10 <- apply(def.rates, 1, function(x) cumprod(1-x)[10])
yy5 <- apply(def.rates, 1, function(x) cumprod(1-x)[5])
yy2 <- apply(def.rates, 1, function(x) cumprod(1-x)[2])
hist(300*yy10, xlab="Surviving 10 year mortgages volume")
hist(300*yy5, xlab="Surviving 5 year mortgages volume")
hist(300*yy2, xlab="Surviving 2 year mortgages volume")
# hist(3-yy10-yy5-yy2)
hist(300*(yy10+yy5+yy2), xlab="Non-defaulted mortgages volume")

# In order to simulation equity in 10 years we must take into account the 
# accumulated profit as well
eq.sample <- sapply(val9.nom, function(x) x[12,10], simplify=TRUE)
hist(-eq.sample, xlab="Equity in 10 years")

# Computation of VaR and Expected Shortfall
quantile(-eq.sample, probs = c(0.001, 0.01, 0.05), na.rm = FALSE,
         names = TRUE, type = 7, digits = 7)
abline(v=quantile(-eq.sample, probs = c(0.001, 0.01, 0.05), na.rm = FALSE),
       col=c("red","blue", "green"), lty=1)
# Expected shortfall
ES <- numeric(3)
ES[1] <- mean(sort(-eq.sample)[1:5])
ES[2] <- mean(sort(-eq.sample)[1:50])
ES[3] <- mean(sort(-eq.sample)[1:250])
abline(v=ES, col=c("red","blue", "green"), lty=2)

