# R-script for homework 2
#-------------------------------------------------------------------------------
# Model of the bank of practice sheet 9 in FEMS.
# This is the basis in order to compute problems 1 to 5 of homework 2
# The script contains comments that should allow you to understand what is done.
# An additional script for the further tasks will follow.

## Preparation --------------
library(FEMS)
options(warnings=-1)
rm(list=ls())

## Initialization -----------
t0 <- "2021-01-01"
# Start date of the contracts. Should already exist at analysis date
tstart <- "2020-12-31" # 
# scale for the amount of capital (in Mio CHF)
scale <- 1000000

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
                      CycleOfRateReset="P3YL0", MarketObjectCodeOfRateReset="YC")
addContracts(list(assets.liquid), FindNode(bank$Assets, "ShortTerm"))

# events(assets.liquid, t0, rf)

# Long term assets ---------
ir.spread.assets <- 0.03
mortgage.value <- 300*scale
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

# plot(mortgage.long1, tstart)

# Modeling of default ----------
# We add contracts with a negative value on the asset side by chosing 'role="short"'
# For each mortgage, we must add every year a contract that has 1% of its value.
# (4% default rate with 75% recovery rate and assumption that recovery happens immediately)
times.default <- timeSequence(from = timeDate(tstart), by = "1 years", length.out = 26)
default.rate <-0.04
recovery <- 0.75
p.disc <- (1-recovery)*default.rate
 
mortgage.long1.df1 <- bond(start = as.character(times.default[2]), maturity = "1 years", 
                       nominal = default.rate*mortgage.value, 
                       PremiumDiscountAtIED=-p.disc*mortgage.value,
                       coupon = spot.rates[2]+ir.spread.assets, 
                       role="short", ContractID="mg1-df1")
# events(mortgage.long1.df1, t0)
mortgage.long1.df2 <- bond(start = "2022-12-30", maturity = "1 day", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[2]+ir.spread.assets, 
                           role="short", ContractID="mg1-df2")
events(mortgage.long1.df2, t0)
mortgage.long2.df1 <- bond(start = as.character(times.default[2]), maturity = "4 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[3]+ir.spread.assets, 
                           role="short", ContractID="mg2-df1")
mortgage.long2.df2 <- bond(start = as.character(times.default[3]), maturity = "3 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[3]+ir.spread.assets, 
                           role="short", ContractID="mg2-df2")
mortgage.long2.df3 <- bond(start = as.character(times.default[4]), maturity = "2 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[3]+ir.spread.assets, 
                           role="short", ContractID="mg2-df3")
mortgage.long2.df4 <- bond(start = as.character(times.default[5]), maturity = "1 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[3]+ir.spread.assets, 
                           role="short", ContractID="mg2-df4")
mortgage.long2.df5 <- bond(start = "2025-12-30", maturity = "1 day", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[3]+ir.spread.assets, 
                           role="short", ContractID="mg2-df5")

mortgage.long3.df1 <- bond(start = as.character(times.default[2]), maturity = "9 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df1")
mortgage.long3.df2 <- bond(start = as.character(times.default[3]), maturity = "8 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df2")
mortgage.long3.df3 <- bond(start = as.character(times.default[4]), maturity = "7 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df3")
mortgage.long3.df4 <- bond(start = as.character(times.default[5]), maturity = "6 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df4")
mortgage.long3.df5 <- bond(start = as.character(times.default[6]), maturity = "5 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df5")
mortgage.long3.df6 <- bond(start = as.character(times.default[7]), maturity = "4 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df6")
mortgage.long3.df7 <- bond(start = as.character(times.default[8]), maturity = "3 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df7")
mortgage.long3.df8 <- bond(start = as.character(times.default[9]), maturity = "2 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df8")
mortgage.long3.df9 <- bond(start = as.character(times.default[10]), maturity = "1 years", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df9")
mortgage.long3.df10 <- bond(start = "2030-12-30", maturity = "1 day", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-p.disc*mortgage.value,
                           coupon = spot.rates[4]+ir.spread.assets, 
                           role="short", ContractID="mg3-df10")

addContracts(list(
  mortgage.long1.df1,mortgage.long1.df2,
  mortgage.long2.df1,mortgage.long2.df2,mortgage.long2.df3,mortgage.long2.df4,mortgage.long2.df5,
  mortgage.long3.df1,mortgage.long3.df2,mortgage.long3.df3,mortgage.long3.df4,mortgage.long3.df5,
  mortgage.long3.df6,mortgage.long3.df7,mortgage.long3.df8,mortgage.long3.df9,mortgage.long3.df10
), FindNode(bank$Assets, "LT.default"))

# events(mortgage.long1.df2, t0)

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

# Define Investments contract
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
# events(Rent, t0, rf)

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
# events(Commissions, t0, rf)
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

showEvents(bank)

### Analysis -------------------
# construction of time bucket
# t2 = "2021-01-02"
by <- timeSequence(t0, by="1 years", length.out=6)
(tb <- timeBuckets(by, bucketLabs=2021:2025, 
                  breakLabs=substr(as.character(by),3,10)))


(val.nom <- value(bank, tb, scale=scale, digits=2)[,1:2])
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

bank$Operations$Revenues$Rent$eventList

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



