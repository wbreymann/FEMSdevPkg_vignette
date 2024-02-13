
library(FEMS)

rm(list=ls())
devtools::load_all()

# load the data for PAMs and ANNs into a Portfolio
file.path.pam <- "./data/PAM_FDIC.xls"
file.path.ann <- "./data/ANN_FDIC2.xls"

ptf <- Portfolio()
import(ptf, source = file.path.pam, sheet="Sheet1")
import(ptf, source = file.path.ann, sheet="Sheet1")



ptf[[3]]
terms(ptf[[3]])

# set rate reset by hand
set(object = ptf[[3]], what = list(
  CycleAnchorDateOfRateReset = "2020-03-31",
  CycleOfRateReset = "P6ML0", # 
  MarketObjectCodeOfRateReset = "YC.USA.TREASURY"))
ptf[[3]]$ContractTerms

# save(ptf, file = "SimpleBank.RData")
load("./data/SimpleBank.RData")

# arrange this into SimpleBank balance sheet structure 
# (This is not exactly the same as in the ARIADNE Example)
SimpleBank <- institution("SimpleBank")
# SimpleBank$RemoveChild("PandL")
# SimpleBank$Assets$RemoveChild("LongTerm")

SimpleBank$Assets$LongTerm$AddChild("Loan")
SimpleBank$Assets$LongTerm$AddChild("Mortgage")
SimpleBank$Liabilities$Debt$AddChild("Bonds")
SimpleBank$AddChild("Equity")

SimpleBank

# Create current account and add to "Current"
# (Where should the current account go?)
# collector <- CurrentAccount(ContractID = "Collector", 
#                             CycleOfInterestPayment = "1Y-",
#                             CycleOfRateReset = "1Y-")
# addContracts(list(collector=collector), 
#              FindNode(SimpleBank, "Current"))

# fill the accounts with different contracts
# (Which accounts go where exactly?)
addContracts(ptf$contracts[1:2], FindNode(SimpleBank, "Loan"))
addContracts(ptf$contracts[6], FindNode(SimpleBank, "Mortgage"))
addContracts(ptf$contracts[c(3,5)], FindNode(SimpleBank, "Bonds"))

SimpleBank
SimpleBank$Assets$Current$contracts
SimpleBank$Assets$LongTerm$Loan$contracts
SimpleBank$Assets$LongTerm$Mortgage$contracts
SimpleBank$Liabilities$Bonds$contracts

# (FindNode(SimpleBank, "Loan"))$isLeaf

# calculate events for the contracts
t0 <- "2020-02-28"

# just some generic Yield Curve
yc.tnr <- c("1W", "1M", "6M", "1Y", "2Y", "5Y", "10Y", "20Y")
yc.rts <- c(0.001, 0.0015, 0.002, 0.01, 0.02, 0.03, 0.045, 0.06)
yc <- YieldCurve(label = "YC.USA.TREASURY",  
                 ReferenceDate = as.character(t0), 
                 Tenors = yc.tnr, 
                 Rates = yc.rts)

# Generate market environment
rf <- RFConn(yc)

# Generate "discounting engine"
diskont <- DcEngine(dc.spread=0.0, dc.object=yc)
set(diskont, rf)


# Operational expenses: 30000/year
expenses.yearly <- rep(10000,16)
ops.times <- timeSequence(t0, by="1 years", length.out=16)
# 
expense.fun <- function(expenses) {
  timeSeries(-expenses, ops.times)
}

# Define contract for operational cash flows
Expenses <- OperationalCF(ContractID="Expenses", Currency="EUR", pattern=expense.fun,
                     args=list(expenses=expenses.yearly) )

SimpleBank$PandL$contracts <- list()
addContracts(list(Expenses), FindNode(SimpleBank, "Expenses"))

# serverURL <- "https://dadfir3-app.zhaw.ch/"
# actusURL <- "http://ractus.ch:8080/"
# FEMS:::actusURL <- "https://dadfir3-app.zhaw.ch/"
# FEMS:::actusURL

events(SimpleBank, t0, rf, end_date="2035-03-31")  

SimpleBank$Assets$Current$eventList
SimpleBank$Assets$LongTerm$Loan$eventList
SimpleBank$Assets$LongTerm$Mortgage$eventList
SimpleBank$Liabilities$Bonds$eventList
SimpleBank$PandL$eventList


by <- timeSequence(t0, by="1 years", length.out=7)

tb <- timeBuckets(by, bucketLabs=2020:2025, breakLabs=substr(as.character(by),3,10))  
tb

scale=1000
liquidity(SimpleBank, tb, scale=scale, digits=2)

# devtools::load_all()

# Income (in kEUR)
income(SimpleBank, tb, scale=scale, digits=2)

# Nominalwert (in kEUR) (is default)
# value(SimpleBank, tb, "nominal", scale=scale, digits=2)
value(SimpleBank, tb, scale=scale, digits=2)


# market valuation
# round(value(SimpleBank, tb, "market", method=diskont)/scale,2)
value(SimpleBank, tb, "market", method=diskont, scale=scale, digits=2)

####################################################

rm(list=ls())
devtools::load_all()
options(warn=-1)

yc <- YieldCurve(label = "YC_EA_AAA", 
                 ReferenceDate = "2019-12-31", 
                 Tenors = c("3M", "1Y", "2Y", "5Y", "7Y", "10Y"),
                 Rates = c(-0.28, -0.26, -0.21, 0.03, 0.20, 0.42)/100)
rf <- RFConn(yc)
rf
plot(yc)

# As zero coupon
loan3 <- bond(start = "2019-04-01", maturity = "11 years", 
              nominal = 10000, coupon = 0.0, role="long")
# Interest rate is set at IED with a spread of 150pb
set(object = loan3, what = list(RateSpread=0.015,
                                CycleAnchorDateOfRateReset = "2020-04-01",
                                CycleOfRateReset = "P9YL0", # 9 years is the max., no double digit allowed.
                                MarketObjectCodeOfRateReset = "YC_EA_AAA"))


# Volume only 92% of asseet volume
deposit3 <- bond(start = "2019-04-01", maturity = "11 years", 
                 nominal = 9200, coupon = 0.0, role="short")
set(object = deposit3, what = list(RateSpread=0.0,
                                   CycleAnchorDateOfRateReset = "2020-04-01",
                                   CycleOfRateReset = "P1YL0", # 9 years is the max., no double digit allowed.
                                   MarketObjectCodeOfRateReset = "YC_EA_AAA"))

# events(loan3, t0, rf)

MRB <- institution("Ideal Bank") # Analysis structure
# MRB$AddChild("Equity")

addContracts(list(deposit3), FindNode(MRB, "Liabilities"))
addContracts(list(loan3), FindNode(MRB, "LongTerm"))

t0 <- "2020-01-01"   # analysis date

# Operational expenses: 
# Gross profit about 0.015 * 10000 
expenses.yearly <- rep(0.018*10000*0.8, 11)
ops.times <- timeSequence(t0, by="1 years", length.out=11)
# 
expense.fun <- function(expenses) {
  timeSeries(-expenses, ops.times)
}

# expense.fun(expenses.yearly)

# Define contract for operational cash flows
Expenses <- OperationalCF(ContractID="Expenses", Currency="EUR", pattern=expense.fun,
                          args=list(expenses=expenses.yearly) )

# MRB$PandL$contracts <- list()
addContracts(list(Expenses), FindNode(MRB, "PandL"))

MRB
# The events
events(MRB, t0, rf, end_date="2031-01-01")
# The time buckets
by <- timeSequence("2019-12-31", by="1 years", length.out=6)
tb <- timeBuckets(by, bucketLabs=2020:2024, 
                  breakLabs=substr(as.character(by),3,10))  

liquidity(MRB,tb)

devtools::load_all()


income(MRB, tb)

options(width=80)
value(MRB, tb)
diskont <- DcEngine(dc.spread=0.0, dc.object=yc)
set(diskont, RFConn(yc))
value(MRB, tb, type="market", method=diskont)
income(MRB, tb, revaluation.gains = TRUE, method=diskont)

