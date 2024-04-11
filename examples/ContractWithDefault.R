# Contract with default
rm(list=ls())
library(FEMS)
library(lubridate)
options(warnings=-1)

tstart <- "2020-12-31" # 
t0 <- "2021-01-01"
scale <- 1000000
spot.rates <- c(0.01, 0.02, 0.03, 0.04)
yc <- YieldCurve(label = "YC", 
                 ReferenceDate = t0, 
                 Tenors = c("1M", "2Y", "5Y", "10Y"),
                 Rates = spot.rates)
yc
plot(yc)
rf <- RFConn(yc)

ir.spread.assets <- 0.03
mortgage.value <- 300*scale # 300 Mio CHF
mtg.mat = "3 years"
mortgage <- bond(start = tstart, maturity = mtg.mat,
                       nominal = mortgage.value, coupon = spot.rates[2]+ir.spread.assets, 
                       role="long", ContractID="mg1")
events(mortgage, t0)

mortgage.args <- unlist(mortgage$ContractTerms)
mortgage.args[mortgage.args!="NULL"]
do.call("Pam", args=as.list(mortgage.args[mortgage.args!="NULL"]))


one.day <- 24*3600 # 1 day in seconds
times.default <- timeSequence(from = timeDate(tstart)-one.day, by = "1 years", length.out = 26)
(times.default <- timeSequence(from = timeDate(tstart)-one.day, to = mortgage$ContractTerms$MaturityDate, 
                              by = "1 years")[-1])
default.rate <-0.04
recovery <- 0.75
eff.loss.rate <- (1-recovery)*default.rate # effective loss rate

mortgage.df1 <- bond(start = as.character(times.default[2]), maturity = "2 years", 
                           nominal = default.rate*mortgage.value, 
                           PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[2]+ir.spread.assets, 
                           role="short", ContractID="mg1-df1")
events(mortgage.df1, t0)
mortgage.df2 <- bond(start = as.character(times.default[3]), maturity = "1 year", 
                           nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                           coupon = spot.rates[2]+ir.spread.assets, 
                           role="short", ContractID="mg1-df2")
events(mortgage.df2, t0)
mortgage.df3 <- bond(start = as.character(times.default[4]), maturity = "1 day", 
                     nominal = default.rate*mortgage.value, PremiumDiscountAtIED=-eff.loss.rate*mortgage.value,
                     coupon = spot.rates[2]+ir.spread.assets, 
                     role="short", ContractID="mg1-df3")
events(mortgage.df3, t0)



BondWithDefault <- function(bond.orig, pd, rr) {
  terms <- unlist(bond.orig$ContractTerms)
  terms <- terms[terms!="NULL"]
print(terms)

  ctrList <- list()
  id.orig <- terms["ContractID"]
  ctrList[[terms["ContractID"]]] <- do.call("Pam", args=as.list(terms))
  nominal <- as.numeric(terms["NotionalPrincipal"])
  times.default <- timeSequence(from = timeDate(terms["InitialExchangeDate"])-one.day, 
                                 to = terms["MaturityDate"],
                                 by = "1 years")
  times.default <- times.default[-1]
  eff.loss.rate <- (1-rr)*pd # effective loss rate
  
  id <- terms["ContractID"]
  terms["ContractRole"] <- "RPL"
  terms["NotionalPrincipal"] <- pd*nominal
  terms["PremiumDiscountAtIED"] <- -eff.loss.rate*nominal # linear default
  id.df <- paste0(id, ".df", 1:length(times.default))
  for (i in 1:length(times.default)) {
    bond.df <- do.call("Pam", args=as.list(terms))
    bond.df$ContractTerms$InitialExchangeDate <- as.character(times.default[i])
    bond.df$ContractTerms$CycleAnchorDateOfInterestPayment <-as.character(times.default[i])
    bond.df$ContractTerms$ContractID <- id.df[i]
    ctrList[[id.df[i]]] <- bond.df
  }
  ctrList
}

mtg.with.default <- BondWithDefault(mortgage, 0.04, 0.75)

evs <- events(mtg.with.default, t0)
evs

by <- timeSequence(t0, by="1 years", length.out=6)
(tb <- timeBuckets(by, bucketLabs=2021:2025, 
                   breakLabs=substr(as.character(by),3,10)))

liquidity(mtg.with.default, tb, scale=scale, digits=2)
value(mtg.with.default, tb, type="nominal",scale=scale, digits=2)
income(mtg.with.default, tb, type="marginal", revaluation.gains=FALSE, scale=scale, digits=2)

bank <- institution("Bank")
bank
bank$Assets$RemoveChild("ShortTerm")
bank$Assets$RemoveChild("LongTerm")
bank$RemoveChild("Liabilities")
bank$RemoveChild("Operations")
bank$Assets$AddChild("Mortgages")
bank 

addContracts(mtg.with.default, bank$Assets$Mortgages)
events(bank, t0, rf, end_date="2030-01-01")
liquidity(bank, tb, scale=scale, digits=2)
value(bank, tb, type="nominal", scale=scale, digits=2)
income(bank, tb, type="marginal", revaluation.gains=FALSE, scale=scale, digits=2)
```

```R
