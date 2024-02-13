################################################################################
# Beginn des Lösungsskripts
################################################################################


#---------------------------------------------------
# Exercise  1 
#---------------------------------------------------

# Deaktiviere Warnungen temporär
options(warn=-1)

# Lade nötige Pakete
library(FEMS)

# Definiere Analysezeitpunkt
t0 <- "2018-01-02"	

P0.gas <- 0.0486
P0.el <- 0.1801
deltaP.gas <- -0.0004
deltaP.el <- 0.0219

# Write function for generating simulated price series
p.sim <- function(n, P.0, t.0, by="1 year", deltaP) {
return(timeSeries(
  data = P.0 + (0:n)*deltaP, # linear function
  timeSequence(t.0, by=by, length.out=n+1)
))
}

# Generate gas prices
gas.p <- p.sim(10, P0.gas, t0, deltaP=deltaP.gas)

# Generate index object as risk factor
gas.idx <- Index(data=gas.p, label = "GAS")

# The same for electricity
el.p <- p.sim(10, P0.el, t0, deltaP=deltaP.el)
el.idx <- Index(data=el.p, label="EL")

# Plot forecasted gas and electricity prices
plot(gas.p,ylim=c(0,0.4),ylab="Commodity price [EUR/kWh]")
lines(el.p, lty=1, col=2)
legend("topleft", legend=c("Gas", "El."), lty=1, col=c("black","red"))

# Yield curve
yc.tnr <- c("1M","10Y")
yc.rts <- c(0.02,0.02)
yc <- YieldCurve(label = "MARKTZINS", 
             ReferenceDate = as.character(t0), 
             Tenors = yc.tnr, Rates = yc.rts)

# Generate market environment
(rf <- RFConn(list(gas.idx, el.idx, yc)))

# Generate "discounting engine"
diskont <- DcEngine(dc.spread=0.0, dc.object=yc)
set(diskont, rf)

#----------------------------------------
# Modelling of the power plant
# Costs of building the plant
investition.nominal <- 100000000

# Model loan
Credit <- loan(
start = "2018-01-03",
maturity = "2028-01-02",
nominal = investition.nominal,
ir = 0.02, # Marktzinsniveau per t0
ContractID = "Credit01",
Currency = "EUR",
role = "short" # Alternative Namen: Passiva (Liabilities)
)

# Operations Zeitachse
ops.times <- timeSequence(from = timeDate("2018-01-03"), by = "1 years", length.out = 11)

# Define time pattern of depreciation
inv.func <- function(times) {
timeSeries(seq(investition.nominal, 0, length.out=11), times)
}
# Define Investments contract
inv <- Investments(pattern=inv.func, 
                 args=list(times=ops.times
                           ), Currency="EUR", ContractID="Invest01")

# Definiere Gas-Einkaufs-Pattern
gas.func <- function(model) {
timeSeries(-24*300*100*1000*(1/0.4)*as.timeSeries.RiskFactor(model[["GAS"]]),
           ops.times)
}

# Define contract for operational cash flows
gas <- OperationalCF(ContractID="Gas01", Currency="EUR", pattern=gas.func,
                   args=list(model=rf) )

# Define pattern for the sales of electricity
el.func <- function(model) {
timeSeries(24*300*100*1000*as.timeSeries.RiskFactor(model[["EL"]]),
           ops.times)
}
# Define the related contract for the operational cash flows
el <- OperationalCF(ContractID="El01", Currency="EUR", pattern=el.func, args=list(model=rf))

# Create institution
# PowerPlant <- institution("PowerPlant", cashcollect=FALSE)
PowerPlant <- institution("PowerPlant", cashcollect=TRUE)
PowerPlant$Assets$AddChild("LongTerm")
PowerPlant
# Add contracts to model structue
addContracts(list(Loan=Credit), FindNode(PowerPlant, "Debt"))
addContracts(list(Invest=inv), FindNode(PowerPlant, "LongTerm"))
addContracts(list(Revenue=el), FindNode(PowerPlant, "Revenues"))
addContracts(list(Expense=gas), FindNode(PowerPlant, "Expenses"))

PowerPlant
PowerPlant$Assets$LongTerm$contracts
PowerPlant$Liabilities$Debt$contracts
PowerPlant$Assets$Current$contracts

events(PowerPlant, t0, rf, end_date=tail(ops.times,1))

# Show event list for single accounts:
print(PowerPlant$Operations$Revenues$eventList)
print(PowerPlant$Operations$Expenses$eventList)
print(PowerPlant$Assets$LongTerm$eventList)
print(PowerPlant$Liabilities$Debt$eventList)

# tmp<- PowerPlant$Operations$Revenues$eventList
# class(tmp)
# names(tmp)
# tmp$El01
# tmp$El01$evs


# The effect of different contract events on financial statements:
# Accounting effects of different event types:
# Event     | liquidity  |  income   | balance
#           |            |           |  sheet
# AD0       |            |           |
# DPR       |            |    x      |   x
# IED       |     x      |           |   x
# IP        |     x      |    x      |
# PR        |     x      |           |   x
# MD        |     x      |           |   x
# OPS       |     x      |    x      |    

# Define time buckets
by <- timeSequence(t0, by="1 years", length.out=6)

tb <- timeBuckets(by, bucketLabs=2018:2022, breakLabs=substr(as.character(by),3,10))  
tb

# Units are in Mio EUR:
scale <- 1000000
# Liquidity, marginal (in MioEUR) 
round(liquidity(PowerPlant, tb, "marginal")/scale,2)

round(liquidity(PowerPlant, tb, "cumulative")/scale,2)

# Income (in MioEUR)
round(income(PowerPlant, tb, type="marginal", revaluation.gains=FALSE)/scale,2)

# Nominalwert (in MioEUR)
round(value(PowerPlant, tb, "nominal")/scale,2)

# market valuation
round(value(PowerPlant, tb, "market", method=diskont)/scale,2)

################################################################################
# This is not part of the solution
liq <- liquidity(PowerPlant, tb, "marginal")
inc <- income(PowerPlant, tb, type="marginal", revaluation.gains=FALSE)
val.nom <- value(PowerPlant, tb, "nominal")
val.npv <- value(PowerPlant, tb, "market", method=diskont)

# Income = Delta value
cumsum(as.numeric(inc[1,]))+as.numeric(val.nom[8,-1])

tmp <- timeSeries(PowerPlant$Operations$Revenues$eventList$El01$evs[,2],
           timeDate(PowerPlant$Operations$Revenues$eventList$El01$evs[,1]))
presentValue(tmp,2)/1.e6
presentValue(tmp[-(1:2),],2)
tmp

