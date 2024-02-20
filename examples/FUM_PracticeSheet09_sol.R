################################################################################
# Beginn des Lösungsskripts
################################################################################


#---------------------------------------------------
# Exercise  1 
#---------------------------------------------------

## ----echo=FALSE,results='hide',message=FALSE------------------------------------------------------------------------------------
library(FEMS)
options(warn=-1)
options(width=60)  #  width of console


## -------------------------------------------------------------------------------------------------------------------------------
# Analysis date:
t0 <- "2016-01-02"
# Create portfolio and import cntracts
ptf <- Portfolio()
ptf.tbl <- read.csv("./R/BankBilanzPositionen.csv", 
                    header = TRUE)
import(ptf,source = ptf.tbl)
# Extract attributes for further usage
pars <- ptf.tbl[, c( "BilanzKonto", "ContractID")]



## -------------------------------------------------------------------------------------------------------------------------------
Bank <- institution("BankA")
temp.node <- FindNode(Bank,"LongTerm")
temp.node$name <- "FixeDarlehen"
Bank$Assets$AddChild("VariableDarlehen")
Bank$Liabilities$AddChild("Interbank")
Bank$Liabilities$AddChild("Kundenkonten")


## -------------------------------------------------------------------------------------------------------------------------------
fd.id <- subset(pars, BilanzKonto == "FixeDarlehen")$ContractID
addContracts(get(ptf, fd.id), FindNode(Bank$Assets, "FixeDarlehen"))
vd.id <- subset(pars, BilanzKonto == "VariableDarlehen")$ContractID
addContracts(get(ptf, vd.id), FindNode(Bank$Assets, "VariableDarlehen"))
ib.id <- subset(pars, BilanzKonto == "Interbank")$ContractID
addContracts(get(ptf, ib.id), FindNode(Bank$Liabilities, "Interbank"))
kk.id <- subset(pars, BilanzKonto == "Kundenkonti")$ContractID
addContracts(get(ptf, kk.id), FindNode(Bank$Liabilities, "Kundenkonten"))

## -------------------------------------------------------------------------------------------------------------------------------
# Extract label for yield cuve
obj <- unique(ptf.tbl$MarketObjectCodeOfRateReset)
obj <- obj[which(obj != "NULL")] # "NULL" stands for "undefined"
obj
# Yield curve
tenors <- c("3M", "1Y", "2Y", "5Y", "7Y", "10Y")
rates <- c(-0.28, -0.26, -0.21, 0.03, 0.20, 0.42)/100
yc <- YieldCurve(label = obj, ReferenceDate = t0, 
                 Tenors = tenors, Rates = rates)
# Market environment
rf <- RFConn(yc)
set(ptf, rf) # Connect to portfolio
# Define discounting method
eng <- DcEngine(dc.spread = 0.0, dc.object = yc)
set(eng, rf)

## -------------------------------------------------------------------------------------------------------------------------------
by <- timeSequence(t0, by = "1 year", length.out=6)
years <- as.character(2016:2020)
tb <- timeBuckets (by, bucketLabs = years, 
        breakLabs=c("16-01-02", "17-01-02", "18-01-02", "19-01-02", "20-01-02", 
                     "21-01-02"))
events(Bank, t0, rf, end_date = "2025-12-31")


## -------------------------------------------------------------------------------------------------------------------------------
options(width=100)  #  width of console
value(Bank, tb, type = "nominal")


## -------------------------------------------------------------------------------------------------------------------------------
value(Bank, tb, type = "market", method=eng)


## -------------------------------------------------------------------------------------------------------------------------------
liquidity(Bank, by = tb, type = "marginal", digits = 0)


## -------------------------------------------------------------------------------------------------------------------------------
income(Bank, by = tb, type = "marginal", revaluation.gains = FALSE, 
       digits = 0)


mc.simulation <- function(bank, t0, end_date, rf, scenarios, by)
{
  yc <- rf[[get(rf, "keys")[1]]]
  rates.old <- yc[["Rates"]] # Store original rates
  results <- list() # empty results list
  # Start with Scenario 0
  events(bank, t0, rf, end_date = end_date)
  value.nom <- value(bank, by, "nominal", digits=0)
  value.npv <- value(bank, by, type = "market", method=eng, digits=0)
  liq <- liquidity(bank, by = tb, type = "marginal", digits = 0)
  inc <- income(bank, by = tb, type = "marginal", revaluation.gains = FALSE, digits = 0)
  results[[1]] <- list(value.nom = value.nom, value.npv = value.npv, 
                         liquidity = liq, income = inc)
  print ("Start MC loop")
  for(i in 1:nrow(scenarios)) { # Loop over yield curve scenarios
    print (paste("MC loop no.",i))
    yc[["Rates"]] <- as.numeric(rates.old + scenarios[i,]) # Interest rate shock
    events(bank, t0, rf, end_date = end_date)
    value.nom <- value(bank, by, "nominal", digits = 0)
    value.npv <- value(bank, by, type = "market", method=eng, digits = 0)
    liq <- liquidity(bank, by = tb, type = "marginal", digits = 0)
    inc <- income(bank, by = tb, type = "marginal", revaluation.gains = FALSE, digits = 0)
    results[[i+1]] <- list(value.nom = value.nom, value.npv = value.npv, 
                           liquidity = liq, income = inc)
    yc[["Rates"]] <- rates.old  # restore old rates
  }
  return(results)
}

## -------------------------------------------------------------------------------------------------------------------------------
# load yield curve scenarios
mc.scenarios = read.table("./R/ZinsSzenarien.csv", sep=";", header=TRUE)
head(mc.scenarios)
# Drop columns with meta information
mc.scenarios = mc.scenarios[,-c(1,2)]
# Only use the first n lines for the MC simulation
n <- 2
results <- mc.simulation(Bank, t0, end_date = "2025-12-31",
                           rf, scenarios=mc.scenarios[1:n,], tb)


load(file="./R/StaticBankMC_Results.RData")
results <- resultate
length(results)
names(results[[1]])

mc.rates <- rbind(rep(0,6), mc.scenarios)
rownames(mc.rates) <- c("0", rownames(mc.scenarios))
scenarios.summary <- rbind(
summary(100*mc.rates[,1]),
summary(100*mc.rates[,2]),
summary(100*mc.rates[,3]),
summary(100*mc.rates[,4]),
summary(100*mc.rates[,5]),
summary(100*mc.rates[,6])
)
rownames(scenarios.summary) <- c("3m", "1y", "2yrs", "5yrs", "7yrs", "10yrs")

scenarios.summary

Bank

equity.nom <- -sapply(resultate, function(x) as.numeric(x$value.nom[9,]))
rownames(equity.nom) <- colnames(resultate[[1]]$value.nom)
dim(equity.nom)
equity.nom[,1:10]


## -------------------------------------------------------------------------------------------------------------------------------
equity.npv <- -sapply(results, function(x) as.numeric(x$value.npv[9,]))
rownames(equity.npv) <- rownames(equity.nom)
equity.npv[,1:10]


## ----eval=FALSE-----------------------------------------------------------------------------------------------------------------
## dd <- "17-01-02"
## par(mfrow=c(1,2))
## hist(as.numeric(equity.nom[dd,]), main="Nominal", xlab="Value")
## hist(as.numeric(equity.npv[dd,]), main="Market Oriented", xlab="Value")


## -------------------------------------------------------------------------------------------------------------------------------
alpha <- 0.95
equity.nom.mean <- rowMeans(equity.nom)
equity.nom.var <- apply(equity.nom, 1, quantile, probs=1-alpha)
equity.nom.es <- equity.nom.var
for (i in 1:6) {
  equity.nom.es[i] <- mean(equity.nom[i, equity.nom[i,]<=equity.nom.var[i]])
}
cbind(equity.nom.mean, equity.nom.var,equity.nom.es)


## -------------------------------------------------------------------------------------------------------------------------------
equity.npv.mean <- rowMeans(equity.npv)
equity.npv.var <- apply(equity.npv, 1, quantile, probs=1-alpha)
equity.npv.es <- equity.npv.var
for (i in 1:6) {
  equity.npv.es[i] <- mean(equity.npv[i, equity.npv[i,]<=equity.npv.var[i]])
}
cbind(equity.npv.mean, equity.npv.var,equity.npv.es)


## -------------------------------------------------------------------------------------------------------------------------------
eq.nom.list = list(  
  "16-01-02" = equity.nom[1,],
  "17-01-02" = equity.nom[2,],
  "18-01-02" = equity.nom[3,],
  "19-01-02" = equity.nom[4,],
  "20-01-02" = equity.nom[5,],
  "21-01-02" = equity.nom[6,])

## ----eval=FALSE-----------------------------------------------------------------------------------------------------------------
## boxplot(eq.nom.list, main="Boxplot of nominal equity",
##         xlab="Date",ylab="EUR")


## ----echo=FALSE,out.extra = 'height=6.5cm,width=7cm'----------------------------------------------------------------------------
par(mfrow=c(1,1))
boxplot(eq.nom.list, main="Boxplot of nominal equity",
        xlab="Date",ylab="EUR")


## ----echo=FALSE,out.extra = 'height=6.5cm,width=7cm'----------------------------------------------------------------------------
eq.npv.list = list(  
  "16-01-02" = equity.npv[1,],
  "17-01-02" = equity.npv[2,],
  "18-01-02" = equity.npv[3,],
  "19-01-02" = equity.npv[4,],
  "20-01-02" = equity.npv[5,],
  "21-01-02" = equity.npv[6,])
# boxplot(liq.list, names = as.character(2016:2024), log="y")
boxplot(eq.npv.list, main="Boxplot of market-oriented equity",
        xlab="Date",ylab="EUR")

liq <- sapply(results, function(x) as.numeric(x$liquidity[1,]))
rownames(liq) <- 2016:2020

par(mfrow=c(1,2))
hist(liq["2016",], 
  main="Liquidity 2016", xlab="EUR")
hist(colSums(liq), 
  main="Cumulative liquidity 2020", xlab="EUR")

par(mfrow=c(1,2))
hist(liq["2020",], main="Marginal liquidity 2020", xlab="EUR")
hist(colSums(liq), main="Cumulative liquidity 2020", xlab="EUR")


## -------------------------------------------------------------------------------------------------------------------------------
liq.cumul <- liq
for (i in 1:101) {
  liq.cumul[,i] <- cumsum(liq[,i])
}
liq.cumul[,1:10]


## -------------------------------------------------------------------------------------------------------------------------------
liq.cumul.mean <- rowMeans(liq.cumul)
liq.cumul.var <- apply(liq.cumul, 1, quantile, probs=1-alpha)
liq.cumul.es <- liq.cumul.var
for (i in 1:5) {
  liq.cumul.es[i] <- mean(liq.cumul[i, liq.cumul[i,]<=liq.cumul.var[i]])
}
cbind(liq.cumul.mean, liq.cumul.var, liq.cumul.es)

par(mfrow=c(1,1))
dd <- "2020"
hist(liq.cumul[dd,], main="5 year cumulative liquidity", xlab="EUR")
abline(v=liq.cumul.mean[dd],col="green")
abline(v=liq.cumul.var[dd],col="blue")
abline(v=liq.cumul.es[dd],col="red")


plot(2016:2020, liq.cumul.mean, type="b", col="green")
lines(2016:2020, liq.cumul.var, col="blue")
points(2016:2020, liq.cumul.var, col="blue")
lines(2016:2020, liq.cumul.es, col="red")
points(2016:2020, liq.cumul.es, col="red")

liq.cumul.list = list(  
  "2016" = liq.cumul["2016",],
  "2017" = liq.cumul["2017",],
  "2018" = liq.cumul["2018",],
  "2019" = liq.cumul["2019",],
  "2020" = liq.cumul["2020",])

par(mfrow=c(1,1))
boxplot(liq.cumul.list, main="Boxplot of cumulative liquidity",
        xlab="Year",ylab="EUR")

inc <- sapply(results, function(x) as.numeric(x$income[1,]))
rownames(inc) <- 2016:2020

par(mfrow=c(1,2))
hist(inc["2016",], 
  main="Income 2016", xlab="EUR")
hist(colSums(inc), 
  main="Cumulative income 2020", xlab="EUR")


par(mfrow=c(1,2))
hist(inc["2020",], main="Marginal income 2020", xlab="EUR")
hist(colSums(inc), main="Cumulative income 2020", xlab="EUR")

inc.cumul <- inc
for (i in 1:101) {
  inc.cumul[,i] <- cumsum(inc[,i])
}
inc.cumul[,1:10]

inc.cumul.mean <- rowMeans(inc.cumul)
inc.cumul.var <- apply(inc.cumul, 1, quantile, probs=1-alpha)
inc.cumul.es <- inc.cumul.var
for (i in 1:5) {
  inc.cumul.es[i] <- mean(inc.cumul[i, inc.cumul[i,]<=inc.cumul.var[i]])
}
cbind(inc.cumul.mean, inc.cumul.var, inc.cumul.es)

par(mfrow=c(1,1))
dd <- "2020"
hist(inc.cumul[dd,], main="5 year cumulative income", xlab="EUR")
abline(v=inc.cumul.mean[dd],col="green")
abline(v=inc.cumul.var[dd],col="blue")
abline(v=inc.cumul.es[dd],col="red")

plot(2016:2020, inc.cumul.mean, type="b", col="green")
lines(2016:2020, inc.cumul.var, col="blue")
points(2016:2020, inc.cumul.var, col="blue")
lines(2016:2020, inc.cumul.es, col="red")
points(2016:2020, inc.cumul.es, col="red")

inc.cumul.list = list(  
  "2016" = inc.cumul["2016",],
  "2017" = inc.cumul["2017",],
  "2018" = inc.cumul["2018",],
  "2019" = inc.cumul["2019",],
  "2020" = inc.cumul["2020",])

par(mfrow=c(1,1))
boxplot(inc.cumul.list, main="Boxplot of cumulative income",
        xlab="Year",ylab="EUR")

