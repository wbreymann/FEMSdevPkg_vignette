## ---- Preparation ------------
options(warn=-1)
options(width=100) 
# Lade nötige Pakete
library(FEMS)
# clean up
rm(list=ls())


## ----------- Task 1 -----------

t0 <- "2020-01-01"   # analysis date

ir <- 0.03
ir.spread <- 0.01
yc <- YieldCurve(label = "YC.CHF", 
                 ReferenceDate = "2019-12-31", 
                 Tenors = c("1W", "20Y"), Rates = c(0.03, 0.03))
rf <- RFConn(yc)

Bank <- institution(" Simple Bank")  # Analysis structure
Bank$RemoveChild("Operations")       # No operations 
Bank$Assets$RemoveChild("ShortTerm") # No contracts present 
Bank 

# generate stochastic volume
nom.assets <- floor(rnorm(10,1000,20))
nom.liab <- floor(rnorm(10,1000,20))

mortgages <- list()
termdeposits <- list()
for (i in 1:10) {
mat <- paste0(i, "years")
mortgages[[i]] <- bond(ContractID = paste0("m",i),
  start = "2020-04-01", maturity = mat, nominal = nom.assets[i], 
  coupon = ir+ir.spread, role="long") # notice the spread
termdeposits[[i]] <- bond(ContractID = paste0("td",i),
  start = "2020-04-01", maturity = mat, nominal = nom.liab[i], 
  coupon = ir, role="short")
}

addContracts(termdeposits, FindNode(Bank, "Debt"))
addContracts(mortgages, FindNode(Bank, "LongTerm"))


## ----------- Task 2 --------------
events(Bank, t0, rf, end_date = "2030-12-31")

# Time buckets
by <- timeSequence(t0, by = "1 year", length.out=11)
years <- as.character(2020:2029)
tb <- timeBuckets (by, bucketLabs = years, 
                   breakLabs=paste0("1.1.",20:30))

# value
val <- value(Bank, tb, type = "nominal")
val[,-1]
liquidity(Bank, by = tb, type = "marginal", digits = 0)
income(Bank, by = tb, type = "marginal", revaluation.gains = FALSE, digits = 0)

## ------------Task 3: Plotting the time evolution of value -----
library(ggplot2)
data <- data.frame(
  Date=colnames(val)[-1],
  Type=rep("Assets",10),
  Value=as.numeric(val[2,-1])
)

data <- rbind(data,
              data.frame(
                Date=colnames(val)[-1],
                Type=rep("Debt",10),
                Value=as.numeric(val[6,-1])
              ))

ggplot(data, aes(fill=Type, y=Value, x=Date), col=colors) + 
  geom_bar(position="stack", stat="identity")


## ----------- Task 4: Going concern simulation with no-growth strategy --------
# Tempalte contracts
Debt.template <- Bank$Liabilities$Debt$contracts[[3]]
LongTerm.template <- Bank$Assets$LongTerm$contracts[[3]]
templates <- list(Debt = Debt.template,
                  LongTerm = LongTerm.template)

# strategy
strategy <- diag(c(1,1))
colnames(strategy) <- c("LongTerm", "Debt")
rownames(strategy) <- colnames(strategy)
strategy

# Time steps for simulation: We start 1 day after the creation of the contracts
t1 <- "2020-04-02"
(by <- timeSequence(t1, by = "1 year", length.out = 10))
# Simulate new business 
newcts <- newbiz(Bank, by, strategy, templates, rf)
Bank.gc <- add.model(Bank, newcts)
events(Bank.gc, t0, rf, end_date = "2030-12-31")

# Listing the newly created contracts
ctr.new <- get(newcts,"contracts")
ctr.pretty <- data.frame()
tmp <- ctr.new$LongTerm
for (i in 1:9) {
  ctr.pretty <- rbind(ctr.pretty,
                      CTterms(tmp[[i]]))
}
tmp <- ctr.new$Debt
for (i in 1:9) {
  ctr.pretty <- rbind(ctr.pretty,
                      CTterms(tmp[[i]]))
}
colnames(ctr.pretty) <- c("ID", "Type", "Role", "IED", "Maturity", "Notional", "I.R.")
ctr.pretty[,c(-2,-7)]

# value
val.gc <- value(Bank.gc, tb, type = "nominal")
val.gc[,-1]

# plot of value
data.gc <-
  data.frame(
    Date=colnames(val.gc)[-1],
    Type=rep("Assets new",10),
    Value=as.numeric(val.gc[2,-1]-val[2,-1])
  )

data.gc <- rbind(data.gc,
                 data.frame(
                   Date=colnames(val)[-1],
                   Type=rep("Assets old",10),
                   Value=as.numeric(val[2,-1])
                 ))

data.gc <- rbind(data.gc,
                 data.frame(
                   Date=colnames(val)[-1],
                   Type=rep("Debt old",10),
                   Value=as.numeric(val[6,-1])
                 ))

data.gc <- rbind(
  data.gc,
  data.frame(
    Date=colnames(val.gc)[-1],
    Type=rep("Debt new",10),
    Value=as.numeric(val.gc[6,-1]-val[6,-1])
  ))

ggplot(data.gc, aes(fill=Type, y=Value, x=Date)) + 
  geom_bar(position="stack", stat="identity")

## ------------Task 5: Repeat task 4 with 10% yearly growth --------------------
strategy2 <- diag(c(1.1,1.1))
colnames(strategy2) <- c("LongTerm", "Debt")
rownames(strategy2) <- colnames(strategy2)
strategy2

# Simulate 
newcts2 <- newbiz(Bank, by, strategy2, templates, rf)
Bank.gc2 <- add.model(Bank, newcts2)
events(Bank.gc2, t0, rf, end_date = "2030-12-31")

# The new contracts
ctr.new <- get(newcts2,"contracts")
ctr.pretty <- data.frame()
tmp <- ctr.new$LongTerm
for (i in 1:9) {
  ctr.pretty <- rbind(ctr.pretty,
                      CTterms(tmp[[i]]))
}
tmp <- ctr.new$Debt
for (i in 1:9) {
  ctr.pretty <- rbind(ctr.pretty,
                      CTterms(tmp[[i]]))
}
colnames(ctr.pretty) <- c("ID", "Type", "Role", "IED", "Maturity", "Notional", "I.R.")
ctr.pretty[,6] <- floor(as.numeric(ctr.pretty[,6]))
ctr.pretty[,c(-2,-7)]

# value
val.gc2 <- value(Bank.gc2, tb, type = "nominal")
val.gc2[,-1]

# plot of value
data.gc <-
  data.frame(
    Date=colnames(val.gc2)[-1],
    Type=rep("Assets new",10),
    Value=as.numeric(val.gc2[2,-1]-val[2,-1])
  )

data.gc <- rbind(data.gc,
                 data.frame(
                   Date=colnames(val)[-1],
                   Type=rep("Assets old",10),
                   Value=as.numeric(val[2,-1])
                 ))

data.gc <- rbind(data.gc,
                 data.frame(
                   Date=colnames(val)[-1],
                   Type=rep("Debt old",10),
                   Value=as.numeric(val[6,-1])
                 ))

data.gc <- rbind(
  data.gc,
  data.frame(
    Date=colnames(val.gc2)[-1],
    Type=rep("Debt new",10),
    Value=as.numeric(val.gc2[6,-1]-val[6,-1])
  ))

ggplot(data.gc, aes(fill=Type, y=Value, x=Date )) + 
  geom_bar(position="stack", stat="identity")

# Growth of debt
tmp <- as.numeric(val.gc2[6,-1])
100 * (tmp[-1]/tmp[-10]-1)# Growth of asset side
# Growth of assets
tmp <- as.numeric(val.gc2[2,-1])
100 * (tmp[-1]/tmp[-10] - 1)
# The assets grow faster because of the interest rate spread 
# which creates a profit for the bank.
