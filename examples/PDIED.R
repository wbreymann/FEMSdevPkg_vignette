# d) Bond

library(FEMS)

b1 <- bond(start = "2020-01-01", maturity = "5 years", nominal = 10000, 
           couponFreq="3 month", coupon = 0.045, PremiumDiscountAtIED=-1000)
b1 <- PrincipalAtMaturity(InitialExchangeDate="2020-01-01", MaturityDate="2025-01-01", 
    NotionalPrincipal=10000, NominalInterestRate=0.05)
cashFlows(b1, "2020-01-01")
plot(b1, "2020-01-01")
evs <- events(b1, "2020-01-01")
print(evs, indices=c(-4,-9))

b1

# 3 month zero bond with premium discount of 200
ZeroBond <- Pam( # Principal at Maturity
  ContractID = "001",             # identifies the contract
  Currency = "CHF",               # currency in which the contract is denominated
  ContractRole = "RPA",                  # real (long) position asset
  ContractDealDate = "2018-01-01T00",    # at this date the contract is transacted
  InitialExchangeDate = "2018-01-02T00", # here starts the contract to be effective
  MaturityDate = "2018-04-02T00",        # the day of the repayment
  StatusDate       = "2018-01-01T00",    # date for which the analysis is carried out
  NotionalPrincipal = 10000,               # nominal value
  PremiumDiscountAtIED = -200,      # Discount on price
  DayCountConvention = "A365"         # convention for day counting
)
# Definiere Analysezeitpunkt
t0="2018-01-01T00"

evs <- events(ZeroBond, t0)
print(evs, indices=c(-4,-9))


# 3 year coupon bond at par with 5% coupon and semi-annual coupon payments
CouponBond <- Pam( # Principal at Maturity
  ContractID = "001",             # identifies the contract
  Currency = "CHF",               # currency in which the contract is denominated
  ContractRole = "RPA",                  # real (long) position asset
  ContractDealDate = "2018-01-01T00",    # at this date the contract is transacted
  InitialExchangeDate = "2018-01-02T00", # here starts the contract to be effective
  MaturityDate = "2021-01-02T00",        # the day of the repayment
  StatusDate       = "2018-01-01T00",    # date for which the analysis is carried out
  NotionalPrincipal = 10000,               # nominal value
  PremiumDiscountAtIED = 0.0,            # Discount on price
  DayCountConvention = "A365",        # convention for day counting
  NominalInterestRate = 0.05,           # Interest rate
  CycleOfInterestPayment = "P6ML0"         # Interest payment cycle
)
evs <- events(CouponBond, t0)
print(evs, indices=c(-4,-9))

# Same as above but with premium discount of 1000
CouponBond <- Pam( # Principal at Maturity
  ContractID = "001",             # identifies the contract
  Currency = "CHF",               # currency in which the contract is denominated
  ContractRole = "RPA",                  # real (long) position asset
  ContractDealDate = "2018-01-01T00",    # at this date the contract is transacted
  InitialExchangeDate = "2018-01-02T00", # here starts the contract to be effective
  MaturityDate = "2021-01-02T00",        # the day of the repayment
  StatusDate       = "2018-01-01T00",    # date for which the analysis is carried out
  NotionalPrincipal = 10000,               # nominal value
  # DayCountConvention = "A365",         # convention for day counting
  PremiumDiscountAtIED = -1000.0,            # Discount on price
  DayCountConvention = "A365",        # convention for day counting
  NominalInterestRate = 0.05,           # Interest rate
  CycleOfInterestPayment = "P6ML0"         # Interest payment cycle
)
evs <- events(CouponBond, t0)
print(evs, indices=c(-4,-9))
