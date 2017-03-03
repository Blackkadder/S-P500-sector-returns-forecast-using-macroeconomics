# 
# Originally a class group project. 
# Objective of the project was to compare different modeling approaches
# - multiple regression, ARIMA and PCA.
# This section of the project deals with feature engineering using stepwise regressions.
# 
# We asked ourselves whether we can use macroeconomic indicators to forecast S&P500 sector returns.
# We looked at monthly returns of 10 sectors of the S&P500 separately 
# - Healthcare, Financial, Energy, IT, Consumer discretionary, Comsumer staples, Industrials, Utilities, Telecom and Materials.

## import
library(tseries)
library(RcmdrMisc)

## Working directory
#setwd()

## Import and transfer data to time series format
clean = read.csv("Data/clean1.csv")
head(clean)

##create time series
clean <- clean[,-1]
Total = ts(clean, start=c(1992,1), end=c(2015,12), frequency=12) 
trade = diff(log(Total[,"export"]/Total[,"import"]))

#### First glance of S&P-500 sector index
par(mfrow=c(3,3))
for(i in colnames(Total))
{
  plot(Total[,i],ylab="S&P-500 Sector Index",type="l",xlab="Year",cex.lab=1.5,
       cex.axis=1.5,cex.main=1.3,main=i)
}

## Returns are obviously not stationary. Taking log difference of  indexes
Total_diff = diff(log(Total[,-23]))
head(Total_diff)

## plot all transformations and dickey fuller test for stationarity
par(mfrow=c(3,3))
for(i in colnames(Total_diff))
{
  plot(Total_diff[,i],ylab="S&P-500 Sector Index",type="l",xlab="Year",cex.lab=1.5,
       cex.axis=1.5,cex.main=1.3,main=i)
  print(i)
  print(adf.test(Total_diff[,i])$p.value)
  print("#####################")
}

##Attach
attach(as.data.frame(Total_diff))

#reg.all = lm(Total_diff[,1:10]~Total_diff[,11:25])
#summary(reg.all)

## The series are stationary. We are happy with the transformations. 
## Let's start stepwise regressions: full model, summary and stepwise algo
## model selection done with BIC

#healthcare
reg.all.healthcare = lm(Healthcare~X30.YEAR+Federal.funds.rate+CPIAUCSL+unemploymentRATE.regular
                        +X5.year.treasury.rate+exchange.rate+real.disposable.personal.income
                        +PCE,industrail.production+M1+M2+Nominal.GDP+Real.GDP+trade)
summary(reg.all.healthcare)
stepwise(reg.all.healthcare)

#Financials
reg.all.financial = lm(Financial~X30.YEAR+Federal.funds.rate+CPIAUCSL+unemploymentRATE.regular
                       +X5.year.treasury.rate+exchange.rate+real.disposable.personal.income
                       +PCE,industrail.production+M1+M2+Nominal.GDP+Real.GDP+trade)
summary(reg.all.financial)
stepwise(reg.all.financial)

#energy
reg.all.energy = lm(Energy~X30.YEAR+Federal.funds.rate+CPIAUCSL+unemploymentRATE.regular
                    +X5.year.treasury.rate+exchange.rate+real.disposable.personal.income
                    +PCE,industrail.production+M1+M2+Nominal.GDP+Real.GDP+trade)
summary(reg.all.energy)
stepwise(reg.all.energy)

#util
reg.all.util = lm(Utilities~X30.YEAR+Federal.funds.rate+CPIAUCSL+unemploymentRATE.regular
                  +X5.year.treasury.rate+exchange.rate+real.disposable.personal.income
                  +PCE,industrail.production+M1+M2+Nominal.GDP+Real.GDP+trade)
summary(reg.all.util)
stepwise(reg.all.util)

#it
reg.all.it = lm(IT~X30.YEAR+Federal.funds.rate+CPIAUCSL+unemploymentRATE.regular
                +X5.year.treasury.rate+exchange.rate+real.disposable.personal.income
                +PCE,industrail.production+M1+M2+Nominal.GDP+Real.GDP+trade)
summary(reg.all.it)
stepwise(reg.all.it)

#condis
reg.all.condis = lm(Consumer.Discretionary~X30.YEAR+Federal.funds.rate+CPIAUCSL+unemploymentRATE.regular
                    +X5.year.treasury.rate+exchange.rate+real.disposable.personal.income
                    +PCE,industrail.production+M1+M2+Nominal.GDP+Real.GDP+trade)
summary(reg.all.condis)
stepwise(reg.all.condis)


#industrial
reg.all.ind = lm(Industrials~X30.YEAR+Federal.funds.rate+CPIAUCSL+unemploymentRATE.regular
                 +X5.year.treasury.rate+exchange.rate+real.disposable.personal.income
                 +PCE,industrail.production+M1+M2+Nominal.GDP+Real.GDP+trade)
summary(reg.all.ind)
stepwise(reg.all.ind)

#consta
reg.all.consta = lm(Consumer.staples~X30.YEAR+Federal.funds.rate+CPIAUCSL+unemploymentRATE.regular
                    +X5.year.treasury.rate+exchange.rate+real.disposable.personal.income
                    +PCE,industrail.production+M1+M2+Nominal.GDP+Real.GDP+trade)
summary(reg.all.consta)
stepwise(reg.all.consta)

#telecon
reg.all.telecon = lm(Telecom~X30.YEAR+Federal.funds.rate+CPIAUCSL+unemploymentRATE.regular
                     +X5.year.treasury.rate+exchange.rate+real.disposable.personal.income
                     +PCE,industrail.production+M1+M2+Nominal.GDP+Real.GDP+trade)
summary(reg.all.telecon)
stepwise(reg.all.telecon)

#materials
reg.all.mat = lm(Materials~X30.YEAR+Federal.funds.rate+CPIAUCSL+unemploymentRATE.regular
                 +X5.year.treasury.rate+exchange.rate+real.disposable.personal.income
                 +PCE,industrail.production+M1+M2+Nominal.GDP+Real.GDP+trade)
summary(reg.all.mat)
stepwise(reg.all.mat)

## Effects of Macroeconomic variables are realistically lagged.
## For each X variable, we created lags of 1 month, 2 months, 3 months, 6 months and 12 months 
## creating lags
len = length(Total_diff[,1])
lag0 = as.data.frame(Total_diff[(13:len),])
lag1 = as.data.frame(Total_diff[(13-1):(len-1),])
lag2 = as.data.frame(Total_diff[(13-2):(len-2),])
lag3 = as.data.frame(Total_diff[(13-3):(len-3),])
lag6 = as.data.frame(Total_diff[(13-6):(len-6),])
lag12 = as.data.frame(Total_diff[(13-12):(len-12),])

##Running stepwise algo again. This time including lags. Also the final model shown.

##Healthcare
reg.health = lm(lag0$Healthcare~ 
                  +lag1$X30.YEAR+lag1$Federal.funds.rate+lag1$CPIAUCSL
                +lag1$unemploymentRATE.regular+lag1$X5.year.treasury.rate+lag1$exchange.rate
                +lag1$real.disposable.personal.income+lag1$PCE+lag1$industrail.production
                +lag1$M1+lag1$M2+lag1$Nominal.GDP+lag1$Real.GDP
                
                
                +lag3$X30.YEAR+lag3$Federal.funds.rate+lag3$CPIAUCSL
                +lag3$unemploymentRATE.regular+lag3$X5.year.treasury.rate+lag3$exchange.rate
                +lag3$real.disposable.personal.income+lag3$PCE+lag3$industrail.production
                +lag3$M1+lag3$M2+lag3$Nominal.GDP+lag3$Real.GDP
                
                +lag6$X30.YEAR+lag6$Federal.funds.rate+lag6$CPIAUCSL
                +lag6$unemploymentRATE.regular+lag6$X5.year.treasury.rate+lag6$exchange.rate
                +lag6$real.disposable.personal.income+lag6$PCE+lag6$industrail.production
                +lag6$M1+lag6$M2+lag6$Nominal.GDP+lag6$Real.GDP
                
                
                +lag12$X30.YEAR+lag12$Federal.funds.rate+lag12$CPIAUCSL
                +lag12$unemploymentRATE.regular+lag12$X5.year.treasury.rate+lag12$exchange.rate
                +lag12$real.disposable.personal.income+lag12$PCE+lag12$industrail.production
                +lag12$M1+lag12$M2+lag12$Nominal.GDP+lag12$Real.GDP
                
)
summary(stepwise(reg.health))

model.health = lm(formula = lag0$Healthcare ~ lag1$exchange.rate + lag6$industrail.production)
summary(model.health)

## Financials
reg.fin = lm(lag0$Financial~ 
               +lag1$X30.YEAR+lag1$Federal.funds.rate+lag1$CPIAUCSL
             +lag1$unemploymentRATE.regular+lag1$X5.year.treasury.rate+lag1$exchange.rate
             +lag1$real.disposable.personal.income+lag1$PCE+lag1$industrail.production
             +lag1$M1+lag1$M2+lag1$Nominal.GDP+lag1$Real.GDP
             
             
             +lag3$X30.YEAR+lag3$Federal.funds.rate+lag3$CPIAUCSL
             +lag3$unemploymentRATE.regular+lag3$X5.year.treasury.rate+lag3$exchange.rate
             +lag3$real.disposable.personal.income+lag3$PCE+lag3$industrail.production
             +lag3$M1+lag3$M2+lag3$Nominal.GDP+lag3$Real.GDP
             
             +lag6$X30.YEAR+lag6$Federal.funds.rate+lag6$CPIAUCSL
             +lag6$unemploymentRATE.regular+lag6$X5.year.treasury.rate+lag6$exchange.rate
             +lag6$real.disposable.personal.income+lag6$PCE+lag6$industrail.production
             +lag6$M1+lag6$M2+lag6$Nominal.GDP+lag6$Real.GDP
             
             
             +lag12$X30.YEAR+lag12$Federal.funds.rate+lag12$CPIAUCSL
             +lag12$unemploymentRATE.regular+lag12$X5.year.treasury.rate+lag12$exchange.rate
             +lag12$real.disposable.personal.income+lag12$PCE+lag12$industrail.production
             +lag12$M1+lag12$M2+lag12$Nominal.GDP+lag12$Real.GDP
             
)

summary(stepwise(reg.fin))
model.fin = lm(formula = lag0$Financial ~ lag1$X5.year.treasury.rate + lag1$exchange.rate + 
                 lag3$CPIAUCSL + lag3$industrail.production + lag3$M1 + lag3$Nominal.GDP + 
                 lag6$industrail.production + lag6$M1)
summary(model.fin)


## Energy
reg.energy = lm(lag0$Energy~ 
                  +lag1$X30.YEAR+lag1$Federal.funds.rate+lag1$CPIAUCSL
                +lag1$unemploymentRATE.regular+lag1$X5.year.treasury.rate+lag1$exchange.rate
                +lag1$real.disposable.personal.income+lag1$PCE+lag1$industrail.production
                +lag1$M1+lag1$M2+lag1$Nominal.GDP+lag1$Real.GDP
                
                
                +lag3$X30.YEAR+lag3$Federal.funds.rate+lag3$CPIAUCSL
                +lag3$unemploymentRATE.regular+lag3$X5.year.treasury.rate+lag3$exchange.rate
                +lag3$real.disposable.personal.income+lag3$PCE+lag3$industrail.production
                +lag3$M1+lag3$M2+lag3$Nominal.GDP+lag3$Real.GDP
                
                +lag6$X30.YEAR+lag6$Federal.funds.rate+lag6$CPIAUCSL
                +lag6$unemploymentRATE.regular+lag6$X5.year.treasury.rate+lag6$exchange.rate
                +lag6$real.disposable.personal.income+lag6$PCE+lag6$industrail.production
                +lag6$M1+lag6$M2+lag6$Nominal.GDP+lag6$Real.GDP
                
                
                +lag12$X30.YEAR+lag12$Federal.funds.rate+lag12$CPIAUCSL
                +lag12$unemploymentRATE.regular+lag12$X5.year.treasury.rate+lag12$exchange.rate
                +lag12$real.disposable.personal.income+lag12$PCE+lag12$industrail.production
                +lag12$M1+lag12$M2+lag12$Nominal.GDP+lag12$Real.GDP
                
)

summary(stepwise(reg.energy))

model.energy = lm(formula = lag0$Energy ~ lag1$X5.year.treasury.rate + lag1$exchange.rate + 
                    lag6$industrail.production)
summary(model.energy)

## Utilities
reg.util = lm(lag0$Utilities~ 
                +lag1$X30.YEAR+lag1$Federal.funds.rate+lag1$CPIAUCSL
              +lag1$unemploymentRATE.regular+lag1$X5.year.treasury.rate+lag1$exchange.rate
              +lag1$real.disposable.personal.income+lag1$PCE+lag1$industrail.production
              +lag1$M1+lag1$M2+lag1$Nominal.GDP+lag1$Real.GDP
              
              
              +lag3$X30.YEAR+lag3$Federal.funds.rate+lag3$CPIAUCSL
              +lag3$unemploymentRATE.regular+lag3$X5.year.treasury.rate+lag3$exchange.rate
              +lag3$real.disposable.personal.income+lag3$PCE+lag3$industrail.production
              +lag3$M1+lag3$M2+lag3$Nominal.GDP+lag3$Real.GDP
              
              +lag6$X30.YEAR+lag6$Federal.funds.rate+lag6$CPIAUCSL
              +lag6$unemploymentRATE.regular+lag6$X5.year.treasury.rate+lag6$exchange.rate
              +lag6$real.disposable.personal.income+lag6$PCE+lag6$industrail.production
              +lag6$M1+lag6$M2+lag6$Nominal.GDP+lag6$Real.GDP
              
              
              +lag12$X30.YEAR+lag12$Federal.funds.rate+lag12$CPIAUCSL
              +lag12$unemploymentRATE.regular+lag12$X5.year.treasury.rate+lag12$exchange.rate
              +lag12$real.disposable.personal.income+lag12$PCE+lag12$industrail.production
              +lag12$M1+lag12$M2+lag12$Nominal.GDP+lag12$Real.GDP
              
)

summary(stepwise(reg.util))

model.util = lm(formula = lag0$Utilities ~ lag1$exchange.rate + lag3$M1 + 
                  lag6$unemploymentRATE.regular + lag6$real.disposable.personal.income + 
                  lag6$PCE)

summary(model.util)



## IT
reg.it = lm(lag0$IT~ 
              +lag1$X30.YEAR+lag1$Federal.funds.rate+lag1$CPIAUCSL
            +lag1$unemploymentRATE.regular+lag1$X5.year.treasury.rate+lag1$exchange.rate
            +lag1$real.disposable.personal.income+lag1$PCE+lag1$industrail.production
            +lag1$M1+lag1$M2+lag1$Nominal.GDP+lag1$Real.GDP
            
            
            +lag3$X30.YEAR+lag3$Federal.funds.rate+lag3$CPIAUCSL
            +lag3$unemploymentRATE.regular+lag3$X5.year.treasury.rate+lag3$exchange.rate
            +lag3$real.disposable.personal.income+lag3$PCE+lag3$industrail.production
            +lag3$M1+lag3$M2+lag3$Nominal.GDP+lag3$Real.GDP
            
            +lag6$X30.YEAR+lag6$Federal.funds.rate+lag6$CPIAUCSL
            +lag6$unemploymentRATE.regular+lag6$X5.year.treasury.rate+lag6$exchange.rate
            +lag6$real.disposable.personal.income+lag6$PCE+lag6$industrail.production
            +lag6$M1+lag6$M2+lag6$Nominal.GDP+lag6$Real.GDP
            
            
            +lag12$X30.YEAR+lag12$Federal.funds.rate+lag12$CPIAUCSL
            +lag12$unemploymentRATE.regular+lag12$X5.year.treasury.rate+lag12$exchange.rate
            +lag12$real.disposable.personal.income+lag12$PCE+lag12$industrail.production
            +lag12$M1+lag12$M2+lag12$Nominal.GDP+lag12$Real.GDP
            
)

summary(stepwise(reg.it))
model.it = lm(formula = lag0$IT ~ lag1$X5.year.treasury.rate + lag1$exchange.rate + 
                lag3$Nominal.GDP + lag3$Real.GDP + lag6$industrail.production)

summary(model.it)



## COnsumer discretionary
reg.condis = lm(lag0$Consumer.Discretionary~ 
                  +lag1$X30.YEAR+lag1$Federal.funds.rate+lag1$CPIAUCSL
                +lag1$unemploymentRATE.regular+lag1$X5.year.treasury.rate+lag1$exchange.rate
                +lag1$real.disposable.personal.income+lag1$PCE+lag1$industrail.production
                +lag1$M1+lag1$M2+lag1$Nominal.GDP+lag1$Real.GDP
                
                
                +lag3$X30.YEAR+lag3$Federal.funds.rate+lag3$CPIAUCSL
                +lag3$unemploymentRATE.regular+lag3$X5.year.treasury.rate+lag3$exchange.rate
                +lag3$real.disposable.personal.income+lag3$PCE+lag3$industrail.production
                +lag3$M1+lag3$M2+lag3$Nominal.GDP+lag3$Real.GDP
                
                +lag6$X30.YEAR+lag6$Federal.funds.rate+lag6$CPIAUCSL
                +lag6$unemploymentRATE.regular+lag6$X5.year.treasury.rate+lag6$exchange.rate
                +lag6$real.disposable.personal.income+lag6$PCE+lag6$industrail.production
                +lag6$M1+lag6$M2+lag6$Nominal.GDP+lag6$Real.GDP
                
                
                +lag12$X30.YEAR+lag12$Federal.funds.rate+lag12$CPIAUCSL
                +lag12$unemploymentRATE.regular+lag12$X5.year.treasury.rate+lag12$exchange.rate
                +lag12$real.disposable.personal.income+lag12$PCE+lag12$industrail.production
                +lag12$M1+lag12$M2+lag12$Nominal.GDP+lag12$Real.GDP
                
)

summary(stepwise(reg.condis))
model.condis = lm(formula = lag0$Consumer.Discretionary ~ lag1$CPIAUCSL + lag1$X5.year.treasury.rate + 
                    lag1$exchange.rate + lag3$industrail.production + lag6$PCE)

summary(model.condis)



## Consumer Staple
reg.constaple = lm(lag0$Consumer.staples~ 
                     +lag1$X30.YEAR+lag1$Federal.funds.rate+lag1$CPIAUCSL
                   +lag1$unemploymentRATE.regular+lag1$X5.year.treasury.rate+lag1$exchange.rate
                   +lag1$real.disposable.personal.income+lag1$PCE+lag1$industrail.production
                   +lag1$M1+lag1$M2+lag1$Nominal.GDP+lag1$Real.GDP
                   
                   
                   +lag3$X30.YEAR+lag3$Federal.funds.rate+lag3$CPIAUCSL
                   +lag3$unemploymentRATE.regular+lag3$X5.year.treasury.rate+lag3$exchange.rate
                   +lag3$real.disposable.personal.income+lag3$PCE+lag3$industrail.production
                   +lag3$M1+lag3$M2+lag3$Nominal.GDP+lag3$Real.GDP
                   
                   +lag6$X30.YEAR+lag6$Federal.funds.rate+lag6$CPIAUCSL
                   +lag6$unemploymentRATE.regular+lag6$X5.year.treasury.rate+lag6$exchange.rate
                   +lag6$real.disposable.personal.income+lag6$PCE+lag6$industrail.production
                   +lag6$M1+lag6$M2+lag6$Nominal.GDP+lag6$Real.GDP
                   
                   
                   +lag12$X30.YEAR+lag12$Federal.funds.rate+lag12$CPIAUCSL
                   +lag12$unemploymentRATE.regular+lag12$X5.year.treasury.rate+lag12$exchange.rate
                   +lag12$real.disposable.personal.income+lag12$PCE+lag12$industrail.production
                   +lag12$M1+lag12$M2+lag12$Nominal.GDP+lag12$Real.GDP
                   
)

summary(stepwise(reg.constaple))



## Industrials
reg.ind = lm(lag0$Industrials~ 
               +lag1$X30.YEAR+lag1$Federal.funds.rate+lag1$CPIAUCSL
             +lag1$unemploymentRATE.regular+lag1$X5.year.treasury.rate+lag1$exchange.rate
             +lag1$real.disposable.personal.income+lag1$PCE+lag1$industrail.production
             +lag1$M1+lag1$M2+lag1$Nominal.GDP+lag1$Real.GDP
             
             
             +lag3$X30.YEAR+lag3$Federal.funds.rate+lag3$CPIAUCSL
             +lag3$unemploymentRATE.regular+lag3$X5.year.treasury.rate+lag3$exchange.rate
             +lag3$real.disposable.personal.income+lag3$PCE+lag3$industrail.production
             +lag3$M1+lag3$M2+lag3$Nominal.GDP+lag3$Real.GDP
             
             +lag6$X30.YEAR+lag6$Federal.funds.rate+lag6$CPIAUCSL
             +lag6$unemploymentRATE.regular+lag6$X5.year.treasury.rate+lag6$exchange.rate
             +lag6$real.disposable.personal.income+lag6$PCE+lag6$industrail.production
             +lag6$M1+lag6$M2+lag6$Nominal.GDP+lag6$Real.GDP
             
             
             +lag12$X30.YEAR+lag12$Federal.funds.rate+lag12$CPIAUCSL
             +lag12$unemploymentRATE.regular+lag12$X5.year.treasury.rate+lag12$exchange.rate
             +lag12$real.disposable.personal.income+lag12$PCE+lag12$industrail.production
             +lag12$M1+lag12$M2+lag12$Nominal.GDP+lag12$Real.GDP
             
)

summary(stepwise(reg.ind))
model.ind = lm(formula = lag0$Industrials ~ lag1$CPIAUCSL + lag1$X5.year.treasury.rate + 
                 lag1$exchange.rate + lag3$industrail.production + lag6$industrail.production + 
                 lag6$M1)
summary(model.ind)



## Telecom
reg.telecom = lm(lag0$Telecom~ 
                   +lag1$X30.YEAR+lag1$Federal.funds.rate+lag1$CPIAUCSL
                 +lag1$unemploymentRATE.regular+lag1$X5.year.treasury.rate+lag1$exchange.rate
                 +lag1$real.disposable.personal.income+lag1$PCE+lag1$industrail.production
                 +lag1$M1+lag1$M2+lag1$Nominal.GDP+lag1$Real.GDP
                 
                 
                 +lag3$X30.YEAR+lag3$Federal.funds.rate+lag3$CPIAUCSL
                 +lag3$unemploymentRATE.regular+lag3$X5.year.treasury.rate+lag3$exchange.rate
                 +lag3$real.disposable.personal.income+lag3$PCE+lag3$industrail.production
                 +lag3$M1+lag3$M2+lag3$Nominal.GDP+lag3$Real.GDP
                 
                 +lag6$X30.YEAR+lag6$Federal.funds.rate+lag6$CPIAUCSL
                 +lag6$unemploymentRATE.regular+lag6$X5.year.treasury.rate+lag6$exchange.rate
                 +lag6$real.disposable.personal.income+lag6$PCE+lag6$industrail.production
                 +lag6$M1+lag6$M2+lag6$Nominal.GDP+lag6$Real.GDP
                 
                 
                 +lag12$X30.YEAR+lag12$Federal.funds.rate+lag12$CPIAUCSL
                 +lag12$unemploymentRATE.regular+lag12$X5.year.treasury.rate+lag12$exchange.rate
                 +lag12$real.disposable.personal.income+lag12$PCE+lag12$industrail.production
                 +lag12$M1+lag12$M2+lag12$Nominal.GDP+lag12$Real.GDP
                 
)

summary(stepwise(reg.telecom))
model.telecom = lm(formula = lag0$Industrials ~ lag1$CPIAUCSL + lag1$X5.year.treasury.rate + 
                     lag1$exchange.rate + lag3$industrail.production + lag6$industrail.production + 
                     lag6$M1)
summary(model.telecom)



## Materials
reg.mat = lm(lag0$Materials~ 
               +lag1$X30.YEAR+lag1$Federal.funds.rate+lag1$CPIAUCSL
             +lag1$unemploymentRATE.regular+lag1$X5.year.treasury.rate+lag1$exchange.rate
             +lag1$real.disposable.personal.income+lag1$PCE+lag1$industrail.production
             +lag1$M1+lag1$M2+lag1$Nominal.GDP+lag1$Real.GDP
             
             
             +lag3$X30.YEAR+lag3$Federal.funds.rate+lag3$CPIAUCSL
             +lag3$unemploymentRATE.regular+lag3$X5.year.treasury.rate+lag3$exchange.rate
             +lag3$real.disposable.personal.income+lag3$PCE+lag3$industrail.production
             +lag3$M1+lag3$M2+lag3$Nominal.GDP+lag3$Real.GDP
             
             +lag6$X30.YEAR+lag6$Federal.funds.rate+lag6$CPIAUCSL
             +lag6$unemploymentRATE.regular+lag6$X5.year.treasury.rate+lag6$exchange.rate
             +lag6$real.disposable.personal.income+lag6$PCE+lag6$industrail.production
             +lag6$M1+lag6$M2+lag6$Nominal.GDP+lag6$Real.GDP
             
             
             +lag12$X30.YEAR+lag12$Federal.funds.rate+lag12$CPIAUCSL
             +lag12$unemploymentRATE.regular+lag12$X5.year.treasury.rate+lag12$exchange.rate
             +lag12$real.disposable.personal.income+lag12$PCE+lag12$industrail.production
             +lag12$M1+lag12$M2+lag12$Nominal.GDP+lag12$Real.GDP
             
)

summary(stepwise(reg.mat))

#export all
Y_diff = cbind(Healthcare = lag0$Healthcare,Financial = lag0$Financial,Energy = lag0$Energy,
               Utilities = lag0$Utilities,IT = lag0$IT, Consumer_disc=lag0$Consumer.Discretionary,
               Consumer_staple = lag0$Consumer.staples, Industrial = lag0$Industrials,
               Telecom = lag0$Telecom,Materials =lag0$Materials)


Y_hat_diff = cbind(Healthcare = fitted(reg.health),Financial = fitted(reg.fin),
                   Energy = fitted(reg.energy),Utilities= fitted(reg.util),
                   IT = fitted(reg.it), Consumer_disc=fitted(reg.condis),
                   Consumer_staple = fitted(reg.constaple), Industrial = fitted(reg.ind),
                   Telecom = fitted(reg.telecom),Materials =fitted(reg.mat))

## Plot results vs actuals. Not bad considering we used only macroeconomic variables!
## Conclusion: Macroeconomic variables good for long run, terrible for short term trading!
par(mfrow=c(3,4))
for (i in colnames(Y_diff))
{
  plot(exp(cumsum(Y_diff[,i])),ylab="Return",type="l",xlab="Year",cex.lab=1.5,
       cex.axis=1.5,cex.main=1.3,main=i,col="blue")
  lines(exp(cumsum(Y_hat_diff[,i])),col='red')
  print(i)
  print("#####################")
}

#write.csv(Y_diff,"Actual_diff.csv")

#write.csv 