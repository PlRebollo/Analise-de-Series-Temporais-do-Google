library(yfR)
library(forecast)
library(GAS)
library(rugarch)
nome_acao <- "GOOG"   # Código no Yahoo Finance
data_ini  <- "2010-01-01" # Data de inicio
data_fim  <- "2023-06-14" # Data de fim
precos <- yf_get(tickers = nome_acao, first_date = data_ini, last_date = data_fim)
head(precos)

ts.plot(precos$price_adjusted)
acf(precos$price_adjusted, lag.max = 100)
pacf(precos$price_adjusted)

# O preço não é estacionário, logo usaremos o retorno
retornos <- precos$ret_adjusted_prices[-1]
head(retornos)
summary(retornos)

#centrando
retornos <- retornos - mean(retornos)
summary(retornos)


ts.plot(retornos)
acf(retornos)# primeira barra sai
pacf(retornos)# tem que moselar
acf(retornos^2)# tem que usar um garch

Box.test(retornos, lag = 10, type = "Ljung-Box")

### Modelos  e diagnóstico - testarei com os erros assumindo uma t 
#garch(1,1), os parametros não são significativos
spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   distribution = 'std')

fit_01 <- ugarchfit(spec, retornos, solver = 'hybrid')
fit_01

e_hat1 <- fit_01@fit$residuals / fit_01@fit$sigma
op = par(mfrow = c(1,2))
acf(e_hat)
acf(e_hat1^2) 
par(op)

plot(fit_01)# QQPLOT - 9 




#AR(1) Garch(1,1), Ar não é sign
spec <- ugarchspec(mean.model = list(armaOrder = c(1, 0), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   distribution = 'std')

fit_02 <- ugarchfit(spec, retornos, solver = 'hybrid')
fit_02

e_hat2 <- fit_02@fit$residuals / fit_02@fit$sigma
op = par(mfrow = c(1,2))
acf(e_hat2)
acf(e_hat2^2) 
par(op)

plot(fit_02)# QQPLOT - 9 

#ARMA(1,1) Garch(1,1), nada é significativo
spec <- ugarchspec(mean.model = list(armaOrder = c(1, 1), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   distribution = 'std')

fit_03 <- ugarchfit(spec, retornos, solver = 'hybrid')
fit_03

e_hat3 <- fit_02@fit$residuals / fit_02@fit$sigma
op = par(mfrow = c(1,2))
acf(e_hat3) 
acf(e_hat3^2)
par(op)

plot(fit_03)# QQPLOT - 9 

#ARMA(1,1) Garch(2,2), vários são não significativos
spec <- ugarchspec(mean.model = list(armaOrder = c(1, 1), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(2, 2)),
                   distribution = 'std')

fit_04 <- ugarchfit(spec, retornos, solver = 'hybrid')
fit_04

e_hat4 <- fit_02@fit$residuals / fit_02@fit$sigma
op = par(mfrow = c(1,2))
acf(e_hat4) 
acf(e_hat4^2) 
par(op)

plot(fit_04) 

#ARMA(2,2) Garch(1,1), um dos melhores e com menor AIC(Akaike) *
spec <- ugarchspec(mean.model = list(armaOrder = c(2, 2), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   distribution = 'std')

fit_05 <- ugarchfit(spec, retornos, solver = 'hybrid')
fit_05

e_hat5 <- fit_02@fit$residuals / fit_02@fit$sigma
op = par(mfrow = c(1,2))
acf(e_hat5) 
acf(e_hat5^2) 
par(op)

plot(fit_05)

#ARMA(2,2) Garch(0,1), parece bom, tudo é significativo *
spec <- ugarchspec(mean.model = list(armaOrder = c(2, 2), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(0, 1)),
                   distribution = 'std')

fit_06 <- ugarchfit(spec, retornos, solver = 'hybrid')
fit_06

e_hat6 <- fit_02@fit$residuals / fit_02@fit$sigma
op = par(mfrow = c(1,2))
acf(e_hat6) 
acf(e_hat6^2) 
par(op)

plot(fit_06)

#ARMA(1,1) Garch(0,1), arma não é significativo
spec <- ugarchspec(mean.model = list(armaOrder = c(1, 1), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(0, 1)),
                   distribution = 'std')

fit_014 <- ugarchfit(spec, retornos, solver = 'hybrid')
fit_014

e_hat14 <- fit_02@fit$residuals / fit_02@fit$sigma
op = par(mfrow = c(1,2))
acf(e_hat14) 
acf(e_hat14^2) 
par(op)

plot(fit_014)

# Garch(0,1), okay***
spec1 <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(0, 1)),
                   distribution = 'std')

fit_07 <- ugarchfit(spec1, retornos, solver = 'hybrid')
fit_07

e_hat7 <- fit_02@fit$residuals / fit_02@fit$sigma
op = par(mfrow = c(1,2))
acf(e_hat7) 
acf(e_hat7^2) 
par(op)

plot(fit_07)

# Arma(2,1) Garch(0,1), ar(2) não é significativo
spec <- ugarchspec(mean.model = list(armaOrder = c(2, 1), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(0, 1)),
                   distribution = 'std')

fit_08 <- ugarchfit(spec, retornos, solver = 'hybrid')
fit_08

e_hat8 <- fit_02@fit$residuals / fit_02@fit$sigma
op = par(mfrow = c(1,2))
acf(e_hat8) 
acf(e_hat8^2) 
par(op)

plot(fit_08)

# Arma(2,1) Garch(1,1), tem um dos menores AIC, parece bom ***
spec2 <- ugarchspec(mean.model = list(armaOrder = c(2, 1), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   distribution = 'std')

fit_09 <- ugarchfit(spec2, retornos, solver = 'hybrid')
fit_09

e_hat9 <- fit_02@fit$residuals / fit_02@fit$sigma
op = par(mfrow = c(1,2))
acf(e_hat9) 
acf(e_hat9^2) 
par(op)

plot(fit_09)

# Arma(1,1) Garch(1,0), arma não é significativo
spec <- ugarchspec(mean.model = list(armaOrder = c(1, 1), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(1, 0)),
                   distribution = 'std')

fit_010 <- ugarchfit(spec, retornos, solver = 'hybrid')
fit_010

e_hat10 <- fit_02@fit$residuals / fit_02@fit$sigma
op = par(mfrow = c(1,2))
acf(e_hat10) 
acf(e_hat10^2) 
par(op)

plot(fit_010)

# Garch(1,0), parece ok
spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(1, 0)),
                   distribution = 'std')

fit_011 <- ugarchfit(spec, retornos, solver = 'hybrid')
fit_011

e_hat11 <- fit_02@fit$residuals / fit_02@fit$sigma
op = par(mfrow = c(1,2))
acf(e_hat11) 
acf(e_hat11^2) 
par(op)

plot(fit_011)

# arma(2,2) Garch(1,0), todos são significativos *
spec <- ugarchspec(mean.model = list(armaOrder = c(2, 2), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(1, 0)),
                   distribution = 'std')

fit_012 <- ugarchfit(spec, retornos, solver = 'hybrid')
fit_012

e_hat12 <- fit_02@fit$residuals / fit_02@fit$sigma
op = par(mfrow = c(1,2))
acf(e_hat12) 
acf(e_hat12^2) 
par(op)

plot(fit_012)

# arma(2,2) Garch(1,2), parece bom, tem um dos menores AIC***
spec3 <- ugarchspec(mean.model = list(armaOrder = c(2, 2), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(1, 2)),
                   distribution = 'std')

fit_013 <- ugarchfit(spec3, retornos, solver = 'hybrid')
fit_013

e_hat13 <- fit_02@fit$residuals / fit_02@fit$sigma
op = par(mfrow = c(1,2))
acf(e_hat13) 
acf(e_hat13^2) 
par(op)


plot(fit_013)


##PRevisão
# OS melhores modelos foram ARMA(2,2) Garch(0,1), fit_06,
# Arma(2,1) Garch(1,1), fit_09
#arma(2,2) Garch(1,2), fit_13

xf = function(x, mu_, sigma_, shape_) {
  x*ddist(distribution = "std", 
          y = x,
          mu = mu_, 
          sigma = sigma_, 
          skew = 0,
          shape = shape_)
}
##Garch(0,1)
# Volatilidade
fore1 <- ugarchforecast(fit_07, n.ahead = 1)

# VaR
alpha <- 0.05
var1 <- qdist(distribution = "std", alpha, mu = fore1@forecast[["seriesFor"]][1], sigma = sigma(fore1)[1], skew = 0, shape = coef(fit_07)["shape"])

# ES
ES1 = integrate(xf, 
                -Inf, 
                var1, 
                mu_ = fore1@forecast[["seriesFor"]][1],
                sigma_ = sigma(fore1)[1],
                shape_ = coef(fit_07)["shape"])$value/alpha


## Arma(2,1) Garch(1,1)
# Volatilidade
fore2 <- ugarchforecast(fit_09, n.ahead = 1)

# VaR
alpha <- 0.05
var2 <- qdist(distribution = "std", alpha, mu = fore2@forecast[["seriesFor"]][1], sigma = sigma(fore2)[1], skew = 0, shape = coef(fit_09)["shape"])

# ES
ES2 = integrate(xf, 
                -Inf, 
                var2, 
                mu_ = fore2@forecast[["seriesFor"]][1],
                sigma_ = sigma(fore2)[1],
                shape_ = coef(fit_09)["shape"])$value/alpha

## Arma(2,2) Garch(1,2)
# Volatilidade
fore3 <- ugarchforecast(fit_013, n.ahead = 1)

# VaR
alpha <- 0.05
var3 <- qdist(distribution = "std", alpha, mu = fore3@forecast[["seriesFor"]][1], sigma = sigma(fore3)[1], skew = 0, shape = coef(fit_013)["shape"])

# ES
ES3 = integrate(xf, 
                -Inf, 
                var3, 
                mu_ = fore3@forecast[["seriesFor"]][1],
                sigma_ = sigma(fore3)[1],
                shape_ = coef(fit_013)["shape"])$value/alpha

###cross-validatiom

ntotal <- length(retornos)
InS <- 3000
OoS <- ntotal - InS
alpha = 0.05

#Garch(0,1), chamei este de 2 por erro meu

var2 <- c()
es2 <- c()


for (i in 1:OoS) {
  print(i)
  ret <- retornos[i:(InS + i - 1)]
  fit <- ugarchfit(spec1, ret, solver = 'hybrid')
  fore <- ugarchforecast(fit, n.ahead = 1)
  var2[i] <- qdist(distribution = "std", alpha, mu = fore@forecast[["seriesFor"]][1], sigma = sigma(fore)[1], skew = 0, shape = coef(fit)["shape"])
  es2[i] <- integrate(xf, 
                      -Inf, 
                      var2[i], 
                      mu_ = fore@forecast[["seriesFor"]][1],
                      sigma_ = sigma(fore)[1],
                      shape_ = coef(fit)["shape"])$value/alpha
}


# Arma(2,1) Garch(1,1)

var1 <- c()
es1 <- c()

for (i in 1:OoS) {
  print(i)
  ret <- retornos[i:(InS + i - 1)]
  fit <- ugarchfit(spec2, ret, solver = 'hybrid')
  fore <- ugarchforecast(fit, n.ahead = 1)
  var1[i] <- qdist(distribution = "std", alpha, mu = fore@forecast[["seriesFor"]][1], sigma = sigma(fore)[1], skew = 0, shape = coef(fit)["shape"])
  es1[i] <- integrate(xf, 
                      -Inf, 
                      var1[i], 
                      mu_ = fore@forecast[["seriesFor"]][1],
                      sigma_ = sigma(fore)[1],
                      shape_ = coef(fit)["shape"])$value/alpha
}

#arma(2,2) Garch(1,2)

var3 <- c()
es3 <- c()

for (i in 1:OoS) {
  print(i)
  ret <- retornos[i:(InS + i - 1)]
  fit <- ugarchfit(spec3, ret, solver = 'hybrid')
  fore <- ugarchforecast(fit, n.ahead = 1)
  var3[i] <- qdist(distribution = "std", alpha, mu = fore@forecast[["seriesFor"]][1], sigma = sigma(fore)[1], skew = 0, shape = coef(fit)["shape"])
  es3[i] <- integrate(xf, 
                      -Inf, 
                      var3[i], 
                      mu_ = fore@forecast[["seriesFor"]][1],
                      sigma_ = sigma(fore)[1],
                      shape_ = coef(fit)["shape"])$value/alpha
}

## Backtesting
ret_oos <- serie[(InS + 1) : (InS + 300)]

cross_var1 <- BacktestVaR(ret_oos, head(var1, 300), alpha = alpha, Lags = 4)
cross_var2 <- BacktestVaR(ret_oos, head(var2, 300), alpha = alpha, Lags = 4)
cross_var3 <- BacktestVaR(ret_oos, head(var3, 300), alpha = alpha, Lags = 4)

cross_es1 <- ESTest(alpha = alpha, ret_oos, head(es1, 300), head(var1, 300), conf.level = 0.95,  boot = FALSE)
cross_es2 <- ESTest(alpha = alpha, ret_oos, head(es2, 300), head(var2, 300), conf.level = 0.95,  boot = FALSE)
cross_es3 <- ESTest(alpha = alpha, ret_oos, head(es3, 300), head(var3, 300), conf.level = 0.95,  boot = FALSE)

cross_es1
cross_es2
cross_es3
## Se minha interpretação estiver correta o melhor modelo é Arma(2,1) Garch(1,1)