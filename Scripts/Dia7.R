# 1. Caso de estudio retornos diarios S&P500
library(ggplot2)
library(rugarch)

sp_datos <- read.csv("./Datos/SP.csv")
tail(sp_datos)

n <- nrow(sp_datos)
ret_sp <- sp_datos$close[2:n] / sp_datos$close[1:(n - 1)] - 1
sp_datos$ret <- c(NA, ret_sp)

sp_datos$date <- as.Date(as.character(sp_datos$date), "%Y-%m-%d")

g <- ggplot(data = sp_datos[2:n,], aes(x = date, y = ret))
g + geom_line() + labs(x = "Fecha", y = "Retorno Diario")

acf(sp_datos$ret[2:(n - 4)], lag.max = 20, main = "ACF Retornos Diaros S&P500", xlim = c(1, 20))
pacf(sp_datos$ret[2:(n - 4)], lag.max = 20, main = "PACF Retornos Diarios S&P500", xlim = c(1, 20))

for (i in c(0, 1, 3, 5)) {
        for (j in c(0, 5)) {
                arma_mod <- arima(sp_datos$ret[2:(n - 4)], order = c(i, 0, j), method = "ML", include.mean = TRUE)
                AIC_val <- AIC(arma_mod)
                BIC_val <- BIC(arma_mod)
                res <- paste("Para el modelo ARMA(", i, ",", j, ") tenemos un AIC de ", AIC_val, 
                             " y un BIC de", BIC_val, sep = "")
                print(res)
        }
}

ar1_mod <- arima(sp_datos$ret[2:(n - 4)], order = c(5, 0, 0), method = "ML", include.mean = TRUE)
acf(ar1_mod$residuals^2, lag.max = 20, main = "ACF Residuos al cuadrado", xlim = c(1, 20))
pacf(ar1_mod$residuals^2, lag.max = 20, main = "PACF Residuos al cuadrado", xlim = c(1, 20))

garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                         mean.model = list(armaOrder = c(5, 0), include.mean = TRUE), distribution.model = "std")
garch_mod <- ugarchfit(spec = garch_spec, data = sp_datos$ret[2:(n - 4)], fit.control = list(stationarity = 1))
show(garch_mod)

acf(garch_mod@fit$residuals, lag.max = 20, main = "ACF residuos GARCH(1, 1)", xlim = c(1, 20))

for (i in c(5, 10, 15)) {
        port_test <- Box.test(garch_mod@fit$residuals, lag = i, type = "Ljung-Box")
        res <- paste("Para los ", i, "primeros rezagos el p.value es ", port_test$p.value)
        print(res)
}

stan_res1 <- garch_mod@fit$residuals / garch_mod@fit$sigma
acf(stan_res1, lag.max = 20, main = "ACF Residuos Estandarizados GARCH(1, 1)", xlim = c(1, 20))

for (i in c(5, 10, 15, 20)) {
        port_test <- Box.test(stan_res1, lag = i, type = "Ljung-Box")
        res <- paste("Para los ", i, "primeros rezagos el p.value es ", port_test$p.value)
        print(res)
}

garch_for <- ugarchforecast(garch_mod, n.ahead = 5)

eval_for <- data.frame(Fecha = sp_datos$date[(n - 4):n], ret_obs = sp_datos$ret[(n - 4):n],
                       Ret_for1 = garch_for@forecast$seriesFor, 
                       Vol_for1 = garch_for@forecast$sigmaFor)

colnames(eval_for)[3:4] <- c("Ret_for1", "Vol_for1")
eval_for

rmse1 <- sqrt(mean((eval_for$ret_obs - eval_for$Ret_for1)^2))
rmse1

igarch_spec <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(5, 0), include.mean = TRUE), distribution.model = "std")
igarch_mod <- ugarchfit(spec = igarch_spec, data = sp_datos$ret[2:(n - 4)])
show(igarch_mod)

acf(igarch_mod@fit$residuals, lag.max = 20, main = "ACF residuos IGARCH(1, 1)", xlim = c(1, 20))

for (i in c(5, 10, 15)) {
        port_test <- Box.test(igarch_mod@fit$residuals, lag = i, type = "Ljung-Box")
        res <- paste("Para los ", i, "primeros rezagos el p.value es ", port_test$p.value)
        print(res)
}

stan_res2 <- igarch_mod@fit$residuals / igarch_mod@fit$sigma
acf(stan_res2, lag.max = 20, main = "ACF Residuos Estandarizados IGARCH(1, 1)", xlim = c(1, 20))

for (i in c(5, 10, 15, 20)) {
        port_test <- Box.test(stan_res2, lag = i, type = "Ljung-Box")
        res <- paste("Para los ", i, "primeros rezagos el p.value es ", port_test$p.value)
        print(res)
}

igarch_for <- ugarchforecast(igarch_mod, n.ahead = 5)

eval_for$Ret_for2 <- igarch_for@forecast$seriesFor
eval_for$Vol_for2 <- igarch_for@forecast$sigmaFor

eval_for

rmse2 <- sqrt(mean((eval_for$ret_obs - eval_for$Ret_for2)^2))
rmse2

egarch_spec <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)), 
                          mean.model = list(armaOrder = c(5, 0), include.mean = TRUE), distribution.model = "std")
egarch_mod <- ugarchfit(spec = egarch_spec, data = sp_datos$ret[2:(n - 4)])

show(egarch_mod)

acf(egarch_mod@fit$residuals, lag.max = 20, main = "ACF residuos EGARCH(1, 1)", xlim = c(1, 20))

for (i in c(5, 10, 15)) {
        port_test <- Box.test(egarch_mod@fit$residuals, lag = i, type = "Ljung-Box")
        res <- paste("Para los ", i, "primeros rezagos el p.value es ", port_test$p.value)
        print(res)
}

stan_res3 <- egarch_mod@fit$residuals / egarch_mod@fit$sigma

acf(stan_res3, lag.max = 20, main = "ACF Residuos Estandarizados EGARCH(1, 1)", xlim = c(1, 20))

for (i in c(5, 10, 15, 20)) {
        port_test <- Box.test(stan_res3, lag = i, type = "Ljung-Box")
        res <- paste("Para los ", i, "primeros rezagos el p.value es ", port_test$p.value)
        print(res)
}

egarch_for <- ugarchforecast(egarch_mod, n.ahead = 5)
eval_for$Ret_for3 <- egarch_for@forecast$seriesFor
eval_for$Vol_for3 <- egarch_for@forecast$sigmaFor
eval_for

rmse3 <- sqrt(mean((eval_for$ret_obs - eval_for$Ret_for3)^2))
rmse3

gjrgarch_spec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), 
                            mean.model = list(armaOrder = c(5, 0), include.mean = TRUE), distribution.model = "std")
gjrgarch_mod <- ugarchfit(spec = gjrgarch_spec, data = sp_datos$ret[2:(n - 4)], fit.control = list(stationarity = 1))

show(gjrgarch_mod)

acf(gjrgarch_mod@fit$residuals, lag.max = 20, main = "ACF residuos GJR-GARCH(1, 1)", xlim = c(1, 20))

for (i in c(5, 10, 15)) {
        port_test <- Box.test(gjrgarch_mod@fit$residuals, lag = i, type = "Ljung-Box")
        res <- paste("Para los ", i, "primeros rezagos el p.value es ", port_test$p.value)
        print(res)
}

stan_res4 <- gjrgarch_mod@fit$residuals / gjrgarch_mod@fit$sigma
acf(stan_res4, lag.max = 20, main = "ACF Residuos Estandarizados GJR-GARCH(1, 1)", xlim = c(1, 20))

for (i in c(5, 10, 15, 20)) {
        port_test <- Box.test(stan_res4, lag = i, type = "Ljung-Box")
        res <- paste("Para los ", i, "primeros rezagos el p.value es ", port_test$p.value)
        print(res)
}

gjrgarch_for <- ugarchforecast(gjrgarch_mod, n.ahead = 5)
eval_for$Ret_for4 <- gjrgarch_for@forecast$seriesFor
eval_for$Vol_for4 <- gjrgarch_for@forecast$sigmaFor
eval_for

rmse4 <- sqrt(mean((eval_for$ret_obs - eval_for$Ret_for4)^2))
rmse4

rmse_all <- c(rmse1, rmse2, rmse3, rmse4)
which.min(rmse_all)

# 2. Ley de Grandes Numeros
set.seed(12345)
z <- rnorm(500, mean = 3, sd = 2)
mean(z)

# 3. Teorema del límite central para secuencias i.i.d

mu <- 3
sd <- 2
T <- 10000
x <- rep(0, 1000)
for (i in 1:1000) {
        z <- rnorm(T, mean = mu, sd = sd)
        x[i] <- sqrt(T) * (mean(z) - mu)
}

datos <- data.frame(x = x)
g <- ggplot(data = datos, aes(x = x))
g + geom_histogram(aes(y = ..density..), breaks = seq(-8, 8, by = 0.1), colour = "black", fill = "white") +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 2), colour = "red")

# 4. Teorema ergódico

set.seed(12345)
epsilon <- rnorm(1000)
y <- rep(0, 1000)
y[1] <- 1
for (i in 2:1000) {
        y[i] <- 1.2 + 0.6 * y[i - 1] + epsilon[i]
}
mean(y[501:1000])

# 5. Teorema límite central procesos ergodicos estacionarios

T <- 10000; n <- 1000
x <- rep(0, n)
for (i in 1:n) {
        epsilon <- rnorm(T)
        y <- rep(0, T)
        for (j in 2:T) {
                y[j] <- 1.4 + epsilon[j] + 0.6 * epsilon[j - 1]
        }
        x[i] <- sqrt(T - 1) * (mean(y[2:T]) - 1.4)
}

datos <- data.frame(x = x)
g <- ggplot(data = datos, aes(x = x))
g + geom_histogram(aes(y = ..density..), breaks = seq(-6, 6, by = 0.1), colour = "black", fill = "white") +
        stat_function(fun = dnorm, args = list(mean = 0, sd = sqrt(2.56)), colour = "red")

# 6. Teorema de limite central para martingalas en diferencia

T <- 10000; n <- 1000
x <- rep(0, n)
for (i in 1:n) {
        u <- rep(0, T)
        u[1] <- rnorm(1); v <- rnorm(T)
        for (j in 2:T) {
                u[j] <- v[j] * sqrt(0.5 + 0.8 * u[j - 1]^2)
        }
        x[i] <- sqrt(T - 1) * mean(u[2:T])
}

datos <- data.frame(x = x)
g <- ggplot(data = datos, aes(x = x))
g + geom_histogram(aes(y = ..density..), breaks = seq(-6, 6, by = 0.1), colour = "black", fill = "white") +
        stat_function(fun = dnorm, args = list(mean = 0, sd = sqrt(2.5)), colour = "red")