# 1. Simulacion de un modelo AR(1)

set.seed(12345)
epsilon <- rnorm(500)
y <- rep(0, 500)
y[1] <- 1
for (i in 2:500) {
        y[i] <- 1.3 - 0.8 * y[i - 1] + epsilon[i]
}

library(ggplot2)
ar1_datos <- data.frame(t = 1:100, y = y[401:500])
g <- ggplot(data = ar1_datos, aes(x = t, y = y))
g + geom_line()

pacf(ar1_datos$y, lag.max = 20, main = "PACF AR(1)")

# 2. Simulacion proceso AR(2)

polyroot(c(1, -1.1, 0.18))

set.seed(12345)
epsilon <- rnorm(500)
y <- rep(0, 500)
y[1] <- 1; y[2] <- 2
for (i in 3:500) {
        y[i] <- 1.1 * y[i - 1] - 0.18 * y[i - 2] + epsilon[i]
}

ar2_datos <- data.frame(t = 1:100, y = y[401:500])
g <- ggplot(data = ar2_datos, aes(x = t, y = y))
g + geom_line()

pacf(ar2_datos$y, lag.max = 20, main = "PACF AR(2)")

# 3. Simulacion proceso ARMA(1, 1)

set.seed(12345)
epsilon <- rnorm(500)
y <- rep(0, 500)
y[1] <- 1
for (i in 2:500) {
        y[i] <- 0.8 * y[i - 1] + epsilon[i] + 0.9 * epsilon[i - 1]
}

arma11_datos <- data.frame(t = 1:100, y = y[401:500])
g <- ggplot(data = arma11_datos, aes(x = t, y = y))
g + geom_line()

acf(arma11_datos$y, lag.max = 20, main = "ACF ARMA(1, 1)")

pacf(arma11_datos$y, lag.max = 20, main = "PACF ARMA(1, 1)")

# 4. Simulacion proceso MA(1) no invertible

set.seed(12345)
theta <- 1.5
epsilon <- rnorm(101)
y <- rep(0, 101)
for (i in 2:101) {
        y[i] <- epsilon[i] + theta * epsilon[i - 1]
}
acf(y[2:101], lag.max = 20, main = "ACF MA(1) no invertible")

epsilon_n <- rnorm(101, sd = 1 / theta)
y_n <- rep(0, 101)
for (i in 2:101) {
        y_n[i] <- epsilon_n[i] + (1 / theta) * epsilon_n[i - 1]
}
acf(y_n[2:101], lag.max = 20, main = "ACF MA(1) Invertible")

# 5. Estimación proceso AR(1)

set.seed(12345)
epsilon <- rnorm(500)
y <- rep(0, 500)
y[1] <- 1
for (i in 2:500) {
        y[i] <- 0.8*y[i - 1] + epsilon[i]
}
ar1_mod <- arima(y[401:500], order = c(1, 0, 0), method = "CSS", include.mean = FALSE)
ar1_mod

# 6. Estimacion proceso MA(1)

set.seed(12345)
epsilon <- rnorm(101, sd = 2)
y <- rep(0, 101)
for (i in 2:101) {
        y[i] <- epsilon[i] + 0.7 * epsilon[i - 1] 
}
ma1_mod <- arima(y[2:101], order = c(0, 0, 1), method = "ML", include.mean = FALSE)
ma1_mod

# 7. Estimacion proceso ARMA(1, 1)

set.seed(12345)
epsilon <- rnorm(500, sd = 2)
y <- rep(0, 500)
y[1] <- 1
for (i in 2:500) {
        y[i] <- 0.6 * y[i - 1] + epsilon[i] + 0.3 * epsilon[i - 1]
}
arma11_mod <- arima(y[401:500], order = c(1, 0, 1), method = "ML", include.mean = FALSE)
arma11_mod

# 8. Diagnóstico
acf(arma11_mod$residuals, lag.max = 30, main = "ACF de los residuos del modelo ARMA(1, 1)")

for (i in c(1, 5, 10, 20)) {
        p_test <- Box.test(arma11_mod$residuals, lag = i, type = "Box-Pierce")
        print(p_test$p.value)
}

for (i in c(1, 5, 10 , 20)) {
        p_test <- Box.test(arma11_mod$residuals, lag = i, type = "Ljung-Box")
        print(p_test$p.value)
}

# 9. Proyeccion MA(1)

set.seed(12345)
epsilon <- rnorm(101)
y <- rep(0, 101)
for (i in 2:101) {
        y[i] <- epsilon[i] + 0.75 * epsilon[i - 1]
}
ma1_mod <- arima(y[2:101], order = c(0, 0, 1), include.mean = FALSE, method = "ML")
ma1_for <- predict(ma1_mod, n.ahead = 10)

# 10. Proyeccion MA(2)
polyroot(c(1, 0.8, -0.1))

set.seed(12345)
epsilon <- rnorm(102)
y <- rep(0, 102)
for (i in 3:102) {
        y[i] <- epsilon[i] + 0.8 * epsilon[i - 1] - 0.1 * epsilon[i - 2]
}
ma2_mod <- arima(y[3:102], order = c(0, 0, 2), include.mean = FALSE, method = "ML")
ma2_for <- predict(ma2_mod, n.ahead = 10)

# 11. Proyeccion AR(1)

set.seed(12345)
epsilon <- rnorm(500)
y <- rep(0, 500)
y[1] <- 1
for (i in 2:500) {
        y[i] <- 0.85 * y[i - 1] + epsilon[i]
}
ar1_mod <- arima(y[401:500], order = c(1, 0, 0), method = "CSS", include.mean = FALSE)
ar1_for <- predict(ar1_mod, n.ahead = 50)
ar1for_datos <- data.frame(t = 1:50, pred = as.numeric(ar1_for$pred))
g <- ggplot(data = ar1for_datos, aes(x = t, y = pred))
g + geom_line() + labs(x = "Horizonte de Pronóstico", y = "Pronóstico")

# 12. Proyeccion AR(2)

set.seed(12345)
epsilon <- rnorm(500)
y <- rep(0, 500)
y[1] <- 1; y[2] <- 2
for (i in 3:500) {
        y[i] <- 1.1 * y[i - 1] - 0.18 * y[i - 2] + epsilon[i]
}
ar2_mod <- arima(y[401:500], order = c(2, 0, 0), method = "CSS", include.mean = FALSE)
ar2_for <- predict(ar2_mod, n.ahead = 50)
ar2for_datos <- data.frame(t = 1:50, pred = as.numeric(ar2_for$pred))
g <- ggplot(data = ar2for_datos, aes(x = t, y = pred))
g + geom_line() + labs(x = "Horizonte de Pronóstico", y = "Pronóstico")

# 13. Proyeccion ARMA(1, 1)

set.seed(12345)
epsilon <- rnorm(500)
y <- rep(0, 500)
y[1] <- 1
for (i in 2:500) {
        y[i] <- 0.55 * y[i - 1] + epsilon[i] - 0.95 * epsilon[i - 1]
}
arma11_mod <- arima(y[401:500], order = c(1, 0, 1), include.mean = FALSE, method = "ML")
arma11_for <- predict(arma11_mod, n.ahead = 50)
arma11for_datos <- data.frame(t = 1:50, pred = as.numeric(arma11_for$pred))
g <- ggplot(data = arma11for_datos, aes(x = t, y = pred))
g + geom_line() + labs(x = "Horizonte de Pronóstico", y = "Pronóstico")

# 14. RMSE ARMA(1, 1)

set.seed(12345)
epsilon <- rnorm(510)
y <- rep(0, 510)
y[1] <- 1
for (i in 2:510) {
        y[i] <- 1.2 + 0.5 * y[i - 1] + epsilon[i] + 0.8 * epsilon[i - 1]
}
arma11_mod <- arima(y[401:500], order = c(1, 0, 1), method = "ML")
h <- 10
arma11_for <- predict(arma11_mod, n.ahead = h)
RMSE_arma11 <- sqrt(mean((y[501:510] - as.numeric(arma11_for$pred))^2))
print(paste("El RMSE del modelo ARMA(1,1) es", RMSE_arma11))
