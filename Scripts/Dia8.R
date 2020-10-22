# 1. Simulacion MCO

set.seed(12345)
epsilon1 <- rnorm(1000, mean = 0, sd = 2)
epsilon2 <- rnorm(1000, mean = 0, sd = 4)
x1 <- rep(0, 1000); x2 <- rep(0, 1000)
x1[1] <- 1; x2[1] <- -1
for (i in 2:1000) {
        x1[i] <- 1.3 + 0.8 * x1[i - 1] + epsilon1[i]
        x2[i] <- -1.2 - 0.6 * x2[i - 1] + epsilon2[i]
}
epsilon <- rnorm(500); y <- rep(0, 500)
for (i in 1:500) {
        y[i] <- 0.5 + 2.3 * x1[500 + i] + 3.1 * x2[500 + i] + epsilon[i]
}

datos <- data.frame(y = y, x1 = x1[501:1000], x2 = x2[501:1000])

library(sandwich)
library(lmtest)

mco_mod <- lm(y ~ x1 + x2, data = datos)

coeftest(mco_mod, df = Inf, vcov = vcovHAC(mco_mod))

acf(mco_mod$residuals, lag.max = 20, main = "ACF Residuos", xlim = c(1, 20))

for (i in c(5, 10, 15, 20)) {
        port_test <- Box.test(mco_mod$residuals, lag = i, type = "Ljung-Box")
        print(port_test$p.value)
}

# 2. Caso de Estudio Expectativas Racionales
if (!file.exists("./Datos/mishkin.xls")) {
        url <- "http://fhayashi.fc2web.com/hayashi%20econometrics/ch2/mishkin.xls"
        download.file(url, destfile = "./Datos/mishkin.xls", mode = "wb")
}

library(readxl)

datos <- read_xls("./Datos/mishkin.xls", sheet = 1, range = "A1:G492")

datos$int_post <- (1 + datos$TB1 / 100) / (1 + datos$PAI1 / 100) - 1

acf(datos$int_post[36:258], lag.max = 20, main = "ACF Tasa real de interés ex-post 1953 - 1971", xlim = c(1, 20))

for (i in c(5, 10 , 15 , 20)) {
        port_test <- Box.test(datos$int_post[36:258], lag = i, type = "Ljung-Box")
        print(port_test$p.value)
}

mco_mod <- lm(PAI1 ~ TB1, data = datos)

resul_mco <- coeftest(mco_mod, df = Inf, vcov = vcovHAC(mco_mod))
resul_mco

z <- abs((0.63 - 1) / 0.11)
pnorm(z, lower.tail =FALSE)

acf(mco_mod$residuals, lag.max = 20, main = "ACF Residuos MCO", xlim = c(1, 20))