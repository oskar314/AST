# 1. Evolucion de billetes y monedas en Bolivia
library(ggplot2)
library(readxl)
##
datos <- read.csv("./Datos/billetes.csv") # Se procede a leer los datos
fecha <- as.Date(datos$ï..fecha,"%m/%d/%Y")# Convertir en formato fecha la variable fecha de la base de datos
datos2 <- data.frame(fecha=fecha,bm = datos$BM/1000000)
g <- ggplot(data=datos2,aes(x=fecha, y=bm)) 
g + geom_line() + labs(x="Fecha",y="Biletes y Monedas (billones Bs)") + geom_hline(yintercept=0)

## 2. Transformar los datos
datos2$lbm <- log(datos2$bm)
g <- ggplot(data = datos2, aes(x = fecha, y = lbm))
g + geom_line() + labs(x = "Fecha", y = "Billetes y Monedas en Logaritmos")

## 3. Eliminar tendencia
datos2$lbm_nt <- c(NA, diff(datos2$lbm))
n <- length(datos2$lbm)
g <- ggplot(data = datos2[2:n,], aes(x = fecha, y = lbm_nt))
g + geom_line()

## 4. Eliminar el componente estacional
datos2$lbm_ntns <- c(rep(NA, 12), diff(datos2$lbm_nt, lag = 12))
g <- ggplot(data = datos2[14:n,], aes(x = fecha, y = lbm_ntns))
g + geom_line()

## 5. Creacion de variables tiempo
datos2$t <- 1:n
datos2$t2 <- datos2$t^2
datos2$t3 <- datos2$t^3

## 6. Estimacion MCO de tendencia
mt_mod <- lm(lbm ~ t + t2 + t3, data = datos2) 
summary(mt_mod)

## 7. Eliminacion tendencia metodo MCO
datos2$lbm_nt2 <- mt_mod$residuals
g <- ggplot(data = datos2, aes(x = fecha, y = lbm_nt2))
g + geom_line()

## 8. Estimacion del componente tendencia
datos2$lbm_mt <- predict(mt_mod)
g <- ggplot(data = datos2, aes(x = fecha, y = lbm_mt))
g + geom_line()

## 9. Estimacion periodograma
# Intalamos el paquete requerido 
# install.packages("TSA")
library(TSA)
period <- periodogram(datos2$lbm_nt2)

period_datos <- data.frame(freq = period$freq, spec = period$spec)
head(period_datos, 40)

n * c(0.02, 0.09, 0.17, 0.25)

# 10. Creacion vector de frecuencias
omega_j <- c(0.02, 11 * (1:(n %/% 22)) / n)

# 11. Estimacion componente estacional
m <- ncol(datos2)
for (i in 1:length(omega_j)) {
        datos2[, m + i] <- cos(2*pi*omega_j[i]*datos2$t)
}
colnames(datos2)[11:17] <- paste(rep("W", 7), 1:7, sep = "")

m <- ncol(datos2)
for (i in 1:length(omega_j)) {
        datos2[, m + i] <- sin(2*pi*omega_j[i]*datos2$t)
}

colnames(datos2)[18:24] <- paste(rep("Z", 7), 1:7, sep = "")

st_mod <- lm(lbm_nt2 ~ W1 + W2 + W3 + W4 + W5 + W6 + W7 + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + Z7, data = datos2)
summary(st_mod)

# 12. Serie de tiempo sin componente tendencia ni estacional
datos2$lbm_ntns2 <- st_mod$residuals
g <- ggplot(data = datos2, aes(x = fecha, y = lbm_ntns2))
g + geom_line()

# 13. Creacion componente estacional
datos2$st <- predict(st_mod)
g <- ggplot(data = datos2, aes(x = fecha, y = st))
g + geom_line()

# 14. Estimacion de tendencia y componente estacional a través del metodo STL
lbm <- ts(datos2$lbm, start = c(2007, 1), end = c(2017, 12), frequency = 12)

decomp <- stl(lbm, s.window = 15, t.window = 15, s.degree = 1, t.degree = 1)

plot(decomp)

# 15. Simulacion proceso MA(1)
set.seed(12345)
epsilon <- rnorm(101)
y <- rep(0, 101)
for (i in 2:101) {
        y[i] <- 1.5 + epsilon[i] + 0.8 * epsilon[i - 1]  
}

ma1_datos <- data.frame(t = 1:100, y = y[2:101])
g <- ggplot(data = ma1_datos, aes(x = t, y = y))
g + geom_line()

acf(ma1_datos$y, lag.max = 20, main = "ACF MA(1)")

# 16. Simulación proceso MA(3)
set.seed(12345)
epsilon <- rnorm(103)
y <- rep(0, 103)
for (i in 4:103) {
        y[i] <- 2.3 + epsilon[i] + 1.3 * epsilon[i - 1] + 0.1 * epsilon[i - 2] - 3.6 * epsilon[i - 3]
}

ma3_datos <- data.frame(t = 1:100, y = y[4:103])
g <- ggplot(data = ma3_datos, aes(x = t, y = y))
g + geom_line()

acf(ma3_datos$y, lag.max = 20, main = "ACF MA(3)")

Ltest <- Box.test(ma3_datos$y, lag = 3, type = "Ljung-Box")
Ltest$p.value

