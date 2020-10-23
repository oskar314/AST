# 1. Caso de Estudio Retornos de Educación

if (!file.exists("./Datos/wage.xls")) {
        url <- "http://fhayashi.fc2web.com/hayashi%20econometrics/ch3/grilic.xls"
        download.file(url, destfile = "./Datos/wage.xls", mode = "wb")
}

library(readxl)
library(gmm)
library(ggplot2)

datos <- read_xls("./Datos/wage.xls", sheet = 1, range = "A1:T759")

View(datos)

g <- ggplot(data = datos, aes(x = S, y = LW))
g + geom_point() + stat_smooth(method = "lm", colour = "red")

datos$AGE2 <- datos$AGE^2

ols_mod <- gmm(LW ~ S + AGE + AGE2 + EXPR, ~ S + AGE + AGE2 + EXPR, data = datos)

summary(ols_mod)

ols_mod2 <- gmm(LW ~ S + AGE + AGE2 + EXPR + IQ, ~ S + AGE + AGE2 + EXPR + IQ, data = datos)
summary(ols_mod2)

iv_mod <- gmm(LW ~ S + AGE + AGE2 + EXPR + IQ, ~ S + AGE + AGE2 + EXPR + KWW, data = datos)
summary(iv_mod)

gmm_mod <- gmm(LW ~ S + AGE + AGE2+ EXPR + IQ, ~ S + AGE + AGE2 + EXPR + KWW + MED, data = datos)
summary(gmm_mod)