# Graficas-pie-y-barras
library(foreign)
sociodemo <- read.dbf("C:\\Users\\SALA-C32\\Downloads\\SDEMT215.dbf")
View(sociodemo)

##PARA RAELIZAR UNA BASE 

precod <- subset(sociodemo, (R_DEF = 00) && (C_RES = 1 || C_RES=3) && (EDA>= 15 && EDA<= 98))
precod
#para pasar variables de caracteres a numericas:

sociodemo$R_DEF1 <- as.numeric(as.character(sociodemo$R_DEF))
sociodemo$C_RES1 <- as.numeric(as.character(sociodemo$C_RES))
sociodemo$EDA1 <- as.numeric(as.character(sociodemo$EDA))

precod <- subset(sociodemo, ((R_DEF1 == 0) & (C_RES1 == 1 | C_RES1 ==3) & (EDA1 >= 15 & EDA1 <= 98)), select = c(EDA1, SEX, HRSOCUP, CLASE2, CLASE1, CLASE3))
precod
View(precod)
table (sociodemo$R_DEF1)
table (precod$R_DEF1)
table (precod$C_RES1)
table (precod$EDA1)




attach(precod)
CLASE2V1 <- table(CLASE2)
CLASE2V1
hist(CLASE2, )
hist(CLASE2V1, main= "Grafica 1. Distribución de la Población de 15 años o más, 2015")
hist(CLASE2V1, main= "Grafica 1. Distribución de la 
     Población de 15 años o más, 2015",
     xlab = "Tipo de ocupada", ylab = "Población (miles)", 
     xlim = c(0,2), ylim = c(0,200000), border = T, pch = 18, 
     col = 100)
hist(CLASE2, main= "Grafica 1. Distribución de la 
     Población de 15 años o más, 2015",
     xlab = "Tipo de ocupada", ylab = "Población (miles)", 
     xlim = c(1,4), ylim = c(0,200000), border = T, pch = 5, 
     col = "yellowgreen")


##CLASE 19/02/2016
## grafico de pie!!

sd1 <- subset(sociodemo, CLASE2 == 1, select = c(EDA,SEX,HRSOCUP,CLASE2, CLASE1, CLASE3, EDA5C,IMSSISSSTE))
frec <- table(sd1$IMSSISSSTE)
frec
help(pie)

pie(frec)

#labels le ponemos la etiqueta en forma de vector

pie(frec, labels = c("IMSS", "ISSSTE", "Otra", "No Recibe", "No Especificado"))

#main para poner titulo

pie(frec, labels = c("IMSS", "ISSSTE", "Otra", "No Recibe", "No Especificado"), main = 
      "Institución de Atención Médica")

#col  es para poner color a cada sector en este caso es un vector

pie(frec, labels = c("IMSS", "ISSSTE", "Otra", "No Recibe", "No Especificado"), main = 
      "Institución de Atención Médica", col = c("green", "red", "blue", "gray", "yellow"))

library(plotrix)
install.packages("plotrix")
pie3D(frec, labels = c("IMSS", "ISSSTE", "Otra", "No Recibe", "No Especificado"), main = 
      "Institución de Atención Médica", col = c("green", "red", "blue", "gray", "yellow"))


#GRAFICA DE BARRAS

frec1 <- table(sd1$EDA5C)
barplot(frec1)
barplot(frec1, main = "Cinco grupos de edad", col = c("green", "red", "blue", "gray", "yellow", "brown"))

frec
#Las etiquetas se pueden poner con names.arg

barplot(frec1, main = "Grafica 1. Cinco grupos de edad", col = c("green", "red", "blue", "gray", "yellow", "brown"),
        names.arg = c("Menor", "14 a 24", "25 a 44", "45 a 64", "65 y más", "N.E."),
        ylim = c(0,85000), xlab = "Edad", ylab = "Población")

frec1

names(frec1) <- c("Menor", "14 a 24", "25 a 44", "45 a 64", "65 y más", "N.E.")
frec1


#Memory se usa para medir el tamaño de la memoria

memory.size()
