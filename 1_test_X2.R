###CARGUEMOS LOS PAQUETES QUE USAREMOS

library(MASS)

###      A. LOS DATOS QUE USAREMOS

#  A.1 Tomaremos como ejemplo varios datos conjuntos de datos:

#  el data frame "survey" que está incluido en el paquete `MASS`
#  survey lista las respuestas de un grupo de 237 estudiantes
#  en una encuesta.
#  Revisemos su contenido

summary(survey)
View(survey)

#  Si no tienes el paquete `MASS` por favor carga la tabla survey.csv usando:

#survey <- read.csv("datos/survey.csv",header=TRUE,row.names = 1,stringsAsFactors = TRUE)

#  Trabajaremos con varias columnas del data frame:

#  Sex (Sexo biologico)
#  W.Hnd (Cual es tu mano dominante)
#  Clap (¡Aplaude! ¿Qué mano tienes arriba?)


#  A.2 Datos de pobreza de CONEVAL
# https://www.coneval.org.mx/Paginas/principal.aspx (Consulta: 06 de agosto de 2019).
#  Población por condición de pobreza y tipo de localidad (rural o urbana)
#  en México. Unidad de medida: Miles de personas


pobreza<-read.table("datos/pobreza.csv", header=TRUE, sep=",", row.names=1)



###      B. CREANDO UNA TABLA DE CONTINGENCIAS

#  B.1 Las siguientes tablas de contingencias relacionan dos variables

#  La mano dominante con el sexo

sex_hand<-table(survey$Sex,survey$W.Hnd)

#  La mano que queda arriba al aplaudir y la mano dominante

hand_clap<-table(survey$W.Hnd,survey$Clap)

#  Ciudadanos en condición de pobreza

pobreza_m<-as.matrix(t(pobreza))

pobreza_t<-as.table(pobreza_m)

#  B.2 Hagamos represenaciones gráficas de estas tablas


mosaicplot(sex_hand, # Tabla de contingencias a graficar
           main= "Mano dominante y sexo",
           xlab="Sexo",
           ylab="Mano dominante",
           col=c(2,3,4,5),
           cex.axis=1.2,
           )


mosaicplot(hand_clap,
           main= "Mano dominante y aplauso",
           xlab="Mano dominante",
           ylab="Mano arriba en aplauso",
           col=c(2,3,4,5),
           cex.axis=1.2,
           )


mosaicplot(pobreza_t,
           main= "Pobreza rural y urbana",
           xlab="Tipo de localidad",
           ylab="Situación",
           cex.axis=1.2,
           las=2,
           col=c(2,3,4,5)
           )





###      C. Test de independencia chi-cuadado
#
#        )  (
#        ( ) )
#          ( (
#      _______)_
#   .-'---------|
#  ( C| Yo <3 R |                       (based on: mrf)
#   '-.         |
#     '_________'
#      '-------'


#      El test de X2 evalúa si la independencia entre las variables:
#      ¿Los valores de ambas variables categóricas son independientes?
#      Hipótesis nula (H0) o, Por el contrario, los valores de una
#      variable dependen de los valores de la otra variable


#  C.1 Llamando a la función chisq.test en varios contextos

#  C.1.a Tabla de contingencia de 2 x 2

chi_sex_hand<-chisq.test(sex_hand)

#  Aquí se nos avisa que se llevó a cabo la corrección de
#  Continuidad de Yates. Esta se usa en las tablas de 2x2
#  Y sirve para pevenir la sobre-estimación de la significancia
#  estadística cuando los datos son pequeños.

#  C.1.b Tabla de contingencias mayor a 2 x 2

chi_pobreza<-chisq.test(pobreza_t)

#  C.1.c Tabla de contingencias con cuentas pequeñas

chi_hand_clap<-chisq.test(hand_clap)

#  Aquí se nos advierte que nuestros resultados podrían
#  ser incorrectos. En general se considera que el test chi cuadrada
#  deja de ser adecuado cuando el valor esperado para
#  alguna de las celdas de la tabla es menor a 5.


#  C.2 Explorando el test de chisq.test

#  C.2.1 Qué valor de X se obtuvo, grados de libertad
#  y qué valor p le corresponde

chi_sex_hand

chi_hand_clap

#  C.2.2 Valores esperados si no hay asociación de variables
#  y valores observados

chi_sex_hand$observed

chi_sex_hand$expected

chi_pobreza$observed

chi_pobreza$expected

#  C.2.3 Residuales

chi_sex_hand$residuals

chi_pobreza$residuals

mosaicplot(pobreza_t, # Tabla de contingencias a graficar
           main= "Pobreza rural y urbana",
           xlab="Tipo de localidad",
           ylab="Situación",
           cex.axis=1.2, # Aumentar tamaño de etiquetas
           las=2,
	   shade=TRUE
           )

library(corrplot)

corrplot(chi_pobreza$residuals, is.cor = FALSE)


# ¿Cuánto contribuye cada celda al valor total de
#  chi-cuadrado?


contrib <- 100*chi_pobreza$residuals^2/chi_pobreza$statistic

corrplot(contrib, is.cor=FALSE)



###      D. Test de Bondad de ajuste chi-cuadrado

#  ¿Se ajusta lo observado a un panorama en que la probabilidad
#  de estar en diferentes situaciones económico-sociales es similar?

pobreza_u<-as.table(pobreza$Urbano)

names(pobreza_u)<-rownames(pobreza)

chi_pobreza_u<-chisq.test(pobreza_u, p=c(0.25,0.25,0.25,0.25))

mosaicplot(pobreza_u, las=2, dir="h")

chi_pobreza_u$expected

chi_pobreza_u$observed

chi_pobreza_u$residuals



###      D. Test exacto de Fisher

#  Este test se usa cuando hay celdas de la tabla de contingencias
#  con cuentas pequeñas, de forma que su valor esperado
#  en el test de chi-cuadrado seria igual o menor a 5.
#  Tiene las mismas hipótesis que el test chi-cuadrado.

fisher.test(hand_clap)




######### EJERCICIOS Y TRUCOS ADICIONALES ###################################


#___ Midiendo el tamaño del efecto de asociacion  ___#

#  El valor de chi-cuadrada o el p.value no nos dicen por si mismos
#  Que tan fuerte es la asociacion entre las variables
#  Sino solamente que la asociacion es significativa
#  Es por eso que se hacen otros cálculos para medir el tamaño
#  del efecto


#  Tamaño del efecto para tablas de 2x2


infartos <- matrix(c(189, 104, 10845, 10933), nrow = 2)
dimnames(infartos) <- list("Grupo" = c("Placebo","Aspirina"), "infartos" = c("Si","No"))

infartos_p<-prop.table(infartos, margin = 1)

# riesgo relativo de infarto en placebo vs. infarto con aspirina

infartos_p[1,1]/infartos_p[2,1]

#  Hay un riesgo de infarto 1.81 veces mayor para los que toman el placebo
#  que para los que toman aspirina


# riesgo relativo de ser mujer zurda vs hombre zurdo

sex_hand_p<-prop.table(sex_hand, margin = 1)

sex_hand[1,1]/sex_hand[2,1]

#  hay un riesgo de ser zurdo de 0.7 para las mujeres en relación con
#  los hombres


#  Si queremos saber si estos riesgos son significativos debemos calcular un intervalo de confianza

#  install.packages("epitools") # Tal vez necesites instalar a epitools

library(epitools)


#  La funcion riskratio de epitools espera este orden de columnas y filas:

#                         disease=0   disease=1
#         tratamiento=0 (ref)    n00         n01
#         tratamiento=1          n10         n11


#  En el caso de la tabla de infartos tendremos que voltear las columnas

riskratio(infartos, rev="b")

#  El riesgo relativo es de 1.81 y su intervalo de confianza no pasa por
#  cero. Tenemos un efecto significativo.

oddsratio(infartos, rev="b")

#  El riesgo relativo es de 1.81 y su intervalo de confianza no pasa por
#  1. Esto quiere decir que por cada infarto con aspirina hay 1.83
#  infartos con el Placebo. Para muestras pequeñas el risk ratio y el
#  odds ratio suelen ser parecidos.

oddsratio(sex_hand, rev="b")

#  El riesgo relativo es 0.69 y su intervalo de confianza no pasa por
#  cero. Tenemos un efecto significativo, pero el test de asociación
#  de variables chi-cuadrado no es significativo.



#  C.1 Tamaño del efecto para tablas mayores a 2x2

#  La V de Cramer, que sirve para medir el tamaño del efecto en este
#  caso se calcula como:
#  V = sqrt(X^2 / [nobs * (min(ncols, nrows) – 1)])

install.packages("DescTools")


library("DescTools")

CramerV(pobreza_t, conf.level=0.95)

CramerV(hand_clap, conf.level=0.95)


# Interpretación de la V de cramer

#	df*	pequeño	mediano	grande   <- Efecto
#	1	0.10	0.30	0.50
#	2	0.07	0.21	0.35
#	3	0.06	0.17	0.29
#	4	0.05	0.15	0.25
#	5	0.04	0.13	0.22

#  *grados de libertad
