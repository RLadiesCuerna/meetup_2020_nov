
#      A. Conjuntos de datos que utilizaremos


#  A.1 Distribucion de la longitud del sepalo de flores de iris
#  dataset (iris de R)

#                   _
#          _-=-_   ::  _-=-_
# Petal   (__/ ¨"\ V /"¨ \__)
# Width |  .:II:>-\|/-<:II:.
#          ----- _/|\_        |
#     Petal     | ´'` |       | Sepal
#     Length     `._.´        | Length
#                  |
#                ------ Sepal
#                  |    Width
#


summary(iris)


#      ¿Se cumplen las suposiciones del test ANOVA?

#      Antes de realizar el test ANOVA tendremos que revisar nuevamente
#      que se cumplen los supuestos del test:
#      Distribución normal de datos, igualdad de varianzas y no outliers.

#      https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6802968/
#      https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3417410/



#  B.1  No presencia de outliers

library("ggplot2")

ggplot(iris, aes(x=Species, y=Sepal.Length)) + 
geom_boxplot(outlier.color="red")


#   Límite inferior para los datos de virginica

virginica<-iris[ iris[5] =="virginica" , 1]

inf_v<-quantile(virginica,0.25) - 1.5*IQR(virginica)


#   Eliminando el outlier identificado


irisf<-iris[ !( iris[1] < inf_v & iris[5] =="virginica" ),]


ggplot(irisf, aes(x=Species, y=Sepal.Length)) + 
geom_boxplot(outlier.color="red")

#   En el nuevo boxplot hay un nuevo outlier 


#   Eliminando el segundo outlier identificado


sup_v<-quantile(virginica,0.25) + 1.5*IQR(virginica)


irisf<-irisf[ !( irisf[1] > sup_v & irisf[5] =="virginica" ),]


ggplot(irisf, aes(x=Species, y=Sepal.Length)) + 
geom_boxplot(outlier.color="red")



#   B.2 Distribucion normal de datos


#    B.2.a ¿Cómo se ve la forma de las distribuciones?

ggplot(data=irisf,aes(x=Sepal.Length, y=..density..)) +
geom_histogram(position="identity") + 
geom_density() +
facet_grid(Species~.)


#   B.2.b Q-Q plots: correlación entre los cuantiles de 
#  los datos y los de una distribución normal

library("ggpubr")

ggqqplot(irisf, x="Sepal.Length", facet.by="Species")


#   B.2.c Test de normalidad de Shapiro-Wilk

#   La hipotesis nula (H0) de este test es que la distribucion
#   de los datos es normal. Un valor p no significativo
#   apunta a que la distribución es normal.

#   Aqui usamos la funcion by() para aplicar el test de shapiro.wilk
#   En los valores de longitud del sépalo una especie a la vez


by(irisf$Sepal.Length, irisf$Species, shapiro.test)



#   B.3 Igualdad de varianzas con el test de Barttlet
#   Este test se usa cuando los datos tienen distribución normal

#   HO: la varianza es igual entre los grupos
#   H1: la varianza es distinta entre los grupos


bartlett.test(Sepal.Length ~ Species, data = irisf)

#   El valor p significativo indica varianzas no homogeneas
#   ¿Qué tan distintas son las varianzas entre sí?

by(irisf$Sepal.Length, irisf$Species, var)




#      C. ANOVA de una via

#      El ANOVA de una via es una extension del test t
#      en el que se analizan si los promedios de 3 o más grupos de
#      datos son significativamente distintos entre si. Las hipótesis
#      nula y alternativa son:


#      H0: Los promedios de los diferentes grupos son iguales entre si
#      H1: El promedio de al menos un grupo es distinto al de los otros


#  C.1 Test ANOVA

#  La funcion aov y la funcion oneway.test obtienen el mismo
#  resultado siempre que en oneway.test se asuma que las varianzas
#  de los grupos son iguales


irisf_aov1<-aov(Sepal.Length~Species, data=irisf)

summary(irisf_aov1)

oneway.test(Sepal.Length~Species, data=irisf, var.equal=TRUE)


#  En el caso de irisf no se cumplio el supuesto de igualdad
#  de varianzas por lo que podemos usar el ANOVA de Welch,
#  que hace un ajuste tomando en cuenta la disimilitud de varianzas


irisf_aov2<-oneway.test(Sepal.Length~Species, data=irisf, var.equal=FALSE)


#  C.2 Comparaciones pareadas tras el ANOVA de una vía
#  Si el valor p del ANOVA fue significativo podemos realizar
#  Comparaciones pareadas entre los grupos para saber qué
#  par o pares de promedios fueron distintos entre si

#   C.2.a Comparaciones pareadas con el test de Tukey HSD
#   (Tukey Honest Significant Differences)
#   Este test requiere que la distribución de los datos sea
#   normal y las varianzas sean homogeneas entre grupos


TukeyHSD(irisf_aov1)

#   C.2.b Test t pareado y corregido por múltiples muestreos
#   mediante el método de Bonferroni.
#   También requiere que la distribución de los datos sea
#   normal. Si las varianzas son heterogeneas entre grupos
#   usar la opcion pool.sd=FALSE. Con esto se pasa al t-test
#   con correccion de Welch

pairwise.t.test(irisf$Sepal.Length, irisf$Species, pool.sd=FALSE,p.adj = "bonf")



#      D. ANOVA de dos vias

#      El ANOVA de dos vias se usa para evaluar simultaneamente
#      el efecto de dos factores (variables categóricas que 
#      dividen a los datos en grupos -niveles-) sobre la variable de 
#      respuesta (los datos numéricos)

#      Se evaluan varios pares de hipótesis nula | alternativa 
#      H0 | H1:

# No hay diferencia entre los promedios para el factor A | Si hay 
# No hay diferencia entre los promedios para el factor B | Si hay
# No hay interacción entre los factores A y B | Si hay

#  D.1 Datos del crecimiento de odontoblastos (células de los dientes) #  en cuyos que recibieron distintos suplementos de vitamina C 
#  (supp: OJ y VC) cada uno en tres dosis distintas (dose: 0.5, 1, 2.0)


summary(ToothGrowth) 

#  Creamos una copia del data frame con un nombre más fácil de
#  escribir

datos <- ToothGrowth

#  Convertimos en factor la columna dose y Renombramos 
#  los niveles para que no sean interpretados como números


datos$dose <- factor(datos$dose, 
                  levels = c(0.5, 1, 2),
                  labels = c("D1", "D2", "D3"))


#  Cuántos datos tenemos por grupo

table(datos$supp, datos$dose)


#  D.2 Supuestos del test ANOVA ¿Se cumplen?


#   D.2.a Normalidad


ggplot(datos, aes(x=len, fill=dose, y=..density..)) + 
facet_grid(supp~.) +
geom_density(alpha=0.4)

ggqqplot(datos, x="len", facet.by=c("supp","dose"))


#   HO: la distribución es normal
#   H1: la distribución no es normal

by(datos$len, datos[,c("supp","dose")], shapiro.test )


#   D.2.b Homogeneidad de varianzas

#   HO: la varianza es igual entre los grupos
#   H1: la varianza es distinta entre los grupos

supp_dose<-paste(datos$supp, datos$dose)
bartlett.test(datos$len, supp_dose)



#  D.3 ANOVA de dos vias

#  En este caso estamos evaluando la influencia de supp y dose
#  en la longitud (len) con un modelo aditivo. Estamos asumiendo
#  que las variables supp y dose son independientes entre si:

datos.aov <- aov(len ~ supp + dose, data = datos)

summary(datos.aov)

#  Los datos indican que tanto supp como dose provocan un cambio
#  significativo de promedios

#  Si pensamos que supp y dose tienen un efecto no aditivo, sino
#  sinérgico podemos evaluar cada variable por separado y también
#  la interacción de las variantes. Esto puede expresarse de las
#  siguientes formas:

datos.aov2 <- aov(len ~ supp * dose, data = datos)

datos.aov2 <- aov(len ~ supp + dose + supp:dose, data = datos)

#  El p value para la interacción es significativo. Esto es por que:
#  el suplemento de vitamina C que se usa (supp) cambia el tamaño 
#  promedio de los odontoblastos, no obstante, la diferencia entre
#  los dos suplementos es dependiente de la dosis. Cuando la dosis
#  es D2 (alta) se vuelve indetectable.

ggplot(datos, aes(y=len, fill=supp)) +
geom_boxplot() +
facet_grid(~dose)

#  Esto se va a ver reflejado en los análisis pareados


#  D.4 Test pareados tras el análisis de dos vías

#  Las varianzas son homogeneas por lo que podemos usar el test de
#  Tukey para hacer las pruebas pareadas.


TukeyHSD(datos.aov2)


######### EJERCICIOS Y TRUCOS ADICIONALES ###################################

#___ Comparaciones pareadas tras ANOVA con Tukey-Krammer  ___#

#   Si las varianzas o el tamaño de los grupos no es homogeneo
#   también podemos usar el test de Tukey Kramer de la siguiente forma

#install.packages("multcomp") #Tal vez tengas que instalar a multcomp

library("multcomp")

Tukey<-glht(irisf_aov1, linfct = mcp(Species = "Tukey"))

summary(Tukey)



#___ Estimación del tamaño del efecto para el ANOVA  ___#

#https://www.r-bloggers.com/2017/07/effect-size-statistics-for-anova-tables-rstats/


library("effectsize")
library("car")

# Eta cuadrada: ¿Qué porcentaje de la varianza es explicada?
# por diferentes variables


# Para un test de una vía

eta_squared(irisf_aov1)


# Para los tests de dos vías


eta_squared(datos.aov)


#  Aqui se está calculando la contribución de las variables a la 
#  varianza en forma secuencial. Esto es, se mide el efecto de supp
#  y una vez restado su efecto se mide el efecto de dose

eta_squared(datos.aov2)

#  Aqui en cambio se está tomando en cuenta el efecto de cada variable
#  controlando para las otras dos variables y sin importar su orden
#  de aparición.

model<-lm(len ~ supp * dose, data = datos)

eta_squared(car::Anova(model , type=3))

#  Para más información ver:
#  https://cran.r-project.org/web/packages/effectsize/vignettes/anovaES.html


