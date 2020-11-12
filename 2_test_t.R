###CARGUEMOS LOS PAQUETES QUE USAREMOS

library("ggplot2")
library("MASS")


#      A. Conjuntos de datos que utilizaremos


#  A.1 Peso de ratones segun dieta A o B

#   Los pesos de ambos grupos provienen de distribuciones normales
#   con la misma sd (y por lo tanto, lo misma varianza)
#   Solo varía el promedio del peso.

A = rnorm(30, mean=5) # Pesos ratones - dieta A
B = rnorm(30, mean=10) # Pesos ratones - dieta B

peso<-c(A,B) # Pesos A y B

dieta<-as.factor(c(rep("A",30),rep("B",30))) # etiquetas de dieta A o B

#   Data frame que relaciona el peso con la dieta

peso_dieta<-data.frame(peso,dieta)


#  A.2 Altura segun sexo (dataframe Survey del paquete MASS)

summary(survey)

#   Tomaremos las columnas Height (altura) y Sex (sexo) de survey
#   Para construir un dataframe que relaciona la altura con el sexo
#   Este tipo de caracteristica (altura) suele seguir una distribucion
#   normal

altura_sexo<-data.frame(
             altura=survey$Height, 
             sexo=as.factor(survey$Sex)
             )

#   Vamos a eliminar las filas que tienen valores NA en alguna columna

altura_sexo<-subset(altura_sexo, !is.na(altura))
altura_sexo<-subset(altura_sexo, !is.na(sexo))

summary(altura_sexo)


#   Vamos a separar los valores para cada grupo en vectores
#   pues nos serviran para varios calculos

Female<-altura_sexo[altura_sexo$sexo=="Female", 1]
Male<-altura_sexo[altura_sexo$sexo=="Male", 1]


#  A.3 Cantidad de usuarios de Internet conectados a un
#  servidor por minuto (dataset WWWusage, R). 

#  este tipo de datos suele seguir una distribucion Poisson


usage<-data.frame(users=as.numeric(WWWusage))



#      B. ¿Se cumplen los supuestos del test t?


#  B.1 Normalidad - por inspeccion visual y prueba estadistica

#   B.1.a Observando la distribucon de los datos ¿Se ve como campana?

library("ggplot2")

ggplot(peso_dieta, aes(x=peso, fill=dieta, y=..density..)) + 
geom_histogram(position="identity")+
geom_density(alpha=0.04)

ggplot(altura_sexo, aes(x=altura, fill=sexo, y=..density..)) + 
geom_histogram(position="identity")+
geom_density(alpha=0.04)

ggplot(data=usage, aes(x=users, y=..density..)) + geom_histogram(bins=20, position="identity") + geom_density()


#   B.1.b Observando la correlación entre los cuantiles de 
#   los datos y los cuantiles de una distribución normal
#   mediante Quantile-Quantile (Q-Q) plots

library("ggpubr")

ggqqplot(peso_dieta, x="peso", facet.by="dieta")

ggqqplot(altura_sexo, x="altura", facet.by="sexo")

ggqqplot(usage$users)



#   B.1.c Evaluando la hipotesis de normalidad mediante el 
#   test de Shapiro-Wilk

#   La hipotesis nula (H0) de este test es que la distribucion
#   de los datos es normal. Un valor p no significativo
#   apunta a que la distribución es normal.

#   Este test tiende a ser muy laxo cuando el tamaño de la
#   muestra es pequeño, por lo que hay que combinarlo con
#   inspección visual. Además es sensible a outliers.


shapiro.test( A )

shapiro.test( B )


shapiro.test( Female )

shapiro.test( Male )


#  B.2 Igualdad de varianzas (homocedasticidad)


#  Los tests estadisticos de igualdad de varianzas tienen como
#  hipotesis nula y alternativa:

#   HO: la varianza es igual entre los grupos
#   H1: la varianza es distinta entre los grupos

#   Así pues, en los siguientes tests esperamos una p no significativa


#   B.2.a Si estamos seguros de que los datos de ambos grupos 
#   provienen de una distribucion normal podemos utilizar el test F
#   -contraste de la razón de varianzas-

var.test(peso ~ dieta, data = peso_dieta)

var.test(altura ~ sexo, data = altura_sexo)

#  En el segundo caso la p es < 0.05 por lo cual se acepta la hipótesis
#  alternativa de que las varianzas son distintas entre los grupos.


#   B.2.b El test de Bartlett permite contrastar las varianzas de dos o
#   mas grupos sin la necesidad de que el tamaño de los grupos 
#   sea el mismo. Es adecuado para datos normales.
  
bartlett.test(peso ~ dieta, data = peso_dieta)

bartlett.test(altura ~ sexo, data = altura_sexo)



#  B.3 No presencia de outliers


#   B.3.a Criterio de rango intercuartil (IQR)
#   Todo lo que este 1.5*IQR por encima del tercer cuartil o 1.5*IQR por
#   debajo del primer cuartil se considera outlier potencial 
#   El IQR es la distancia entre el primer y el tercer cuartil


ggplot(altura_sexo, aes(x=sexo, y=altura)) + geom_boxplot(outlier.colour="red") + 
geom_jitter(position=position_jitter(0.2))


#   Encontrando los limites superior e inferior segun este criterio

#   En este caso tuvimos un solo outlier en el extremo inferior
#   delgrupo Female

inf_f<-quantile(Female,0.25) - 1.5*IQR(Female)


#   Eliminando el outlier en el grupo Female

altura_sexo<-altura_sexo[!(altura_sexo$altura <inf_f),]

Female<-Female[Female>=lim_inf]


#   Para eliminar los outliers en ambos extremos de ambos grupos

#   Para aplicar el criterio a los extremos superior e inferior
#   de ambos grupos

   inf_f=quantile(Female,0.25) - 1.5*IQR(Female)
   sup_f=quantile(Female,0.75) + 1.5*IQR(Female)
   inf_m=quantile(Male,0.25) - 1.5*IQR(Male)
   sup_m=quantile(Male,0.75) + 1.5*IQR(Male)

altura_sexo<-altura_sexo[!(

(altura_sexo$altura < inf_f && altura_sexo$sexo=="Female") &&
(altura_sexo$altura > sup_f && altura_sexo$sexo=="Female") &&
(altura_sexo$altura < inf_m && altura_sexo$sexo=="Male") &&
(altura_sexo$altura > sup_m && altura_sexo$sexo=="Male")

),]


#   Distribución tras la eliminación del outlier

ggplot(altura_sexo, aes(x=sexo, y=altura)) + 
geom_boxplot(outlier.colour="red") + 
geom_jitter(position=position_jitter(0.2))


#  Hay varios otros criterios y pruebas estadísticas para identificar
#  outliers. Varios ejemplos se muestran en Ejercicios y Trucos
#  adicionales.




#      C. Realizando el test t

#      EL test compara promedios, ya sea con un valor fijo o entre 
#      dos grupos independientes

#      Queremos saber si el promedio de un grupo es 
#      Significativamente distinto a un valor

t.test(A, mu = 3)

#      O si es significativamente mayor a un valor

t.test(A, mu=3, alternative="greater")

t.test(Female, mu = 1.50, alternative="greater")


#      Ahora queremos saber si los promedios de ambos grupos 
#      son significativamente distintos entre si

t.test(peso~dieta, peso_dieta, var.equal=TRUE)

t.test(altura~sexo, altura_sexo)

# Si en t.test se utiliza la opcion: 
# var.equal=TRUE el calculo se vuelve igual al ajuste al modelo lineal:
# lm(size~group, size_group)
# el t-test es un caso particular del ajuste a un modelo lineal
# var.equal=FALSE por defecto y provoca que se aplique el test t de Welch
# En el cual se hace un ajuste de grados de libertad para contender
# con la diferencia de varianzas de los grupos.


#      Tambien podemos usar la siguiente formula

t.test(x=A, y=B, var.equal=TRUE)

t.test(x=Female, y=Male)

#      Si queremos saber si el promedio de x es mayor al de y

t.test(x=Female, y=Male, alternative="greater")

#      Y si solo nos interesa que el promedio de x sea menor al de y

t.test(x=Female, y=Male, alternative="less")



#      D. Tamaño del efecto para el test t

# install.packages("rstatix")  # Puede que necesites instalar a rstatix

library("rstatix")

#      La función cohens_d solo recibe data frames, así que para medir
#      el efecto para un solo grupo transformaremos al vector A

#      Efecto de la diferencia entre el promedio de A y 3

A_df<-as.data.frame(A)

cohens_d(A_df, A ~ 1  , mu=3, ci=TRUE)


#      Efecto de la diferencia entre el promedio de A y 3

cohens_d(peso_dieta, peso ~ dieta, var.equal = TRUE, ci=TRUE)

cohens_d(altura_sexo, altura ~ sexo, var.equal = FALSE, ci=TRUE)

#      En todos los casos tenemos un efecto alto (ver columna magnitude)
#      cuyo intervalo de confianza no pasa por cero.



######### EJERCICIOS Y TRUCOS ADICIONALES ###################################


#___ Evaluando la homogeneidad de varianzas con el test de Levene  ___#

#   Un test ampliamente usado es el test de Levene. Puede comparar
#   dos o mas grupos y no se requiere que los datos se distribuyan
#   de forma normal. Sus hipótesis son:

#   HO: la varianza es igual entre los grupos
#   H1: la varianza es distinta entre los grupos

#   Por lo que esperamos una p > 0.05 para aceptar la igualdad de varianzas


# install.packages("car") # Puede que debas instalar al paquete car
library("car")

leveneTest(peso ~ dieta, data = peso_dieta)

leveneTest(altura ~ sexo, data = altura_sexo)


#___ Outliers: criterios y pruebas estadísticas para su detección  ___#


#   Considerar outlier los datos que queden fuera del 
#   rango que va del percentil 2.5 al percentil 97.5


limite_inf <- quantile(Female, 0.025)

limite_sup <- quantile(Female, 0.975)

outliers<-which(Female < limite_inf | Female > limite_sup)

Female[outliers]



#   Criterio de Hampel, en el que se usa la desviacion absoluta
#   de la mediana (MAD) y se considera outlier lo que esta 3 MAD
#   por encima o por debajo de la mediana


limite_inf <- median(Female) - 3 * mad(Female)

limite_sup <- median(Female) + 3 * mad(Female)

outliers<-which(Female < limite_inf | Female > limite_sup)

Female[outliers]


#   Tests de Grubb y Dixon

# install.packages("outliers") Tal vez debas instalar el paquete outliers
library("outliers")


#   El test de Grubb determina si el valor más alto o más bajo
#   Del conjunto de datos es un outlier
#   NOTA: Este test no es recomendable el tamaño de la muestra n <= 6

#   H0: El valor más alto/bajo no es un outlier
#   H1: El valor más alto/bajo es un outlier

#   Test para el valor más bajo
grubbs.test(A) 
#   Test para el valor más alto
grubbs.test(A, opposite=TRUE)


#   El test de Dixon Tambien evalúa si el dato mas alto/bajo
#   Es un outlier y se utiliza cuando el tamaño de la muestra n <=25

#   Test para el valor más bajo y más alto

dixon.test(A)

dixon.test(A, opposite =TRUE)

#   Testt de Rosner

#   Este test detecta varios outliers a la vez. Está diseñado para
#   resolver el problema de enmascaramiento, donde dos outliers
#   se encuentran muy cerca entre si pueden no ser detectados.
#   El test es apropiado cuando el tamaño de muestra n >= 20

#   install.packages (EnvStats) # Tal vez debas instalar a EnvStats

library("EnvStats")

#  En el test indicamos cuantos outliers suponemos que hay
#  En este caso suponemos que hay 2 

rosner_A <- rosnerTest(A, k = 3)

rosner_A$all.stats



