#1-Lectura y muestreo

redwine<-read.csv("winequality-red.csv", sep = ";")

whitewine<-read.csv("winequality-white.csv", sep = ";")

set.seed(311)

redwine <- redwine[sample(nrow(redwine), size=1000, replace=FALSE),]
whitewine <- whitewine[sample(nrow(whitewine), size=1000, replace=FALSE),]

#Este dataframe almacena la combinación de ambas muestras para procesamiento futuro
wine <- rbind(cbind(redwine, type = rep("Tinto",1000)),cbind(whitewine, type = rep("Blanco",1000)))

#2-Análisis estadístico de variables numéricas REVISAR SI LAS ESTADÍSTICAS TIENEN SENTIDO

#Nota: En caso de empate toma el primer valor
#Mode <- function(x) {
#  ux <- unique(x)
#  ux[which.max(tabulate(match(x, ux)))]
#}

cv <- function(x) {
  sd(x)/mean(x)
}

install.packages("moments")
library(moments)
install.packages("modeest")
library(modeest)

stats_table <- function(df) {
  min    = sapply(df,min)
  max    = sapply(df,max)
  mean   = sapply(df,mean)
  med    = sapply(df,median)
  mode   = sapply(df,mlv,method = "mfv")
  var    = sapply(df,var)
  std    = sapply(df,sd)
  cv     = sapply(df,cv)
  q1     = sapply(df,quantile,probs = 0.25)
  q3     = sapply(df,quantile,probs = 0.75)
  iqr    = sapply(df,IQR)
  mad    = sapply(df,mad)
  skew   = sapply(df,skewness)#Fisher: https://rdrr.io/cran/moments/src/R/skewness.R
  kurt   = sapply(df,kurtosis)
  stats  = rbind(min,max,mean,med,mode,var,std, cv, q1, q3, iqr, mad, skew,kurt)
}

r_stats = stats_table(redwine)
r_stats
w_stats = stats_table(whitewine)
w_stats
w_stats['mode','sulphates']

#3-Gráficos de variables
library(grDevices)
#colores con transparencias, para que luzca como vino pero también para que permita comparar ambas distribuciones
r_color = adjustcolor("deeppink4",alpha.f = 0.4)
w_color = adjustcolor("antiquewhite", alpha = 0.4)
c_list = c(r_color,w_color)

#calcular número de bins! 10logn = 70 (Dixon y Kronmal)
boxplot(fixed.acidity~type, data = wine, col = c_list, horizontal = TRUE, xlab = "Acidez fija", ylab = "Variedad de vino")
hist(redwine$fixed.acidity, col = r_color, xlab = "Acidez fija", ylab = "Frecuencia observada", 
     main = "Distribución acidez fija del vino", breaks = 70)
hist(whitewine$fixed.acidity, col = w_color, breaks = 70, add = TRUE)
legend("topright",cex=1, title="Variedad",c("Tinto","Blanco"),
       fill=c_list, horiz=F)

boxplot(volatile.acidity~type, data = wine, col = c_list, horizontal = TRUE, xlab = "Acidez volátil", ylab = "Variedad de vino")
hist(redwine$volatile.acidity, col = r_color, xlab = "Acidez volátil", ylab = "Frecuencia observada", 
     main = "Distribución acidez volátil del vino", breaks = 70)
hist(whitewine$volatile.acidity, col = w_color, breaks = 70, add = TRUE)
legend("topright",cex=1, title="Variedad",c("Tinto","Blanco"),
       fill=c_list, horiz=F)

boxplot(citric.acid~type, data = wine, col = c_list, horizontal = TRUE, xlab = "Ácido cítrico", ylab = "Variedad de vino")
hist(redwine$citric.acid, col = r_color, xlab = "Ácido cítrico", ylab = "Frecuencia observada", 
     main = "Distribución de la cantidad de ácido cítrico del vino", breaks = 70)
hist(whitewine$citric.acid, col = w_color, breaks = 70, add = TRUE)
legend("topright",cex=1, title="Variedad",c("Tinto","Blanco"),
       fill=c_list, horiz=F)

#tiene outliers, quizás usar boxplot
boxplot(residual.sugar~type, data = wine, col = c_list, horizontal = TRUE, xlab = "Azúcar residual", ylab = "Variedad de vino")
hist(redwine$residual.sugar, col = r_color, xlab = "Azúcar residual", ylab = "Frecuencia observada", 
     main = "Distribución de la cantidad de azúcar residual del vino", breaks = 70, xlim = c(0,25), ylim = c(0,300))
hist(whitewine$residual.sugar, col = w_color, breaks = 70, add = TRUE)
legend("topright",cex=1, title="Variedad",c("Tinto","Blanco"),
       fill=c_list, horiz=F)

boxplot(chlorides~type, data = wine, col = c_list, horizontal = TRUE, xlab = "Cloruros", ylab = "Variedad de vino")
hist(redwine$chlorides, col = r_color, xlab = "Cloruros", ylab = "Frecuencia observada", 
     main = "Distribución de la cantidad de cloruros", breaks = 70)
hist(whitewine$chlorides, col = w_color, breaks = 70, add = TRUE)
legend("topright",cex=1, title="Variedad",c("Tinto","Blanco"),
       fill=c_list, horiz=F)

boxplot(free.sulfur.dioxide~type, data = wine, col = c_list, horizontal = TRUE, xlab = "Anhídrido sulfuroso libre", ylab = "Variedad de vino")
hist(redwine$free.sulfur.dioxide, col = r_color, xlab = "Anhídrido sulfuroso libre", ylab = "Frecuencia observada", 
     main = "Distribución de anhídrido sulfuroso libre", breaks = 70, xlim = c(0,80))
hist(whitewine$free.sulfur.dioxide, col = w_color, breaks = 70, add = TRUE)
legend("topright",cex=1, title="Variedad",c("Tinto","Blanco"),
       fill=c_list, horiz=F)

boxplot(total.sulfur.dioxide~type, data = wine, col = c_list, horizontal = TRUE, xlab = "Anhídrido sulfuroso total", ylab = "Variedad de vino")
hist(redwine$total.sulfur.dioxide, col = r_color, xlab = "Anhídrido sulfuroso total", ylab = "Frecuencia observada", 
     main = "Distribución de anhídrido sulfuroso total", breaks = 70)
hist(whitewine$total.sulfur.dioxide, col = w_color, breaks = 70, add = TRUE)
legend("topright",cex=1, title="Variedad",c("Tinto","Blanco"),
       fill=c_list, horiz=F)

boxplot(density~type, data = wine, col = c_list, horizontal = TRUE, xlab = "Densidad", ylab = "Variedad de vino")
hist(redwine$density, col = r_color, xlab = "Densidad", ylab = "Frecuencia observada", 
     main = "Densidad", breaks = 70, xlim = c(0.985, 1.005), ylim = c(0,150))
hist(whitewine$density, col = w_color, breaks = 70, add = TRUE)
legend("topright",cex=1, title="Variedad",c("Tinto","Blanco"),
       fill=c_list, horiz=F)

boxplot(pH~type, data = wine, col = c_list, horizontal = TRUE, xlab = "pH", ylab = "Variedad de vino")
hist(redwine$pH, col = r_color, xlab = "pH", ylab = "Frecuencia observada", 
     main = "pH", breaks = 70)
hist(whitewine$pH, col = w_color, breaks = 70, add = TRUE)
legend("topright",cex=1, title="Variedad",c("Tinto","Blanco"),
       fill=c_list, horiz=F)

boxplot(sulphates~type, data = wine, col = c_list, horizontal = TRUE, xlab = "Sulfatos", ylab = "Variedad de vino")
hist(redwine$sulphates, col = r_color, xlab = "Sulfatos", ylab = "Frecuencia observada", 
     main = "Sulfatos", breaks = 70, xlim = c(0,2))
hist(whitewine$sulphates, col = w_color, breaks = 70, add = TRUE)
legend("topright",cex=1, title="Variedad",c("Tinto","Blanco"),
       fill=c_list, horiz=F)

boxplot(alcohol~type, data = wine, col = c_list, horizontal = TRUE, xlab = "Alcohol", ylab = "Variedad de vino")
hist(redwine$alcohol, col = r_color, xlab = "Alcohol", ylab = "Frecuencia observada", 
     main = "alcohol", breaks = 70)
hist(whitewine$alcohol, col = w_color, breaks = 70, add = TRUE)
legend("topright",cex=1, title="Variedad",c("Tinto","Blanco"),
       fill=c_list, horiz=F)

hist(redwine$quality, col = r_color, xlab = "Calidad", ylab = "Frecuencia observada", 
     main = "Calidad del vino", breaks = 70, ylim = c(0,500))
hist(whitewine$quality, col = w_color, breaks = 70, add = TRUE)
legend("topright",cex=1, title="Variedad",c("Tinto","Blanco"),
       fill=c_list, horiz=F)

#4-Tabla de frecuencia y porcentaje de calidad para cada tipo de vino
qualitytable <- table(wine$quality, wine$type)
qualitytable
pctqualitytable <- qualitytable/apply(qualitytable,2,sum) * 100
pctqualitytable

#5-Gráfico de las tablas del punto anterior
mosaicplot(t(qualitytable),ylab = "Calidad del vino", xlab = "Tipo de vino", main = "Calidad de vino por tipo")

barplot(t(qualitytable), col = c_list, beside = TRUE, 
        xlab = "Calidad del vino (medido del 0 al 10)", ylab = "Frecuencia observada")
legend("topright",cex=1, title="Variedad",c("Tinto","Blanco"),
       fill=c_list, horiz=F)

#6-Análisis de asociación entre dos variables continuas convertidas a categóricas
wine$cat_pH <- cut(wine$pH, breaks = c(0,3.1,3.4,4), labels = c('Bastante ácido','Algo ácido','Un poco ácido'))
wine$cat_fixed.acidity <- cut(wine$fixed.acidity, breaks = c(0,7,9,16), labels = c('Baja acidez fija','Media acidez fija','Alta acidez fija'))

library(stats)
D <- table(wine$cat_fixed.acidity,wine$cat_pH)
D
X2 <- chisq.test(D)
X2#rechazo independencia con alpha = 0.05

#7-Estimación de la diferencia de medias con una significación del 5%

var.test(sulphates~type, data = wine, alternative = 'two.sided')#varianzas diferentes

r_sulphates_mean = mean(redwine$sulphates)
w_sulphates_mean = mean(whitewine$sulphates)
r_sulphates_var = var(redwine$sulphates)
w_sulphates_var = var(whitewine$sulphates)

lowerbound = (r_sulphates_mean - w_sulphates_mean) - qnorm(0.975) * sqrt((r_sulphates_var + w_sulphates_var)/1000)
upperbound = (r_sulphates_mean - w_sulphates_mean) + qnorm(0.975) * sqrt((r_sulphates_var + w_sulphates_var)/1000)

print(paste("El intervalo de confianza de la media es: [", lowerbound, ";", upperbound, "]"))

#8-Test de hipótesis sobre las medias (t.test por defecto asume que las varianzas son diferentes)
t.test(sulphates~type , data = wine, alternative = "greater", conf.level = 0.95)

#9-Test de hipótesis sobre la calidad del vino 
var.test(quality~type, data = wine)#varianzas iguales
t.test(quality~type, data = wine, var.equal = TRUE)#no hay diferencia entre calidades de vino

#install.packages("BiocManager")
#BiocManager::install('mixOmics')
#install.packages("RVAideMemoire")
#library(RVAideMemoire)

#mood.medtest(quality~type, data = wine)

#10-Diferencias en la cantidad de alcohol según la calidad del vino
wine$cat_quality <- cut(as.numeric(wine$quality), breaks = c(0,5,7,10), labels = c('Calidad baja','Calidad media','Calidad alta'))

boxplot(alcohol~cat_quality,data=wine,xlab="Calidad",ylab="Alcohol")

#se rechaza la hipótesis de que las distribuciones sean normales
shapiro.test(wine[wine$cat_quality == "Calidad baja",'alcohol'])
shapiro.test(wine[wine$cat_quality == "Calidad media",'alcohol'])
shapiro.test(wine[wine$cat_quality == "Calidad alta",'alcohol'])

#se rechaza la hipótesis de que las muestras son de poblaciones iguales
kruskal.test(alcohol~cat_quality, data = wine)

#install.packages("coda")
#install.packages("LearnBayes")
#install.packages("gmodels")
#install.packages("expm")
#install.packages('sf')
#install.packages("pgirmess")
#library(pgirmess)
#kruskalmc(alcohol~cat_quality, data = wine)
