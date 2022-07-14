
install.packages("readxl")
install.packages("tidyverse")
install.packages("GGally")
install.packages("corrplot")
install.packages("ggfortify")
install.packages("rsample")
install.packages("mvnormtest")
install.packages("klaR")
install.packages("mlr")
install.packages("dendextend")
install.packages("pracma")
install.packages("cluster")
install.packages("scatterplot3d")
install.packages("corpcor")
install.packages("Hotelling")
#install.packages("car")
install.packages("vegan")
install.packages("npmv")
#install.packages("devtools")
#library(devtools)
#install_github("fawda123/ggord")
#library(ggord)
library(npmv)
library(vegan)
library(corpcor)
library(Hotelling)
library(scatterplot3d)
library(cluster)
library(pracma)
library(dendextend)
library(mlr)
library(klaR) 
library(biotools)#sobreescribe Select de Tidyverse, cargar antes de Tidyverse para evitar problemas
library(mvnormtest)
library(rsample)
library(ggfortify)
library(corrplot)
library(readxl)
library(GGally)
library(tidyverse)

wine_data <- read_excel("DatosTP1.xlsx") %>% 
              mutate_at('variedad', as.factor)

set.seed(311)

#Esto va a ser útil para trabajar con los datos que no están en wine: 
#https://dplyr.tidyverse.org/reference/filter-joins.html
wine <- wine_data %>%
        group_by(variedad) %>%
        slice_sample(n = 1000)

wine_numeric <- wine %>% 
                ungroup() %>% 
                select(where(is.numeric))
variety <- wine$variedad

# gpairs_lower <- function(g){
#   g$plots <- g$plots[-(1:g$nrow)]
#   g$yAxisLabels <- g$yAxisLabels[-1]
#   g$nrow <- g$nrow -1
#   
#   g$plots <- g$plots[-(seq(g$ncol, length(g$plots), by = g$ncol))]
#   g$xAxisLabels <- g$xAxisLabels[-g$ncol]
#   g$ncol <- g$ncol - 1
#   
#   g
# }
# 
# 
# g <- ggpairs(wine_numeric, 
#         aes(color = wine$variedad, alpha = 0.5),
#         lower = list(continuous = "points", combo = "dot"), 
#         upper  = list(continuous = "blank"), legend = 1)+ theme(legend.position = "bottom")
# 
# gpairs_lower(g)

#corregir
# ggpairs(wine_numeric, 
#         aes(color = wine$variedad, alpha = 0.1),
#         lower = list(continuous = "points", combo = "dot"), 
#         upper  = list(continuous = "blank"), legend = 1)+ theme(legend.position = "bottom") +
#         scale_color_manual(name="variedad",values=c("antiquewhite","deeppink4"),labels=c("Blanco",'Tinto'))

ggpairs(wine %>% ungroup(), columns = colnames(wine_numeric), labeller = label_wrap_gen(10), legend = 1,
        aes(color = variedad, fill = variedad, alpha = 0.1),
        lower = list(continuous = "points", combo = "dot"), 
        upper  = list(continuous = "blank"))+ theme(legend.position = "right") +
  scale_color_manual(name="variedad",values=c("antiquewhite","deeppink4"),labels=c("Blanco",'Tinto')) +
  scale_fill_manual(name="variedad",values=c("antiquewhite","deeppink4"),labels=c("Blanco",'Tinto'))

# wine %>%
#   pivot_longer(!variedad,names_to = 'variable',values_to = 'valor') %>%
#   ggplot(aes(sample = valor)) +
#   geom_qq() + 
#   geom_qq_line(color="red") + 
#   xlab("Theoretical") +
#   ylab("Sample") +
#   facet_grid(variable ~ variedad)

wine %>%
  pivot_longer(!variedad,names_to = 'variable',values_to = 'valor') %>%
  ggplot(aes(sample = valor, color = variedad)) +
  geom_qq() + 
  geom_qq_line(color = 'black') + 
  facet_wrap(~variable + variedad, ncol = 6, nrow = 4, scales = 'free') + 
  xlab("") +
  ylab("") +
  scale_color_manual(name="variedad",values=c("antiquewhite","deeppink4"),labels=c("Blanco",'Tinto'))

wine %>%
  pivot_longer(!variedad,names_to = 'variable',values_to = 'valor') %>%
  ggplot(aes(y = valor, x = variedad, fill = variedad, alpha = 0.1)) +
  geom_boxplot() + 
  facet_wrap(~variable, ncol = 4, nrow = 3, scales = 'free') + 
  xlab("") +
  ylab("") +
  scale_fill_manual(name="variedad",values=c("antiquewhite","deeppink4"),labels=c("Blanco",'Tinto')) 

as.data.frame(wine %>%
  pivot_longer(!variedad,names_to = 'variable',values_to = 'valor') %>%
  group_by(variedad, variable) %>% 
  summarise(pvalor = shapiro.test(valor)$p.value))

print(hotelling.test(.~ variedad, data = wine_data)) 

#leveneTest(~variedad,data = wine)

#Parte 1: PCA
pca <- prcomp(wine_numeric,scale = TRUE)
round(pca$rotation,2)

prop_variance <- pca$sdev^2 / sum(pca$sdev^2)
prop_variance_cum <- cumsum(prop_variance)

ggplot(data = data.frame(prop_variance_cum, pc = 1:12),
       aes(x = pc, y = prop_variance_cum, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")

screeplot(pca, type = "l", npcs = 12)
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

contrib <- as.matrix(round(pca$rotation,2))
corrplot(contrib,is.corr=FALSE)

# install.packages("devtools")
# library(devtools)
# install_github("vqv/ggbiplot")
# library(ggbiplot)
# require(ggbiplot)
# 
# ggbiplot(pca,obs.scale=0.1 ,var.scale=1,alpha=0.2,groups=factor(wine$variedad)) +
# scale_color_manual(name="variedad",values=c("antiquewhite","deeppink4"),labels=c("Blanco",'Tinto')) + 
# labs(title='Análisis de componentes principales') + 
# theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
# xlim(c(-4,4.2))

autoplot(pca, data = wine, colour = 'variedad', alpha = 0.5, loadings = TRUE, 
         loadings.colour = 'black', loadings.label = TRUE, loadings.label.size = 3) +
scale_color_manual(name="variedad",values=c("antiquewhite","deeppink4"),labels=c("Blanco",'Tinto')) + 
labs(title='Análisis de componentes principales') + xlim(c(-0.08,0.08))

#Parte 2: análisis discriminante

wine_split <- wine %>% 
                initial_split(prop = 0.7, strata = variedad)
wine_train <- wine_split %>% training()
wine_test  <- wine_split %>% testing()

white_wine <- wine_train %>% 
                filter(variedad == 1) %>% 
                ungroup() %>% 
                select(where(is.numeric))
red_wine   <- wine_train %>% 
                filter(variedad == 2) %>% 
                ungroup() %>% 
                select(where(is.numeric))
mshapiro.test(t(white_wine))

mshapiro.test(t(red_wine))

wine_train_numeric <- wine_train%>% ungroup() %>% select(where(is.numeric))
variety_train <- wine_train$variedad
wine_test_numeric <- wine_test%>% ungroup() %>% select(where(is.numeric))
variety_test <- wine_test$variedad

#fijarse Levene y otros métodos
boxM(data = wine_train%>% ungroup() %>% select(where(is.numeric)), grouping = wine_train$variedad)

test_levene <- wine_train_numeric %>% 
                dist(method = 'euclidean') %>%
                vegan::betadisper(variety_train, type = c("median","centroid"), bias.adjust = T,sqrt.dist = FALSE, add = FALSE) %>%
                anova()
test_levene$`Pr(>F)`[1]

#lda requiere MASS que está dentro de biotools
model_lda <- lda(variedad~., data = wine_train)
model_lda$scaling

##recomendado: gráfico unidimensional

table(predict(model_lda,type="class")$class,variety_train)

#por si realmente uno quisiera hacerlo
#X11(width=15, height=15)
#partimat(wine_train_numeric,wine_train$variedad, method="lda", mar=c(1,4,1,2))

table(predict(model_lda,wine_test)$class,variety_test)

tibble(LD1 = predict(model_lda)$x, variedad = variety_train) %>%
  ggplot() +
  geom_density(aes(LD1, fill = variedad), alpha = 0.3) +
  scale_fill_manual(name="variedad",values=c("antiquewhite","deeppink4"),labels=c("Blanco",'Tinto')) + 
  labs(title='Distribución de los resultados de la función discriminante') + ylab('Densidad')

acclda_ <- round(measureACC(variety_train, predict(model_lda,type="class")$class),3)
acclda <- round(measureACC(variety_test, predict(model_lda,wine_test)$class),3)

Metric <- c('valor','datos')
Accuracy <- c(acclda,'prueba')
Accuracy. <- c(acclda_,'entrenamiento')

rbind(Metric, Accuracy, Accuracy.)

#ggord(model_lda, variety_train)

model_qda <- qda(variedad~., data = wine_train)

table(predict(model_qda,type="class")$class,variety_train)

table(predict(model_qda,wine_test)$class,variety_test)

accqda_ <- round(measureACC(variety_train, predict(model_qda,type="class")$class),3)
accqda <- round(measureACC(variety_test, predict(model_qda,wine_test)$class),3)

Metric <- c('valor','datos')
Accuracy <- c(acclda,'prueba')
Accuracy. <- c(acclda_,'entrenamiento')

rbind(Metric, Accuracy, Accuracy.)

#Parte 3: SVM

#escalo los datos y renombro las columnas para que sean compatibles con mlr:
wine_scaled <- wine_numeric %>% 
                mutate_all(~(scale(.) %>% as.vector)) %>% 
                rename_with(~ (iconv(make.names(.),to="ASCII//TRANSLIT"))) %>% 
                add_column(variedad = variety)
        

wine_train_scaled <- wine_train_numeric %>%
                      mutate_all( ~(scale(.) %>% as.vector)) %>% 
                      rename_with(~ (iconv(make.names(.),to="ASCII//TRANSLIT"))) %>% 
                      add_column(variedad = variety_train)

wine_test_scaled <- wine_test_numeric %>% 
                      mutate(across(everything(), 
                                    ~(scale(.,
                                            center = mean(wine_train_numeric[[cur_column()]]),
                                            scale = sd(wine_train_numeric[[cur_column()]])
                                            ) %>% as.vector))) %>% 
                      rename_with(~ (iconv(make.names(.),to="ASCII//TRANSLIT"))) %>% 
                      add_column(variedad = variety_test)

#Kernel lineal:
task = makeClassifTask(data = wine_train_scaled, target = "variedad") 
lrn_svm1 = makeLearner("classif.svm", predict.type = "prob", par.vals = list( kernel = "linear", cost = 2)) 
mod_svm1 = mlr::train(lrn_svm1, task)

pred_svm1 = predict(mod_svm1, newdata = wine_test_scaled) 
acc_svm1 <- round(measureACC(as.data.frame(pred_svm1)$truth, as.data.frame(pred_svm1)$response),3)
AUC_svm1_test <- round(measureAUC(as.data.frame(pred_svm1)$prob.1,as.data.frame(pred_svm1)$truth, 2,1),3)

pred_svm_1 = predict(mod_svm1, newdata = wine_train_scaled) # por si quiero ver naive sobre training
acc_svm_1 <- round(measureACC(as.data.frame(pred_svm_1)$truth, as.data.frame(pred_svm_1)$response),3)
AUC_svm1_train <- round(measureAUC(as.data.frame(pred_svm_1)$prob.1,as.data.frame(pred_svm_1)$truth, 2,1),3)

acc=NULL
acc2=NULL
threshold = seq(0.1,0.95,0.01)
for (i in 1:length(threshold)) {
  pred = setThreshold(pred_svm1, threshold = threshold[i])
  acc[i] = measureACC(as.data.frame(pred)$truth, as.data.frame(pred)$response)}
for (i in 1:length(threshold)) {
  pred2 = setThreshold(pred_svm_1, threshold = threshold[i])
  acc2[i] = measureACC(as.data.frame(pred2)$truth, as.data.frame(pred2)$response)}
par(mfcol = c(1,2))

new_df1 <- as.data.frame(cbind(threshold,acc))
new_df1 <- new_df1%>%mutate(sub_data='test')
new_df2 <- as.data.frame(cbind(threshold,acc2))
colnames(new_df2) <- c('threshold','acc')
new_df2 <- new_df2%>%mutate(sub_data='train')

new_df <- as.data.frame(rbind(new_df1,new_df2))

ggplot(new_df, aes(x=threshold, y=acc)) + geom_line(aes(color = sub_data,linetype=sub_data)) +
  labs(x='Umbral', y='Métrica de performance (accuracy)', 
              title= 'Evaluación del modelo de Máquinas de soporte vectorial SVM') +
  scale_color_manual(values = c("red", "darkred"),labels=c('prueba','entrenamiento')) +
  scale_linetype_manual(values=c(1,2), labels=c('prueba','entrenamiento')) + 
  labs(color='Conjunto de\n evaluación',linetype='Conjunto de\n evaluación')

df_svm = generateThreshVsPerfData(list(svm_te = pred_svm1, svm_tr = pred_svm_1), 
                                  measures = list(fpr, tpr, mmce))

plotROCCurves(df_svm) +
  labs(title='Curva ROC del modelo de Máquinas de soporte vectorial SVM kernel lineal', 
       x='Tasa de falsos positivos (FPR)', y='Tasa de positivos verdaderos (TPR)',
       color='Conjunto de\n evaluación') +
  scale_color_manual(values = c("red", "darkred"), labels=c('prueba','entrenamiento'))

table(as.data.frame(pred_svm_1)$response,as.data.frame(pred_svm_1)$truth)

table(as.data.frame(pred_svm1)$response,as.data.frame(pred_svm1)$truth)#test

Metric <- c('valor','datos')
Accuracy <- c(acc_svm1,'prueba')
Accuracy. <- c(acc_svm_1,'entrenamiento')
AUC_ROC <- c(AUC_svm1_test,'prueba')
AUC_ROC. <- c(AUC_svm1_train,'entrenamiento')

rbind(Metric, Accuracy, Accuracy., AUC_ROC, AUC_ROC.)

#Kernel sigmoide:
task = makeClassifTask(data = wine_train_scaled, target = "variedad") 
lrn_svm2 = makeLearner("classif.svm", predict.type = "prob", par.vals = list( kernel = "sigmoid", cost = 2)) 
mod_svm2 = mlr::train(lrn_svm2, task)

pred_svm2 = predict(mod_svm2, newdata = wine_test_scaled) 
acc_svm2 <- round(measureACC(as.data.frame(pred_svm2)$truth, as.data.frame(pred_svm2)$response),3)
AUC_svm2_test <- round(measureAUC(as.data.frame(pred_svm2)$prob.1,as.data.frame(pred_svm2)$truth, 2,1),3)

pred_svm_2 = predict(mod_svm2, newdata = wine_train_scaled) # por si quiero ver naive sobre training
acc_svm_2 <- round(measureACC(as.data.frame(pred_svm_2)$truth, as.data.frame(pred_svm_2)$response),3)
AUC_svm2_train <- round(measureAUC(as.data.frame(pred_svm_2)$prob.1,as.data.frame(pred_svm_2)$truth, 2,1),3)

acc=NULL
acc2=NULL
threshold = seq(0.1,0.95,0.01)
for (i in 1:length(threshold)) {
  pred = setThreshold(pred_svm2, threshold = threshold[i])
  acc[i] = measureACC(as.data.frame(pred)$truth, as.data.frame(pred)$response)}
for (i in 1:length(threshold)) {
  pred2 = setThreshold(pred_svm_2, threshold = threshold[i])
  acc2[i] = measureACC(as.data.frame(pred2)$truth, as.data.frame(pred2)$response)}
par(mfcol = c(1,2))

new_df1 <- as.data.frame(cbind(threshold,acc))
new_df1 <- new_df1%>%mutate(sub_data='test')
new_df2 <- as.data.frame(cbind(threshold,acc2))
colnames(new_df2) <- c('threshold','acc')
new_df2 <- new_df2%>%mutate(sub_data='train')

new_df <- as.data.frame(rbind(new_df1,new_df2))

ggplot(new_df, aes(x=threshold, y=acc)) + geom_line(aes(color = sub_data,linetype=sub_data)) +
  labs(x='Umbral', y='Métrica de performance (accuracy)', 
       title= 'Evaluación del modelo de Máquinas de soporte vectorial SVM') +
  scale_color_manual(values = c("red", "darkred"),labels=c('prueba','entrenamiento')) +
  scale_linetype_manual(values=c(1,2), labels=c('prueba','entrenamiento')) + 
  labs(color='Conjunto de\n evaluación',linetype='Conjunto de\n evaluación')

df_svm = generateThreshVsPerfData(list(svm_te = pred_svm2, svm_tr = pred_svm_2), 
                                  measures = list(fpr, tpr, mmce))

plotROCCurves(df_svm) +
  labs(title='Curva ROC del modelo de Máquinas de soporte vectorial SVM kernel sigmoideo', 
       x='Tasa de falsos positivos (FPR)', y='Tasa de positivos verdaderos (TPR)',
       color='Conjunto de\n evaluación') +
  scale_color_manual(values = c("red", "darkred"), labels=c('prueba','entrenamiento'))

Metric <- c('valor','datos')
Accuracy <- c(acc_svm2,'prueba')
Accuracy. <- c(acc_svm_2,'entrenamiento')
AUC_ROC <- c(AUC_svm2_test,'prueba')
AUC_ROC. <- c(AUC_svm2_train,'entrenamiento')

rbind(Metric, Accuracy, Accuracy., AUC_ROC, AUC_ROC.)

table(as.data.frame(pred_svm_2)$response,as.data.frame(pred_svm_2)$truth)

table(as.data.frame(pred_svm2)$response,as.data.frame(pred_svm2)$truth)#test

#Kernel radial:
task = makeClassifTask(data = wine_train_scaled, target = "variedad") 
lrn_svm3 = makeLearner("classif.svm", predict.type = "prob", par.vals = list( kernel = "radial", cost = 2)) 
mod_svm3 = mlr::train(lrn_svm3, task)

pred_svm3 = predict(mod_svm3, newdata = wine_test_scaled) 
acc_svm3 <- round(measureACC(as.data.frame(pred_svm3)$truth, as.data.frame(pred_svm3)$response),3)
AUC_svm3_test <- round(measureAUC(as.data.frame(pred_svm3)$prob.1,as.data.frame(pred_svm3)$truth, 2,1),3)

pred_svm_3 = predict(mod_svm3, newdata = wine_train_scaled) # por si quiero ver naive sobre training
acc_svm_3 <- round(measureACC(as.data.frame(pred_svm_3)$truth, as.data.frame(pred_svm_3)$response),3)
AUC_svm3_train <- round(measureAUC(as.data.frame(pred_svm_3)$prob.1,as.data.frame(pred_svm_3)$truth, 2,1),3)

acc=NULL
acc2=NULL
threshold = seq(0.1,0.95,0.01)
for (i in 1:length(threshold)) {
  pred = setThreshold(pred_svm3, threshold = threshold[i])
  acc[i] = measureACC(as.data.frame(pred)$truth, as.data.frame(pred)$response)}
for (i in 1:length(threshold)) {
  pred2 = setThreshold(pred_svm_3, threshold = threshold[i])
  acc2[i] = measureACC(as.data.frame(pred2)$truth, as.data.frame(pred2)$response)}
par(mfcol = c(1,2))

new_df1 <- as.data.frame(cbind(threshold,acc))
new_df1 <- new_df1%>%mutate(sub_data='test')
new_df2 <- as.data.frame(cbind(threshold,acc2))
colnames(new_df2) <- c('threshold','acc')
new_df2 <- new_df2%>%mutate(sub_data='train')

new_df <- as.data.frame(rbind(new_df1,new_df2))

ggplot(new_df, aes(x=threshold, y=acc)) + geom_line(aes(color = sub_data,linetype=sub_data)) +
  labs(x='Umbral', y='Métrica de performance (accuracy)', 
       title= 'Evaluación del modelo de Máquinas de soporte vectorial SVM') +
  scale_color_manual(values = c("red", "darkred"),labels=c('prueba','entrenamiento')) +
  scale_linetype_manual(values=c(1,2), labels=c('prueba','entrenamiento')) + 
  labs(color='Conjunto de\n evaluación',linetype='Conjunto de\n evaluación')

df_svm = generateThreshVsPerfData(list(svm_te = pred_svm3, svm_tr = pred_svm_3), 
                                  measures = list(fpr, tpr, mmce))

plotROCCurves(df_svm) +
  labs(title='Curva ROC del modelo de Máquinas de soporte vectorial SVM kernel radial', 
       x='Tasa de falsos positivos (FPR)', y='Tasa de positivos verdaderos (TPR)',
       color='Conjunto de\n evaluación') +
  scale_color_manual(values = c("red", "darkred"), labels=c('prueba','entrenamiento'))

Metric <- c('valor','datos')
Accuracy <- c(acc_svm3,'prueba')
Accuracy. <- c(acc_svm_3,'entrenamiento')
AUC_ROC <- c(AUC_svm3_test,'prueba')
AUC_ROC. <- c(AUC_svm3_train,'entrenamiento')

rbind(Metric, Accuracy, Accuracy., AUC_ROC, AUC_ROC.)

table(as.data.frame(pred_svm_3)$response,as.data.frame(pred_svm_3)$truth)

table(as.data.frame(pred_svm3)$response,as.data.frame(pred_svm3)$truth)#test

#Comparaciones
SVM_metrics1 <- calculateROCMeasures(pred_svm1)
SVM_metrics2 <- calculateROCMeasures(pred_svm2)
SVM_metrics3 <- calculateROCMeasures(pred_svm3)

pred_todos=NULL
pred_all_svm1 <- as.data.frame(predict(mod_svm1, newdata = wine_scaled))
pred_all_svm2 <- as.data.frame(predict(mod_svm2, newdata = wine_scaled))
pred_all_svm3 <- as.data.frame(predict(mod_svm3, newdata = wine_scaled))

autoplot(pca, data = wine %>% add_column(svm3 = factor(pred_all_svm3$response)), colour = 'svm3' , loadings = TRUE, 
         loadings.colour = 'black', loadings.label = TRUE, loadings.label.size = 3) +
  scale_color_manual(name="svm3",values=c("antiquewhite","deeppink4"),labels=c("Blanco",'Tinto')) + 
  labs(title='Representación de las predicciones ingenuas del modelo SVM (r)\nen las componentes principales 1 y 2') + xlim(c(-0.08,0.08))

df_all = generateThreshVsPerfData(list(svm1 = pred_svm1,
                                       svm2 = pred_svm2,
                                       svm3 = pred_svm3), measures = list(fpr, tpr, mmce))

plotROCCurves(df_all) +  
  labs(title='Curvas ROC de modelos de clasificación supervisada (datos de prueba)',
       x='Tasa de falsos positivos (FPR)', y='Tasa de positivos verdaderos (TPR)', 
       color=' Modelo en\n evaluación') +
  scale_color_manual(values = c("red", "blue", "darkgreen"),
                     labels=c('SVM (l)','SVM (s)','SVM (r)'))+
  theme(legend.position=c(0.915,0.25))

AUC_values <- rbind(AUC_svm1_test,AUC_svm2_test,AUC_svm3_test)
AUC_values <- as.data.frame(AUC_values)
AUC_values$Modelo <- c('SVM (l)','SVM (s)','SVM (r)')
colnames(AUC_values) <- c('Area debajo de la curva (AUC)','Modelo')
row.names(AUC_values) <- NULL
AUC_values <- AUC_values %>% select(2,1)
AUC_values

#Comparación entre métodos supervisados

accuracy_table <- tribble(
                          ~Clasificador,~Valor,~Datos,
                          "LDA"          , acclda_  , 'entrenamiento',
                          "LDA"          , acclda   , 'prueba',
                          "QDA"          , accqda_  , 'entrenamiento',
                          "QDA"          , accqda   , 'prueba',
                          "SVM lineal"   , acc_svm_1, 'entrenamiento',
                          "SVM lineal"   , acc_svm1 , 'prueba',
                          "SVM sigmoide" , acc_svm_2, 'entrenamiento',
                          "SVM sigmoide" , acc_svm2 , 'prueba',
                          "SVM radial"   , acc_svm_3, 'entrenamiento',
                          "SVM radial"   , acc_svm3 , 'prueba'
                        )

accuracy_table

#Parte 4: Clasificación jerárquica
n_clusters = 2
mat_dist <- dist(x = wine_scaled %>% select(-variedad), method = "euclidean") 
hc_ward <- hclust(d = mat_dist, method = "ward.D2")
round(cor(x = mat_dist, cophenetic(hc_ward)),3)
hier_ward<-cutree(hc_ward,k=n_clusters)
#wine_scaled$hier_ward=hier_ward
mar = c(5.1, 4.1, 4.1, 2.1) 
pch=c("antiquewhite","deeppink4") 
cols=alpha(pch[wine_scaled$variedad[order.dendrogram(as.dendrogram(hc_ward))]],0.7)
dend_ward <- color_branches(as.dendrogram(hc_ward), k = 2)
dend_ward <- set(dend_ward, "labels_cex", 0.1)
grafico1 <- dend_ward %>%  set("leaves_pch",19)%>%
  set("leaves_cex", .9) %>% set("leaves_col", cols) %>% 
  plot(main = "Dendrograma jerárquico", ylab='Distancia',cex.lab=1, cex.axis=.6)+
  mtext(side = 3, line = 0.5, at = 1, adj = -1.8, 'Distancia Ward')+
  mtext(side = 1, line = 0.5, at = 1, adj = -4.1, 'Vino')
legend(5,75, title='Diagnóstico', 
       legend = c("Blanco" , "Tinto"), 
       col = pch , 
       pch = c(19,19), bty = "n",  pt.cex = 1.5, cex = 0.8 , 
       text.col = "black", horiz = FALSE, inset = c(0, 0.1))

cluster_table <- table(hier_ward,variety, dnn = NULL)
colnames(cluster_table) <- c('Blanco','Tinto')
rownames(cluster_table) <- c('Clúster 1','Clúster 2')
cluster_table

#Parte 5: K-medias
sil = array()
sse = array()
kmax = 10
for ( i in  2:kmax) { 
  CL  = kmeans(wine_scaled %>% select(-variedad),centers=i,nstart=50,iter.max = kmax)
  sse[i]  = CL$tot.withinss 
  CL_sil = silhouette(CL$cluster, mat_dist)
  sil[i]  = summary(CL_sil)$avg.width
}

m1 = tibble(kcluster = seq(1,kmax), sse, sil)
m1 <- m1 %>% 
  drop_na() %>%
  pivot_longer(cols = c('sse','sil'), names_to = 'metric', values_to = 'value')

ggplot(m1, aes(kcluster, value, linetype=metric)) + geom_line(col='red') + 
  facet_wrap(~metric, ncol=1, scales='free') + geom_point(col='red', size=2, fill='pink', shape=21)+
  labs(title='Determinación de número de clusters', 
       x='k Número de clusters', y='Valor', linetype='Métrica')+
  scale_x_continuous(breaks = seq(1, kmax, by = 1))+
  scale_linetype_manual(values=c(1,2))

#dos clústeres
n_clusters = 2
CL  = kmeans(wine_scaled %>% select(-variedad),n_clusters,nstart=50,iter.max = kmax)

par(mfrow=c(1,2))

col1 <- c("antiquewhite","deeppink4")
col1 <- col1[as.numeric(variety)]

scatterplot3d(wine_scaled$pH,wine_scaled$azucar.residual,wine_scaled$anhidrido.sulfuroso.total, color = alpha(col1,0.1), box=F,angle=45, pch = 19, grid = TRUE, tick.marks = FALSE, xlab = "pH", ylab = "Azúcar Residual", zlab = "Anhídrido sulfuroso total", main='Realidad')
legend("topright", bty = "n", cex = .9, title = "Diagnóstico", c("Blanco", "Tinto"), fill = c("antiquewhite","deeppink4"))

colors <- c('orange','#a25da2a5')
colors <- colors[as.numeric(CL$cluster)]

scatterplot3d(wine_scaled$pH,wine_scaled$azucar.residual,wine_scaled$anhidrido.sulfuroso.total, color = alpha(colors,0.1), box=F,angle=45, pch = 19, grid = TRUE, tick.marks = FALSE, xlab = "pH", ylab = "Azúcar Residual", zlab = "Anhídrido sulfuroso total", main='Clustering')
legend("topright", bty = "n", cex = .9, title = "Grupo k-means", c("1", "2"), fill = c('orange','#a25da2a5'))

autoplot(pca, data = wine %>% add_column(cluster = factor(CL$cluster)), colour = 'cluster' , loadings = TRUE, 
         loadings.colour = 'black', loadings.label = TRUE, loadings.label.size = 3) +
  scale_color_manual(name="cluster",values=c('orange','#a25da2a5'),labels=c("Clúster 1",'Clúster 2')) + 
  labs(title='Representación del clustering usando K-medias') + xlim(c(-0.08,0.08))

cluster_table2 <- table(CL$cluster,variety, dnn = NULL)
colnames(cluster_table2) <- c('Blanco','Tinto')
rownames(cluster_table2) <- c('Clúster 1','Clúster 2')
cluster_table2

#cinco clústeres
n_clusters = 5
CL  = kmeans(wine_scaled %>% select(-variedad),n_clusters,nstart=50,iter.max = kmax)

par(mfrow=c(1,2))

col1 <- c("antiquewhite","deeppink4")
col1 <- col1[as.numeric(variety)]

scatterplot3d(wine_scaled$pH,wine_scaled$azucar.residual,wine_scaled$anhidrido.sulfuroso.total, color = alpha(col1,0.1), box=F,angle=45, pch = 19, grid = TRUE, tick.marks = FALSE, xlab = "pH", ylab = "Azúcar Residual", zlab = "Anhídrido sulfuroso total", main='Realidad')
legend("topright", bty = "n", cex = .9, title = "Diagnóstico", c("Blanco", "Tinto"), fill = c("antiquewhite","deeppink4"))

colors <- c('orange','#a25da2a5', 'blue', 'cyan', 'green')
colors <- colors[as.numeric(CL$cluster)]

scatterplot3d(wine_scaled$pH,wine_scaled$azucar.residual,wine_scaled$anhidrido.sulfuroso.total, color = alpha(colors,0.1), box=F,angle=45, pch = 19, grid = TRUE, tick.marks = FALSE, xlab = "pH", ylab = "Azúcar Residual", zlab = "Anhídrido sulfuroso total", main='Clustering')
legend("topright", bty = "n", cex = .9, title = "Grupo k-means", c("1", "2","3", "4","5"), fill = c('orange','#a25da2a5', 'blue', 'cyan', 'green'))

autoplot(pca, data = wine %>% add_column(cluster = factor(CL$cluster)), colour = 'cluster' , loadings = TRUE, 
         loadings.colour = 'black', loadings.label = TRUE, loadings.label.size = 3) +
  scale_color_manual(name="cluster",values=c('orange','#a25da2a5', 'blue', 'cyan', 'green'),labels=c("Clúster 1",'Clúster 2',"Clúster 3",'Clúster 4',"Clúster 5")) + 
  labs(title='Representación del clustering usando K-medias') + xlim(c(-0.08,0.08))

cluster_table3 <- table(CL$cluster,variety, dnn = NULL)
colnames(cluster_table3) <- c('Blanco','Tinto')
rownames(cluster_table3) <- c("Clúster 1",'Clúster 2',"Clúster 3",'Clúster 4',"Clúster 5")
cluster_table3