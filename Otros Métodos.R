
# http://topepo.github.io/caret/modelList.html<- lista de todos los métodos y sus parametros

setwd("~/Magdalena/ITAM/Tesis/Matlab")

########################################################################################
# Cargar datos
DatosTrain <- read.csv("PuntosTrain_norm", header=F)
DatosTest <- read.csv("PuntosTest_norm", header=F)

library(e1071)
library(caret)
library(kernlab)
library(caTools)

########################################################################################
# MÉTODO 1: KNN:                                  K*=9, Accuracy: 0.7985

grid=expand.grid(k=seq(1,40,1))
knnFit<-train(as.factor(V3)~., data=DatosTrain, method="knn",  
              trControl=trainControl("cv",seeds=set.seed(1234)), tuneGrid=grid)
knnFit
plot(knnFit, main="K-Vecinos más Cercanos",xlab='Número de vecinos', ylab='Precisión')
knnPredict <- predict(knnFit,newdata=DatosTest)
confusionMatrix(knnPredict,DatosTest$V3)

########################################################################################
# MÉTODO 2: Árboles                               CP*=0.002, Accuracy: 0.7812

grid = expand.grid(cp=c(seq(0,0.005,0.0005),seq(0.006,0.02,0.001)))
treeFit<-train(as.factor(V3)~., data=DatosTrain, method="rpart",
               trControl=trainControl("cv",seeds=set.seed(1234)),tuneGrid=grid) #CP
treeFit
plot(treeFit, main="Árbol de Clasificación",xlab='Parámetro de Complejidad', ylab='Precisión')
treePredict <- predict(treeFit,newdata=DatosTest)
confusionMatrix(treePredict,DatosTest$V3)

########################################################################################
# MÉTODO 3: Logit                                 Accuracy: 0.7505

logit <- multinom(V3~., data = DatosTrain)
pred_logit <- predict(logit, newdata = DatosTest, type = 'class')
MatConfPorc_logit <- (table(DatosTest$V3, pred_logit,dnn=c("Real", "Predicho"))/length(pred_logit))
sum(diag(MatConfPorc_logit))

########################################################################################
# MÉTODO 4: Boosting                                  nIter=6, Accuracy: 0.8545

grid=expand.grid(nIter=seq(1,30,1))
logitFit <- train(as.factor(V3)~., data=DatosTrain, method="LogitBoost", tuneGrid = grid,
                  trControl=trainControl("cv",seeds=set.seed(1234)))
logitFit
plot(logitFit,main="Boosting de Regresión Logística",xlab='Número de iteraciones', ylab='Precisión')
logitPredict <- predict(logitFit,newdata=DatosTest)
confusionMatrix(logitPredict,DatosTest$V3)

########################################################################################
# MÉTODO 5: Redes Neuronales                    size*=80, decay*=0.01, Accuracy: 0.8138
                                               #      10         0.01             0.8138    
                                               #      19        0.009             0.8253
                                               #      30       0.0001             0.8157

grid=expand.grid(size=seq(20,150,20), decay=c(0,0.001,0.004,0.001,0.002))
grid=expand.grid(size=c(10,20,50,60,70,80), decay=c(0,0.0001,0.001,0.01,0.1,1,10,100))
grid=expand.grid(size=c(5,10,15,19,25), decay=c(0.0001,0.001,0.009,0.01))
grid=expand.grid(size=c(27,28,29,30,31,32,33), decay=c(0.001,0.002,0.003,0.004))

grid=expand.grid(size=c(5,10,20,30,40,50,60,70,80,90), decay=c(0,0.0001,0.001,0.01))

set.seed(1234)
nnetFit<-train(as.factor(V3)~., data=DatosTrain, method="nnet", tuneGrid = grid,
               trControl=trainControl("cv",seeds=set.seed(1234))) #sigma decay
nnetFit
plot(nnetFit,main="Red Neuronal",xlab='Número de nodos', ylab='Precisión')
nnetPredict <- predict(nnetFit,newdata=DatosTest)
confusionMatrix(nnetPredict,DatosTest$V3)

########################################################################################
# MÉTODO 6: SVM

# Lineal:                                     C*=16, Accuracy: 0.7505 59 veradero *
grid = expand.grid(C=seq(1,110,2))
grid = expand.grid(C=c(1,10,16,20,30,40,50,60,70,80,90,100,110,120))
grid = expand.grid(C=16)
set.seed(1234)
svmFit<-train(as.factor(V3)~., data=DatosTrain, method="svmLinear", tuneGrid=grid,
              trControl=trainControl("cv",seeds=set.seed(1234)))
svmFit
plot(svmFit,main="Kernel Lineal",xlab='Costo', ylab='Precisión')
svmPredict <- predict(svmFit,newdata=DatosTest)
confusionMatrix(svmPredict,DatosTest$V3)

# Polinomial:                             C*=2000, d*=3, Accuracy: 0.9049
                                        # C*=3000  d*=4, Accuracy: 0.8042
                                        # C*=1400, d*=2, Accuracy: 0.7985
grid=expand.grid(degree=c(2,3,4,5),scale=c(1),C=c(1,1000,1400,seq(2000,4000,1000),4500))

#grid=expand.grid(degree=c(2),scale=c(1),C=c(1300,1350,1400,1450,1500))

svmFit<-train(as.factor(V3)~., data=DatosTrain, method="svmPoly", tuneGrid=grid,
              trControl=trainControl("cv",seeds=set.seed(1234)))
svmFit
plot(svmFit,main="Kernel Polinomial",xlab='Costo', ylab='Precisión')
svmPredict <- predict(svmFit,newdata=DatosTest)
confusionMatrix(svmPredict,DatosTest$V3)

# Radial:                               sigma*=5.34589, C*=500, Accuracy: 0.89913
                                       #sigma*=3.9781,  C*=27,  Accuracy: 0.8119
grid <- expand.grid(sigma=c(5.34589), C=c(1,10,50,100,seq(300,1500,100)))

grid <- expand.grid(sigma=c(3.9781), C=c(seq(1,90,2),seq(100,150,10)))
svmFit<-train(as.factor(V3)~., data=DatosTrain, method="svmRadial", tuneGrid=grid,
              trControl=trainControl("cv",seeds=set.seed(1234)))
svmFit
plot(svmFit,main='Kernel Radial',xlab='Costo',ylab='Precisión')
svmPredict <- predict(svmFit,newdata=DatosTest)
confusionMatrix(svmPredict,DatosTest$V3)



