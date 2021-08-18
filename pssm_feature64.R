
library(MASS) #library for lda
library(e1071) #svm and naive base 
library(randomForest) #randomForest
library(RSNNS) #mlp and RBF
library(reshape2)
library(ggplot2)
library(parallel)
library(imputeTS)
library(caret) #for cross validation 


#read files 
ppi.posF64 <- read.csv("C:/Users/roozane/Desktop/PSSM/pos_pssm_feature64.csv")
ppi.posF64 <- as.data.frame(ppi.posF64)

ppi.negF64 <- read.csv("C:/Users/roozane/Desktop/PSSM/neg_pssm_feature64.csv")
ppi.negF64 <- as.data.frame(ppi.negF64)


#add column interactions for negative and positive
ppi.posF64<-cbind(ppi.posF64,1)
ppi.negF64<-cbind(ppi.negF64,0)

colnames(ppi.posF64)[130]<-"interact"
colnames(ppi.negF64)[130]<-"interact"

#choose 100 rows for code now
ppi.posF64 <- ppi.posF64[1:100,]
ppi.negF64 <- ppi.negF64[1:100,]


##bind positive and negative
ppi<-rbind(ppi.posF64 ,ppi.negF64)

##shuffel rows
rand<-sample(nrow(ppi))
ppi<-ppi[rand,]

#dont need column 1 wich are protien names
ppi <- ppi[,2:130]

View(ppi)
#write.csv(ppi,"C:/Users/roozane/Desktop/PSSM/ppi.csv")

##binary classification using factor 
ppi$interact = factor(ppi$interact,levels = c(0, 1))



###Split Data and Classificy with CROSS VALIDATIOn and TRAIN function 

##WITH CROSS VALIDATION
# define training control
train_control <- trainControl(method="cv", number=10)

#NB
model.nb <- train(interact~., data=ppi, trControl=train_control, method="nb")
print(model.nb)

#RF
Rmodel.rf <- train(interact~., data=ppi, trControl=train_control, method="rf")
print(Rmodel.rf)

#LDA
Rmodel.lda <- train(interact~., data=ppi, trControl=train_control, method="lda")
print(Rmodel.lda)

#SVM
Rmodel.svmL <- train(interact~., data=ppi, trControl=train_control, method="svmLinear")
print(Rmodel.svmL)

Rmodel.svmP <- train(interact~., data=ppi, trControl=train_control, method="svmPoly")
print(Rmodel.svmP)


###WITH REAPETED CROSS VALIDATION  
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

#NB
Rmodel.nb <- train(interact~., data=ppi, trControl=train_control, method="nb")
print(Rmodel.nb)

#RF
Rmodel.rf <- train(interact~., data=ppi, trControl=train_control, method="rf")
print(Rmodel.rf)

#LDA
Rmodel.lda <- train(interact~., data=ppi, trControl=train_control, method="lda")
print(Rmodel.lda)

#SVM
Rmodel.svmL <- train(interact~., data=ppi, trControl=train_control, method="svmLinear")
print(Rmodel.svmL)
Rmodel.svmP <- train(interact~., data=ppi, trControl=train_control, method="svmPoly")
print(Rmodel.svmP)




###Split Random Trainset and Testset Normaly

N=nrow(ppi)
train<-sample(seq(N),(3/5)*N)
train<-as.numeric((train))

test<-setdiff(seq(N),train)
testset<-ppi[test,]

###CLASSIFICATION NORMALY

#RF
model.randomForest<-randomForest(interact~.,(ppi[train,]))
predict.RF <- predict(model.randomForest,(ppi[test,1:128]))
cat("RF Accurancy is :",sum(predict.RF == ppi[test,"interact"])/length(test))


#SVM
model.SVM<-svm(interact~., ppi[train,],kernel="linear")
predict.SVM<- predict(model.SVM,ppi[test,1:128])
cat("SVM Accurancy SVM is :",sum(predict.SVM == ppi[test,"interact"])/length(test))

#NB
model.NB <- naiveBayes(interact~.,ppi[train,])
predict.NB<- predict(model.NB,ppi[test,1:128])
cat("NB Accurancy is :",sum(predict.NB == ppi[test,"interact"])/length(test))

#LDA
#predict.lad$class ---> is a predict column 
model.LDA<-lda(interact~., ppi[train,])
predict.LDA<- predict(model.LDA, ppi[test,1:128])
cat("LDA Accurancy is :",sum(predict.LDA$class == ppi[test,"interact"])/length(test))


##MLP
ppii<-ppi
ppi.values<- ppii[,1:128]
ppi.targets <- decodeClassLabels(ppii[,129])
#ppi.targets = factor(ppi.targets,levels = c(0, 1))
ppii <- splitForTrainingAndTest(ppi.values, ppi.targets, ratio=0.40)


model.MLP <- mlp(ppii$inputsTrain, ppii$targetsTrain)
predict.MLP<- predict(model.MLP,ppii$inputsTest)



#to make one column of targettest to be like others and comparable 
mlp.targetsTest<-numeric(0)

for(i in 1:nrow(ppii$targetsTest)){
  if(ppii$targetsTest[i,1]==1)
    mlp.targetsTest<-c(mlp.targetsTest,0)
  
  else if(ppii$targetsTest[i,2]==1)
    mlp.targetsTest<-c(mlp.targetsTest,1)
  
}

#to make the predict look like others and be comparable 
predict.MLP<-round(predict.MLP)
mlppredict<-numeric(0)

for(i in 1:nrow(predict.MLP)){
  if(predict.MLP[i,1]==1)
    mlppredict<-c(mlppredict,0)
  
  else if(predict.MLP[i,2]==1)
    mlppredict<-c(mlppredict,1)
  
}

cat("MLP Accurancy is :", sum(mlppredict == mlp.targetsTest)/length(mlp.targetsTest))


