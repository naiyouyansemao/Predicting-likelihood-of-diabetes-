rm(list = ls())
library(caret)
library(ggplot2)


RNGkind(sample.kind = "Rounding")
Diabetesdata=read.csv(file.choose(),header=T)
attach(Diabetesdata)
names(Diabetesdata)
summary(Diabetesdata)
Diabetesdata$class=as.factor(Diabetesdata$class)
class=as.factor(class)
Gender=as.factor(Gender)
Polyuria=as.factor(Polyuria)
contrasts(class)
contrasts(Gender)
contrasts(Polyuria)

glm.fit.all.data=glm(as.factor(class)~.,family="binomial",data=Diabetesdata)

set.seed(1)
k=5
folds=sample(1:k,nrow(Diabetesdata),replace=TRUE)
accuracy.lr=rep(0,k)
recall.lr=rep(0,k)
precision.lr=rep(0,k)

for(i in 1:k)
{
  glm.fit=glm(as.factor(class)~.,family="binomial",data=Diabetesdata[folds!=i,])
  Diabetesdata.test=Diabetesdata[folds==i, ]
  glm.probs =predict(glm.fit,Diabetesdata.test, type="response")
  glm.pred=rep("Negative",nrow(Diabetesdata[folds==i,]))
  glm.pred[glm.probs>.5]="Positive"
  glm.pred=as.factor(glm.pred)
  
  test.truevalue=as.factor(class)[folds==i]
  table(glm.pred,test.truevalue)
  accuracy.lr[i]=mean(glm.pred==test.truevalue)
  recall.lr[i]=recall(data = glm.pred, reference = test.truevalue, relevant = "Positive")
  precision.lr[i]=precision(data = glm.pred, reference = test.truevalue, relevant = "Positive")
  table <- data.frame(table(glm.pred,test.truevalue))
  plot(ggplot(data = table, mapping = aes(x = test.truevalue, y = glm.pred)) +
         geom_text(aes(label = Freq), vjust = .5, size=10, fontface  = "bold",alpha = 1) +
         xlim(rev(levels(Diabetesdata$class))))
}
mean(accuracy)
mean(recall)
mean(precision)

library(tree)

Diabetesdata$Gender=as.factor(Diabetesdata$Gender)
Diabetesdata$Polyuria=as.factor(Diabetesdata$Polyuria)
Diabetesdata$Polydipsia =as.factor(Diabetesdata$Polydipsia)
Diabetesdata$sudden.weight.loss=as.factor(Diabetesdata$sudden.weight.loss)
Diabetesdata$weakness =as.factor(Diabetesdata$weakness)
Diabetesdata$Polyphagia =as.factor(Diabetesdata$Polyphagia)
Diabetesdata$Genital.thrush=as.factor(Diabetesdata$Genital.thrush)
Diabetesdata$visual.blurring  =as.factor(Diabetesdata$visual.blurring)
Diabetesdata$Itching=as.factor(Diabetesdata$Itching)
Diabetesdata$Irritability=as.factor(Diabetesdata$Irritability)
Diabetesdata$delayed.healing=as.factor(Diabetesdata$delayed.healing)
Diabetesdata$partial.paresis=as.factor(Diabetesdata$partial.paresis)
Diabetesdata$muscle.stiffness=as.factor(Diabetesdata$muscle.stiffness)
Diabetesdata$Alopecia=as.factor(Diabetesdata$Alopecia)
Diabetesdata$Obesity=as.factor(Diabetesdata$Obesity)


set.seed(1)
tree.model.all.data=tree(as.factor(class)~.,data=Diabetesdata)
cv.model=cv.tree(tree.model.all.data,K=10,FUN=prune.misclass)
cv.model
prune.model=prune.tree(tree.model.all.data,best=14)
summary(prune.model)
plot(prune.model)
text(prune.model,pretty=0)



set.seed(1)
accuracy1=rep(0,k)
recall1=rep(0,k)
precision1=rep(0,k)

for(i in 1:k)
{
  tree.model=tree(as.factor(class)~.,data=Diabetesdata[folds!=i,])
  Diabetesdata.test=Diabetesdata[folds==i, ]
  class.test=as.factor(class)[folds==i]

  
  cv.model=cv.tree(tree.model,K=10,FUN=prune.misclass)
  cv.model 
  min.idx=which.min(cv.model$dev)
  bestsize=max(cv.model$size[min.idx])
  prune.model=prune.tree(tree.model,best=bestsize)
  
  #prune.model=prune.tree(tree.model,best=max(cv.model$size[cv.model$dev==min(cv.model$dev)]))
  
  plot(prune.model)
  text(prune.model,pretty=0) 
  
  prunetree.pred=predict(prune.model,Diabetesdata.test,type="class")
  table(prunetree.pred,class.test) 
  
  accuracy1[i]=mean(prunetree.pred==class.test)
  recall1[i]=recall(data = prunetree.pred, reference = class.test, relevant = "Positive")
  precision1[i]=precision(data = prunetree.pred, reference = class.test, relevant = "Positive")
  
  table <- data.frame(table(prunetree.pred,class.test))
  plot(ggplot(data = table, mapping = aes(x = class.test, y = prunetree.pred)) +
    geom_text(aes(label = Freq), vjust = .5, size=10, fontface  = "bold",alpha = 1) +
    xlim(rev(levels(Diabetesdata$class))))
}
mean(accuracy1)
mean(recall1)
mean(precision1)




library(randomForest)

set.seed(1)
rf.Diabetes.all.data=randomForest(as.factor(class)~.,data=Diabetesdata,mtry=4,importance=TRUE)


set.seed(1)
accuracy3=rep(0,k)
recall3=rep(0,k)
precision3=rep(0,k)


for(i in 1:k)
{
  rf.Diabetes=randomForest(as.factor(class)~.,data=Diabetesdata[folds!=i,],mtry=4,importance=TRUE)
  yhat.rf = predict(rf.Diabetes,newdata=Diabetesdata[folds==i,])
  class.test=as.factor(class)[folds==i]
  
  
  table(yhat.rf,class.test) 
  
  accuracy3[i]=mean(yhat.rf==class.test)
  recall3[i]=recall(data = yhat.rf, reference = class.test, relevant = "Positive")
  precision3[i]=precision(data = yhat.rf, reference = class.test, relevant = "Positive")
  
  importance(rf.Diabetes)
  varImpPlot(rf.Diabetes)
  table <- data.frame(table(yhat.rf,class.test))
  plot(ggplot(data = table, mapping = aes(x = class.test, y = yhat.rf)) +
       geom_text(aes(label = Freq), vjust = .5, size=10, fontface  = "bold",alpha = 1) +
       xlim(rev(levels(Diabetesdata$class))))
  
}

mean(accuracy3)
mean(recall3)
mean(precision3)
