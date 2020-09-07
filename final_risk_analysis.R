library(dplyr)
library(ggplot2)
library(caret)
library(imputeMissings)
library(rpart)
library(ISLR)
library(randomForest)
library(smotefamily)
library(e1071)
library(bnclassify)

#cervical cancer data is found here
#https://www.kaggle.com/loveall/cervical-cancer-risk-classification
#git hub site is: https://github.com/taertman/cervical_cancer.git

workdir<-getwd()
fname<-"/cervdata.zip"
path<-paste(workdir,fname,sep="")
listdir<-"/cervical_cancer-master"
url<-"https://github.com/taertman/cervical_cancer/archive/master.zip"

download.file(url,path)
unzip(path)
csv_path<-paste(workdir,listdir,"/risk.csv",sep="")


#file<-file.choose()
#read in the data assigning NA to features with a ? in the row
dfile<-read.csv(csv_path,header=TRUE,na.strings="?")

#drop dx
dfile<-dfile[,-32]
#drop the two features with substantial NA's
dfile<-dfile[,-28]
dfile<-dfile[,-27]

#now we use the imput function to fill in the rest of the NA values using
#median for numerical data and mean for factors
df2<-imputeMissings::impute(dfile)





#we add a summary factor to our data, a combination of all the diagnositic
#tests so we can caculate an overall risk factor
data_set<-df2%>%mutate(risk_level=Hinselmann+Schiller+Citology+Biopsy)
data_set$risk_level<-factor(data_set$risk_level)



#create the train and test sets
set.seed(1999)
index<-createDataPartition(data_set$risk_level,list=FALSE,p=0.8)
training_set<-data_set[index,]
test_set<-data_set[-index,]


#for random forest we are setting up our model to perform 10 cross-validations
#using a grid search for optimal parameters
contrl=trainControl(method="cv",number=10,search="grid")

#execute the model on our training set
rf_model<-train(risk_level~.,data=training_set,method="rf",trControl=contrl)

#produce the confusion matrix for our results
confusionMatrix(predict(rf_model),training_set$risk_level)

#now make a prediction on our test set



final_rf<-predict(rf_model,newdata=test_set)
confusionMatrix(final_rf,test_set$risk_level)






#for our decision tree model we choose a range of parameters to test for 
#split, complexity parameters and maximum depth

split<-seq(1,20,2)
cp=seq(.001,.02,.002)
mdepth=seq(20,30,5)


parameters=as.matrix(expand.grid(msplit=split,pval=cp,mxdepth=mdepth))


#the funcion returns the Accuracy of our model from a given set of parameters
rpart_test<-function(msplit,p,mxdepth){
  
  contrl=rpart.control(minsplit=msplit,cp=p,maxdepth=mxdepth)
  dtree=rpart(data=training_set,risk_level~.,control=contrl)
  confusionMatrix(predict(dtree,type="class"),training_set$risk_level)$overall["Accuracy"]
  
}


#we cycle through each row of our parmemter grid and record the resulting accuracy
acc<-matrix()
for ( i in seq(1,nrow(parameters))){
  
   acc[i]<-rpart_test(parameters[i,1],parameters[i,2],parameters[i,3])   
  
}


#now we identify which row in our parameter grid gives us the best results
#and assign it to our rpart.control object
first(which(acc==max(acc)))
contrl<-rpart.control(minsplit=1,cp=.003,maxdepth=20)

#test on our train set
dtree<-rpart(data=training_set,risk_level~.,control=contrl)

#results from our training model
confusionMatrix(predict(dtree,type="class"),training_set$risk_level)



#now use these parameters on our test set
confusionMatrix(predict(dtree,newdata = test_set,type="class"),test_set$risk_level)
 




