library(parallel)
library(plyr)
library(dplyr)
library(rpart)
library(RODBC)
library(rowr)
library(reshape2)
library(Matrix)
library(xgboost)
library(glmnet)
source('Z:/Share/source/Analytics Library.R')
source('Z:/Share/source/ML.R')

createDataSet<-function(data,template1,template2)
{
  data<-conform(data,template1)
  browser()
  dataNumerics<-Filter(is.numeric,data)
  nm<-Matrix(as.matrix(dataNumerics),sparse=TRUE)
  rm(dataNumerics)
  gc()
  tm<-arules.getTransactions(data,template1,20)
  cols<-intersect(colnames(template2),colnames(tm))
  tm<-tm[,cols]
  tm<-c(template2,tm)[-1]
  m<-nm %>% cbind2(t(tm@data))
  m@Dimnames[[2]]<-c(nm@Dimnames[[2]],tm@itemInfo$labels)
  return(m)
  
}

main<-function()
{
  data<-data.table::fread('train.csv',integer64="numeric",stringsAsFactors=TRUE) %>% as.data.frame
  data[,lapply(data,is.character) %>% unlist]<-lapply(data[,lapply(data,is.character) %>% unlist],as.factor)
  
  test<-data.table::fread('test.csv',integer64="numeric",stringsAsFactors=TRUE) %>% as.data.frame
  test[,lapply(test,is.character) %>% unlist]<-lapply(test[,lapply(test,is.character) %>% unlist],as.factor)
  data<-prepData(data,test)
  test<-conform(test,data)
  dataOutcome<-data$target
  data$target<-NULL
  
  dataNumerics<-Filter(is.numeric,data)
  nm<-Matrix(as.matrix(dataNumerics),sparse=TRUE)
  tm<-arules.getTransactions(data,test,10)
  m<-nm %>% cbind2(t(tm@data))
  m@Dimnames[[2]]<-c(nm@Dimnames[[2]],tm@itemInfo$labels)
  
  param <- list(  objective           = "binary:logistic", 
                  # booster = "gblinear",
                  eta                 = 0.01,
                  max_depth           = 14,  # changed from default of 6
                  subsample           = 0.6,
                  colsample_bytree    = 0.6,
                  eval_metric         = "auc"
                  # alpha = 0.0001, 
                  # lambda = 1
  )
  
  model<-xgboost(data = m[trainIndex,],label = dataOutcome[trainIndex],params=param,nrounds = 1,verbose=2)
  #model<-xgb.train(param, data=m[trainIndex,],nrounds=10,watchlist=list(m[valIndex,],label=dataOutcome[trainIndex,]))
  
  trainIndex<-1:nrow(data)
  valIndex<-getTestIndex(trainIndex,500,500,15000)
  trainIndex<-setdiff(trainIndex,valIndex)

  model2<-xgboost(data = m[trainIndex,],label = dataOutcome[trainIndex],params=param,nrounds = 10,verbose=2)
  
}