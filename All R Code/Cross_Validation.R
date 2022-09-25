################################################################################
## Cross Validation (Parallel)


# Parallel Cross-Validation Example

library(foreach)    
library(caret)      
library(doParallel) 
registerDoParallel(makeCluster(64)) # Use 64 cores for parallel CV


perfom_cv <- function(train, NF=10){
  
  ### No. of rows in train set
  N<-nrow(train)
  
  ## Creating NF Folds
  folds<-split(1:N,cut(1:N, quantile(1:N, probs = seq(0, 1, by =1/NF))))
  
  ## Shuffling
  ridx<-sample(1:nrow(train),nrow(train),replace=FALSE)
  
  data <- train[ridx,]
  
  # 'dopar' here would run this on multiple threads (change to just 'do' for synchronous runs)
  outputs <- foreach(idx = folds, .packages = "e1071", .combine = rbind ) %dopar% {
    
    # Get the fold data where 'idx' is nothing more than a list of indexes for test observations in the data
    data.train <- data[-idx,] # Get the opposite of the test observations to train on
    data.test <- data[idx,]
    
    # Fit the model and make predictions
    m <- naiveBayes(readmitted~., data=data.train )   # Fit the model
    p <- predict(m,data.test[,-c(19)], type='raw')
    pc <- unlist(apply(round(p),1,which.max))-1
    pc <- as.factor(pc)
    pred_cfm<-caret::confusionMatrix(pc, data.test$readmitted)
    
    list(fold=idx,m=m,cfm=pred_cfm) # store the fold, model,cfm
  }

  outputs
}


results  <- perfom_cv(train, 10)


get_Eval_Metrix <- function(results) {
  # Results is a list, so you could process it to extract the accuracies like this:
  
  tstres.perf<-as.data.frame(do.call('rbind',lapply(results[,"cfm"],FUN=function(cfm)c(cfm$overall))))
  tst.overall<-apply(tstres.perf,2,mean)
   
  
  ## Class 0: "<30"
  tstres.perf.1<-as.data.frame(do.call('rbind',lapply(results[,"cfm"],FUN=function(cfm)c(cfm$byClass[1,]))))
  cv.tst.perf.1<-apply(tstres.perf.1,2,mean)
  
  ## Class 1: ">30"
  tstres.perf.2<-as.data.frame(do.call('rbind',lapply(results[,"cfm"],FUN=function(cfm)c(cfm$byClass[2,]))))
  cv.tst.perf.2<-apply(tstres.perf.2,2,mean)
  
  ## Class 2: "NO"
  tstres.perf.3<-as.data.frame(do.call('rbind',lapply(results[,"cfm"],FUN=function(cfm)c(cfm$byClass[3,]))))
  cv.tst.perf.3<-apply(tstres.perf.3,2,mean)
  
  
  df <- as.data.frame(do.call("rbind", list(cv.tst.perf.1, cv.tst.perf.2, cv.tst.perf.3))) %>%
    select(c("Specificity","Precision","Recall","Balanced Accuracy"))
  
  rownames(df) <- c("Class: <30","Class: >30","Class: NO")
  
  list(overall=tst.overall, byClass=df)

}

get_Eval_Metrix(results)
## $overall
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   5.510226e-01   1.249835e-01   5.393832e-01   5.626203e-01   5.391180e-01 
## AccuracyPValue  McnemarPValue 
##   3.340418e-02  3.126661e-294 
## 
## $byClass
##            Specificity Precision    Recall Balanced Accuracy
## Class: <30   0.9341373 0.2264230 0.1534932         0.5438152
## Class: >30   0.9205516 0.5065966 0.1519341         0.5362429
## Class: NO    0.2649705 0.5866739 0.8918740         0.5784223



################################################################################
## Cross Validation (Sequential)


start_tm <- proc.time() 

(N<-nrow(train))
## [1] 71234

NF=10
folds<-split(1:N,cut(1:N, quantile(1:N, probs = seq(0, 1, by =1/NF))))
length(folds)
## [1] 10

lapply(folds,length)
## $`(1,7.12e+03]`
## [1] 7123
## 
## $`(7.12e+03,1.42e+04]`
## [1] 7123
## 
## $`(1.42e+04,2.14e+04]`
## [1] 7123
## 
## $`(2.14e+04,2.85e+04]`
## [1] 7124
## 
## $`(2.85e+04,3.56e+04]`
## [1] 7123
## 
## $`(3.56e+04,4.27e+04]`
## [1] 7123
## 
## $`(4.27e+04,4.99e+04]`
## [1] 7124
## 
## $`(4.99e+04,5.7e+04]`
## [1] 7123
## 
## $`(5.7e+04,6.41e+04]`
## [1] 7123
## 
## $`(6.41e+04,7.12e+04]`
## [1] 7124



train_knn <- train
test_knn <- test
train_knn[sig10.cat.attrs] <- lapply(train_knn[sig10.cat.attrs], as.numeric) 
test_knn[sig10.cat.attrs] <- lapply(test_knn[sig10.cat.attrs], as.numeric) 


## KNN with k=3
knn.model <- knn(train_knn,test_knn,cl = train_knn$readmitted, k = 3)

confusionMatrix(knn.model, test$readmitted)

ridx<-sample(1:nrow(train),nrow(train),replace=FALSE) # randomize the data

train_knn[ridx,]$readmitted

m <- naiveBayes(readmitted~., data=train ) 

cv_df <- do.call('rbind',lapply(folds,FUN=function(idx,data=train[ridx,]) {
  
  m <- naiveBayes(readmitted~., data=data[-idx,] ) # keep one fold for validation
  p <- predict(m,data[idx,-c(19)], type='raw') # predict for that test fold
  pc <- unlist(apply(round(p),1,which.max))-1
  pred_tbl<-table(data[idx,c(19)],pc) #table(actual,predicted)
  pred_cfm<-caret::confusionMatrix(pred_tbl)
  list(fold=idx,m=m,cfm=pred_cfm) # store the fold, model,cfm 
}
)) # lapply repeats over all folds


## model <- naiveBayes(readmitted~., data=train) 
## pred <- predict(model,train[,-c(19)], type='raw') 
## pred_class <- unlist(apply(round(pred),1,which.max)) - 1
## 
## pred_tbl<-table(train[,c(19)],pred_class) #table(actual,predicted)
## pred_tbl
## pred_cfm<-caret::confusionMatrix(pred_tbl)
## pred_cfm
## list(fold=10,m=model,cfm=pred_cfm) # store the fold, model,cfm 

pred_cfm$byClass

pred_cfm$overall
##     Accuracy          Kappa  AccuracyLower  AccuracyUpper 
##    0.5510852      0.1248704      0.5474236      0.5547426 
## AccuracyNull AccuracyPValue  McnemarPValue 
##    0.8200298      1.0000000      0.0000000 


pred_cfm$byClass
##          Sensitivity Specificity Pos Pred Value Neg Pred Value
## Class: 0   0.2246417   0.8976329      0.1518430      0.9341708
## Class: 1   0.5070498   0.6691332      0.1517624      0.9208034
## Class: 2   0.5867258   0.6777691      0.8924331      0.2646665
##          Precision    Recall        F1 Prevalence Detection Rate
## Class: 0 0.1518430 0.2246417 0.1812040 0.07542746     0.01694416
## Class: 1 0.1517624 0.5070498 0.2336055 0.10454277     0.05300839
## Class: 2 0.8924331 0.5867258 0.7079882 0.82002976     0.48113261
##          Detection Prevalence Balanced Accuracy
## Class: 0            0.1115900         0.5611373
## Class: 1            0.3492855         0.5880915
## Class: 2            0.5391246         0.6322474

cv_df<-as.data.frame(cv_df)


cv_df$cfm$`(7.12e+03,1.42e+04]`$byClass[1,]
class(cv_df$cfm$`(7.12e+03,1.42e+04]`$byClass)

tstcv.perf.1<-as.data.frame(do.call('rbind',lapply(cv_df$cfm,FUN=function(cfm)c(cfm$byClass[1,]))))
tstcv.perf.1

tstcv.perf.2<-as.data.frame(do.call('rbind',lapply(cv_df$cfm,FUN=function(cfm)c(cfm$byClass[2,]))))
tstcv.perf.2

tstcv.perf.3<-as.data.frame(do.call('rbind',lapply(cv_df$cfm,FUN=function(cfm)c(cfm$byClass[3,]))))
tstcv.perf.3


tstcv.perf<-as.data.frame(do.call('rbind',lapply(cv_df$cfm,FUN=function(cfm)c(cfm$overall))))
tstcv.perf


(cv.tst.perf<-apply(tstcv.perf,2,mean))
##     Accuracy          Kappa  AccuracyLower  AccuracyUpper 
## 5.506156e-01   1.240421e-01   5.389759e-01   5.622138e-01 
## AccuracyNull AccuracyPValue  McnemarPValue 
## 8.199290e-01   1.000000e+00  6.198934e-303

(cv.tst.perf.var<-apply(tstcv.perf,2,sd))
##     Accuracy          Kappa  AccuracyLower  AccuracyUpper 
##  0.006356700    0.007698140    0.006369304    0.006338889 
## AccuracyNull AccuracyPValue  McnemarPValue 
##  0.003707489    0.000000000    0.000000000 

(cv.tst.perf.1<-apply(tstcv.perf.1,2,mean))
##          Sensitivity          Specificity       Pos Pred Value 
##           0.22125502           0.89741167           0.14993876 
##       Neg Pred Value            Precision               Recall 
##           0.93382549           0.14993876           0.22125502 
##                   F1           Prevalence       Detection Rate 
##           0.17862683           0.07552667           0.01673377 
## Detection Prevalence    Balanced Accuracy 
##           0.11157750           0.55933335 

(cv.tst.perf.2<-apply(tstcv.perf.2,2,mean))
##          Sensitivity          Specificity       Pos Pred Value 
##           0.50641214           0.66905281           0.15159186 
##       Neg Pred Value            Precision               Recall 
##           0.92068902           0.15159186           0.50641214 
##                   F1           Prevalence       Detection Rate 
##           0.23327078           0.10454436           0.05293899 
## Detection Prevalence    Balanced Accuracy 
##           0.34929039           0.58773247 

(cv.tst.perf.3<-apply(tstcv.perf.3,2,mean))
##          Sensitivity          Specificity       Pos Pred Value 
##            0.5865745            0.6769078            0.8920677 
##       Neg Pred Value            Precision               Recall 
##            0.2645072            0.8920677            0.5865745 
##                   F1           Prevalence       Detection Rate 
##            0.7077365            0.8199290            0.4809428 
## Detection Prevalence    Balanced Accuracy 
##            0.5391321            0.6317411 




tstcv_preds<-lapply(cv_df$m,FUN=function(M,D=test[,-c(19)])predict(M,D,type='raw'))

tstcv_cfm <- do.call('rbind',lapply(tstcv_preds,FUN=function(P,A=test[[19]]) {
  pred_class<-unlist(apply(round(P),1,which.max))-1
  pred_tbl<-table(pred_class,A)
  pred_cfm<-caret::confusionMatrix(pred_tbl)
  list(cfm=pred_cfm)
}))



tstcv.perf<-as.data.frame(do.call('rbind',lapply(test_cfm,FUN=function(cfm)c(cfm$overall))))

tstcv.perf

tstcv.perf.1<-as.data.frame(do.call('rbind',lapply(test_cfm,FUN=function(cfm)c(cfm$byClass[1,]))))
tstcv.perf.1


(cv.tst.perf<-apply(tstcv.perf,2,mean))
##     Accuracy          Kappa  AccuracyLower  AccuracyUpper 
## 5.535176e-01   1.319381e-01   5.479202e-01   5.591049e-01 
## AccuracyNull AccuracyPValue  McnemarPValue 
## 5.391065e-01   3.776679e-07   0.000000e+00 


(cv.tst.perf.1<-apply(tstcv.perf.1,2,mean))
(cv.tst.perf.var<-apply(tstcv.perf,2,sd))
##     Accuracy          Kappa  AccuracyLower  AccuracyUpper 
## 5.656070e-04   7.545549e-04   5.662329e-04   5.648734e-04 
## AccuracyNull AccuracyPValue  McnemarPValue 
## 0.000000e+00   4.885795e-07   0.000000e+00 

################################################################################

