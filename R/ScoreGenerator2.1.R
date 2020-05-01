

#preprocess

#preprocessing:
#requirement of data
# finish preselection first 
#1.data type(only numeric or factor),all character will be transformed to factor automatically
#2.point out the outcome(should be factor and binary outcome)
#3.The number of categories for each factor should be less than 9


#Preprocess(data,outcome)
#outcome: character class object wich point to the name of the outcome in the dataset
#data: should be a dataframe

Preprocess<-function (data, outcome) 
  {
    a <- data[, outcome]#update
    data[, outcome] <- NULL#update
    for (i in names(data)) {
      if ((class(data[[i]]) != "factor") && (class(data[[i]]) != 
                                             "numeric")) {
        data[[i]] <- as.factor(data[[i]])
        if (length(levels(data[[i]])) > 10) {
          print(i)
          stop("Error!! The number of categories should be less than 9")
        }
      }
    }
    print("Table of missing value for each observation ")
    table(rowSums(is.na(data)))
    print("Average missing rate for each feature")
    print(colMeans(is.na(data)))
    print("Median imputation processing:")
    library(caret)
    preProcValues <- preProcess(data, method = c("medianImpute"))
    data <- predict(preProcValues, data)
    sum(is.na(data))
    data$label <- a
    data$label <- as.factor(data$label)
    return(data)
  }





#table one
tableone<-function(x){
  library(tableone)
  MD_table<-CreateTableOne(vars = names(x),strata = "label",data=x)
  MD_table_overall<-CreateTableOne(vars = names(x),data=x)
  print(MD_table)
  print(MD_table_overall)}



#Univariable
UniVariable<-function(x){
  b<-data.frame()
  for(i in names(x)[names(x)!="label"]){
    model<-glm(label~.,data=subset(x,select=c("label",i)),family=binomial,na.action= na.omit)
    a<-cbind(exp(cbind(OR = coef(model), confint.default(model))),summary(model)$coef[, "Pr(>|z|)"])
    b<-rbind(b,a)
  }
  b<-b[!grepl("Intercept", row.names(b),ignore.case = T),]
  b<-round(b,digits = 3)
  b$OR<-paste(b$OR,"(",b$`2.5 %`,"-",b$`97.5 %`,")", sep = "")
  b$`2.5 %`<-NULL
  b$`97.5 %`<-NULL
  names(b)[names(b)=="V4"]<-"p value"
  return(b)
}



#full logistic model
MultiVariable<-function(x){
  model<-glm(label~.,data=x,family=binomial,na.action= na.omit)
  b<-cbind(exp(cbind(OR = coef(model), confint.default(model))),summary(model)$coef[, "Pr(>|z|)"])
  b<-b[!grepl("Intercept", row.names(b),ignore.case = T),]
  b<-round(b,digits = 3)
  b<-as.data.frame(b)
  b$OR<-paste(b$OR,"(",b$`2.5 %`,"-",b$`97.5 %`,")", sep = "")
  b$`2.5 %`<-NULL
  b$`97.5 %`<-NULL
  names(b)[names(b)=="V4"]<-"p value"
  return(b) } 


#lasso
lasso<-function(MD3){
  x <- model.matrix(label ~ ., MD3)[, -1]
  y <- MD3$label
  
  library("glmnet")
  # glmnet with alpha=1 means LASSO
  #lasso.mod <- glmnet(x, y, alpha=1,family = "binomial")
  #plot(lasso.mod, xvar="lambda", label=TRUE)
  
  
  # CV for optimal lambda
  set.seed(1)
  lasso.cv <- cv.glmnet(x, y, alpha=1,family = "binomial")
  plot(lasso.cv)
  
  
  #### alternatively, set optimal lambda to lambda.1se for a more parsimonious model ####
  lasso.lam2 <- lasso.cv$lambda.1se
  log(lasso.lam2)
  #min(lasso.cv$cvm) + lasso.cv$cvsd[which.min(lasso.cv$cvm)]
  #points(log(lasso.lam2), min(lasso.cv$cvm) + lasso.cv$cvsd[which.min(lasso.cv$cvm)], cex=3)
  
  # plot optimal lambda
  #plot(lasso.mod, xvar="lambda", label = TRUE)
  #abline(v=log(lasso.lam), lty=2)
  #abline(v=log(lasso.lam2), lty=2)
  
  
  d<-predict(lasso.cv, type="coefficient", s=lasso.lam2)
  print("The selected variables from lasso are shown below:")
  print(d@Dimnames[[1]][d@i])
  coef(lasso.cv)
  #print(lasso.cv)
  #return(lasso.cv)
}


#stepwise
stepwise<-function(MD3){
  names(MD3)<-gsub("\\.","_",names(MD3))
  MD4<-MD3
  for(i in 1:(length(MD4)-1)){
    if(is.factor(MD4[,i]))
      names(MD4)[i]<-paste(names(MD4)[i],".",sep = "")
  }
  
  
  ##delete redundent information 
  UniTbale<-Uni_glmTable(MD4) 
  #print(UniTbale)
  UniTable_sig<-UniTbale[UniTbale$V4<0.1,]
  seq<-gsub("\\..*","",c(rownames(UniTable_sig),"label"))
  seq<-seq[!duplicated(seq)]
  MD4<-MD3[,seq]
  seq<-gsub("\\..*","",names(MD4))
  seq<-seq[!duplicated(seq)]
  MD4<-MD4[,seq]
  
  
  null1 <- glm(label ~1, family = binomial(link="logit"), data = MD4)
  full1 <- glm(label ~., family = binomial(link="logit"), data = MD4)
  ModelStep1<-step(full1, scope = list(lower=null1), data = MD4, direction = "backward", trace = F)
  print(coef(ModelStep1))
  #print(length(coef(ModelStep1)))
  
}














##AutoScore(MD3,n)
##MD: preprocessed data frame
##n: number of variables you would like

AutoScore<-function(MD3,n){
  
  library(pROC)
  library(randomForest)
  
  
  #2.Random Forest Selection
  s<-selectionRF(MD3,n)
  SD<-MD3[,s]
  print("The selected variables for score generation are shown below")
  print(names(SD)[1:n])
  
  # 3. cut numeric and transfer categories
  SD2<-Dftransform_insample(SD)
  
  # 4.  multivariable analysis after
  
  model <- glm(label~., family = binomial(link="logit"), data = SD2)
  
  # 5. Build ScoreAE
  coefVec<-coef(model)
  SD2<-ChangeRef(SD2,coefVec)
  model <- glm(label ~., family = binomial(link="logit"), data = SD2)
  #print(model)
  #summary(model)
  coefVec<-coef(model)
  a<-round(coefVec/min(coefVec[-1]))
  myvec<-AddBaseline(SD2,a)
  print("The generated Scores are shown below:")
  print(as.data.frame(myvec))
  
  
  # 6.Auto test and  performance
  SD3<-AutoTest(SD2,myvec)
  SD3$TotalScore<-rowSums(subset(SD3,select=-label))
  y_test<-SD3$label
  
  Modelroc<-roc(y_test,SD3$TotalScore,auc = T,ci=T)
  print("The best cutoff of using this score：")
  print(coords(Modelroc, "best", ret="threshold"))

}



##AutoScore_imsample(data,n)
##MD: preprocessed data frame
##n: number of variables you would like

AutoScore_insample<-function(MD3,n){
  
  library(pROC)
  library(randomForest)
  
  
  #2.Random Forest Selection
  s<-selectionRF(MD3,n)
  SD<-MD3[,s]
  print("The selected variables for score generation are shown below")
  print(names(SD)[1:n])
  
  # 3. cut numeric and transfer categories
  SD2<-Dftransform_insample(SD)
  
  # 4.  multivariable analysis after
  
  model <- glm(label~., family = binomial(link="logit"), data = SD2)
  
  # 5. Build ScoreAE
  coefVec<-coef(model)
  SD2<-ChangeRef(SD2,coefVec)
  model <- glm(label ~., family = binomial(link="logit"), data = SD2)
  #print(model)
  #summary(model)
  coefVec<-coef(model)
  a<-round(coefVec/min(coefVec[-1]))
  myvec<-AddBaseline(SD2,a)
  print("The generated Scores are shown below")
  print(as.data.frame(myvec))
  
  
  # 6.Auto test and  performance
  SD3<-AutoTest(SD2,myvec)
  SD3$TotalScore<-rowSums(subset(SD3,select=-label))
  y_test<-SD3$label
  PlotROCCurve(SD3$TotalScore,as.numeric(y_test)-1)

  print("Performance using AutoScore (out of sample validation):")
  Modelroc<-roc(y_test,SD3$TotalScore,auc = T,ci=T)
  print("AUC:")
  print(ci(Modelroc))
  print("The best cutoff of using this score：")
  print(coords(Modelroc, "best", ret="threshold"))
  print("Other Performance indicators based on this cutoff: ")
  print(coords(Modelroc, "best", ret=c("specificity", "sensitivity", "accuracy",
                                       "npv", "ppv", "precision")))
  
}



##AutoScore_validation(data,n)
##MD: preprocessed data frame
##n: number of variables you would like


AutoScore_validation<-function(Dataset,n){
  
  library(pROC)
  library(randomForest)
  library(ggplot2)
  
#1.divide data into training(80%) and test(20%) set
  set.seed(4)
  testindex<-sample((1:length(Dataset[,1])),round(length(Dataset[,1])*0.2))
  MD3<-Dataset[-testindex,]
  testSet<-Dataset[testindex,]
  y_test<-testSet$label
  
  
#2.Random Forest Selection
  s<-selectionRF(MD3,n)
  SD<-MD3[,s]
  testSet1<-testSet[,s]
  print("The selected variables for score generation are shown below")
  print(names(SD)[1:n])
  
  
# 3. cut numeric and transfer categories
  SDlist<-Dftransform(SD,testSet1)
  SD2<-SDlist[[1]]
  testSet2<-SDlist[[2]]
  #str(SD2)
  #str(testSet2)
  
  # 4.  multivariable analysis after
  
  model <- glm(label~., family = binomial(link="logit"), data = SD2)
  y_test<-testSet2$label
  
  # 5. Build ScoreAE
  coefVec<-coef(model)
  SD2<-ChangeRef(SD2,coefVec)
  model <- glm(label ~., family = binomial(link="logit"), data = SD2)
  #print(model)
  #summary(model)
  coefVec<-coef(model)
  a<-round(coefVec/min(coefVec[-1]))
  myvec<-AddBaseline(SD2,a)
  print("The generated Scores are shown below")
  print(as.data.frame(myvec))
  
  
  # 6.Auto test and  performance
  testSet3<-AutoTest(testSet2,myvec)
  testSet3$TotalScore<-rowSums(subset(testSet3,select=-label))
  y_test<-testSet3$label
  PlotROCCurve(testSet3$TotalScore,as.numeric(y_test)-1)
  
  print("Performance using AutoScore (out of sample validation):")
  Modelroc<-roc(y_test,testSet3$TotalScore,auc = T,ci=T)
  print("AUC:")
  print(ci(Modelroc))
  print(auc(Modelroc))
  print("The best cutoff of using this score：")
  print(coords(Modelroc, "best", ret="threshold"))
  print("Other Performance indicators based on this cutoff: ")
  print(coords(Modelroc, "best", ret=c("specificity", "sensitivity", "accuracy",
                                  "npv", "ppv", "precision")))
  
}



##AutoScore_imsample(data,n)
##MD: preprocessed data frame
##nmin: minimal number of variables you would like
##nmax: mmaximal number of variables you would like



AutoScore_validation_range<-function(Dataset,nmin=1,nmax=20){
  
  library(pROC)
  library(randomForest)
  library(ggplot2)
  
  #1.divide data into training(70%),validation(10%) and test(20%) set
  set.seed(4)
  testindex<-sample((1:length(Dataset[,1])),round(length(Dataset[,1])*0.2))
  MD3<-Dataset[-testindex,]
  testSet<-Dataset[testindex,]
  y_test<-testSet$label
  
  #validation (10%)
  set.seed(4)
  Validationindex<-sample((1:length(MD3[,1])),round(length(MD3[,1])*0.125))
  ValidationSet<-Dataset[Validationindex,]
  MD3<-MD3[-Validationindex,]
  
  
  #2.Random Forest Selection
  s<-selectionRF(MD3,nmax)
  print("The selected variables for score generation are shown below")
  print(s)
  Ri<-c()
  for(i in nmin:nmax){
    print("Select the number of Variables")
    print(i)
    SD<-MD3[,c(s[1:i],"label")]
    ValidationSet1<-ValidationSet[,c(s[1:i],"label")]
    
    # 3. cut numeric and transfer categories
    SDlist<-Dftransform(SD,ValidationSet1)
    SD2<-SDlist[[1]]
    ValidationSet2<-SDlist[[2]]
    #str(SD2)
    #str(testSet2)
    
    # 4.  multivariable analysis after
    
    model <- glm(label~., family = binomial(link="logit"), data = SD2)
    y_validation<-ValidationSet2$label
    
    # 5. Build ScoreAE
    coefVec<-coef(model)
    SD2<-ChangeRef(SD2,coefVec)
    model <- glm(label ~., family = binomial(link="logit"), data = SD2)
    #print(model)
    #summary(model)
    coefVec<-coef(model)
    a<-round(coefVec/min(coefVec[-1]))
    myvec<-AddBaseline(SD2,a)
    
    
    ValidationSet3<-AutoTest(ValidationSet2,myvec)
    ValidationSet3$TotalScore<-rowSums(subset(ValidationSet3,select=-label))
    y_validation<-ValidationSet3$label
    PlotROCCurve(ValidationSet3$TotalScore,as.numeric(y_validation)-1)
    
    #print("Performance using AutoScore (out of sample validation):")
    Modelroc<-roc(y_validation,ValidationSet3$TotalScore,auc = T,ci=T)
    #print("AUC:")
    #print(ci(Modelroc))
    print(auc(Modelroc))
    Ri<-c(Ri,auc(Modelroc))
    #print("The best cutoff of using this score：")
    #print(coords(Modelroc, "best", ret="threshold"))
    print("Other Performance indicators based on this cutoff: ")
    print(coords(Modelroc, "best", ret=c("specificity", "sensitivity", "accuracy",
                                                                              "npv", "ppv", "precision")))
                                         
  }
  
  imax<-(nmin:nmax)[which(Ri==max(Ri))]
  
  
}












##below are functions



selectionRF<-function(x,num){
  library(caret)
  set.seed(4)
  # prepare training scheme
  #control <- trainControl(method="repeatedcv", number=10, repeats=3)
  x$label<-as.factor(x$label)
  # train the model
  model <- randomForest(label~., data=x
                        , preProcess="scale")
  
  
  # estimate variable importance
  importance <- varImp(model, scale=FALSE)
  # summarize importance
  
  b<-importance$Overall
  names(b)<-rownames(importance)
  b<-sort(b,decreasing = T)
  
  a<-length(b)
  if(a>num) return(c(names(b)[1:num],"label"))
  else return(c(names(b),"label"))
}  

selectionRF_list<-function(x,num){
  library(caret)
  set.seed(4)
  # prepare training scheme
  #control <- trainControl(method="repeatedcv", number=10, repeats=3)
  x$label<-as.factor(x$label)
  # train the model
  model <- randomForest(label~., data=x
                        , preProcess="scale")
  
  
  # estimate variable importance
  importance <- varImp(model, scale=FALSE)
  # summarize importance
  
  b<-importance$Overall
  names(b)<-rownames(importance)
  b<-sort(b,decreasing = T)
  
return(c(names(b)))
}  


Dftransform<-function(x,testSet1){
  for(i in 1:(length(x)-1)){
    if(class(x[,i])=="factor"){
      if(length(levels(x[,i]))<10) next()
      else stop("Error!! The number of categories should be less than 9")
    }
    # options(scipen = 20)
    a<-quantile(x[,i], probs = c(0, 0.05,0.2,0.8,0.95,1))
    a<-CheckVector(a)
    a1<-signif(a,3) # remain 3 digits
   # print(a1) update
    
    if(length(a1)<=2) {x[,i]<-as.factor(x[,i])
    testSet1[,i]<-as.factor(testSet1[,i])}
    
    else {x[,i]<-cut(x[,i],breaks=a1, right = F,include.lowest = T,dig.lab = 3)
    #xmin<-unlist(strsplit(levels(x[,i])[1],","))[1]
    #xmax<-unlist(strsplit(levels(x[,i])[length(levels(x[,i]))],","))[2]
    levels(x[,i])[1]<-gsub(".*,","(,",levels(x[,i])[1])
    levels(x[,i])[length(levels(x[,i]))]<-gsub(",.*",",)",levels(x[,i])[length(levels(x[,i]))])
    
    at<-a1
    at[1]<-min(testSet1[,i])
    at[length(at)]<-max(testSet1[,i])
    at1<-signif(at,3)
    at1<-CheckVector(at1) ###revised update##
    testSet1[,i]<-cut(testSet1[,i],breaks=at1, right = F,include.lowest = T,dig.lab = 3)
    #xmin<-as.character(min(at1))
    #xmax<-as.character(max(at1))
    levels(testSet1[,i])[1]<-gsub(".*,","(,",levels(testSet1[,i])[1])
    levels(testSet1[,i])[length(levels(testSet1[,i]))]<-gsub(",.*",",)",levels(testSet1[,i])[length(levels(testSet1[,i]))])
    
    
    }
    #在此时应该转换，先读取最小和最大值！！！把[最小值统一换成负无穷， ] 只是适用于cross validation的情况
  #  print(summary(x[,i]))update
   # print(summary(testSet1[,i]))update
    
  }
  
  return(list(x,testSet1))
  
}



Dftransform_insample<-function(x){
  for(i in 1:(length(x)-1)){
    a<-quantile(x[,i], probs = c(0, 0.05,0.2,0.8,0.95,1))
    a<-CheckVector(a)
  #  print(a)
    if(length(a)<=2) x[,i]<-as.factor(x[,i])
    else x[,i]<-cut(x[,i],breaks=a, right = F,include.lowest = T,dig.lab = 3)
    
    #在此时应该转换，先读取最小和最大值！！！把[最小值统一换成负无穷， ] 只是适用于cross validation的情况
  #  print(summary(x[,i]))
    
    
  }
  
  return(x)
  
}


CheckVector<-function(x){
  dele<-c()
  for(i in 1:(length(x)-1)){
    if(x[i]==x[i+1]){
      dele<-c(dele,i+1)
    }}
  if(is.null(dele)) return(x)
  else return(x[-dele])
}

PlotROCCurve<-function(prob,labels){
  library(ROCR)
  #prob<-predict(model.glm,newdata=X_test, type = "response")
  pred<-prediction(prob,labels)
  perf<-performance(pred,measure="tpr",x.measure = "fpr")
  auc<-performance(pred,measure = "auc")
  auc<-auc@y.values[[1]]
  
  
  roc.data<-data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
  p<-ggplot(roc.data,aes(x=fpr,ymin=0,ymax=tpr))+
    geom_ribbon(alpha=0.2)+
    geom_line(aes(y=tpr))+
    ggtitle(paste0("ROC Curve /w AUC="),auc)
  print(p)
}


ChangeRef<-function(df1,coefVec){
  
  df<-subset(df1,select=-label)
  for(i in (1:length(df))){
    c1<-paste('^',names(df)[i],sep="")
    a<-coefVec[grepl(c1,names(coefVec))]
    if(min(a)<0){
      ref<-gsub(names(df)[i],"",names(a)[which.min(a)])
      df[,i]<-relevel(df[,i],ref = ref)
    }}
  
  df$label<-df1$label 
  return(df) }



AddBaseline<-function(df,coefVec){
  df<-subset(df,select=-label)
  vec<-c()
  m<-0
  n<-2
  for(i in (1:length(df))){
    vec<-c(vec,0)
    m<-m+1
    names(vec)[m]<-paste(names(df)[i],levels(df[,i])[1],sep="")
    for(j in levels(df[,i])[2:length(levels(df[,i]))]){
      vec<-c(vec,coefVec[n])
      n<-n+1
      m<-m+1
    }}
  return(vec)}


AutoTest<-function(df,myvec){
  for(i in 1:length(names(df))){
    a<-myvec[grepl(names(df)[i],names(myvec))]
    df[,i]<-as.character(df[,i])
    for(j in 1:length(names(a))){
      df[,i][df[,i] %in% gsub(names(df)[i],"",names(a)[j])]<-a[j]
    }
    
    df[,i]<-as.numeric(df[,i])
  }
  
  return(df)}

Uni_glmTable<-function(x){
  b<-data.frame()
  for(i in names(x)[names(x)!="label"]){
    model<-glm(label~.,data=subset(x,select=c("label",i)),family=binomial,na.action= na.omit)
    a<-cbind(exp(cbind(OR = coef(model), confint.default(model))),summary(model)$coef[, "Pr(>|z|)"])
    b<-rbind(b,a)
  }
  b<-b[!grepl("Intercept", row.names(b),ignore.case = T),]
  b<-round(b,digits = 3)
  b$OR<-paste(b$OR,"(",b$`2.5 %`,"-",b$`97.5 %`,")", sep = "")
  b$`2.5 %`<-NULL
  b$`97.5 %`<-NULL
  
  return(b)
}

