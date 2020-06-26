#######################################################
# Source Codes for AutoScore 2.4 (release version AutoScore 0.1)

# preprocess
# preprocessing: requirement of data finish preselection first 1.data type(only numeric or factor),all character will be transformed
# to factor automatically 2.point out the outcome(should be factor and binary outcome) 3.The number of categories for each factor
# should be less than 9
# Preprocess(data,outcome) outcome: character class object wich point to the name of the outcome in the dataset data: should be a
# dataframe

Preprocess <- function(data, outcome) {
  a <- data[, outcome]  #update
  data[, outcome] <- NULL  #update
  for (i in names(data)) {
    if ((class(data[[i]]) != "factor") && (class(data[[i]]) != "numeric")) {
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



# Generate table one based on stratified outcomes
Descriptive <- function(x) {
  library(tableone)
  MD_table <- CreateTableOne(vars = names(x), strata = "label", data = x)
  MD_table_overall <- CreateTableOne(vars = names(x), data = x)
  print(MD_table)
  print(MD_table_overall)
}


# Univariable analysis
UniVariable <- function(x) {
  b <- data.frame()
  for (i in names(x)[names(x) != "label"]) {
    model <- glm(label ~ ., data = subset(x, select = c("label", i)), family = binomial, na.action = na.omit)
    a <- cbind(exp(cbind(OR = coef(model), confint.default(model))), summary(model)$coef[, "Pr(>|z|)"])
    b <- rbind(b, a)
  }
  b <- b[!grepl("Intercept", row.names(b), ignore.case = T), ]
  b <- round(b, digits = 3)
  b$OR <- paste(b$OR, "(", b$`2.5 %`, "-", b$`97.5 %`, ")", sep = "")
  b$`2.5 %` <- NULL
  b$`97.5 %` <- NULL
  names(b)[names(b) == "V4"] <- "p value"
  return(b)
}


# full logistic model-multivariable analysis
MultiVariable <- function(x) {
  model <- glm(label ~ ., data = x, family = binomial, na.action = na.omit)
  b <- cbind(exp(cbind(OR = coef(model), confint.default(model))), summary(model)$coef[, "Pr(>|z|)"])
  b <- b[!grepl("Intercept", row.names(b), ignore.case = T), ]
  b <- round(b, digits = 3)
  b <- as.data.frame(b)
  b$OR <- paste(b$OR, "(", b$`2.5 %`, "-", b$`97.5 %`, ")", sep = "")
  b$`2.5 %` <- NULL
  b$`97.5 %` <- NULL
  names(b)[names(b) == "V4"] <- "p value"
  return(b)
}


######################### AutoScore with in-sample validation
## AutoScore_imsample(data,m) MD: preprocessed data frame m: number of variables you would like

AutoScore_insample <- function(data, m, MaxScore = 100, probs = c(0, 0.05, 0.2, 0.8, 0.95, 1)) {
  library(pROC)
  library(randomForest)

  # 2.Random Forest Selection
  s <- selectionRF(data, m)
  SD <- data[, s]
  print("The selected variables for score generation are shown below")
  print(names(SD)[1:m])

  # 3. cut numeric and transfer categories
  SD2 <- Dftransform_insample(SD,probs=probs)

  # 4.  multivariable analysis after
  model <- glm(label ~ ., family = binomial(link = "logit"), data = SD2)

  # 5. Build Score
  coefVec <- coef(model)
  SD2 <- ChangeRef(SD2, coefVec)
  model <- glm(label ~ ., family = binomial(link = "logit"), data = SD2)
  # print(model) summary(model)
  coefVec <- coef(model)
  a <- round(coefVec/min(coefVec[-1]))
  myvec <- AddBaseline(SD2, a)

  total_max <- MaxScore
  total <- 0
  for (i in 1:m) total <- total + max(myvec[grepl(names(SD)[1:m][i], names(myvec))])
  myvec <- round(myvec/(total/total_max))

  print("The generated Scores are shown below")
  print(as.data.frame(myvec))

  # 6.Auto test and performance
  SD3 <- AutoTest(SD2, myvec)
  SD3$TotalScore <- rowSums(subset(SD3, select = -label))
  y_test <- SD3$label
  PlotROCCurve(SD3$TotalScore, as.numeric(y_test) - 1)

  print("Performance using AutoScore (out of sample validation):")
  Modelroc <- roc(y_test, SD3$TotalScore, quiet = T)
  print("AUC:")
  print(ci(Modelroc))
  print(Modelroc$auc)  ##update
  print("The best cutoff of using this score£º")
  print(coords(Modelroc, "best", ret = "threshold", transpose = TRUE))
  print("Other Performance indicators based on this cutoff: ")
  print(coords(Modelroc, "best", ret = c("specificity", "sensitivity", "accuracy", "npv", "ppv", "precision"), transpose = TRUE))

}


############################################ AutoScore with out-of-sample validation
## AutoScore_outofsample(data,m) MD: preprocessed data frame m: number of variables you would like
AutoScore_outofsample <- function(Dataset, m = 8 , Percentage_test = 0.2, MaxScore = 100, probs = c(0, 0.05, 0.2, 0.8, 0.95, 1)) {
  library(pROC)
  library(randomForest)
  library(ggplot2)

  # 1.divide data into training(80%) and test(20%) set
  set.seed(4)
  testindex <- sample((1:length(Dataset[, 1])), round(length(Dataset[, 1]) * Percentage_test))
  MD3 <- Dataset[-testindex, ]
  testSet <- Dataset[testindex, ]
  y_test <- testSet$label

  # 2.Random Forest Selection
  s <- selectionRF(MD3, m)
  SD <- MD3[, s]
  testSet1 <- testSet[, s]
  print("The selected variables for score generation are shown below")
  print(names(SD)[1:m])

  # 3. cut numeric and transfer categories
  SDlist <- Dftransform(SD, testSet1, probs=probs)
  SD2 <- SDlist[[1]]
  testSet2 <- SDlist[[2]]
  # str(SD2) str(testSet2)

  # 4.  multivariable analysis
  model <- glm(label ~ ., family = binomial(link = "logit"), data = SD2)
  y_test <- testSet2$label

  # 5. Build Score
  coefVec <- coef(model)
  SD2 <- ChangeRef(SD2, coefVec)
  model <- glm(label ~ ., family = binomial(link = "logit"), data = SD2)
  # print(model) summary(model)
  coefVec <- coef(model)
  a <- round(coefVec/min(coefVec[-1]))
  myvec <- AddBaseline(SD2, a)

  total_max <- MaxScore
  total <- 0
  for (i in 1:m) total <- total + max(myvec[grepl(names(SD)[1:m][i], names(myvec))])
  myvec <- round(myvec/(total/total_max))

  print("The generated Scores are shown below")
  print(as.data.frame(myvec))

  # 6.Auto test and performance
  testSet3 <- AutoTest(testSet2, myvec)
  testSet3$TotalScore <- rowSums(subset(testSet3, select = -label))
  y_test <- testSet3$label
  PlotROCCurve(testSet3$TotalScore, as.numeric(y_test) - 1)

  print("Performance using AutoScore (out of sample validation):")
  Modelroc <- roc(y_test, testSet3$TotalScore, quiet = T)
  print("AUC:")
  print(ci(Modelroc))
  print(Modelroc$auc)  ##update
  print("The best cutoff of using this score£º")
  print(coords(Modelroc, "best", ret = "threshold", transpose = TRUE))
  print("Other Performance indicators based on this cutoff: ")
  print(coords(Modelroc, "best", ret = c("specificity", "sensitivity", "accuracy", "npv", "ppv", "precision"), transpose = TRUE))

}
