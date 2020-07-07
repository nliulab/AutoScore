############################################
## built-in function for AutoScore below are functions

split_data <- function(data, ratio) {
  n <- length(data[, 1])
  test_ratio <- ratio[3] / sum(ratio)
  validation_ratio <- ratio[2] / sum(ratio)
  set.seed(4)
  Testindex <- sample((1:n), test_ratio * n)
  Validateindex <-
    sample((1:n)[(1:n) %in% Testindex], validation_ratio * n)
  
  TrainSet <- df_AutoScore[-c(Validateindex, Testindex), ]
  TestSet <- df_AutoScore[Testindex, ]
  ValidationSet <- df_AutoScore[Validateindex, ]
  
  return(list(
    TrainSet = TrainSet,
    ValidationSet = ValidationSet,
    TestSet = TestSet
  ))
}


selectionRF <- function(x, num) {
  library(caret)
  set.seed(4)

  # prepare training scheme control <- trainControl(method='repeatedcv', number=10, repeats=3)
  x$label <- as.factor(x$label)

  # train the model
  model <- randomForest(label ~ ., data = x, preProcess = "scale")

  # estimate variable importance
  importance <- importance(model, scale = FALSE)

  # summarize importance
  b <- importance
  names(b) <- rownames(importance)
  b <- sort(b, decreasing = T)

  a <- length(b)
  if (a > num)
    return(c(names(b)[1:num], "label")) else return(c(names(b), "label"))
}


selectionRF_list <- function(x, num) {
  library(caret)
  set.seed(4)
  # prepare training scheme control <- trainControl(method='repeatedcv', number=10, repeats=3)
  x$label <- as.factor(x$label)

  # train the model
  model <- randomForest(label ~ ., data = x, preProcess = "scale")

  # estimate variable importance
  importance <- importance(model, scale = FALSE)

  # summarize importance

  b <- importance
  names(b) <- rownames(importance)
  b <- sort(b, decreasing = T)

  return(b)
}


Dftransform <- function(x, testSet1, probs = c(0, 0.05, 0.2, 0.8, 0.95, 1), Print_categories = FALSE) {
  CutVec <- list()
  for (i in 1:(length(x) - 1)) {
    if (class(x[, i]) == "factor") {
      if (length(levels(x[, i])) < 10)
        (next)() else stop("Error!! The number of categories should be less than 10")
    }
    # options(scipen = 20)
    a <- quantile(x[, i], probs = probs)
    a <- CheckVector(a)
    a1 <- signif(a, 3)  # remain 3 digits
    if (Print_categories == TRUE)
    {
      print(names(x)[i])
      # print(a1)
      l <- list(a1)
      names(l)[1] <- names(x)[i]
      CutVec <- append(CutVec, l)
    }  #update

    if (length(a1) <= 2) {
      x[, i] <- as.factor(x[, i])
      testSet1[, i] <- as.factor(testSet1[, i])
    } else {
      x[, i] <- cut(x[, i], breaks = a1, right = F, include.lowest = T, dig.lab = 3)
      # xmin<-unlist(strsplit(levels(x[,i])[1],','))[1] xmax<-unlist(strsplit(levels(x[,i])[length(levels(x[,i]))],','))[2]
      levels(x[, i])[1] <- gsub(".*,", "(,", levels(x[, i])[1])
      levels(x[, i])[length(levels(x[, i]))] <- gsub(",.*", ",)", levels(x[, i])[length(levels(x[, i]))])

      at <- a1
      at[1] <- min(testSet1[, i])
      at[length(at)] <- max(testSet1[, i])
      at1 <- signif(at, 3)
      at1 <- CheckVector(at1)  ###revised update##
      testSet1[, i] <- cut(testSet1[, i], breaks = at1, right = F, include.lowest = T, dig.lab = 3)
      # xmin<-as.character(min(at1)) xmax<-as.character(max(at1))
      levels(testSet1[, i])[1] <- gsub(".*,", "(,", levels(testSet1[, i])[1])
      levels(testSet1[, i])[length(levels(testSet1[, i]))] <- gsub(",.*", ",)", levels(testSet1[, i])[length(levels(testSet1[, i]))])

    }
    # print(summary(x[,i]))update print(summary(testSet1[,i]))update

  }

  if (Print_categories == TRUE)
    return(list(x, testSet1, CutVec)) else return(list(x, testSet1))

}



Dftransform_FineTune <- function(x, testSet1, CutVec) {
  j <- 1
  for (i in 1:(length(x) - 1)) {
    if (class(x[, i]) == "factor") {
      if (length(levels(x[, i])) < 10)
        (next)() else stop("Error!! The number of categories should be less than 10")
    }
    # options(scipen = 20)

    a1 <- CutVec[[j]]
    j <- j + 1
    if (length(a1) <= 2) {
      x[, i] <- as.factor(x[, i])
      testSet1[, i] <- as.factor(testSet1[, i])
    } else {
      x[, i] <- cut(x[, i], breaks = a1, right = F, include.lowest = T, dig.lab = 3)
      # xmin<-unlist(strsplit(levels(x[,i])[1],','))[1] xmax<-unlist(strsplit(levels(x[,i])[length(levels(x[,i]))],','))[2]
      levels(x[, i])[1] <- gsub(".*,", "(,", levels(x[, i])[1])
      levels(x[, i])[length(levels(x[, i]))] <- gsub(",.*", ",)", levels(x[, i])[length(levels(x[, i]))])

      at <- a1
      at[1] <- min(testSet1[, i])
      at[length(at)] <- max(testSet1[, i])
      at1 <- signif(at, 3)
      at1 <- CheckVector(at1)  ###revised update##
      testSet1[, i] <- cut(testSet1[, i], breaks = at1, right = F, include.lowest = T, dig.lab = 3)
      # xmin<-as.character(min(at1)) xmax<-as.character(max(at1))
      levels(testSet1[, i])[1] <- gsub(".*,", "(,", levels(testSet1[, i])[1])
      levels(testSet1[, i])[length(levels(testSet1[, i]))] <- gsub(",.*", ",)", levels(testSet1[, i])[length(levels(testSet1[,
                                                                                                                             i]))])


    }
    # print(summary(x[,i]))update print(summary(testSet1[,i]))update

  }
  return(list(x, testSet1))

}


Dftransform_insample <- function(x,probs = c(0, 0.05, 0.2, 0.8, 0.95, 1)) {
  for (i in 1:(length(x) - 1)) {
    a <- quantile(x[, i], probs = probs)
    a <- CheckVector(a)
    # print(a)
    if (length(a) <= 2)
      x[, i] <- as.factor(x[, i]) else x[, i] <- cut(x[, i], breaks = a, right = F, include.lowest = T, dig.lab = 3)

    # print(summary(x[,i]))
  }
  return(x)
}


## This function is for delete repeated items in the Variable tranforamtion
CheckVector <- function(x) {
  dele <- c()
  for (i in 1:(length(x) - 1)) {
    if (x[i] == x[i + 1]) {
      dele <- c(dele, i + 1)
    }
  }
  if (is.null(dele))
    return(x) else return(x[-dele])
}


PlotROCCurve <- function(prob, labels) {
  library(pROC)

  # prob<-predict(model.glm,newdata=X_test, type = 'response')
  Modelroc <- roc(labels, prob, quiet = TRUE)
  auc <- auc(Modelroc)

  roc.data <- data.frame(fpr = as.vector(coords(Modelroc, "local maximas", ret = "1-specificity", transpose = TRUE)), tpr = as.vector(coords(Modelroc,
                                                                                                                                             "local maximas", ret = "sensitivity", transpose = TRUE)))
  p <- ggplot(roc.data, aes(x = fpr, ymin = 0, ymax = tpr)) + geom_ribbon(alpha = 0.2) + geom_line(aes(y = tpr)) + xlab("1-Specificity") +
    ylab("Sensitivity") + ggtitle(paste0("Receiver Operating Characteristic(ROC) Curve \nAUC=", round(auc, digits = 4)))
  print(p)
}


ChangeRef <- function(df1, coefVec) {
  df <- subset(df1, select = -label)
  for (i in (1:length(df))) {
    c1 <- paste("^", names(df)[i], sep = "")
    a <- coefVec[grepl(c1, names(coefVec))]
    if (min(a) < 0) {
      ref <- gsub(names(df)[i], "", names(a)[which.min(a)])
      df[, i] <- relevel(df[, i], ref = ref)
    }
  }

  df$label <- df1$label
  return(df)
}


AddBaseline <- function(df, coefVec) {
  df <- subset(df, select = -label)
  vec <- c()
  m <- 0
  n <- 2
  for (i in (1:length(df))) {
    vec <- c(vec, 0)
    m <- m + 1
    names(vec)[m] <- paste(names(df)[i], levels(df[, i])[1], sep = "")
    for (j in levels(df[, i])[2:length(levels(df[, i]))]) {
      vec <- c(vec, coefVec[n])
      n <- n + 1
      m <- m + 1
    }
  }
  return(vec)
}


AutoTest <- function(df, myvec) {
  for (i in 1:length(names(df))) {
    a <- myvec[grepl(names(df)[i], names(myvec))]
    df[, i] <- as.character(df[, i])
    for (j in 1:length(names(a))) {
      df[, i][df[, i] %in% gsub(names(df)[i], "", names(a)[j])] <- a[j]
    }

    df[, i] <- as.numeric(df[, i])
  }

  return(df)
}


Uni_glmTable <- function(x) {
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

  return(b)
}



Dftransform_fixed <- function(x, CutVec = CutVec) {
  j <- 1
  for (i in 1:(length(x) - 1)) {
    if (class(x[, i]) == "factor") {
      if (length(levels(x[, i])) < 10)
        (next)() else stop("Error!! The number of categories should be less than 9")
    }
    at <- c(min(x[, i]), CutVec[[j]], max(x[, i]))
    at1 <- signif(at, 3)
    at1 <- CheckVector(at1)  ###revised update##
    x[, i] <- cut(x[, i], breaks = at1, right = F, include.lowest = T, dig.lab = 3)
    # xmin<-as.character(min(at1)) xmax<-as.character(max(at1))
    levels(x[, i])[1] <- gsub(".*,", "(,", levels(x[, i])[1])
    levels(x[, i])[length(levels(x[, i]))] <- gsub(",.*", ",)", levels(x[, i])[length(levels(x[, i]))])
    j <- j + 1
  }
  return(x)
}

