# Pipeline_function --------------------------------------------------------

#' @title AutoScore STEP (1) for survival outcomes: Generate variable ranking
#'   List by machine learning (Random Survival Forest) (AutoScore Module 1)
#' @details The first step in the AutoScore framework is variable ranking. We
#'   use Random Survival Forest (RSF) for survival outcome to identify the
#'   top-ranking predictors for subsequent score generation. This step
#'   correspond to Module 1 in the AutoScore-Survival paper.
#' @inheritParams AutoScore_rank
#' @inherit AutoScore_rank return
#' @examples
#' \dontrun{
#' # see AutoScore-Survival Guidebook for the whole 5-step workflow
#' data("sample_data_survival") # Output is named `label_time` and `label_status`
#' ranking <- AutoScore_rank_Survival(sample_data_survival, ntree = 50)
#' }
#' @references
#' \itemize{
#'  \item{Ishwaran, H., Kogalur, U. B., Blackstone, E. H., & Lauer, M. S. (2008).
#'  Random survival forests. The annals of applied statistics, 2(3), 841-860.}
#'  \item{Xie F, Ning Y, Yuan H, et al. AutoScore-Survival: Developing
#'  interpretable machine learning-based time-to-event scores with right-censored
#'  survival data. J Biomed Inform. 2022;125:103959. doi:10.1016/j.jbi.2021.103959}
#' }
#' @seealso \code{\link{AutoScore_parsimony_Survival}},
#'   \code{\link{AutoScore_weighting_Survival}},
#'   \code{\link{AutoScore_fine_tuning_Survival}},
#'   \code{\link{AutoScore_testing_Survival}}.
#' @importFrom randomForestSRC rfsrc vimp
#' @export
AutoScore_rank_Survival <- function(train_set, ntree = 50) {
  #set.seed(4)
  train_set <- AutoScore_impute(train_set)
  model <-
    rfsrc(
      Surv(label_time, label_status) ~ .,
      train_set,
      nsplit = 10,
      ntree = ntree,
      importance = "permute",
      do.trace = T
    )

  # estimate variable importance
  importance <- vimp(model)

  # summarize importance
  importance_a <- sort(importance$importance, decreasing = T)
  cat("The ranking based on variable importance was shown below for each variable: \n")
  print(importance_a)
  plot_importance(importance_a)
  return(importance_a)
}


#' @title AutoScore STEP(ii) for survival outcomes: Select the best model with
#'   parsimony plot (AutoScore Modules 2+3+4)
#' @inheritParams AutoScore_parsimony
#' @param rank the raking result generated from AutoScore STEP(i) for survival
#'   outcomes (\code{\link{AutoScore_rank_Survival}}).
#' @details This is the second step of the general AutoScore-Survival workflow for
#'   ordinal outcomes, to generate the parsimony plot to help select a
#'   parsimonious model. In this step, it goes through AutoScore-Survival Module 2,3 and
#'   4 multiple times and to evaluate the performance under different variable
#'   list. The generated parsimony plot would give researcher an intuitive
#'   figure to choose the best models. If data size is small (eg, <5000), an
#'   independent validation set may not be a wise choice. Then, we suggest using
#'   cross-validation to maximize the utility of data. Set
#'   \code{cross_validation=TRUE}.
#' @return List of iAUC (ie, the integrated AUC by integral under a time-dependent AUC curve
#'  for different number of variables
#' @examples
#' \dontrun{
#' # see AutoScore-Survival Guidebook for the whole 5-step workflow
#' data("sample_data_survival")
#' out_split <- split_data(data = sample_data_survival, ratio = c(0.7, 0.1, 0.2))
#' train_set <- out_split$train_set
#' validation_set <- out_split$validation_set
#' ranking <- AutoScore_rank_Survival(train_set, ntree=10)
#' iAUC <- AutoScore_parsimony_Survival(
#'   train_set = train_set, validation_set = validation_set,
#'   rank = ranking, max_score = 100, n_min = 1, n_max = 20,
#'   categorize = "quantile", quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1)
#' )
#' }
#' @references
#' \itemize{
#'  \item{Xie F, Ning Y, Yuan H, et al. AutoScore-Survival: Developing
#'  interpretable machine learning-based time-to-event scores with right-censored
#'  survival data. J Biomed Inform. 2022;125:103959. doi:10.1016/j.jbi.2021.103959}
#' }
#' @seealso \code{\link{AutoScore_rank_Survival}},
#'   \code{\link{AutoScore_weighting_Survival}},
#'   \code{\link{AutoScore_fine_tuning_Survival}},
#'   \code{\link{AutoScore_testing_Survival}}.
#' @export
#' @import pROC
AutoScore_parsimony_Survival <-
  function(train_set,
           validation_set,
           rank,
           max_score = 100,
           n_min = 1,
           n_max = 20,
           cross_validation = FALSE,
           fold = 10,
           categorize = "quantile",
           quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1),
           max_cluster = 5,
           do_trace = FALSE,
           auc_lim_min = 0.5,
           auc_lim_max = "adaptive") {
    if (n_max > length(rank)) {
      warning(
        "WARNING: the n_max (",
        n_max,
        ") is larger the number of all variables (",
        length(rank),
        "). We Automatically revise the n_max to ",
        length(rank)
      )
      n_max <- length(rank)
    }
    # Cross Validation scenario
    if (cross_validation == TRUE) {
      # Divide the data equally into n fold, record its index number
      #set.seed(4)
      index <- list()
      all <- 1:length(train_set[, 1])
      for (i in 1:(fold - 1)) {
        a <- sample(all, trunc(length(train_set[, 1]) / fold))
        index <- append(index, list(a))
        all <- all[!(all %in% a)]
      }
      index <- c(index, list(all))

      # Create a new variable auc_set to store all AUC value during the cross-validation
      auc_set <- data.frame(rep(0, n_max - n_min + 1))

      # for each fold, generate train_set and validation_set
      for (j in 1:fold) {
        validation_set_temp <- train_set[index[[j]],]
        train_set_tmp <- train_set[-index[[j]],]

        #variable_list <- names(rank)
        AUC <- c()

        # Go through AUtoScore Module 2/3/4 in the loop
        for (i in n_min:n_max) {
          variable_list <- names(rank)[1:i]
          train_set_1 <- train_set_tmp[, c(variable_list, "label_time", "label_status")]
          validation_set_1 <-
            validation_set_temp[, c(variable_list, "label_time", "label_status")]

          model_roc <-
            compute_auc_val_survival(
              train_set_1,
              validation_set_1,
              variable_list,
              categorize,
              quantiles,
              max_cluster,
              max_score
            )
          #print(auc(model_roc))
          AUC <- c(AUC, model_roc)
        }

        # plot parsimony plot for each fold
        names(AUC) <- n_min:n_max

        # only print and plot when do_trace = TRUE
        if (do_trace) {
          print(paste("list of AUC values for fold", j))
          print(data.frame(AUC))
          plot(
            AUC,
            main = paste("Parsimony plot (cross validation) for fold", j),
            xlab = "Number of Variables",
            ylab = "Area Under the Curve",
            col = "#2b8cbe",
            lwd = 2,
            type = "o"
          )
        }

        # store AUC result from each fold into "auc_set"
        auc_set <- cbind(auc_set, data.frame(AUC))
      }

      # finish loop and then output final results averaged by all folds
      auc_set$rep.0..n_max...n_min...1. <- NULL
      auc_set$sum <- rowSums(auc_set) / fold
      cat("***list of final mean AUC values through cross-validation are shown below \n")
      print(data.frame(auc_set$sum))
      plot(plot_auc(AUC = auc_set$sum, variables = names(rank)[n_min:n_max],
                    num = n_min:n_max,
                    auc_lim_min = auc_lim_min, auc_lim_max = auc_lim_max,
                    ylab = "Integrated Area Under the Curve",
                    title = paste0("Final Parsimony plot based on ", fold,
                                   "-fold Cross Validation")))
      return(auc_set)
    }


    # if Cross validation is FALSE
    else{
      AUC <- c()

      # Go through AutoScore Module 2/3/4 in the loop
      for (i in n_min:n_max) {
        cat(paste("Select", i, "Variable(s):  "))

        variable_list <- names(rank)[1:i]
        train_set_1 <- train_set[, c(variable_list, "label_time", "label_status")]
        validation_set_1 <-
          validation_set[, c(variable_list, "label_time", "label_status")]
        model_roc <-
          compute_auc_val_survival(
            train_set_1,
            validation_set_1,
            variable_list,
            categorize,
            quantiles,
            max_cluster,
            max_score
          )
        cat(model_roc,"\n")
        AUC <- c(AUC, model_roc)
      }


      plot(plot_auc(AUC = AUC, variables = names(rank)[n_min:n_max],
                    num = n_min:n_max,
                    auc_lim_min = auc_lim_min, auc_lim_max = auc_lim_max,
                    ylab = "Area Under the Curve",
                    title = "Parsimony plot on the validation set"))
      names(AUC) <- names(rank)[n_min:n_max]

      return(AUC)
    }
  }

#' @title AutoScore STEP(iii) for survival outcomes: Generate the initial score
#'   with the final list of variables (Re-run AutoScore Modules 2+3)
#' @inheritParams AutoScore_weighting
#' @inherit AutoScore_weighting return
#' @inherit AutoScore_parsimony_Survival references
#' @param time_point The time points to be evaluated using time-dependent AUC(t).
#' @examples
#' \dontrun{
#' data("sample_data_survival") #
#' out_split <- split_data(data = sample_data_survival, ratio = c(0.7, 0.1, 0.2))
#' train_set <- out_split$train_set
#' validation_set <- out_split$validation_set
#' ranking <- AutoScore_rank_Survival(train_set, ntree=5)
#' num_var <- 6
#' final_variables <- names(ranking[1:num_var])
#' cut_vec <- AutoScore_weighting_Survival(
#'   train_set = train_set, validation_set = validation_set,
#'   final_variables = final_variables, max_score = 100,
#'   categorize = "quantile", quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1),
#'   time_point = c(1,3,7,14,30,60,90)
#' )
#' }
#' @seealso \code{\link{AutoScore_rank_Survival}},
#'   \code{\link{AutoScore_parsimony_Survival}},
#'   \code{\link{AutoScore_fine_tuning_Survival}},
#'   \code{\link{AutoScore_testing_Survival}}.
#' @export
#' @import knitr
AutoScore_weighting_Survival <-
  function(train_set,
           validation_set,
           final_variables,
           max_score = 100,
           categorize = "quantile",
           max_cluster = 5,
           quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1),
           time_point = c(1,3,7,14,30,60,90)) {
    # prepare train_set and Validation Set
    cat("****Included Variables: \n")
    print(data.frame(variable_name = final_variables))
    train_set_1 <- train_set[, c(final_variables, "label_time", "label_status")]
    validation_set_1 <-
      validation_set[, c(final_variables, "label_time", "label_status")]

    # AutoScore Module 2 : cut numeric and transfer categories and generate "cut_vec"
    cut_vec <-
      get_cut_vec(
        train_set_1,
        categorize = categorize,
        quantiles = quantiles,
        max_cluster = max_cluster
      )
    train_set_2 <- transform_df_fixed(train_set_1, cut_vec)
    validation_set_2 <-
      transform_df_fixed(validation_set_1, cut_vec)

    # AutoScore Module 3 : Score weighting
    score_table <-
      compute_score_table_survival(train_set_2, max_score, final_variables)
    cat("****Initial Scores: \n")
    #print(as.data.frame(score_table))
    print_scoring_table(scoring_table = score_table, final_variable = final_variables)

    # Using "assign_score" to generate score based on new dataset and Scoring table "score_table"
    validation_set_3 <- assign_score(validation_set_2, score_table)
    validation_set_3$total_score <-
      rowSums(subset(validation_set_3, select = names(validation_set_3)[names(validation_set_3) != c("label_status")& names(validation_set_3) != c("label_time")]))
    y_time <- validation_set_3$label_time
    y_status <- validation_set_3$label_status

    # Intermediate evaluation based on Validation Set
    print_performance_survival(validation_set_3$total_score, validation_set_3, time_point)
    cat(
      "***The cutoffs of each variable generated by the AutoScore are saved in cut_vec. You can decide whether to revise or fine-tune them \n"
    )
    #print(cut_vec)
    return(cut_vec)
  }


#' @title AutoScore STEP(iv) for survival outcomes: Fine-tune the score by
#'   revising cut_vec with domain knowledge (AutoScore Module 5)
#' @inherit AutoScore_fine_tuning description return examples
#' @inherit AutoScore_parsimony_Survival references
#' @inheritParams AutoScore_fine_tuning
#' @inheritParams AutoScore_weighting_Survival
#' @param cut_vec Generated from STEP(iii)
#'   \code{AutoScore_weighting_Survival()}.Please follow the guidebook
#' @param time_point The time points to be evaluated using time-dependent AUC(t).
#' @seealso \code{\link{AutoScore_rank_Survival}},
#'   \code{\link{AutoScore_parsimony_Survival}},
#'   \code{\link{AutoScore_weighting_Survival}},
#'   \code{\link{AutoScore_testing_Survival}}.
#' @import pROC
#' @import ggplot2
#' @export
AutoScore_fine_tuning_Survival <-
  function(train_set,
           validation_set,
           final_variables,
           cut_vec,
           max_score = 100,
           time_point = c(1,3,7,14,30,60,90)) {
    # Prepare train_set and Validation Set
    train_set_1 <- train_set[, c(final_variables, "label_time","label_status")]
    validation_set_1 <-
      validation_set[, c(final_variables, "label_time","label_status")]

    # AutoScore Module 2 : cut numeric and transfer categories (based on fix "cut_vec" vector)
    train_set_2 <-
      transform_df_fixed(train_set_1, cut_vec = cut_vec)
    validation_set_2 <-
      transform_df_fixed(validation_set_1, cut_vec = cut_vec)

    # AutoScore Module 3 : Score weighting
    score_table <-
      compute_score_table_survival(train_set_2, max_score, final_variables)
    cat("***Fine-tuned Scores: \n")
    #print(as.data.frame(score_table))
    print_scoring_table(scoring_table = score_table, final_variable = final_variables)

    # Using "assign_score" to generate score based on new dataset and Scoring table "score_table"
    validation_set_3 <- assign_score(validation_set_2, score_table)
    validation_set_3$total_score <-
      rowSums(subset(validation_set_3, select = names(validation_set_3)[names(validation_set_3) != c("label_status")& names(validation_set_3) != c("label_time")])) ## which name ="label"
    y_time <- validation_set_3$label_time
    y_status <- validation_set_3$label_status

    cat("***Performance (based on validation set, after fine-tuning):\n")
    print_performance_survival(validation_set_3$total_score, validation_set_3, time_point)
    return(score_table)
  }


#' @title AutoScore STEP(v) for survival outcomes: Evaluate the final score with
#'   ROC analysis (AutoScore Module 6)
#' @inherit AutoScore_testing description return examples
#' @inherit AutoScore_parsimony_Survival references
#' @inheritParams AutoScore_testing
#' @inheritParams AutoScore_weighting_Survival
#' @param cut_vec Generated from STEP(iii)
#'   \code{AutoScore_weighting_Survival()}.Please follow the guidebook
#' @param with_label Set to TRUE if there are labels(`label_time` and `label_status`) in the test_set and
#'   performance will be evaluated accordingly (Default:TRUE).
#' @param time_point The time points to be evaluated using time-dependent AUC(t).
#' @seealso \code{\link{AutoScore_rank_Survival}},
#'   \code{\link{AutoScore_parsimony_Survival}},
#'   \code{\link{AutoScore_weighting_Survival}},
#'   \code{\link{AutoScore_fine_tuning_Survival}}.
#' @export
AutoScore_testing_Survival <-
  function(test_set,
           final_variables,
           cut_vec,
           scoring_table,
           threshold = "best",
           with_label = TRUE,
           time_point = c(1,3,7,14,30,60,90)) {
    if (with_label) {
      # prepare test set: categorization and "assign_score"
      test_set_1 <- test_set[, c(final_variables, c("label_time","label_status"))]
      test_set_2 <-
        transform_df_fixed(test_set_1, cut_vec = cut_vec)
      test_set_3 <- assign_score(test_set_2, scoring_table)
      test_set_3$total_score <-
        rowSums(subset(test_set_3, select = names(test_set_3)[names(test_set_3) != c("label_status")& names(test_set_3) != c("label_time")]))
      test_set_3$total_score[which(is.na(test_set_3$total_score))] <-
        0

      # Final evaluation based on testing set
      cat("***Performance using AutoScore (based on unseen test Set):\n")
      print_performance_ci_survival(test_set_3$total_score, test_set_3, time_point)
      #Modelprc <- pr.curve(test_set_3$total_score[which(y_test == 1)],test_set_3$total_score[which(y_test == 0)],curve = TRUE)
      #values<-coords(model_roc, "best", ret = c("specificity", "sensitivity", "accuracy", "npv", "ppv", "precision"), transpose = TRUE)
      pred_score <-
        data.frame(pred_score = test_set_3$total_score, label_time = test_set_3$label_time, label_status = test_set_3$label_status)
      return(pred_score)

    } else {
      test_set_1 <- test_set[, c(final_variables)]
      test_set_2 <-
        transform_df_fixed(test_set_1, cut_vec = cut_vec)
      test_set_3 <- assign_score(test_set_2, scoring_table)
      test_set_3$total_score <-
        rowSums(subset(test_set_3, select = names(test_set_3)[names(test_set_3) != c("label_status")& names(test_set_3) != c("label_time")]))
      test_set_3$total_score[which(is.na(test_set_3$total_score))] <-
        0
      pred_score <-
        data.frame(pred_score = test_set_3$total_score, label_time = NA, label_status = NA)
      return(pred_score)
    }
  }


# Direct_function ---------------------------------------------------------

#' @title AutoScore function for survival outcomes: Print predictive performance
#' @description Print mean area under the curve (mAUC) and generalised c-index
#'   (if requested)
#' @param label outcome variable
#' @param score predicted score
#' @seealso \code{\link{AutoScore_testing_Ordinal}}
#' @return No return value and the ROC performance will be printed out directly.
#' @import survAUC survival
#' @export
print_performance_survival <-
  function(score, ValidationSet, time_point) {
    cat("Integrated AUC by all time points: " )
    iAUC<- eva_performance_iauc(score, ValidationSet)
    Surv.rsp.new <- Surv(ValidationSet$label_time, ValidationSet$label_status)
    AUC_c<-concordancefit(Surv.rsp.new, -score)
    cat("C_index: ", AUC_c$concordance, "\n")

    #3. D-index
    #AUC_d<-D.index(x=score, surv.time=ValidationSet$time,surv.event=ValidationSet$status)
    #cat("D_index: ", AUC_d$d.index, "\n")

    #7. AUC_uno all
    AUC_uno <- AUC.uno(Surv.rsp.new, Surv.rsp.new, lpnew = score, times = time_point)
    AUC_t <- data.frame(time_point=time_point, AUC_t=AUC_uno$auc)
    cat("The AUC(t) are shown as bwlow:\n")
    print(AUC_t)

  }

#' @title AutoScore function for survival outcomes: Print predictive performance
#' @description Print mean area under the curve (mAUC) and generalised c-index
#'   (if requested)
#' @param label outcome variable
#' @param score predicted score
#' @seealso \code{\link{AutoScore_testing_Ordinal}}
#' @return No return value and the ROC performance will be printed out directly.
#' @import survAUC survival
#' @export
print_performance_ci_survival  <-
  function(score, ValidationSet, time_point,cycle=100) {
    result_all<-data.frame(matrix(nrow=0,ncol=2+length(time_point)))
    for(i in 1:cycle){
      len<-length(ValidationSet[,1])
      index<-sample(1:len,len,replace = TRUE)
      Val_tmp<-ValidationSet[index,]
      score_tmp <- score[index]
    result<-c()
    iAUC<- eva_performance_iauc(score_tmp, Val_tmp,print = FALSE)
    result<-c(result,iAUC)
    Surv.rsp.new <- Surv(Val_tmp$label_time, Val_tmp$label_status)
    AUC_c<-concordancefit(Surv.rsp.new, -score_tmp)
    result<-c(result,AUC_c$concordance)
    #3. D-index
    #AUC_d<-D.index(x=score, surv.time=ValidationSet$time,surv.event=ValidationSet$status)
    #cat("D_index: ", AUC_d$d.index, "\n")

    #7. AUC_uno all
    AUC_uno <- AUC.uno(Surv.rsp.new, Surv.rsp.new, lpnew = score_tmp, times = time_point)
    AUC_t <- data.frame(time_point=time_point, AUC_t=AUC_uno$auc)
    result<-c(result,AUC_t$AUC_t)
    result_all<-rbind(result_all,result)}

    #colSums(AUC_all_tmp)
    m<-colMeans(result_all)
    up<-sapply(result_all,function(x){quantile(x,probs = c(0.975))})
    down<-sapply(result_all,function(x){quantile(x,probs = c(0.025))})
    result_final<-paste0(round(m,3)," (",round(down,3),"-",round(up,3),")")
    #names(result_final)<-c("iAUC","C_index",time_point)}
    cat("Integrated AUC by all time points: " )
    cat(result_final[1])
    cat("\n")
    cat("C_index: ", result_final[2], "\n")
    AUC_t <- data.frame(time_point=time_point, AUC_t=result_final[-c(1,2)])
    cat("The AUC(t) are shown as bwlow:\n")
    print(AUC_t)

  }


#' @title AutoScore function: Print scoring performance (KM curve) for survival outcomes
#' @description Print scoring performance (KM curve) for survival outcome
#' @param pred_score Generated from STEP(v)\code{AutoScore_testing_Survival()}
#' @param score_cut Score cut-offs to be used for the analysis
#' @param time_point The time points to be evaluated using time-dependent AUC(t).
#' @param risk.table Allowed values include: TRUE or FALSE specifying whether
#'  to show or not the risk table. Default is TRUE.
#' @param title Title displayed in the KM curve
#' @param legend.title Legend title displayed in the KM curve
#' @param xlim limit for x
#' @param break.x.by Threshold for analyze sensitivity,
#' @param ... additional parameters to pass to
#'   \code{\link[survminer]{ggsurvplot}} .
#' @seealso \code{\link{AutoScore_testing_Survival}}
#' @return No return value and the KM performance will be plotted.
#' @export
#' @importFrom survminer ggsurvplot
#' @importFrom survival survfit Surv
plot_survival_km <- function(pred_score, score_cut = c(40,50,60),risk.table=TRUE,
                             title=NULL,
                             legend.title="Score",
                             xlim=c(0,90),break.x.by=30, ...){
  #library(survival)
  test_score<-pred_score$pred_score
  pred_score$test_score_cut<-cut(test_score, breaks = c(min(test_score),score_cut,max(test_score)), right = F, include.lowest = T, dig.lab = 3)
  sfit <- survfit(Surv(label_time, label_status) ~ test_score_cut, data = pred_score)
  summary(sfit, times=seq(min(pred_score$label_time),max(pred_score$label_time),1))
  #summary(sfit, times=seq(0,365,30))
  names(sfit$strata)<-levels(pred_score$test_score_cut)
  ggsurvplot(sfit, data = pred_score, conf.int=TRUE, pval=TRUE, title=title,risk.table=TRUE, palette = "lancet",legend.title = legend.title,
             pval.size=-1,ggtheme=theme_bw(),xlim=xlim,break.x.by=break.x.by, ...)

}


#' @title AutoScore function: Print conversion table for survival outcomes
#' @description Print conversion table for survival outcomes
#' @param pred_score a data frame with outcomes and final scores generated from \code{\link{AutoScore_testing_Survival}}
#' @param score_cut Score cut-offs to be used for generating conversion table
#' @param time_point The time points to be evaluated using time-dependent AUC(t).
#' @seealso \code{\link{AutoScore_testing_Survival}}
#' @return conversion table and the it will also be printed out directly.
#' @export
conversion_table_survival<- function(pred_score, score_cut = c(40,50,60), time_point = c(7,14,30,60,90)){

  test_score<-pred_score$pred_score
  test_score_cut<-cut(test_score, breaks = c(min(test_score),score_cut,max(test_score)), right = F, include.lowest = T, dig.lab = 3)
  seed<-rep(1,length(time_point)+2)
  interval_table_test<-data.frame(seed)

  for(i in levels(test_score_cut)){
    index<-test_score_cut==i
    r<-c()
    r<-c(r,length(pred_score$label_time[index]))
    r<-c(r,convert_percent(length(pred_score$label_time[index])/length(pred_score$label_time)))
    for(j in time_point) r<-c(r,convert_percent(mean(pred_score$label_time[index]<=j)))
    interval_table_test<-cbind(interval_table_test,r)}
  interval_table_test$seed<-NULL
  row.names(interval_table_test)<-c("Number of patients","Percentage of patients",paste0("t=",time_point))
  names(interval_table_test)<- levels(test_score_cut)
  #ktable(interval_table_test)
  return(interval_table_test)
}

#' @title AutoScore function: Univariate Analysis for survival outcomes
#' @description Generate tables for Univariate analysis for survival outcomes
#' @param df data frame after checking
#' @return result of the Univariate analysis for survival outcomes
#' @examples
#' data("sample_data_survival")
#' uni_table<-compute_uni_variable_table_survival(sample_data_survival)
#' @import survival
#' @export
compute_uni_variable_table_survival <- function(df) {
  uni_table <- data.frame()
  for (i in names(df)[names(df) != c("label_status")& names(df) != c("label_time")]) {
    model <-
      coxph(Surv(label_time, label_status) ~ ., data=subset(df, select = c("label_time","label_status", i)))
    coef_vec <- coef(model)

    a <-
      cbind(exp(cbind(OR = coef(model), confint.default(model))), summary(model)$coef[, "Pr(>|z|)"])
    uni_table <- rbind(uni_table, a)
  }
  #uni_table <-
  #  uni_table[!grepl("Intercept", row.names(uni_table), ignore.case = T), ]
  uni_table <- round(uni_table, digits = 3)
  uni_table$V4[uni_table$V4 < 0.001] <- "<0.001"
  uni_table$OR <-
    paste(uni_table$OR,
          "(",
          uni_table$`2.5 %`,
          "-",
          uni_table$`97.5 %`,
          ")",
          sep = "")
  uni_table$`2.5 %` <- NULL
  uni_table$`97.5 %` <- NULL
  names(uni_table)[names(uni_table) == "V4"] <- "p value"
  return(uni_table)
}


#' @title AutoScore function: Multivariate Analysis for survival outcomes
#' @description Generate tables for multivariate analysis for survival outcomes
#' @param df data frame after checking
#' @return result of the multivariate analysis for survival outcomes
#' @examples
#' data("sample_data_survival")
#' multi_table<-compute_multi_variable_table_survival(sample_data_survival)
#' @import survival
#' @export
compute_multi_variable_table_survival <- function(df) {
  model <-
    coxph(Surv(label_time, label_status) ~ ., data = df)
  coef_vec <- coef(model)
  #model1 <-
  #  glm(status ~ ., data = df,family = binomial,
  #      na.action = na.omit)

  multi_table <-
    cbind(exp(cbind(
      adjusted_OR = coef(model), confint.default(model)
    )), summary(model)$coef[, "Pr(>|z|)"])
  #multi_table <-
  #  multi_table[!grepl("Intercept", row.names(multi_table), ignore.case = T), ]
  multi_table <- round(multi_table, digits = 3)
  multi_table <- as.data.frame(multi_table)
  multi_table$V4[multi_table$V4 < 0.001] <- "<0.001"
  multi_table$adjusted_OR <-
    paste(
      multi_table$adjusted_OR,
      "(",
      multi_table$`2.5 %`,
      "-",
      multi_table$`97.5 %`,
      ")",
      sep = ""
    )
  multi_table$`2.5 %` <- NULL
  multi_table$`97.5 %` <- NULL
  names(multi_table)[names(multi_table) == "V4"] <- "p value"
  return(multi_table)
}

# Internal_function -------------------------------------------------------
## built-in function for AutoScore below
## Those functions are cited by pipeline functions

#' @title Internal function: Compute scoring table for survival outcomes based on training dataset (AutoScore Module 3)
#' @description Compute scoring table for survival outcomes based on training dataset
#' @param train_set_2 Processed training set after variable transformation (AutoScore Module 2)
#' @param max_score Maximum total score
#' @param variable_list List of included variables
#' @return A scoring table
#' @import survival
compute_score_table_survival <-
  function(train_set_2, max_score, variable_list) {
    #AutoScore Module 3 : Score weighting
    # First-step logistic regression

    model <-
      coxph(Surv(label_time, label_status) ~ ., data = train_set_2)
    coef_vec <- coef(model)
    if (length(which(is.na(coef_vec))) > 0) {
      warning(" WARNING: GLM output contains NULL, Replace NULL with 1")
      coef_vec[which(is.na(coef_vec))] <- 1
    }
    train_set_2 <- change_reference(train_set_2, coef_vec)

    # Second-step logistic regression
    model <-
      coxph(Surv(label_time, label_status) ~ ., data = train_set_2)
    coef_vec <- coef(model)
    if (length(which(is.na(coef_vec))) > 0) {
      warning(" WARNING: GLM output contains NULL, Replace NULL with 1")
      coef_vec[which(is.na(coef_vec))] <- 1
    }

    # rounding for final scoring table "score_table"
    coef_vec_tmp <- round(coef_vec / min(coef_vec))
    score_table <- add_baseline(train_set_2, coef_vec_tmp)

    # normalization according to "max_score" and regenerate score_table
    total_max <- max_score
    total <- 0
    for (i in 1:length(variable_list))
      total <-
      total + max(score_table[grepl(variable_list[i], names(score_table))])
    score_table <- round(score_table / (total / total_max))
    return(score_table)
  }


#' @title Internal function for survival outcomes: Compute AUC based on validation set for plotting parsimony (AutoScore Module 4)
#' @description  Compute AUC based on validation set for plotting parsimony (survival outcomes)
#' @param train_set_1 Processed training set
#' @param validation_set_1 Processed validation set
#' @param max_score Maximum total score
#' @param variable_list List of included variables
#' @param categorize  Methods for categorize continuous variables. Options include "quantile" or "kmeans"
#' @param quantiles Predefined quantiles to convert continuous variables to categorical ones. Available if \code{categorize = "quantile"}.
#' @param max_cluster The max number of cluster (Default: 5). Available if \code{categorize = "kmeans"}.
#' @return A List of AUC for parsimony plot
compute_auc_val_survival <-
  function(train_set_1,
           validation_set_1,
           variable_list,
           categorize,
           quantiles,
           max_cluster,
           max_score) {
    # AutoScore Module 2 : cut numeric and transfer categories
    cut_vec <-
      get_cut_vec(
        train_set_1,
        categorize = categorize,
        quantiles = quantiles,
        max_cluster = max_cluster
      )
    train_set_2 <- transform_df_fixed(train_set_1, cut_vec)
    validation_set_2 <-
      transform_df_fixed(validation_set_1, cut_vec)
    if (sum(is.na(validation_set_2)) > 0)
      warning("NA in the validation_set_2: ", sum(is.na(validation_set_2)))
    if (sum(is.na(train_set_2)) > 0)
      warning("NA in the train_set_2: ", sum(is.na(train_set_2)))

    # AutoScore Module 3 : Variable Weighting
    score_table <-
      compute_score_table_survival(train_set_2, max_score, variable_list)
    if (sum(is.na(score_table)) > 0)
      warning("NA in the score_table: ", sum(is.na(score_table)))

    # Using "assign_score" to generate score based on new dataset and Scoring table "score_table"
    validation_set_3 <- assign_score(validation_set_2, score_table)
    if (sum(is.na(validation_set_3)) > 0)
      warning("NA in the validation_set_3: ", sum(is.na(validation_set_3)))

    validation_set_3$total_score <-
      rowSums(subset(validation_set_3, select = names(validation_set_3)[names(validation_set_3) != "label_status" & names(validation_set_3) != "label_time"]))
    y_time <- validation_set_3$label_time
    y_status <- validation_set_3$label_status
    # plot_roc_curve(validation_set_3$total_score,as.numeric(y_validation)-1)

    # calculate iAUC value for plotting parsimony plot.
    model_roc <-
      eva_performance_iauc( validation_set_3$total_score, validation_set_3,print=FALSE)

    return(model_roc)
  }



#' calculate iAUC for validation set (survival outcome)
#' @import survival
#' @import survAUC
eva_performance_iauc<-function(score,ValidationSet,print=TRUE){
  AUC<-c()
  #1. iAUC_uno +
  Surv.rsp.new <- Surv(ValidationSet$label_time, ValidationSet$label_status)

  #iAUC
  km_fit_test <- survfit(Surv.rsp.new ~ 1, data = ValidationSet)
  km_time<-summary(km_fit_test)$time
  km_survival<-summary(km_fit_test)$surv
  km_time<-km_time[-length(km_time)]
  km_survival<-km_survival[-length(km_survival)]

  AUC_uno <- AUC.uno(Surv.rsp.new, Surv.rsp.new, lpnew = score, times = km_time)
  #iAUC<-IntAUC(AUC_uno$auc,AUC_uno$times,km_survival,90,auc.type = "cumulative")
  km_survival_w<-c(1,km_survival[-length(km_survival)])
  km_survival_sum<-sum(km_survival_w-km_survival)
  weight<-(km_survival_w-km_survival)/km_survival_sum
  iAUC<-sum(weight*AUC_uno$auc)
  if(print){cat(iAUC)
  cat("\n")}
  return(iAUC)
}


convert_percent<-function(m){paste(round(100*m, 2), "%", sep="")}



# Data --------------------------------------------------------------------

#' 20000 simulated MIMIC sample data with survival outcomes
#' @description 20000 simulated samples, with the same distribution
#' as the data in the MIMIC-III ICU database. Data were simulated based on the dataset
#'   analysed in the AutoScore-Survival paper. It is used for demonstration
#' only in the Guidebook. Run \code{vignette("Guide_book", package = "AutoScore")}
#' to see the guidebook or vignette.
#'  \itemize{
#'  \item{Johnson, A., Pollard, T., Shen, L. et al. MIMIC-III, a freely accessible critical care database. Sci Data 3, 160035 (2016).}
#' }
"sample_data_survival"

#' 1000 simulated MIMIC sample data with survival outcomes
#' @description 1000 simulated samples, with the same distribution
#' as the data in the MIMIC-III ICU database. Data were simulated based on the dataset
#'   analysed in the AutoScore-Survival paper. It is used for demonstration
#' only in the Guidebook. Run \code{vignette("Guide_book", package = "AutoScore")}
#' to see the guidebook or vignette.
#'  \itemize{
#'  \item{Johnson, A., Pollard, T., Shen, L. et al. MIMIC-III, a freely accessible critical care database. Sci Data 3, 160035 (2016).}
#' }
"sample_data_survival_small"

