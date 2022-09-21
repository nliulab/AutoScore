
# Pipeline_function --------------------------------------------------------

#' @title AutoScore STEP (i) for ordinal outcomes: Generate variable ranking
#'   list by machine learning (AutoScore Module 1)
#' @details The first step in the AutoScore framework is variable ranking. We
#'   use random forest (RF) for multiclass classification to identify the
#'   top-ranking predictors for subsequent score generation. This step
#'   corresponds to Module 1 in the AutoScore-Ordinal paper.
#' @inheritParams AutoScore_rank
#' @inherit AutoScore_rank return
#' @examples
#' \dontrun{
#' # see AutoScore-Ordinal Guidebook for the whole 5-step workflow
#' data("sample_data_ordinal") # Output is named `label`
#' ranking <- AutoScore_rank_ordinal(sample_data_ordinal, ntree = 50)
#' }
#' @references
#' \itemize{
#'  \item{Breiman, L. (2001), Random Forests, Machine Learning 45(1), 5-32}
#'  \item{Saffari SE, Ning Y, Feng X, Chakraborty B, Volovici V, Vaughan R, Ong
#'        ME, Liu N, AutoScore-Ordinal: An interpretable machine learning framework for
#'        generating scoring models for ordinal outcomes, arXiv:2202.08407}
#' }
#' @seealso \code{\link{AutoScore_parsimony_Ordinal}},
#'   \code{\link{AutoScore_weighting_Ordinal}},
#'   \code{\link{AutoScore_fine_tuning_Ordinal}},
#'   \code{\link{AutoScore_testing_Ordinal}}.
#' @importFrom randomForest randomForest importance
#' @export
AutoScore_rank_Ordinal <- function(train_set, ntree = 100) {
  train_set <- AutoScore_impute(train_set)
  train_set$label <- ordered(train_set$label) # Ordered factor
  model <- randomForest::randomForest(label ~ ., data = train_set, ntree = ntree,
                                      preProcess = "scale")

  # estimate variable importance
  importance <- randomForest::importance(model, scale = F)

  # summarize importance
  names(importance) <- rownames(importance)
  importance <- sort(importance, decreasing = T)
  cat("The ranking based on variable importance was shown below for each variable: \n")
  print(importance)
  plot_importance(importance)
  return(importance)
}


#' @title AutoScore STEP(ii) for ordinal outcomes: Select the best model with
#'   parsimony plot (AutoScore Modules 2+3+4)
#' @inheritParams AutoScore_parsimony
#' @param rank The raking result generated from AutoScore STEP(i) for ordinal
#'   outcomes (\code{\link{AutoScore_rank_Ordinal}}).
#' @param link The link function used to model ordinal outcomes. Default is
#'   \code{"logit"} for proportional odds model. Other options are
#'   \code{"cloglog"} (proportional hazards model) and \code{"probit"}.
#' @details This is the second step of the general AutoScore workflow for
#'   ordinal outcomes, to generate the parsimony plot to help select a
#'   parsimonious model. In this step, it goes through AutoScore Module 2,3 and
#'   4 multiple times and to evaluate the performance under different variable
#'   list. The generated parsimony plot would give researcher an intuitive
#'   figure to choose the best models. If data size is small (eg, <5000), an
#'   independent validation set may not be a wise choice. Then, we suggest using
#'   cross-validation to maximize the utility of data. Set
#'   \code{cross_validation=TRUE}.
#' @return List of mAUC (ie, the average AUC of dichotomous classifications)
#'   value for different number of variables
#' @examples
#' \dontrun{
#' # see AutoScore-Ordinal Guidebook for the whole 5-step workflow
#' data("sample_data_ordinal") # Output is named `label`
#' out_split <- split_data(data = sample_data_ordinal, ratio = c(0.7, 0.1, 0.2))
#' train_set <- out_split$train_set
#' validation_set <- out_split$validation_set
#' ranking <- AutoScore_rank_Ordinal(train_set, ntree=100)
#' mAUC <- AutoScore_parsimony_Ordinal(
#'   train_set = train_set, validation_set = validation_set,
#'   rank = ranking, max_score = 100, n_min = 1, n_max = 20,
#'   categorize = "quantile", quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1)
#' )
#' }
#' @references
#' \itemize{
#'  \item{Saffari SE, Ning Y, Feng X, Chakraborty B, Volovici V, Vaughan R, Ong
#'        ME, Liu N, AutoScore-Ordinal: An interpretable machine learning framework for
#'        generating scoring models for ordinal outcomes, arXiv:2202.08407}
#' }
#' @seealso \code{\link{AutoScore_rank_Ordinal}},
#'   \code{\link{AutoScore_weighting_Ordinal}},
#'   \code{\link{AutoScore_fine_tuning_Ordinal}},
#'   \code{\link{AutoScore_testing_Ordinal}}.
#' @export
#' @import pROC
AutoScore_parsimony_Ordinal <- function(train_set, validation_set, rank,
                                        link = "logit",
                                        max_score = 100, n_min = 1, n_max = 20,
                                        cross_validation = FALSE, fold = 10,
                                        categorize = "quantile",
                                        quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1),
                                        max_cluster = 5,
                                        do_trace = FALSE,
                                        auc_lim_min = 0.5,
                                        auc_lim_max = "adaptive") {
  link <- check_link(link = link)
  if (n_max > length(rank)) {
    warning("WARNING: the n_max (", n_max, ") is larger the number of all variables (",
            length(rank), "). We Automatically revise the n_max to ", length(rank))
    n_max <- length(rank)
  }
  # Cross Validation scenario
  if (cross_validation == TRUE) {
    # Divide the data equally into n fold, record its index number
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
      train_set_tmp <- train_set[-index[[j]], ]
      validation_set_temp <- train_set[index[[j]], ]

      # Go through AutoScore-Ordinal Module 2/3/4 in the loop
      mauc <- unlist(lapply(n_min:n_max, function(i) {
        cat("Select", i, "variables:  ")
        variable_list <- names(rank)[1:i]
        train_set_1 <- train_set_tmp[, c(variable_list, "label")]
        validation_set_1 <- validation_set_temp[, c(variable_list, "label")]
        model_auc <- compute_auc_val_ord(train_set_1 = train_set_1,
                                         validation_set_1 = validation_set_1,
                                         variable_list = variable_list,
                                         link = link,
                                         categorize = categorize,
                                         quantiles = quantiles,
                                         max_cluster = max_cluster,
                                         max_score = max_score)
        cat("Mean area under the curve:", model_auc, "\n")
        model_auc
      }))
      # plot parsimony plot for each fold
      names(mauc) <- n_min:n_max

      # only print and plot when do_trace = TRUE
      if (do_trace) {
        print(paste("list of mAUC values for fold",j))
        print(data.frame(mAUC = mauc))

        plot(mauc, main = paste("Parsimony plot (cross validation) for fold",j),
             xlab = "Number of Variables", ylab = "Mean Area Under the Curve",
             col = "#2b8cbe", lwd = 2, type = "o")}

      # store AUC result from each fold into "auc_set"
      auc_set <- cbind(auc_set, data.frame(mAUC = mauc))
    }

    # finish loop and then output final results averaged by all folds
    auc_set$rep.0..n_max...n_min...1. <- NULL
    auc_set$sum <- rowSums(auc_set) / fold
    cat("list of fianl Mean AUC values through cross validation are shown below")
    print(data.frame(mAUC = auc_set$sum))
    plot(plot_auc(AUC = auc_set$sum, variables = names(rank)[n_min:n_max],
                  num = n_min:n_max,
                  auc_lim_min = auc_lim_min, auc_lim_max = auc_lim_max,
                  ylab = "Mean Area Under the Curve",
                  title = paste0("Final Parsimony plot based on ", fold,
                                 "-fold Cross Validation")))
    return(auc_set)
  } else {# Go through AutoScore-Ordinal Module 2/3/4 in the loop
    mauc <- unlist(lapply(n_min:n_max, function(i) {
      cat("Select", i, "variables:  ")
      variable_list <- names(rank)[1:i]
      train_set_1 <- train_set[, c(variable_list, "label")]
      validation_set_1 <- validation_set[, c(variable_list, "label")]
      model_auc <- compute_auc_val_ord(train_set_1 = train_set_1,
                                       validation_set_1 = validation_set_1,
                                       variable_list = variable_list,
                                       link = link,
                                       categorize = categorize,
                                       quantiles = quantiles,
                                       max_cluster = max_cluster,
                                       max_score = max_score)
      cat("Mean area under the curve:", model_auc, "\n")
      model_auc
    }))
    # output final results and plot parsimony plot
    plot(plot_auc(AUC = mauc, variables = names(rank)[n_min:n_max],
                  num = n_min:n_max,
                  auc_lim_min = auc_lim_min, auc_lim_max = auc_lim_max,
                  ylab = "Mean Area Under the Curve",
                  title = "Parsimony plot on the validation set"))
    names(mauc) <- names(rank)[n_min:n_max]
    mauc
  }
}
#' @title AutoScore STEP(iii) for ordinal outcomes: Generate the initial score
#'   with the final list of variables (Re-run AutoScore Modules 2+3)
#' @inheritParams AutoScore_weighting
#' @inheritParams AutoScore_parsimony_Ordinal
#' @param final_variables A vector containing the list of selected variables,
#'   selected from Step(ii) \code{\link{AutoScore_parsimony_Ordinal}}.
#' @return Generated \code{cut_vec} for downstream fine-tuning process STEP(iv)
#'   \code{\link{AutoScore_fine_tuning_Ordinal}}.
#' @inherit AutoScore_parsimony_Ordinal references
#' @param n_boot Number of bootstrap cycles to compute 95\% CI for performance
#'   metrics.
#' @examples
#' \dontrun{
#' data("sample_data_ordinal") # Output is named `label`
#' out_split <- split_data(data = sample_data_ordinal, ratio = c(0.7, 0.1, 0.2))
#' train_set <- out_split$train_set
#' validation_set <- out_split$validation_set
#' ranking <- AutoScore_rank_Ordinal(train_set, ntree=100)
#' num_var <- 6
#' final_variables <- names(ranking[1:num_var])
#' cut_vec <- AutoScore_weighting_Ordinal(
#'   train_set = train_set, validation_set = validation_set,
#'   final_variables = final_variables, max_score = 100,
#'   categorize = "quantile", quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1)
#' )
#' }
#' @seealso \code{\link{AutoScore_rank_Ordinal}},
#'   \code{\link{AutoScore_parsimony_Ordinal}},
#'   \code{\link{AutoScore_fine_tuning_Ordinal}},
#'   \code{\link{AutoScore_testing_Ordinal}}.
#' @export
#' @import knitr
AutoScore_weighting_Ordinal <- function(train_set, validation_set, final_variables,
                                        link = "logit", max_score = 100,
                                        categorize = "quantile",
                                        quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1),
                                        max_cluster = 5, n_boot = 100) {
  link <- check_link(link = link)
  # prepare train_set and validation_set
  cat("****Included Variables: \n")
  print(data.frame(variable_name = final_variables))
  train_set_1 <- train_set[, c(final_variables, "label")]

  # AutoScore Module 2 : cut numeric and transform categories and generate "cut_vec"
  cut_vec <- get_cut_vec(df = train_set_1, categorize = categorize,
                         quantiles = quantiles, max_cluster = max_cluster)
  train_set_2 <- transform_df_fixed(df = train_set_1, cut_vec = cut_vec)

  # AutoScore Module 3 : Score weighting
  score_table <- compute_score_table_ord(train_set_2 = train_set_2,
                                         max_score = max_score,
                                         variable_list = final_variables,
                                         link = link)
  cat("****Initial Scores: \n")
  print_scoring_table(scoring_table = score_table, final_variable = final_variables)

  # Intermediate evaluation based on Validation Set
  validation_set_1 <- validation_set[, c(final_variables, "label")]
  validation_set_1$total_score <- compute_final_score_ord(
    data = validation_set_1, final_variables = final_variables,
    cut_vec = cut_vec, scoring_table = score_table
  )
  cat("***Performance (based on validation set):\n")
  print_performance_ordinal(label = validation_set_1$label,
                            score = validation_set_1$total_score,
                            n_boot = n_boot, report_cindex = FALSE)
  cat("***The cutoffs of each variables generated by the AutoScore-Ordinal are saved in cut_vec. You can decide whether to revise or fine-tune them \n")
  return(cut_vec)
}
#' @title AutoScore STEP(iv) for ordinal outcomes: Fine-tune the score by
#'   revising \code{cut_vec} with domain knowledge (AutoScore Module 5)
#' @inherit AutoScore_fine_tuning description return examples
#' @inherit AutoScore_parsimony_Ordinal references
#' @inheritParams AutoScore_fine_tuning_Ordinal
#' @inheritParams AutoScore_weighting_Ordinal
#' @param cut_vec Generated from STEP(iii) \code{\link{AutoScore_weighting_Ordinal}}.
#' @param report_cindex Whether to report generalized c-index for model
#'   evaluation (Default:FALSE for faster evaluation).
#' @seealso \code{\link{AutoScore_rank_Ordinal}},
#'   \code{\link{AutoScore_parsimony_Ordinal}},
#'   \code{\link{AutoScore_weighting_Ordinal}},
#'   \code{\link{AutoScore_testing_Ordinal}}.
#' @import pROC
#' @import ggplot2
#' @export
AutoScore_fine_tuning_Ordinal <- function(train_set, validation_set, final_variables,
                                          link = "logit",
                                          cut_vec, max_score = 100, n_boot = 100,
                                          report_cindex = FALSE) {
  link <- check_link(link = link)
  # Prepare train_set and VadalitionSet
  train_set_1 <- train_set[, c(final_variables, "label")]
  # AutoScore Module 2 : cut numeric and transfer categories (based on fix "cut_vec" vector)
  train_set_2 <- transform_df_fixed(df = train_set_1, cut_vec = cut_vec)
  # AutoScore Module 3 : Score weighting
  score_table <- compute_score_table_ord(train_set_2 = train_set_2,
                                         max_score = max_score,
                                         variable_list = final_variables,
                                         link = link)
  cat("***Fine-tuned Scores: \n")
  print_scoring_table(scoring_table = score_table, final_variable = final_variables)

  validation_set_1 <- validation_set[, c(final_variables, "label")]
  validation_set_1$total_score <- compute_final_score_ord(
    data = validation_set_1, final_variables = final_variables,
    cut_vec = cut_vec, scoring_table = score_table
  )
  cat("***Performance (based on Validation Set, after fine-tuning):\n")
  print_performance_ordinal(label = validation_set_1$label,
                            score = validation_set_1$total_score,
                            n_boot = n_boot, report_cindex = report_cindex)
  # Intermediate evaluation based on Validation Set after fine-tuning
  return(score_table)
}
#' @title AutoScore STEP(v) for ordinal outcomes: Evaluate the final score
#'   (AutoScore Module 6)
#' @inherit AutoScore_testing description return examples
#' @inherit AutoScore_parsimony_Ordinal references
#' @param test_set A processed data.frame that contains data for testing
#'   purpose. This data.frame should have same format as train_set (same
#'   variable names and outcomes)
#' @param scoring_table The final scoring table after fine-tuning, generated
#'   from STEP(iv) \code{\link{AutoScore_fine_tuning_Ordinal}}.Please follow the
#'   guidebook
#' @inheritParams AutoScore_fine_tuning_Ordinal
#' @param with_label Set to TRUE if there are labels in the test_set and
#'   performance will be evaluated accordingly (Default:TRUE).
#' @seealso \code{\link{AutoScore_rank_Ordinal}},
#'   \code{\link{AutoScore_parsimony_Ordinal}},
#'   \code{\link{AutoScore_weighting_Ordinal}},
#'   \code{\link{AutoScore_fine_tuning_Ordinal}}.
#' @export
AutoScore_testing_Ordinal <- function(test_set, final_variables, link = "logit",
                                      cut_vec, scoring_table, with_label = TRUE,
                                      n_boot = 100) {
  link <- check_link(link = link)
  # prepare test set: categorization and "assign_score"
  if (with_label) {
    test_set_1 <- test_set[, c(final_variables, "label")]
  } else {
    test_set_1 <- cbind(test_set[, final_variables], label = NA)
  }
  if (with_label) {
    test_set_1$total_score <- compute_final_score_ord(
      data = test_set_1, final_variables = final_variables,
      cut_vec = cut_vec, scoring_table = scoring_table
    )
    cat("***Performance using AutoScore-Ordinal (based on unseen test Set):\n")
    print_performance_ordinal(label = test_set_1$label,
                              score = test_set_1$total_score,
                              n_boot = n_boot, report_cindex = TRUE)
    return(data.frame(pred_score = test_set_1$total_score, Label = test_set_1$label))
  } else {
    return(data.frame(pred_score = test_set_1$total_score, Label = NA))
  }
}


# Direct functions --------------------------------------------------------


#' @title AutoScore function for ordinal outcomes: Check whether the input
#'   dataset fulfil the requirement of the AutoScore
#' @inheritParams check_data
#' @inherit check_data return
#' @examples
#' data("sample_data_ordinal")
#' check_data_ordinal(sample_data_ordinal)
#' @export
check_data_ordinal <- function(data) {
  surv_labels <- c("label_time", "label_status")
  #1. check label and binary
  if (is.null(data$label)) {
    if (any(surv_labels %in% names(data))) {
      stop(paste0(
        "ERROR: detected labels for survival data ('",
        paste(surv_labels, collapse = "', '"),
        "'). If outcome is survival, please use function 'check_data_survival()' and pipeline functions for survival outcome."
      ))
    }
    stop(
      "ERROR: for this dataset: These is no dependent variable 'label' to indicate the outcome. Please add one first\n"
    )
  }
  if (!"factor" %in% class(data$label)) {
    warning("Please code outcome label variable as a factor\n")
  }
  if (length(levels(factor(data$label))) < 3)
    warning("Expecting 3 or more categories in an ordinal outcome. If 'label' only has 2 categories, please use function 'check_data()' and pipeline functions for binary outcome.")
  check_predictor(data_predictor = data[, setdiff(names(data), "label")])
}
#' @title AutoScore-Ordinal function: Univariable Analysis
#' @description Perform univariable analysis and generate the result table with
#'   odd ratios from proportional odds models.
#' @inheritParams compute_uni_variable_table
#' @inheritParams extract_or_ci_ord
#' @inheritParams AutoScore_parsimony_Ordinal
#' @return result of univariate analysis
#' @examples
#' data("sample_data_ordinal")
#' # Using just a few variables to demonstrate usage:
#' uni_table<-compute_uni_variable_table_ordinal(sample_data_ordinal[, 1:6])
#' @importFrom ordinal clm
#' @export
compute_uni_variable_table_ordinal <- function(df, link = "logit", n_digits = 3) {
  link <- check_link(link = link)
  x_names <- setdiff(names(df), "label")
  tb <- do.call("rbind", lapply(x_names, function(x_name) {
    model <- ordinal::clm(as.formula("label ~ ."), 
                          data = df[, c("label", x_name)],
                          link = link, na.action = na.omit)
    extract_or_ci_ord(model = model, n_digits = n_digits)
  }))
  if (link != "logit") names(tb)[1] <- "exp(coefficient)"
  tb
}
#' @title AutoScore-Ordinal function: Multivariate Analysis
#' @description Generate tables for multivariate analysis
#' @inheritParams compute_multi_variable_table
#' @inheritParams extract_or_ci_ord
#' @inheritParams AutoScore_parsimony_Ordinal
#' @return result of the multivariate analysis
#' @examples
#' data("sample_data_ordinal")
#' # Using just a few variables to demonstrate usage:
#' multi_table<-compute_multi_variable_table_ordinal(sample_data_ordinal[, 1:6])
#' @importFrom ordinal clm
#' @export
compute_multi_variable_table_ordinal <- function(df, link = "logit", n_digits = 3) {
  link <- check_link(link = link)
  model <- ordinal::clm(as.formula("label ~ ."), data = df,
                        link = link, na.action = na.omit)
  tb <- extract_or_ci_ord(model = model, n_digits = n_digits)
  if (link == "logit") names(tb)[1] <- "adjusted_OR"
  if (link != "logit") names(tb)[1] <- "exp(adjusted_coefficient)"
  tb
}
#' @title AutoScore function for ordinal outcomes: Print predictive performance
#' @description Print mean area under the curve (mAUC) and generalised c-index
#'   (if requested)
#' @inheritParams print_roc_performance
#' @inheritParams AutoScore_fine_tuning_Ordinal
#' @seealso \code{\link{AutoScore_testing_Ordinal}}
#' @return No return value and the ROC performance will be printed out directly.
#' @export
print_performance_ordinal <- function(label, score, n_boot = 100, report_cindex = FALSE) {
  n_boot <- round(n_boot)
  if (n_boot < 1) n_boot <- 1
  if (all(is.na(score))) stop(simpleError("All entries in score are NA."))
  i_na <- which(is.na(score))
  if (length(i_na) > 0) {
    warning("NA in the score: ", length(i_na))
    label <- label[-i_na]
    score <- score[-i_na]
  }
  perf_list <- evaluate_model_ord(label = label, score = score, n_boot = n_boot,
                                  report_cindex = report_cindex)
  if (n_boot > 1) {
    cat(sprintf(
      "mAUC: %.4f \t 95%% CI: %.4f-%.4f (from %d bootstrap samples)\n",
      perf_list$mauc, min(perf_list$mauc_ci), max(perf_list$mauc_ci), n_boot
    ))
    if (report_cindex) {
      cat(sprintf(
        "Generalised c-index: %.4f \t 95%% CI: %.4f-%.4f (from %d bootstrap samples)\n",
        perf_list$cindex, min(perf_list$cindex_ci), max(perf_list$cindex_ci), n_boot
      ))
    }
  } else {
    message(simpleMessage("To obtain 95% CI of metrics, set n_boot>1.\n"))
    cat(sprintf("mAUC: %.4f\n", perf_list$mauc))
    if (report_cindex) {
      cat(sprintf("Generalised c-index: %.4f\n", perf_list$cindex))
    }
  }
}
#' @title AutoScore function: Print conversion table for ordinal outcomes to map score to risk
#' @inheritParams AutoScore_testing_Ordinal
#' @param pred_score A \code{data.frame} with outcomes and final scores
#'   generated from \code{\link{AutoScore_fine_tuning_Ordinal}}
#' @param score_breaks A vector of score breaks to group scores. The average
#'   predicted risk will be reported for each score interval in the lookup
#'   table. Users are advised to first visualise the predicted risk for all
#'   attainable scores to determine \code{scores} (see
#'   \code{\link{plot_predicted_risk}})
#' @param max_score Maximum attainable value of final scores.
#' @param ... Additional parameters to pass to \code{\link[knitr]{kable}}.
#' @seealso \code{\link{AutoScore_testing_Ordinal}}
#' @inherit conversion_table return
#' @export
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom knitr kable
conversion_table_ordinal <- function(pred_score, link = "logit", max_score = 100,
                                     score_breaks = seq(from = 5, to = 70, by = 5),
                                     ...) {
  lookup_long <- compute_prob_predicted(pred_score = pred_score, link = link,
                                        max_score = max_score,
                                        score_breaks = score_breaks)
  lookup <- pivot_wider(data = lookup_long, id_cols = .data$score,
                        names_from = .data$`Outcome category`,
                        values_from = .data$p_label)
  names(lookup) <- c("Score", paste("Predicted risk, category", names(lookup)[-1]))
  knitr::kable(lookup, ...)
}


# Internal functions ------------------------------------------------------


#' Internal function: Check link function
#' @inheritParams AutoScore_parsimony_Ordinal
check_link <- function(link) {
  match.arg(arg = tolower(link), choices = c("logit", "cloglog", "probit"))
}
#' Extract OR, CI and p-value from a proportional odds model
#' @param model An ordinal regression model fitted using \code{\link[ordinal]{clm}}.
#' @param n_digits Number of digits to print for OR or exponentiated
#'   coefficients (Default:3).
extract_or_ci_ord <- function(model, n_digits = 3) {
  s_str <- paste0("%.", n_digits, "f")
  # Number of threshold parameters, not to include in results:
  n_theta <- length(model$alpha)
  summ <- summary(model)
  df_coef <- as.data.frame(summ$coefficients)[-(1:n_theta), ]
  conf_int <- confint(model) # Naturally excludes threshold parameters
  or_ci <- cbind(exp(cbind(OR = df_coef$Estimate, conf_int)))
  tb <- data.frame(
    OR = unlist(lapply(1:nrow(or_ci), function(i) {
      sprintf(paste0(s_str, " (", s_str, " - ", s_str, ")"),
              or_ci[i, 1], or_ci[i, 2], or_ci[i, 3])
    })),
    `p value` = ifelse(df_coef$`Pr(>|z|)` < 0.001,
                       "<0.001", sprintf("%.3f", df_coef$`Pr(>|z|)`)),
    check.names = FALSE
  )
  rownames(tb) <- rownames(df_coef)
  tb
}
#' @title Internal function: Compute scoring table for ordinal outcomes based on
#'   training dataset
#' @description Compute scoring table based on training dataset
#' @inheritParams compute_score_table
#' @inheritParams AutoScore_parsimony_Ordinal
#' @param train_set_2 Processed training set after variable transformation
#' @return A scoring table
#' @importFrom ordinal clm
#' @importFrom stats as.formula na.omit
compute_score_table_ord <- function(train_set_2, max_score, variable_list, link) {
  link <- check_link(link = link)
  #AutoScore Module 3 : Score weighting
  # First-step ordinal regression
  model <- ordinal::clm(as.formula("label ~ ."), link = link, data = train_set_2)
  if (anyNA(model$alpha)) {
    stop(" Error: Ordinal regression produced invalid threshold. Check data.")
  }
  coef_vec <- model$beta
  if (anyNA(coef_vec)) {
    warning(" WARNING: Ordinal regression output contains NULL, Replace NULL with 1")
    coef_vec[which(is.na(coef_vec))] <- 1
  }
  train_set_2 <- change_reference(df = train_set_2, coef_vec = coef_vec)

  # Second-step ordinal regression
  model <- ordinal::clm(as.formula("label ~ ."), link = link, data = train_set_2)
  coef_vec <- model$beta
  if (anyNA(coef_vec)) {
    warning(" WARNING: Ordinal regression output contains NA, Replace NA with 1")
    coef_vec[which(is.na(coef_vec))] <- 1
  }

  # rounding for final scoring table "score_table"
  score_table <- add_baseline(train_set_2, round(coef_vec / min(coef_vec)))

  if (!is.null(max_score)) {
    # normalization according to "max_score" and regenerate score_table
    total_max <- max_score
    total <- 0
    for (i in 1:length(variable_list)) {
      total <- total + max(score_table[grepl(variable_list[i], names(score_table))])
    }
    score_table <- round(score_table / (total / total_max))
  }
  return(score_table)
}
#' Internal function: Compute mAUC for ordinal predictions
#' @param y An ordered factor representing the ordinal outcome, with length n
#'   and J categories.
#' @param fx Either (i) a numeric vector of predictor (e.g., predicted scores)
#'   of length n or (ii) a numeric matrix of predicted cumulative probabilities
#'   with n rows and (J-1) columns.
#' @return The mean AUC of J-1 cumulative AUCs (i.e., when evaluating
#'   the prediction of Y<=j, j=1,...,J-1).
#' @import pROC
compute_mauc_ord <- function(y, fx) {
  y <- as.numeric(y)
  J <- length(unique(y))
  if (is.null(dim(fx))) {# fx is a vector of linear predictor
    auc_df <- do.call("rbind", lapply(1:(J - 1), function(j) {
      model_roc <- pROC::roc(response = y <= j, predictor = fx, quiet = TRUE)
      auc_ci_vec <- pROC::ci.auc(model_roc)
      data.frame(j = j, auc = auc_ci_vec[2],
                 se = (auc_ci_vec[3] - auc_ci_vec[1]) / (1.96 * 2),
                 auc_lower = auc_ci_vec[1], auc_upper = auc_ci_vec[3])
    }))
  } else {# fx is a matrix with J-1 columns
    fx <- as.matrix(fx)
    if (nrow(fx) != length(y) | ncol(fx) != J - 1) {
      stop(simpleError("Wrong dimention for fx."))
    }
    auc_df <- do.call("rbind", lapply(1:(J - 1), function(j) {
      model_roc <- pROC::roc(response = y <= j, predictor = fx[, j], quiet = TRUE)
      auc_ci_vec <- pROC::ci.auc(model_roc)
      data.frame(j = j, auc = auc_ci_vec[2],
                 se = (auc_ci_vec[3] - auc_ci_vec[1]) / (1.96 * 2),
                 auc_lower = auc_ci_vec[1], auc_upper = auc_ci_vec[3])
    }))
  }
  mean(auc_df$auc)
}
#' @title Internal function: Compute mean AUC for ordinal outcomes based on
#'   validation set for plotting parsimony
#' @description  Compute mean AUC based on validation set for plotting parsimony
#' @inheritParams compute_auc_val
#' @inheritParams AutoScore_parsimony_Ordinal
#' @return A list of mAUC for parsimony plot
compute_auc_val_ord <- function(train_set_1, validation_set_1, variable_list, link,
                                categorize, quantiles, max_cluster, max_score) {
  # AutoScore-Ordinal Module 2 : cut numeric and transfer categories
  cut_vec <- get_cut_vec(df = train_set_1, categorize = categorize,
                         quantiles = quantiles, max_cluster = max_cluster)
  train_set_2 <- transform_df_fixed(df = train_set_1, cut_vec = cut_vec)
  validation_set_2 <- transform_df_fixed(df = validation_set_1, cut_vec = cut_vec)
  if (sum(is.na(validation_set_2)) > 0) {
    warning("NA in the validation_set_2: ", sum(is.na(validation_set_2)))
  }
  if (sum(is.na(train_set_2)) > 0) {
    warning("NA in the train_set_2: ", sum(is.na(train_set_2)))
  }

  # AutoScore-Ordinal Module 3 : Variable Weighting
  score_table <- compute_score_table_ord(train_set_2 = train_set_2,
                                         variable_list = variable_list,
                                         link = link,
                                         max_score = max_score)

  # Using "assign_score" to generate score based on new dataset and Scoring
  # table "score_table"
  validation_set_3 <- assign_score(df = validation_set_2,
                                   score_table = score_table)
  if (sum(is.na(validation_set_3)) > 0) {
    warning("NA in the validation_set_3: ", sum(is.na(validation_set_3)))
  }
  validation_set_3$total_score <- rowSums(subset(
    validation_set_3,
    select = setdiff(names(validation_set_3), "label")
  ))

  compute_mauc_ord(y = validation_set_3$label, fx = validation_set_3$total_score)
}
#' Internal function: Compute risk scores for ordinal data given variables
#' selected, cut-off values and scoring table
#' @inheritParams AutoScore_fine_tuning_Ordinal
#' @inheritParams AutoScore_testing_Ordinal
#' @param data A processed \code{data.frame} that contains data for validation
#'   or testing purpose. This \code{data.frame} must have variable \code{label}
#'   and should have same format as \code{train_set} (same variable names and
#'   outcomes)
compute_final_score_ord <- function(data, final_variables, cut_vec, scoring_table) {
  data_1 <- data[, c(final_variables, "label")]
  data_2 <- transform_df_fixed(df = data_1, cut_vec = cut_vec)
  data_3 <- assign_score(df = data_2, score_table = scoring_table)
  d3 <- data_3[, final_variables]
  if (length(final_variables) == 1) d3 <- matrix(d3, ncol = 1)
  data_3$total_score <- as.numeric(apply(d3, 1, sum))
  if (anyNA(data_3$total_score)) {
    warning(simpleWarning("NA in risk score. Check analyses."))
    data_3$total_score[which(is.na(data_3$total_score))] <- 0
  }
  data_3$total_score
}
#' Internal function: Evaluate model performance on ordinal data
#' @inheritParams print_performance_ordinal
#' @inheritParams compute_final_score_ord
#' @inheritParams AutoScore_weighting_Ordinal
#' @param report_cindex If generalized c-index should be reported alongside
#'   mAUC (Default:FALSE).
#' @import survival
#' @importFrom Hmisc rcorrcens
#' @importFrom coxed bca
#' @import ggplot2
#' @return Returns a list of the mAUC (mauc) and generalized c-index (cindex, if
#'   requested for) and their 95% bootstrap CIs (mauc_ci and cindex_ci).
evaluate_model_ord <- function(label, score, n_boot, report_cindex = TRUE) {
  model_auc <- compute_mauc_ord(y = label, fx = score)
  # Compute bootstrap CI:
  score_list <- lapply(1:n_boot, function(iter) {
    rows <- sample(x = 1:length(score), size = length(score), replace = TRUE)
    data.frame(label = label[rows], score = score[rows])
  })
  model_auc_boot <- unlist(lapply(score_list, function(score_boot) {
    compute_mauc_ord(y = score_boot$label, fx = score_boot$score)
  }))
  model_auc_ci <- coxed::bca(model_auc_boot, conf.level = 0.95)
  if (report_cindex) {
    model_cindex <- Hmisc::rcorrcens(
      Surv(label, status) ~ pred_score,
      data = data.frame(pred_score = score, label = as.numeric(label),
                        status = 1)
    )[, "C"]
    model_cindex_boot <- unlist(lapply(score_list, function(score_boot) {
      Hmisc::rcorrcens(
        Surv(label, status) ~ pred_score,
        data = data.frame(pred_score = score_boot$score,
                          label = as.numeric(score_boot$label),
                          status = 1)
      )[, "C"]
    }))
    model_cindex_ci <- coxed::bca(model_cindex_boot, conf.level = 0.95)
  } else {
    model_cindex <- NULL
    model_cindex_ci <- NULL
  }
  list(mauc = model_auc, mauc_ci = model_auc_ci,
       cindex = model_cindex, cindex_ci = model_cindex_ci)
}
#' Internal function: Inverse logit link
#' @param x A numeric vector.
inv_logit <- function(x) {
  exp(x) / (1 + exp(x))
}
#' Internal function: Inverse cloglog link
#' @param x A numeric vector.
inv_cloglog <- function(x) {
  1 - exp(-exp(x))
}
#' Internal function: Inverse probit link
#' @param x A numeric vector.
#' @importFrom stats pnorm
inv_probit <- function(x) {
  pnorm(x)
}
#' Internal function: generate probability matrix for ordinal outcomes given
#' thresholds, linear predictor and link function
#' @inheritParams AutoScore_parsimony_Ordinal
#' @param theta numeric vector of thresholds
#' @param z numeric vector of linear predictor
estimate_p_mat <- function(theta, z, link) {
  n <- length(z)
  inv_link <- get(paste0("inv_", link))
  cump_mat <- inv_link(matrix(rep(theta, n), nrow = n, byrow = TRUE) -
                         matrix(rep(z, length(theta)), nrow = n, byrow = FALSE))
  cump_mat <- cbind(0, cump_mat, 1)
  t(apply(cump_mat, 1, diff))
}
#' Internal function: Group scores based on given score breaks, and use friendly
#' names for first and last intervals.
#' @inheritParams conversion_table_ordinal
#' @param score numeric vector of scores.
#' @importFrom car recode
group_score <- function(score, max_score, score_breaks) {
  score_breaks <- sort(score_breaks)
  len <- length(score_breaks)
  level_min0 <- paste0("(-Inf,", min(score_breaks), "]")
  level_min <- paste0("[0,", min(score_breaks), "]")
  level_max0 <- paste0("(", max(score_breaks), ", Inf]")
  level_max <- paste0("(", max(score_breaks), ",", max_score, "]")
  all_scores <- c(
    level_min,
    paste0("(", score_breaks[1:(len - 1)], ",", score_breaks[-1], "]"),
    level_max
  )
  score_group <- cut(score, breaks = c(-Inf, score_breaks, Inf), right = TRUE)
  factor(car::recode(
    var = score_group,
    recodes = paste0("'", level_min0, "'='", level_min, "'; ",
                     "'", level_max0, "'='", level_max, "'")
  ), levels = all_scores)
}
#' Internal function: Based on given labels and scores, compute average
#' predicted risks in given score intervals.
#' @inheritParams conversion_table_ordinal
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom ordinal clm
#' @importFrom dplyr mutate group_by summarise rename
#' @importFrom rlang .data
compute_prob_predicted <- function(pred_score, link = "logit", max_score = 100,
                                   score_breaks = seq(from = 5, to = 70, by = 5)) {
  n_labels <- length(unique(pred_score$Label))
  link <- check_link(link = link)
  # Compute theoretical predicted risk
  clm_model <- ordinal::clm(ordered(Label) ~ pred_score, link = link,
                            data = pred_score)
  # Evaluate classification performance:
  score_table <- 0:max_score
  lookup <- data.frame(score = score_table,
                       estimate_p_mat(theta = clm_model$alpha,
                                      z = clm_model$beta * score_table,
                                      link = link))
  names(lookup)[-1] <- 1:n_labels
  lookup_long <- tidyr::pivot_longer(data = lookup, cols = -.data$score,
                                     names_to = "Label", values_to = "p_label")
  names(lookup)[-1] <- paste("Predicted risk, category",
                             1:length(unique(pred_score$Label)))
  # Group predicted risk accordingly
  lookup_long %>%
    mutate(score = group_score(score = .data$score, max_score = max_score,
                               score_breaks = score_breaks)) %>%
    group_by(.data$Label, .data$score) %>%
    summarise(p_label = mean(.data$p_label)) %>%
    rename(`Outcome category` = .data$Label) %>%
    mutate(`Outcome category` = factor(.data$`Outcome category`))
}
#' Internal function: Based on given labels and scores, compute proportion of
#' subjects observed in each outcome category in given score intervals.
#' @inheritParams conversion_table_ordinal
#' @importFrom dplyr mutate group_by summarise rename right_join select n
#' @importFrom rlang .data
#' @importFrom magrittr %>%
compute_prob_observed <- function(pred_score, link = "logit", max_score = 100,
                                  score_breaks = seq(from = 5, to = 70, by = 5)) {
  n_labels <- length(unique(pred_score$Label))
  link <- check_link(link = link)
  obs_risk <- pred_score %>%
    mutate(score = group_score(score = .data$pred_score, max_score = max_score,
                               score_breaks = score_breaks)) %>%
    group_by(.data$Label, .data$score) %>%
    summarise(n_label = n()) %>%
    group_by(.data$score) %>%
    mutate(n = sum(.data$n_label), p_label = .data$n_label / .data$n)
  # Fill up any missing with 0
  all_scores <- levels(obs_risk$score)
  obs_risk_full <- data.frame(
    Label = ordered(rep(1:length(unique(pred_score$Label)), length(all_scores))),
    score = factor(rep(all_scores, each = length(unique(pred_score$Label))),
                   levels = all_scores)
  )
  obs_risk %>% right_join(obs_risk_full) %>%
    mutate(p_label = ifelse(is.na(.data$p_label), 0, .data$p_label)) %>%
    select(-.data$n_label) %>%
    rename(`Outcome category` = .data$Label) %>%
    mutate(`Outcome category` = factor(.data$`Outcome category`, ordered = FALSE))
}


# Data --------------------------------------------------------------------

#' Simulated ED data with ordinal outcome
#'
#' @description Simulated data for 20,000 inpatient visits with demographic
#'   information, healthcare resource utilisation and associated laboratory
#'   tests and vital signs measured in the emergency department (ED). Data were
#'   simulated based on the dataset analysed in the AutoScore-Ordinal paper, and
#'   only includes a subset of variables (with masked variable names) for the
#'   purpose of demonstrating the AutoScore framework for ordinal outcomes.
#' @inherit AutoScore_parsimony_Ordinal references
"sample_data_ordinal"

#' Simulated ED data with ordinal outcome (small sample size)
#'
#' @description 5,000 observations randomly sampled from
#'   \code{\link{sample_data_ordinal}}. It is used for demonstration only in the
#'   Guidebook.
"sample_data_ordinal_small"
