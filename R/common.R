# For plotting predicted risk (binary & ordinal) -----

#' Internal function: Find column indices in design matrix that should be 1
#' @param x_inds A list of column indices corresponding to each final variable.
find_one_inds <- function(x_inds) {
  if (length(x_inds) < 2) stop(simpleError("Should have at least 2 variables"))
  for (x1 in x_inds[[1]]) {
    for (x2 in x_inds[[2]]) {
      paste(x1, x2, sep = ";")
    }
  }
  x_first2 <- unlist(lapply(x_inds[[1]], function(x1) {
    lapply(x_inds[[2]], function(x2) paste(x1, x2, sep = ";"))
  }))
  if (length(x_inds) == 2) {
    return(x_first2)
  } else {
    unlist(find_one_inds(c(list(x_first2), x_inds[-(1:2)])))
  }
}
#' Internal function: Based on \code{find_one_inds}, make a design matrix to
#' compute all scores attainable.
#' @param one_inds Output from \code{find_one_inds}.
make_design_mat <- function(one_inds) {
  one_inds_num <- do.call("rbind", lapply(one_inds, function(one_ind) {
    as.integer(unlist(strsplit(x = one_ind, split = ";")))
  }))
  x_mat <- matrix(0, nrow = nrow(one_inds_num), ncol = max(one_inds_num))
  for (i in 1:nrow(one_inds_num)) x_mat[i, one_inds_num[i, ]] <- 1
  x_mat
}
#' Internal function: Compute all scores attainable.
#' @param final_variables	A vector containing the list of selected variables.
#' @param scoring_table The final scoring table after fine-tuning.
#' @return Returns a numeric vector of all scores attainable.
find_possible_scores <- function(final_variables, scoring_table) {
  x_inds <- list()
  n_prev <- 0
  for (i in seq_along(final_variables)) {
    score_table_tmp <- scoring_table[grepl(final_variables[i], names(scoring_table))]
    len <- length(score_table_tmp)
    x_inds[[i]] <- 1:len + n_prev
    n_prev <- n_prev + len
  }
  one_inds <- find_one_inds(x_inds)
  x_mat <- make_design_mat(one_inds = one_inds)
  scores <- x_mat %*% scoring_table
  sort(unique(scores))
}
#' AutoScore function for binary and ordinal outcomes: Plot predicted risk
#' @param pred_score Output from \code{\link{AutoScore_testing}} (for binary
#'   outcomes) or \code{\link{AutoScore_testing_Ordinal}} (for ordinal
#'   outcomes).
#' @param link (For ordinal outcome only) The link function used in ordinal
#'   regression, which must be the same as the value used to build the risk
#'   score. Default is \code{"logit"} for proportional odds model.
#' @param max_score Maximum total score (Default: 100).
#' @param final_variables A vector containing the list of selected variables,
#'   selected from Step(ii) \code{\link{AutoScore_parsimony}} (for binary
#'   outcomes) or \code{\link{AutoScore_parsimony_Ordinal}} (for ordinal
#'   outcomes).
#' @param scoring_table The final scoring table after fine-tuning, generated
#'   from STEP(iv) \code{\link{AutoScore_fine_tuning}} (for binary outcomes) or
#'   \code{\link{AutoScore_fine_tuning_Ordinal}} (for ordinal outcomes).
#' @param point_size Size of points in the plot. Default is 0.5.
#' @export
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom plotly ggplotly layout subplot
#' @importFrom rlang .data
plot_predicted_risk <- function(pred_score, link = "logit", max_score = 100,
                                final_variables, scoring_table, point_size = 0.5) {
  n_class <- length(unique(pred_score$Label))
  z <- seq(from = 0, to = max_score, by = 1)
  if (n_class == 2) {
    glm_model <- glm(Label ~ pred_score, family = "binomial", data = pred_score)
    pred_risk <- predict(object = glm_model, newdata = data.frame(pred_score = z),
                         type = "response")
    pred_risk <- data.frame(outcome = pred_risk)
  } else {
    link <- check_link(link = link)
    # Compute theoretical predicted risk
    clm_model <- ordinal::clm(ordered(Label) ~ pred_score, link = link,
                              data = pred_score)
    pred_risk <- estimate_p_mat(theta = clm_model$alpha, z = clm_model$beta * z,
                                link = link)
    names(pred_risk) <- 1:ncol(pred_risk)
  }
  # Proportion of subjects with each score value:
  p_scores <- table(pred_score$pred_score) / nrow(pred_score)
  ps <- data.frame(Score = as.numeric(names(p_scores)),
                   Proportion = as.numeric(p_scores)) %>%
    ggplot(aes_string(x = "Score", y = "Proportion")) +
    coord_cartesian(xlim = c(0, max_score)) +
    geom_bar(stat = "identity") +
    labs(y = "Proportion\nof subjects") +
    theme_classic() +
    theme(panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_line(color = "grey90"),
          axis.text = element_text(size = 12))
  possible_scores <- find_possible_scores(final_variables = final_variables,
                                          scoring_table = scoring_table)
  dat_plot <- data.frame(Score = z, pred_risk, check.names = FALSE) %>%
    pivot_longer(-.data$Score, names_to = "Outcome category",
                 values_to = "Predicted risk") %>%
    mutate(add_point = .data$Score %in% possible_scores)
  p <- dat_plot %>%
    ggplot(aes_string(x = "Score", y = "`Predicted risk`", color = "`Outcome category`")) +
    coord_cartesian(ylim = c(0, 1)) +
    geom_line() +
    geom_point(data = dat_plot[dat_plot$add_point, ],
               aes_string(x = "Score", y = "`Predicted risk`"), size = point_size) +
    labs(title = "Points indicate attainable final scores", x = "Score") +
    theme_classic() +
    theme(legend.position = "bottom",
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_line(color = "grey90"),
          axis.text = element_text(size = 12))
  ps_title <- "Proportion of subjects"
  if (n_class == 2) p <- p + theme(legend.position = "none")
  plotly::subplot(
    plotly::ggplotly(p),
    plotly::ggplotly(ps), nrows = 2, heights = c(5, 2) / 7, margin = 0.08,
    shareX = FALSE, shareY = TRUE, titleX = TRUE
  )
}


# For parsimony plot (all outcomes) -----

#' Internal function: Make parsimony plot
#' @inheritParams AutoScore_parsimony
#' @param AUC A vector of AUC values (or mAUC for ordinal outcomes).
#' @param variables A vector of variable names
#' @param num A vector of indices for AUC values to plot. Default is to plot all.
#' @param ylab Title of y-axis
#' @param title Plot title
#' @import ggplot2
plot_auc <- function(AUC, variables, num = seq_along(variables),
                     auc_lim_min, auc_lim_max,
                     ylab = "Mean Area Under the Curve",
                     title = "Parsimony plot on the validation set") {
  if (auc_lim_max == "adaptive") {
    auc_lim_max <- max(AUC)
  }
  dt <- data.frame(AUC = AUC, variables = factor(variables, levels = variables),
                   num = num)
  p <- ggplot(data = dt, mapping = aes_string(x = "variables", y = "AUC")) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_cartesian(ylim = c(auc_lim_min, auc_lim_max)) +
    theme_bw() +
    labs(x = "", y = ylab, title = title) +
    theme(legend.position = "none",
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  # Add number of variables to bar (resolves previous warning regarding dt$num):
  if (nrow(dt) >= 100) {
    p + geom_text(data = dt, aes_string(label = "num"),
                  vjust = 1.5, colour = "white", angle = 90)
  } else {
    p + geom_text(data = dt, aes_string(label = "num"),
                  vjust = 1.5, colour = "white")
  }
}

# For non-plot tasks (all outcomes) -----

#' @title AutoScore Function: Automatically splitting dataset to train,
#'   validation and test set, possibly stratified by label
#' @param data The dataset to be split
#' @param ratio The ratio for dividing dataset into training, validation and
#'   testing set. (Default: c(0.7, 0.1, 0.2))
#' @param cross_validation If set to \code{TRUE}, cross-validation would be used
#'   for generating parsimony plot, which is suitable for small-size data.
#'   Default to \code{FALSE}
#' @param strat_by_label If set to \code{TRUE}, data splitting is stratified on
#'   the outcome variable. Default to \code{FALSE}
#' @return Returns a list containing training, validation and testing set
#' @examples
#' data("sample_data")
#' names(sample_data)[names(sample_data) == "Mortality_inpatient"] <- "label"
#' set.seed(4)
#' #large sample size
#' out_split <- split_data(data = sample_data, ratio = c(0.7, 0.1, 0.2))
#' #small sample size
#' out_split <- split_data(data = sample_data, ratio = c(0.7, 0, 0.3),
#'                         cross_validation = TRUE)
#' #large sample size, stratified
#' out_split <- split_data(data = sample_data, ratio = c(0.7, 0.1, 0.2),
#'                         strat_by_label = TRUE)
#' @export
split_data <- function(data, ratio, cross_validation = FALSE,
                       strat_by_label = FALSE) {
  data <- as.data.frame(data)
  ratio <- ratio / sum(ratio)
  if (!strat_by_label) {
    n <- nrow(data)
    test_ratio <- ratio[3]
    validation_ratio <- ratio[2]
    test_index <- sample((1:n), test_ratio * n)
    validate_index <- sample((1:n)[!(1:n) %in% test_index], validation_ratio * n)
    # non cross validation: default
    if (cross_validation == FALSE) {
      train_set <- data[-c(validate_index, test_index), ]
      test_set <- data[test_index, ]
      validation_set <- data[validate_index, ]
    } else {# cross validation: train = validation
      train_set <- data[-c(test_index),]
      test_set <- data[test_index,]
      validation_set <- train_set
    }
  } else {
    index_list <- lapply(levels(data$label), function(j) {
      n_j <- which(data$label == j)
      ind_test <- sample(n_j, round(ratio[3] * length(n_j)), replace = FALSE)
      ind_val <- sample(setdiff(n_j, ind_test), round(ratio[2] * length(n_j)),
                        replace = FALSE)
      list(test = ind_test, val = ind_val)
    })
    index_test <- unlist(lapply(index_list, function(l) l$test))
    index_validation <- unlist(lapply(index_list, function(l) l$val))
    # non cross validation: default
    if (cross_validation == FALSE) {
      train_set <- data[-unlist(index_list), ]
      test_set <- data[index_test, ]
      validation_set <- data[index_validation, ]
    } else {# cross validation: train = validation
      train_set <- data[-c(index_test),]
      test_set <- data[index_test,]
      validation_set <- train_set
    }
  }
  return(list(train_set = train_set, validation_set = validation_set,
              test_set = test_set))
}
#' @title AutoScore function: Descriptive Analysis
#' @description Compute descriptive table (usually Table 1 in the medical
#'   literature) for the dataset.
#' @param df data frame after checking and fulfilling the requirement of AutoScore
#' @param ... additional parameters to pass to
#'   \code{\link[tableone]{print.TableOne}} and \code{\link[knitr]{kable}}.
#' @examples
#' data("sample_data")
#' names(sample_data)[names(sample_data) == "Mortality_inpatient"] <- "label"
#' compute_descriptive_table(sample_data)
#' # Report median and IQR (instead of default mean and SD) for Age, and add a
#' # caption to printed table:
#' compute_descriptive_table(sample_data, nonnormal = "Age",
#'                           caption = "Table 1. Patient characteristics")
#' @return No return value and the result of the descriptive analysis will be printed out.
#' @export
#' @import tableone
compute_descriptive_table <- function(df, ...) {
  label_name <- "label"
  surv_labels <- c("label_time", "label_status")
  if (all(surv_labels %in% names(df))) label_name <- surv_labels[2]
  descriptive_table <- CreateTableOne(vars = names(df), strata = label_name,
                                      data = df, addOverall = TRUE)
  kableone(descriptive_table, ...)
}




#' @title Internal Function: Change Reference category after first-step logistic regression (part of AutoScore Module 3)
#' @param df A \code{data.frame} used for logistic regression
#' @param coef_vec Generated from logistic regression
#' @return Processed \code{data.frame} after changing reference category
change_reference <- function(df, coef_vec) {
  # delete label first
  df_tmp <- subset(df, select = names(df)[!names(df) %in% c("label", "label_time", "label_status")])
  names(coef_vec) <- gsub("[`]", "", names(coef_vec)) # remove the possible "`" in the names

  # one loops to go through all variable
  for (i in (1:length(df_tmp))) {
    var_name <- names(df_tmp)[i]
    var_levels <- levels(df_tmp[, i])
    var_coef_names <- paste0(var_name, var_levels)
    coef_i <- coef_vec[which(names(coef_vec) %in% var_coef_names)]
    # if min(coef_tmp)<0, the current lowest one will be used for reference
    if (min(coef_i) < 0) {
      ref <-
        var_levels[which(var_coef_names == names(coef_i)[which.min(coef_i)])]
      df_tmp[, i] <- relevel(df_tmp[, i], ref = ref)
    }
    # char_tmp <- paste("^", names(df_tmp)[i], sep = "")
    # coef_tmp <- coef_vec[grepl(char_tmp, names(coef_vec))]
    # coef_tmp <- coef_tmp[!is.na(coef_tmp)]

    # if min(coef_tmp)<0, the current lowest one will be used for reference
    # if (min(coef_tmp) < 0) {
    #   ref <- gsub(names(df_tmp)[i], "", names(coef_tmp)[which.min(coef_tmp)])
    #   df_tmp[, i] <- relevel(df_tmp[, i], ref = ref)
    # }
  }

  # add label again
  if(!is.null(df$label))  df_tmp$label <- df$label
  else{
  df_tmp$label_time <- df$label_time
  df_tmp$label_status <- df$label_status}
  return(df_tmp)
}


#' @title Internal Function: Add baselines after second-step logistic regression (part of AutoScore Module 3)
#' @param df A \code{data.frame} used for logistic regression
#' @param coef_vec Generated from logistic regression
#' @return Processed \code{vector} for generating the scoring table
add_baseline <- function(df, coef_vec) {
  names(coef_vec) <- gsub("[`]", "", names(coef_vec)) # remove the possible "`" in the names
  df <- subset(df, select = names(df)[!names(df) %in% c("label", "label_time", "label_status")])
  coef_names_all <- unlist(lapply(names(df), function(var_name) {
    paste0(var_name, levels(df[, var_name]))
  }))
  coef_vec_all <- numeric(length(coef_names_all))
  names(coef_vec_all) <- coef_names_all
  # Remove items in coef_vec that are not meant to be in coef_vec_all
  # (i.e., the intercept)
  coef_vec_core <-
    coef_vec[which(names(coef_vec) %in% names(coef_vec_all))]
  i_coef <-
    match(x = names(coef_vec_core),
          table = names(coef_vec_all))
  coef_vec_all[i_coef] <- coef_vec_core
  coef_vec_all
}


#' @title Internal Function: Automatically assign scores to each subjects given new data set and scoring table (Used for intermediate and final evaluation)
#' @param df A \code{data.frame} used for testing, where variables keep before categorization
#' @param score_table A \code{vector} containing the scoring table
#' @return Processed \code{data.frame} with assigned scores for each variables
assign_score <- function(df, score_table) {
  for (i in setdiff(names(df), c("label", "label_time", "label_status"))) {
    score_table_tmp <-
      score_table[grepl(i, names(score_table))]
    df[, i] <- as.character(df[, i])
    for (j in 1:length(names(score_table_tmp))) {
      df[, i][df[, i] %in% gsub(i, "", names(score_table_tmp)[j])] <-
        score_table_tmp[j]
    }

    df[, i] <- as.numeric(df[, i])
  }

  return(df)
}



getmode <- function(vect) {
  uniqvect <- unique(vect)
  freq <- tabulate(match(vect, uniqvect))
  mode_tmp <- uniqvect[which.max(freq)]
  if (is.na(mode_tmp)){
    mode_tmp <- uniqvect[order(freq, decreasing=T)[2]]
  }
  return(mode_tmp)
}


#' @title AutoScore function for datasets with binary outcomes: Check whether the input dataset fulfill the requirement of the AutoScore
#' @param data The data to be checked
#' @examples
#' data("sample_data")
#' names(sample_data)[names(sample_data) == "Mortality_inpatient"] <- "label"
#' check_data(sample_data)
#' @return No return value, the result of the checking will be printed out.
#' @export
check_data <- function(data) {
  #1. check label and binary
  if (is.null(data$label))
    stop(
      "ERROR: for this dataset: These is no dependent variable 'label' to indicate the outcome. Please add one first\n"
    )
  if (length(levels(factor(data$label))) != 2)
    warning("Please keep outcome label variable binary\n")
  check_predictor(data)}

#' Internal function: Check predictors
#' @param data_predictor Predictors to be checked
#' @return No return value, the result of the checking will be printed out.
check_predictor <- function(data_predictor) {
  data <- data_predictor
  data <- as.data.frame(data)
  #2. check each variable
  non_num_fac <- c()
  fac_large <- c()
  special_case <- c()

  for (i in names(data)) {
    if ((class(data[[i]]) != "factor") &&
        (class(data[[i]]) != "numeric")&&
        (class(data[[i]]) != "integer")&&
        (class(data[[i]]) != "logical"))
      non_num_fac <- c(non_num_fac, i)
    if ((length(levels(data[[i]])) > 10) &&
        (is.factor(data[[i]])))
      fac_large <- c(fac_large, i)

    if (grepl(",", i))
      warning(simpleWarning(paste0(
        "WARNING: the dataset has variable names '",
        i,
        "' with character ','. Please change it. Consider using '_' to replace\n"
      )))

    if (grepl(")", i))
      warning(simpleWarning(paste0(
        "WARNING: the dataset has variable names '",
        i,
        "' with character ')'. Please change it. Consider using '_' to replace\n"
      )))

    if (grepl("]", i))
      warning(simpleWarning(paste0(
        "WARNING: the dataset has variable names '",
        i,
        "' with character ']'. Please change it. Consider using '_' to replace\n"
      )))

    if (is.factor(data[[i]])) {
      if (sum(grepl(",", levels(data[[i]]))) > 0)
        warning(simpleWarning(paste0(
          "WARNING: the dataset has categorical variable '",
          i,
          "', where their levels contain ','. Please use 'levels(*your_variable*)' to change the name of the levels before using the AutoScore. Consider replacing ',' with '_'. Thanks! \n "
        )))
    }


    if (sum(grepl(i, names(data))) > 1) {
      a <- names(data)[grepl(i, names(data))]
      a <- a[a != i]
      warning(simpleWarning(paste0(
        "WARNING: the dataset has variable name '",
        i,
        "', which is entirely included by other variable names:\n",
        paste(paste0("'", a, "'"), collapse = "  "),
        "\nPlease use 'names(*your_df*)' to change the name of variable '",
        i,
        "' before using the AutoScore. Consider adding '_1', '_2',..., '_x, or other similar stuff at end of that name, such as '",
        paste0(i, "_1") ,
        "', to make them totally different and not contain each other. Thanks!\n "
      )))

    }


  }

  if (!is.null(non_num_fac))
    warning(simpleWarning(paste(
      "\nWARNING: the dataset has variable of character and user should transform them to factor or numeric before using AutoScore:(consider using 'df$xxx  <- as.factor(df$xxx))' or 'df$xxx  <- as.numeric(df$xxx))'\n",
      non_num_fac
    )))
  if (!is.null(fac_large))
    warning(simpleWarning(paste(
      "\nWARNING: The number of categories for some variables is too many :larger than: ",
      fac_large
    )))

  #3. check missing values
  missing_rate <- colSums(is.na(data))
  if (sum(missing_rate) > 0) {
    warning(simpleWarning(
      "\n WARNING: Your dataset contains NA. AutoScore may also automatically handle missing values by treating them as a new category named 'Unknown'.
      if you believe the missingness in your dataset is informative and prevalent enough that you prefer to preserve them as NA rather than removing or doing imputation.
      Please evaluate the missingness in your dataset. The variables with missing values are shown below:"
    ))
    print(missing_rate[missing_rate != 0])
  } else message("\nYour dataset doesn't have any missing values.\n")


  if (is.null(non_num_fac) & is.null(fac_large)) message("Data type check passed.")
  #cat("Please fixed the problem of your dataset before AutoScore if you see any Warnings below.\n")
}

#' Internal function: induce informative missing in a single variable
#' @param x Variable to induce missing in.
#' @inheritParams induce_informative_missing
induce_median_missing <- function(x, prop_missing) {
  if (anyNA(x)) stop(simpleError("x already has missing."))
  x_mdn <- median(x)
  sqr_diff <- (x - x_mdn) ^ 2
  # Probability of missing is based on square difference from median: smaller
  # difference means higher probability
  # Handle 0 difference:
  p <- 1 / sqr_diff
  p <- ifelse(sqr_diff == 0, min(p), p)
  i_na <- sample(x = 1:length(x), size = round(prop_missing * length(x)),
                 replace = FALSE, prob = p)
  x[i_na] <- NA
  x
}

# For missing-realted issues -----

#' Internal function: induce informative missing to sample data in the package
#' to demonstrate how AutoScore handles missing as a separate category
#' @details Assume subjects with normal values (i.e., values close to the
#'   median) are more likely to not have measurements.
#' @param df A data.frame of sample data.
#' @param vars_to_induce Names of variables to induce informative missing in.
#'   Default is c("Lab_A", "Vital_A").
#' @param prop_missing Proportion of missing to induce for each
#'   \code{vars_to_induce}. Can be a single value for a common proportion for
#'   all variables (default is 0.4), or a vector with same length as
#'   \code{vars_to_induce}.
#' @return Returns \code{df} with selected columns modified to have missing.
induce_informative_missing <- function(df, vars_to_induce = c("Lab_A", "Vital_A"),
                                       prop_missing = 0.4) {
  # Only work on variable names that can be found in df:
  vars_not_in <- vars_to_induce[which(!vars_to_induce %in% names(df))]
  if (length(vars_not_in) > 0) {
    warning(simpleWarning(paste(
      "The following variables are not in df and are ignored:", toString(vars_not_in)
    )))
  }
  vars_to_induce <- setdiff(vars_to_induce, vars_not_in)
  prop_missing <- unlist(as.numeric(prop_missing))
  if (length(prop_missing) == 1) {
    prop_missing <- rep(prop_missing, length = length(vars_to_induce))
  }
  if (length(prop_missing) != length(vars_to_induce)) {
    stop(simpleError(paste(
      "prop_missing should be either a single value or a vector of length",
      length(vars_to_induce)
    )))
  }
  names(prop_missing) <- vars_to_induce
  # Induce missing to each variable:
  for (var in vars_to_induce) {
    df[, var] <- induce_median_missing(x = df[, var], prop_missing = prop_missing[var])
  }
  df
}

AutoScore_impute <- function(train_set, validation_set = NULL){
  n_train <- nrow(train_set)
  df <- rbind(train_set, validation_set)
  for (i in 1:ncol(df)){
    # print(names(df)[i])
    if (names(df)[i] == "label" & sum(is.na(df[, i])) > 0){
      stop("There are missing values in the outcome: label! Please fix it and try again")
    }

    if (names(df)[i] == "label_time" & sum(is.na(df[, i])) > 0){
      stop("There are missing values in the outcome: label_time!Please fix it and try again")
    }

    if (names(df)[i] == "label_status" & sum(is.na(df[, i])) > 0){
      stop("There are missing values in the outcome:lable_status!Please fix it and try again")
    }

    var = df[1:n_train, i]
    if (is.factor(df[, i]) & sum(is.na(df[, i])) > 0){
      df[is.na(df[, i]), i] <- getmode(var)
    } else if (is.numeric(var) & sum(is.na(var)) > 0) {
      df[is.na(df[, i]), i] <- median(var, na.rm=T)
    }
  }

  if (is.null(validation_set)){
    return(df)
  } else {
    train_set <- df[1:n_train, ]
    validation_set <- df[(n_train+1):nrow(df), ]
    return(list("train" = train_set, "validation" = validation_set))
  }
}



#' @title Internal function: Calculate cut_vec from the training set (AutoScore Module 2)
#' @param df training set to be used for calculate the cut vector
#' @param categorize  Methods for categorize continuous variables. Options include "quantile" or "kmeans" (Default: "quantile").
#' @param quantiles Predefined quantiles to convert continuous variables to categorical ones. (Default: c(0, 0.05, 0.2, 0.8, 0.95, 1)) Available if \code{categorize = "quantile"}.
#' @param max_cluster The max number of cluster (Default: 5). Available if \code{categorize = "kmeans"}.
#' @return cut_vec for \code{transform_df_fixed}
get_cut_vec <-
  function(df,
           quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1),
           #by default
           max_cluster = 5,
           categorize = "quantile") {
    # Generate cut_vec for downstream usage
    cut_vec <- list()

    for (i in setdiff(names(df), c("label", "label_time", "label_status"))) {
      # for factor variable
      if (is.factor(df[, i])) {
        if (length(levels(df[, i])) < 10)
          #(next)() else stop("ERROR: The number of categories should be less than 10")
          (next)()
        else
          warning("WARNING: The number of categories should be less than 10",
                  i)
      }

      ## mode 1 - quantiles
      if (categorize == "quantile") {
        # options(scipen = 20)
        #print("in quantile")
        cut_off_tmp <- quantile(df[, i], quantiles, na.rm=T)
        cut_off_tmp <- unique(cut_off_tmp)
        cut_off <- signif(cut_off_tmp, 3)  # remain 3 digits
        #print(cut_off)

        ## mode 2 k-means clustering
      } else if (categorize == "k_means") {
        #print("using k-means")
        clusters <- kmeans(na.omit(df[, i]), max_cluster)
        cut_off_tmp <- c()
        for (j in unique(clusters$cluster)) {
          #print(min(df[,i][clusters$cluster==j]))
          #print(length(df[,i][clusters$cluster==j]))
          cut_off_tmp <-
            append(cut_off_tmp, min(df[, i][clusters$cluster == j], na.rm=T))
          #print(cut_off_tmp)
        }
        cut_off_tmp <- append(cut_off_tmp, max(df[, i], na.rm=T))
        cut_off_tmp <- sort(cut_off_tmp)
        #print(names(df)[i])
        #assert (length(cut_off_tmp) == 6)
        cut_off_tmp <- unique(cut_off_tmp)
        cut_off <- signif(cut_off_tmp, 3)
        cut_off <- unique(cut_off)
        #print (cut_off)

      } else {
        stop('ERROR: please specify correct method for categorizing:  "quantile" or "k_means".')
      }

      l <- list(cut_off)
      names(l)[1] <- i
      cut_vec <- append(cut_vec, l)
      #print("****************************cut_vec*************************")
      #print(cut_vec)
    }
    ## delete min and max for each cut-off (min and max will be captured in the new dataset)
    if (length(cut_vec) != 0) { ## in case all the variables are categorical
      for (i in 1:length(cut_vec)) {
        if (length(cut_vec[[i]]) <= 2)
          cut_vec[[i]] <- c("let_binary")
        else
          cut_vec[[i]] <- cut_vec[[i]][2:(length(cut_vec[[i]]) - 1)]
      }
    }
    return(cut_vec)

  }

#' @title Internal function: Categorizing continuous variables based on cut_vec (AutoScore Module 2)
#' @param df dataset(training, validation or testing) to be processed
#' @param cut_vec fixed cut vector
#' @return  Processed \code{data.frame} after categorizing based on fixed cut_vec
#' @export
transform_df_fixed <- function(df, cut_vec) {
  j <- 1

  # for loop going through all variables
  for (i in setdiff(names(df), c("label", "label_time", "label_status"))) {
    if (is.factor(df[, i])) {
      df[, i] <- factor(car::recode(var = df[, i], recodes = "NA = 'Unknown'"))
      if (length(levels(df[, i])) < 10)
        (next)()
      else
        stop("ERROR: The number of categories should be less than 9")
    }

    ## make conresponding cutvec for validation_set: cut_vec_new
    #df<-validation_set_1
    #df<-train_set_1
    vec <- df[, i]
    cut_vec_new <- cut_vec[[j]]

    if (cut_vec_new[1] == "let_binary") {
      vec[vec != getmode(vec)] <- paste0("not_", getmode(vec))
      vec <- as.factor(vec)
      df[, i] <- vec
    } else{
      if (min(vec, na.rm=T) < cut_vec[[j]][1])
        cut_vec_new <- c(floor(min(df[, i], na.rm=T)) - 100, cut_vec_new)
      if (max(vec, na.rm=T) >= cut_vec[[j]][length(cut_vec[[j]])])
        cut_vec_new <- c(cut_vec_new, ceiling(max(df[, i], na.rm=T) + 100))

      cut_vec_new_tmp <- signif(cut_vec_new, 3)
      cut_vec_new_tmp <- unique(cut_vec_new_tmp)  ###revised update##
      df_i_tmp <-  cut(
        df[, i],
        breaks = cut_vec_new_tmp,
        right = F,
        include.lowest = F,
        dig.lab = 3
      )
      # xmin<-as.character(min(cut_vec_new_tmp)) xmax<-as.character(max(cut_vec_new_tmp))

      ## delete min and max for the Interval after discretion: validation_set
      if (min(vec, na.rm=T) < cut_vec[[j]][1])
        levels(df_i_tmp)[1] <- gsub(".*,", "(,", levels(df_i_tmp)[1])
      if (max(vec, na.rm=T) >= cut_vec[[j]][length(cut_vec[[j]])])
        levels(df_i_tmp)[length(levels(df_i_tmp))] <-
        gsub(",.*", ",)", levels(df_i_tmp)[length(levels(df_i_tmp))])

      df[, i] <- as.factor(ifelse(is.na(df[, i]), "*Unknown", as.character(df_i_tmp)))

    }

    j <- j + 1
  }
  return(df)
}



#' @title AutoScore Function: Print scoring tables for visualization
#' @param scoring_table Raw scoring table generated by AutoScore step(iv) \code{\link{AutoScore_fine_tuning}}
#' @param final_variable Final included variables
#' @return Data frame of formatted scoring table
#' @seealso \code{\link{AutoScore_fine_tuning}}, \code{\link{AutoScore_weighting}}
#' @export
#' @importFrom knitr kable
print_scoring_table <- function(scoring_table, final_variable) {
  #library(knitr)
  table_tmp <- data.frame()
  var_name <- names(scoring_table)
  var_name_tmp<-gsub("\\(.*","",var_name)
  var_name_tmp<-gsub("\\[.*","",var_name_tmp)
  var_name_tmp<-gsub("\\*.*","",var_name_tmp) # for "Unknown" category
  for (i in 1:length(final_variable)) {
    var_tmp <- final_variable[i]
    # num <- grepl(var_tmp, var_name)
    # rank_indicator[which(rank_indicator=="")]<-max(as.numeric(rank_indicator[-which(rank_indicator=="")]))+1
    {
      num <- grep(var_tmp, var_name_tmp)
      if (sum(grepl(",", var_name[num])) == 0) {
        # for categorical variable
        table_1 <-
          data.frame(name = var_name[num], value = unname(scoring_table[num]))
        table_1$rank_indicator <- c(seq(1:nrow(table_1)))
        interval <-
          c(gsub(
            pattern = var_tmp,
            replacement = "",
            table_1$name
          ))
        table_1$interval <- interval
        table_2 <- table_1[order(table_1$interval),]
        table_2$variable <- c(var_tmp, rep("", (nrow(table_2) - 1)))
        table_3 <- rbind(table_2, rep("", ncol(table_2)))
        table_tmp <- rbind(table_tmp, table_3)
      }
      else
      {
        num <- grep(paste("^",var_tmp,"$", sep=""), var_name_tmp)
        table_1 <-
          data.frame(name = var_name[num], value = unname(scoring_table[num]))
        rank_indicator <- gsub(".*,", "", table_1$name)
        rank_indicator <-
          gsub(")", "", rank_indicator)
        rank_indicator <-
          gsub(".*\\*", "", rank_indicator) # for "Unknown" category
        rank_indicator[which(rank_indicator == "")] <-
          max(as.numeric(rank_indicator[-which(rank_indicator %in% c("", "Unknown"))])) + 1

        # rank_indicator <- as.numeric(rank_indicator)
        table_1$rank_indicator <- rank_indicator
        if ("Unknown" %in% rank_indicator){
          table_unknown <- table_1[rank_indicator == "Unknown", ]
          table_unknown$interval <- "Unknown"
          table_1 <- table_1[rank_indicator != "Unknown", ]
          table_1$rank_indicator <- as.numeric(table_1$rank_indicator)
        } else {
          table_unknown <- data.frame()
          table_1$rank_indicator <- as.numeric(table_1$rank_indicator)
        }
        table_2 <- table_1[order(table_1$rank_indicator),]

        {
          if (length(rank_indicator[rank_indicator != "Unknown"]) == 2) {
            # table_1$rank_indicator <- rank_indicator
            # table_2 <- table_1[order(table_1$rank_indicator),]
            interval <- c(paste0("<", table_2$rank_indicator[1]))
            interval <-
              c(interval, paste0(">=", table_2$rank_indicator[length(rank_indicator) -
                                                                1]))
            # table_2$interval <- interval
            # table_2 <- rbind(table_2, table_unknown)
            # table_2$variable <- c(var_tmp, rep("", (nrow(
            #   table_2
            # ) - 1)))
            # table_3 <- rbind(table_2, rep("", ncol(table_2)))
            # table_tmp <- rbind(table_tmp, table_3)
          } else{
            interval <- c(paste0("<", table_2$rank_indicator[1]))
            for (j in 1:(length(table_2$rank_indicator) - 2)) {
              interval <-
                c(
                  interval,
                  paste0(
                    "[",
                    table_2$rank_indicator[j],
                    ",",
                    table_2$rank_indicator[j + 1],
                    ")"
                  )
                )
            }
            interval <-
              c(interval, paste0(">=", table_2$rank_indicator[length(rank_indicator) -
                                                                1]))
          }

          table_2$interval <- interval
          table_2 <- rbind(table_2, table_unknown)
          table_2$variable <- c(var_tmp, rep("", (nrow(
            table_2
          ) - 1)))
          table_3 <- rbind(table_2, rep("", ncol(table_2)))
          table_tmp <- rbind(table_tmp, table_3)
        }
      }
    }
  }
  table_tmp <- table_tmp[1:(nrow(table_tmp) - 1),]
  table_final <-
    data.frame(
      variable = table_tmp$variable,
      interval = table_tmp$interval,
      point = table_tmp$value
    )
  table_kable_format <-
    kable(table_final,
          align = "llc",
          caption = "AutoScore-created scoring model",
          format = "rst")
  print(table_kable_format)
  invisible(table_final)
}


#' @title Internal Function: Print plotted variable importance
#' @param ranking vector output generated by functions: AutoScore_rank, AutoScore_rank_Survival or AutoScore_rank_Ordinal
#' @seealso \code{\link{AutoScore_rank}}, \code{\link{AutoScore_rank_Survival}}, \code{\link{AutoScore_rank_Ordinal}}
#' @export
#' @import ggplot2
plot_importance <- function(ranking){

  df = data.frame(Imp = ranking, Index = factor(names(ranking), levels = rev(names(ranking))))
  p <- ggplot(data = df, mapping = aes_string(y = "Index", x = "Imp")) +
    geom_bar(stat = "identity", fill = "deepskyblue") +
    scale_y_discrete(expand = expansion(mult = 0, add = 1)) +
    labs(x = "Importance", y = "", title = "Importance Ranking") +
    theme(plot.title = element_text(hjust = 0.5, vjust = 0.3),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text = element_text(size = 13.5),
          axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0))
  if(nrow(df) > 25){
    p <- p + theme(axis.text.y = element_text(size = 13.5 - 1.5 * (floor(nrow(df)/21))-1))
  }

  print(p)
}

