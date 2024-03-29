% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AutoScore_Ordinal.R
\name{AutoScore_parsimony_Ordinal}
\alias{AutoScore_parsimony_Ordinal}
\title{AutoScore STEP(ii) for ordinal outcomes: Select the best model with
  parsimony plot (AutoScore Modules 2+3+4)}
\usage{
AutoScore_parsimony_Ordinal(
  train_set,
  validation_set,
  rank,
  link = "logit",
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
  auc_lim_max = "adaptive"
)
}
\arguments{
\item{train_set}{A processed \code{data.frame} that contains data to be analyzed, for training.}

\item{validation_set}{A processed \code{data.frame} that contains data for validation purpose.}

\item{rank}{The raking result generated from AutoScore STEP(i) for ordinal
outcomes (\code{\link{AutoScore_rank_Ordinal}}).}

\item{link}{The link function used to model ordinal outcomes. Default is
\code{"logit"} for proportional odds model. Other options are
\code{"cloglog"} (proportional hazards model) and \code{"probit"}.}

\item{max_score}{Maximum total score (Default: 100).}

\item{n_min}{Minimum number of selected variables (Default: 1).}

\item{n_max}{Maximum number of selected variables (Default: 20).}

\item{cross_validation}{If set to \code{TRUE}, cross-validation would be used for generating parsimony plot, which is
suitable for small-size data. Default to \code{FALSE}}

\item{fold}{The number of folds used in cross validation (Default: 10). Available if \code{cross_validation = TRUE}.}

\item{categorize}{Methods for categorize continuous variables. Options include "quantile" or "kmeans" (Default: "quantile").}

\item{quantiles}{Predefined quantiles to convert continuous variables to categorical ones. (Default: c(0, 0.05, 0.2, 0.8, 0.95, 1)) Available if \code{categorize = "quantile"}.}

\item{max_cluster}{The max number of cluster (Default: 5). Available if \code{categorize = "kmeans"}.}

\item{do_trace}{If set to TRUE, all results based on each fold of cross-validation would be printed out and plotted (Default: FALSE). Available if \code{cross_validation = TRUE}.}

\item{auc_lim_min}{Min y_axis limit in the parsimony plot (Default: 0.5).}

\item{auc_lim_max}{Max y_axis limit in the parsimony plot (Default: "adaptive").}
}
\value{
List of mAUC (ie, the average AUC of dichotomous classifications)
  value for different number of variables
}
\description{
AutoScore STEP(ii) for ordinal outcomes: Select the best model with
  parsimony plot (AutoScore Modules 2+3+4)
}
\details{
This is the second step of the general AutoScore workflow for
  ordinal outcomes, to generate the parsimony plot to help select a
  parsimonious model. In this step, it goes through AutoScore Module 2,3 and
  4 multiple times and to evaluate the performance under different variable
  list. The generated parsimony plot would give researcher an intuitive
  figure to choose the best models. If data size is small (eg, <5000), an
  independent validation set may not be a wise choice. Then, we suggest using
  cross-validation to maximize the utility of data. Set
  \code{cross_validation=TRUE}.
}
\examples{
\dontrun{
# see AutoScore-Ordinal Guidebook for the whole 5-step workflow
data("sample_data_ordinal") # Output is named `label`
out_split <- split_data(data = sample_data_ordinal, ratio = c(0.7, 0.1, 0.2))
train_set <- out_split$train_set
validation_set <- out_split$validation_set
ranking <- AutoScore_rank_Ordinal(train_set, ntree=100)
mAUC <- AutoScore_parsimony_Ordinal(
  train_set = train_set, validation_set = validation_set,
  rank = ranking, max_score = 100, n_min = 1, n_max = 20,
  categorize = "quantile", quantiles = c(0, 0.05, 0.2, 0.8, 0.95, 1)
)
}
}
\references{
\itemize{
 \item{Saffari SE, Ning Y, Feng X, Chakraborty B, Volovici V, Vaughan R, Ong
       ME, Liu N, AutoScore-Ordinal: An interpretable machine learning framework for
       generating scoring models for ordinal outcomes, arXiv:2202.08407}
}
}
\seealso{
\code{\link{AutoScore_rank_Ordinal}},
  \code{\link{AutoScore_weighting_Ordinal}},
  \code{\link{AutoScore_fine_tuning_Ordinal}},
  \code{\link{AutoScore_testing_Ordinal}}.
}
