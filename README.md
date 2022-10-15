[![](https://www.r-pkg.org/badges/version/AutoScore?color=green)](https://cran.r-project.org/package=AutoScore)
[![](http://cranlogs.r-pkg.org/badges/grand-total/AutoScore?color=green)](https://cran.r-project.org/package=AutoScore)
[![](http://cranlogs.r-pkg.org/badges/last-month/AutoScore?color=green)](https://cran.r-project.org/package=AutoScore)
[![](http://cranlogs.r-pkg.org/badges/last-week/AutoScore?color=green)](https://cran.r-project.org/package=AutoScore)
[![](https://img.shields.io/badge/doi-10.2196/21798-yellow.svg)](https://doi.org/10.2196/21798)



AutoScore: An Interpretable Machine Learning-Based Automatic Clinical
Score Generator
================

# AutoScore Introduction

AutoScore is a novel machine learning framework to automate the development of interpretable clinical scoring models. AutoScore consists of six modules: 1) variable ranking with machine learning, 2) variable transformation, 3) score derivation, 4) model selection, 5) domain knowledge-based score fine-tuning, and 6) performance evaluation. The original AutoScore structure is elaborated in [this article](http://dx.doi.org/10.2196/21798) and its flowchart is shown in the following figure. AutoScore was originally designed for binary outcomes and later extended to [survival outcomes](http://dx.doi.org/10.1016/j.jbi.2021.103959) and [ordinal outcomes](https://doi.org/10.48550/arxiv.2202.08407). AutoScore could seamlessly generate risk scores using a parsimonious set of variables for different types of clinical outcomes, which can be easily implemented and validated in clinical practice. Moreover, it enables users to build transparent and interpretable clinical scores quickly in a straightforward manner. 

> ### Please visit our [bookdown page](https://nliulab.github.io/AutoScore/) for a full tutorial on AutoScore usage.


## Usage

The five pipeline functions constitute the 5-step AutoScore-based
process for generating point-based clinical scores for binary, survival
and ordinal outcomes.

This 5-step process gives users the flexibility of customization (e.g.,
determining the final list of variables according to the parsimony plot, and
fine-tuning the cutoffs in variable transformation):

-   STEP(i): `AutoScore_rank()`or `AutoScore_rank_Survival()` or
    `AutoScore_rank_Ordinal()` - Rank variables with machine learning
    (AutoScore Module 1)
-   STEP(ii): `AutoScore_parsimony()` or
    `AutoScore_parsimony_Survival()` or
    `AutoScore_parsimony_Ordinal()` - Select the best model with
    parsimony plot (AutoScore Modules 2+3+4)
-   STEP(iii): `AutoScore_weighting()` or
    `AutoScore_weighting_Survival()` or
    `AutoScore_weighting_Ordinal()` - Generate the initial score with
    the final list of variables (Re-run AutoScore Modules 2+3)
-   STEP(iv): `AutoScore_fine_tuning()` or
    `AutoScore_fine_tuning_Survival()` or
    `AutoScore_fine_tuning_Ordinal()` - Fine-tune the score by revising
    `cut_vec` with domain knowledge (AutoScore Module 5)
-   STEP(v): `AutoScore_testing()` or `AutoScore_testing_Survival()` or
    `AutoScore_testing_Ordinal()` - Evaluate the final score with ROC
    analysis (AutoScore Module 6)

We also include several optional functions in the package, which could help with data analysis and result reporting.


## Citation

### Core paper

* Xie F, Chakraborty B, Ong MEH, Goldstein BA, Liu N. [AutoScore: A machine learning-based automatic clinical score generator and its application to mortality prediction using electronic health records](http://dx.doi.org/10.2196/21798). JMIR Medical Informatics 2020; 8(10): e21798.

### Method extensions

* Xie F, Ning Y, Yuan H, Goldstein BA, Ong MEH, Liu N, Chakraborty B. [AutoScore-Survival: Developing interpretable machine learning-based time-to-event scores with right-censored survival data](http://dx.doi.org/10.1016/j.jbi.2021.103959). Journal of Biomedical Informatics 2022; 125: 103959.

* Saffari SE, Ning Y, Xie F, Chakraborty B, Volovici V, Vaughan R, Ong MEH, Liu N, [AutoScore-Ordinal: An interpretable machine learning framework for generating scoring models for ordinal outcomes](https://doi.org/10.48550/arxiv.2202.08407), arXiv:2202.08407.

* Ning Y, Li S, Ong ME, Xie F, Chakraborty B, Ting DS, Liu N. [A novel interpretable machine learning system to generate clinical risk scores: An application for predicting early mortality or unplanned readmission in a retrospective cohort study](https://doi.org/10.1371/journal.pdig.0000062). PLOS Digital Health 2022; 1(6): e0000062.

## Contact

-   Feng Xie (Email: <xief@u.duke.nus.edu>)
-   Yilin Ning (Email: <yilin.ning@duke-nus.edu.sg>)
-   Nan Liu (Email: <liu.nan@duke-nus.edu.sg>)

# AutoScore package installation

Install from GitHub or CRAN：

``` r
# From Github
install.packages("devtools")
library(devtools)
install_github(repo = "nliulab/AutoScore", build_vignettes = TRUE)

# From CRAN (recommended)
install.packages("AutoScore")
```
[devtools]: https://github.com/hadley/devtools

Load AutoScore package: 

``` r
library(AutoScore)
```
