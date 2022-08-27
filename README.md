[![](https://www.r-pkg.org/badges/version/AutoScore?color=green)](https://cran.r-project.org/package=AutoScore)
[![](http://cranlogs.r-pkg.org/badges/grand-total/AutoScore?color=green)](https://cran.r-project.org/package=AutoScore)
[![](http://cranlogs.r-pkg.org/badges/last-month/AutoScore?color=green)](https://cran.r-project.org/package=AutoScore)
[![](http://cranlogs.r-pkg.org/badges/last-week/AutoScore?color=green)](https://cran.r-project.org/package=AutoScore)
[![](https://img.shields.io/badge/doi-10.2196/21798-yellow.svg)](https://doi.org/10.2196/21798)



AutoScore: An Interpretable Machine Learning-Based Automatic Clinical
Score Generator
================

-   [AutoScore Introduction](#autoscore-introduction)
    -   [Usage](#usage)
    -   [Citation](#citation)
    -   [Contact](#contact)
-   [Install the package and prepare
    data](#install-the-package-and-prepare-data)
    -   [Install the development version from GitHub or the stable
        version from CRAN
        (recommended):](#install-the-development-version-from-github-or-the-stable-version-from-cran-recommended)
    -   [Load R package](#load-r-package)


                                        


-   **Check out bookdown pages (<https://tinyurl.com/AutoScoreBook/>)
    for guidebook and tutorial**
-   **Check out [**AutoScore Related Published Papers**](https://github.com/nliulab/AutoScore/blob/master/README_Application.md)
- **GitHub Package (version 0.4.0)**
      - 2021.9.15: Updated to version 0.2.1 with imporved parsimony plot
      - 2022.4.5: Updated to version 0.3.0 with more functions (including conversion tables, improved ROC curve, AUC-based varaible ranking, etc.)
      - 2022.4.27: Updated to version 0.3.1 after fixing some bugs on categorization issue on Module 2
      - 2022.8.5: Updated to 0.4.0 
      - 2022.9: To be updated to 1.0.0 (Beta)
  - **[CRAN Package (version 0.3.0)](<https://cran.r-project.org/web/packages/AutoScore/>)**



# AutoScore Introduction

AutoScore is a novel machine learning framework to automate the
development of interpretable clinical scoring models. AutoScore consists
of six modules: 1) variable ranking with machine learning, 2) variable
transformation, 3) score derivation, 4) model selection, 5) domain
knowledge-based score fine-tuning, and 6) performance evaluation. The
original AutoScore structure is elaborated in the article
(<http://dx.doi.org/10.2196/21798>) and its flowchart is shown in the
following figure. AutoScore was originally designed for binary outcomes
and later extended to survival outcomes
(<http://dx.doi.org/10.1016/j.jbi.2021.103959>) and ordinal outcomes
(<https://doi.org/10.48550/arxiv.2202.08407>). AutoScore could
seamlessly generate risk scores using a parsimonious set of variables
for different types of clinical outcomes, which can be easily
implemented and validated in clinical practice. Moreover, it enables
users to build transparent and interpretable clinical scores quickly in
a straightforward manner. Please go to our bookdown page
(<https://nliulab.github.io/AutoScore/>) to find the full tutorial of
using AutoScore package

## Usage

The five pipeline functions constitute the 5-step AutoScore-based
process for generating point-based clinical scores for binary, survival
and ordinal outcomes.

This 5-step process gives users the flexibility of customization (e.g.,
determining the final list of variables according to the parsimony plot,
and fine-tuning the cutoffs in variable transformation). Please follow
the step-by-step instructions (in following Demos) to build your own
scores.

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

We also include several optional functions in the package, which could
help with data analysis and result reporting.

Please go to Our bookdown page (<https://nliulab.github.io/AutoScore/>)
to find the full tutorial of using AutoScore package

## Citation

Xie F, Chakraborty B, Ong MEH, Goldstein BA, Liu N. AutoScore: A Machine
Learning-Based Automatic Clinical Score Generator and Its Application to
Mortality Prediction Using Electronic Health Records. *JMIR Medical
Informatics* 2020;8(10):e21798 (<http://dx.doi.org/10.2196/21798>)

Xie F, Ning Y, Yuan H, et al. AutoScore-Survival: Developing
interpretable machine learning-based time-to-event scores with
right-censored survival data. *J Biomed Inform.* 2022;125:103959.
(<http://dx.doi.org/10.1016/j.jbi.2021.103959>)

Saffari SE, Ning Y, Xie F, Chakraborty B, Volovici V, Vaughan R, Ong
MEH, Liu N, AutoScore-Ordinal: An interpretable machine learning
framework for generating scoring models for ordinal outcomes,
arXiv:2202.08407 (<https://doi.org/10.48550/arxiv.2202.08407>)

Ning Y, Li S, Ong MEH, Xie F, Chakraborty B, Ting DSW, Liu N. A novel
interpretable machine learning system to generate clinical risk scores:
An application for predicting early mortality or unplanned readmission
in a retrospective cohort study, *PLOS Digit Health* 1(6): e0000062
(<https://doi.org/10.1371/journal.pdig.0000062>).

## Contact

-   Feng Xie (Email: <xief@u.duke.nus.edu>)
-   Yilin Ning (Email: <yilin.ning@duke-nus.edu.sg>)
-   Nan Liu (Email: <liu.nan@duke-nus.edu.sg>)

# Install the package and prepare data

## Install the development version from GitHub or the stable version from CRAN (recommended):

``` r
# From Github
install.packages("devtools")
library(devtools)
install_github(repo = "nliulab/AutoScore", build_vignettes = TRUE)

# From CRAN (recommended)
install.packages("AutoScore")
```

## Load R package

``` r
library(AutoScore)
```

Please go to our bookdown page (<https://nliulab.github.io/AutoScore/>)
for looking at the full tutorial of using AutoScore package
