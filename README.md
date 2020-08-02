# AutoScore: A Machine Learning-Based Automatic Clinical Score Generator

- **AutoScore R package (version 0.1)**

### Description

AutoScore, a novel framework to automate the development of a clinical scoring model for predefined outcomes. AutoScore consists of six modules: variable ranking with machine learning, variable transformation, score derivation, model selection, domain knowledge-based score fine-tuning, and performance evaluation. The details are described in the manuscript (<http://dx.doi.org/10.2196/21798>). Users (clinicians or scientists) could seamlessly generate parsimonious sparse-score risk models (i.e., risk scores), which can be easily implemented and validated in clinical practice. Also, it enables users to build transparent and straightforward clinical scores quickly. We hope to see its application in various medical case studies.

### Functions and pipeline

The five pipeline function *AutoScore_rank()*, *AutoScore_parsimony()*, *AutoScore_weighting()*, *AutoScore_fine_tuning()* and
*AutoScore_testing()* constitute the standard 5-step AutoScore-based Score generation process. This 5-step process is flexible for users to make some choices (e.g. determine the final list variable according to the parsimony plot, or fine-tune the cut-offs in variable transformation). Please follow the step-by-step instructions to build your own scores.

* STEP (1): *AutoScore_rank()* - Rank variables by machine learning (AutoScore Module 1)
* STEP (2): *AutoScore_parsimony()* - Select the best model with parsimony plot (AutoScore Modules 2+3+4)
* STEP (3): *AutoScore_weighting()* - Generate initial score with the final list of variables (Re-run AutoScore Modules 2+3)
* STEP (4): *AutoScore_fine_tuning()* - Fine-tune the score by revising "CutVec" with domain knowledge (AutoScore Module 5)
* STEP (5): *AutoScore_testing()* - Evaluate the final score with ROC analysis (AutoScore Module 6)

We also several functions in the package, which are optional. They include *Preprocess()* for preprocessing dataset, *Descriptive* for generating the descriptive table (table one) of your dataset, *UniVariable* for creating the table of univariable analysis for your dataset, *MultiVariable* for generating the table of multivariable analysis for your dataset. These functions are handy in building predictive models, especially for preparing clinical manuscripts.

### Please cite as:
Xie F, Chakraborty B, Ong MEH, Goldstein BA, Liu N*. AutoScore: A machine learning-based automatic clinical score generator and its application to mortality prediction using electronic health records. JMIR Medical Informatics 2020:21798 (forthcoming/in press)
DOI: 10.2196/21798 (https://preprints.jmir.org/preprint/21798)

### Contact
- Feng Xie (Email: <xief@u.duke.nus.edu>)
- Nan Liu (Email: <liu.nan@duke-nus.edu.sg>)

# **AutoScore Demonstration**

## **1. Prepare data and package installation**
### Install the development version from GitHub:

```r
# install.packages("devtools")
library(devtools)
install_github(repo = "nliulab/AutoScore")
```
[devtools]: https://github.com/hadley/devtools

### Load R packages (including AutoScore package)
```r
library(AutoScore)
library(caret)
library(pROC)
library(randomForest)
library(ggplot2)
```

### Load data (input data from csv or excel)
- Users can use the integrated sample data in the package for demo
```r
df_AutoScore <- Sample_Data
```

### Data preprocessing
- Users are suggested to preprocess their data (missing values, outliers, etc) to ensure that data are in good quality before running the AutoScore Pipeline
- "Preprocess" built-in function may help with missing value imputation

### Prepare TrainSet, ValidationSet, and Testset
- Option 1: Prepare three separate datasets to train, validate, and test models
- Option 2: Use demo codes below to split their dataset into Train/validation/test datasets (70/10/20 in percentage)

### Other requirements for input data
- Independent variables (X) can be numeric (class: num/int) or categorical (class: factor/logic)
- Categories/levels for each factor should be less than 10
- Variables of character class (in R environment) are not supported. Please convert them into categorical variables first before running AutoScore
- Dependent variable (Y) should be binary, and its name should be changed to "label" (Can use codes below to do it.)

### Change "Y" (Dependent variable/Outcome) to "label" before running AutoScore
```r
names(df_AutoScore)[names(df_AutoScore)=="Mortality_inpatient"]<-"label"
```

### Data splitting (split dataset into Train/validation/test datasets (70/10/20 in percentage)；optional if users have predefined training/validation/test datasets)
```r
Out_split <- split_data(data = df_AutoScore, ratio = c(7, 1, 2))
TrainSet <- Out_split$TrainSet
ValidationSet <- Out_split$ValidationSet
TestSet <- Out_split$TestSet
```

### Data displaying
```r
head(TrainSet)
head(ValidationSet)
head(TestSet)
```

## **2. Run AutoScore to build clinical scores: 5-step process**

### STEP (1): Genrate variable ranking list (AutoScore Module 1)
- ntree: Number of trees in random forest algorithm, default: 100
```r
Ranking <- AutoScore_rank(TrainSet, ntree=100)
```

### STEP (2): Select the best model with parsimony plot (AutoScore Modules 2+3+4)
- nmin: Minimum number of selected variables, default: 1
- nmax: Maximum number of selected variables, default: 20
- probs: Predefine quantiles to convert continuous variables to categorical, default:(0, 0.05, 0.2, 0.8, 0.95, 1)
```r
AUC <- AutoScore_parsimony(TrainSet, ValidationSet, rank=Ranking, nmin=1, nmax=20, probs=c(0, 0.05, 0.2, 0.8, 0.95, 1))
```

**Determine the final list of variables "num_var" for creating the risk score, based on the parsimony plot in STEP (2)**
```r
num_var <- 6
FinalVariable <- names(Ranking[1:num_var])
```

### STEP (3): Generate the initial score with the final list of variables (Re-run AutoScore Modules 2+3)
- MaxScore: Predefined cap of of the risk score (e.g. <=100)
```r
CutVec <- AutoScore_weighting(TrainSet, ValidationSet, FinalVariable, MaxScore=100, probs=c(0, 0.05, 0.2, 0.8, 0.95, 1))
```

### STEP (4): Fine-tune the initial score generated in STEP-3 (AutoScore Module 5 & Re-run AutoScore Modules 2+3) 
- Revise "CutVec" with domain knowledge to update the scoring table (AutoScore Module 5)
- Rerun AutoScore Modules 2+3
- Users can choose any cut-off values and/or any number of categories, but are suggested to choose numbers close to the automatically determined values
```r
CutVec$tempc_mean <- c(36, 36.5, 37.3, 38)
CutVec$platelet_min <- c(60, 120, 280, 400)
CutVec$lactate_max <- c(1, 1.7, 2.8, 5.7)
CutVec$resprate_mean <- c(13, 16, 21, 26)
CutVec$spo2_mean <- c(95, 99)
ScoringTable <- AutoScore_fine_tuning(TrainSet, ValidationSet, FinalVariable, CutVec, MaxScore=100)
```

### STEP (5): Evalute the final risk score (AutoScore Module 6)
```r
AutoScore_testing(TestSet, FinalVariable, CutVec, ScoringTable)
```

## **3. Demo on several related functions**

Preprocessing
```r
# df_AutoScore <- Preprocess(Sample_Data, outcome="Mortality_inpatient")
```

Descriptive analysis and result table generation
```r
Descriptive(df_AutoScore)
```

Univariable analysis and result table generation
```r
UniTable <- UniVariable(df_AutoScore)
```

Multivariable analysis and result table generation
```r
MultiTable <- MultiVariable(df_AutoScore)
```

