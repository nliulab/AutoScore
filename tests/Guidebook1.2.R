## Load R packages(besides AutoScore package)
#library(caret)
library(AutoScore)
library(pROC)
library(randomForest)
library(ggplot2)

## Load data (input data from csv or excel)
## can use sample data built in the package for the demo
df_AutoScore <- testdf6_mimic_20000

##------------------------------------------------------------------
## Data preprocessing
## - Users are suggested to handle missing values, outliers, etc
## - Make Sure data are in good quality and reasonable distribution before entering our AutoScore Pipeline
## - "Preprocess" built-in function may help in missing value imputation
##
## Prepare TrainSet, ValidationSet, and Testset
## - Users need to define datasets to train, validate, and test model
## - Users can also use codes below to split their dataset into Train/validation/test(7:1:2)
##
## Other requirements for Data input
## - Independent variables (X) can be numeric (class: num / int) or categorical (class: factor/logic).
## - Categories/levels for each factor should be less than 10
## - Variables of character class (in R environment) were not accepted. Please transfer them into categorical variables first before going on with this codebook
## - Dependent variable (Y) should be binary, and its name should be changed to ¡°label¡± (Can use codes below to do it.)
##-------------------------------------------------------------------

## Change name of Dependent variable (Y)/Outcome to "label" before going on with this codebook
names(df_AutoScore)[names(df_AutoScore)=="Mortality_inpatient"]<-"label"

## Data Splitting
set.seed(4)
Testindex <- sample((1:20000), 4000)
Validateindex <- sample((1:20000)[(1:20000) %in% Testindex], 2000)

TrainSet <- df_AutoScore[-c(Validateindex, Testindex),]
TestSet <- df_AutoScore[Testindex,]
ValidationSet <- df_AutoScore[Validateindex,]

## Data displaying
head(TrainSet)
head(ValidationSet)
head(TestSet)

summary(TrainSet$label)

TrainSet1<-rbind(TrainSet[TrainSet$label==1,][1:150,],TrainSet[TrainSet$label==0,])
TrainSet<-TrainSet1
##-------------------------------------------------------------------
## Run AutoScore to build clinical scores: 5-step standard process
##-------------------------------------------------------------------

## STEP (1): Genrate variable ranking List (AutoScore Module 1)
## - ntree: Number of trees in random forest algorithm, default:100
Ranking <- AutoScore_rank(TrainSet, ntree=30)

## STEP (2): Select the best model with parsimony plot (AutoScore Modules 2+3+4)
## - nmin: Minimum number of selected variables, default:1
## - nmax: Maximum number of selected variables, default:20
## - probs: Predefine quantiles to convert continuous variables to categorical, default:(0, 0.05, 0.2, 0.8, 0.95, 1)
AUC <- AutoScore_parsimony(TrainSet, ValidationSet, rank=Ranking, nmin=1, nmax=20, probs=c(0, 0.05, 0.2, 0.8, 0.95, 1))

## Determine final num_var for predictive modeling
## -- decided with the parsimony plot from STEP(2)
num_var <- 7
FinalVariable <- names(Ranking[1:num_var])

## STEP (3): Generate initial score with Final Variable list (Rerun AutoScore Module 2+3)
## - MaxScore: Predefined cap of final score, e.g. 100
CutVec <- AutoScore_weighting(TrainSet, ValidationSet, FinalVariable, MaxScore=100, probs=c(0, 0.05, 0.2, 0.8, 0.95, 1))

## STEP (4): Fine-tune the score
## - Revise CutVec with domain knowledge to update scoring table (AutoScore Module 5)
## - Rerun AutoScore Modules 2+3
## - User can choose any cut-off values/any number of categories
CutVec$tempc_mean <- c(36, 36.5, 37.3, 38)
CutVec$platelet_min <- c(60, 120, 280, 400)
CutVec$lactate_max <- c(1, 1.7, 2.8, 5.7)
CutVec$resprate_mean <- c(13, 16, 21, 26)
CutVec$spo2_mean <- c(95, 99)
ScoringTable <- AutoScore_fine_tuning(TrainSet, ValidationSet, FinalVariable, CutVec, MaxScore=100)

## STEP (5): Final score evaluation (AutoScore Module 6)
AutoScore_testing(TestSet, FinalVariable, CutVec, ScoringTable)


##-------------------------------------------------------------------
## Run AutoScore to build clinical scores: direct function usage
##-------------------------------------------------------------------

# preprocessing
# df_AutoScore<-Preprocess(Sample_Data,outcome = "Mortality_inpatient")

## This function is used to generate scoring model based on a dataset and predefined number of variables. And it uses all sample for performance evaluation, which is good for some studied with small sample size.
AutoScore_insample(df_AutoScore, m = 6, MaxScore = 150, probs = c(0, 0.05, 0.2, 0.8, 0.95, 1))

## This function is used to generate scoring model based on a dataset and predefined number of variables. And it uses testing data randomly selected from the original dataset for performance evaluation, which is good for some studied with relatively big sample size.
AutoScore_outofsample(df_AutoScore, m = 6 , Percentage_test = 0.2, MaxScore = 150, probs = c(0, 0.05, 0.2, 0.8, 0.95, 1))


##-------------------------------------------------------------------
## Other analysis direct function usage
##-------------------------------------------------------------------

## Descriptive Analysis and generate the result table
Descriptive(df_AutoScore)

## Univariable Analysis and generate the result table
UniTable<-UniVariable(df_AutoScore)

## Multivariable Analysis and generate the result table
MultiTable<-MultiVariable(df_AutoScore)


