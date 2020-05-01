# AutoScore: Automatic Clinical Score Generator

This is the current version of the AutoScore R package (version 1.*).

**Features**

* AutoScore, a novel framework to automate the development of a clinical score for predefined outcomes
* Consists of three main components/modules : variable selection with machine learning, variable transformation, score derivation, 
* Three optional components: performance evaluation, determination of the number of variables by parsimony plot, fine-tuning.
* Includes framework to quickly add custom algorithms to the ensemble.
* Visualize the performance of each algorithm using built-in plotting  through ROC analysis
* Screen variables (feature selection) based on  Random Forest
* Integrate univariable analysis and multivariable analysis
* 20% test set validation to estimate the performance through ROC and other main metrics.

### Install the development version from GitHub:

```r
# install.packages("devtools")
library(devtools)
install_github(repo = "nliulab/AutoScore")
```

[devtools]: https://github.com/hadley/devtools

## Examples 

AutoSCore makes it trivial to process and develop the clinical scores given a dataset.

```r
set.seed(4)
library(AutoScore)

#data preprocessing

data<-Preprocess(testdf1_mimic,outcome = "label")

#Allbaselines model
tableone(data)
UniVariable(data)
MultiVariable(data)
stepwise(data)
lasso(data)

#
#AutoScore building
#n
AutoScore(data,n=12)

#AutoScore insample validation 
##AutoScore_imsample(data,n)
##data: preprocessed data frame
##n: number of variables you would like
AutoScore_insample(data,n=10)

#AutoScore validation(80% training,20%test)
##AutoScore_validation(data,n)
##data: preprocessed data frame
##n: number of variables you would like
AutoScore_validation(data,n=8)

AutoScore_validation(data,n=12)


#divide data into training(70%),validation(10%) and test(20%) set
##AutoScore_imsample(data,n)
##data: preprocessed data frame
##nmin: minimal number of variables you would like
##nmax: mmaximal number of variables you would like
AutoScore_validation_range(data,nmin=1,nmax=20)

```


## References 

