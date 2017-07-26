#Needed libraries
library(mice)
library(lattice)
library(VIM)

## Reading the data
titanic <- read.table('/Users/Xuanshao/Desktop/鲁汶/硕士Statistics/titanic.txt', header=T, sep=",")
head(titanic)
titanic.missing <- titanic[,c(2,3,5,11)]
titanic.missing <- titanic.missing[,c(2,1,4,3)]
head(titanic.missing,15)

## Exploring the missingness  
#aggr from 'VIM':Aggregations for missing/imputed values
titanic.missing.aggr <- aggr(titanic.missing, numbers=TRUE, prop=TRUE, ylab=c("Histogram of missing data","Pattern"))
titanic.missing.aggr #age: missing

aggr(titanic.missing, combined=TRUE, numbers = TRUE, prop = TRUE, cex.numbers=0.87, varheight = FALSE)

#barMiss from 'VIM': Barplot with information about missing/imputed values
barMiss(titanic.missing[,c("survived","age")])
barMiss(titanic.missing[,c("sex","age")])
histMiss(titanic.missing)

#One of the reason that some age is missing is because that the age of the unsurvivors are not recorded.
#That is to say, the missing value of age is dependent of the survived variable (Y variable).
#Therefore, it is missing at random (MAR).
#Generally, under MAR, simple techniques like complete and available case analysis,
#and overall mean imputation, give biased results.
#Thus, we use Multiple imputation (MI) or Inverse Probability Weighting (IPW) to handle it.
#In this trial, we use MI.

# Studying the patterns of missiness
#md.pattern from 'mice': Missing data pattern
pattern <- md.pattern(titanic.missing)
pattern
#as indicated in the aggr and barMiss plot above, there is only missing values from 'age'.
#another way to check the pattern is use md.pairs from 'mice':Missing data pattern by variable pairs

## Imputing the missing values
imp <- mice(titanic.missing, meth = c("", "", "", "pmm"), m=100)
#metho =, corresponding to four columns, survived, pclass, sex, and age.
#since only 'age' is missing, so we use 'pmm'(predictive mean matching) to impute.
#other methods include: (default is 'pmm')
#'norm': Bayesian linear regression (numeric)
#'norm.nob': Linear regression, non-Bayesian (numeric)
#'mean': unconditional mean imputation (numeric)
#'2l.norm': Two-level linear model (numeric)
#'logreg': Logistic regression (handle categorical variable, level =2, such as 'sex' variable here)
#'polyreg': Polytomous (unordered) regression (categorical, level > 2)
#'lda': Linear discriminant analysis (categorical)
#'sample': Random sample from the observed data (any type)
imp

## Imputed values for age. Each row corresponds to a missing entry in age. 
## The columns contain the multiple imputations.
imp$imp$age[1:10,1:5]

## The complete data combine
## The first completed data set can be obtained as (only first 10 passenger shown)
complete(imp,1)[1:10,]
#The complete() from 'mice' extracts the original data and the imputed datasets from the imp object as
#a long (row-stacked) matrix. The col vector separates the observed (blue) and imputed (red) data for age
#We can plot the imputed values along with the original data to see the distribution. (but not necessary step)
com <- complete(imp, "long", inc=T)
col <- rep(c("blue","red")[1+as.numeric(is.na(imp$data$age))],101)
stripplot(age~.imp, data=com, jit=TRUE, fac=0.8, col=col, pch=20, cex=1.4,xlab="Imputation number")

# Analyzing the imputed data sets >
fit <- with(data=imp, exp=glm(survived ~ pclass + sex + age, family=binomial))
#since we mimiced 100 times, so we have 100 times of the model fit.
#Use Rubin's rule to combine result into one.
#pool() from 'mice': Multiple imputation pooling
est <- pool(fit)
summary(est)
## The column fmi contains the fraction of missing information, i.e. the proportion of the
## variability that is attributable to the uncertainty causedby the missing data.



####Extra####
#Missing not at random (MNAR): P (Missing) depends on both observed and missing values.
#MNAR: Highly problematic.
#The only way to obtain unbiased estimates of the parameters is to model missingness. 
#In other words, we would need to write a model that accounts for the missing data mechanism 
#and hope this model is approximately correct.

#Missing at random (MAR): P (Missing) depends only on observed values.

#Missing completely at random (MCAR): P (Missing) depends neither on observed nor on unobserved values.
#MCAR: Most methods work.
#Although very inefficient, some simple techniques like complete and available case analysis will give unbiased results under MCAR. 
#However, MS(Mean Substitution) and LOCF (Last Observation Carried Forward) do not work in this setting neither.

#In general, use Multiple Imputations.
