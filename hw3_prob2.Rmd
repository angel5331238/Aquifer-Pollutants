---
title: "hw3_prob2"
author: "An-Chi Ho"
date: "`r Sys.Date()`"
output:
  rmdformats::html_docco:
    highlight: kate
    toc: true
    toc_depth: 3
---

```{r knitr_init, echo=FALSE, cache=FALSE}
# DO NOT edit this block
knitr::opts_chunk$set(
  cache=TRUE,
  comment=NA,
  message=FALSE,
  warning=FALSE,
  fig.width=12,
  fig.height=7
)
```

# Package Installation & Data Read-in

Install the required package first.
```{r}
if(!require(pacman)) install.packages('pacman')

pacman::p_load(dplyr, # data manipulation
  readr, # reading in data
  ggplot2, # visualizing data
  car, #for vif
  olsrr #model selection
  )
```

## Appendix 15

The definitions of all nine variables in the data set are as follows:

- `logca` log contributing area
- `logimp` log impervious area
- `mjtemp` mean minimum January temperature
- `msrain` mean seasonal rainfall
- `pres` percentage of area residential
- `pnon` percentage of area non-urban
- `pcomm` percentage of area commercial
- `pind` percentage of area industrial
- `nitrogen` total nitrogen load

Read in the data:
```{r, message=FALSE}
app15 <- read_csv('appc15.csv') %>%
  setNames(tolower(names(.))) # variable names are lower case
glimpse(app15)
```

# Introduction

To predict the total nitrogen load for the basin, we want to choose the most suitable combination of explanatory variables and build a multiple linear regression model. The flow of the investigation is as follows:

(1) Plot diagnostic residual figures to see if the required assumptions are satisfied.

(2) Transform the data to improve the problems.

(3) Examine the multi-collinearity problem.

(4) Use different variable selection methods to generate the suitable models.

(5) Analyze the regression summary and diagnostic plots of the model candidates.

(6) Discuss the advantages and drawbacks of the model candidates and make final decision.


# Building Model

## Diagnostic plots

To begin with model construction, plot diagnostic plots of the original full data (without transformation and with all the explanatory variables) to have a feeling about this data set and to see if the transformation of y is needed.
```{r residual plots}
#ori data
app15_lm <- lm(nitrogen ~ ., data = app15)
summary(app15_lm)

par(mfrow = c(2, 2)) # set up 2 by 2 plot
plot(app15_lm) # built-in diagnostic plots
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)

#Is residual normal distribution?
plot(density(app15_lm$residuals))
boxplot(app15_lm$residuals, main = "Residual boxplot")

```

Examine the diagnostic plots first to see if the characteristics of residuals can support the OLS assumptions or not.

- Residuals vs. Fitted: The curvature pattern shows the residuals are not linear. Some transformations or other variables are needed to improve this problem. What should be noticed is that we can see the rising trend at the right side is led by only two data points. The curvature pattern may not exist without them. The sparsity of data points is a potential problem for regression.

- Normal Q-Q: Residuals follow normal distribution in the former half part but deviate in the latter half part. However, the deviation part has only a few data points. From the density distribution plot, we can see the positive skewness pattern. The boxplot also shows the dissymmetric distribution.

- Scale-Location: The residual variance is not constant all the time. Again, the upward trend is caused by two data points. The increase of variance shows its heteroscedasticity, which rejects the model's ability to predict variance and conduct the significance test.

- Residuals vs. Leverage: There is one point locating at the right but not falling outside of Cook's distance line, so its influence is not big. Most of the data concentrate near the bottom-right corner, which is a good thing for linear regression.

Since the diagnostic plots show many problems of the required assumptions, we should transform the data first before accepting the regression result.

Due to the uneven distribution of residual points (more points on the left and less on the right), the wider variance as fitted value increases, and the positive skewness pattern, try to transform `nitrogen` into `log(nitrogen)`. This transformation can also ensure nitrogen is always non-negative 

```{r}
#logy
app15_logy <- app15 %>%
  mutate(nitrogen = log(nitrogen, base = 10))

app15_logy_lm <- lm(nitrogen ~ ., data = app15_logy)
summary(app15_logy_lm)

par(mfrow = c(2, 2)) # set up 2 by 2 plot
plot(app15_logy_lm) # built-in diagnostic plots
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)  

#Is residual normal distribution?
plot(density(app15_logy_lm$residuals))
boxplot(app15_logy_lm$residuals, main = "Residual boxplot")

```

After transforming, many problemes are improved.The originally troublesome points on the right side are now located closer to the majority. The residual pattern becomes more linear, more normal-distributed, and more constant variance. The original outlier is far away from the Cook's distance line now. With the justified assumptions (listed below), we can look at the regression summary now.

Assumptions for linear regression:

(1) y is linearly related to x.
(2) Data used to fit the model are representative of data of interest. 
(3) Variance of the residuals is constant.
(4) The residuals are independent. 
(5) The residuals are normally distributed. 
All the assumptions meet could we do the hypothesis test.

$R^2$ and adjusted $R^2$ are not low, and the p-value is small enough to reject the null hypothesis (see below). However, most of the explanatory variables are insignificant. Only `logda` and `msrain` pass t-test. This phenomenon implies the possilbe multi-collinearity problem. This problem makes the coefficients not reliable.


$H_{0}: \beta_{i} = 0, i=1,2,...,8$ (There is no variable can explain nitrogen.)

$H_{1}: At least one \beta_{i} \neq 0, i=1,2,...,8$ (At least one variable can explain nitrogen.)


Calculate VIF to see how much the variance of an estimated regression coefficient is increased compared with what it would be if the variable is independent to others.
```{r}
#find multi-collinearity
vif(app15_logy_lm)

```
The latter four variables have high VIF. If the criteria of high VIF is 10, `mjtemp` also has high multi-colliearity. For example, the standard error of `pres`'s coefficient is 9227 times as large as it would be if it is independent with other predictor variables. Under this situation, we cannot trust the coefficient results. The solution of multi-collinearity is removing the troublesome variables. It is unecessary to use two variables measuring similar thing, and additional variable reduces the degree of freedom. Therefore, what to do next is select the necessary variables for the model.


## Variable selection

### Best Subset Regression

Use `olsrr` package to get the best models of each variable number subset.
```{r}
#all_subset <- ols_all_subset(app15_logy_lm)
#plot(all_subset)

best_subset <- ols_best_subset(app15_logy_lm)
plot(best_subset)

```

Lower Cp, AIC, BIC, and higher $R^2$ indicates better models. Therefore, the best choices suggested by this function lies in using 5 or 6 variables.

See which variables they are:
```{r}
best_subset %>% filter(mindex==6)
best_subset %>% filter(mindex==5)

```

Then, build the linear models based on the results, then examine the diagnostic plots and multi-collinearity.

#### 6 variables

```{r}
lm_best_6 <- lm(nitrogen ~ logda + logimp + mjtemp + msrain + pres + pind, data = app15_logy)
summary(lm_best_6)
par(mfrow = c(2, 2)) # set up 2 by 2 plot
plot(lm_best_6) # built-in diagnostic plots
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)  

#Is residual normal distribution?
plot(density(lm_best_6$residuals))
#plot(lm_best_6$residuals)

#vif
vif(lm_best_6)

```

The diagnostic plots and residual density distribution look almost the same as the full model (with all variables), but the significance level of many coefficients improves. The $R^2$ and adjusted $R^2$ increase and p-value decreases greatly. Regarding VIF, all the variables are under 10, eliminating the multi-collinearity problem.

Overall saying, the model is better than the full model.


#### 5 variables

```{r}
lm_best_5 <- lm(nitrogen ~ logda + logimp + mjtemp + msrain + pind, data = app15_logy)
summary(lm_best_5)
par(mfrow = c(2, 2)) # set up 2 by 2 plot
plot(lm_best_5) # built-in diagnostic plots
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)  

#Is residual normal distribution?
plot(density(lm_best_5$residuals))
#plot(lm_best_5$residuals)

#vif
vif(lm_best_5)

```

Similar to the 6-variable model, the diagnostic plots do not change much. All the coefficients pass t-test now, significant at 95% level at least. The p-value is higher than the 6-variable model, while $R^2$ and adjusted $R^2$ is a bit lower. Regarding VIF, all the variables are under 10, eliminating the multi-collinearity problem.

Comparing the 6-variable and 5-variable models, adding `pres` can raise $R^2$ a bit, but the cost is higher p-value and the decrease of all the coefficients' significance. Besides, if we want to penalize the model complexity more (i.e. what BIC suggests), 5-variable one should be chosen.


Desicion for *Best Subset method*: choose the **5-variable** model, $nitrogen = 2.5385 + 0.6088logda + 0.3645logimp + 0.0323mjtemp - 0.0514msrain - 0.0140pind$.



### Variable selection: Stepwise Regression

Select the vaiables based on p-value. Stepwise method could not test all the possible combinations, so the result cannot be called as the 'best'.

#### Stepwise Forward Regression
Adding one variable at each step until achieve significance. The criteria of entering p-value is 0.3.
```{r}
# stepwise forward regression
ols_step_forward(app15_logy_lm)

```

The result is identical to 6-variable model.


#### Stepwise Backward Regression
Removing one variable at each step until achieve significance. The criteria of removing p-value is 0.3.
```{r}
# stepwise backward regression
ols_step_backward(app15_logy_lm)
```
The backward result is different from forward one, removing none of the variables. That is, all the variables are significant within the model.
The inconsistency between backward and forward results show the multi-collinearity problem. The variables which are not independent may have low significance alone but become highly significant when another variable is added in the model. As a result, the stepwise method is not suitalbe for the case with multi-collinearity. 

#### Stepwise Regression
Adding or removing one variable at each step until achieve significance. The criteria of entering p-value is 0.1, and of removing p-value is 0.3.
```{r}
# stepwise regression
ols_stepwise(app15_logy_lm)
```

The stepwise result is the same as stepwise forward one.



### Variable selection: Stepwise AIC Regression
The selection of the vaiables is based on AIC. Find the model with the smallest AIC by removing or adding  variable one at a time. Stepwise method could not test all the possible combinations, so the result cannot be called as the 'best'.

#### Stepwise AIC Forward Regression
Adding one variable at each step. The one will help the model get smallest AIC will be chosen.
```{r}
null <- lm(nitrogen ~ 1, data = app15_logy)
lm_stepaic_for <- step(null, scope = list(lower=null, upper=app15_logy_lm), direction = 'forward')
summary(lm_stepaic_for)

```
The result is the same as the 6-variable model.


#### Stepwise AIC Backward Regression
Removing one variable at each step. The one will help the model get smallest AIC will be chosen.

```{r}
lm_stepaic_back <- step(app15_logy_lm, direction = 'backward')
summary(lm_stepaic_back)

```

The result is different from any of the previous ones. The variable `pcomm` is chosen but not `logimp`.
Take a look at the diagnostic plots and VIF:

```{r}

par(mfrow = c(2, 2)) # set up 2 by 2 plot
plot(lm_stepaic_back) # built-in diagnostic plots
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)  

#Is residual normal distribution?
plot(density(lm_stepaic_back$residuals))

vif(lm_stepaic_back)

```

This model performs very well! The biggest advantage is the improvement of residual distribution, which is closer to normal distribution than all other models so far. Besides, the VIF values are all under 10.


###Stepwise AIC Regression
Adding or removing one variable at each step. The one will help the model get smallest AIC will be chosen.
```{r}
#The goal is to find the model with the smallest AIC by removing or adding variables in your scope. 
lm_stepaic_both <- step(app15_logy_lm, direction = 'both')
summary(lm_stepaic_both)

```

The result is the same as stepwise AIC backward result.


#Final Choice and Conclusion

After all the tests, the best two models I consider are as follows: $$nitrogen = 2.5385 + 0.6088logda + 0.3645logimp + 0.0323mjtemp - 0.0514msrain - 0.0140pind$$, which is generated by `olsrr` best model selection, and $$nitrogen = 2.8158 + 0.6406mjtemp - 0.0637msrain + 0.0128pres + 0.0151pcomm$$, which is generated by stepwise AIC (backward) method.


Put the results of these two models together for comparison.
```{r}

summary(lm_best_5)
summary(lm_stepaic_back)

par(mfrow = c(2, 2)) # set up 2 by 2 plot
plot(lm_best_5) # built-in diagnostic plots
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)  

par(mfrow = c(2, 2)) # set up 2 by 2 plot
plot(lm_stepaic_back) # built-in diagnostic plots
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)  

#Is residual normal distribution?
par(mfrow = c(1, 2)) 
plot(density(lm_best_5$residuals))
plot(density(lm_stepaic_back$residuals))
par(mfrow = c(1, 1))   

vif(lm_best_5)
vif(lm_stepaic_back)

#PRESS
pr1 <- lm_best_5$residuals/(1 - lm.influence(lm_best_5)$hat)
sum(pr1^2)  #PRESS1

pr2 <- lm_stepaic_back$residuals/(1 - lm.influence(lm_stepaic_back)$hat)
sum(pr2^2)  #PRESS2

```

Overall saying, the performances of these two models are very similar. Both the $R^2$ and adjusted $R^2$ are close to each other, the coefficients are all significant, and p-values are both very low. They both improve the multi-collinearity problem of the full model, limiting all VIF values under 10. The diagnostic plots show their residuals are both close to linear and constant variance, as well as little influenced by leverage effect. Further examining their PRESS, which can effectively validate the model's prediction ability, the two values are both around 3.4.

The primary difference can be seen from Normal Q-Q and density distribution plots. We can see that the 2nd model shows greater normal distribution than the 1st model. The normal distribution of residuals is the assumption for hypotheses test and confidnece/prediction interval estimations. Though the distribution of the 1st model is not extremely far away from normal distribution, it still decreases our confidence to believe the p-value and significance level. Therefore, since other performances are similar, I will choose the 2nd model because it satisfies the normal distribution assumption, which could support the hypothesis test and justify the significances. In fact, the 2nd model is also the second best 5-variable recommendation of the `ols_best_subset` function, so the best subset method and stepwise method are not conflict with each other actually.

To obtain to most suitable linear regression model for nitrogen prediction, firstly we have to transform the data to make sure the assumptions of linear regression are satisfied, or the model is meaningless. Then, the high multi-collinearity of several variables indicates the necessity of variable elimination. Through different methods, we get the best models suggested by each analysis and further examine thier diagnostic plots and significance etc. Finally, we obtain the best model we could find: $$nitrogen = 2.8158 + 0.6406mjtemp - 0.0637msrain + 0.0128pres + 0.0151pcomm$$
 
-------
[Reflection]

1. Did you work with anyone on this homework (this is allowed)? Please list their names
and UNI

Miriam Nielsen (mn2812)
Sruti Devendran (sd3159)

2. Did you use any of the suggested references? If so, how helpful was it?

H&H textbook Ch9 and Ch11, other leture slides and textbooks provided on coursework, and mounts of online resources. They're all helpful and give me different vision for the analysis.

3. About how long did you spend on this homework?
20+ hours.

ps. I understand there are numerous ways to analyze the data and there is no absolute right answer. But it can be quite confusing and don't know where to start since the instruction of the assignment is short and what have been taught in class are a lot. If you don't mind, could you provide more direction for the following homework? Such as the concept should be discussed and the method should be used. Thanks a lot! And thanks for all of your help with the class.
