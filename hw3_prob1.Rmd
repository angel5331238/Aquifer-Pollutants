---
title: "hw3_prob1"
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
  olsrr
  )
```


## Appendix 16
The definitions of the variables in the data set are as follows:

- `tds` total dissolved salts
- `uranium` uranium concentration
- `bicarbonate` a variable that is either 0 or 1 indicating whether there was at least 50% of a bicarbonate present

Read in the data:
```{r, message=FALSE}
app16 <- read_csv('appc16.csv') %>%
  setNames(tolower(names(.))) # variable names are lower case
glimpse(app16)

```

# Introduction
To investigate the relationship between uranium and total dissolved salts and bicarbonate, we want to find a suitable linear regression model. The flow of the investigation is as follows:

(1) Plot the data set to understand what the data looks like.

(2) Modify the data set appropriately to make it reasonable to develop linear model.

(3) Examine the diagnostic residual plots to see if the required assumptions are satisfied.

(4) Transform the data to improve the problems.

(5) Analyze the regression summary and make the decision.

(6) Discuss the characteristics and drawbacks of the model.


# Building Model

To begin with model construction, take a look at the relationship of uranium and the explanatoy variables (tds, bicarbonate).
```{r plot ori data}
  ggplot(data=app16,aes(x=tds,y=uranium))+
  geom_point(size=3,alpha=0.4)+
  ggtitle("uranium vs. tds")
  
  ggplot(data=app16,aes(x=bicarbonate,y=uranium))+
  geom_point(alpha=0.4,size=3)+
  ggtitle("uranium vs. bicarbonate")
  
```

It is hard to directly see a pattern of the `uranium` vs. `tds` scatter plot.
As for `uranium` vs. `bicarbonate`, `urarium` tends to be higher when `bicarbonate` equals to 1.

Seperate the data into two groups.

Plot `uranium` vs. `bicarbonate` again but categorize by `bicarbonate`.
```{r}
  ggplot(data=app16,aes(x = tds,y = uranium, color = as.factor(bicarbonate)))+
  geom_point()+
  ggtitle("uranium vs. tds")+
  labs(color = "bicarbonate")

```

The relationship of `uranium` and `tds` appears now. It looks like two different linear relationships with different level of bicarbonate concentration. We can group the data by bicarbonate and build linear model for each.

Separate data into two groups: *bicarbinate = 0* & *bicarbinate = 1*
```{r seperate 0 and 1}
app16_0 <- app16 %>%
  filter(bicarbonate==0)
app16_1 <- app16 %>%
  filter(bicarbonate==1)
```



## Bicarbonate = 0

Build linear model and plot diagnostic plots to see if there is any problem.
```{r}
#bicarbonate = 0
app16_0_lm <- lm(uranium ~ tds, data = app16_0)
summary(app16_0_lm)

par(mfrow = c(2, 2)) # set up 2 by 2 plot
plot(app16_0_lm) # built-in diagnostic plots
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)

plot(density(app16_0_lm$residuals))
boxplot(app16_0_lm$residuals, main = "Residual boxplot")

  #leverage
lev = hat(model.matrix(app16_0_lm))
plot(lev)
app16_0[lev>(3*2/23),] 

```

Examine the diagnostic plots first to see if the characteristics of residuals can support the OLS assumptions or not.

- Residuals vs. Fitted: There is no obvious pattern of residuals, while they are not very random as well. Because of data separation, there is only 23 data to construct the model, increasing the uncertainty.

- Normal Q-Q: Residuals are roughly normal-distributed but deviate a bit at the both ends. From the density distribution plot and boxplot, we can see the shape and symmetry are not perfect.

- Scale-Location: Here we see the biggest problem of this model -- the residual variance is not constant. The increase of variance shows its heteroscedasticity, which makes the model only able to predict uranium with given tds but without other information (e.g. predicted variance) and significance test.

- Residuals vs. Leverage: There is one point locating at the right but not falling outside of Cook's distance, so its influence is low. Further calculating *hi*, we can see no data is high leverage ($h_{i}>3*p/n$).

Back to the model summary, the $R^2$ is 0.4063 and the adjusted $R^2$ is 0.378, which are not very high. The coefficient of tds passes t-test and the model's p-value is also significant at 0.01 level, but we can't be confident with these results because the homoscedasticity premise is not satisfied.


Next, transform `uranium` into `log(uranium)` to improve the heteroscedasticity.
```{r}
#bicarbonate=0, log(y)
app16_0_logy <- app16_0 %>%
  mutate(uranium = log(uranium, base = 10))

app16_0_logy_lm <- lm(uranium ~ tds, data = app16_0_logy)
summary(app16_0_logy_lm)

par(mfrow = c(2, 2)) # set up 2 by 2 plot
plot(app16_0_logy_lm) # built-in diagnostic plots
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)

plot(density(app16_0_logy_lm$residuals))
boxplot(app16_0_logy_lm$residuals, main = "Residual boxplot")

#leverage
lev = hat(model.matrix(app16_0_logy_lm))
plot(lev)
app16_0_logy[lev>(3*2/23),] 

```

After transforming, the overall performance improves. Residuals become more normal-distributed, while the skewness becomes slightly negative due to log transformation. The variance of residuals becomes more homogeneous. The randomness of residuals is still not quite clear.

Now, the five assumptions for the purpose of OLS are all imperfectly satisfied:
(1) y is linearly related to x.
(2) Data used to fit the model are representative of data of interest. 
(3) Variance of the residuals is constant.
(4) The residuals are independent. 
(5) The residuals are normally distributed. 

$R^2$ and adjusted $R^2$ increase. The two coefficients are both significant at t-test, and p-value of regression is also significant at 99.9% level, rejecting the null hypothesis.

$H_{0}: \beta_{1} = 0$ (There is no linear relationship between uranium and tds.)

$H_{1}: \beta_{1} \neq 0$ (There is linear relationship between uranium and tds.)


Plot scattor plots of the original and transformed data.
```{r}
lm_eqn <- function(m){
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2, 
         list(a = format(coef(m)[1], digits = 3), 
              b = format(coef(m)[2], digits = 3), 
             r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));                 
}

#origin
ggplot(data=app16_0,aes(x=tds,y=uranium))+
  geom_point(size=3,alpha=0.4)+
  ggtitle("uranium vs. tds, bicarbonate = 0")+
  geom_smooth(method='lm', se=FALSE)+
  geom_text(x = 1000, y = -1, label = lm_eqn(app16_0_lm), parse = TRUE)



#logy
ggplot(data=app16_0_logy,aes(x=tds,y=uranium))+
  geom_point(size=3,alpha=0.4)+
  ggtitle("log(uranium) vs. tds, bicarbonate = 0")+
  labs(y = "log(uranium)")+
  geom_smooth(method='lm', se=FALSE)+
  geom_text(x = 1000, y = -0.75, label = lm_eqn(app16_0_logy_lm), parse = TRUE)


```

The regression model for 'bicarbonate = 0' group is $$log(uranium)=-0.836+0.00129*tds$$



## Bicarbonate = 1

Build linear model and plot diagnostic plots to see if there is any problem.
```{r}
#bicarbonate = 1
app16_1_lm <- lm(uranium ~ tds, data = app16_1)
summary(app16_1_lm)

par(mfrow = c(2, 2)) # set up 2 by 2 plot
plot(app16_1_lm) # built-in diagnostic plots
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)

plot(density(app16_1_lm$residuals))
boxplot(app16_1_lm$residuals, main = "Residual boxplot")

  #leverage
lev = hat(model.matrix(app16_1_lm))
plot(lev)
app16_1[lev>(3*2/21),]  

ols_dffits_plot(app16_1_lm)

```

Examine the diagnostic plots first to see if the characteristics of residuals can support the OLS assumptions or not.

- Residuals vs. Fitted: The curvature pattern shows the residuals are not linear. Some transformations or other variables are needed to improve this problem. Again, there is only 21 data to construct the model, so the uncertainty is large. We can see the rising trend at the right of the figure is led by three points.

- Normal Q-Q: Residuals are not far away from normal distribution, but not convinced as well. From the density distribution plot and boxplot, we can see the shape and symmetry are not perfect.

- Scale-Location: THe variance of residuals is homogeneous.

- Residuals vs. Leverage: There are three points locating at the right and one of them falls at the Cook's distance line. Further calculating *hi*, we can see there is one data has high leverage ($h_{i}>3*p/n$). Using DFFITS to see if this data has high influence, and the result plot shows it does excess the threshold. However, due to the limited knowledge of this data, we cannot remove this point arbitrarily, but we should keep in mind that it may have impact on the regression model.

Back to the model summary, the $R^2$ is 0.7045 and the adjusted $R^2$ is 0.689, which are quite high. The two coefficients both pass t-test and the model's p-value is also very small. However, since the residuals show non-linear pattern, this model is not good and reliable. We need to transform the data to ameliorate it.


Transform `uranium` into `log(uranium)`.
```{r}
#bicarbonate=1, log(y)
app16_1_logy <- app16_1 %>%
  mutate(uranium = log(uranium, base = 10))


app16_1_logy_lm <- lm(uranium ~ tds, data = app16_1_logy)
summary(app16_1_logy_lm)

par(mfrow = c(2, 2)) # set up 2 by 2 plot
plot(app16_1_logy_lm) # built-in diagnostic plots
par(mfrow = c(1, 1)) # go back to 1 by 1 plots (default)

plot(density(app16_1_logy_lm$residuals))
boxplot(app16_1_logy_lm$residuals, main = "Residual boxplot")

  #leverage
lev = hat(model.matrix(app16_1_logy_lm))
plot(lev)
app16_1[lev>(3*2/21),] 

ols_dffits_plot(app16_1_logy_lm)

```

After transforming, the curvature problem improves. The Q-Q plot shows residuals become more normal-distributed, and the density plot also shows the improvement even though the skewness becomes slightly negative due to log transformation. As for leverage, there is still one outlier but not the same one as before. This outlier has high influence as well.

Now, the five assumptions for the purpose of OLS are all roughly satisfied. We can go back to interpret the regression summary. $R^2$ and adjusted $R^2$ are around 0.5, and only the coefficient of tds is significant. The p-value of regression is significant at 99.9% level, rejecting the null hypothesis.

$H_{0}: \beta_{1} = 0$ (There is no linear relationship between uranium and tds.)

$H_{1}: \beta_{1} \neq 0$ (There is linear relationship between uranium and tds.)


Plot scattor plots of the original and transformed data.
```{r}
#origin
ggplot(data=app16_1,aes(x=tds,y=uranium))+
  geom_point(size=3,alpha=0.4)+
  ggtitle("uranium vs. tds, bicarbonate = 1")+
  geom_smooth(method='lm', se=FALSE)+
  geom_text(x = 500, y = 2.5, label = lm_eqn(app16_1_lm), parse = TRUE)

#logy
ggplot(data=app16_1_logy,aes(x=tds,y=uranium))+
  geom_point(size=3,alpha=0.4)+
  ggtitle("log(uranium) vs. tds, bicarbonate = 1")+
  labs(y = "log(uranium)")+
  geom_smooth(method='lm', se=FALSE)+
  geom_text(x = 500, y = 0.5, label = lm_eqn(app16_1_logy_lm), parse = TRUE)

```

The final regression model for 'bicarbonate = 1' group is $$log(uranium)=-0.356+0.00486*tds$$


# Discussion and Conclusion

The uranium data can be categorized by bicarbonate, so I built the regression model for the two groups separately. 

The final results are as follows:

If bicarbonate = 0, $log(uranium)=-0.836+0.00129*tds$

If bicarbonate = 1, $log(uranium)=-0.356+0.00486*tds$

The uranium concentration changes more rapidly with total dissolved salts when bicarbonate presents more than 50%. The changing rate is 4 times more than low bicarbonate situation.


The drawback of the separation is that the data amount decreases to half in each model, decreasing the credibility of models. Take the second group (i.e. bicarbonate = 1) as an example. The data point number with tds > 450 is only three, leveraging the regression easily. 

Though the non-linear pattern of residuals in the second group is improved through transformation, the other performances such as $R^2$ and coefficient significance become worse. There might be other influence factors not included here able to improve the problem and keep the good performances at the same time. The other possible solution is involving the interaction term between the two explanatory variables (i.e. tds*bicarbonate.) However, it cannot be reasonable unless the interaction between tds and bicarbonate does exist in reality, and unfortunately I have limited knowledge about it. The two possible methods can avoid the drawback of less data due to separation and may generate better performance. 


