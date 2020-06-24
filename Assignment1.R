---
title: "Time Series Assignment1"
author: "Santosh Kumaravel Sundaravadivelu (S3729461)"
output:
  html_document: default
  word_document: default
---

# Task I

## Introduction

The sun emits UV radiation and ozone layer protects the earth from it. The thickness of the ozone layer is calculated in the Dobson Unit. The data contains the thickness of the ozone layer from the year 1927 to 2016, where the negative value implies the decrease in the ozone thickness layer and vice versa for the positive value. In the first task, A hypothesis model is built over this data in order to predict the change in the ozone layer thickness over the next five years. In the second task, the data will be used to build all set of ARIMA(p,d,q) Models with plausible evidence.

### Importing Libraries

The libraries such as dplyr and TSA is being imported to the environment for the processing the time-series data

```{r}
library(dplyr)
library(TSA)
```

### Data Description

The data represents the thickness of the ozone layer over the 90 years starting from 1927 to 2016.

The CSV file is read by read.csv function and stored in the variable OzoneThicknessdf. 

The OzoneThicknessdf data frame will be used for future processes.


```{r}
OzoneThicknessdf <- read.csv("data1.csv", header = FALSE)
rownames(OzoneThicknessdf) <- seq(from=1927, to=2016)
class(OzoneThicknessdf)
```

```{r}
head(OzoneThicknessdf)
```

Converting the Dataframe to Timeseries object is a first step where the model understands the data as time-series data. The difference can be seen with the output of the head(OzoneThicknessTS). It will make the model understand that the data is in sequence starting from the year 1927 with frequency 1.

```{r}
# Converting Dataframe into timeseries object
OzoneThicknessTS <- ts(as.vector(OzoneThicknessdf), start = 1927, end = 2016, frequency = 1)
head(OzoneThicknessTS)

```

```{r}
# time-series plot
plot(OzoneThicknessTS, type = "o", ylab = "Ozone Layer Thickness", xlab = "Year", main = 
       "Time series plot for change in the thickness of the ozone layer")
```

The trend of the time series plot clearly shows the negative trend, which implies that the thickness of the ozone layer becomes slimmer over time.

Scatter plot is also plotted with the lag 1

```{r}
plot(y=OzoneThicknessTS,x=zlag(OzoneThicknessTS),ylab=' Current year Ozone Layer Thickness',xlab = ' Previous year Ozone Layer Thickness',main ='Scatter Plot for Ozone Layer Thickness')

```

The correlation is being calculated with lag 1 and without lag. If the Lag refers to how far the series is offset. In this case, we are considering lag 1, which means that the one series is shifted and compared the correlation. If the lag increases the number of the possible match will be decreased.

```{r}
lag_OzoneThicknessTS = zlag(OzoneThicknessTS)
index = 2:length(OzoneThicknessTS)
cor(OzoneThicknessTS[index], lag_OzoneThicknessTS[index])
```

From the above plots, insights could be gained such as,

*	From the time series plot, The negative trend in the ozone layer thickness which implies the thickness
  of the ozone layer tends to be slimmer over time. There is no seasonality present since there is no
  repeating pattern. The behavior indicates as autoregressive.
*	From the Scatter plot, It shows the upward trend with values accompanied by their sizes.
*	From the Correlation, the lag 1 change in the ozone layer is highly correlated with the current ozone 
  thickness


## Modeling Techniques

The correlation is being calculated with lag 1 and without lag. If the Lag refers to how far the series is offset. In this case, we are considering lag 1, which means that the one series is shifted and compared the correlation. If the lag increases the number of the possible match will be decreased.

### Linear Model

```{r}
linearModel = lm(OzoneThicknessTS~time(OzoneThicknessTS))
summary(linearModel)
```

```{r}
plot(OzoneThicknessTS, type = "o", ylab = "Thickness of ozone layer", main = "Timeseries plot to Ozone layer thickness with Linear Regression")
abline(linearModel,col = ' blue', lty=1)
```

From the above plots, insights could be gained such as,

*	The P-value for intercept and slope is significant because it is less than 0.05
*	Model is significant since the p-value is less than 0.05
*	R Squared and adjusted R Squared value are similar to each other, which implies the model is not a good   fit.

### Quadratic Regression

We are using order 2 and order 3 quadratic regression to enhance the efficiency and to have a better representation of data points.

```{r}
#Quadratic Regression Order 2
t = time(OzoneThicknessTS)
t2 = t ^ 2
QuadRegsquaremodel = lm(OzoneThicknessTS ~ t + t2)
summary(QuadRegsquaremodel)
```

```{r}
plot(ts(fitted(QuadRegsquaremodel)), ylim = c(min(c(fitted(QuadRegsquaremodel), as.vector(OzoneThicknessTS))), max(c(fitted(QuadRegsquaremodel),as.vector(OzoneThicknessTS)))), ylab ="Ozone Layer Thickness" ,col="blue", main = "Fitted Quadratic Curve Order 2 to Ozone Layer Thickness")
lines(as.vector(OzoneThicknessTS),type="o")
```

```{r}
#Quadratic Regression Order 3
time = time(OzoneThicknessTS)
time2 = time ^ 2
time3 = time ^ 3
QuadRegcubemodel = lm(OzoneThicknessTS ~ time+time2+time3)
summary(QuadRegcubemodel)
```


```{r}
plot(ts(fitted(QuadRegcubemodel)), ylim = c(min(c(fitted(QuadRegcubemodel), as.vector(OzoneThicknessTS))), max(c(fitted(QuadRegcubemodel),as.vector(OzoneThicknessTS)))), ylab ="Ozone Layer Thickness" ,col="blue", main = "Fitted Quadratic Curve Order 3 to Ozone Layer Thickness")
lines(as.vector(OzoneThicknessTS),type="o")
```

From the above plots, insights could be gained such as,

*	R Squared is higher comparing the quadratic regression of order 2 with the linear regression model.
*	The P-value for the quadratic regression model for order 2 is significant.
*	For Quadratic Regression for order 3, the p-value for both intercept and slopes is greater than 0.05,
  which implies insignificant in data variables.


### Cosine Modelling

Although the data does not have any seasonality effect as it is a yearly data, still a model is built to explore the insights.

```{r}
har. =harmonic(OzoneThicknessTS,0.45)
harmonicmodel = lm(OzoneThicknessTS ~ har.)
summary(harmonicmodel)
```


```{r}
plot(ts(fitted(harmonicmodel)), ylim = c(min(c(fitted(harmonicmodel), 
    as.vector(OzoneThicknessTS))), max(c(fitted(harmonicmodel),
    as.vector(OzoneThicknessTS)))), col="blue",ylab ="Ozone layer thickness" ,
    main = "Fitted Harmonic   Curve to Ozone Layer Thickness")
lines(as.vector(OzoneThicknessTS),type="o")
```

The harmonic model is considered insignificant because one harmonic term is missing and another with a p-value greater than 0.05. This proves that the harmonic model is insignificant.

## Residual Analysis

The next step is to asses the model correctly since the model found is almost fit. The residual analysis will give the behavior of the residuals like the stochastic process and assumptions will be assessed in the following plots such as Standardized Residuals vs Time, Histogram plot, QQ plot, ACF, Shapiro-Wilk normality test.

### Time Series Residuals for different models

Time series plots are plotted to compare the models. It is plotted by (2,2) alignment for easy comparison.

```{r}

# Time series of residuals
par(mfrow=c(2,2) , cex.lab = 0.8, cex.main=0.8, cex.axis = 0.8)
plot(y=rstudent(linearModel),
  x=as.vector(time(OzoneThicknessTS)),
  ylab=' Standardized Residuals',
  xlab= 'Year(Time)' ,
  type= 'o' ,
  main = " Standardized residuals of Linear Regression")
plot(y=rstudent(QuadRegsquaremodel),
  x=as.vector(time(OzoneThicknessTS)),
  ylab=' Standardized Residuals',
  xlab= 'Year(Time)' ,
  type= 'o' ,
  main = " Standardized residuals of Quadratic Regression (order 2)")
plot(y=rstudent(QuadRegcubemodel),
  x=as.vector(time(OzoneThicknessTS)),
  ylab=' Standardized Residuals',
  xlab= 'Year(Time)' ,
  type= 'o',
  main = " Standardized residuals of Quadratic Regression (order 3)")
plot(y=rstudent(harmonicmodel),
  x=as.vector(time(OzoneThicknessTS)),
  ylab=' Standardized Residuals',
  xlab= 'Year(Time)' ,
  type= 'o' ,
  main = " Standardized residuals of harmonic model")


```

Since most of the values are lying around the zero marks, the histogram might give a broader view of the distribution.

### Histogram of the different models

```{r}
# Histogram
par(mfrow=c(2,2) , cex.lab = 0.8, cex.main=0.8, cex.axis = 0.8)
hist(rstudent(linearModel),
  xlab=' Standardized Residuals',
  main = " Standardized residuals of Linear Regression")
hist(rstudent(QuadRegsquaremodel),
  xlab=' Standardized Residuals',
  main = " Standardized residuals of Quadratic Regression (order 2)")
hist(rstudent(QuadRegcubemodel),
  xlab=' Standardized Residuals',
  main = " Standardized residuals of Quadratic Regression (order 3)")
hist(rstudent(harmonicmodel),
  xlab=' Standardized Residuals',
  main = " Standardized residuals of harmonic model")

```

When comparing all the models, a histogram of linear and quadratic regression (order 2) gives a better representation of normal distribution than the harmonic and quadratic regression (order 3). The reason for concluding that the two plots more normally distributed than others, when we overlap the plots among each other, the first two plots are the best fits compared to the other two plots.

### Residuals vs fitted trend

A Scatter plot is plotted to understand the residuals similar to the fitted trend and to check the small (large) fitted values is associated with the small (large) residuals.

```{r}

# Residual vs fitted trend plot
par(mfrow=c(2,2) , cex.lab = 0.8, cex.main=0.8, cex.axis = 0.8)
plot(y=rstudent(linearModel),
  x=fitted(linearModel),
  ylab=' Standardized Residuals',
  xlab=' Fitted Trend Line Values',
  type= 'p' ,
  main = " Standardized residuals of Linear Regression")
plot(y=rstudent(QuadRegsquaremodel),
  x=fitted(QuadRegsquaremodel),
  ylab=' Standardized Residuals',
  xlab=' Fitted Trend Line Values',
  type= 'p' ,
  main = " Standardized residuals of Quadratic Regression (order 2)")
plot(y=rstudent(QuadRegcubemodel),
  x=fitted(QuadRegcubemodel),
  ylab=' Standardized Residuals',
  xlab=' Fitted Trend Line Values',
  type= 'p' ,
  main = " Standardized residuals of Quadratic Regression (order 3)")
plot(y=rstudent(harmonicmodel),
  x=fitted(harmonicmodel),
  ylab=' Standardized Residuals',
  xlab=' Fitted Trend Line Values',
  type= 'p' ,
  main = " Standardized residuals of harmonic model")

```





### QQ Plot

QQ plots are plotted to analyse the normality of residuals.

```{r}
par(mfrow=c(2,2) , cex.lab = 0.8, cex.main=0.8, cex.axis = 0.8)
qqnorm(rstudent(linearModel), main=" QQ Plot of Linear Regression")
qqline(rstudent(linearModel), col = 2 , lwd = 1 , lty = 2 )
qqnorm(rstudent(QuadRegsquaremodel), main=" QQ Plot of Quadratic Regression")
qqline(rstudent(QuadRegsquaremodel), col = 2 , lwd = 1 , lty = 2 )
qqnorm(rstudent(QuadRegcubemodel), main=" QQ Plot of Quadratic Regression (order 3)")
qqline(rstudent(QuadRegcubemodel), col = 2 , lwd = 1 , lty = 2 )
qqnorm(rstudent(harmonicmodel), main=" QQ Plot of harmonic model")
qqline(rstudent(harmonicmodel), col = 2 , lwd = 1 , lty = 2 )

```

It is normally distributed stochastic process because of the Q-Q plot of quadratic regression (order 2) and linear regression implies the normal distribution.

### Shapiro Wilk Test

The normality of residuals of the standardized residuals is analysed with Shapiro Wilk Test.

```{r}
shapiro.test(rstudent(linearModel))

```
```{r}
shapiro.test(rstudent(QuadRegsquaremodel))
```
```{r}
shapiro.test(rstudent(QuadRegcubemodel))
```
```{r}
shapiro.test(rstudent(harmonicmodel))
```

### ACF Plots

```{r}

par(mfrow=c(2 , 2 ) , cex.lab = 0.8, cex.main=0.8, cex.axis = 0.8)
acf(rstudent(linearModel), main=" ACF Plot of Linear Regression")
acf(rstudent(QuadRegsquaremodel), main=" ACF Plot of Quadratic Regression (order 2)")
acf(rstudent(QuadRegcubemodel), main=" ACF Plot of Quadratic Regression (order 3)")
acf(rstudent(harmonicmodel), main=" ACF Plot of harmonic model")

```
```{r}
par(mfrow=c(1 , 1 ))
```

The analysis that was performed concludes that the residuals such as Linear model and Quadratic regression order2 are more normally distributed.

## Model Selection and analysis

From the descriptive analysis comparing all the models, It is much evident that the quadratic regression (order 2) is the best fit. It is followed by the linear regression, quadratic regression (order 3) and harmonic model. The reason for selecting the Quadratic Regression order 2 is that the R-squared value and Adjusted R-squared value is high compared to the other models alongside with behavior analysed in the residuals analysis which can conclude Quadratic regression order 2 is the best fit. The R squared value and adjusted R squared value could be improved by adding quantity and the quality of data can be used while model building.

## Forecasting for next 5 years

The quadratic regression model (order 2) is used to predict the changes in the ozone layer thickness for the next 5 years(2017 to 2021) ahead.

```{r}

h = 5
t = c(2017,2018,2019,2020,2021)
t2 = t^2
new = data.frame(t,t2)
forecast = predict(QuadRegsquaremodel, new, interval ="prediction")
print(forecast)

```


```{r}
plot(OzoneThicknessTS,
xlim = c(1927, 2021) ,
ylim = c(‐15,5) ,type= 'o' ,xlab = ' Time(Year)',ylab = ' Ozone Layer Thickness (in Dobson Units)',
main = ' 5 Years of Forecasted changes in Ozone Layer Thickness')
lines(ts(as.vector(forecast[,1] ), start = 2017) , col= " red", type="l", lwd=2 )
lines(ts(as.vector(forecast[,2] ), start = 2017) , col= " black", type="l", lwd=2 )
lines(ts(as.vector(forecast[,3] ), start = 2017) , col= " blue", type="l", lwd=2 )
legend("bottomleft", lty=1 , pch=1 ,col=c(" red", " black", " blue") , text.width = 22,c("Data","5% forecast limits", "Forecasts") )

```

## Conclusion

The time series plot was plotted to find the trend of the ozone layer thickness. Various models created to compare the R-squared value and then Residuals analysis to analyse the behavior of the models. Considering the coefficient of goodness, i.e, Multiplied R-Squared and Adjusted R-Squared and the Residual analysis, Quadratic regression (order 2) was the best fit compared to the other models. So it was selected to forecast for the next 5 years of the ozone layer thickness. From the above, the forecasted plot, we could see the model has best defined the trend and provided a much more closer prediction. There is still a scope to enhance the efficiency of the model.





# Task II

## Importing Libraries

The libraries such as Timeseries and TSA is being imported to the environment for the processing the time-series data

```{r}
library (TSA)
library (tseries)
```

## Data Preparation

The data represents the thickness of ozone layer over the 90 years starting from 1927 to 2016.

The CSV file is read by read.csv function and stored in the variable OzoneThicknessdf. The OzoneThicknessdf data frame will be used for future processes.

The dataframe is converted into time series data for building the ARIMA models.

```{r}
OzoneThicknessdf <- read.csv('data1.csv', header = FALSE)
rownames(OzoneThicknessdf) <- seq(from=1927, to=2016)

#convertion data frame to time series
OzoneThicknessTS <- ts(as.vector(OzoneThicknessdf), start=1927, end=2016)
class(OzoneThicknessTS)
```

## ARIMA Model Analysis

A large class of ARIMA models such as Stationary and non-stationary time series analysis is used for analysing the changes in the ozone layer thickness. To analyse the trend, seasonality, and change in variance, time-series graph is plotted. Thus, the autocorrelation function(ACF) and partial autocorrelation function(PACF) are plotted to predict the p,d,q values of the ARIMA models.


```{r}
plot(OzoneThicknessTS,type= 'o' ,ylab=' Ozone Layer Thickness',xlab = ' Year',
     main ='Change in Ozone Layer Thickness')
```

To plot both ACF and PACF in the same panel mfrow function is used, c(1,2) will plot 2 graphs in the same row with size 0.7. It is visible that the time series plot has a negative trend and the thickness of the ozone layer is getting slimmer as it moves forward in time. For calculating Stationary ACF and PACF plots are plotted giving correlation values different lags.

```{r}
#plotting ACF and PACF
par(mfrow=c(1,2),cex.lab = 0.7, cex.axis = 0.7) 
acf(OzoneThicknessTS)
pacf(OzoneThicknessTS)

```

The PACF plot has a trend and non-stationary of time series, while the ACF plot has a significant correlation at lag 1 and slowly decaying. Using the Dicker Fuller Unit Root Test stationary of time series could be analysed.

```{r}
par(mfrow=c(1,1))
adf.test(OzoneThicknessTS)
```

The presence of non-stationary of time series is much evident because the p-value is 0.0867 which is greater than 0.01 at a 1% significance level. the change in the variance is handled using the BoxCox transformation. Since BoxCox only accepts positive values, the constant is added to make the data positive since most of the data is negative.

```{r warning=FALSE}
#Altered baseline of time series plot
OzoneThicknessTS.transform.data = 1 + OzoneThicknessTS - min(OzoneThicknessTS)

#BoxCox Transformation
OzoneThicknessTS.transform = BoxCox.ar(OzoneThicknessTS.transform.data)
```
```{r}
OzoneThicknessTS.transform$ci
```

```{r}
lambda = 1.2
BC.OzoneThicknessTS.transform = (OzoneThicknessTS.transform.data^lambda-1) / lambda
```

```{r}
#plotting boxcox transformed data
par(mfrow=c(1,1))
plot(BC.OzoneThicknessTS.transform,
type= 'o' ,
ylab=' Ozone Layer Thickness (in Dobson Units)',
xlab = ' Year',
main = ' Change in Ozone Layer Thickness' )

```

Since the Lambda value, 1 is much more significant, no transformation is required to alter the variance. This could be proved using QQplot and Shapiro test.

```{r}
# QQ Test
qqnorm(BC.OzoneThicknessTS.transform)
qqline(BC.OzoneThicknessTS.transform, col = "blue" )

```

```{r}
# shapiro Test
shapiro.test(BC.OzoneThicknessTS.transform)

```

From the above QQplot, it is evident that there are points which are lying away from the central blue line that implies that there is insufficiency in improving the time series normality. From the Shapiro test, the p-value is less than 0.05, rejecting the hypothesis to accept the normality of data.

To remove the non-stationary, order 1 of differencing is used and the Dicker Unit Root test is used to analyse the stationary again.

```{r}
# Order 1 of differencing
diff.OzoneThickness = diff(OzoneThicknessTS)
#plotting difference 1
par(mfrow=c(1,1))
plot(diff.OzoneThickness,
type= 'o',
ylab=' Ozone Layer Thickness (in Dobson Units)',
xlab = ' Year',
main = ' Change in Ozone Layer Thickness')

```

```{r}
#AIC is computed for AR process
ar(diff.OzoneThickness)
```

```{r}
#Dicker‐Fuller Unit‐Root
adf.test(diff.OzoneThickness)
```

The stationary presence is much evident from the inferred time series plot for differenced data for order 1 and the dicker fuller unit test of the p-value is less than 0.01, so it rejects the hypothesis that implies time series is nonstationary. 

Also, AIC is computed for the AR process that suggests the significant lag of order 6.

## ARIMA Model Selection

Order 1 differencing could be devised from the analysis of ARIMA models. to predict the p and q values of the ARIMA model, ECF and PACF plots are used.

```{r}

par(mfrow=c(1,2))
acf(diff.OzoneThickness,ci.type = 'ma')
pacf(diff.OzoneThickness)
```

In ACF plot is decayed sinusoidal with a bigger significant spike at lag 3 and PACF has spikes at lag 3, lag 4 and lag 6. Thus, ARI(3,1), ARI(4,1), IMA(1,3) are most significant and validated using EACF. to avoid higher order of lag we used the previously calculated maximum lag from the AIC test.

```{r}
#EACF test with maximum order of 6
par(mfrow=c(1,1))
eacf(diff.OzoneThickness,ar.max = 6,ma.max = 6)
```

The clear vertex could be seen at (4,1) in EACF plot, the rows such as MA(3), AR(3) and AR(4) are analysed to propose the ARIMA(3,1,1), ARIMA(4,1,1), ARIMA(4,1,2) and ARIMA(3,1,3) models included in the set of all possible models. BIC table will give the best subset of the models.

```{r}
par(mfrow=c(1,1))
result = armasubsets(y=diff.OzoneThickness,nar=6 , nma=6 , y.name='test', ar.method='ols')
plot(result)
```

From BIC table output, the components AR(3) and MA(2) are more significant which gives ARIMA(3,1,2) model in the candidate set.

## Conclusion

The data was imported into the environment for predicting all sets of possible ARIMA models in compliance with the law of parsimony. The ARIMA models with possible values are ARIMA(3,1,1), ARIMA(3,1,2), ARIMA(3,1,3), ARIMA(4,1,1) and ARIMA(4,1,2) are the models which form the set of candidate models for the dataset. From the above analysis various ARIMA models can be deployed to change the nonstationary data to stationary using BIC and EACF. These models can be used to fit the model and then selecting the best one for the test statistics.

## Reference

1. Task modules and Lecture notes for MATH2318 Time Series Analysis course.


