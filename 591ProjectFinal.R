######################################################################################
# Preliminary Stage
######################################################################################

## Install and load necessary packages
pacman::p_load(reshape2, fpp2, urca, plotly, readxl, seasonal)

install.packages("reshape2")     # used to reshape and melt down the data format. 
install.packages("fpp2")
install.packages("urca")
library(reshape2)
library(fpp2)
library(urca)
library(readxl)

## load the data file into R
getwd()
Bauman_MMC_Student_Count_Data <- read_excel("C:/Users/udam0/Desktop/sta589/Bauman MMC Student Count Data.xlsx")

## Reshape the data format
longstudent <- melt(Bauman_MMC_Student_Count_Data, id.vars = "Year")
workdata <- longstudent[,c(1,3)]
workdata <- workdata[order(workdata$Year),]
workdata <- workdata[,2]
workdata <- workdata[-1]
workdata <- workdata[-103]
## Check the data to see if it has the right format without obmitting any values
workdata

## Transform the data to be time series data. 
ts.Students <- ts(workdata, start = 1968, frequency = 2) # The number of students were measured two times in a year from 1968 to 2018.

######################################################################################
# 1. Introduction
######################################################################################
# Project objective #

# Data source and description #

######################################################################################
# 2. Data visualization (ch. 2)
######################################################################################
autoplot(ts.Students, size =0.7) + 
  ggtitle("Number of students for each semester") + 
  xlab("Year") + ylab("Number of Students")
# Overall, the data has a increasing trend with non-constant variance. Relatively severe fluctutions is detected between 1975 and 1990. 
# It is hard to detect consistent seasonal or cyclic pattern.

ggseasonplot(ts.Students, year.labels = T) +
  ggtitle("Seasonal plot: Number of students for each semester") +
  xlab("Semester") + ylab("Number of Students")
# Overall the number of students increased from season 1 to 2. Espeically, around the end of the 1900 and early 2000, most of them showed increasing trend. 

ggsubseriesplot(ts.Students) + 
  ggtitle("Subseries plot: Number of Studetns") +
  yalb("The number of studetns for each semester")
# This plot makes it easy to compare varitions within each season. The mean number of students are greater in seaosn 2. However, season 2 also has a lot more fluctuations. 

gglagplot(ts.Students) + 
  ggtitle("Lagplot: Number of students for each semester")
# The relationship is strongly positive at lags 1 and 2, reflecting the strong linear trend and seasonality at the same time. 

### 2.3. Recommendations
# I am not sure what this section supposed to be. 


### 2.3.1.Transformation?
# According to the autoplot, data seems to have non-constant variance. Therefore, we might need to transform the data to have constant variance. 
# However, we can assure the need with mathematical tests below. 


## Test for constant variance
diff(ts.Students)^2 %>% Box.test(type ="Lj")     # p-value is smaller than 0.05. Therefore, we reject the null hypothesis. In other words, Effect of ARCH is present, and the data has non-constant variance. 
# Therefore, we need transformation.

## Transform the data to have constant variance using Box.cox transformation
ld <- BoxCox.lambda(ts.Students)
trs.ts.Students <- BoxCox(ts.Students, lambda = ld) 

## Compare the difference 
gridExtra::grid.arrange(autoplot(trs.ts.Students)+ggtitle("After Transformation"), autoplot(ts.Students) + ggtitle("Before Transformation"))

### 2.3.2.Simple forecasting methods 
autoplot(ts.Students) +
  autolayer(meanf(ts.Students), 
            series = "Mean",  PI = F) +
  autolayer(rwf(ts.Students), 
            series = "Naive", PI = F) +
  autolayer(snaive(ts.Students), 
            series = "Seasonal Naive", PI = F) + 
  autolayer(rwf(ts.Students, drift =  T), 
            series = "Drift", PI = F) +
  ggtitle("Forecasts for the number of students for each semester") +
  xlab ("Year") + ylab("The number of students") +
  guides(colour = guide_legend(title="Forecasts"))

######################################################################################
# 3. Simple forecasting models ch.3
######################################################################################
### 3.1. Forecasting models

fit.m <- meanf(ts.Students)
fits.m <- fitted(fit.m)
autoplot(ts.Students, series = "Data") + 
  autolayer(fits.m, series = "Fitted") + 
  xlab("Year") + ylab("The number of students") +
  ggtitle("The number of studetns for each semester")

fit.n <- naive(ts.Students)
fits.n <- fitted(fit.n)
autoplot(ts.Students, series = "Data") + 
  autolayer(fits.n, series = "Fitted") + 
  xlab("Year") + ylab("The number of students") +
  ggtitle("The number of studetns for each semester")

fit.s <- snaive(ts.Students)
fits.s <- fitted(fit.s)
autoplot(ts.Students, series = "Data") + 
  autolayer(fits.s, series = "Fitted") + 
  xlab("Year") + ylab("The number of students") +
  ggtitle("The number of studetns for each semester")

fit.d <- rwf(ts.Students, drift =T)
fits.d <- fitted(fit.d)
autoplot(ts.Students, series = "Data") + 
  autolayer(fits.d, series = "Fitted") + 
  xlab("Year") + ylab("The number of students") +
  ggtitle("The number of studetns for each semester")

### 3.2. Model Diagnostic
checkresiduals(fit.m)
# According to the Ljung Box test, Reject the null: autocorrelated

checkresiduals(fit.n)
# According to the Ljung Box test, Reject the null: autocorrelated

checkresiduals(fit.s)
# According to the Ljung Box test, Reject the null: autocorrelated

checkresiduals(fit.d)
# According to the Ljung Box test, Reject the null: autocorrelated
# seasonal naive models seems to work the best. Residuals are relatively in best condition than other methods. 


### 3.3. Forecasts (errors)

mean <- accuracy(fit.m)[,1:7] 
naive <- accuracy(fit.n)[,1:7]  
snaive <- accuracy(fit.s)[,1:7]
drift <- accuracy(fit.d)[,1:7]
writeLines("Comapre Forecasts Errors of the Simple Models");rbind(mean, naive, snaive, drift)
# In the criteria of RMSE, MAE, MAPE, MASE, seasonal naive perforemed the best among four simple methods. 

######################################################################################
# 4. Data Exploration ch.6
######################################################################################
### 4.1 Decompositions.
# x-11 decomposition is not applicable because it is only developed for quarterly and monthly data. 
# SEATS secomposition is not applicable because it is only developed for quarterly and monthly data

fit.stl <- stl(ts.Students, s.window = 7)
autoplot(fit.stl)

ggsubseriesplot(seasonal(fit.stl))
autoplot(fit.stl)
autoplot(ts.Students, series = "Data") + 
  autolayer(seasadj(fit.stl), series = "Seasonally Adjusted")

fit.dec <- decompose(ts.Students)# seasonal pattern is consistent over time "additive"
autoplot(fit.dec)

ggsubseriesplot(seasonal(fit.dec))
autoplot(ts.Students, series = "Data") + 
  autolayer(seasadj(fit.dec), series = "Seasonally Adjusted")


### 4.2. Diagnostics

### 4.3. Forecasts

### 4.4. Recommendations

### 4.4.1. ETS Model


### 4.4.2. ARIMA Model 

######################################################################################
# 5. Exponentially smoothed models ch.7
######################################################################################
### 5.1. Forecasting models (damped or not)


### 5.2. Diagnostics


### 5.3. Forecasts


### 5.4. Model Selection 


######################################################################################
# 6. Forecasts using ARIMA ch.8
######################################################################################
### 6.1. Model selection 
### 6.1.1. Using ACF and PACF
### 6.1.2. Automatic


### 6.2. Diagnostics
### 6.3. Stationarity, invertability, and cyclicity check
### 6.3.1. Model representation in backshift notation
### 6.2.2. Solivng characteristics equations
### 6.4. Forecasts


######################################################################################
# 7. Forecasting using auxilliary information
######################################################################################

### 7.1. Predictors
### 7.2. Regression with ARIMA errors ch.9
### 7.2.1. diagnostics
### 7.2.2. Forecasts

######################################################################################
# 8. Model Comparison
######################################################################################

### 8.1. Forecasts accuracy 
### 8.1.1. VAlidation using trainning and testing sets
### 8.1.2. Leave-One-Out Cross-Validation
### 8.2. Best foreacsts

######################################################################################
# 9. Conclusion
######################################################################################

