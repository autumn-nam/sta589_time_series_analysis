######################################################################################
# Preliminary Stage
######################################################################################

## Install and load necessary packages
pacman::p_load(reshape2, fpp2, urca, plotly, readxl)

install.packages("reshape2")     # used to reshape and melt down the data format. 
install.packages("fpp2")
install.packages("urca")
library(reshape2)
library(fpp2)
library(urca)
library(readxl)

## load the data file into R
getwd()
Bauman_MMC_Student_Count_Data <- read_excel("C:/Users/nam1w/Desktop/sta589/Bauman MMC Student Count Data.xlsx")

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
# Transform Stage 
######################################################################################
## exploratory analysis / eventually checking whether our data is stationary or not and the variance is constant or not.  

autoplot(ts.Students) + ggtitle("Number of students for each semester")

## Test for constant variance
diff(ts.Students)^2 %>% Box.test(type ="Lj")     # p-value is smaller than 0.05. Therefore, we reject the null hypothesis. In other words, Effect of ARCH is present, and the data has non-constant variance. 
                                                 # Therefore, we need transformation.
## Transform the data to have constant variance using Box.cox transformation
ld <- BoxCox.lambda(ts.Students)
trs.ts.Students <- BoxCox(ts.Students, lambda = ld) 

ggAcf(trs.ts.Students)              # According to the plot, the data has a trend. 
summary(ur.kpss((trs.ts.Students))) # Make sure it is stationary or not. we use the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test (Kwiatkowski, Phillips, Schmidt, & Shin, 1992). 
                                    # In this test, the null hypothesis is that the data are stationary, and we look for evidence that the null hypothesis is false.
                                    # The test statistic (1.5573) is much greater than the 1% critical value (0.739), indicating tha tthe null hypothesis is rejected. That is, the data is non-stationary.  Results says it is not stationary. 
                                    # so far, we learned the data is non-stationary and have a trend. In other words, before we go further to make a model, we need transformation. 
## Differencing
# When both seasonal and first differences are applied, it makes no difference which is done first-the result will be the same.
# However, if the data have a strong seasonal pattern, we recommend that seasonal differencing be done first, because the resulting series will sometimes be stationary and there will be no need for a further first difference. If first differencing is done first, there will still be seasonality present.

nsdiffs(trs.ts.Students)
ndiffs(trs.ts.Students)                              # we need both first and seasoanl differencing

d.trs.ts.Students = diff(trs.ts.Students)            # performed first differencing 
d4.trs.ts.Students = diff(trs.ts.Students, lag = 2)  # performed seasonal differencing 

d4.trs.ts.Students %>% ndiffs()                      # we've done the seasonal differencing first and checked if we need further differncing
                                                     # Results says we don't need to do first differncing 
## Test for Stationary 
ur.kpss(d4.trs.ts.Students) %>% summary()            # The test statistic 0.1063 is smaller than the 10%, 5%, 5.2%, and1% critival values, indicating that the null hyotheis is failed to rejected. That is the data is stationary. 

######################################################################################
# Analysis Stage 
######################################################################################                                                     # we are now good to go and make a model
ggtsdisplay(d4.trs.ts.Students)

m2 <- auto.arima(ts.Students)
m1 = fit <- Arima(ts.Students, order=c(4,0,1), seasonal = c(1,0,0))
checkresiduals(m1)
checkresiduals(m2)

m1 %>% forecast(h=10) %>% autoplot(include=100, ylim=c(0,6000))
m2 %>% forecast(h=10) %>% autoplot(include=100, ylim=c(0,6000))
forecast(m2)

BoxCox.lambda(ts.Students)
diff(ts.Students)^2 %>% Box.test(type = "Ljung-Box")
