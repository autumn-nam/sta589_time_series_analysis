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
Bauman_MMC_Student_Count_Data <- read_excel("C:/Users/udam0/Downloads/Bauman MMC Student Count Data.xlsx")

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
ts.Students <- ts(workdata, start = 1968, frequency = 2)

######################################################################################
# Analysis Stage 
######################################################################################
autoplot(ts.Students) + ggtitle("Number of students for each semester")
ggAcf(ts.Students)              # According to the plot, it has a trend. 
ndiffs(ts.Students)             # Make sure it needs first differencing using function. Results says yes. 
summary(ur.kpss((ts.Students))) # Make sure it is stationary or not. Results says it is not stationary. 

d.ts.students = diff(ts.Students)            # performed first differencing 
d4.ts.students = diff(ts.Students, lag = 4)  # performed seasonal differencing 

ts.Students %>% diff(lag=4) %>% ndiffs()

ggtsdisplay((ts.Students))
ggtsdisplay(d.ts.students)
ggtsdisplay(d4.ts.students)

m2 <- auto.arima(ts.Students)
m1 = fit <- Arima(ts.Students, order=c(4,0,1), seasonal = c(1,0,0))
checkresiduals(m1)
checkresiduals(m2)

m1 %>% forecast(h=10) %>% autoplot(include=100, ylim=c(0,6000))
m2 %>% forecast(h=10) %>% autoplot(include=100, ylim=c(0,6000))
forecast(m2)

BoxCox.lambda(ts.Students)
diff(ts.Students)^2 %>% Box.test(type = "Ljung-Box")
