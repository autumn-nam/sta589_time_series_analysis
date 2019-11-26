install.packages("reshape2")
install.packages("fpp2")
install.packages("urc")
library(reshape2)
library(fpp2)
library(urca)
library(readxl)

Bauman_MMC_Student_Count_Data <- read_excel("U:/STA589/Bauman MMC Student Count Data.xlsx")

longstudent <- melt(Bauman_MMC_Student_Count_Data, id.vars = "Year")
workdata <- longstudent[,c(1,3)]
workdata <- workdata[order(workdata$Year),]
workdata <- workdata[,2]
workdata <- workdata[-1]
workdata <- workdata[-103]
ts.Students <- ts(workdata, start = 1968, frequency = 2)
tsStudents <- ts(workdata, start = 1968, frequency = 2)
autoplot(ts.Students)
ggAcf(ts.Students)
ndiffs(ts.Students)
summary(ur.kpss((ts.Students)))

d.ts.students = diff(ts.Students)
d4.ts.students = diff(ts.Students, lag = 4)

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
