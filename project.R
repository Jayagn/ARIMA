library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(viridis)
library(highcharter)

gt <- read.csv("C:/Users/JAYAGN/Desktop/GlobalTemperatures.csv")

library(lubridate)
data_country$Year <- year(data_country$dt)
IND_data <- subset(data_country, Country == "India")
summary(IND_data)
str(IND_data)

IND_data$dt <- as.Date(IND_data$dt)
str(IND_data)
IND_data$Year <- year(IND_data$dt)
names(IND_data)

Avg_temp_IND <- aggregate(AverageTemperature ~ Year, FUN=mean, data = IND_data)

library(ggplot2)

IND_50 <- subset(Avg_temp_IND, Year > 1900)

ggplot(IND_50, aes(Year, AverageTemperature)) + 
  geom_line(size = 2, aes(colour = AverageTemperature)) +
  scale_colour_gradient(low="orange", high="red") +
  scale_x_continuous(breaks=seq(1900, 2015, 10)) + 
  scale_y_continuous(breaks=seq(20, 26, 0.2)) +  
  theme(plot.background = element_rect(fill='NA') ,legend.key=element_rect(fill=NA)) +
  geom_smooth(method = "lm", se=FALSE, color="red") +
  ggtitle("Surface Temperature of India in the last 50 Years")


library('forecast')
library('tseries')

#Creating a subset from year 1990 to 2015
gty2000<- subset(gt,(gt$yyyy<-substr((as.Date(gt$dt)),1,4)>=1990))
str(gty2000)
gty2000$dt<-as.Date(gty2000$dt)
ggplot(gty2000, aes(dt, LandAverageTemperature)) + geom_line() + scale_x_date('Year')  + ylab("Land Average Temperature")

lat_ts = ts(gty2000[, c('LandAverageTemperature')])
gty2000$cleanlat = tsclean(lat_ts)
ggplot(data = gty2000, aes(x=dt, y=cleanlat)) + geom_line() +
  scale_x_date('Year')  + ylab('Cleaned Land Average Temperature')

gty2000$lat_ma30 = ma(gty2000$cleanlat, order=30)
ggplot() +
  geom_line(data = gty2000, aes(x = dt, y = cleanlat, colour = "Counts")) +
  geom_line(data = gty2000, aes(x = dt, y = lat_ma30, colour = "Monthly Moving Average"))  +
  ylab('Land Average Temperature')

delat_ma = ts(na.omit(gty2000$LandAverageTemperature), frequency=30)
decomlat = stl(delat_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomlat)
plot(decomlat)

adf.test(delat_ma, alternative = "stationary")

Acf(delat_ma, main='')

Pacf(delat_ma, main='')

adf.test(difflat_d, alternative = "stationary")

Acf(difflat_d, main='ACF for Differenced Series')

Pacf(difflat_d, main='PACF for Differenced Series')

auto.arima(deseasonal_cnt, seasonal=FALSE)

eval1<-auto.arima(deseasonal_cnt, seasonal=FALSE)
eval1

tsdisplay(residuals(eval1), lag.max=45, main='(1,1,1) Model Residuals')

eval2 = arima(deseasonal_cnt, order=c(8,1,14))
eval2

tsdisplay(residuals(eval2), lag.max=15, main='Seasonal Model Residuals')

eval_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
eval_seasonality

seas_fcast <- forecast(eval_seasonality, h=50)
plot(seas_fcast)

tsdisplay(residuals(seas_fcast), lag.max=15, main='Forecast Model Residuals')

seas_fcast

fcastarima <- forecast(eval2, h=50)
plot(fcastarima)

tsdisplay(residuals(fcastarima), lag.max=15, main='Forecast Model Residuals')

fcastarima
Year<-seq(2016,2065,1)
Yeardf<-as.data.frame(Year)
fcastarimadf<-as.data.frame(fcastarima)
nrow(Yeardf)
nrow(fcastarimadf)
fcastarima1<-cbind(Yeardf,fcastarimadf)
row.names(fcastarima1)<- NULL
fcastarima1
