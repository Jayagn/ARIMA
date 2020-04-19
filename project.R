library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(viridis)
library(countrycode)
library(highcharter)

data_city <- read.csv("C:/Users/JAYAGN/Desktop/GlobalLandTemperaturesByCity.csv/GlobalLandTemperaturesByCity.csv")
data_country <- read.csv("C:/Users/JAYAGN/Desktop/GlobalLandTemperaturesByCountry.csv/GlobalLandTemperaturesByCountry.csv")
gt <- read.csv("C:/Users/JAYAGN/Desktop/GlobalTemperatures.csv")

library(lubridate)
data_country$Year <- year(data_country$dt)
IND_data <- subset(data_country, Country == "India")
summary(IND_data)
str(IND_data)

IND_data$dt <- as.Date(IND_data$dt)
str(IND_data)
#Extracting the Years of all the dates and adding it as a new column
IND_data$Year <- year(IND_data$dt)
names(IND_data)

#Aggregating the average temperatures year wise
Avg_temp_IND <- aggregate(AverageTemperature ~ Year, FUN=mean, data = IND_data)

#Data Visualizations
library(ggplot2)

#Plotting the increase in temperature in last 50 years
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
ggplot(gty2000, aes(dt, LandAverageTemperature)) +
  geom_line() + scale_x_date('Year')  + ylab("Land Average Temperature")

lat_ts = ts(gty2000[, c('LandAverageTemperature')])
gty2000$cleanlat = tsclean(lat_ts)
ggplot(data = gty2000, aes(x=dt, y=cleanlat)) + geom_line() +
  scale_x_date('Year')  + ylab('Cleaned Land Average Temperature')

#moving average to average the monthly values to check pattern
gty2000$lat_ma30 = ma(gty2000$cleanlat, order=30)
ggplot() +
  geom_line(data = gty2000, aes(x = dt, y = cleanlat, colour = "Counts")) +
  geom_line(data = gty2000, aes(x = dt, y = lat_ma30, colour = "Monthly Moving Average"))  +
  ylab('Land Average Temperature')

#STL is a flexible function for decomposing and forecasting the series. 
#It calculates the seasonal component of the series using smoothing, 
#and adjusts the original series by subtracting seasonality to create a deseasonalised map.
delat_ma = ts(na.omit(gty2000$LandAverageTemperature), frequency=30)
decomlat = stl(delat_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomlat)
plot(decomlat)

#Fitting an ARIMA model requires the series to be stationary. 
#A series is said to be stationary when its mean, variance, and autocovariance are time invariant.

adf.test(delat_ma, alternative = "stationary")

#ACF are a useful visual tool in determining whether a series is stationary. 
#These plots can also help to choose the order parameters for ARIMA model.
#They display correlation between a series and its lags. 
#While PACF display correlation between a variable and its lags that is not explained by previous lags. 
#PACF plots are useful when determining the order of the AR(p) model.

Acf(delat_ma, main='')

Pacf(delat_ma, main='')

#These plots help judging the required order parameters for ARIMA,
#by checking the lags on the plot. The blue lines seen on the plots are 95% significance boundaries.
#We can see above in PACF plot, there are 2 lags close to 0 that are falling outside the boundary.

#Differencing is done to remove the trend and coerce the data to stationarity.
#Differencing looks at the difference between the value of a time series at a certain point in time and its preceding value.

difflat_d = diff(deseasonal_cnt, differences = 1)
plot(difflat_d)

adf.test(difflat_d, alternative = "stationary")

Acf(difflat_d, main='ACF for Differenced Series')

Pacf(difflat_d, main='PACF for Differenced Series')

#Differencing of d=1 has not worked well for this data.
#This suggests more iterations have to be done with AR or MA models to find the best suit prediction model.

#auto.arima()automatically generates a set of optimal (p, d, q) and 
#searches through combinations of order parameters to pick the set that optimizes model fit criteria.

auto.arima(deseasonal_cnt, seasonal=FALSE)

eval1<-auto.arima(deseasonal_cnt, seasonal=FALSE)
eval1

tsdisplay(residuals(eval1), lag.max=45, main='(1,1,1) Model Residuals')

#Still, there is one lag close to 0. Auto arima uses a default of 1,1,1 for p,d,q model.
#I am going to try fitting manual arima to see if the lags can be removed.After several trials model order of 8,1,14 for p,d,q worked.

eval2 = arima(deseasonal_cnt, order=c(8,1,14))
eval2

tsdisplay(residuals(eval2), lag.max=15, main='Seasonal Model Residuals')

eval_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
eval_seasonality

#Forecasting using a fitted model is straightforward in R. 
#We can specify forecast horizon h periods ahead for predictions to be made, and use the fitted model to generate those predictions:
#here h refers to the no of years we would like to forecast in future
seas_fcast <- forecast(eval_seasonality, h=50)
plot(seas_fcast)

tsdisplay(residuals(seas_fcast), lag.max=15, main='Forecast Model Residuals')

seas_fcast

fcastarima <- forecast(eval2, h=50)
plot(fcastarima)

tsdisplay(residuals(fcastarima), lag.max=15, main='Forecast Model Residuals')
#This shows very good results and the temperature forecasts also follow the actual pattern of history. 
#The blue line is the temperature forecast of Land Average Temperature for next 50 years starting from 2015.

fcastarima
#The output did not show Year values, so I added them. Here is what the final forecast avaerage land temperature values of next fifty years look like.
Year<-seq(2016,2065,1)
Yeardf<-as.data.frame(Year)
fcastarimadf<-as.data.frame(fcastarima)
nrow(Yeardf)
nrow(fcastarimadf)
fcastarima1<-cbind(Yeardf,fcastarimadf)
row.names(fcastarima1)<- NULL
fcastarima1

#Conclusion: From the study its clear that there seems to be a seasonality and trend with temperature data. 
#When we look at the graph, it suggests the Land Average temperature will trace the temperature levels as of past years.
#This study is done for training purpose, however if all variables like maximum, minimum and oceans temperatures were taken into consideration,
#it might give actual results, where the temperature trend in fact is increasing as years go by. Again, requesting to share your valuable feedback. 
#Thanks, Jayagn


