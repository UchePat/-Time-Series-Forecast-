                     # Time Series Analysis: Decomposition  

View(AirPassengers)   # the dataset is within R

ap <- AirPassengers
str(ap)
head(ap)

ts(ap, frequency = 12, start = c(1949, 1))   

attributes(ap)  # in $tsp- d 1st value is d start year and 2nd value is end year of the dataset

plot(ap)  # displays a line chart. 


# Log transformation- this is done so that the span of the trendlines from year to year is not fluctuating too much
ap <- log(ap)
plot(ap)

# Decomposition of additive Time Series
mydecomp <- decompose(ap)
mydecomp

mydecomp$figure

plot(mydecomp$figure, type = 'b', xlab = 'Month',
     ylab = 'Seasonality Index', col = 'blue', las = 2)  # las = 2 will make the x-axis values to be vertical. from the chart, months 6 to 9 are very high in volume of passengers

plot(mydecomp)


#--------------------------------------------------------------------------------------------------------------


                          # Time Series Analysis: Forecasting

View(AirPassengers)   # the dataset is within R

ap <- AirPassengers
str(ap)
head(ap)

library(forecast)

# creating d model
mymodel <- auto.arima(ap)
mymodel   

attributes(mymodel)
mymodel$coef


# creating ACF and PACF plots
acf(mymodel$residuals, main = 'Correlogram')   # d dotted lines are significance bounds. check if all the vertical lines are within the dotted lines(significance bounds)

pacf(mymodel$residuals, main = 'Partial Correlogram') # pacf means partial acf. check if all the vertical lines are within the dotted lines(significance bounds)


# Ljung-Box test
Box.test(mymodel$residuals, lag = 20, type = 'Ljung-Box')  # displays p-value and df values. p-value must be < 0.05 for dere to be a statistical significance


# Residual Plot
hist(mymodel$residuals, col = 'blue', xlab = 'Error',
     main = 'Histogram of Residuals', freq = FALSE)  # displays a histogram. lines(density(mymodel$residuals)) # adds a normal distribution (line) curve to the histogram chart


# creating a Forecast 
library(ggplot2)

fc <- forecast(mymodel, 48)  # creating a forecast for the next 48 months
autoplot(fc)  # adds a forecast to the chart

accuracy(fc)  # ME- mean error, RMSE- root mean square error 




#-----------------------------------------------------------------------------------------------


                # Time Series Analysis: Clustering (using Hierarchical clustering)   
  
mydata <- read.table("synthetic_control.data", header = F, sep = "")  
str(mydata)

plot(mydata[,60])  # displays a scatterplot of only 60th column values

plot(mydata[,60], type = "l")  # uses type = "l" to display a line chart since it is Time Series data. The line chart displays 6 diff patterns

j <- c(5, 105, 205, 305, 405, 505)

mysample <- t(mydata[j,])  # using transpose on the dataset and j data

plot.ts(mysample)  # plotting a Time Series of the j data just created and the dataset

plot.ts(mysample, main = "Time Series Plot",
        col = 'blue', type = 'b')     # changes the color of d lines to blue


# Data Preparation
n <- 10

ourdata <- sample(1:100, n)  # using a random sample of 10 values(since n = 10) from 1 to 100
ourdata

yrdata <- c(ourdata, 100 + ourdata, 200 + ourdata, 300 + ourdata, 400 + ourdata,
            500 + ourdata)   # this is systematic sampling

yrdata1 <- mydata[yrdata,]
str(yrdata1)

mypattern <- c(rep("Normal", n), rep("Cyclic", n), rep("Increasing trend", n),
               rep("Decreasing trend", n), rep("Upward shift", n), rep("Downward shift", n))  # replicates the stated value x10(since n=10)
mypattern

# Calculate Distance
install.packages("dtw")

library(dtw)
mydist <- dist(yrdata1, method = "DTW")
mydist


# Hierarchical clustering
hc <- hclust(mydist, method = 'average')

plot(hc)        # creates a dindogram chart with number values as labels

plot(hc, labels = mypattern, cex = 0.7, hang = -1, col = "blue") # creates a better/readable dindogram chart
  #  labels = mypattern- uses mypattern object values as labels, cex = 0.7- is the size of the labels, hang = -1 will rotate/align the labels

rect.hclust(hc, k = 4)  # creates boxes for each cluster. k can be any value



#-------------------------------------------------------------------------------------------------


                       # Time Series Analysis: Classification (with Decision Tree)

mydata <- read.table("synthetic_control.data", header = F, sep = "")  
View(mydata)  
str(mydata)

mypattern2 <- c(rep("Normal", 100), rep("Cyclic", 100), rep("Increasing trend", 100),
               rep("Decreasing trend", 100), rep("Upward shift", 100), rep("Downward shift", 100))  # replicates each the stated value x100

hisdata <- data.frame(mydata, mypattern2)  # adds mypattern2 column to the dataset
hisdata
str(hisdata)

hisdata$mypattern2 <- as.factor(hisdata$mypattern2)
str(hisdata)


# Decision Tree model
library(party)

mytree <- ctree(mypattern2 ~., hisdata)  # ctree means classification tree. 
mytree   # displays number of terminal nodes- 25, and number of branches- 49


# Prediction
mypred <- predict(mytree, hisdata)
mypred

# Classification performance
tab <- table(Predicted = mypred, Actual = hisdata$mypattern2)

# Accuracy
sum(diag(tab)) / sum(tab)

