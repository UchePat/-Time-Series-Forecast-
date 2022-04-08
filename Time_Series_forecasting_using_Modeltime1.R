
# LIBRARIES ----
install.packages("modeltime")
install.packages("glmnet")

library(modeltime)   # dis is a package centered around forecasting of time series. It integrates both classical and machine learning algorithms 
library(tidymodels)   # tidymodel and modeltime packages work together for modeling time series
library(tidyverse)    # tidyverse and timetk packages work 2geda for data wrangling and visualization of time series
library(timetk)
library(lubridate)   # dis package is a datetime library for working with dates and datetimes
library(glmnet)

# DATA ----
View(bike_sharing_daily)    # dis dataset is in timetk package

mydata <- bike_sharing_daily %>%
  select(dteday, cnt)    

mydata

mydata %>% plot_time_series(dteday, cnt)   # displays an interactive time series chart


# TRAIN / TEST SPLITS ----
?time_series_split
splits <- time_series_split(mydata, assess = "3 months",
                            cumulative = TRUE)       

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(dteday, cnt)  # displays d training ad testing interactive time-series chart



# creating FORECAST using Auto-Arima model, Prophet model and GLM machine learning algorithm----
# * AUTO ARIMA (this is an auto-regressive forecasting algorithm)----
mymodel <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(cnt ~ dteday, training(splits))   # using training dataset. cnt column is dependent variable. fit() trains d model to learn from the training dataset

mymodel


# * Prophet ----
ourmodel <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>%
  fit(cnt ~ dteday, training(splits))

ourmodel


# * GLM Machine Learning algorithm ----
yrmodel <- linear_reg(penalty = 0.01) %>%
  set_engine("glmnet") %>%
  fit(cnt ~ wday(dteday, label = TRUE) +
        month(dteday, label = TRUE) +
        as.numeric(dteday), training(splits))
    
yrmodel


# using MODELTIME package to organize and compare our models----

# * Create a Modeltime Table (this helps to organize our models)----
hismodel <- modeltime_table(mymodel, ourmodel, yrmodel)  # displays a table of d stated models
hismodel  


# * Calibration. Here we calculate predictions and residuals(error) for the test data 
mycalib <- hismodel %>%
  modeltime_calibrate(testing(splits))  # using testing data
mycalib


# * Accuracy. lets see d accuracy from our testing dataset predictions ----
mycalib %>% modeltime_accuracy()  # . mae- Mean Absolute Error(is d average error aggregated for across d prediction. d smaller d value, d better); we see that ARIMA model is d worst
# rsq- R-Squared value (Its value is from 0 to 1. the higher d value, d better): we see PROPHET is d best

 
# * Visualization of d Testing data----
mycalib %>%
  modeltime_forecast(new_data = testing(splits),
                     actual_data = mydata) %>%  # displays an interactive time series chart using d 3 models on training and testing data
  plot_modeltime_forecast()       # by hovering on d time series chart, we see that ARIMA model is not following d time series trend unlike d oda models so you can click on ARIMA in d legend bar to disable it in d chart

 
 
# * Forecast the Future ----
myforecast <- mycalib %>%
  modeltime_refit(mydata) %>%      # modeltime_refit() will re-train d 3 models on d full dataset(hence mydata and not training data) so we get d most accurate predictions in d future
  modeltime_forecast(h = "3 months", actual_data = mydata)   # we will forecast for the next 3 months

myforecast %>%
  plot_modeltime_forecast()  # by hovering on d time series chart, we can see that ARIMA model is not following d time series trend unlike d oda models so you can click on ARIMA in d legend bar to disable it in d chart
