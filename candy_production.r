
library(forecast)
library(zoo)
setwd("C:\\Users\\STSC\\Documents\\SPRING-2023\\timeseries\\project\\CandyProduction\\");
 
### Reading file and creating dataframe
candy_df <- read.csv("candy_production.csv");

candy_df

head(candy_df)


## Renaming column name


names(candy_df)[1] <- 'date'
names(candy_df)[2] <- 'production'

head(candy_df)

### Data cleaning


which(is.na(candy_df))

sum(is.na(candy_df))


prod.ts <- ts(candy_df$production, start = c(1972,1), end=c(2023,3),
               freq = 12);

prod.ts

production.stl <- stl(prod.ts, s.window = "periodic")
autoplot(production.stl, main="Candy production")


plot(prod.ts, 
     xlab = "Time", ylab = "Production (Millions)", 
     ylim = c(0, 150), main = "Candy Productions", 
     col = "blue", bty = "l", lwd = 2)
axis(1, at = seq(1972, 2023, 3), labels = format(seq(1972, 2023, 3)))
 

autocor <- Acf(prod.ts, lag.max = 12, 
               main = "Autocorrelation for candy Production Data")


Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

length(prod.ts)

##### 
# Spliting the dataset into validation and training dataset
# Spliting the dataset into 80-20 ratio of training and validation set
# Since the dataset has 615 entries assiging 120 as size of validation set
###
nValid <- 120

nTrain <- length(prod.ts) - nValid
train.ts <- window(prod.ts, start = c(1972, 1), end = c(1972, nTrain))
train.ts
valid.ts <- window(prod.ts, start = c(1972, nTrain + 1), end = c(1972, nTrain + nValid))
valid.ts

####
##
# Linear Regression model with trend
#
#

train.lin <- tslm(train.ts ~ trend)

summary(train.lin)

####
##
# Trailing MA with 4, 7 and 10 window size
#
#

ma.trailing_4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing_7 <- rollmean(train.ts, k = 7, align = "right")
ma.trailing_10 <- rollmean(train.ts, k = 10, align = "right")
head(ma.trailing_4)
tail(ma.trailing_4)
head(ma.trailing_10)
tail(ma.trailing_7)

#####
##
#
#  Trailing MA forecasting
#
#

ma.trail_4.pred <- forecast(ma.trailing_4, h = nValid, level = 0)
ma.trail_4.pred
ma.trail_7.pred <- forecast(ma.trailing_7, h = nValid, level = 0)
ma.trail_7.pred
ma.trail_10.pred <- forecast(ma.trailing_10, h = nValid, level = 0)
ma.trail_10.pred

###
##
#  Accuracy comparision on Trailing MA
#
#

round(accuracy(ma.trail_4.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_7.pred$mean, valid.ts),3)
round(accuracy(ma.trail_10.pred$mean, valid.ts), 3)

##
#
#  Regression Model - trend with seasonality for training data
# 
trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)
trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred

####
##
#  Regression Model residuals of training set
#
#
trend.seas.res <- trend.seas$residuals
trend.seas.res

####
##
# Trailing MA on the residuels of LM of trend+seasonality 
#
ma.trail.res <- rollmean(trend.seas.res, k = 4, align = "right")
ma.trail.res
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred


###
# 
#  Two level residuels forecasting
#
#
fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level
valid.df <- round(data.frame(valid.ts, trend.seas.pred$mean, 
                             ma.trail.res.pred$mean, 
                             fst.2level), 3)
names(valid.df) <- c("Production", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df

###
#
# Accuracy calculation based on validation data for 
# both trend+seasonality linear model and 2level model(ma+lm)
#
round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(fst.2level, valid.ts), 3)
 

####
##
#
#  LM model on whole dataset on trend and seasonality
#
#
tot.trend.seas <- tslm(prod.ts ~ trend + season)
summary(tot.trend.seas)
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, level = 0)
tot.trend.seas.pred

###
# Residuels
#
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 4, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12.df <- data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                          tot.fst.2level)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df

 
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
product.snaive.pred <- snaive(train.ts, h = nValid)
product.snaive.pred
round(accuracy(product.snaive.pred$mean, valid.ts), 3)
round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(fst.2level, valid.ts), 3)



# Create simple exponential smoothing (SES)with alpha = 0.2.
# Use ets() function with model = "ANN", i.e., additive error(A), no trend (N),
# & no seasonality (N). Use alpha = 0.2 to fit SES over the original data.
ses.orig <- ets(prod.ts, model = "ANN", alpha = 0.2)
ses.orig

# Use ets() function with model = "AAN", i.e., additive error(A), 
# additive trend (A), & no seasonality (N). 
h.AAN <- ets(train.ts, model = "AAN", alpha = 0.1, beta = 0.1)
h.AAN
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ
# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred


## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for  production data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(prod.ts, model = "ZZZ")
HW.ZZZ
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ
# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = 12 , level = 0)
hw.ZZZ.pred
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred


# Identify performance measures for HW forecast.
round(accuracy(HW.ZZZ.pred$fitted, prod.ts), 3)
round(accuracy(hw.ZZZ.pred$fitted, prod.ts), 3)
round(accuracy(snaive(prod.ts)$fitted, prod.ts), 3)

