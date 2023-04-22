
library(forecast)
library(zoo)
#  setwd("C:\\Users\\STSC\\Documents\\SPRING-2023\\timeseries\\project\\");

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


production.ts <- ts(candy_df$production, start = c(1972,1),end=c(2023,3),
               freq = 12);

production.ts
production.stl <- stl(production.ts, s.window = "periodic")
autoplot(production.stl, main="Candy production")

autocor <- Acf(production.ts, lag.max = 12, 
               main = "Autocorrelation for candy Production Data")


