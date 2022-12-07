# Fion Ma EMS and Weather EDA

# Load Libraries
library(stringr)
library(class)
library(ggplot2)


# Load Datasets
Weather <- read.csv("GHCND_NY_Central_Park_20080101_20161231.csv")
EMS <- read.csv("EMS_Incident_Dispatch_Data.csv")

str(EMS)
str(Weather)


# Selecting only 10305 Zipcode data from EMS 
EMS_Richmond <- EMS[EMS$BOROUGH == 'RICHMOND / STATEN ISLAND',]
EMS_Richmond_10305 <- EMS_Richmond[EMS_Richmond$ZIPCODE == '10305',]


# Splitting Date and Time into components for new EMS and Weather
EMS_Richmond_10305[c('date', 'time', 'time of day')] <- str_split_fixed(EMS_Richmond_10305$INCIDENT_DATETIME, ' ', 3)
str(EMS_Richmond_10305)
EMS_Richmond_10305[c('month', 'day', 'year')] <- str_split_fixed(EMS_Richmond_10305$date, '/', 3)
str(EMS_Richmond_10305)

table(EMS_Richmond_10305$month)
table(EMS_Richmond_10305$day)
table(EMS_Richmond_10305$year)

Weather[c('month', 'day', 'year')] <- str_split_fixed(Weather$DATE, '/', 3)
str(Weather)

#Combined_DF <- merge(x = EMS_Richmond_10305, y = Weather, by = "year")


# Selecting only 2012 data
EMS_Richmond_10305_2012 <- EMS_Richmond_10305[EMS_Richmond_10305$year == '2012',]
EMS_Richmond_10305_2012$index <- 1:nrow(EMS_Richmond_10305_2012)
str(EMS_Richmond_10305_2012)
Weather_2012 <- Weather[Weather$year == '2012',]
str(Weather_2012)


# Averaging TMIN and TMAX in Weather
Weather_2012$avg_temp <- (Weather_2012$TMAX + Weather_2012$TMIN)/2
str(Weather_2012)


# Adding average temperature to new EMS
EMS_Richmond_10305_2012$avg_temp <- 0
EMS_Richmond_10305_2012$date <- str_replace(EMS_Richmond_10305_2012$date, "0", "")
EMS_Richmond_10305_2012$date <- str_replace(EMS_Richmond_10305_2012$date, "0", "")
EMS_Richmond_10305_2012$date <- str_replace(EMS_Richmond_10305_2012$date, "212", "2012")


for (row in 1:nrow(EMS_Richmond_10305_2012)){
  ems_date <- EMS_Richmond_10305_2012[row, "date"]
  weather_row <- Weather_2012[Weather_2012$DATE == ems_date,]
  EMS_Richmond_10305_2012[row, "avg_temp"] <- weather_row["avg_temp"]
}

str(EMS_Richmond_10305_2012)


#EDA
hist(as.double(EMS_Richmond_10305_2012$month), main = 'Histogram of EMS Month Variable for 2012')
hist(as.double(EMS_Richmond_10305_2012$day), main = 'Histogram of EMS Day Variable for 2012')
hist(as.double(EMS_Richmond_10305_2012$year), main = 'Histogram of EMS Year Variable for 2012')
hist(EMS_Richmond_10305_2012$avg_temp, main = 'Histogram of EMS Avg Temp Variable for 2012')

plot(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY, main = 'Scatterplot of Response Times')
plot(EMS_Richmond_10305_2012$INCIDENT_TRAVEL_TM_SECONDS_QY, main = 'Scatterplot of Travel Times')
plot(EMS_Richmond_10305_2012$avg_temp, main = 'Scatterplot of Average Temperatures')

hist(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY, main = 'Histogram of Response Times')
hist(EMS_Richmond_10305_2012$INCIDENT_TRAVEL_TM_SECONDS_QY, main = 'Histogram of Travel Times')

plot(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY ~ EMS_Richmond_10305_2012$avg_temp, pch = 19, 
     main = 'Plot of Response Times vs Average Temperatures')
plot(EMS_Richmond_10305_2012$INCIDENT_TRAVEL_TM_SECONDS_QY ~ EMS_Richmond_10305_2012$avg_temp, pch = 19,
     main = 'Plot of Travel Times vs Average Temperatures')

par(mfrow=c(1,2))
boxplot(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY, main = 'Incident Response Times')
boxplot(EMS_Richmond_10305_2012$INCIDENT_TRAVEL_TM_SECONDS_QY, main = 'Incident Travel Times')

boxplot(EMS_Richmond_10305_2012$avg_temp, main = 'Average Temperatures in 2012')


# Linear Regression
lin_response1 <- lm(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY ~ 1)
summary(lin_response1) # 426.08

lin_response_tavg <- lm(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY ~ 
                          EMS_Richmond_10305_2012$avg_temp)
summary(lin_response_tavg) # 586.429 - 2.847*avg_temp, p-value = 1.291e-05


# Logistic Regression
log_response1 <- glm(as.factor(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY) ~ 1, 
                     family="binomial")
summary(log_response1) # 4.706
exp(coefficients(log_response1)) # (Intercept) 
#    110.6087 
exp(confint(log_response1)) #     2.5 %    97.5 % 
#  83.83577 150.02121 

log_response_tavg <- glm(as.factor(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY) ~
                           EMS_Richmond_10305_2012$avg_temp, family="binomial")
summary(log_response_tavg) # 4.96248 - 0.004504*avg_temp
exp(coefficients(log_response_tavg)) # (Intercept) EMS_Richmond_10305_2012$avg_temp 
# 142.9478064                        0.9955062 
exp(confint(log_response_tavg)) #                     2.5 %     97.5 %
# (Intercept)                      52.4798123 423.786438
# EMS_Richmond_10305_2012$avg_temp  0.9782479   1.012952


# KNN
set.seed(100)
split <- sample(1:2, nrow(EMS_Richmond_10305_2012), replace=TRUE, prob=c(0.7, 0.3))
trainsample <- EMS_Richmond_10305_2012[split==1,]
testsample <- EMS_Richmond_10305_2012[split==2,]
train <- trainsample[,c("INCIDENT_RESPONSE_SECONDS_QY","avg_temp")]
test <- testsample[,c("INCIDENT_RESPONSE_SECONDS_QY","avg_temp")]

train <- train[complete.cases(train),]
test <- test[complete.cases(test),]
sqrt(3602) # round to 61
predict_response <- knn(train=train, test=test, cl=train$INCIDENT_RESPONSE_SECONDS_QY, k=61)
table(predict_response)
predict_response <- as.character(predict_response)
predict_response <- as.numeric(predict_response)

par(mfrow=c(1,2))
plot(table(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY), main = 'Incident Response Times')
plot(table(predict_response), main = 'KNN Predict Response Times')


# K-means
table(EMS_Richmond_10305_2012$COMMUNITYDISTRICT)
table(EMS_Richmond_10305_2012$POLICEPRECINCT)
ggplot(EMS_Richmond_10305_2012,aes(x = index, y = INCIDENT_RESPONSE_SECONDS_QY, 
                                   col = COMMUNITYDISTRICT)) + geom_point()
ggplot(EMS_Richmond_10305_2012,aes(x = index, y = INCIDENT_RESPONSE_SECONDS_QY, 
                                   col = POLICEPRECINCT)) + geom_point()

set.seed(100)
k.max1 <- 3
k.max2 <- 2
wss_com <- sapply(1:k.max1,function(k){kmeans(EMS_Richmond_10305_2012$COMMUNITYDISTRICT,
                                              k,nstart = 20,iter.max = 20)$tot.withinss})
wss_pol <- sapply(1:k.max2,function(k){kmeans(EMS_Richmond_10305_2012$POLICEPRECINCT,
                                              k,nstart = 20,iter.max = 20)$tot.withinss})

plot(1:k.max1,wss_com,type = "b",xlab = "Number of clusters(K)",
     ylab = "Within cluster sum of squares for COMMUNITYDISTRICT")
plot(1:k.max2,wss_pol,type = "b",xlab = "Number of clusters(K)",
     ylab = "Within cluster sum of squares for POLICEPRECINCT")

km_com = kmeans(EMS_Richmond_10305_2012$COMMUNITYDISTRICT,centers=2,nstart=20)
km_com2 = kmeans(EMS_Richmond_10305_2012$COMMUNITYDISTRICT,centers=3,nstart=20)
km_pol = kmeans(EMS_Richmond_10305_2012$POLICEPRECINCT,centers=2,nstart=20)

plot(EMS_Richmond_10305_2012$COMMUNITYDISTRICT, col=(km_com$cluster+1), 
     main="K-Means Clustering with K=2", xlab="", 
     ylab="", pch=20, cex=2)
plot(EMS_Richmond_10305_2012$COMMUNITYDISTRICT, col=(km_com2$cluster+1), 
     main="K-Means Clustering with K=3", xlab="", 
     ylab="", pch=20, cex=2)
plot(EMS_Richmond_10305_2012$POLICEPRECINCT, col=(km_pol$cluster+1), 
     main="K-Means Clustering with K=2", xlab="", 
     ylab="", pch=20, cex=2)

table(km_com$cluster,EMS_Richmond_10305_2012$COMMUNITYDISTRICT)
#      501  502  595
#   1 1629 3892    0
#   2    0    0   98
table(km_com2$cluster,EMS_Richmond_10305_2012$COMMUNITYDISTRICT)
#      501  502  595
#   1    0    0   98
#   2    0 3892    0
#   3 1629    0    0

table(km_pol$cluster,EMS_Richmond_10305_2012$POLICEPRECINCT)
#      120  122
#   1 1655    0
#   2    0 3964




table(EMS_Richmond_10305_2012$avg_temp)
cat_avgtemp <- cut(EMS_Richmond_10305_2012$avg_temp, br=c(20,45,86.5), 
                   labels=c('cold_temp','hot_temp'))
queenssales <- as.factor(queenssales)
summary(cat_avgtemp)
table(cat_avgtemp)
colnames(EMS_Richmond_10305_2012)
