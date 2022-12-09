# Fion Ma EMS and Weather EDA

# Load Libraries
library(stringr)
library(class)
library(ggplot2)
library(caret)


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

table(Weather$month)
table(Weather$day)
table(Weather$year)

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
summary(EMS_Richmond_10305_2012$avg_temp)

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
summary(lin_response_tavg) # 586.429 - 2.847*avg_temp, p-value = 1.291e-05, st.dev. = 0.652

lin_response2 <- lm(EMS_Richmond_10305_2012$INCIDENT_TRAVEL_TM_SECONDS_QY ~ 1)
summary(lin_response2) # 383.8

lin_response_tavg2 <- lm(EMS_Richmond_10305_2012$INCIDENT_TRAVEL_TM_SECONDS_QY ~ 
                           EMS_Richmond_10305_2012$avg_temp)
summary(lin_response_tavg2) # 528.1238 - 2.5623*avg_temp, p-value = 0.00161, st.dev. = 0.8119

# Logistic Regression
summary(EMS_Richmond_10305_2012$INCIDENT_TRAVEL_TM_SECONDS_QY)
res_times <- cut(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY, br=c(0,344,30831), 
                 labels=c('fast','slow'))
res_times <- as.factor(res_times)
summary(res_times) # fast slow NA's 
# 2528 2560  531 

trav_times <- cut(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY, br=c(0,319,61418), 
                  labels=c('fast','slow'))
trav_times <- as.factor(trav_times)
summary(trav_times) # fast slow NA's 
# 2242 2846  531

log_response1 <- glm(res_times ~ EMS_Richmond_10305_2012$avg_temp, family="binomial")
summary(log_response1) # 0.159498 - 0.002609*avg_temp, p-value = 0.120
exp(coefficients(log_response1)) # (Intercept) EMS_Richmond_10305_2012$avg_temp 
#  1.1729214                        0.9973946 
exp(confint(log_response1)) #                     2.5 %     97.5 %
# (Intercept)                      0.9670647 1.422830
# EMS_Richmond_10305_2012$avg_temp 0.9941212 1.000676

log_response2 <- glm(trav_times ~ EMS_Richmond_10305_2012$avg_temp, family="binomial")
summary(log_response2) # 0.335946 - 0.001729*avg_temp, p-value = 0.305747
exp(coefficients(log_response2)) # (Intercept) EMS_Richmond_10305_2012$avg_temp 
# 1.3992636                        0.9982728
exp(confint(log_response2)) #          2.5 %     97.5 %
# (Intercept)                      1.1521249 1.700154
# EMS_Richmond_10305_2012$avg_temp 0.9949747 1.001580

# KNN
set.seed(100)
split <- sample(1:2, nrow(EMS_Richmond_10305_2012), replace=TRUE, prob=c(0.7, 0.3))
trainsample <- EMS_Richmond_10305_2012[split==1,]
testsample <- EMS_Richmond_10305_2012[split==2,]
train <- trainsample[,c("INCIDENT_RESPONSE_SECONDS_QY","avg_temp")]
test <- testsample[,c("INCIDENT_RESPONSE_SECONDS_QY","avg_temp")]
train2 <- trainsample[,c("INCIDENT_TRAVEL_TM_SECONDS_QY","avg_temp")]
test2 <- testsample[,c("INCIDENT_TRAVEL_TM_SECONDS_QY","avg_temp")]

train <- train[complete.cases(train),]
test <- test[complete.cases(test),]
train2 <- train2[complete.cases(train2),]
test2 <- test2[complete.cases(test2),]


sqrt(3602) # round to 61
sqrt(3604) # round to 61
predict_response <- knn(train=train, test=test, cl=train$INCIDENT_RESPONSE_SECONDS_QY, k=61)
table(predict_response)
predict_response <- as.character(predict_response)
predict_response <- as.numeric(predict_response)
predict_response2 <- knn(train=train2, test=test2, cl=train2$INCIDENT_TRAVEL_TM_SECONDS_QY, k=61)
table(predict_response2)
predict_response2 <- as.character(predict_response2)
predict_response2 <- as.numeric(predict_response2)

par(mfrow=c(1,2))
plot(table(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY), main = 'Incident Response Times')
plot(table(predict_response), main = 'KNN Predict Response Times')
plot(table(EMS_Richmond_10305_2012$INCIDENT_TRAVEL_TM_SECONDS_QY), main = 'Incident Travel Times')
plot(table(predict_response2), main = 'KNN Predict Travel Times')


# K-means
table(EMS_Richmond_10305_2012$COMMUNITYDISTRICT)
table(EMS_Richmond_10305_2012$POLICEPRECINCT)
EMS_Richmond_10305_2012 <- EMS_Richmond_10305_2012[complete.cases(EMS_Richmond_10305_2012),]
ggplot(EMS_Richmond_10305_2012,aes(x = index, y = INCIDENT_RESPONSE_SECONDS_QY, 
                                   col = COMMUNITYDISTRICT)) + geom_point()
ggplot(EMS_Richmond_10305_2012,aes(x = index, y = INCIDENT_RESPONSE_SECONDS_QY, 
                                   col = POLICEPRECINCT)) + geom_point()
ggplot(EMS_Richmond_10305_2012,aes(x = index, y = INCIDENT_TRAVEL_TM_SECONDS_QY, 
                                   col = COMMUNITYDISTRICT)) + geom_point()
ggplot(EMS_Richmond_10305_2012,aes(x = index, y = INCIDENT_TRAVEL_TM_SECONDS_QY, 
                                   col = POLICEPRECINCT)) + geom_point()

set.seed(100)
k.max <- 6
wss_res <- sapply(1:k.max,function(k){kmeans(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY,
                                             k,nstart = 20,iter.max = 20)$tot.withinss})
wss_trav <- sapply(1:k.max,function(k){kmeans(EMS_Richmond_10305_2012$INCIDENT_TRAVEL_TM_SECONDS_QY,
                                              k,nstart = 20,iter.max = 20)$tot.withinss})

plot(1:k.max,wss_res,type = "b",xlab = "Number of clusters(K)",
     ylab = "WSS for Incident Response Times")
plot(1:k.max,wss_trav,type = "b",xlab = "Number of clusters(K)",
     ylab = "WSS for Incident Travel Times")

km_res = kmeans(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY,centers=2,nstart=20)
km_trav = kmeans(EMS_Richmond_10305_2012$INCIDENT_TRAVEL_TM_SECONDS_QY,centers=2,nstart=20)

plot(EMS_Richmond_10305_2012$INCIDENT_RESPONSE_SECONDS_QY, col=(km_res$cluster+1), 
     main="K-Means with K=2 for Incident Response Times", xlab="", 
     ylab="", pch=20, cex=2)
plot(EMS_Richmond_10305_2012$INCIDENT_TRAVEL_TM_SECONDS_QY, col=(km_trav$cluster+1), 
     main="K-Means with K=2 for Incident Travel Times", xlab="", 
     ylab="", pch=20, cex=2)

table(km_res$cluster,EMS_Richmond_10305_2012$COMMUNITYDISTRICT)
#    501  502  595
# 1 1584 3442   94
# 2    1    4    0
table(km_res$cluster,EMS_Richmond_10305_2012$POLICEPRECINCT)
#    120  122
# 1 1609 3511
# 2    1    4
table(km_trav$cluster,EMS_Richmond_10305_2012$COMMUNITYDISTRICT)
#    501  502  595
# 1    0    1    0
# 2 1585 3445   94
table(km_trav$cluster,EMS_Richmond_10305_2012$POLICEPRECINCT)
#    120  122
# 1    0    1
# 2 1610 3514



