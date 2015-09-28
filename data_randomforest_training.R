

#_____________________________________________________________________________________________________________________________
#dataset splitting
temp <- t
summary(temp$network_type)
summary(temp[which(temp$network_type == "Cell Radio Off"),]$download_speed)
summary(temp[which(temp$network_type == "Cell Radio Off"),]$upload_speed)
summary(temp[which(temp$network_type == "Emergency Calls Only"),]$download_speed)
summary(temp[which(temp$network_type == "Emergency Calls Only"),]$upload_speed)
summary(temp[which(temp$network_type == "iDEN"),]$download_speed)
summary(temp[which(temp$network_type == "iDEN"),]$upload_speed)
summary(temp[which(temp$network_type == "Out of service"),]$download_speed)
summary(temp[which(temp$network_type == "Out of service"),]$upload_speed)
summary(temp[which(temp$network_type == "Out of Service"),]$download_speed)
summary(temp[which(temp$network_type == "Out of Service"),]$upload_speed)
summary(temp[which(is.na(temp$network_type)),]$download_speed)
summary(temp[which(is.na(temp$network_type)),]$upload_speed)


#_____________________________________________________________________________________________________________________________
# by NW type

temp$nw_type <- NA
temp[which(temp$network_type == "LTE"),]$nw_type <- "LTE"
temp[which(temp$network_type == "1xRTT" | temp$network_type == "eHRPD" | temp$network_type == "EVDO 0" | 
                   temp$network_type == "EVDO A" | temp$network_type == "EVDO B" | temp$network_type == "CDMA"),]$nw_type <- "CDMA"
temp[which(temp$network_type == "EDGE" | temp$network_type == "GPRS"),]$nw_type <- "GSM"
temp[which(temp$network_type == "HSDPA" | temp$network_type == "HSUPA" | temp$network_type == "HSPA" | 
                   temp$network_type == "HSPAP" | temp$network_type == "UMTS"),]$nw_type <- "UMTS"
temp$nw_type <- factor(temp$nw_type)
summary(temp$nw_type)




#_____________________________________________________________________________________________________________________________
# LTE RANDOMFOREST

library(caret)
library(dplyr)
library(randomForest)
set.seed(16)
temp.lte <- temp[which(temp$nw_type == "LTE"),]

# LTE DL RANDOMFOREST

summary(temp.lte$download_speed)
# remove NAs in download_speed column
temp.lte.d <- temp.lte[which(!is.na(temp.lte$download_speed)),]
summary(temp.lte.d$download_speed)
temp.lte.d$new_year <- factor(temp.lte.d$new_year)
temp.lte.d$new_month <- factor(temp.lte.d$new_month)
temp.lte.d$new_weekday <- factor(temp.lte.d$new_weekday)
str(temp.lte.d)
# create train and test sets
trainIndex <- createDataPartition(temp.lte.d$download_speed, p = .8, list = FALSE)
temp.lte.d.train <- temp.lte.d[trainIndex,]
temp.lte.d.test <- temp.lte.d[-trainIndex,]
rf.lte.d <- randomForest(download_speed ~ new_month + new_year + new_weekday
                         + new_network_country + network_id + network_id_sim + rsrp + rsrq + rssnr
                         + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                         temp.lte.d.train, importance=TRUE, ntree = 100, na.action=na.roughfix)
Sys.time()
print(rf.lte.d)
importance(rf.lte.d)
plot(rf.lte.d)
#prediction
temp.lte.d.test.prediction <- cbind(predict(rf.lte.d, temp.lte.d.test), temp.lte.d.test$download_speed)
summary(temp.lte.d.test.prediction)
temp.lte.d.test.prediction <- na.omit(temp.lte.d.test.prediction)
summary(temp.lte.d.test.prediction)
rsq.lte.d <- 1 - sum((temp.lte.d.test.prediction[,2] - temp.lte.d.test.prediction[,1])^2) / 
        sum((temp.lte.d.test.prediction[,2] - mean(temp.lte.d.test.prediction[,2]))^2)
rmsle.lte.d <- sqrt(1 / nrow(temp.lte.d.test.prediction) * 
                            sum((log1p(temp.lte.d.test.prediction[,1]) - 
                                         log1p(temp.lte.d.test.prediction[,2]))^2))
mse.lte.d <- sqrt(sum((temp.lte.d.test.prediction[,1] - 
                               temp.lte.d.test.prediction[,2])^2) / nrow(temp.lte.d.test.prediction))

# LTE UL RANDOMFOREST

summary(temp.lte$upload_speed)
# remove NAs in upload_speed column
temp.lte.u <- temp.lte[which(!is.na(temp.lte$upload_speed)),]
summary(temp.lte.u$upload_speed)
temp.lte.u$new_year <- factor(temp.lte.u$new_year)
temp.lte.u$new_month <- factor(temp.lte.u$new_month)
temp.lte.u$new_weekday <- factor(temp.lte.u$new_weekday)
# create train and test sets
trainIndex <- createDataPartition(temp.lte.u$upload_speed, p = .8, list = FALSE)
temp.lte.u.train <- temp.lte.u[trainIndex,]
temp.lte.u.test <- temp.lte.u[-trainIndex,]
Sys.time()
rf.lte.u <- randomForest(upload_speed ~ new_month + new_year + new_weekday
                         + new_network_country + network_id + network_id_sim + rsrp + rsrq + rssnr
                         + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                         temp.lte.u.train, importance=TRUE, ntree = 100, na.action=na.roughfix)
Sys.time()
print(rf.lte.u)
importance(rf.lte.u)
plot(rf.lte.u)
#prediction
temp.lte.u.test.prediction <- cbind(predict(rf.lte.u, temp.lte.u.test), temp.lte.u.test$upload_speed)
summary(temp.lte.u.test.prediction)
temp.lte.u.test.prediction <- na.omit(temp.lte.u.test.prediction)
summary(temp.lte.u.test.prediction)
rsq.lte.u <- 1 - sum((temp.lte.u.test.prediction[,2] - temp.lte.u.test.prediction[,1])^2) / 
        sum((temp.lte.u.test.prediction[,2] - mean(temp.lte.u.test.prediction[,2]))^2)
rmsle.lte.u <- sqrt(1 / nrow(temp.lte.u.test.prediction) * 
                            sum((log1p(temp.lte.u.test.prediction[,1]) - 
                                         log1p(temp.lte.u.test.prediction[,2]))^2))
mse.lte.u <- sqrt(sum((temp.lte.u.test.prediction[,1] - 
                               temp.lte.u.test.prediction[,2])^2) / nrow(temp.lte.u.test.prediction))



#_____________________________________________________________________________________________________________________________
# CDMA RANDOMFOREST

library(caret)
library(dplyr)
library(randomForest)
set.seed(16)
temp.cdma <- temp[which(temp$nw_type == "CDMA"),]

# CDMA DL RANDOMFOREST

summary(temp.cdma$download_speed)
# remove NAs in download_speed column
temp.cdma.d <- temp.cdma[which(!is.na(temp.cdma$download_speed)),]
summary(temp.cdma.d$download_speed)
#factorize some columls
temp.cdma.d$new_year <- factor(temp.cdma.d$new_year)
temp.cdma.d$new_month <- factor(temp.cdma.d$new_month)
temp.cdma.d$new_weekday <- factor(temp.cdma.d$new_weekday)
summary(temp.cdma.d$new_network_country)
temp.cdma.d$new_network_country <- factor(temp.cdma.d$new_network_country)
summary(temp.cdma.d$new_network_country)
str(temp.cdma.d$new_network_country)
levels(temp.cdma.d$new_network_country)
str(temp.cdma.d$app_version_code)
# create train and test sets
trainIndex <- createDataPartition(temp.cdma.d$download_speed, p = .8, list = FALSE)
temp.cdma.d.train <- temp.cdma.d[trainIndex,]
temp.cdma.d.test <- temp.cdma.d[-trainIndex,]
rf.cdma.d <- randomForest(download_speed ~ new_month + new_year + new_weekday +
                                  new_network_country + network_id + ec_io + network_id_sim + test_type +
                                  icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                          temp.cdma.d.train, importance = TRUE, ntree = 50, na.action = na.roughfix)
Sys.time()
print(rf.cdma.d)
importance(rf.cdma.d)
plot(rf.cdma.d)
#prediction
temp.cdma.d.test.prediction <- cbind(predict(rf.cdma.d, temp.cdma.d.test), temp.cdma.d.test$download_speed)
summary(temp.cdma.d.test.prediction)
temp.cdma.d.test.prediction <- na.omit(temp.cdma.d.test.prediction)
summary(temp.cdma.d.test.prediction)
rsq.cdma.d <- 1 - sum((temp.cdma.d.test.prediction[,2] - temp.cdma.d.test.prediction[,1])^2) / 
        sum((temp.cdma.d.test.prediction[,2] - mean(temp.cdma.d.test.prediction[,2]))^2)
rmsle.cdma.d <- sqrt(1 / nrow(temp.cdma.d.test.prediction) * 
                             sum((log1p(temp.cdma.d.test.prediction[,1]) - 
                                          log1p(temp.cdma.d.test.prediction[,2]))^2))
mse.cdma.d <- sqrt(sum((temp.cdma.d.test.prediction[,1] - 
                                temp.cdma.d.test.prediction[,2])^2) / nrow(temp.cdma.d.test.prediction))


# CDMA UL RANDOMFOREST

summary(temp.cdma$upload_speed)
# remove NAs in upload_speed column
temp.cdma.u <- temp.cdma[which(!is.na(temp.cdma$upload_speed)),]
summary(temp.cdma.u$upload_speed)
temp.cdma.u$new_year <- factor(temp.cdma.u$new_year)
temp.cdma.u$new_month <- factor(temp.cdma.u$new_month)
temp.cdma.u$new_weekday <- factor(temp.cdma.u$new_weekday)
summary(temp.cdma.u$new_network_country)
temp.cdma.u$new_network_country <- factor(temp.cdma.u$new_network_country)
summary(temp.cdma.u$new_network_country)
str(temp.cdma.u$new_network_country)
levels(temp.cdma.u$new_network_country)
str(temp.cdma.u$app_version_code)
# create train and test sets
trainIndex <- createDataPartition(temp.cdma.u$upload_speed, p = .8, list = FALSE)
temp.cdma.u.train <- temp.cdma.u[trainIndex,]
temp.cdma.u.test <- temp.cdma.u[-trainIndex,]
rf.cdma.u <- randomForest(upload_speed ~ new_month + new_year + new_weekday +
                                  new_network_country + network_id + ec_io + network_id_sim + test_type +
                                  icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                          temp.cdma.u.train, importance = TRUE, ntree = 50, na.action = na.roughfix)
Sys.time()
print(rf.cdma.u)
importance(rf.cdma.u)
plot(rf.cdma.u)
#prediction
temp.cdma.u.test.prediction <- cbind(predict(rf.cdma.u, temp.cdma.u.test), temp.cdma.u.test$upload_speed)
summary(temp.cdma.u.test.prediction)
temp.cdma.u.test.prediction <- na.omit(temp.cdma.u.test.prediction)
summary(temp.cdma.u.test.prediction)
rsq.cdma.u <- 1 - sum((temp.cdma.u.test.prediction[,2] - temp.cdma.u.test.prediction[,1])^2) / 
        sum((temp.cdma.u.test.prediction[,2] - mean(temp.cdma.u.test.prediction[,2]))^2)
rmsle.cdma.u <- sqrt(1 / nrow(temp.cdma.u.test.prediction) * 
                             sum((log1p(temp.cdma.u.test.prediction[,1]) - 
                                          log1p(temp.cdma.u.test.prediction[,2]))^2))
mse.cdma.u <- sqrt(sum((temp.cdma.u.test.prediction[,1] - 
                                temp.cdma.u.test.prediction[,2])^2) / nrow(temp.cdma.u.test.prediction))


#_____________________________________________________________________________________________________________________________
# UMTS RANDOMFOREST

library(caret)
library(dplyr)
library(randomForest)
set.seed(16)
temp.umts <- temp[which(temp$nw_type == "UMTS"),]

# UMTS DL RANDOMFOREST

summary(temp.umts$download_speed)
# remove NAs in download_speed column
temp.umts.d <- temp.umts[which(!is.na(temp.umts$download_speed)),]
summary(temp.umts.d$download_speed)
#factorize some columls
temp.umts.d$new_year <- factor(temp.umts.d$new_year)
temp.umts.d$new_month <- factor(temp.umts.d$new_month)
temp.umts.d$new_weekday <- factor(temp.umts.d$new_weekday)
str(temp.cdma.d$app_version_code)
# create train and test sets
trainIndex <- createDataPartition(temp.umts.d$download_speed, p = .8, list = FALSE)
temp.umts.d.train <- temp.umts.d[trainIndex,]
temp.umts.d.test <- temp.umts.d[-trainIndex,]
rf.umts.d <- randomForest(download_speed ~ new_month + new_year + new_weekday +
                                  new_network_country + network_id + rssi + network_id_sim + test_type +
                                  icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                          temp.umts.d.train, importance = TRUE, ntree = 50, na.action = na.roughfix)
Sys.time()
print(rf.umts.d)
importance(rf.umts.d)
plot(rf.umts.d)
#prediction
temp.umts.d.test.prediction <- cbind(predict(rf.umts.d, temp.umts.d.test), temp.umts.d.test$download_speed)
summary(temp.umts.d.test.prediction)
temp.umts.d.test.prediction.na <- na.omit(temp.umts.d.test.prediction)
summary(temp.umts.d.test.prediction.na)
rsq.umts.d <- 1 - sum((temp.umts.d.test.prediction[,2] - temp.umts.d.test.prediction[,1])^2) / 
        sum((temp.umts.d.test.prediction[,2] - mean(temp.umts.d.test.prediction[,2]))^2)
rmsle.umts.d <- sqrt(1 / nrow(temp.umts.d.test.prediction) * 
                             sum((log1p(temp.umts.d.test.prediction[,1]) - 
                                          log1p(temp.umts.d.test.prediction[,2]))^2))
mse.umts.d <- sqrt(sum((temp.umts.d.test.prediction[,1] - 
                                temp.umts.d.test.prediction[,2])^2) / nrow(temp.umts.d.test.prediction))


# UMTS UL RANDOMFOREST

summary(temp.umts$upload_speed)
# remove NAs in upload_speed column
temp.umts.u <- temp.umts[which(!is.na(temp.umts$upload_speed)),]
summary(temp.umts.u$upload_speed)
temp.umts.u$new_year <- factor(temp.umts.u$new_year)
temp.umts.u$new_month <- factor(temp.umts.u$new_month)
temp.umts.u$new_weekday <- factor(temp.umts.u$new_weekday)
str(temp.umts.u$app_version_code)
# create train and test sets
trainIndex <- createDataPartition(temp.umts.u$upload_speed, p = .8, list = FALSE)
temp.umts.u.train <- temp.umts.u[trainIndex,]
temp.umts.u.test <- temp.umts.u[-trainIndex,]
Sys.time()
rf.umts.u <- randomForest(upload_speed ~ new_month + new_year + new_weekday +
                                  new_network_country + network_id + rssi + network_id_sim + test_type +
                                  icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                          temp.umts.u.train, importance = TRUE, ntree = 50, na.action = na.roughfix)
Sys.time()
print(rf.umts.u)
importance(rf.umts.u)
plot(rf.umts.u)
#prediction
temp.umts.u.test.prediction <- cbind(predict(rf.umts.u, 
                                             na.roughfix(select(temp.umts.u.test, upload_speed, new_month,
                                                                new_year,new_weekday, new_network_country, network_id, rssi, 
                                                                network_id_sim, test_type, icmp_ping_time, icmp_ping_packet_loss,
                                                                icmp_ping_range, app_version_code))), temp.umts.u.test$upload_speed)
summary(temp.umts.u.test.prediction)
temp.umts.u.test.prediction <- na.omit(temp.umts.u.test.prediction)
summary(temp.umts.u.test.prediction)
rsq.umts.u <- 1 - sum((temp.umts.u.test.prediction[,2] - temp.umts.u.test.prediction[,1])^2) / 
        sum((temp.umts.u.test.prediction[,2] - mean(temp.umts.u.test.prediction[,2]))^2)
rmsle.umts.u <- sqrt(1 / nrow(temp.umts.u.test.prediction) * 
                             sum((log1p(temp.umts.u.test.prediction[,1]) - 
                                          log1p(temp.umts.u.test.prediction[,2]))^2))
mse.umts.u <- sqrt(sum((temp.umts.u.test.prediction[,1] - 
                                temp.umts.u.test.prediction[,2])^2) / nrow(temp.umts.u.test.prediction))


#_____________________________________________________________________________________________________________________________
# GSM RANDOMFOREST

library(caret)
library(dplyr)
library(randomForest)
set.seed(16)
temp.gsm <- temp[which(temp$nw_type == "GSM"),]

# GSM DL RANDOMFOREST

summary(temp.gsm$download_speed)
# remove NAs in download_speed column
temp.gsm.d <- temp.gsm[which(!is.na(temp.gsm$download_speed)),]
summary(temp.gsm.d$download_speed)
#factorize some columls
temp.gsm.d$new_year <- factor(temp.gsm.d$new_year)
temp.gsm.d$new_month <- factor(temp.gsm.d$new_month)
temp.gsm.d$new_weekday <- factor(temp.gsm.d$new_weekday)
summary(temp.gsm.d$new_network_country)
length(table(temp.gsm.d$new_network_country))
str(temp.gsm.d$app_version_code)
# create train and test sets
trainIndex <- createDataPartition(temp.gsm.d$download_speed, p = .8, list = FALSE)
temp.gsm.d.train <- temp.gsm.d[trainIndex,]
temp.gsm.d.test <- temp.gsm.d[-trainIndex,]
Sys.time()
rf.gsm.d <- randomForest(download_speed ~ new_month + new_year + new_weekday +
                                 new_network_country + network_id + rssi + network_id_sim + test_type +
                                 icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                         temp.gsm.d.train, importance = TRUE, ntree = 500, na.action = na.roughfix)
Sys.time()
print(rf.gsm.d)
importance(rf.gsm.d)
plot(rf.gsm.d)
#prediction
temp.gsm.d.test.prediction <- cbind(predict(rf.gsm.d, temp.gsm.d.test), temp.gsm.d.test$download_speed)
summary(temp.gsm.d.test.prediction)
temp.gsm.d.test.prediction <- na.omit(temp.gsm.d.test.prediction)
summary(temp.gsm.d.test.prediction)
rsq.gsm.d <- 1 - sum((temp.gsm.d.test.prediction[,2] - temp.gsm.d.test.prediction[,1])^2) / 
        sum((temp.gsm.d.test.prediction[,2] - mean(temp.gsm.d.test.prediction[,2]))^2)
rmsle.gsm.d <- sqrt(1 / nrow(temp.gsm.d.test.prediction) * 
                            sum((log1p(temp.gsm.d.test.prediction[,1]) - 
                                         log1p(temp.gsm.d.test.prediction[,2]))^2))
mse.gsm.d <- sqrt(sum((temp.gsm.d.test.prediction[,1] - 
                               temp.gsm.d.test.prediction[,2])^2) / nrow(temp.gsm.d.test.prediction))


# GSM UL RANDOMFOREST

summary(temp.gsm$upload_speed)
# remove NAs in upload_speed column
temp.gsm.u <- temp.gsm[which(!is.na(temp.gsm$upload_speed)),]
summary(temp.gsm.u$upload_speed)
temp.gsm.u$new_year <- factor(temp.gsm.u$new_year)
temp.gsm.u$new_month <- factor(temp.gsm.u$new_month)
temp.gsm.u$new_weekday <- factor(temp.gsm.u$new_weekday)
str(temp.gsm.u$app_version_code)
# create train and test sets
trainIndex <- createDataPartition(temp.gsm.u$upload_speed, p = .8, list = FALSE)
temp.gsm.u.train <- temp.gsm.u[trainIndex,]
temp.gsm.u.test <- temp.gsm.u[-trainIndex,]
Sys.time()
rf.gsm.u <- randomForest(upload_speed ~ new_month + new_year + new_weekday +
                                 new_network_country + network_id + rssi + network_id_sim + test_type +
                                 icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                         temp.gsm.u.train, importance = TRUE, ntree = 100, na.action = na.roughfix)
Sys.time()
print(rf.gsm.u)
importance(rf.gsm.u)
plot(rf.gsm.u)
#prediction
temp.gsm.u.test.prediction <- cbind(predict(rf.gsm.u, temp.gsm.u.test), temp.gsm.u.test$upload_speed)
summary(temp.gsm.u.test.prediction)
temp.gsm.u.test.prediction <- na.omit(temp.gsm.u.test.prediction)
summary(temp.gsm.u.test.prediction)
rsq.gsm.u <- 1 - sum((temp.gsm.u.test.prediction[,2] - temp.gsm.u.test.prediction[,1])^2) / 
        sum((temp.gsm.u.test.prediction[,2] - mean(temp.gsm.u.test.prediction[,2]))^2)
rmsle.gsm.u <- sqrt(1 / nrow(temp.gsm.u.test.prediction) * 
                            sum((log1p(temp.gsm.u.test.prediction[,1]) - 
                                         log1p(temp.gsm.u.test.prediction[,2]))^2))
mse.gsm.u <- sqrt(sum((temp.gsm.u.test.prediction[,1] - 
                               temp.gsm.u.test.prediction[,2])^2) / nrow(temp.gsm.u.test.prediction))































# UMTS DL RANDOMFOREST

summary(temp.umts$download_speed)
# remove NAs in download_speed column
temp.umts.d <- temp.umts[which(!is.na(temp.umts$download_speed)),]
summary(temp.umts.d$download_speed)
#factorize some columls
temp.umts.d$new_year <- factor(temp.umts.d$new_year)
temp.umts.d$new_month <- factor(temp.umts.d$new_month)
temp.umts.d$new_weekday <- factor(temp.umts.d$new_weekday)
str(temp.cdma.d$app_version_code)
# create train and test sets
trainIndex <- createDataPartition(temp.umts.d$download_speed, p = .8, list = FALSE)
temp.umts.d.train <- temp.umts.d[trainIndex,]
temp.umts.d.test <- temp.umts.d[-trainIndex,]
rf.umts.d <- randomForest(download_speed ~ new_month + new_year + new_weekday +
                                  new_network_country + network_id + rssi + network_id_sim + test_type +
                                  icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                          temp.umts.d.train, importance = TRUE, ntree = 50, na.action = na.roughfix)
Sys.time()
print(rf.umts.d)
importance(rf.umts.d)
plot(rf.umts.d)
#prediction
temp.umts.d.test.prediction <- cbind(predict(rf.umts.d, temp.umts.d.test), temp.umts.d.test$download_speed)
summary(temp.umts.d.test.prediction)
temp.umts.d.test.prediction.na <- na.omit(temp.umts.d.test.prediction)
summary(temp.umts.d.test.prediction.na)
rsq.umts.d <- 1 - sum((temp.umts.d.test.prediction[,2] - temp.umts.d.test.prediction[,1])^2) / 
        sum((temp.umts.d.test.prediction[,2] - mean(temp.umts.d.test.prediction[,2]))^2)
rmsle.umts.d <- sqrt(1 / nrow(temp.umts.d.test.prediction) * 
                             sum((log1p(temp.umts.d.test.prediction[,1]) - 
                                          log1p(temp.umts.d.test.prediction[,2]))^2))
mse.umts.d <- sqrt(sum((temp.umts.d.test.prediction[,1] - 
                                temp.umts.d.test.prediction[,2])^2) / nrow(temp.umts.d.test.prediction))


# UMTS UL RANDOMFOREST

summary(temp.umts$upload_speed)
# remove NAs in upload_speed column
temp.umts.u <- temp.umts[which(!is.na(temp.umts$upload_speed)),]
summary(temp.umts.u$upload_speed)
temp.umts.u$new_year <- factor(temp.umts.u$new_year)
temp.umts.u$new_month <- factor(temp.umts.u$new_month)
temp.umts.u$new_weekday <- factor(temp.umts.u$new_weekday)
str(temp.umts.u$app_version_code)
# create train and test sets
trainIndex <- createDataPartition(temp.umts.u$upload_speed, p = .8, list = FALSE)
temp.umts.u.train <- temp.umts.u[trainIndex,]
temp.umts.u.test <- temp.umts.u[-trainIndex,]
Sys.time()
rf.umts.u <- randomForest(upload_speed ~ new_month + new_year + new_weekday +
                                  new_network_country + network_id + rssi + network_id_sim + test_type +
                                  icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                          temp.umts.u.train, importance = TRUE, ntree = 50, na.action = na.roughfix)
Sys.time()
print(rf.umts.u)
importance(rf.umts.u)
plot(rf.umts.u)
#prediction
temp.umts.u.test.prediction <- cbind(predict(rf.umts.u, 
                                             na.roughfix(select(temp.umts.u.test, upload_speed, new_month,
                                                                new_year,new_weekday, new_network_country, network_id, rssi, 
                                                                network_id_sim, test_type, icmp_ping_time, icmp_ping_packet_loss,
                                                                icmp_ping_range, app_version_code))), temp.umts.u.test$upload_speed)
summary(temp.umts.u.test.prediction)
temp.umts.u.test.prediction <- na.omit(temp.umts.u.test.prediction)
summary(temp.umts.u.test.prediction)
rsq.umts.u <- 1 - sum((temp.umts.u.test.prediction[,2] - temp.umts.u.test.prediction[,1])^2) / 
        sum((temp.umts.u.test.prediction[,2] - mean(temp.umts.u.test.prediction[,2]))^2)
rmsle.umts.u <- sqrt(1 / nrow(temp.umts.u.test.prediction) * 
                             sum((log1p(temp.umts.u.test.prediction[,1]) - 
                                          log1p(temp.umts.u.test.prediction[,2]))^2))
mse.umts.u <- sqrt(sum((temp.umts.u.test.prediction[,1] - 
                                temp.umts.u.test.prediction[,2])^2) / nrow(temp.umts.u.test.prediction))

