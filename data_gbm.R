setwd("C:\\freelancervvp\\Projects\\2015_Data_Mining\\new_data")
load("temp")
#t <- read.csv("training_set_all_fields.csv")


#temp nw_type

summary(temp$nw_type)
temp$nw_type <- NA
temp[which(temp$network_type == "LTE"),]$nw_type <- "LTE"

temp[which(temp$network_type == "1xRTT" | temp$network_type == "eHRPD" | temp$network_type == "EVDO 0" | 
             temp$network_type == "EVDO A" | temp$network_type == "EVDO B" | temp$network_type == "CDMA"),]$nw_type <- "CDMA"

temp[which(temp$network_type == "EDGE" | temp$network_type == "GPRS"),]$nw_type <- "GSM"

temp[which(temp$network_type == "HSDPA"),]$nw_type <- "HSDPA"
temp[which(temp$network_type == "HSPA"),]$nw_type <- "HSPA"
temp[which(temp$network_type == "HSPAP"),]$nw_type <- "HSPAP"
temp[which(temp$network_type == "HSUPA"),]$nw_type <- "UMTS"
temp[which(temp$network_type == "UMTS"),]$nw_type <- "UMTS"

temp[which(temp$network_type == "Out of service"),]$nw_type <- "OOS"

temp[which(!is.na(temp$wifi_rssi)),]$nw_type <- "WIFI"

temp$nw_type <- factor(temp$nw_type)
summary(temp$nw_type)

summary(temp[which(!is.na(temp$download_speed)),]$nw_type)
summary(temp[which(!is.na(temp$upload_speed)),]$nw_type)


str(temp)

#
temp$network_id <- t$network_id
summary(temp$network_id)
table(temp$network_id)
sort(table(temp$network_id), decreasing = T)
#names(tail(sort(table(temp$network_id), decreasing = T), 100))
temp$network_id[temp$network_id %in% names(tail(sort(table(temp$network_id), decreasing = T), 759))] <- NA
temp$network_id <- factor(temp$network_id)
str(temp$network_id)
temp$network_id <- factor(temp$network_id, levels = levels(t$network_id)[1:1024])
summary(temp$network_id)
str(temp$network_id)

#

temp$new_year <- factor(temp$new_year)
temp$new_month <- factor(temp$new_month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
temp$new_weekday <- factor(temp$new_weekday)

#

str(temp)

#

temp$new_network_country <- substring(t$network_id,1,3)
temp$new_network_country <- as.integer(temp$new_network_country)

temp$new_network_country <- factor(temp$new_network_country)
str(temp$new_network_country)
levels.new_network_county.orig <- levels(temp$new_network_country)
summary(temp$new_network_country)
table(temp$new_network_country)
sort(table(temp$new_network_country), decreasing = T)
temp$new_network_country[temp$new_network_country %in% names(tail(sort(table(temp$new_network_country), decreasing = T), 79))] <- NA
temp$new_network_country <- factor(temp$new_network_country)
str(temp$new_network_country)
temp$new_network_country <- factor(temp$new_network_country, levels = levels.new_network_county.orig)
str(temp$new_network_country)

#

str(temp$app_version_code)
str(t$app_version_code)
temp$app_version_code <- factor(temp$app_version_code, levels = levels(t$app_version_code))
str(temp$app_version_code)
summary(temp$app_version_code)
#

library(dplyr)
temp.tr <- select(temp, download_speed, upload_speed, 
                             new_month, new_year, new_weekday, new_network_country, network_id, 
                             rssi, 
                             ec_io, 
                             rsrp, rsrq, rssnr, 
                             wifi_rssi, wifi_band,
                             icmp_ping_time, icmp_ping_packet_loss, icmp_ping_range,
                             app_version_code, test_type,
                             network_type, nw_type)
str(temp.tr)
summary(temp.tr)
library(randomForest)
temp.tr <- na.roughfix(temp.tr)  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
temp.tr$download_speed <- temp$download_speed  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
temp.tr$upload_speed <- temp$upload_speed  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
summary(temp.tr)


#

temp.tr.d <- temp.tr[which(!is.na(temp.tr$download_speed)),]
temp.tr.u <- temp.tr[which(!is.na(temp.tr$upload_speed)),]

library(caret)
trainIndex <- createDataPartition(temp.tr.d$download_speed, p = .8, list = FALSE)
temp.tr.d.train <- temp.tr.d[trainIndex,]
temp.tr.d.test <- temp.tr.d[-trainIndex,]

temp.tr.d.train.lte <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "LTE"),]
temp.tr.d.train.gsm <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "GSM"),]
temp.tr.d.train.umts <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "UMTS"),]
temp.tr.d.train.hsdpa <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "HSDPA"),]
temp.tr.d.train.hspa <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "HSPA"),]
temp.tr.d.train.hspap <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "HSPAP"),]
temp.tr.d.train.cdma <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "CDMA"),]
temp.tr.d.train.oos <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "OOS"),]
temp.tr.d.train.wifi <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "WIFI"),]

temp.tr.d.test.lte <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "LTE"),]
temp.tr.d.test.gsm <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "GSM"),]
temp.tr.d.test.umts <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "UMTS"),]
temp.tr.d.test.hsdpa <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "HSDPA"),]
temp.tr.d.test.hspa <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "HSPA"),]
temp.tr.d.test.hspap <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "HSPAP"),]
temp.tr.d.test.cdma <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "CDMA"),]
temp.tr.d.test.oos <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "OOS"),]
temp.tr.d.test.wifi <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "WIFI"),]

library(caret)
trainIndex <- createDataPartition(temp.tr.u$upload_speed, p = .8, list = FALSE)
temp.tr.u.train <- temp.tr.u[trainIndex,]
temp.tr.u.test <- temp.tr.u[-trainIndex,]


#LTE DL


library(gbm)
Sys.time()
gbm.lte.d <- gbm(download_speed ~ new_month + new_year + new_weekday
                         + new_network_country + network_id 
                         + rsrp + rsrq + rssnr
                         + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                         + app_version_code + test_type, 
                         data = temp.tr.d.train.lte, 
                         distribution = "gaussian",
                         n.trees = 4000,
                         interaction.depth = 5,
                         n.minobsinnode = 10,
                         shrinkage = 0.001,
                         keep.data = TRUE,
                         verbose = TRUE)
Sys.time()
gbm.lte.d
summary(gbm.lte.d)
gbm.perf(gbm.lte.d)

gbm.lte.d.predict.train <- predict.gbm(object = gbm.lte.d, newdata = temp.tr.d.train.lte, 4000)
gbm.lte.d.predict.test <- predict.gbm(object = gbm.lte.d, newdata = temp.tr.d.test.lte, 4000)

rmsle.lte.d.train <- sqrt(1 / length(gbm.lte.d.predict.train) * 
                      sum((log1p(gbm.lte.d.predict.train) - 
                             log1p(temp.tr.d.train.lte$download_speed))^2))
rmsle.lte.d.test <- sqrt(1 / length(gbm.lte.d.predict.test) * 
                            sum((log1p(gbm.lte.d.predict.test) - 
                                   log1p(temp.tr.d.test.lte$download_speed))^2))


# all data DL

Sys.time()
gbm.d <- gbm(download_speed ~ new_month + new_year + new_weekday
                 + new_network_country + network_id 
                 + rsrp + rsrq + rssnr
                 + rssi + ec_io
                 + wifi_rssi + wifi_band
                 + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                 + app_version_code + test_type
                 + network_type, 
                 data = temp.tr.d.train, 
                 distribution = "gaussian",
                 n.trees = 4000,
                 interaction.depth = 5,
                 n.minobsinnode = 10,
                 shrinkage = 0.001,
                 keep.data = TRUE,
                 verbose = TRUE)
Sys.time()

gbm.d
summary(gbm.d)
gbm.perf(gbm.d)

gbm.d.predict.train <- predict.gbm(object = gbm.d, newdata = temp.tr.d.train, 4000)
gbm.d.predict.test <- predict.gbm(object = gbm.d, newdata = temp.tr.d.test, 4000)

rmsle.d.train <- sqrt(1 / length(gbm.d.predict.train) * 
                            sum((log1p(gbm.d.predict.train) - 
                                   log1p(temp.tr.d.train$download_speed))^2))
rmsle.d.test <- sqrt(1 / length(gbm.d.predict.test) * 
                           sum((log1p(gbm.d.predict.test) - 
                                  log1p(temp.tr.d.test$download_speed))^2))


# all data UL

Sys.time()
gbm.u <- gbm(upload_speed ~ new_month + new_year + new_weekday
             + new_network_country + network_id 
             + rsrp + rsrq + rssnr
             + rssi + ec_io
             + wifi_rssi + wifi_band
             + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
             + app_version_code + test_type
             + network_type, 
             data = temp.tr.u.train, 
             distribution = "gaussian",
             n.trees = 4000,
             interaction.depth = 5,
             n.minobsinnode = 10,
             shrinkage = 0.001,
             keep.data = TRUE,
             verbose = TRUE)
Sys.time()

gbm.u
summary(gbm.u)
gbm.perf(gbm.u)

gbm.u.predict.train <- predict.gbm(object = gbm.u, newdata = temp.tr.u.train, 4000)
gbm.u.predict.test <- predict.gbm(object = gbm.u, newdata = temp.tr.u.test, 4000)

rmsle.u.train <- sqrt(1 / length(gbm.u.predict.train) * 
                        sum((log1p(gbm.u.predict.train) - 
                               log1p(temp.tr.u.train$download_speed))^2))
rmsle.u.test <- sqrt(1 / length(gbm.u.predict.test) * 
                       sum((log1p(gbm.u.predict.test) - 
                              log1p(temp.tr.u.test$download_speed))^2))
