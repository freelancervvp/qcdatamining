setwd("C:\\freelancervvp\\Projects\\2015_Data_Mining\\new_data")
load("temp.new")

str(temp)

# nw_type

temp$nw_type <- NA
temp[which(temp$network_type == "LTE"),]$nw_type <- "LTE"
temp[which(temp$network_type == "1xRTT" | temp$network_type == "eHRPD" | temp$network_type == "EVDO 0" | 
             temp$network_type == "EVDO A" | temp$network_type == "EVDO B" | temp$network_type == "CDMA"),]$nw_type <- "CDMA"
temp[which(temp$network_type == "EDGE" | temp$network_type == "GPRS"),]$nw_type <- "GSM"
temp[which(temp$network_type == "HSDPA" | temp$network_type == "HSUPA" | temp$network_type == "HSPA" | 
             temp$network_type == "HSPAP" | temp$network_type == "UMTS"),]$nw_type <- "UMTS"
temp[which(temp$network_type == "Out of service"),]$nw_type <- "OOS"
temp[which(temp$is_wifi),]$nw_type <- "WIFI"
temp$nw_type <- factor(temp$nw_type)
summary(temp$nw_type)

table(temp[which(is.na(temp$nw_type)),]$download_speed)
table(temp[which(is.na(temp$nw_type)),]$upload_speed)

str(temp)

library(dplyr)
temp.tr <- select(temp, 
                  download_speed, upload_speed, 
                  new_month, new_year, new_weekday, new_hour,
                  new_network_country, 
                  network_id, network_id_sim,
                  phone_model,
                  loc_source_gps_one_net_zero, location_precision,
                  rssi, 
                  ec_io, 
                  rsrp, rsrq, rssnr, 
                  wifi_rssi, wifi_band,
                  icmp_ping_time, icmp_ping_packet_loss, icmp_ping_range,
                  app_version_code, 
                  test_type, unreliable,
                  network_type, nw_type)
str(temp.tr)





###########
###_________________________________________________________________________________________
# Formatting for RF

sort(table(temp.tr$network_id), decreasing = T)
temp.tr$network_id[temp.tr$network_id %in% names(tail(sort(table(temp.tr$network_id), decreasing = T), 1010))] <- "NULL"
temp.tr$network_id <- factor(temp.tr$network_id)
str(temp.tr)

sort(table(temp.tr$network_id_sim), decreasing = T)
temp.tr$network_id_sim[temp.tr$network_id_sim %in% names(tail(sort(table(temp.tr$network_id_sim), decreasing = T), 1103))] <- "NULL"
temp.tr$network_id_sim <- factor(temp.tr$network_id_sim)
str(temp.tr)

sort(table(temp.tr$new_network_country), decreasing = T)
temp.tr$new_network_country[temp.tr$new_network_country %in% names(tail(sort(table(temp.tr$new_network_country), decreasing = T), 176))] <- "NULL"
temp.tr$new_network_country <- factor(temp.tr$new_network_country)
str(temp.tr)

sort(table(temp.tr$phone_model), decreasing = T)
temp.tr$phone_model[temp.tr$phone_model %in% names(tail(sort(table(temp.tr$phone_model), decreasing = T), 425))] <- "NULL"
temp.tr$phone_model <- factor(temp.tr$phone_model)
str(temp.tr)

sort(table(temp.tr$app_version_code), decreasing = T)
temp.tr$app_version_code[temp.tr$app_version_code %in% names(tail(sort(table(temp.tr$app_version_code), decreasing = T), 5))] <- "NULL"
temp.tr$app_version_code <- factor(temp.tr$app_version_code)
str(temp.tr)

str(temp.tr)
summary(temp.tr)
table(is.na(temp.tr$network_id))

# filling NAs
#library(randomForest)
#temp.tr <- na.roughfix(temp.tr)  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#temp.tr$download_speed <- temp$download_speed  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#temp.tr$upload_speed <- temp$upload_speed  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# Creating split on train/test sets

#dl
temp.tr.d <- temp.tr[which(!is.na(temp.tr$download_speed)),]

library(caret)
set.seed(16)
trainIndex <- createDataPartition(temp.tr.d$download_speed, p = .8, list = FALSE)
temp.tr.d.train <- temp.tr.d[trainIndex,]
temp.tr.d.test <- temp.tr.d[-trainIndex,]

temp.tr.d.train.lte <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "LTE"),]
temp.tr.d.train.gsm <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "GSM"),]
temp.tr.d.train.umts <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "UMTS"),]
temp.tr.d.train.cdma <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "CDMA"),]
temp.tr.d.train.oos <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "OOS"),]
temp.tr.d.train.wifi <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "WIFI"),]

temp.tr.d.test.lte <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "LTE"),]
temp.tr.d.test.gsm <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "GSM"),]
temp.tr.d.test.umts <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "UMTS"),]
temp.tr.d.test.cdma <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "CDMA"),]
temp.tr.d.test.oos <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "OOS"),]
temp.tr.d.test.wifi <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "WIFI"),]

#ul
temp.tr.u <- temp.tr[which(!is.na(temp.tr$upload_speed)),]

library(caret)
set.seed(16)
trainIndex <- createDataPartition(temp.tr.u$upload_speed, p = .8, list = FALSE)
temp.tr.u.train <- temp.tr.u[trainIndex,]
temp.tr.u.test <- temp.tr.u[-trainIndex,]

temp.tr.u.train.lte <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "LTE"),]
temp.tr.u.train.gsm <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "GSM"),]
temp.tr.u.train.umts <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "UMTS"),]
temp.tr.u.train.cdma <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "CDMA"),]
temp.tr.u.train.oos <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "OOS"),]
temp.tr.u.train.wifi <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "WIFI"),]

temp.tr.u.test.lte <- temp.tr.u.test[which(temp.tr.u.test$nw_type == "LTE"),]
temp.tr.u.test.gsm <- temp.tr.u.test[which(temp.tr.u.test$nw_type == "GSM"),]
temp.tr.u.test.umts <- temp.tr.u.test[which(temp.tr.u.test$nw_type == "UMTS"),]
temp.tr.u.test.cdma <- temp.tr.u.test[which(temp.tr.u.test$nw_type == "CDMA"),]
temp.tr.u.test.oos <- temp.tr.u.test[which(temp.tr.u.test$nw_type == "OOS"),]
temp.tr.u.test.wifi <- temp.tr.u.test[which(temp.tr.u.test$nw_type == "WIFI"),]

#_____________________________________________________________________________________________________________________________
# LTE RANDOMFOREST

library(caret)
library(dplyr)
library(randomForest)
set.seed(16)

# LTE DL RANDOMFOREST

str(temp.tr.d.train.lte)
summary(temp.tr.d.train.lte)
#temp.tr.d.train.lte <- na.omit(temp.tr.d.train.lte[,-c(2,13,14,18,19)])
temp.tr.d.train.lte <- na.roughfix(temp.tr.d.train.lte)
summary(temp.tr.d.train.lte)
Sys.time()
rf.lte.d.2.new <- randomForest(select(temp.tr.d.train.lte, new_month, new_year, new_weekday, new_hour,
                           new_network_country, network_id, network_id_sim,
                           phone_model, loc_source_gps_one_net_zero, location_precision,
                           rsrp, rsrq, rssnr,
                           icmp_ping_time, icmp_ping_packet_loss, icmp_ping_range,
                           app_version_code, test_type, unreliable), 
                           log10(temp.tr.d.train.lte$download_speed), 
                           importance=TRUE, 
                           ntree = 100, 
                           nodesize = 10,
                           #na.action = na.omit,
                           do.trace = T)
Sys.time()
print(rf.lte.d.2)
importance(rf.lte.d.2)
plot(rf.lte.d.2)

#prediction
rf.lte.d.predict.train <- 10^(predict(rf.lte.d.2.new, temp.tr.d.train.lte))
summary(temp.tr.d.train.lte$download_speed)
summary(rf.lte.d.predict.train)
head(cbind(temp.tr.d.train.lte$download_speed, rf.lte.d.predict.train), 25)

rf.lte.d.predict.test <- 10^(predict(rf.lte.d.2.new, na.roughfix(temp.tr.d.test.lte[,-13])))
summary(temp.tr.d.test.lte$download_speed)
summary(rf.lte.d.predict.test)
head(cbind(temp.tr.d.test.lte$download_speed, rf.lte.d.predict.test), 25)

rmsle.lte.d.train <- sqrt(1 / length(rf.lte.d.predict.train) * 
                            sum((log1p(rf.lte.d.predict.train) - 
                                   log1p(temp.tr.d.train.lte$download_speed))^2))
rmsle.lte.d.test <- sqrt(1 / length(rf.lte.d.predict.test) * 
                           sum((log1p(rf.lte.d.predict.test) - 
                                  log1p(temp.tr.d.test.lte$download_speed))^2))

save(rf.lte.d.2, file = "rf.lte.d.2")