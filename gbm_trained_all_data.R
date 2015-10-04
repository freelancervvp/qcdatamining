#data loading
setwd("C:\\freelancervvp\\Projects\\2015_Data_Mining\\new_data")
load("temp.new")

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
# Formatting for GBM

sort(table(temp.tr$network_id), decreasing = T)
temp.tr$network_id[temp.tr$network_id %in% names(tail(sort(table(temp.tr$network_id), decreasing = T), 63))] <- "NULL"
temp.tr$network_id <- factor(temp.tr$network_id)
str(temp.tr)

sort(table(temp.tr$network_id_sim), decreasing = T)
temp.tr$network_id_sim[temp.tr$network_id_sim %in% names(tail(sort(table(temp.tr$network_id_sim), decreasing = T), 156))] <- "NULL"
temp.tr$network_id_sim <- factor(temp.tr$network_id_sim)
str(temp.tr)

str(temp.tr)
summary(temp.tr)
table(is.na(temp.tr$network_id))

# filling NAs
#library(randomForest)
#temp.tr <- na.roughfix(temp.tr)  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#temp.tr$download_speed <- temp$download_speed  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#temp.tr$upload_speed <- temp$upload_speed  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

str(temp.tr)
summary(temp.tr)


# Creating split on train/test sets

#dl
temp.tr.d <- temp.tr[which(!is.na(temp.tr$download_speed)),]

#library(caret)
#set.seed(16)
#trainIndex <- createDataPartition(temp.tr.d$download_speed, p = .8, list = FALSE)
temp.tr.d.train <- temp.tr.d
#temp.tr.d.test <- temp.tr.d[-trainIndex,]

temp.tr.d.train.lte <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "LTE"),]
temp.tr.d.train.gsm <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "GSM"),]
temp.tr.d.train.umts <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "UMTS"),]
temp.tr.d.train.cdma <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "CDMA"),]
temp.tr.d.train.oos <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "OOS"),]
temp.tr.d.train.wifi <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "WIFI"),]

#ul
temp.tr.u <- temp.tr[which(!is.na(temp.tr$upload_speed)),]

#library(caret)
#set.seed(16)
#trainIndex <- createDataPartition(temp.tr.u$upload_speed, p = .8, list = FALSE)
temp.tr.u.train <- temp.tr.u
#temp.tr.u.test <- temp.tr.u[-trainIndex,]

temp.tr.u.train.lte <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "LTE"),]
temp.tr.u.train.gsm <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "GSM"),]
temp.tr.u.train.umts <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "UMTS"),]
temp.tr.u.train.cdma <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "CDMA"),]
temp.tr.u.train.oos <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "OOS"),]
temp.tr.u.train.wifi <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "WIFI"),]


# gbm lte dl

summary(temp.tr.d.train.lte)
str(temp.tr.d.train.lte)

# filling NAs - 2
#install.packages("imputeR")
#library(imputeR)
#temp.tr.d.train.lte.impute <- impute(temp.tr.d.train.lte, lmFun = "gbmC", cFun = "gbmC", verbose = T)
#Rmse(temp.tr[,3:27]$imp, missdata, parkinson, norm = TRUE)
#head(temp.tr.d.train.lte[which(is.na(temp.tr.d.train.lte$rsrp)),])
#summary(na.omit(temp.tr.d.train.lte))
#str(na.omit(temp.tr.d.train.lte))

library(gbm)
Sys.time()
gbm.lte.d.2 <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
                 + new_network_country + network_id #+ network_id_sim
                 + phone_model + loc_source_gps_one_net_zero + location_precision
                 + rsrp + rsrq + rssnr
                 + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                 + app_version_code + test_type + unreliable, 
                 data = temp.tr.d.train.lte, 
                 distribution = "gaussian",
                 n.trees = 1000,
                 interaction.depth = 20,
                 n.minobsinnode = 10,
                 shrinkage = 0.01,
                 cv.folds = 5,
                 keep.data = TRUE,
                 verbose = TRUE)
Sys.time()
gbm.lte.d
summary(gbm.lte.d)
gbm.perf(gbm.lte.d)
pretty.gbm.tree(gbm.lte.d, i.tree = 500)

gbm.lte.d.predict.train <- 10^(predict.gbm(object = gbm.lte.d, newdata = temp.tr.d.train.lte, 658))
summary(temp.tr.d.train.lte$download_speed)
summary(gbm.lte.d.predict.train)
head(cbind(temp.tr.d.train.lte$download_speed, gbm.lte.d.predict.train), 25)
gbm.lte.d.predict.test <- 10^(predict.gbm(object = gbm.lte.d, newdata = temp.tr.d.test.lte, 658))
head(cbind(temp.tr.d.test.lte$download_speed, gbm.lte.d.predict.test), 25)

rmsle.lte.d.train <- sqrt(1 / length(gbm.lte.d.predict.train) * 
                            sum((log1p(gbm.lte.d.predict.train) - 
                                   log1p(temp.tr.d.train.lte$download_speed))^2))
rmsle.lte.d.test <- sqrt(1 / length(gbm.lte.d.predict.test) * 
                           sum((log1p(gbm.lte.d.predict.test) - 
                                  log1p(temp.tr.d.test.lte$download_speed))^2))

save(gbm.lte.d, file = "gbm.lte.d")


# gbm lte ul

summary(temp.tr.u.train.lte)
str(temp.tr.u.train.lte)

library(gbm)
Sys.time()
gbm.lte.u <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
                 + new_network_country + network_id #+ network_id_sim
                 + phone_model + loc_source_gps_one_net_zero + location_precision
                 + rsrp + rsrq + rssnr
                 + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                 + app_version_code + test_type + unreliable, 
                 data = temp.tr.u.train.lte, 
                 distribution = "gaussian",
                 n.trees = 1000,
                 interaction.depth = 20,
                 n.minobsinnode = 10,
                 shrinkage = 0.01,
                 keep.data = TRUE,
                 verbose = TRUE)
Sys.time()
gbm.lte.u
summary(gbm.lte.u)
gbm.perf(gbm.lte.u)
#pretty.gbm.tree(gbm.lte.u, i.tree = 500)

gbm.lte.u.predict.train <- 10^(predict.gbm(object = gbm.lte.u, newdata = temp.tr.u.train.lte, 413))
summary(temp.tr.u.train.lte$upload_speed)
summary(gbm.lte.u.predict.train)
head(cbind(temp.tr.u.train.lte$upload_speed, gbm.lte.u.predict.train), 25)

gbm.lte.u.predict.test <- 10^(predict.gbm(object = gbm.lte.u, newdata = temp.tr.u.test.lte, 413))
summary(temp.tr.u.test.lte$upload_speed)
summary(gbm.lte.u.predict.test)
head(cbind(temp.tr.u.test.lte$upload_speed, gbm.lte.u.predict.test), 25)

rmsle.lte.u.train <- sqrt(1 / length(gbm.lte.u.predict.train) * 
                            sum((log1p(gbm.lte.u.predict.train) - 
                                   log1p(temp.tr.u.train.lte$upload_speed))^2))
rmsle.lte.u.test <- sqrt(1 / length(gbm.lte.u.predict.test) * 
                           sum((log1p(gbm.lte.u.predict.test) - 
                                  log1p(temp.tr.u.test.lte$upload_speed))^2))

save(gbm.lte.u, file = "gbm.lte.u")

#


# gbm umts dl

summary(temp.tr.d.train.umts)
str(temp.tr.d.train.umts)

library(gbm)
Sys.time()
gbm.umts.d <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
                  + new_network_country + network_id #+ network_id_sim
                  + phone_model + loc_source_gps_one_net_zero + location_precision
                  + rssi
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                  + app_version_code + test_type + unreliable
                  + network_type, 
                  data = temp.tr.d.train.umts, 
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 20,
                  n.minobsinnode = 10,
                  shrinkage = 0.01,
                  keep.data = TRUE,
                  verbose = TRUE)
Sys.time()
gbm.umts.d
summary(gbm.umts.d)
gbm.perf(gbm.umts.d)

gbm.umts.d.predict.train <- 10^(predict.gbm(object = gbm.umts.d, newdata = temp.tr.d.train.umts, 722))
summary(temp.tr.d.train.umts$download_speed)
summary(gbm.umts.d.predict.train)
head(cbind(temp.tr.d.train.umts$download_speed, gbm.umts.d.predict.train), 25)
gbm.umts.d.predict.test <- 10^(predict.gbm(object = gbm.umts.d, newdata = temp.tr.d.test.umts, 722))
head(cbind(temp.tr.d.test.umts$download_speed, gbm.umts.d.predict.test), 25)

rmsle.umts.d.train <- sqrt(1 / length(gbm.umts.d.predict.train) * 
                             sum((log1p(gbm.umts.d.predict.train) - 
                                    log1p(temp.tr.d.train.umts$download_speed))^2))
rmsle.umts.d.test <- sqrt(1 / length(gbm.umts.d.predict.test) * 
                            sum((log1p(gbm.umts.d.predict.test) - 
                                   log1p(temp.tr.d.test.umts$download_speed))^2))

save(gbm.umts.d, file = "gbm.umts.d")


# gbm umts ul

summary(temp.tr.u.train.umts)
str(temp.tr.u.train.umts)

library(gbm)
Sys.time()
gbm.umts.u <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
                  + new_network_country + network_id #+ network_id_sim
                  + phone_model + loc_source_gps_one_net_zero + location_precision
                  + rssi
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                  + app_version_code + test_type + unreliable
                  + network_type, 
                  data = temp.tr.u.train.umts, 
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 20,
                  n.minobsinnode = 10,
                  shrinkage = 0.01,
                  keep.data = TRUE,
                  verbose = TRUE)
Sys.time()
gbm.umts.u
summary(gbm.umts.u)
gbm.perf(gbm.umts.u)
#pretty.gbm.tree(gbm.umts.u, i.tree = 500)

gbm.umts.u.predict.train <- 10^(predict.gbm(object = gbm.umts.u, newdata = temp.tr.u.train.umts, 583))
summary(temp.tr.u.train.umts$upload_speed)
summary(gbm.umts.u.predict.train)
head(cbind(temp.tr.u.train.umts$upload_speed, gbm.umts.u.predict.train), 25)

gbm.umts.u.predict.test <- 10^(predict.gbm(object = gbm.umts.u, newdata = temp.tr.u.test.umts, 583))
summary(temp.tr.u.test.umts$upload_speed)
summary(gbm.umts.u.predict.test)
head(cbind(temp.tr.u.test.umts$upload_speed, gbm.umts.u.predict.test), 25)

rmsle.umts.u.train <- sqrt(1 / length(gbm.umts.u.predict.train) * 
                             sum((log1p(gbm.umts.u.predict.train) - 
                                    log1p(temp.tr.u.train.umts$upload_speed))^2))
rmsle.umts.u.test <- sqrt(1 / length(gbm.umts.u.predict.test) * 
                            sum((log1p(gbm.umts.u.predict.test) - 
                                   log1p(temp.tr.u.test.umts$upload_speed))^2))

rsq.umts.u.test <- 1 - sum((gbm.umts.u.predict.test - temp.tr.u.test.umts$upload_speed)^2) / 
  sum((gbm.umts.u.predict.test - mean(gbm.umts.u.predict.test))^2)

save(gbm.umts.u, file = "gbm.umts.u")

#

# gbm gsm dl

summary(temp.tr.d.train.gsm)
str(temp.tr.d.train.gsm)

library(gbm)
Sys.time()
gbm.gsm.d <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
                 + new_network_country + network_id #+ network_id_sim
                 + phone_model + loc_source_gps_one_net_zero + location_precision
                 + rssi
                 + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                 + app_version_code + test_type + unreliable
                 + network_type, #forgot to use!!!
                 data = temp.tr.d.train.gsm, 
                 distribution = "gaussian",
                 n.trees = 2000,
                 interaction.depth = 20,
                 n.minobsinnode = 10,
                 shrinkage = 0.005,
                 keep.data = TRUE,
                 verbose = TRUE)
Sys.time()
gbm.gsm.d
summary(gbm.gsm.d)
gbm.perf(gbm.gsm.d)

gbm.gsm.d.predict.train <- 10^(predict.gbm(object = gbm.gsm.d, newdata = temp.tr.d.train.gsm, 148))
summary(temp.tr.d.train.gsm$download_speed)
summary(gbm.gsm.d.predict.train)
head(cbind(temp.tr.d.train.gsm$download_speed, gbm.gsm.d.predict.train), 25)
gbm.gsm.d.predict.test <- 10^(predict.gbm(object = gbm.gsm.d, newdata = temp.tr.d.test.gsm, 148))
head(cbind(temp.tr.d.test.gsm$download_speed, gbm.gsm.d.predict.test), 25)

rmsle.gsm.d.train <- sqrt(1 / length(gbm.gsm.d.predict.train) * 
                            sum((log1p(gbm.gsm.d.predict.train) - 
                                   log1p(temp.tr.d.train.gsm$download_speed))^2))
rmsle.gsm.d.test <- sqrt(1 / length(gbm.gsm.d.predict.test) * 
                           sum((log1p(gbm.gsm.d.predict.test) - 
                                  log1p(temp.tr.d.test.gsm$download_speed))^2))

save(gbm.gsm.d, file = "gbm.gsm.d")


# gbm gsm ul

summary(temp.tr.u.train.gsm)
str(temp.tr.u.train.gsm)

library(gbm)
Sys.time()
gbm.gsm.u <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
                 + new_network_country + network_id #+ network_id_sim
                 + phone_model + loc_source_gps_one_net_zero + location_precision
                 + rssi
                 + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                 + app_version_code + test_type + unreliable
                 + network_type,
                 data = temp.tr.u.train.gsm, 
                 distribution = "gaussian",
                 n.trees = 1000,
                 interaction.depth = 20,
                 n.minobsinnode = 10,
                 shrinkage = 0.01,
                 keep.data = TRUE,
                 verbose = TRUE)
Sys.time()
gbm.gsm.u
summary(gbm.gsm.u)
gbm.perf(gbm.gsm.u)

gbm.gsm.u.predict.train <- 10^(predict.gbm(object = gbm.gsm.u, newdata = temp.tr.u.train.gsm, 184))
summary(temp.tr.u.train.gsm$upload_speed)
summary(gbm.gsm.u.predict.train)
head(cbind(temp.tr.u.train.gsm$upload_speed, gbm.gsm.u.predict.train), 25)

gbm.gsm.u.predict.test <- 10^(predict.gbm(object = gbm.gsm.u, newdata = temp.tr.u.test.gsm, 184))
summary(temp.tr.u.test.gsm$upload_speed)
summary(gbm.gsm.u.predict.test)
head(cbind(temp.tr.u.test.gsm$upload_speed, gbm.gsm.u.predict.test), 25)

rmsle.gsm.u.train <- sqrt(1 / length(gbm.gsm.u.predict.train) * 
                            sum((log1p(gbm.gsm.u.predict.train) - 
                                   log1p(temp.tr.u.train.gsm$upload_speed))^2))
rmsle.gsm.u.test <- sqrt(1 / length(gbm.gsm.u.predict.test) * 
                           sum((log1p(gbm.gsm.u.predict.test) - 
                                  log1p(temp.tr.u.test.gsm$upload_speed))^2))

save(gbm.gsm.u, file = "gbm.gsm.u")

#


# gbm cdma dl

summary(temp.tr.d.train.cdma)
str(temp.tr.d.train.cdma)

library(gbm)
Sys.time()
gbm.cdma.d <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
                  + new_network_country + network_id #+ network_id_sim
                  + phone_model + loc_source_gps_one_net_zero + location_precision
                  + rssi + ec_io
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                  + app_version_code + test_type + unreliable
                  + network_type,
                  data = temp.tr.d.train.cdma, 
                  distribution = "gaussian",
                  n.trees = 2000,
                  interaction.depth = 20,
                  n.minobsinnode = 10,
                  shrinkage = 0.005,
                  keep.data = TRUE,
                  verbose = TRUE)
Sys.time()
gbm.cdma.d
summary(gbm.cdma.d)
gbm.perf(gbm.cdma.d)

gbm.cdma.d.predict.train <- 10^(predict.gbm(object = gbm.cdma.d, newdata = temp.tr.d.train.cdma, 600))
summary(temp.tr.d.train.cdma$download_speed)
summary(gbm.cdma.d.predict.train)
head(cbind(temp.tr.d.train.cdma$download_speed, gbm.cdma.d.predict.train), 25)
gbm.cdma.d.predict.test <- 10^(predict.gbm(object = gbm.cdma.d, newdata = temp.tr.d.test.cdma, 600))
head(cbind(temp.tr.d.test.cdma$download_speed, gbm.cdma.d.predict.test), 25)

rmsle.cdma.d.train <- sqrt(1 / length(gbm.cdma.d.predict.train) * 
                             sum((log1p(gbm.cdma.d.predict.train) - 
                                    log1p(temp.tr.d.train.cdma$download_speed))^2))
rmsle.cdma.d.test <- sqrt(1 / length(gbm.cdma.d.predict.test) * 
                            sum((log1p(gbm.cdma.d.predict.test) - 
                                   log1p(temp.tr.d.test.cdma$download_speed))^2))

save(gbm.cdma.d, file = "gbm.cdma.d")


# gbm cdma ul

summary(temp.tr.u.train.cdma)
str(temp.tr.u.train.cdma)

library(gbm)
Sys.time()
gbm.cdma.u <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
                  + new_network_country + network_id #+ network_id_sim
                  + phone_model + loc_source_gps_one_net_zero + location_precision
                  + rssi + ec_io
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                  + app_version_code + test_type + unreliable
                  + network_type,
                  data = temp.tr.u.train.cdma, 
                  distribution = "gaussian",
                  n.trees = 2000,
                  interaction.depth = 20,
                  n.minobsinnode = 10,
                  shrinkage = 0.005,
                  keep.data = TRUE,
                  verbose = TRUE)
Sys.time()
gbm.cdma.u
summary(gbm.cdma.u)
gbm.perf(gbm.cdma.u)

gbm.cdma.u.predict.train <- 10^(predict.gbm(object = gbm.cdma.u, newdata = temp.tr.u.train.cdma, 652))
summary(temp.tr.u.train.cdma$upload_speed)
summary(gbm.cdma.u.predict.train)
head(cbind(temp.tr.u.train.cdma$upload_speed, gbm.cdma.u.predict.train), 25)

gbm.cdma.u.predict.test <- 10^(predict.gbm(object = gbm.cdma.u, newdata = temp.tr.u.test.cdma, 652))
summary(temp.tr.u.test.cdma$upload_speed)
summary(gbm.cdma.u.predict.test)
head(cbind(temp.tr.u.test.cdma$upload_speed, gbm.cdma.u.predict.test), 25)

rmsle.cdma.u.train <- sqrt(1 / length(gbm.cdma.u.predict.train) * 
                             sum((log1p(gbm.cdma.u.predict.train) - 
                                    log1p(temp.tr.u.train.cdma$upload_speed))^2))
rmsle.cdma.u.test <- sqrt(1 / length(gbm.cdma.u.predict.test) * 
                            sum((log1p(gbm.cdma.u.predict.test) - 
                                   log1p(temp.tr.u.test.cdma$upload_speed))^2))

save(gbm.cdma.u, file = "gbm.cdma.u")

#

# gbm wifi dl

summary(temp.tr.d.train.wifi)
str(temp.tr.d.train.wifi)

library(gbm)
Sys.time()
gbm.wifi.d <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
                  + new_network_country + network_id #+ network_id_sim
                  + phone_model + loc_source_gps_one_net_zero + location_precision
                  + wifi_rssi + wifi_band
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                  + app_version_code + test_type + unreliable
                  + network_type,
                  data = temp.tr.d.train.wifi, 
                  distribution = "gaussian",
                  n.trees = 2000,
                  interaction.depth = 20,
                  n.minobsinnode = 10,
                  shrinkage = 0.005,
                  keep.data = TRUE,
                  verbose = TRUE)
Sys.time()
gbm.wifi.d
summary(gbm.wifi.d)
gbm.perf(gbm.wifi.d)

gbm.wifi.d.predict.train <- 10^(predict.gbm(object = gbm.wifi.d, newdata = temp.tr.d.train.wifi, 444))
summary(temp.tr.d.train.wifi$download_speed)
summary(gbm.wifi.d.predict.train)
head(cbind(temp.tr.d.train.wifi$download_speed, gbm.wifi.d.predict.train), 25)
gbm.wifi.d.predict.test <- 10^(predict.gbm(object = gbm.wifi.d, newdata = temp.tr.d.test.wifi, 444))
head(cbind(temp.tr.d.test.wifi$download_speed, gbm.wifi.d.predict.test), 25)

rmsle.wifi.d.train <- sqrt(1 / length(gbm.wifi.d.predict.train) * 
                             sum((log1p(gbm.wifi.d.predict.train) - 
                                    log1p(temp.tr.d.train.wifi$download_speed))^2))
rmsle.wifi.d.test <- sqrt(1 / length(gbm.wifi.d.predict.test) * 
                            sum((log1p(gbm.wifi.d.predict.test) - 
                                   log1p(temp.tr.d.test.wifi$download_speed))^2))

save(gbm.wifi.d, file = "gbm.wifi.d")


# gbm wifi ul

summary(temp.tr.u.train.wifi)
str(temp.tr.u.train.wifi)

library(gbm)
Sys.time()
gbm.wifi.u <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
                  + new_network_country + network_id #+ network_id_sim
                  + phone_model + loc_source_gps_one_net_zero + location_precision
                  + wifi_rssi + wifi_band
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                  + app_version_code + test_type + unreliable
                  + network_type,
                  data = temp.tr.u.train.wifi, 
                  distribution = "gaussian",
                  n.trees = 2000,
                  interaction.depth = 20,
                  n.minobsinnode = 10,
                  shrinkage = 0.005,
                  keep.data = TRUE,
                  verbose = TRUE)
Sys.time()
gbm.wifi.u
summary(gbm.wifi.u)
gbm.perf(gbm.wifi.u)

gbm.wifi.u.predict.train <- 10^(predict.gbm(object = gbm.wifi.u, newdata = temp.tr.u.train.wifi, 332))
summary(temp.tr.u.train.wifi$upload_speed)
summary(gbm.wifi.u.predict.train)
head(cbind(temp.tr.u.train.wifi$upload_speed, gbm.wifi.u.predict.train), 25)

gbm.wifi.u.predict.test <- 10^(predict.gbm(object = gbm.wifi.u, newdata = temp.tr.u.test.wifi, 332))
summary(temp.tr.u.test.wifi$upload_speed)
summary(gbm.wifi.u.predict.test)
head(cbind(temp.tr.u.test.wifi$upload_speed, gbm.wifi.u.predict.test), 25)

rmsle.wifi.u.train <- sqrt(1 / length(gbm.wifi.u.predict.train) * 
                             sum((log1p(gbm.wifi.u.predict.train) - 
                                    log1p(temp.tr.u.train.wifi$upload_speed))^2))
rmsle.wifi.u.test <- sqrt(1 / length(gbm.wifi.u.predict.test) * 
                            sum((log1p(gbm.wifi.u.predict.test) - 
                                   log1p(temp.tr.u.test.wifi$upload_speed))^2))

save(gbm.wifi.u, file = "gbm.wifi.u")

#


# gbm oos dl

summary(temp.tr.d.train.oos)
str(temp.tr.d.train.oos)

library(gbm)
Sys.time()
gbm.oos.d <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
                 + new_network_country + network_id #+ network_id_sim
                 + phone_model + loc_source_gps_one_net_zero + location_precision
                 + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                 + app_version_code + test_type + unreliable,
                 data = temp.tr.d.train.oos, 
                 distribution = "gaussian",
                 n.trees = 2000,
                 interaction.depth = 20,
                 n.minobsinnode = 10,
                 shrinkage = 0.005,
                 keep.data = TRUE,
                 verbose = TRUE)
Sys.time()
gbm.oos.d
summary(gbm.oos.d)
gbm.perf(gbm.oos.d)

gbm.oos.d.predict.train <- 10^(predict.gbm(object = gbm.oos.d, newdata = temp.tr.d.train.oos, 221))
summary(temp.tr.d.train.oos$download_speed)
summary(gbm.oos.d.predict.train)
head(cbind(temp.tr.d.train.oos$download_speed, gbm.oos.d.predict.train), 25)
gbm.oos.d.predict.test <- 10^(predict.gbm(object = gbm.oos.d, newdata = temp.tr.d.test.oos, 221))
head(cbind(temp.tr.d.test.oos$download_speed, gbm.oos.d.predict.test), 25)

rmsle.oos.d.train <- sqrt(1 / length(gbm.oos.d.predict.train) * 
                            sum((log1p(gbm.oos.d.predict.train) - 
                                   log1p(temp.tr.d.train.oos$download_speed))^2))
rmsle.oos.d.test <- sqrt(1 / length(gbm.oos.d.predict.test) * 
                           sum((log1p(gbm.oos.d.predict.test) - 
                                  log1p(temp.tr.d.test.oos$download_speed))^2))

save(gbm.oos.d, file = "gbm.oos.d")


# gbm oos ul

summary(temp.tr.u.train.oos)
str(temp.tr.u.train.oos)

library(gbm)
Sys.time()
gbm.oos.u <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
                 + new_network_country + network_id #+ network_id_sim
                 + phone_model + loc_source_gps_one_net_zero + location_precision
                 + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                 + app_version_code + test_type + unreliable,
                 data = temp.tr.u.train.oos, 
                 distribution = "gaussian",
                 n.trees = 2000,
                 interaction.depth = 20,
                 n.minobsinnode = 10,
                 shrinkage = 0.005,
                 keep.data = TRUE,
                 verbose = TRUE)
Sys.time()
gbm.oos.u
summary(gbm.oos.u)
gbm.perf(gbm.oos.u)

gbm.oos.u.predict.train <- 10^(predict.gbm(object = gbm.oos.u, newdata = temp.tr.u.train.oos, 249))
summary(temp.tr.u.train.oos$upload_speed)
summary(gbm.oos.u.predict.train)
head(cbind(temp.tr.u.train.oos$upload_speed, gbm.oos.u.predict.train), 25)

gbm.oos.u.predict.test <- 10^(predict.gbm(object = gbm.oos.u, newdata = temp.tr.u.test.oos, 249))
summary(temp.tr.u.test.oos$upload_speed)
summary(gbm.oos.u.predict.test)
head(cbind(temp.tr.u.test.oos$upload_speed, gbm.oos.u.predict.test), 25)

rmsle.oos.u.train <- sqrt(1 / length(gbm.oos.u.predict.train) * 
                            sum((log1p(gbm.oos.u.predict.train) - 
                                   log1p(temp.tr.u.train.oos$upload_speed))^2))
rmsle.oos.u.test <- sqrt(1 / length(gbm.oos.u.predict.test) * 
                           sum((log1p(gbm.oos.u.predict.test) - 
                                  log1p(temp.tr.u.test.oos$upload_speed))^2))

save(gbm.oos.u, file = "gbm.oos.u")

#