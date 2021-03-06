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
gbm.lte.d.4 <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
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
                   keep.data = TRUE,
                   verbose = TRUE)
Sys.time()
gbm.lte.d.4
summary(gbm.lte.d.4)
gbm.perf(gbm.lte.d.4)

gbm.lte.d.predict.train <- 10^(predict.gbm(object = gbm.lte.d.4, newdata = temp.tr.d.train.lte, 782))
summary(temp.tr.d.train.lte$download_speed)
summary(gbm.lte.d.predict.train)
head(cbind(temp.tr.d.train.lte$download_speed, gbm.lte.d.predict.train), 25)

rmsle.lte.d.train <- sqrt(1 / length(gbm.lte.d.predict.train) * 
                            sum((log1p(gbm.lte.d.predict.train) - 
                                   log1p(temp.tr.d.train.lte$download_speed))^2))

save(gbm.lte.d.4, file = "gbm.lte.d.4")


# gbm lte ul

summary(temp.tr.u.train.lte)
str(temp.tr.u.train.lte)

library(gbm)
Sys.time()
gbm.lte.u.4 <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
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
gbm.lte.u.4
summary(gbm.lte.u.4)
gbm.perf(gbm.lte.u.4)

gbm.lte.u.predict.train <- 10^(predict.gbm(object = gbm.lte.u.4, newdata = temp.tr.u.train.lte, 465))
summary(temp.tr.u.train.lte$upload_speed)
summary(gbm.lte.u.predict.train)
head(cbind(temp.tr.u.train.lte$upload_speed, gbm.lte.u.predict.train), 25)

rmsle.lte.u.train <- sqrt(1 / length(gbm.lte.u.predict.train) * 
                            sum((log1p(gbm.lte.u.predict.train) - 
                                   log1p(temp.tr.u.train.lte$upload_speed))^2))

save(gbm.lte.u.4, file = "gbm.lte.u.4")

#


# gbm umts dl

summary(temp.tr.d.train.umts)
str(temp.tr.d.train.umts)

library(gbm)
Sys.time()
gbm.umts.d.4 <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
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
gbm.umts.d.4
summary(gbm.umts.d.4)
gbm.perf(gbm.umts.d.4)

gbm.umts.d.predict.train <- 10^(predict.gbm(object = gbm.umts.d.4, newdata = temp.tr.d.train.umts, 794))
summary(temp.tr.d.train.umts$download_speed)
summary(gbm.umts.d.predict.train)
head(cbind(temp.tr.d.train.umts$download_speed, gbm.umts.d.predict.train), 25)

rmsle.umts.d.train <- sqrt(1 / length(gbm.umts.d.predict.train) * 
                             sum((log1p(gbm.umts.d.predict.train) - 
                                    log1p(temp.tr.d.train.umts$download_speed))^2))

save(gbm.umts.d.4, file = "gbm.umts.d.4")


# gbm umts ul

summary(temp.tr.u.train.umts)
str(temp.tr.u.train.umts)

library(gbm)
Sys.time()
gbm.umts.u.4 <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
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
gbm.umts.u.4
summary(gbm.umts.u.4)
gbm.perf(gbm.umts.u.4)
#pretty.gbm.tree(gbm.umts.u, i.tree = 500)

gbm.umts.u.predict.train <- 10^(predict.gbm(object = gbm.umts.u.4, newdata = temp.tr.u.train.umts, 665))
summary(temp.tr.u.train.umts$upload_speed)
summary(gbm.umts.u.predict.train)
head(cbind(temp.tr.u.train.umts$upload_speed, gbm.umts.u.predict.train), 25)

rmsle.umts.u.train <- sqrt(1 / length(gbm.umts.u.predict.train) * 
                             sum((log1p(gbm.umts.u.predict.train) - 
                                    log1p(temp.tr.u.train.umts$upload_speed))^2))

save(gbm.umts.u.4, file = "gbm.umts.u.4")

#

# gbm gsm dl

summary(temp.tr.d.train.gsm)
str(temp.tr.d.train.gsm)

library(gbm)
Sys.time()
gbm.gsm.d.4 <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
                 + new_network_country + network_id #+ network_id_sim
                 + phone_model + loc_source_gps_one_net_zero + location_precision
                 + rssi
                 + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                 + app_version_code + test_type + unreliable
                 + network_type, #forgot to use!!!
                 data = temp.tr.d.train.gsm, 
                 distribution = "gaussian",
                 n.trees = 1000,
                 interaction.depth = 20,
                 n.minobsinnode = 10,
                 shrinkage = 0.01,
                 keep.data = TRUE,
                 verbose = TRUE)
Sys.time()
gbm.gsm.d.4
summary(gbm.gsm.d.4)
gbm.perf(gbm.gsm.d.4)

gbm.gsm.d.predict.train <- 10^(predict.gbm(object = gbm.gsm.d.4, newdata = temp.tr.d.train.gsm, 95))
summary(temp.tr.d.train.gsm$download_speed)
summary(gbm.gsm.d.predict.train)
head(cbind(temp.tr.d.train.gsm$download_speed, gbm.gsm.d.predict.train), 25)

rmsle.gsm.d.train <- sqrt(1 / length(gbm.gsm.d.predict.train) * 
                            sum((log1p(gbm.gsm.d.predict.train) - 
                                   log1p(temp.tr.d.train.gsm$download_speed))^2))

save(gbm.gsm.d.4, file = "gbm.gsm.d.4")


# gbm gsm ul

summary(temp.tr.u.train.gsm)
str(temp.tr.u.train.gsm)

library(gbm)
Sys.time()
gbm.gsm.u.4 <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
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
gbm.gsm.u.4
summary(gbm.gsm.u.4)
gbm.perf(gbm.gsm.u.4)

gbm.gsm.u.predict.train <- 10^(predict.gbm(object = gbm.gsm.u.4, newdata = temp.tr.u.train.gsm, 205))
summary(temp.tr.u.train.gsm$upload_speed)
summary(gbm.gsm.u.predict.train)
head(cbind(temp.tr.u.train.gsm$upload_speed, gbm.gsm.u.predict.train), 25)

rmsle.gsm.u.train <- sqrt(1 / length(gbm.gsm.u.predict.train) * 
                            sum((log1p(gbm.gsm.u.predict.train) - 
                                   log1p(temp.tr.u.train.gsm$upload_speed))^2))

save(gbm.gsm.u.4, file = "gbm.gsm.u.4")

#


# gbm cdma dl

summary(temp.tr.d.train.cdma)
str(temp.tr.d.train.cdma)

library(gbm)
Sys.time()
gbm.cdma.d.4 <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
                  + new_network_country + network_id #+ network_id_sim
                  + phone_model + loc_source_gps_one_net_zero + location_precision
                  + rssi + ec_io
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                  + app_version_code + test_type + unreliable
                  + network_type,
                  data = temp.tr.d.train.cdma, 
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 20,
                  n.minobsinnode = 10,
                  shrinkage = 0.01,
                  keep.data = TRUE,
                  verbose = TRUE)
Sys.time()
gbm.cdma.d.4
summary(gbm.cdma.d.4)
gbm.perf(gbm.cdma.d.4)

gbm.cdma.d.predict.train <- 10^(predict.gbm(object = gbm.cdma.d.4, newdata = temp.tr.d.train.cdma, 284))
summary(temp.tr.d.train.cdma$download_speed)
summary(gbm.cdma.d.predict.train)
head(cbind(temp.tr.d.train.cdma$download_speed, gbm.cdma.d.predict.train), 25)

rmsle.cdma.d.train <- sqrt(1 / length(gbm.cdma.d.predict.train) * 
                             sum((log1p(gbm.cdma.d.predict.train) - 
                                    log1p(temp.tr.d.train.cdma$download_speed))^2))

save(gbm.cdma.d.4, file = "gbm.cdma.d.4")


# gbm cdma ul

summary(temp.tr.u.train.cdma)
str(temp.tr.u.train.cdma)

library(gbm)
Sys.time()
gbm.cdma.u.4 <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
                  + new_network_country + network_id #+ network_id_sim
                  + phone_model + loc_source_gps_one_net_zero + location_precision
                  + rssi + ec_io
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                  + app_version_code + test_type + unreliable
                  + network_type,
                  data = temp.tr.u.train.cdma, 
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 20,
                  n.minobsinnode = 10,
                  shrinkage = 0.01,
                  keep.data = TRUE,
                  verbose = TRUE)
Sys.time()
gbm.cdma.u.4
summary(gbm.cdma.u.4)
gbm.perf(gbm.cdma.u.4)

gbm.cdma.u.predict.train <- 10^(predict.gbm(object = gbm.cdma.u.4, newdata = temp.tr.u.train.cdma, 340))
summary(temp.tr.u.train.cdma$upload_speed)
summary(gbm.cdma.u.predict.train)
head(cbind(temp.tr.u.train.cdma$upload_speed, gbm.cdma.u.predict.train), 25)

rmsle.cdma.u.train <- sqrt(1 / length(gbm.cdma.u.predict.train) * 
                             sum((log1p(gbm.cdma.u.predict.train) - 
                                    log1p(temp.tr.u.train.cdma$upload_speed))^2))

save(gbm.cdma.u.4, file = "gbm.cdma.u.4")

#

# gbm wifi dl

summary(temp.tr.d.train.wifi)
str(temp.tr.d.train.wifi)

library(gbm)
Sys.time()
gbm.wifi.d.4 <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
                  + new_network_country + network_id #+ network_id_sim
                  + phone_model + loc_source_gps_one_net_zero + location_precision
                  + wifi_rssi + wifi_band
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                  + app_version_code + test_type + unreliable
                  + network_type,
                  data = temp.tr.d.train.wifi, 
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 20,
                  n.minobsinnode = 10,
                  shrinkage = 0.01,
                  keep.data = TRUE,
                  verbose = TRUE)
Sys.time()
gbm.wifi.d.4
summary(gbm.wifi.d.4)
gbm.perf(gbm.wifi.d.4)

gbm.wifi.d.predict.train <- 10^(predict.gbm(object = gbm.wifi.d.4, newdata = temp.tr.d.train.wifi, 224))
summary(temp.tr.d.train.wifi$download_speed)
summary(gbm.wifi.d.predict.train)
head(cbind(temp.tr.d.train.wifi$download_speed, gbm.wifi.d.predict.train), 25)

rmsle.wifi.d.train <- sqrt(1 / length(gbm.wifi.d.predict.train) * 
                             sum((log1p(gbm.wifi.d.predict.train) - 
                                    log1p(temp.tr.d.train.wifi$download_speed))^2))

save(gbm.wifi.d.4, file = "gbm.wifi.d.4")


# gbm wifi ul

summary(temp.tr.u.train.wifi)
str(temp.tr.u.train.wifi)

library(gbm)
Sys.time()
gbm.wifi.u.4 <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
                  + new_network_country + network_id #+ network_id_sim
                  + phone_model + loc_source_gps_one_net_zero + location_precision
                  + wifi_rssi + wifi_band
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                  + app_version_code + test_type + unreliable
                  + network_type,
                  data = temp.tr.u.train.wifi, 
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 20,
                  n.minobsinnode = 10,
                  shrinkage = 0.01,
                  keep.data = TRUE,
                  verbose = TRUE)
Sys.time()
gbm.wifi.u.4
summary(gbm.wifi.u.4)
gbm.perf(gbm.wifi.u.4)

gbm.wifi.u.predict.train <- 10^(predict.gbm(object = gbm.wifi.u.4, newdata = temp.tr.u.train.wifi, 180))
summary(temp.tr.u.train.wifi$upload_speed)
summary(gbm.wifi.u.predict.train)
head(cbind(temp.tr.u.train.wifi$upload_speed, gbm.wifi.u.predict.train), 25)

rmsle.wifi.u.train <- sqrt(1 / length(gbm.wifi.u.predict.train) * 
                             sum((log1p(gbm.wifi.u.predict.train) - 
                                    log1p(temp.tr.u.train.wifi$upload_speed))^2))

save(gbm.wifi.u.4, file = "gbm.wifi.u.4")

#


# gbm oos dl

summary(temp.tr.d.train.oos)
str(temp.tr.d.train.oos)

library(gbm)
Sys.time()
gbm.oos.d.4 <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
                 + new_network_country + network_id #+ network_id_sim
                 + phone_model + loc_source_gps_one_net_zero + location_precision
                 + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                 + app_version_code + test_type + unreliable,
                 data = temp.tr.d.train.oos, 
                 distribution = "gaussian",
                 n.trees = 1000,
                 interaction.depth = 20,
                 n.minobsinnode = 10,
                 shrinkage = 0.01,
                 keep.data = TRUE,
                 verbose = TRUE)
Sys.time()
gbm.oos.d.4
summary(gbm.oos.d.4)
gbm.perf(gbm.oos.d.4)

gbm.oos.d.predict.train <- 10^(predict.gbm(object = gbm.oos.d.4, newdata = temp.tr.d.train.oos, 122))
summary(temp.tr.d.train.oos$download_speed)
summary(gbm.oos.d.predict.train)
head(cbind(temp.tr.d.train.oos$download_speed, gbm.oos.d.predict.train), 25)

rmsle.oos.d.train <- sqrt(1 / length(gbm.oos.d.predict.train) * 
                            sum((log1p(gbm.oos.d.predict.train) - 
                                   log1p(temp.tr.d.train.oos$download_speed))^2))

save(gbm.oos.d.4, file = "gbm.oos.d.4")


# gbm oos ul

summary(temp.tr.u.train.oos)
str(temp.tr.u.train.oos)

library(gbm)
Sys.time()
gbm.oos.u.4 <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
                 + new_network_country + network_id #+ network_id_sim
                 + phone_model + loc_source_gps_one_net_zero + location_precision
                 + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                 + app_version_code + test_type + unreliable,
                 data = temp.tr.u.train.oos, 
                 distribution = "gaussian",
                 n.trees = 1000,
                 interaction.depth = 20,
                 n.minobsinnode = 10,
                 shrinkage = 0.01,
                 keep.data = TRUE,
                 verbose = TRUE)
Sys.time()
gbm.oos.u.4
summary(gbm.oos.u.4)
gbm.perf(gbm.oos.u.4)

gbm.oos.u.predict.train <- 10^(predict.gbm(object = gbm.oos.u.4, newdata = temp.tr.u.train.oos, 131))
summary(temp.tr.u.train.oos$upload_speed)
summary(gbm.oos.u.predict.train)
head(cbind(temp.tr.u.train.oos$upload_speed, gbm.oos.u.predict.train), 25)

rmsle.oos.u.train <- sqrt(1 / length(gbm.oos.u.predict.train) * 
                            sum((log1p(gbm.oos.u.predict.train) - 
                                   log1p(temp.tr.u.train.oos$upload_speed))^2))

save(gbm.oos.u.4, file = "gbm.oos.u.4")

#




##################################################
#PREDICTING

load("evalset.for.gbm.2")
str(evalset)

# nw_type

evalset$nw_type <- NA
evalset[which(evalset$network_type == "LTE"),]$nw_type <- "LTE"
evalset[which(evalset$network_type == "1xRTT" | evalset$network_type == "eHRPD" | evalset$network_type == "EVDO 0" | 
             evalset$network_type == "EVDO A" | evalset$network_type == "EVDO B" | evalset$network_type == "CDMA"),]$nw_type <- "CDMA"
evalset[which(evalset$network_type == "EDGE" | evalset$network_type == "GPRS"),]$nw_type <- "GSM"
evalset[which(evalset$network_type == "HSDPA" | evalset$network_type == "HSUPA" | evalset$network_type == "HSPA" | 
             evalset$network_type == "HSPAP" | evalset$network_type == "UMTS"),]$nw_type <- "UMTS"
evalset[which(evalset$network_type == "Out of service"),]$nw_type <- "OOS"
evalset[which(evalset$is_wifi),]$nw_type <- "WIFI"
evalset$nw_type <- factor(evalset$nw_type)
summary(evalset$nw_type)



library(caret)
library(dplyr)
set.seed(16)

evalset.predict.gbm <- select(evalset, 
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

str(evalset.predict.gbm)
evalset.predict.gbm.backup <- evalset.predict.gbm
str(temp.tr)
evalset.predict.gbm$new_network_country <- factor(evalset.predict.gbm$new_network_country, levels = levels(temp.tr$new_network_country))
evalset.predict.gbm$network_id <- factor(evalset.predict.gbm$network_id, levels = levels(temp.tr$network_id))
evalset.predict.gbm$network_id_sim <- factor(evalset.predict.gbm$network_id_sim, levels = levels(temp.tr$network_id_sim))
evalset.predict.gbm$phone_model <- factor(evalset.predict.gbm$phone_model, levels = levels(temp.tr$phone_model))
str(evalset.predict.gbm)
str(temp.tr)

library(gbm)
evalset.predict.gbm$lte.d <- 10^(predict.gbm(object = gbm.lte.d.4, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$lte.u <- 10^(predict.gbm(object = gbm.lte.u.4, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$gsm.d <- 10^(predict.gbm(object = gbm.gsm.d.4, newdata = evalset.predict.gbm, 150))
evalset.predict.gbm$gsm.u <- 10^(predict.gbm(object = gbm.gsm.u.4, newdata = evalset.predict.gbm, 250))
evalset.predict.gbm$umts.d <- 10^(predict.gbm(object = gbm.umts.d.4, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$umts.u <- 10^(predict.gbm(object = gbm.umts.u.4, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$cdma.d <- 10^(predict.gbm(object = gbm.cdma.d.4, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$cdma.u <- 10^(predict.gbm(object = gbm.cdma.u.4, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$oos.d <- 10^(predict.gbm(object = gbm.oos.d.4, newdata = evalset.predict.gbm, 200))
evalset.predict.gbm$oos.u <- 10^(predict.gbm(object = gbm.oos.u.4, newdata = evalset.predict.gbm, 200))
evalset.predict.gbm$wifi.d <- 10^(predict.gbm(object = gbm.wifi.d.4, newdata = evalset.predict.gbm, 300))
evalset.predict.gbm$wifi.u <- 10^(predict.gbm(object = gbm.wifi.u.4, newdata = evalset.predict.gbm, 300))

summary(evalset.predict.gbm)
save(evalset.predict.gbm, file = "evalset.predict.gbm.3")

#
evalset.predict.gbm$download_speed <- as.character(levels(evalset.predict.gbm$download_speed))[evalset.predict.gbm$download_speed]
table(evalset.predict.gbm$download_speed)
evalset.predict.gbm$upload_speed <- as.character(levels(evalset.predict.gbm$upload_speed))[evalset.predict.gbm$upload_speed]
table(evalset.predict.gbm$upload_speed)
#

summary(evalset$nw_type)
summary(evalset.predict.gbm$nw_type)


#Replacing PREDICTs with speeds
evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "LTE"),]$download_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "LTE"),]$lte.d
evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "LTE"),]$upload_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "LTE"),]$lte.u

evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "GSM"),]$download_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "GSM"),]$gsm.d
evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "GSM"),]$upload_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "GSM"),]$gsm.u

evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "UMTS"),]$download_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "UMTS"),]$umts.d
evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "UMTS"),]$upload_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "UMTS"),]$umts.u

evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "CDMA"),]$download_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "CDMA"),]$cdma.d
evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "CDMA"),]$upload_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "CDMA"),]$cdma.u

evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "OOS"),]$download_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "OOS"),]$oos.d
evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "OOS"),]$upload_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "OOS"),]$oos.u

evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "WIFI"),]$download_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "WIFI"),]$wifi.d
evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "WIFI"),]$upload_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "WIFI"),]$wifi.u



#checking missing speeds
length(evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT"),]$download_speed)
length(evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "SKIP"),]$download_speed)
length(evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT"),]$upload_speed)
length(evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "SKIP"),]$upload_speed)


#Saving outputs

write.table(select(evalset.predict.gbm, upload_speed, download_speed), file = "YetAnotherTeam_final_test_7.csv", sep = ",", row.names = F)

