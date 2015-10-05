#data loading
setwd("C:\\freelancervvp\\Projects\\2015_Data_Mining\\new_data")
load("temp.new.2")

str(temp)

library(dplyr)
temp.tr <- select(temp, 
                  download_speed, upload_speed, 
                  new_month, new_year, new_weekday, new_hour, tz,
                  new_network_country,
                  network_id, network_id_sim,
                  phone_model,
                  loc_source_gps_one_net_zero, location_precision,
                  rssi, 
                  ec_io, 
                  rsrp, rsrq, rssnr, pci,
                  wifi_rssi, wifi_band,
                  icmp_ping_time, icmp_ping_packet_loss, icmp_ping_range,
                  app_version_code, 
                  test_type, unreliable, screen_state, release_code,
                  network_type, nw_type)
str(temp.tr)
summary(temp.tr)


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


# gbm lte dl

summary(temp.tr.d.train.lte)
str(temp.tr.d.train.lte)

library(gbm)
library(caret)
Sys.time()
gbm.lte.d.5 <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour + tz
                   + new_network_country + network_id + network_id_sim
                   + phone_model + loc_source_gps_one_net_zero + location_precision
                   + rsrp + rsrq + rssnr + pci
                   + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                   + app_version_code + test_type + unreliable + screen_state + release_code, 
                   data = temp.tr.d.train.lte, 
                   distribution = "gaussian",
                   n.trees = 1000,
                   interaction.depth = 20,
                   n.minobsinnode = 10,
                   shrinkage = 0.01,
                   keep.data = TRUE,
                   verbose = TRUE)
Sys.time()
gbm.lte.d.5
summary(gbm.lte.d.5)
gbm.perf(gbm.lte.d.5)

gbm.lte.d.predict.train <- 10^(predict.gbm(object = gbm.lte.d.5, newdata = temp.tr.d.train.lte, 900))
summary(temp.tr.d.train.lte$download_speed)
summary(gbm.lte.d.predict.train)
head(cbind(temp.tr.d.train.lte$download_speed, gbm.lte.d.predict.train), 25)

gbm.lte.d.predict.test <- 10^(predict.gbm(object = gbm.lte.d.5, newdata = temp.tr.d.test.lte, 900))
summary(temp.tr.d.test.lte$download_speed)
summary(gbm.lte.d.test.train)
head(cbind(temp.tr.d.test.lte$download_speed, gbm.lte.d.predict.test), 25)

rmsle.lte.d.train <- sqrt(1 / length(gbm.lte.d.predict.train) * 
                                  sum((log1p(gbm.lte.d.predict.train) - 
                                               log1p(temp.tr.d.train.lte$download_speed))^2))
rmsle.lte.d.test <- sqrt(1 / length(gbm.lte.d.predict.test) * 
                                 sum((log1p(gbm.lte.d.predict.test) - 
                                              log1p(temp.tr.d.test.lte$download_speed))^2))

save(gbm.lte.d.5, file = "gbm.lte.d.5")


# gbm lte ul

summary(temp.tr.u.train.lte)
str(temp.tr.u.train.lte)

library(gbm)
Sys.time()
gbm.lte.u.5 <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour + tz
                   + new_network_country + network_id + network_id_sim
                   + phone_model + loc_source_gps_one_net_zero + location_precision
                   + rsrp + rsrq + rssnr + pci
                   + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                   + app_version_code + test_type + unreliable + screen_state + release_code, 
                   data = temp.tr.u.train.lte, 
                   distribution = "gaussian",
                   n.trees = 1000,
                   interaction.depth = 20,
                   n.minobsinnode = 10,
                   shrinkage = 0.01,
                   keep.data = TRUE,
                   verbose = TRUE)
Sys.time()
gbm.lte.u.5
summary(gbm.lte.u.5)
gbm.perf(gbm.lte.u.5)
#pretty.gbm.tree(gbm.lte.u, i.tree = 500)

gbm.lte.u.predict.train <- 10^(predict.gbm(object = gbm.lte.u.5, newdata = temp.tr.u.train.lte, 1000))
summary(temp.tr.u.train.lte$upload_speed)
summary(gbm.lte.u.predict.train)
head(cbind(temp.tr.u.train.lte$upload_speed, gbm.lte.u.predict.train), 25)

gbm.lte.u.predict.test <- 10^(predict.gbm(object = gbm.lte.u.5, newdata = temp.tr.u.test.lte, 1000))
summary(temp.tr.u.test.lte$upload_speed)
summary(gbm.lte.u.predict.test)
head(cbind(temp.tr.u.test.lte$upload_speed, gbm.lte.u.predict.test), 25)

rmsle.lte.u.train <- sqrt(1 / length(gbm.lte.u.predict.train) * 
                                  sum((log1p(gbm.lte.u.predict.train) - 
                                               log1p(temp.tr.u.train.lte$upload_speed))^2))
rmsle.lte.u.test <- sqrt(1 / length(gbm.lte.u.predict.test) * 
                                 sum((log1p(gbm.lte.u.predict.test) - 
                                              log1p(temp.tr.u.test.lte$upload_speed))^2))

save(gbm.lte.u.5, file = "gbm.lte.u.5")

#


# gbm umts dl

summary(temp.tr.d.train.umts)
str(temp.tr.d.train.umts)

library(gbm)
Sys.time()
gbm.umts.d.5 <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour + tz
                    + new_network_country + network_id + network_id_sim
                    + phone_model + loc_source_gps_one_net_zero + location_precision
                    + rssi
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                    + app_version_code + test_type + unreliable + screen_state + release_code
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
gbm.umts.d.5
summary(gbm.umts.d.5)
gbm.perf(gbm.umts.d.5)

gbm.umts.d.predict.train <- 10^(predict.gbm(object = gbm.umts.d.5, newdata = temp.tr.d.train.umts, 750))
summary(temp.tr.d.train.umts$download_speed)
summary(gbm.umts.d.predict.train)
head(cbind(temp.tr.d.train.umts$download_speed, gbm.umts.d.predict.train), 25)

gbm.umts.d.predict.test <- 10^(predict.gbm(object = gbm.umts.d.5, newdata = temp.tr.d.test.umts, 750))
head(cbind(temp.tr.d.test.umts$download_speed, gbm.umts.d.predict.test), 25)

rmsle.umts.d.train <- sqrt(1 / length(gbm.umts.d.predict.train) * 
                                   sum((log1p(gbm.umts.d.predict.train) - 
                                                log1p(temp.tr.d.train.umts$download_speed))^2))
rmsle.umts.d.test <- sqrt(1 / length(gbm.umts.d.predict.test) * 
                                  sum((log1p(gbm.umts.d.predict.test) - 
                                               log1p(temp.tr.d.test.umts$download_speed))^2))

save(gbm.umts.d.5, file = "gbm.umts.d.5")


# gbm umts ul

summary(temp.tr.u.train.umts)
str(temp.tr.u.train.umts)

library(gbm)
Sys.time()
gbm.umts.u.5 <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour + tz
                    + new_network_country + network_id + network_id_sim
                    + phone_model + loc_source_gps_one_net_zero + location_precision
                    + rssi
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                    + app_version_code + test_type + unreliable + screen_state + release_code
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
gbm.umts.u.5
summary(gbm.umts.u.5)
gbm.perf(gbm.umts.u.5)
#pretty.gbm.tree(gbm.umts.u, i.tree = 500)

gbm.umts.u.predict.train <- 10^(predict.gbm(object = gbm.umts.u.5, newdata = temp.tr.u.train.umts, 800))
summary(temp.tr.u.train.umts$upload_speed)
summary(gbm.umts.u.predict.train)
head(cbind(temp.tr.u.train.umts$upload_speed, gbm.umts.u.predict.train), 25)

gbm.umts.u.predict.test <- 10^(predict.gbm(object = gbm.umts.u.5, newdata = temp.tr.u.test.umts, 800))
summary(temp.tr.u.test.umts$upload_speed)
summary(gbm.umts.u.predict.test)
head(cbind(temp.tr.u.test.umts$upload_speed, gbm.umts.u.predict.test), 25)

rmsle.umts.u.train <- sqrt(1 / length(gbm.umts.u.predict.train) * 
                                   sum((log1p(gbm.umts.u.predict.train) - 
                                                log1p(temp.tr.u.train.umts$upload_speed))^2))
rmsle.umts.u.test <- sqrt(1 / length(gbm.umts.u.predict.test) * 
                                  sum((log1p(gbm.umts.u.predict.test) - 
                                               log1p(temp.tr.u.test.umts$upload_speed))^2))

save(gbm.umts.u.5, file = "gbm.umts.u.5")

#

# gbm gsm dl

summary(temp.tr.d.train.gsm)
str(temp.tr.d.train.gsm)

library(gbm)
Sys.time()
gbm.gsm.d.5 <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour + tz
                   + new_network_country + network_id + network_id_sim
                   + phone_model + loc_source_gps_one_net_zero + location_precision
                   + rssi
                   + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                   + app_version_code + test_type + unreliable  + screen_state + release_code
                   + network_type,
                   data = temp.tr.d.train.gsm, 
                   distribution = "gaussian",
                   n.trees = 1000,
                   interaction.depth = 20,
                   n.minobsinnode = 10,
                   shrinkage = 0.01,
                   keep.data = TRUE,
                   verbose = TRUE)
Sys.time()
gbm.gsm.d.5
summary(gbm.gsm.d.5)
gbm.perf(gbm.gsm.d.5)

gbm.gsm.d.predict.train <- 10^(predict.gbm(object = gbm.gsm.d.5, newdata = temp.tr.d.train.gsm, 60))
summary(temp.tr.d.train.gsm$download_speed)
summary(gbm.gsm.d.predict.train)
head(cbind(temp.tr.d.train.gsm$download_speed, gbm.gsm.d.predict.train), 25)

gbm.gsm.d.predict.test <- 10^(predict.gbm(object = gbm.gsm.d.5, newdata = temp.tr.d.test.gsm, 60))
head(cbind(temp.tr.d.test.gsm$download_speed, gbm.gsm.d.predict.test), 25)

rmsle.gsm.d.train <- sqrt(1 / length(gbm.gsm.d.predict.train) * 
                                  sum((log1p(gbm.gsm.d.predict.train) - 
                                               log1p(temp.tr.d.train.gsm$download_speed))^2))
rmsle.gsm.d.test <- sqrt(1 / length(gbm.gsm.d.predict.test) * 
                                 sum((log1p(gbm.gsm.d.predict.test) - 
                                              log1p(temp.tr.d.test.gsm$download_speed))^2))

save(gbm.gsm.d.5, file = "gbm.gsm.d.5")


# gbm gsm ul

summary(temp.tr.u.train.gsm)
str(temp.tr.u.train.gsm)

library(gbm)
Sys.time()
gbm.gsm.u.5 <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour + tz
                   + new_network_country + network_id + network_id_sim
                   + phone_model + loc_source_gps_one_net_zero + location_precision
                   + rssi
                   + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                   + app_version_code + test_type + unreliable + screen_state + release_code
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
gbm.gsm.u.5
summary(gbm.gsm.u.5)
gbm.perf(gbm.gsm.u.5)

gbm.gsm.u.predict.train <- 10^(predict.gbm(object = gbm.gsm.u.5, newdata = temp.tr.u.train.gsm, 200))
summary(temp.tr.u.train.gsm$upload_speed)
summary(gbm.gsm.u.predict.train)
head(cbind(temp.tr.u.train.gsm$upload_speed, gbm.gsm.u.predict.train), 25)

gbm.gsm.u.predict.test <- 10^(predict.gbm(object = gbm.gsm.u.5, newdata = temp.tr.u.test.gsm, 200))
summary(temp.tr.u.test.gsm$upload_speed)
summary(gbm.gsm.u.predict.test)
head(cbind(temp.tr.u.test.gsm$upload_speed, gbm.gsm.u.predict.test), 25)

rmsle.gsm.u.train <- sqrt(1 / length(gbm.gsm.u.predict.train) * 
                                  sum((log1p(gbm.gsm.u.predict.train) - 
                                               log1p(temp.tr.u.train.gsm$upload_speed))^2))
rmsle.gsm.u.test <- sqrt(1 / length(gbm.gsm.u.predict.test) * 
                                 sum((log1p(gbm.gsm.u.predict.test) - 
                                              log1p(temp.tr.u.test.gsm$upload_speed))^2))

save(gbm.gsm.u.5, file = "gbm.gsm.u.5")

#


# gbm cdma dl

summary(temp.tr.d.train.cdma)
str(temp.tr.d.train.cdma)

library(gbm)
Sys.time()
gbm.cdma.d.5 <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour + tz
                    + new_network_country + network_id + network_id_sim
                    + phone_model + loc_source_gps_one_net_zero + location_precision
                    + rssi + ec_io
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                    + app_version_code + test_type + unreliable + screen_state + release_code
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
gbm.cdma.d.5
summary(gbm.cdma.d.5)
gbm.perf(gbm.cdma.d.5)

gbm.cdma.d.predict.train <- 10^(predict.gbm(object = gbm.cdma.d.5, newdata = temp.tr.d.train.cdma, 1000))
summary(temp.tr.d.train.cdma$download_speed)
summary(gbm.cdma.d.predict.train)
head(cbind(temp.tr.d.train.cdma$download_speed, gbm.cdma.d.predict.train), 25)

gbm.cdma.d.predict.test <- 10^(predict.gbm(object = gbm.cdma.d.5, newdata = temp.tr.d.test.cdma, 1000))
head(cbind(temp.tr.d.test.cdma$download_speed, gbm.cdma.d.predict.test), 25)

rmsle.cdma.d.train <- sqrt(1 / length(gbm.cdma.d.predict.train) * 
                                   sum((log1p(gbm.cdma.d.predict.train) - 
                                                log1p(temp.tr.d.train.cdma$download_speed))^2))
rmsle.cdma.d.test <- sqrt(1 / length(gbm.cdma.d.predict.test) * 
                                  sum((log1p(gbm.cdma.d.predict.test) - 
                                               log1p(temp.tr.d.test.cdma$download_speed))^2))

save(gbm.cdma.d.5, file = "gbm.cdma.d.5")


# gbm cdma ul

summary(temp.tr.u.train.cdma)
str(temp.tr.u.train.cdma)

library(gbm)
Sys.time()
gbm.cdma.u.5 <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour + tz
                    + new_network_country + network_id + network_id_sim
                    + phone_model + loc_source_gps_one_net_zero + location_precision
                    + rssi + ec_io
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                    + app_version_code + test_type + unreliable + screen_state + release_code
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
gbm.cdma.u.5
summary(gbm.cdma.u.5)
gbm.perf(gbm.cdma.u.5)

gbm.cdma.u.predict.train <- 10^(predict.gbm(object = gbm.cdma.u.5, newdata = temp.tr.u.train.cdma, 900))
summary(temp.tr.u.train.cdma$upload_speed)
summary(gbm.cdma.u.predict.train)
head(cbind(temp.tr.u.train.cdma$upload_speed, gbm.cdma.u.predict.train), 25)

gbm.cdma.u.predict.test <- 10^(predict.gbm(object = gbm.cdma.u.5, newdata = temp.tr.u.test.cdma, 900))
summary(temp.tr.u.test.cdma$upload_speed)
summary(gbm.cdma.u.predict.test)
head(cbind(temp.tr.u.test.cdma$upload_speed, gbm.cdma.u.predict.test), 25)

rmsle.cdma.u.train <- sqrt(1 / length(gbm.cdma.u.predict.train) * 
                                   sum((log1p(gbm.cdma.u.predict.train) - 
                                                log1p(temp.tr.u.train.cdma$upload_speed))^2))
rmsle.cdma.u.test <- sqrt(1 / length(gbm.cdma.u.predict.test) * 
                                  sum((log1p(gbm.cdma.u.predict.test) - 
                                               log1p(temp.tr.u.test.cdma$upload_speed))^2))

save(gbm.cdma.u.5, file = "gbm.cdma.u.5")

#

# gbm wifi dl

summary(temp.tr.d.train.wifi)
str(temp.tr.d.train.wifi)

library(gbm)
Sys.time()
gbm.wifi.d.5 <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour + tz
                    + new_network_country + network_id + network_id_sim
                    + phone_model + loc_source_gps_one_net_zero + location_precision
                    + wifi_rssi + wifi_band
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                    + app_version_code + test_type + unreliable + screen_state + release_code
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
gbm.wifi.d.5
summary(gbm.wifi.d.5)
gbm.perf(gbm.wifi.d.5)

gbm.wifi.d.predict.train <- 10^(predict.gbm(object = gbm.wifi.d.5, newdata = temp.tr.d.train.wifi, 300))
summary(temp.tr.d.train.wifi$download_speed)
summary(gbm.wifi.d.predict.train)
head(cbind(temp.tr.d.train.wifi$download_speed, gbm.wifi.d.predict.train), 25)

gbm.wifi.d.predict.test <- 10^(predict.gbm(object = gbm.wifi.d.5, newdata = temp.tr.d.test.wifi, 300))
head(cbind(temp.tr.d.test.wifi$download_speed, gbm.wifi.d.predict.test), 25)

rmsle.wifi.d.train <- sqrt(1 / length(gbm.wifi.d.predict.train) * 
                                   sum((log1p(gbm.wifi.d.predict.train) - 
                                                log1p(temp.tr.d.train.wifi$download_speed))^2))
rmsle.wifi.d.test <- sqrt(1 / length(gbm.wifi.d.predict.test) * 
                                  sum((log1p(gbm.wifi.d.predict.test) - 
                                               log1p(temp.tr.d.test.wifi$download_speed))^2))

save(gbm.wifi.d.5, file = "gbm.wifi.d.5")


# gbm wifi ul

summary(temp.tr.u.train.wifi)
str(temp.tr.u.train.wifi)

library(gbm)
Sys.time()
gbm.wifi.u.5 <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour + tz
                    + new_network_country + network_id + network_id_sim
                    + phone_model + loc_source_gps_one_net_zero + location_precision
                    + wifi_rssi + wifi_band
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                    + app_version_code + test_type + unreliable + screen_state + release_code
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
gbm.wifi.u.5
summary(gbm.wifi.u.5)
gbm.perf(gbm.wifi.u.5)

gbm.wifi.u.predict.train <- 10^(predict.gbm(object = gbm.wifi.u.5, newdata = temp.tr.u.train.wifi, 180))
summary(temp.tr.u.train.wifi$upload_speed)
summary(gbm.wifi.u.predict.train)
head(cbind(temp.tr.u.train.wifi$upload_speed, gbm.wifi.u.predict.train), 25)

gbm.wifi.u.predict.test <- 10^(predict.gbm(object = gbm.wifi.u.5, newdata = temp.tr.u.test.wifi, 180))
summary(temp.tr.u.test.wifi$upload_speed)
summary(gbm.wifi.u.predict.test)
head(cbind(temp.tr.u.test.wifi$upload_speed, gbm.wifi.u.predict.test), 25)

rmsle.wifi.u.train <- sqrt(1 / length(gbm.wifi.u.predict.train) * 
                                   sum((log1p(gbm.wifi.u.predict.train) - 
                                                log1p(temp.tr.u.train.wifi$upload_speed))^2))
rmsle.wifi.u.test <- sqrt(1 / length(gbm.wifi.u.predict.test) * 
                                  sum((log1p(gbm.wifi.u.predict.test) - 
                                               log1p(temp.tr.u.test.wifi$upload_speed))^2))

save(gbm.wifi.u.5, file = "gbm.wifi.u.5")

#


# gbm oos dl

summary(temp.tr.d.train.oos)
str(temp.tr.d.train.oos)

library(gbm)
Sys.time()
gbm.oos.d.5 <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour + tz
                   + new_network_country + network_id + network_id_sim
                   + phone_model + loc_source_gps_one_net_zero + location_precision
                   + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                   + app_version_code + test_type + unreliable + screen_state + release_code,
                   data = temp.tr.d.train.oos, 
                   distribution = "gaussian",
                   n.trees = 1000,
                   interaction.depth = 20,
                   n.minobsinnode = 10,
                   shrinkage = 0.01,
                   keep.data = TRUE,
                   verbose = TRUE)
Sys.time()
gbm.oos.d.5
summary(gbm.oos.d.5)
gbm.perf(gbm.oos.d.5)

gbm.oos.d.predict.train <- 10^(predict.gbm(object = gbm.oos.d.5, newdata = temp.tr.d.train.oos, 92))
summary(temp.tr.d.train.oos$download_speed)
summary(gbm.oos.d.predict.train)
head(cbind(temp.tr.d.train.oos$download_speed, gbm.oos.d.predict.train), 25)

gbm.oos.d.predict.test <- 10^(predict.gbm(object = gbm.oos.d.5, newdata = temp.tr.d.test.oos, 92))
head(cbind(temp.tr.d.test.oos$download_speed, gbm.oos.d.predict.test), 25)

rmsle.oos.d.train <- sqrt(1 / length(gbm.oos.d.predict.train) * 
                                  sum((log1p(gbm.oos.d.predict.train) - 
                                               log1p(temp.tr.d.train.oos$download_speed))^2))
rmsle.oos.d.test <- sqrt(1 / length(gbm.oos.d.predict.test) * 
                                 sum((log1p(gbm.oos.d.predict.test) - 
                                              log1p(temp.tr.d.test.oos$download_speed))^2))

save(gbm.oos.d.5, file = "gbm.oos.d.5")


# gbm oos ul

summary(temp.tr.u.train.oos)
str(temp.tr.u.train.oos)

library(gbm)
Sys.time()
gbm.oos.u.5 <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour + tz
                   + new_network_country + network_id + network_id_sim
                   + phone_model + loc_source_gps_one_net_zero + location_precision
                   + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                   + app_version_code + test_type + unreliable + screen_state + release_code,
                   data = temp.tr.u.train.oos, 
                   distribution = "gaussian",
                   n.trees = 1000,
                   interaction.depth = 20,
                   n.minobsinnode = 10,
                   shrinkage = 0.01,
                   keep.data = TRUE,
                   verbose = TRUE)
Sys.time()
gbm.oos.u.5
summary(gbm.oos.u.5)
gbm.perf(gbm.oos.u.5)

gbm.oos.u.predict.train <- 10^(predict.gbm(object = gbm.oos.u.5, newdata = temp.tr.u.train.oos, 122))
summary(temp.tr.u.train.oos$upload_speed)
summary(gbm.oos.u.predict.train)
head(cbind(temp.tr.u.train.oos$upload_speed, gbm.oos.u.predict.train), 25)

gbm.oos.u.predict.test <- 10^(predict.gbm(object = gbm.oos.u.5, newdata = temp.tr.u.test.oos, 122))
summary(temp.tr.u.test.oos$upload_speed)
summary(gbm.oos.u.predict.test)
head(cbind(temp.tr.u.test.oos$upload_speed, gbm.oos.u.predict.test), 25)

rmsle.oos.u.train <- sqrt(1 / length(gbm.oos.u.predict.train) * 
                                  sum((log1p(gbm.oos.u.predict.train) - 
                                               log1p(temp.tr.u.train.oos$upload_speed))^2))
rmsle.oos.u.test <- sqrt(1 / length(gbm.oos.u.predict.test) * 
                                 sum((log1p(gbm.oos.u.predict.test) - 
                                              log1p(temp.tr.u.test.oos$upload_speed))^2))

save(gbm.oos.u.5, file = "gbm.oos.u.5")

#
#

#goto new_data_gbm_evalset.R