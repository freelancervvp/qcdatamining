#data loading
setwd("C:\\freelancervvp\\Projects\\2015_Data_Mining\\new_data")
load("temp.new")

#temp nw_type

summary(temp$nw_type)
temp$nw_type <- NA
temp[which(temp$network_type == "LTE"),]$nw_type <- "LTE"

temp[which(temp$network_type == "1xRTT" | temp$network_type == "eHRPD" | temp$network_type == "EVDO 0" | 
             temp$network_type == "EVDO A" | temp$network_type == "EVDO B" | temp$network_type == "CDMA"),]$nw_type <- "CDMA"

temp[which(temp$network_type == "EDGE" | temp$network_type == "GPRS"),]$nw_type <- "GSM"

temp[which(temp$network_type == "HSDPA" | temp$network_type == "HSUPA"),]$nw_type <- "HSDPA"
temp[which(temp$network_type == "HSPA"),]$nw_type <- "HSPA"
temp[which(temp$network_type == "HSPAP"),]$nw_type <- "HSPAP"
temp[which(temp$network_type == "UMTS"),]$nw_type <- "UMTS"

temp[which(temp$is_wifi),]$nw_type <- "WIFI"
temp[which(temp$network_type == "Out of service"),]$nw_type <- "OOS"

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
summary(temp.tr)

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

library(caret)
set.seed(16)
trainIndex <- createDataPartition(temp.tr.d$download_speed, p = .8, list = FALSE)
temp.tr.d.train <- temp.tr.d[trainIndex,]
temp.tr.d.test <- temp.tr.d[-trainIndex,]

temp.tr.d.train.umts <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "UMTS"),]
temp.tr.d.train.hsdpa <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "HSDPA"),]
temp.tr.d.train.hspa <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "HSPA"),]
temp.tr.d.train.hspap <- temp.tr.d.train[which(temp.tr.d.train$nw_type == "HSPAP"),]

temp.tr.d.test.umts <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "UMTS"),]
temp.tr.d.test.hsdpa <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "HSDPA"),]
temp.tr.d.test.hspa <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "HSPA"),]
temp.tr.d.test.hspap <- temp.tr.d.test[which(temp.tr.d.test$nw_type == "HSPAP"),]

#ul
temp.tr.u <- temp.tr[which(!is.na(temp.tr$upload_speed)),]

library(caret)
set.seed(16)
trainIndex <- createDataPartition(temp.tr.u$upload_speed, p = .8, list = FALSE)
temp.tr.u.train <- temp.tr.u[trainIndex,]
temp.tr.u.test <- temp.tr.u[-trainIndex,]

temp.tr.u.train.umts <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "UMTS"),]
temp.tr.u.train.hsdpa <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "HSDPA"),]
temp.tr.u.train.hspa <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "HSPA"),]
temp.tr.u.train.hspap <- temp.tr.u.train[which(temp.tr.u.train$nw_type == "HSPAP"),]

temp.tr.u.test.umts <- temp.tr.u.test[which(temp.tr.u.test$nw_type == "UMTS"),]
temp.tr.u.test.hsdpa <- temp.tr.u.test[which(temp.tr.u.test$nw_type == "HSDPA"),]
temp.tr.u.test.hspa <- temp.tr.u.test[which(temp.tr.u.test$nw_type == "HSPA"),]
temp.tr.u.test.hspap <- temp.tr.u.test[which(temp.tr.u.test$nw_type == "HSPAP"),]



# gbm umts dl

summary(temp.tr.d.train.umts)
str(temp.tr.d.train.umts)

library(gbm)
Sys.time()
gbm.umts.d.split <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
                  + new_network_country + network_id #+ network_id_sim
                  + phone_model + loc_source_gps_one_net_zero + location_precision
                  + rssi
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                  + app_version_code + test_type + unreliable, 
                  data = temp.tr.d.train.umts, 
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 20,
                  n.minobsinnode = 10,
                  shrinkage = 0.01,
                  keep.data = TRUE,
                  verbose = TRUE)
Sys.time()
gbm.umts.d.split
summary(gbm.umts.d.split)
gbm.perf(gbm.umts.d.split)

gbm.umts.d.predict.train <- 10^(predict.gbm(object = gbm.umts.d.split, newdata = temp.tr.d.train.umts, 80))
summary(temp.tr.d.train.umts$download_speed)
summary(gbm.umts.d.predict.train)
head(cbind(temp.tr.d.train.umts$download_speed, gbm.umts.d.predict.train), 25)
gbm.umts.d.predict.test <- 10^(predict.gbm(object = gbm.umts.d.split, newdata = temp.tr.d.test.umts, 80))
head(cbind(temp.tr.d.test.umts$download_speed, gbm.umts.d.predict.test), 25)

rmsle.umts.d.train <- sqrt(1 / length(gbm.umts.d.predict.train) * 
                             sum((log1p(gbm.umts.d.predict.train) - 
                                    log1p(temp.tr.d.train.umts$download_speed))^2))
rmsle.umts.d.test <- sqrt(1 / length(gbm.umts.d.predict.test) * 
                            sum((log1p(gbm.umts.d.predict.test) - 
                                   log1p(temp.tr.d.test.umts$download_speed))^2))

save(gbm.umts.d.split, file = "gbm.umts.d.split")


# gbm umts ul

summary(temp.tr.u.train.umts)
str(temp.tr.u.train.umts)

library(gbm)
Sys.time()
gbm.umts.u.split <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
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
gbm.umts.u.split
summary(gbm.umts.u.split)
gbm.perf(gbm.umts.u.split)
#pretty.gbm.tree(gbm.umts.u, i.tree = 500)

gbm.umts.u.predict.train <- 10^(predict.gbm(object = gbm.umts.u.split, newdata = temp.tr.u.train.umts, 36))
summary(temp.tr.u.train.umts$upload_speed)
summary(gbm.umts.u.predict.train)
head(cbind(temp.tr.u.train.umts$upload_speed, gbm.umts.u.predict.train), 25)

gbm.umts.u.predict.test <- 10^(predict.gbm(object = gbm.umts.u.split, newdata = temp.tr.u.test.umts, 36))
summary(temp.tr.u.test.umts$upload_speed)
summary(gbm.umts.u.predict.test)
head(cbind(temp.tr.u.test.umts$upload_speed, gbm.umts.u.predict.test), 25)

rmsle.umts.u.train <- sqrt(1 / length(gbm.umts.u.predict.train) * 
                             sum((log1p(gbm.umts.u.predict.train) - 
                                    log1p(temp.tr.u.train.umts$upload_speed))^2))
rmsle.umts.u.test <- sqrt(1 / length(gbm.umts.u.predict.test) * 
                            sum((log1p(gbm.umts.u.predict.test) - 
                                   log1p(temp.tr.u.test.umts$upload_speed))^2))

save(gbm.umts.u.split, file = "gbm.umts.u.split")

#
#

# gbm hsdpa dl

summary(temp.tr.d.train.hsdpa)
str(temp.tr.d.train.hsdpa)

library(gbm)
Sys.time()
gbm.hsdpa.d.split <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
                        + new_network_country + network_id #+ network_id_sim
                        + phone_model + loc_source_gps_one_net_zero + location_precision
                        + rssi
                        + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                        + app_version_code + test_type + unreliable, 
                        data = temp.tr.d.train.hsdpa, 
                        distribution = "gaussian",
                        n.trees = 1000,
                        interaction.depth = 20,
                        n.minobsinnode = 10,
                        shrinkage = 0.01,
                        keep.data = TRUE,
                        verbose = TRUE)
Sys.time()
gbm.hsdpa.d.split
summary(gbm.hsdpa.d.split)
gbm.perf(gbm.hsdpa.d.split)

gbm.hsdpa.d.predict.train <- 10^(predict.gbm(object = gbm.hsdpa.d.split, newdata = temp.tr.d.train.hsdpa, 274))
summary(temp.tr.d.train.hsdpa$download_speed)
summary(gbm.hsdpa.d.predict.train)
head(cbind(temp.tr.d.train.hsdpa$download_speed, gbm.hsdpa.d.predict.train), 25)
gbm.hsdpa.d.predict.test <- 10^(predict.gbm(object = gbm.hsdpa.d.split, newdata = temp.tr.d.test.hsdpa, 274))
head(cbind(temp.tr.d.test.hsdpa$download_speed, gbm.hsdpa.d.predict.test), 25)

rmsle.hsdpa.d.train <- sqrt(1 / length(gbm.hsdpa.d.predict.train) * 
                             sum((log1p(gbm.hsdpa.d.predict.train) - 
                                    log1p(temp.tr.d.train.hsdpa$download_speed))^2))
rmsle.hsdpa.d.test <- sqrt(1 / length(gbm.hsdpa.d.predict.test) * 
                            sum((log1p(gbm.hsdpa.d.predict.test) - 
                                   log1p(temp.tr.d.test.hsdpa$download_speed))^2))

save(gbm.hsdpa.d.split, file = "gbm.hsdpa.d.split")


# gbm hsdpa ul

summary(temp.tr.u.train.hsdpa)
str(temp.tr.u.train.hsdpa)

library(gbm)
Sys.time()
gbm.hsdpa.u.split <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
                        + new_network_country + network_id #+ network_id_sim
                        + phone_model + loc_source_gps_one_net_zero + location_precision
                        + rssi
                        + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                        + app_version_code + test_type + unreliable
                        + network_type, 
                        data = temp.tr.u.train.hsdpa, 
                        distribution = "gaussian",
                        n.trees = 1000,
                        interaction.depth = 20,
                        n.minobsinnode = 10,
                        shrinkage = 0.01,
                        keep.data = TRUE,
                        verbose = TRUE)
Sys.time()
gbm.hsdpa.u.split
summary(gbm.hsdpa.u.split)
gbm.perf(gbm.hsdpa.u.split)
#pretty.gbm.tree(gbm.hsdpa.u, i.tree = 500)

gbm.hsdpa.u.predict.train <- 10^(predict.gbm(object = gbm.hsdpa.u.split, newdata = temp.tr.u.train.hsdpa, 268))
summary(temp.tr.u.train.hsdpa$upload_speed)
summary(gbm.hsdpa.u.predict.train)
head(cbind(temp.tr.u.train.hsdpa$upload_speed, gbm.hsdpa.u.predict.train), 25)

gbm.hsdpa.u.predict.test <- 10^(predict.gbm(object = gbm.hsdpa.u.split, newdata = temp.tr.u.test.hsdpa, 268))
summary(temp.tr.u.test.hsdpa$upload_speed)
summary(gbm.hsdpa.u.predict.test)
head(cbind(temp.tr.u.test.hsdpa$upload_speed, gbm.hsdpa.u.predict.test), 25)

rmsle.hsdpa.u.train <- sqrt(1 / length(gbm.hsdpa.u.predict.train) * 
                             sum((log1p(gbm.hsdpa.u.predict.train) - 
                                    log1p(temp.tr.u.train.hsdpa$upload_speed))^2))
rmsle.hsdpa.u.test <- sqrt(1 / length(gbm.hsdpa.u.predict.test) * 
                            sum((log1p(gbm.hsdpa.u.predict.test) - 
                                   log1p(temp.tr.u.test.hsdpa$upload_speed))^2))

save(gbm.hsdpa.u.split, file = "gbm.hsdpa.u.split")

#
#


# gbm hspa dl

summary(temp.tr.d.train.hspa)
str(temp.tr.d.train.hspa)

library(gbm)
Sys.time()
gbm.hspa.d.split <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
                         + new_network_country + network_id #+ network_id_sim
                         + phone_model + loc_source_gps_one_net_zero + location_precision
                         + rssi
                         + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                         + app_version_code + test_type + unreliable, 
                         data = temp.tr.d.train.hspa, 
                         distribution = "gaussian",
                         n.trees = 1000,
                         interaction.depth = 20,
                         n.minobsinnode = 10,
                         shrinkage = 0.01,
                         keep.data = TRUE,
                         verbose = TRUE)
Sys.time()
gbm.hspa.d.split
summary(gbm.hspa.d.split)
gbm.perf(gbm.hspa.d.split)

gbm.hspa.d.predict.train <- 10^(predict.gbm(object = gbm.hspa.d.split, newdata = temp.tr.d.train.hspa, 270))
summary(temp.tr.d.train.hspa$download_speed)
summary(gbm.hspa.d.predict.train)
head(cbind(temp.tr.d.train.hspa$download_speed, gbm.hspa.d.predict.train), 25)
gbm.hspa.d.predict.test <- 10^(predict.gbm(object = gbm.hspa.d.split, newdata = temp.tr.d.test.hspa, 270))
head(cbind(temp.tr.d.test.hspa$download_speed, gbm.hspa.d.predict.test), 25)

rmsle.hspa.d.train <- sqrt(1 / length(gbm.hspa.d.predict.train) * 
                              sum((log1p(gbm.hspa.d.predict.train) - 
                                     log1p(temp.tr.d.train.hspa$download_speed))^2))
rmsle.hspa.d.test <- sqrt(1 / length(gbm.hspa.d.predict.test) * 
                             sum((log1p(gbm.hspa.d.predict.test) - 
                                    log1p(temp.tr.d.test.hspa$download_speed))^2))

save(gbm.hspa.d.split, file = "gbm.hspa.d.split")


# gbm hspa ul

summary(temp.tr.u.train.hspa)
str(temp.tr.u.train.hspa)

library(gbm)
Sys.time()
gbm.hspa.u.split <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
                         + new_network_country + network_id #+ network_id_sim
                         + phone_model + loc_source_gps_one_net_zero + location_precision
                         + rssi
                         + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                         + app_version_code + test_type + unreliable
                         + network_type, 
                         data = temp.tr.u.train.hspa, 
                         distribution = "gaussian",
                         n.trees = 1000,
                         interaction.depth = 20,
                         n.minobsinnode = 10,
                         shrinkage = 0.01,
                         keep.data = TRUE,
                         verbose = TRUE)
Sys.time()
gbm.hspa.u.split
summary(gbm.hspa.u.split)
gbm.perf(gbm.hspa.u.split)
#pretty.gbm.tree(gbm.hspa.u, i.tree = 500)

gbm.hspa.u.predict.train <- 10^(predict.gbm(object = gbm.hspa.u.split, newdata = temp.tr.u.train.hspa, 232))
summary(temp.tr.u.train.hspa$upload_speed)
summary(gbm.hspa.u.predict.train)
head(cbind(temp.tr.u.train.hspa$upload_speed, gbm.hspa.u.predict.train), 25)

gbm.hspa.u.predict.test <- 10^(predict.gbm(object = gbm.hspa.u.split, newdata = temp.tr.u.test.hspa, 232))
summary(temp.tr.u.test.hspa$upload_speed)
summary(gbm.hspa.u.predict.test)
head(cbind(temp.tr.u.test.hspa$upload_speed, gbm.hspa.u.predict.test), 25)

rmsle.hspa.u.train <- sqrt(1 / length(gbm.hspa.u.predict.train) * 
                              sum((log1p(gbm.hspa.u.predict.train) - 
                                     log1p(temp.tr.u.train.hspa$upload_speed))^2))
rmsle.hspa.u.test <- sqrt(1 / length(gbm.hspa.u.predict.test) * 
                             sum((log1p(gbm.hspa.u.predict.test) - 
                                    log1p(temp.tr.u.test.hspa$upload_speed))^2))

save(gbm.hspa.u.split, file = "gbm.hspa.u.split")

#
#


# gbm hspap dl

summary(temp.tr.d.train.hspap)
str(temp.tr.d.train.hspap)

library(gbm)
Sys.time()
gbm.hspap.d.split <- gbm(log10(download_speed) ~ new_month + new_year + new_weekday + new_hour
                        + new_network_country + network_id #+ network_id_sim
                        + phone_model + loc_source_gps_one_net_zero + location_precision
                        + rssi
                        + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                        + app_version_code + test_type + unreliable, 
                        data = temp.tr.d.train.hspap, 
                        distribution = "gaussian",
                        n.trees = 1000,
                        interaction.depth = 20,
                        n.minobsinnode = 10,
                        shrinkage = 0.01,
                        keep.data = TRUE,
                        verbose = TRUE)
Sys.time()
gbm.hspap.d.split
summary(gbm.hspap.d.split)
gbm.perf(gbm.hspap.d.split)

gbm.hspap.d.predict.train <- 10^(predict.gbm(object = gbm.hspap.d.split, newdata = temp.tr.d.train.hspap, 526))
summary(temp.tr.d.train.hspap$download_speed)
summary(gbm.hspap.d.predict.train)
head(cbind(temp.tr.d.train.hspap$download_speed, gbm.hspap.d.predict.train), 25)
gbm.hspap.d.predict.test <- 10^(predict.gbm(object = gbm.hspap.d.split, newdata = temp.tr.d.test.hspap, 526))
head(cbind(temp.tr.d.test.hspap$download_speed, gbm.hspap.d.predict.test), 25)

rmsle.hspap.d.train <- sqrt(1 / length(gbm.hspap.d.predict.train) * 
                             sum((log1p(gbm.hspap.d.predict.train) - 
                                    log1p(temp.tr.d.train.hspap$download_speed))^2))
rmsle.hspap.d.test <- sqrt(1 / length(gbm.hspap.d.predict.test) * 
                            sum((log1p(gbm.hspap.d.predict.test) - 
                                   log1p(temp.tr.d.test.hspap$download_speed))^2))

save(gbm.hspap.d.split, file = "gbm.hspap.d.split")


# gbm hspap ul

summary(temp.tr.u.train.hspap)
str(temp.tr.u.train.hspap)

library(gbm)
Sys.time()
gbm.hspap.u.split <- gbm(log10(upload_speed) ~ new_month + new_year + new_weekday + new_hour
                        + new_network_country + network_id #+ network_id_sim
                        + phone_model + loc_source_gps_one_net_zero + location_precision
                        + rssi
                        + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range 
                        + app_version_code + test_type + unreliable
                        + network_type, 
                        data = temp.tr.u.train.hspap, 
                        distribution = "gaussian",
                        n.trees = 1000,
                        interaction.depth = 20,
                        n.minobsinnode = 10,
                        shrinkage = 0.01,
                        keep.data = TRUE,
                        verbose = TRUE)
Sys.time()
gbm.hspap.u.split
summary(gbm.hspap.u.split)
gbm.perf(gbm.hspap.u.split)
#pretty.gbm.tree(gbm.hspap.u, i.tree = 500)

gbm.hspap.u.predict.train <- 10^(predict.gbm(object = gbm.hspap.u.split, newdata = temp.tr.u.train.hspap, 1000))
summary(temp.tr.u.train.hspap$upload_speed)
summary(gbm.hspap.u.predict.train)
head(cbind(temp.tr.u.train.hspap$upload_speed, gbm.hspap.u.predict.train), 25)

gbm.hspap.u.predict.test <- 10^(predict.gbm(object = gbm.hspap.u.split, newdata = temp.tr.u.test.hspap, 1000))
summary(temp.tr.u.test.hspap$upload_speed)
summary(gbm.hspap.u.predict.test)
head(cbind(temp.tr.u.test.hspap$upload_speed, gbm.hspap.u.predict.test), 25)

rmsle.hspap.u.train <- sqrt(1 / length(gbm.hspap.u.predict.train) * 
                             sum((log1p(gbm.hspap.u.predict.train) - 
                                    log1p(temp.tr.u.train.hspap$upload_speed))^2))
rmsle.hspap.u.test <- sqrt(1 / length(gbm.hspap.u.predict.test) * 
                            sum((log1p(gbm.hspap.u.predict.test) - 
                                   log1p(temp.tr.u.test.hspap$upload_speed))^2))

save(gbm.hspap.u.split, file = "gbm.hspap.u.split")

#
#


##################################################
#PREDICTING

load("evalset.for.gbm.2")

#evalset nw_type

summary(evalset$nw_type)
evalset$nw_type <- NA
evalset[which(evalset$network_type == "LTE"),]$nw_type <- "LTE"

evalset[which(evalset$network_type == "1xRTT" | evalset$network_type == "eHRPD" | evalset$network_type == "EVDO 0" | 
             evalset$network_type == "EVDO A" | evalset$network_type == "EVDO B" | evalset$network_type == "CDMA"),]$nw_type <- "CDMA"

evalset[which(evalset$network_type == "EDGE" | evalset$network_type == "GPRS"),]$nw_type <- "GSM"

evalset[which(evalset$network_type == "HSDPA" | evalset$network_type == "HSUPA"),]$nw_type <- "HSDPA"
evalset[which(evalset$network_type == "HSPA"),]$nw_type <- "HSPA"
evalset[which(evalset$network_type == "HSPAP"),]$nw_type <- "HSPAP"
evalset[which(evalset$network_type == "UMTS"),]$nw_type <- "UMTS"

evalset[which(evalset$is_wifi),]$nw_type <- "WIFI"
evalset[which(evalset$network_type == "Out of service"),]$nw_type <- "OOS"

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
evalset.predict.gbm$lte.d <- 10^(predict.gbm(object = gbm.lte.d, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$lte.u <- 10^(predict.gbm(object = gbm.lte.u, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$gsm.d <- 10^(predict.gbm(object = gbm.gsm.d, newdata = evalset.predict.gbm, 148))
evalset.predict.gbm$gsm.u <- 10^(predict.gbm(object = gbm.gsm.u, newdata = evalset.predict.gbm, 184))

evalset.predict.gbm$umts.d.split <- 10^(predict.gbm(object = gbm.umts.d.split, newdata = evalset.predict.gbm, 80))
evalset.predict.gbm$umts.u.split <- 10^(predict.gbm(object = gbm.umts.u.split, newdata = evalset.predict.gbm, 100))
evalset.predict.gbm$hsdpa.d.split <- 10^(predict.gbm(object = gbm.hsdpa.d.split, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$hsdpa.u.split <- 10^(predict.gbm(object = gbm.hsdpa.u.split, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$hspa.d.split <- 10^(predict.gbm(object = gbm.hspa.d.split, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$hspa.u.split <- 10^(predict.gbm(object = gbm.hspa.u.split, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$hspap.d.split <- 10^(predict.gbm(object = gbm.hspap.d.split, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$hspap.u.split <- 10^(predict.gbm(object = gbm.hspap.u.split, newdata = evalset.predict.gbm, 1000))


evalset.predict.gbm$cdma.d <- 10^(predict.gbm(object = gbm.cdma.d, newdata = evalset.predict.gbm, 2000))
evalset.predict.gbm$cdma.u <- 10^(predict.gbm(object = gbm.cdma.u, newdata = evalset.predict.gbm, 2000))
evalset.predict.gbm$oos.d <- 10^(predict.gbm(object = gbm.oos.d, newdata = evalset.predict.gbm, 221))
evalset.predict.gbm$oos.u <- 10^(predict.gbm(object = gbm.oos.u, newdata = evalset.predict.gbm, 249))
evalset.predict.gbm$wifi.d <- 10^(predict.gbm(object = gbm.wifi.d, newdata = evalset.predict.gbm, 444))
evalset.predict.gbm$wifi.u <- 10^(predict.gbm(object = gbm.wifi.u, newdata = evalset.predict.gbm, 332))

summary(evalset.predict.gbm)
str(evalset.predict.gbm)
#save(evalset.predict.gbm, file = "evalset.predict.gbm.3")

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

#splitted
evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "UMTS"),]$download_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "UMTS"),]$umts.d.split
evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "UMTS"),]$upload_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "UMTS"),]$umts.u.split

evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "HSDPA"),]$download_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "HSDPA"),]$hsdpa.d.split
evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "HSDPA"),]$upload_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "HSDPA"),]$hsdpa.u.split

evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "HSPA"),]$download_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "HSPA"),]$hspa.d.split
evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "HSPA"),]$upload_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "HSPA"),]$hspa.u.split

evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "HSPAP"),]$download_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$download_speed == "PREDICT" & evalset.predict.gbm$nw_type == "HSPAP"),]$hspap.d.split
evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "HSPAP"),]$upload_speed <- 
  evalset.predict.gbm[which(evalset.predict.gbm$upload_speed == "PREDICT" & evalset.predict.gbm$nw_type == "HSPAP"),]$hspap.u.split
#/splitted

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

write.table(select(evalset.predict.gbm, upload_speed, download_speed), file = "YetAnotherTeam_final_test_8.csv", sep = ",", row.names = F)


