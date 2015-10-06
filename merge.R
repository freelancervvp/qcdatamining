setwd("C:\\freelancervvp\\Projects\\2015_Data_Mining\\new_data")
sim <- read.csv("YetAnotherTeam_final_test_9.csv")
ver4 <- read.csv("YetAnotherTeam_final_test_4.csv")
ver5 <- read.csv("YetAnotherTeam_final_test_10.csv")

load("evalset.for.gbm.2")
summary(evalset)


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

load("new.best.lte.d")
load("new.best.lte.u")
load("new.best.umts.d")

evalset.predict.gbm$lte.d <- new.best.lte.d
evalset.predict.gbm$lte.u <- new.best.lte.u
evalset.predict.gbm$gsm.d <- as.numeric(levels(sim$download_speed))[as.integer(sim$download_speed)]
evalset.predict.gbm$gsm.u <- as.numeric(levels(sim$upload_speed))[as.integer(sim$upload_speed)]
evalset.predict.gbm$umts.d <- new.best.umts.d
evalset.predict.gbm$umts.u <- as.numeric(levels(sim$upload_speed))[as.integer(sim$upload_speed)]
evalset.predict.gbm$cdma.d <- as.numeric(levels(ver5$download_speed))[as.integer(ver5$download_speed)]
evalset.predict.gbm$cdma.u <- as.numeric(levels(ver5$upload_speed))[as.integer(ver5$upload_speed)]
evalset.predict.gbm$oos.d <- as.numeric(levels(ver4$download_speed))[as.integer(ver4$download_speed)]
evalset.predict.gbm$oos.u <- as.numeric(levels(ver4$upload_speed))[as.integer(ver4$upload_speed)]
evalset.predict.gbm$wifi.d <- as.numeric(levels(sim$download_speed))[as.integer(sim$download_speed)]
evalset.predict.gbm$wifi.u <- as.numeric(levels(sim$upload_speed))[as.integer(sim$upload_speed)]

evalset.predict.gbm$download_speed <- as.character(levels(evalset.predict.gbm$download_speed))[evalset.predict.gbm$download_speed]
table(evalset.predict.gbm$download_speed)
evalset.predict.gbm$upload_speed <- as.character(levels(evalset.predict.gbm$upload_speed))[evalset.predict.gbm$upload_speed]
table(evalset.predict.gbm$upload_speed)

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

write.table(select(evalset.predict.gbm, upload_speed, download_speed), file = "YetAnotherTeam_final_test_13.csv", sep = ",", row.names = F)


