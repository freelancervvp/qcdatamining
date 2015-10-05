
#_____________________________________________________________________________________________________________________________
#data loading
setwd("C:\\freelancervvp\\Projects\\2015_Data_Mining\\new_data")
evalset <- read.csv("eval_set_all_fields.csv")

t <- evalset

str(t)
summary(t)


#_____________________________________________________________________________________________________________________________
#reading_time_s_utc formatting
temp <- t
hist(temp$reading_time_s_utc)
temp$reading_time_s_utc <- as.POSIXct(temp$reading_time_s_utc, origin="1970-01-01")

temp$new_weekday <- strftime(temp$reading_time_s_utc, "%u") #add weekday variable
table(temp$new_weekday)
temp$new_weekday <- factor(temp$new_weekday, levels = c("1","2","3","4","5","6","7"))

temp$new_year <- strftime(temp$reading_time_s_utc, "%Y") #add year variable
table(temp$new_year)
temp$new_year <- factor(temp$new_year)
ggplot(temp, aes(x=new_year, y=download_speed)) + geom_boxplot() + ggtitle("Download speed vs. year")

temp$new_month <- strftime(temp$reading_time_s_utc, "%m") #add month variable
table(temp$new_month)
temp$new_month <- factor(temp$new_month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"))
ggplot(temp, aes(x=new_month, y=download_speed)) + geom_boxplot() + ggtitle("Download speed vs. month")

temp$new_hour <- strftime(temp$reading_time_s_utc, "%H") #add hour variable
table(temp$new_hour)
temp$new_hour <- factor(temp$new_hour)
ggplot(temp, aes(x=new_hour, y=download_speed)) + geom_boxplot() + ggtitle("Download speed vs. hour")

temp$tz <- NA
source("timezone.R")
temp[1:100000,]$tz <- find_tz(temp[1:100000,]$lng, temp[1:100000,]$lat)
temp[100001:200000,]$tz <- find_tz(temp[100001:200000,]$lng, temp[100001:200000,]$lat)
temp[200001:300000,]$tz <- find_tz(temp[200001:300000,]$lng, temp[200001:300000,]$lat)
temp[300001:400000,]$tz <- find_tz(temp[300001:400000,]$lng, temp[300001:400000,]$lat)
temp[400001:500000,]$tz <- find_tz(temp[400001:500000,]$lng, temp[400001:500000,]$lat)
temp[500001:600000,]$tz <- find_tz(temp[500001:600000,]$lng, temp[500001:600000,]$lat)
temp[600001:700000,]$tz <- find_tz(temp[600001:700000,]$lng, temp[600001:700000,]$lat)
temp[700001:800000,]$tz <- find_tz(temp[700001:800000,]$lng, temp[700001:800000,]$lat)
temp[800001:900000,]$tz <- find_tz(temp[800001:900000,]$lng, temp[800001:900000,]$lat)
temp[900001:1000000,]$tz <- find_tz(temp[900001:1000000,]$lng, temp[900001:1000000,]$lat)
temp[1000001:1100000,]$tz <- find_tz(temp[1000001:1100000,]$lng, temp[1000001:1100000,]$lat)
temp[1100001:1200000,]$tz <- find_tz(temp[1100001:1200000,]$lng, temp[1100001:1200000,]$lat)
temp[1200001:1300000,]$tz <- find_tz(temp[1200001:1300000,]$lng, temp[1200001:1300000,]$lat)
temp[1300001:1416018,]$tz <- find_tz(temp[1300001:1416018,]$lng, temp[1300001:1416018,]$lat)
head(temp$tz)
temp$tz <- as.factor(temp$tz)
summary(temp$tz)
str(temp$tz)
levels(temp$tz) <- c(levels(temp$tz), "GMT")
temp[which(is.na(temp$tz)),]$tz <- "GMT"
summary(temp$tz)
str(temp$tz)

t <- temp


#_____________________________________________________________________________________________________________________________
#phone_model formatting
temp <- t
head(sort(table(temp$phone_model), decreasing = T), 1000)
head(sort(table(temp$phone_model), decreasing = F), 50)
temp$phone_model[!(temp$phone_model %in% names(head(sort(table(temp$phone_model), decreasing = T), 1024)))] <- NA
temp$phone_model <- factor(temp$phone_model)
str(temp$phone_model)
t <- temp


#_____________________________________________________________________________________________________________________________
#network_id and network_id_sim formatting
temp <- t
summary(temp$network_id)
str(temp$network_id)
temp$network_id[!(temp$network_id %in% names(head(sort(table(temp$network_id), decreasing = T), 1024)))] <- NA
temp$network_id <- factor(temp$network_id)
summary(temp$network_id)
str(temp$network_id)

#Correcting id_sim's
summary(temp$network_id_sim)
temp$network_id_sim[!(temp$network_id_sim %in% names(head(sort(table(temp$network_id_sim), decreasing = T), 1024)))] <- NA
temp$network_id_sim <- factor(temp$network_id_sim)
summary(temp$network_id_sim)
str(temp$network_id_sim)

t <- temp


#_____________________________________________________________________________________________________________________________
#Feature generation from network_name, network_name_sim, network_id and network_id_sim
temp <- t

#network_country form MCC
temp$new_network_country <- substring(temp$network_id,1,3)
temp$new_network_country <- factor(temp$new_network_country)
summary(temp$new_network_country)
str(temp$new_network_country)
levels(temp$new_network_country)
sort(table(temp$new_network_country), decreasing = T)

t <- temp


#_____________________________________________________________________________________________________________________________
#network_type and network_type_int formatting
temp <- t
table(temp$network_type, temp$network_type_int)
sort(summary(temp$network_type))
temp$network_type[temp$network_type == "- " | temp$network_type == "129" | temp$network_type == "16" | 
                          temp$network_type == "17" | temp$network_type == "18" | temp$network_type == "20" | 
                          temp$network_type == "30" | temp$network_type == "NULL" | temp$network_type == "unknown"] <- "NULL"
temp$network_type <- factor(temp$network_type)
sort(summary(temp$network_type))
t <- temp


#_____________________________________________________________________________________________________________________________
#rssi formatting
temp <- t
summary(temp$rssi)
str(temp$rssi)
temp$rssi <- as.integer(levels(temp$rssi))[as.integer(temp$rssi)]
temp$rssi[temp$rssi > -30] <- NA
table(temp$rssi)
summary(temp$rssi)
t <- temp


#_____________________________________________________________________________________________________________________________
#ec_io formatting
temp <- t
summary(temp$ec_io)
sort(table(temp$ec_io))
temp$ec_io[temp$ec_io >= -1] <- NA
table(temp$ec_io)
temp$ec_io[temp$ec_io < -315] <- NA
table(temp$ec_io)
temp$ec_io <- temp$ec_io/10
table(temp$ec_io)
t <- temp


#_____________________________________________________________________________________________________________________________
#rsrp formatting 
temp <- t
str(temp$rsrp)
table(temp$rsrp)
temp$rsrp <- as.integer(levels(temp$rsrp))[as.integer(temp$rsrp)]
temp$rsrp[temp$rsrp == -140] <- NA
t <- temp


#_____________________________________________________________________________________________________________________________
#rsrq formatting 
temp <- t
str(temp$rsrq)
temp$rsrq <- as.integer(levels(temp$rsrq))[as.integer(temp$rsrq)]
temp[which(temp$network_type != "LTE"),]$rsrq <- NA
t <- temp


#_____________________________________________________________________________________________________________________________
#rssnr formatting 
temp <- t
temp$rssnr <- as.numeric(levels(temp$rssnr))[as.integer(temp$rssnr)]
temp$rssnr[temp$rssnr < -15] <- NA
temp$rssnr[temp$rssnr == 30] <- NA
temp[which(temp$network_type != "LTE"),]$rssnr <- NA
t <- temp

#_____________________________________________________________________________________________________________________________
#screen_state formatting 
temp <- t
str(temp$screen_state)
table(temp$screen_state)
temp[which(temp$screen_state == -1),]$screen_state <- NA
temp$screen_state <- factor(temp$screen_state)
str(temp$screen_state)
t <- temp


#_____________________________________________________________________________________________________________________________
#pci formatting
temp <- t
summary(temp$pci)
hist(temp$pci)
temp$pci[temp$pci>503]
table(temp$pci>503, temp$network_type)
temp$pci[temp$pci>503] <- NA
temp$pci[temp$pci <= 0] <- NA
hist(temp$pci)
t <- temp



#_____________________________________________________________________________________________________________________________
#location_precision formatting
temp <- t
summary(temp$location_precision)
temp$location_precision[temp$location_precision < 0] <- NA
temp$location_precision[temp$location_precision > 5000] <- NA
t <- temp


#_____________________________________________________________________________________________________________________________
#loc_source_gps_one_net_zero formatting
temp <- t
str(temp$loc_source_gps_one_net_zero)
temp$loc_source_gps_one_net_zero[temp$loc_source_gps_one_net_zero == -1] <- NA
temp$loc_source_gps_one_net_zero <- factor(temp$loc_source_gps_one_net_zero)
table(temp$loc_source_gps_one_net_zero)
t <- temp


#_____________________________________________________________________________________________________________________________
#test_type formatting
temp <- t
str(temp$test_type)
table(temp$test_type)
temp$test_type <- factor(temp$test_type)
table(temp$test_type)
str(temp$test_type)
ggplot(data = temp, aes(test_type, download_speed)) + geom_boxplot()
t <- temp


#_____________________________________________________________________________________________________________________________
#icmp_ping_time formatting
temp <- t
str(temp$icmp_ping_time)
levels(temp$icmp_ping_time)
temp$icmp_ping_time <- as.integer(levels(temp$icmp_ping_time))[as.integer(temp$icmp_ping_time)]
temp$icmp_ping_time[temp$icmp_ping_time == -1] <- NA
summary(temp$icmp_ping_time)
t <- temp


#_____________________________________________________________________________________________________________________________
#icmp_ping_packet_loss formatting
temp <- t
summary(temp$icmp_ping_packet_loss)
str(temp$icmp_ping_packet_loss)
table(temp$icmp_ping_packet_loss)
temp$icmp_ping_packet_loss[temp$icmp_ping_packet_loss == -1] <- NA
summary(temp$icmp_ping_packet_loss)
t <- temp


#_____________________________________________________________________________________________________________________________
#icmp_ping_range formatting
temp <- t
str(temp$icmp_ping_range)
temp$icmp_ping_range[which(temp$icmp_ping_range > 1000)] <- NA
temp$icmp_ping_range[which(temp$icmp_ping_range == -1)] <- NA
summary(temp$icmp_ping_range)
t <- temp


#_____________________________________________________________________________________________________________________________
#unreliable formatting
temp <- t
summary(temp$unreliable)
table(temp$unreliable)
temp$unreliable <- factor(temp$unreliable)
t <- temp


#_____________________________________________________________________________________________________________________________
#ip_remote formatting
temp <- t
summary(temp$ip_remote)
head(sort(table(temp$ip_remote), decreasing = T))
table(temp$ip_remote == 0)
t <- temp


#_____________________________________________________________________________________________________________________________
#wifi_rssi formatting
temp <- t
summary(temp$wifi_rssi)
temp$wifi_rssi[temp$wifi_rssi >= 0 | temp$wifi_rssi <= -127] <- NA
t <- temp


#_____________________________________________________________________________________________________________________________
#frequency formatting
temp <- t
summary(temp$frequency)
temp$frequency[temp$frequency == 0 | temp$frequency == 1000000] <- NA
temp$wifi_band <- NA 
temp$wifi_band[temp$frequency >= 2000 & temp$frequency <= 3000] <- 2.4
temp$wifi_band[temp$frequency >= 5000 & temp$frequency <= 6000] <- 5.0
table(temp$wifi_band)
temp$wifi_band <- factor(temp$wifi_band)
table(temp$wifi_band)
str(temp$wifi_band)

#generate is_wifi feature
temp$is_wifi <- NA
temp$is_wifi <- !is.na(temp$wifi_band)
table(temp$is_wifi)

t <- temp


#_____________________________________________________________________________________________________________________________
#api_level formatting
temp <- t
summary(temp$api_level)
temp$api_level <- factor(temp$api_level)
summary(temp$api_level)
table(is.na(temp$api_level))
t <- temp


#release_code formatting
temp <- t
summary(temp$release_code)
str(temp$release_code)
t <- temp


#_____________________________________________________________________________________________________________________________
#app_version_code formatting
temp <- t
sort(table(temp$app_version_code), decreasing = T)
str(temp$app_version_code)
temp$app_version_code[!(temp$app_version_code %in% names(head(sort(table(temp$app_version_code), decreasing = T), 1024)))] <- NA
temp$app_version_code <- factor(temp$app_version_code)
summary(temp$app_version_code)
str(temp$app_version_code)
t <- temp

# nw_type

temp <- t
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
t <- temp


evalset <- t

save(evalset, file = "evalset.for.gbm.5")



##################################################
#PREDICTING

load("evalset.for.gbm.5")

library(caret)
library(dplyr)
set.seed(16)

evalset.predict.gbm <- select(evalset, 
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


########
#PREPARING FOR GBM

str(evalset.predict.gbm)
str(temp.tr)

str(evalset.predict.gbm$tz)
summary(evalset.predict.gbm$tz)
str(temp.tr$tz)
evalset.predict.gbm[which(evalset.predict.gbm$tz == "uninhabited"),]$tz
temp.tr[which(temp.tr$tz == "uninhabited"),]$tz
evalset.predict.gbm$tz <- factor(evalset.predict.gbm$tz, levels = levels(temp.tr$tz))
evalset.predict.gbm[which(is.na(evalset.predict.gbm$tz)),]$tz <- "GMT"
str(evalset.predict.gbm$tz)
str(temp.tr$tz)
summary(evalset.predict.gbm$tz)

str(evalset.predict.gbm$new_network_country)
summary(evalset.predict.gbm$new_network_country)
str(temp.tr$new_network_country)
evalset.predict.gbm$new_network_country <- factor(evalset.predict.gbm$new_network_country, levels = levels(temp.tr$new_network_country))
levels(temp.tr$new_network_country)
evalset.predict.gbm[which(is.na(evalset.predict.gbm$new_network_country)),]$new_network_country <- "NUL"
str(evalset.predict.gbm$new_network_country)
str(temp.tr$new_network_country)

str(evalset.predict.gbm$network_id)
summary(evalset.predict.gbm$network_id)
str(temp.tr$network_id)
evalset.predict.gbm$network_id <- factor(evalset.predict.gbm$network_id, levels = levels(temp.tr$network_id))
levels(temp.tr$network_id)
summary(evalset.predict.gbm$network_id)
evalset.predict.gbm[which(is.na(evalset.predict.gbm$network_id)),]$network_id <- "NULL"
str(evalset.predict.gbm$network_id)
str(temp.tr$network_id)

str(evalset.predict.gbm$network_id_sim)
summary(evalset.predict.gbm$network_id_sim)
str(temp.tr$network_id_sim)
evalset.predict.gbm$network_id_sim <- factor(evalset.predict.gbm$network_id_sim, levels = levels(temp.tr$network_id_sim))
levels(temp.tr$network_id_sim)
summary(evalset.predict.gbm$network_id_sim)
evalset.predict.gbm[which(is.na(evalset.predict.gbm$network_id_sim)),]$network_id_sim <- "NULL"
str(evalset.predict.gbm$network_id_sim)
str(temp.tr$network_id_sim)

str(evalset.predict.gbm$phone_model)
summary(evalset.predict.gbm$phone_model)
str(temp.tr$phone_model)
summary(temp.tr$phone_model)
evalset.predict.gbm$phone_model <- factor(evalset.predict.gbm$phone_model, levels = levels(temp.tr$phone_model))
levels(temp.tr$phone_model)
summary(evalset.predict.gbm$phone_model)
evalset.predict.gbm[which(is.na(evalset.predict.gbm$phone_model)),]$phone_model <- "NULL"
str(evalset.predict.gbm$phone_model)
str(temp.tr$phone_model)

str(evalset.predict.gbm$app_version_code)
summary(evalset.predict.gbm$app_version_code)
str(temp.tr$app_version_code)
summary(temp.tr$app_version_code)
evalset.predict.gbm$app_version_code <- factor(evalset.predict.gbm$app_version_code, levels = levels(temp.tr$app_version_code))
levels(temp.tr$app_version_code)
summary(evalset.predict.gbm$app_version_code)
evalset.predict.gbm[which(is.na(evalset.predict.gbm$app_version_code)),]$app_version_code <- "NULL"
str(evalset.predict.gbm$app_version_code)
str(temp.tr$app_version_code)

str(evalset.predict.gbm$release_code)
summary(evalset.predict.gbm$release_code)
str(temp.tr$release_code)
summary(temp.tr$release_code)
evalset.predict.gbm$release_code <- factor(evalset.predict.gbm$release_code, levels = levels(temp.tr$release_code))
levels(temp.tr$release_code)
summary(evalset.predict.gbm$release_code)
evalset.predict.gbm[which(is.na(evalset.predict.gbm$release_code)),]$release_code <- "NULL"
str(evalset.predict.gbm$release_code)
str(temp.tr$release_code)

str(evalset.predict.gbm)
str(temp.tr)

##PREDICTING

library(gbm)
evalset.predict.gbm$lte.d <- 10^(predict.gbm(object = gbm.lte.d.5, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$lte.u <- 10^(predict.gbm(object = gbm.lte.u.5, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$gsm.d <- 10^(predict.gbm(object = gbm.gsm.d.5, newdata = evalset.predict.gbm, 60))
evalset.predict.gbm$gsm.u <- 10^(predict.gbm(object = gbm.gsm.u.5, newdata = evalset.predict.gbm, 250))
evalset.predict.gbm$umts.d <- 10^(predict.gbm(object = gbm.umts.d.5, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$umts.u <- 10^(predict.gbm(object = gbm.umts.u.5, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$cdma.d <- 10^(predict.gbm(object = gbm.cdma.d.5, newdata = evalset.predict.gbm, 1000))
evalset.predict.gbm$cdma.u <- 10^(predict.gbm(object = gbm.cdma.u.5, newdata = evalset.predict.gbm, 900))
evalset.predict.gbm$oos.d <- 10^(predict.gbm(object = gbm.oos.d.5, newdata = evalset.predict.gbm, 92))
evalset.predict.gbm$oos.u <- 10^(predict.gbm(object = gbm.oos.u.5, newdata = evalset.predict.gbm, 122))
evalset.predict.gbm$wifi.d <- 10^(predict.gbm(object = gbm.wifi.d.5, newdata = evalset.predict.gbm, 300))
evalset.predict.gbm$wifi.u <- 10^(predict.gbm(object = gbm.wifi.u.5, newdata = evalset.predict.gbm, 180))

summary(evalset.predict.gbm)
save(evalset.predict.gbm, file = "evalset.predict.gbm.5")

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

write.table(select(evalset.predict.gbm, upload_speed, download_speed), file = "YetAnotherTeam_final_test_10.csv", sep = ",", row.names = F)

