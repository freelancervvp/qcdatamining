library(ggplot2)

#_____________________________________________________________________________________________________________________________
#data loading
setwd("C:\\freelancervvp\\Projects\\2015_Data_Mining\\new_data")
t <- read.csv("training_set_all_fields.csv")
str(t)
summary(t)



# DL speed and UL speed
temp$download_speed <- 
        as.integer(levels(temp$download_speed))[as.integer(temp$download_speed)]
summary(temp$download_speed)
temp$upload_speed <- 
        as.integer(levels(temp$upload_speed))[as.integer(temp$upload_speed)]
summary(temp$upload_speed)

ggplot(temp, aes(x=upload_speed, y=download_speed)) + geom_point()
ggplot(temp[which(temp$upload_speed < 2000),], aes(x=upload_speed, y=download_speed)) + geom_point()
summary(temp[which(temp$upload_speed > 200 & temp$upload_speed < 300 & temp$download_speed > 15000),]$network_type)
summary(temp[which(temp$upload_speed < 100 & temp$download_speed > 15000),]$network_type)


#_____________________________________________________________________________________________________________________________
#reading_time_s_utc formatting
temp <- t
hist(temp$reading_time_s_utc)
temp$reading_time_s_utc <- as.POSIXct(temp$reading_time_s_utc, origin="1970-01-01")

temp$new_weekday <- strftime(temp$reading_time_s_utc, "%u") #add weekday variable
table(temp$new_weekday)
temp$new_weekday <- factor(temp$new_weekday, levels = c("1","2","3","4","5","6","7"))



ggplot(temp, aes(x=new_weekday, y=download_speed)) + geom_boxplot() + ggtitle("Download speed vs. weekday")

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

temp$new_hour <- NA
for (k in 1:100) {
        temp[k,]$new_hour <- format(temp[k,]$reading_time_s_utc, format = "%H", tz = as.character(temp[k,]$tz))
        temp[k,]$new_hour
}

load("tz")
temp$tz <- tz
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

save(temp, file = "temp.new.2")













