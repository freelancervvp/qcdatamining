library(ggplot2)

setwd("C:\\freelancervvp\\Projects\\2015_Data_Mining\\new_data")
evalset <- read.csv("eval_set_all_fields.csv")
str(evalset)
summary(evalset)

#For convinience
t <- evalset

#_____________________________________________________________________________________________________________________________
#udid exploration
temp <- t
head(sort(table(temp$udid), decreasing = T), 15)
t <- temp


#_____________________________________________________________________________________________________________________________
#reading_time_s_utc formatting
temp <- t
hist(temp$reading_time_s_utc)
temp$reading_time_s_utc <- as.POSIXct(temp$reading_time_s_utc, origin="1970-01-01")
hist(temp$reading_time_s_utc, breaks = 10000) # xlim = c(as.POSIXct("2014-11-01 00:00:00 MSK"), as.POSIXct("2015-06-01 00:00:00 MSK")))
library(plyr)
ddply(temp, .(strftime(temp$reading_time_s_utc, "%Y")), nrow) #by years
ddply(temp, .(strftime(temp$reading_time_s_utc, "%d")), nrow) #by days of month
ddply(temp, .(strftime(temp$reading_time_s_utc, "%m")), nrow) #by months
ddply(temp, .(strftime(temp$reading_time_s_utc, "%u")), nrow) #by weekdays
temp$new_weekday <- strftime(temp$reading_time_s_utc, "%u") #add weekday variable
temp$new_year <- strftime(temp$reading_time_s_utc, "%Y") #add year variable
temp$new_month <- strftime(temp$reading_time_s_utc, "%m") #add month variable
t <- temp


#_____________________________________________________________________________________________________________________________
#reading_timestamp formatting
temp <- t
temp$reading_timestamp <- as.POSIXct(temp$reading_timestamp)
hist(temp$reading_timestamp, breaks = 10000)
t <- temp


#_____________________________________________________________________________________________________________________________
#phone_type exploration
temp <- t
table(temp$phone_type)
t <- temp


#_____________________________________________________________________________________________________________________________
#phone_model formatting
temp <- t
head(sort(table(temp$phone_model), decreasing = T), 50)
str(temp)
t <- temp


#_____________________________________________________________________________________________________________________________
#network_name and network_name_sim formatting
temp <- t
sort(summary(temp$network_name))
sort(summary(temp$network_name_sim))
#easier to use MCC-MNC
t <- temp


#_____________________________________________________________________________________________________________________________
#network_id and network_id_sim formatting
temp <- t
summary(temp$network_id)
#Correcting id's
levels(temp$network_id)[lapply(levels(temp$network_id), function(x) {stringi::stri_length(x)}) > 6]
levels(temp$network_id)[lapply(levels(temp$network_id), function(x) {stringi::stri_length(x)}) < 5]
sort(table(temp$network_id[lapply(temp$network_id, function(x) {stringi::stri_length(x)}) > 6]), decreasing = T)
#table(temp$network_id_sim[temp$network_id=="1023127"])
temp$network_id[lapply(temp$network_id, function(x) {stringi::stri_length(x)}) > 6] <- NA
temp$network_id <- factor(temp$network_id)
sort(table(temp$network_id[lapply(temp$network_id, function(x) {stringi::stri_length(x)}) < 5]), decreasing = T)
#table(temp$network_id_sim[temp$network_id=="3167"])
#temp$network_id[temp$network_id == "3167"] <- "311480"
#table(temp$network_id_sim[temp$network_id=="3107"])
#temp$network_id[temp$network_id == "3107"] <- "311480"
#table(temp$network_id_sim[temp$network_id=="4047"])
#temp$network_id[temp$network_id == "4047"] <- "40400"
temp$network_id[lapply(temp$network_id, function(x) {stringi::stri_length(x)}) < 5] <- NA
temp$network_id <- factor(temp$network_id)
summary(temp$network_id)
temp$network_id <- as.integer(levels(temp$network_id))[as.integer(temp$network_id)]
summary(temp$network_id)
#Correcting id_sim's
summary(temp$network_id_sim)
#Checking >6 symbols
levels(temp$network_id_sim)[lapply(levels(temp$network_id_sim), function(x) {stringi::stri_length(x)}) > 6]
#temp$network_id_sim[temp$network_id_sim == "-310260-"] <- "310260"
#temp$network_id_sim[temp$network_id_sim == "22210 22210"] <- "22210"
#temp$network_id_sim[temp$network_id_sim == "25001 25002"] <- "25001"
#head(temp$network_id[temp$network_id_sim=="3118790"])
#temp$network_id_sim[temp$network_id_sim == "3118790"] <- "311870"
#head(temp$network_id[temp$network_id_sim=="40436 40552"])
#temp$network_id_sim[temp$network_id_sim == "40436 40552"] <- "40552"
#head(temp$network_id[temp$network_id_sim=="4048442195544"])
#head(temp$network_name[temp$network_id_sim=="4048442195544"])
#temp$network_id_sim[temp$network_id_sim == "4048442195544"] <- "40484"
#head(temp$network_id[temp$network_id_sim=="404864466"])
#temp$network_id_sim[temp$network_id_sim == "404864466"] <- "40486"
#head(temp$network_id[temp$network_id_sim=="41403 41401"])
#head(temp$network_name[temp$network_id_sim=="41403 41401"])
#temp$network_id_sim[temp$network_id_sim == "41403 41401"] <- "41403"
#head(temp$network_id[temp$network_id_sim=="50212 50219"])
#temp$network_id_sim[temp$network_id_sim == "50212 50219"] <- "50212"
#head(temp$network_id[temp$network_id_sim=="61301 61302"])
#temp$network_id_sim[temp$network_id_sim == "61301 61302"] <- "61301"
#head(temp$network_id[temp$network_id_sim=="61301 61303"])
#temp$network_id_sim[temp$network_id_sim == "61301 61303"] <- "61301"
#head(temp$network_id[temp$network_id_sim=="61302 61301"])
#temp$network_id_sim[temp$network_id_sim == "61302 61301"] <- "61302"
#temp$network_id_sim[temp$network_id_sim == "63002 63086"] <- "63002"
#temp$network_id_sim[temp$network_id_sim == "64604 64604"] <- "64604"
#head(temp$network_id[temp$network_id_sim=="lÃ¯Â¿â°Ã¯Â¿Å¾"])
head(temp$network_id[lapply(temp$network_id_sim, function(x) {stringi::stri_length(x)}) > 6])
temp$network_id_sim[lapply(temp$network_id_sim, function(x) {stringi::stri_length(x)}) > 6] <- NA
temp$network_id_sim <- factor(temp$network_id_sim)
levels(temp$network_id_sim)[lapply(levels(temp$network_id_sim), function(x) {stringi::stri_length(x)}) > 6]
#Checking <5 symbols
levels(temp$network_id_sim)[lapply(levels(temp$network_id_sim), function(x) {stringi::stri_length(x)}) < 5]
head(temp$network_id[temp$network_id_sim=="4321"])
temp$network_id_sim[temp$network_id_sim == "4321"] <- "43211"
temp$network_id_sim[temp$network_id_sim == "" | temp$network_id_sim == "- " | 
                      temp$network_id_sim == " " | temp$network_id_sim == "NULL" ] <- NA
temp$network_id_sim <- factor(temp$network_id_sim)
levels(temp$network_id_sim)[lapply(levels(temp$network_id_sim), function(x) {stringi::stri_length(x)}) < 5]
#Convert from factor to integer
temp$network_id_sim <- as.integer(levels(temp$network_id_sim))[as.integer(temp$network_id_sim)]
summary(temp$network_id_sim)
t <- temp


#_____________________________________________________________________________________________________________________________
#Feature generation from network_name, network_name_sim, network_id and network_id_sim
temp <- t
cbind(head(levels(temp$network_name)[temp$network_name], 20), head(levels(temp$network_name_sim)[temp$network_name_sim], 20), 
      head(temp$network_id, 20), head(temp$network_id_sim, 20))
summary(temp$network_id_sim)
summary(temp$network_name_sim)
summary(temp$network_id - temp$network_id_sim)
table(temp$network_id - temp$network_id_sim)
#network_country form MCC
temp$new_network_country <- substring(temp$network_id,1,3)
temp$new_network_country <- as.integer(temp$new_network_country)
summary(temp$new_network_country)
sort(table(temp$new_network_country), decreasing = T)
t <- temp


#_____________________________________________________________________________________________________________________________
#roaming formatting
temp <- t
summary(temp$roaming)
table(temp$roaming)
#everything 0
#temp$roaming <- factor(temp$roaming)
#summary(temp$roaming)
#str(temp$roaming)
t <- temp


#_____________________________________________________________________________________________________________________________
#network_connection_type formatting
temp <- t
summary(temp$network_connection_type)
table(temp$network_connection_type)
#as no WiFi, no sense to use
#temp$network_connection_type <- as.factor(temp$network_connection_type)
t <- temp


#_____________________________________________________________________________________________________________________________
#network_type and network_type_int formatting
temp <- t
table(temp$network_type, temp$network_type_int)
sort(summary(temp$network_type))
#temp$network_type[] <- NA
temp$network_type[temp$network_type == "- " | temp$network_type == "129" | temp$network_type == "16" | 
                    temp$network_type == "17" | temp$network_type == "18" | temp$network_type == "20" | 
                    temp$network_type == "30" | temp$network_type == "NULL" | temp$network_type == "unknown"] <- NA
temp$network_type <- factor(temp$network_type)
sort(summary(temp$network_type))
table(temp$network_type, temp$network_type_int)
#need to format download_speed for boxplots
#temp$download_speed <- 
#  as.integer(levels(temp$download_speed))[as.integer(temp$download_speed)]
#summary(temp$download_speed)
#boxplot(temp$download_speed ~ temp$network_type)
#ggplot(temp, aes(x=network_type, y=download_speed)) + geom_boxplot() + ggtitle("Download speed vs. network type")
str(temp$network_type_int)
#temp$network_type_int <- as.factor(temp$network_type_int)
t <- temp


#_____________________________________________________________________________________________________________________________
#cell_type formatting
temp <- t
summary(temp$cell_type)
temp$cell_type[temp$cell_type == "unknown"] <- NA
temp$cell_type <- factor(temp$cell_type)
summary(temp$cell_type)
#boxplot(temp$download_speed ~ temp$cell_type)
t <- temp


#_____________________________________________________________________________________________________________________________
#rssi formatting
temp <- t
summary(temp$rssi)
str(temp$rssi)
temp$rssi <- as.integer(levels(temp$rssi))[as.integer(temp$rssi)]
hist(temp$rssi)
temp$rssi[temp$rssi > -30] <- NA
table(temp$rssi)
summary(temp$rssi)
table(temp$network_type, is.na(temp$rssi))
ggplot(temp, aes(x=rssi)) + geom_histogram(binwidth=1, colour="black", fill="white")
#ggplot(data = temp, aes(rssi, download_speed)) + geom_point()
t <- temp


#_____________________________________________________________________________________________________________________________
#bit_error_rate formatting
temp <- t
table(temp$bit_error_rate)
temp$bit_error_rate <- as.integer(levels(temp$bit_error_rate))[as.integer(temp$bit_error_rate)]
summary(temp$bit_error_rate)
table(temp$bit_error_rate)
#temp$bit_error_rate[temp$bit_error_rate < -2] <- NA
#table(temp$bit_error_rate)
#temp$bit_error_rate[temp$bit_error_rate == -1] <- NA
#table(temp$bit_error_rate)
summary(temp$bit_error_rate)
#ggplot(temp, aes(x=as.factor(bit_error_rate), y=download_speed)) + geom_boxplot() 
#+ ggtitle("Download speed vs. bit error rate")
table(temp$network_type, is.na(temp$bit_error_rate))
#ggplot(temp[which(temp$network_type == "LTE"),], aes(x=as.factor(bit_error_rate), y=download_speed)) 
#+ geom_boxplot() + ggtitle("Download speed vs. bit error rate")
str(temp)
t <- temp


#_____________________________________________________________________________________________________________________________
#ec_io formatting
temp <- t
head(subset(temp, ec_io != "-1" & ec_io != "NULL"), 20)
table(subset(temp, ec_io != "-1" & ec_io != "NULL")$phone_type)
summary(temp$ec_io)
sort(table(temp$ec_io))
ggplot(subset(temp,temp$ec_io > -200 & temp$ec_io < -1), aes(x=ec_io)) + geom_histogram(binwidth=.5, colour="black", fill="white")
temp$ec_io[temp$ec_io >= -1] <- NA
table(temp$ec_io)
temp$ec_io[temp$ec_io < -315] <- NA
table(temp$ec_io)
temp$ec_io <- temp$ec_io/10
table(temp$ec_io)
ggplot(temp, aes(x=ec_io)) + geom_histogram(binwidth=.5, colour="black", fill="white")
t <- temp


#_____________________________________________________________________________________________________________________________
#rsrp formatting 
temp <- t
str(temp$rsrp)
table(temp$rsrp)
temp$rsrp <- as.integer(levels(temp$rsrp))[as.integer(temp$rsrp)]
table(temp[which(temp$network_type != "LTE"),]$rsrp)
table(temp[which(temp$network_type == "LTE"),]$rsrp)
table(temp[which(is.na(temp$rsrp)),]$network_type)
table(temp[which(!is.na(temp$rsrp)),]$network_type)
temp[which(temp$network_type != "LTE"),]$rsrp <- NA
ggplot(temp, aes(x=rsrp)) + geom_histogram(binwidth=.5, colour="black", fill="white")
temp$rsrp[temp$rsrp == -140] <- NA
#plotting download_speed vs rsrp for LTE
#ggplot(data = temp[which(temp$network_type == "LTE"),], aes(rsrp, download_speed)) + geom_point()
#ggplot(data = temp[which(temp$network_type == "LTE"),], aes(as.factor(rsrp), download_speed)) + geom_boxplot()
t <- temp


#_____________________________________________________________________________________________________________________________
#rsrq formatting 
temp <- t
str(temp$rsrq)
levels(temp$rsrq)
table(temp$rsrq)
table(temp[which(temp$network_type != "LTE"),]$rsrq)
table(temp[which(temp$network_type == "LTE"),]$rsrq)
temp$rsrq <- as.integer(levels(temp$rsrq))[as.integer(temp$rsrq)]
table(temp$rsrq)
temp[which(temp$network_type != "LTE"),]$rsrq <- NA
table(temp[which(temp$network_type != "LTE"),]$rsrq)
#ggplot(data = temp[which(temp$network_type == "LTE"),], aes(rsrq, download_speed)) + geom_point()
#ggplot(data = temp[which(temp$network_type == "LTE"),], aes(as.factor(rsrq), download_speed)) + geom_boxplot()
t <- temp


#_____________________________________________________________________________________________________________________________
#rssnr formatting 
temp <- t
head(table(temp$rssnr),20)
str(temp$rssnr)
temp$rssnr <- as.numeric(levels(temp$rssnr))[as.integer(temp$rssnr)]
ggplot(temp, aes(x=rssnr)) + geom_histogram(binwidth=.5, colour="black", fill="white")
summary(temp$rssnr)
temp$rssnr[temp$rssnr < -15] <- NA
temp$rssnr[temp$rssnr == 30] <- NA
table(temp[which(temp$network_type != "LTE"),]$rssnr)
temp[which(temp$network_type != "LTE"),]$rssnr <- NA
ggplot(temp, aes(x=rssnr)) + geom_histogram(binwidth=.5, colour="black", fill="white")
summary(temp$rssnr)
summary(temp[which(temp$network_type == "LTE"),]$rssnr)
t <- temp

#_____________________________________________________________________________________________________________________________
#screen_state formatting 
temp <- t
str(temp$screen_state)
table(temp$screen_state)
temp[which(temp$screen_state == -1),]$screen_state <- NA
table(temp$screen_state)
summary(temp$screen_state)
temp$screen_state <- factor(temp$screen_state)
str(temp$screen_state)
ggplot(data = temp, aes(screen_state, download_speed)) + geom_boxplot()
t <- temp


#_____________________________________________________________________________________________________________________________
#call_state formatting 
temp <- t
str(temp$call_state)
table(temp$call_state)
temp$call_state[temp$call_state > 2 | temp$call_state < 0] <- NA
table(temp$call_state)
t <- temp


#_____________________________________________________________________________________________________________________________
#cid formatting 
temp <- t
str(temp$cid)
temp$cid <- as.integer(levels(temp$cid))[as.integer(temp$cid)]
str(temp$cid)
summary(temp$cid)
ggplot(temp, aes(x=cid)) + geom_histogram(binwidth=.5, colour="black", fill="white")
table(is.na(temp$cid), temp$network_type)
t <- temp


#_____________________________________________________________________________________________________________________________
#lac formatting
temp <- t
str(temp$lac)
temp$lac <- as.integer(levels(temp$lac))[as.integer(temp$lac)]
str(temp$lac)
summary(temp$lac)
hist(temp$lac)
hist(temp$lac[temp$lac<100000], breaks = 100)
summary(temp$lac[temp$lac<100000])
temp$lac[temp$lac > 81920] <- NA
table(is.na(temp$lac), temp$network_type)
t <- temp


#_____________________________________________________________________________________________________________________________
#bsid formatting
temp <- t
str(temp$bsid)
temp$bsid <- as.integer(levels(temp$bsid))[as.integer(temp$bsid)]
str(temp$bsid)
summary(temp$bsid)
ggplot(temp, aes(x=bsid)) + geom_histogram(binwidth=10, colour="black", fill="white")
table(is.na(temp$bsid), temp$network_type)
t <- temp


#_____________________________________________________________________________________________________________________________
#sid formatting
temp <- t
str(temp$sid)
temp$sid <- as.integer(levels(temp$sid))[as.integer(temp$sid)]
str(temp$sid)
summary(temp$sid)
ggplot(temp, aes(x=sid)) + geom_histogram(binwidth=10, colour="black", fill="white")
table(is.na(temp$sid), temp$network_type)
t <- temp


#_____________________________________________________________________________________________________________________________
#nid formatting
temp <- t
str(temp$nid)
temp$nid <- as.integer(levels(temp$nid))[as.integer(temp$nid)]
str(temp$nid)
summary(temp$nid)
ggplot(temp, aes(x=nid)) + geom_histogram(binwidth=10, colour="black", fill="white")
table(is.na(temp$nid), temp$network_type)
t <- temp


#_____________________________________________________________________________________________________________________________
#pci formatting
temp <- t
summary(temp$pci)
hist(temp$pci)
temp$pci[temp$pci>503]
table(temp$pci>503, temp$network_type)
temp$pci[temp$pci>503] <- NA
length(temp$pci[temp$pci == -1])
length(temp$pci[temp$pci == 0])
hist(temp$pci[temp$pci > 0])
ggplot(temp[temp$pci > 0,], aes(x=pci)) + geom_histogram(binwidth=1, colour="black", fill="white")
summary(temp[which(temp$network_type != "LTE"),]$pci)
summary(temp[which(temp$network_type == "LTE"),]$pci)
table(temp$pci == 0, temp$network_type)
temp$pci[temp$pci <= 0] <- NA
hist(temp$pci)
t <- temp


#_____________________________________________________________________________________________________________________________
#lat and lng formatting
temp <- t
summary(temp$lat)
table(is.na(temp$lat))
summary(temp$lng)
table(is.na(temp$lng))
t <- temp


#_____________________________________________________________________________________________________________________________
#Plotting maps
temp <- t
library(maps)
library(plyr)
library(ggplot2)
library(sp)
library(ggmap)
worldmap <- map_data("world")
worldmap.p <- ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_polygon(aes(group = group), fill="#f9f9f9", colour = "grey65") + 
  scale_y_continuous(limits = c(-60, 85)) + 
  coord_equal() +  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.border = element_rect(colour = "black"))
worldmap.p
worldmap.p + scale_fill_continuous("Density", low="pink", high="red", limits = c(0, 0.1)) + 
  geom_point(data=temp, mapping=aes(lng, lat), shape =".")
t <- temp


#_____________________________________________________________________________________________________________________________
#Defining country by lat/lng
temp <- t
library(sp)
library(rworldmap)
countriesSP <- getMap(resolution='low')
points.ll <- with(temp, cbind(lng, lat))
pointsSP = SpatialPoints(points.ll, proj4string=CRS(proj4string(countriesSP)))
indices = over(pointsSP, countriesSP)
head(indices$ADMIN)
table(indices$ADMIN)
sort(table(indices$ADMIN))
sort(table(temp$new_network_country))
table(is.na(indices$ADMIN))
str(indices)
head(indices[is.na(indices$ADMIN),])
worldmap.p + geom_point(data=temp[is.na(indices$ADMIN),], mapping=aes(lng, lat), shape =".")
#seems that non recognized objects are located near the borders...
temp$new_country_by_lat_lon <- indices$ADMIN
t <- temp


#_____________________________________________________________________________________________________________________________
#altitude formatting
temp <- t
summary(temp$altitude)
str(temp$altitude)
head(levels(temp$altitude), 30)
temp$altitude <- as.integer(levels(temp$altitude))[as.integer(temp$altitude)]
hist(temp$altitude)
summary(temp$altitude)
hist(temp$altitude[temp$altitude < 3000 & temp$altitude > -300], breaks = 100)
temp$altitude[temp$altitude > 3000] <- NA
temp$altitude[temp$altitude < -250] <- NA
hist(temp$altitude)
t <- temp


#_____________________________________________________________________________________________________________________________
#location_precision formatting
temp <- t
summary(temp$location_precision)
hist(temp$location_precision)
hist(temp$location_precision[temp$location_precision > 0 & temp$location_precision < 10000])
hist(temp$location_precision[temp$location_precision > 1000 & temp$location_precision < 6000])
length(temp$location_precision[temp$location_precision < -1])
length(temp$location_precision[temp$location_precision < 0])
temp$location_precision[temp$location_precision < 0] <- NA
temp$location_precision[temp$location_precision > 5000] <- NA
hist(temp$location_precision)
t <- temp


#_____________________________________________________________________________________________________________________________
#loc_source_gps_one_net_zero formatting
temp <- t
str(temp$loc_source_gps_one_net_zero)
table(temp$loc_source_gps_one_net_zero)
table(is.na(temp$loc_source_gps_one_net_zero))
temp$loc_source_gps_one_net_zero[temp$loc_source_gps_one_net_zero == -1] <- NA
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
temp$icmp_ping_time <- as.integer(levels(temp$icmp_ping_time))[as.integer(temp$icmp_ping_time)]
ggplot(temp, aes(x=icmp_ping_time)) + geom_histogram(binwidth=1, colour="black", fill="white")
ggplot(temp[temp$icmp_ping_time > 0,], aes(x=icmp_ping_time)) + 
  geom_histogram(binwidth=1, colour="black", fill="white")
summary(temp$icmp_ping_time)
table(temp$icmp_ping_time == -1)
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
table(temp$icmp_ping_packet_loss)
t <- temp


#_____________________________________________________________________________________________________________________________
#icmp_ping_range formatting
temp <- t
str(temp$icmp_ping_range)
ggplot(temp, aes(x=icmp_ping_range)) + geom_histogram(binwidth=10, colour="black", fill="white")
ggplot(temp[temp$icmp_ping_range < 1100,], aes(x=icmp_ping_range)) + geom_histogram(binwidth=10, colour="black", fill="white")
temp$icmp_ping_range[which(temp$icmp_ping_range > 1000)] <- NA
temp$icmp_ping_range[which(temp$icmp_ping_range == -1)] <- NA
ggplot(temp, aes(x=icmp_ping_range)) + geom_histogram(binwidth=10, colour="black", fill="white")
summary(temp$icmp_ping_range)
t <- temp


#_____________________________________________________________________________________________________________________________
#unreliable formatting
temp <- t
summary(temp$unreliable)
hist(temp$unreliable)
table(temp$unreliable)
temp$unreliable <- factor(temp$unreliable)
table(temp$unreliable)
t <- temp


#_____________________________________________________________________________________________________________________________
#ip_remote formatting
temp <- t
summary(temp$ip_remote)
head(sort(table(temp$ip_remote), decreasing = T))
table(temp$ip_remote == 0)
temp$has_wifi_ip_addr <- (temp$ip_remote != 0)
table(temp$has_wifi_ip_addr)
t <- temp


#_____________________________________________________________________________________________________________________________
#ssid formatting
temp <- t
summary(temp$ssid)
t <- temp


#_____________________________________________________________________________________________________________________________
#bssid formatting
temp <- t
summary(temp$bssid)
t <- temp


#_____________________________________________________________________________________________________________________________
#capabilities formatting
temp <- t
summary(temp$capabilities)
t <- temp


#_____________________________________________________________________________________________________________________________
#wifi_rssi formatting
temp <- t
summary(temp$wifi_rssi)
head(sort(table(temp$wifi_rssi), decreasing = T), 10)
temp$wifi_rssi[temp$wifi_rssi >= 0 | temp$wifi_rssi <= -127] <- NA
ggplot(temp, aes(x=wifi_rssi)) + geom_histogram(binwidth=1, colour="black", fill="white")
t <- temp


#_____________________________________________________________________________________________________________________________
#frequency formatting
temp <- t
summary(temp$frequency)
table(temp$frequency)
temp$frequency[temp$frequency == 0 | temp$frequency == 1000000] <- NA
table(temp$frequency)
summary(temp$frequency)
temp$wifi_band <- NA 
temp$wifi_band[temp$frequency >= 2000 & temp$frequency <= 3000] <- 2.4
temp$wifi_band[temp$frequency >= 5000 & temp$frequency <= 6000] <- 5.0
table(temp$wifi_band)
temp$wifi_band <- factor(temp$wifi_band)
table(temp$wifi_band)
str(temp$wifi_band)
t <- temp


#_____________________________________________________________________________________________________________________________
#is_connected formatting
temp <- t
summary(temp$is_connected)
table(temp$is_connected)
table(temp$rsrp, temp$is_connected)
table(temp$network_type, temp$is_connected)
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
t <- temp


#_____________________________________________________________________________________________________________________________
#app_version_code formatting
temp <- t
sort(table(temp$app_version_code), decreasing = T)
str(temp$app_version_code)
ggplot(temp, aes(x=app_version_code, y=upload_speed)) + geom_boxplot() + ggtitle("Upload speed vs. app_version_code")
ggplot(temp[temp$app_version_code == 2.63 | temp$app_version_code == 3.31,], aes(x=app_version_code, y=upload_speed)) + geom_boxplot() + ggtitle("Upload speed vs. app_version_code")
ggplot(temp, aes(x=app_version_code, y=download_speed)) + geom_boxplot() + ggtitle("Download speed vs. app_version_code")
ggplot(temp[temp$app_version_code == 2.63 | temp$app_version_code == 3.31,], aes(x=app_version_code, y=download_speed)) + geom_boxplot() + ggtitle("Download speed vs. app_version_code")
summary(temp$app_version_code)
#NAying not popular versions
sort(table(temp$app_version_code), decreasing = T)
tail(sort(table(temp$app_version_code), decreasing = T), 80)
names(tail(sort(table(temp$app_version_code), decreasing = T), 80))
temp$app_version_code[temp$app_version_code %in% names(tail(sort(table(temp$app_version_code), decreasing = T), 80))] <- NA
temp$app_version_code[temp$app_version_code == "NULL"] <- NA
temp$app_version_code <- factor(temp$app_version_code)
summary(temp$app_version_code)
str(temp$app_version_code)
sort(table(temp$app_version_code), decreasing = T)
ggplot(temp, aes(x=app_version_code, y=download_speed)) + geom_boxplot() + ggtitle("Download speed vs. app_version_code")
t <- temp






#_____________________________________________________________________________________________________________________________
# mean value per technology

mean.lte.d <- mean(temp[which(temp$nw_type == "LTE"),]$download_speed, na.rm = T)
mean.cdma.d <- mean(temp[which(temp$nw_type == "CDMA"),]$download_speed, na.rm = T)
mean.gsm.d <- mean(temp[which(temp$nw_type == "GSM"),]$download_speed, na.rm = T)
mean.umts.d <- mean(temp[which(temp$nw_type == "UMTS"),]$download_speed, na.rm = T)
mean.na.d <- mean(temp[which(is.na(temp$nw_type)),]$download_speed, na.rm = T)
mean.lte.u <- mean(temp[which(temp$nw_type == "LTE"),]$upload_speed, na.rm = T)
mean.cdma.u <- mean(temp[which(temp$nw_type == "CDMA"),]$upload_speed, na.rm = T)
mean.gsm.u <- mean(temp[which(temp$nw_type == "GSM"),]$upload_speed, na.rm = T)
mean.umts.u <- mean(temp[which(temp$nw_type == "UMTS"),]$upload_speed, na.rm = T)
mean.na.u <- mean(temp[which(is.na(temp$nw_type)),]$upload_speed, na.rm = T)



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
t <- temp


#_____________________________________________________________________________________________________________________________
# by NW type

temp <- t
temp$nw_type <- NA
temp[which(temp$network_type == "LTE"),]$nw_type <- "LTE"
temp[which(temp$network_type == "1xRTT" | temp$network_type == "eHRPD" | temp$network_type == "EVDO 0" | 
             temp$network_type == "EVDO A" | temp$network_type == "EVDO B" | temp$network_type == "CDMA"),]$nw_type <- "CDMA"
temp[which(temp$network_type == "EDGE" | temp$network_type == "GPRS"),]$nw_type <- "GSM"
temp[which(temp$network_type == "HSDPA" | temp$network_type == "HSUPA" | temp$network_type == "HSPA" | 
             temp$network_type == "HSPAP" | temp$network_type == "UMTS"),]$nw_type <- "UMTS"
temp$nw_type <- factor(temp$nw_type)
summary(temp$nw_type)
t <- temp


#_____________________________________________________________________________________________________________________________
#####

evalset <- t

####



#RF prediction
library(dplyr)

#_____________________________________________________________________________________________________________________________
#Predicting speeds by technology

evalset.predict.rf <- select(evalset, download_speed, upload_speed, new_month,
                               new_year, new_weekday, new_network_country, network_id, 
                               rssi, ec_io, rsrp, rsrq, rssnr, 
                               network_id_sim, test_type, icmp_ping_time, icmp_ping_packet_loss,
                               icmp_ping_range, app_version_code,
                               nw_type)



#lte.d
str(evalset.predict.rf)
str(select(temp, download_speed, upload_speed, new_month,
           new_year,new_weekday, new_network_country, network_id, 
           rssi, ec_io, rsrp, rsrq, rssnr, 
           network_id_sim, test_type, icmp_ping_time, icmp_ping_packet_loss,
           icmp_ping_range, app_version_code,
           nw_type))
evalset.predict.rf$new_month <- as.factor(evalset.predict.rf$new_month)
evalset.predict.rf$new_year <- as.factor(evalset.predict.rf$new_year)
evalset.predict.rf$new_weekday <- as.factor(evalset.predict.rf$new_weekday)
evalset.predict.rf <- na.roughfix(evalset.predict.rf)
evalset.predict.rf$new_network_country <- as.integer(evalset.predict.rf$new_network_country)
evalset.predict.rf$network_id <- as.integer(evalset.predict.rf$network_id)

levels(evalset.predict.rf$app_version_code)
levels(temp.lte.d$app_version_code)
table(evalset.predict.rf$app_version_code)
table(temp.lte.d$app_version_code)
levels(evalset.predict.rf$app_version_code) <- levels(temp$app_version_code)

evalset.predict.rf$lte.d <- predict(rf.lte.d, evalset.predict.rf)
summary(evalset.predict.rf[which(evalset.predict.rf$nw_type == "LTE" & evalset.predict.rf$download_speed == "PREDICT"),]$lte.d)

#lte.u
levels(evalset.predict.rf$new_month)
levels(temp.lte.u.train$new_month)
table(evalset.predict.rf$new_month)
table(temp.lte.u.train$new_month)
evalset.predict.rf[which(evalset.predict.rf$new_month == 11),]$new_month <- 12 #shame on me
evalset.predict.rf$new_month <- factor(evalset.predict.rf$new_month)
evalset.predict.rf$lte.u <- predict(rf.lte.u, evalset.predict.rf)
summary(evalset.predict.rf[which(evalset.predict.rf$nw_type == "LTE" & evalset.predict.rf$upload_speed == "PREDICT"),]$lte.u)

#umts.d
str(evalset.predict.rf)
str(select(temp, download_speed, upload_speed, new_month,
           new_year,new_weekday, new_network_country, network_id, 
           rssi, ec_io, rsrp, rsrq, rssnr, 
           network_id_sim, test_type, icmp_ping_time, icmp_ping_packet_loss,
           icmp_ping_range, app_version_code))
#levels(evalset.predict.rf$app_version_code) <- levels(temp$app_version_code)
#evalset.predict.rf <- na.roughfix(evalset.predict.rf)
#evalset.predict.rf$umts.d <- NA
str(evalset.predict.rf$new_month)
evalset.predict.rf$new_month <- evalset$new_month
evalset.predict.rf$new_month <- factor(evalset.predict.rf$new_month)

evalset.predict.rf$umts.d <- predict(rf.umts.d, evalset.predict.rf)
summary(evalset.predict.rf[which(evalset.predict.rf$nw_type == "UMTS" & evalset.predict.rf$download_speed == "PREDICT"),]$umts.d)

#umts.u
evalset.predict.rf$umts.u <- predict(rf.umts.u, evalset.predict.rf)
summary(evalset.predict.rf[which(evalset.predict.rf$nw_type == "UMTS" & evalset.predict.rf$upload_speed == "PREDICT"),]$umts.u)

#cdma.d
str(evalset.predict.rf)
str(select(temp.cdma.d, download_speed, upload_speed, new_month,
           new_year,new_weekday, new_network_country, network_id, 
           rssi, ec_io, rsrp, rsrq, rssnr, 
           network_id_sim, test_type, icmp_ping_time, icmp_ping_packet_loss,
           icmp_ping_range, app_version_code))
levels(evalset.predict.rf$new_month)
levels(temp$new_month)
table(evalset.predict.rf$new_month)
table(temp$new_month)
evalset.predict.rf[which(evalset.predict.rf$new_month == 11),]$new_month <- 12 #shame on me
evalset.predict.rf$new_month <- factor(evalset.predict.rf$new_month)
evalset.predict.rf$new_network_country <- as.factor(evalset.predict.rf$new_network_country)
str(evalset.predict.rf)
evalset.predict.rf$new_network_country <- factor(evalset.predict.rf$new_network_country, levels = levels(temp.cdma.d$new_network_country))
str(evalset.predict.rf$new_network_country)
levels(evalset.predict.rf$new_network_country)
levels(temp.cdma.d$new_network_country)
evalset.predict.rf$cdma.d <- predict(rf.cdma.d, evalset.predict.rf)
summary(evalset.predict.rf[which(evalset.predict.rf$nw_type == "CDMA" & evalset.predict.rf$download_speed == "PREDICT"),]$cdma.d)


#cdma.u
str(evalset.predict.rf)
str(select(temp.cdma.u, download_speed, upload_speed, new_month,
           new_year,new_weekday, new_network_country, network_id, 
           rssi, ec_io, rsrp, rsrq, rssnr, 
           network_id_sim, test_type, icmp_ping_time, icmp_ping_packet_loss,
           icmp_ping_range, app_version_code))
evalset.predict.rf$new_network_country <- factor(evalset.predict.rf$new_network_country, levels = levels(temp.cdma.u$new_network_country))
evalset.predict.rf <- na.roughfix(evalset.predict.rf)
evalset.predict.rf$cdma.u <- predict(rf.cdma.u, evalset.predict.rf)
summary(evalset.predict.rf[which(evalset.predict.rf$nw_type == "CDMA" & evalset.predict.rf$upload_speed == "PREDICT"),]$cdma.u)

#gsm.d
str(evalset.predict.rf)
str(select(temp, download_speed, upload_speed, new_month,
           new_year,new_weekday, new_network_country, network_id, 
           rssi, ec_io, rsrp, rsrq, rssnr, 
           network_id_sim, test_type, icmp_ping_time, icmp_ping_packet_loss,
           icmp_ping_range, app_version_code))
evalset.predict.rf$new_network_country <- evalset$new_network_country
evalset.predict.rf <- na.roughfix(evalset.predict.rf)
evalset.predict.rf$gsm.d <- predict(rf.gsm.d, evalset.predict.rf)
summary(evalset.predict.rf[which(evalset.predict.rf$nw_type == "GSM" & evalset.predict.rf$download_speed == "PREDICT"),]$gsm.d)

#gsm.u
str(evalset.predict.rf)
str(select(temp, download_speed, upload_speed, new_month,
           new_year,new_weekday, new_network_country, network_id, 
           rssi, ec_io, rsrp, rsrq, rssnr, 
           network_id_sim, test_type, icmp_ping_time, icmp_ping_packet_loss,
           icmp_ping_range, app_version_code))
evalset.predict.rf$gsm.u <- predict(rf.gsm.u, evalset.predict.rf)
summary(testset.predict.rf.1$gsm.u)
summary(evalset.predict.rf[which(evalset.predict.rf$nw_type == "GSM" & evalset.predict.rf$upload_speed == "PREDICT"),]$gsm.u)


#
summary(evalset.predict.rf)
#testset.predict.rf.1$download_speed <- testset$download_speed
#testset.predict.rf.1$upload_speed <- testset$upload_speed
evalset.backup <- evalset.predict.rf

evalset.predict.rf$download_speed <- as.character(levels(evalset.predict.rf$download_speed))[evalset.predict.rf$download_speed]
table(evalset.predict.rf$download_speed)
evalset.predict.rf$upload_speed <- as.character(levels(evalset.predict.rf$upload_speed))[evalset.predict.rf$upload_speed]
table(evalset.predict.rf$upload_speed)

summary(evalset$nw_type)
#testset.predict.rf.1$nw_type <- testset$nw_type

#Replacing PREDICTs with speeds
evalset.predict.rf[which(evalset.predict.rf$download_speed == "PREDICT" & 
                           evalset.predict.rf$nw_type == "LTE"),]$download_speed <- 
  evalset.predict.rf[which(evalset.predict.rf$download_speed == "PREDICT" & 
                             evalset.predict.rf$nw_type == "LTE"),]$lte.d
head(evalset.predict.rf$download_speed, 100)
evalset.predict.rf[which(evalset.predict.rf$upload_speed == "PREDICT" & 
                           evalset.predict.rf$nw_type == "LTE"),]$upload_speed <- 
  evalset.predict.rf[which(evalset.predict.rf$upload_speed == "PREDICT" & 
                             evalset.predict.rf$nw_type == "LTE"),]$lte.u
head(evalset.predict.rf$upload_speed, 100)

evalset.predict.rf[which(evalset.predict.rf$download_speed == "PREDICT" & 
                           evalset.predict.rf$nw_type == "UMTS"),]$download_speed <- 
  evalset.predict.rf[which(evalset.predict.rf$download_speed == "PREDICT" & 
                             evalset.predict.rf$nw_type == "UMTS"),]$umts.d
head(evalset.predict.rf$download_speed, 100)
evalset.predict.rf[which(evalset.predict.rf$upload_speed == "PREDICT" & 
                           evalset.predict.rf$nw_type == "UMTS"),]$upload_speed <- 
  evalset.predict.rf[which(evalset.predict.rf$upload_speed == "PREDICT" & 
                             evalset.predict.rf$nw_type == "UMTS"),]$umts.u
head(evalset.predict.rf$upload_speed, 100)

evalset.predict.rf[which(evalset.predict.rf$download_speed == "PREDICT" & 
                           evalset.predict.rf$nw_type == "CDMA"),]$download_speed <- 
  evalset.predict.rf[which(evalset.predict.rf$download_speed == "PREDICT" & 
                             evalset.predict.rf$nw_type == "CDMA"),]$cdma.d
head(evalset.predict.rf$download_speed, 100)
evalset.predict.rf[which(evalset.predict.rf$upload_speed == "PREDICT" & 
                           evalset.predict.rf$nw_type == "CDMA"),]$upload_speed <- 
  evalset.predict.rf[which(evalset.predict.rf$upload_speed == "PREDICT" & 
                             evalset.predict.rf$nw_type == "CDMA"),]$cdma.u
head(evalset.predict.rf$upload_speed, 100)

evalset.predict.rf[which(evalset.predict.rf$download_speed == "PREDICT" & 
                           evalset.predict.rf$nw_type == "GSM"),]$download_speed <- 
  evalset.predict.rf[which(evalset.predict.rf$download_speed == "PREDICT" & 
                             evalset.predict.rf$nw_type == "GSM"),]$gsm.d
head(evalset.predict.rf$download_speed, 100)
evalset.predict.rf[which(evalset.predict.rf$upload_speed == "PREDICT" & 
                           evalset.predict.rf$nw_type == "GSM"),]$upload_speed <- 
  evalset.predict.rf[which(evalset.predict.rf$upload_speed == "PREDICT" & 
                             evalset.predict.rf$nw_type == "GSM"),]$gsm.u
head(evalset.predict.rf$upload_speed, 100)

#checking missing speeds
length(evalset.predict.rf[which(evalset.predict.rf$download_speed == "PREDICT"),]$download_speed)
length(evalset.predict.rf[which(evalset.predict.rf$download_speed == "SKIP"),]$download_speed)
length(evalset.predict.rf[which(evalset.predict.rf$upload_speed == "PREDICT"),]$upload_speed)
length(evalset.predict.rf[which(evalset.predict.rf$upload_speed == "SKIP"),]$upload_speed)

#Saving outputs

write.table(select(evalset.predict.rf, upload_speed, download_speed), file = "YetAnotherTeam_final_test_0.csv", sep = ",", row.names = F)

#


#