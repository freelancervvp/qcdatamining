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




#evalset nw_type

summary(evalset$nw_type)
evalset$nw_type <- NA
evalset[which(evalset$network_type == "LTE"),]$nw_type <- "LTE"

evalset[which(evalset$network_type == "1xRTT" | evalset$network_type == "CDMA"),]$nw_type <- "CDMA"
evalset[which(evalset$network_type == "eHRPD"),]$nw_type <- "HRPD"
evalset[which(evalset$network_type == "EVDO 0" | evalset$network_type == "EVDO A" | 
                evalset$network_type == "EVDO B"),]$nw_type <- "EVDO"

evalset[which(evalset$network_type == "EDGE" | evalset$network_type == "GPRS"),]$nw_type <- "GSM"

evalset[which(evalset$network_type == "HSDPA"),]$nw_type <- "HSDPA"
evalset[which(evalset$network_type == "HSPA"),]$nw_type <- "HSPA"
evalset[which(evalset$network_type == "HSPAP"),]$nw_type <- "HSPAP"
evalset[which(evalset$network_type == "HSUPA"),]$nw_type <- "HSUPA"
evalset[which(evalset$network_type == "UMTS"),]$nw_type <- "UMTS"

evalset[which(evalset$network_type == "Out of service"),]$nw_type <- "OOS"

evalset[which(!is.na(evalset$wifi_rssi)),]$nw_type <- "WIFI"

evalset$nw_type <- factor(evalset$nw_type)
summary(evalset$nw_type)



library(caret)
library(dplyr)
library(rpart)
set.seed(16)

#_____________________________________________________________________________________________________________________________
#Predicting speeds by technology

str(evalset)
evalset$new_month <- factor(evalset$new_month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
evalset$new_year <- factor(evalset$new_year)
evalset$new_weekday <- factor(evalset$new_weekday)
evalset$new_network_country <- factor(evalset$new_network_country, levels = levels(temp$new_network_country))
evalset$network_id <- factor(evalset$network_id, levels = levels(temp$network_id))
str(evalset$app_version_code)
summary(evalset$app_version_code)
evalset$app_version_code <- factor(evalset$app_version_code, levels = levels(temp$app_version_code))
str(evalset$app_version_code)
summary(evalset$app_version_code)
str(evalset)

evalset.predict.rt <- select(evalset, download_speed, upload_speed, new_month,
                             new_year, new_weekday, new_network_country, network_id, 
                             rssi, ec_io, rsrp, rsrq, rssnr, wifi_rssi, wifi_band,
                             icmp_ping_time, icmp_ping_packet_loss,
                             icmp_ping_range, app_version_code,
                             nw_type)

str(evalset.predict.rt)
evalset.predict.rt.backup <- evalset.predict.rt


#come on!
evalset.predict.rt$lte.d <- predict(rt.lte.d, evalset.predict.rt)
evalset.predict.rt$lte.u <- predict(rt.lte.u, evalset.predict.rt)
evalset.predict.rt$gsm.d <- predict(rt.gsm.d, evalset.predict.rt)
evalset.predict.rt$gsm.u <- predict(rt.gsm.u, evalset.predict.rt)
evalset.predict.rt$umts.d <- predict(rt.umts.d, evalset.predict.rt)
evalset.predict.rt$umts.u <- predict(rt.umts.u, evalset.predict.rt)
evalset.predict.rt$hsdpa.d <- predict(rt.hsdpa.d, evalset.predict.rt)
evalset.predict.rt$hsdpa.u <- predict(rt.hsdpa.u, evalset.predict.rt)
evalset.predict.rt$hsupa.d <- predict(rt.hsupa.d, evalset.predict.rt)
evalset.predict.rt$hsupa.u <- predict(rt.hsupa.u, evalset.predict.rt)
evalset.predict.rt$hspa.d <- predict(rt.hspa.d, evalset.predict.rt)
evalset.predict.rt$hspa.u <- predict(rt.hspa.u, evalset.predict.rt)
evalset.predict.rt$hspap.d <- predict(rt.hspap.d, evalset.predict.rt)
evalset.predict.rt$hspap.u <- predict(rt.hspap.u, evalset.predict.rt)
evalset.predict.rt$cdma.d <- predict(rt.cdma.d, evalset.predict.rt)
evalset.predict.rt$cdma.u <- predict(rt.cdma.u, evalset.predict.rt)
evalset.predict.rt$evdo.d <- predict(rt.evdo.d, evalset.predict.rt)
evalset.predict.rt$evdo.u <- predict(rt.evdo.u, evalset.predict.rt)
evalset.predict.rt$hrpd.d <- predict(rt.hrpd.d, evalset.predict.rt)
evalset.predict.rt$hrpd.u <- predict(rt.hrpd.u, evalset.predict.rt)
evalset.predict.rt$oos.d <- predict(rt.oos.d, evalset.predict.rt)
evalset.predict.rt$oos.u <- predict(rt.oos.u, evalset.predict.rt)
evalset.predict.rt$wifi.d <- predict(rt.wifi.d, evalset.predict.rt)
evalset.predict.rt$wifi.u <- predict(rt.wifi.u, evalset.predict.rt)

summary(evalset.predict.rt)

#
evalset.predict.rt$download_speed <- as.character(levels(evalset.predict.rt$download_speed))[evalset.predict.rt$download_speed]
table(evalset.predict.rt$download_speed)
evalset.predict.rt$upload_speed <- as.character(levels(evalset.predict.rt$upload_speed))[evalset.predict.rt$upload_speed]
table(evalset.predict.rt$upload_speed)
#

summary(evalset$nw_type)
summary(evalset.predict.rt$nw_type)

#Replacing PREDICTs with speeds
evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "LTE"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "LTE"),]$lte.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "LTE"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "LTE"),]$lte.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "GSM"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "GSM"),]$gsm.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "GSM"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "GSM"),]$gsm.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "UMTS"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "UMTS"),]$umts.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "UMTS"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "UMTS"),]$umts.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSDPA"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSDPA"),]$hsdpa.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSDPA"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSDPA"),]$hsdpa.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSUPA"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSUPA"),]$hsupa.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSUPA"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSUPA"),]$hsupa.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPA"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPA"),]$hspa.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPA"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPA"),]$hspa.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPAP"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPAP"),]$hspap.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPAP"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPAP"),]$hspap.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "CDMA"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "CDMA"),]$cdma.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "CDMA"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "CDMA"),]$cdma.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "EVDO"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "EVDO"),]$evdo.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "EVDO"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "EVDO"),]$evdo.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HRPD"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HRPD"),]$hrpd.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HRPD"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HRPD"),]$hrpd.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "OOS"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "OOS"),]$oos.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "OOS"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "OOS"),]$oos.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "WIFI"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "WIFI"),]$wifi.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "WIFI"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "WIFI"),]$wifi.u





#checking missing speeds
length(evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT"),]$download_speed)
length(evalset.predict.rt[which(evalset.predict.rt$download_speed == "SKIP"),]$download_speed)
length(evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT"),]$upload_speed)
length(evalset.predict.rt[which(evalset.predict.rt$upload_speed == "SKIP"),]$upload_speed)


#Saving outputs

write.table(select(evalset.predict.rt, upload_speed, download_speed), file = "YetAnotherTeam_final_test_1.csv", sep = ",", row.names = F)



#
# using pruned trees


#come on!
evalset.predict.rt$lte.d <- predict(rt.p.lte.d, evalset.predict.rt)
evalset.predict.rt$lte.u <- predict(rt.p.lte.u, evalset.predict.rt)
evalset.predict.rt$gsm.d <- predict(rt.p.gsm.d, evalset.predict.rt)
evalset.predict.rt$gsm.u <- predict(rt.p.gsm.u, evalset.predict.rt)
evalset.predict.rt$umts.d <- predict(rt.p.umts.d, evalset.predict.rt)
evalset.predict.rt$umts.u <- predict(rt.p.umts.u, evalset.predict.rt)
evalset.predict.rt$hsdpa.d <- predict(rt.p.hsdpa.d, evalset.predict.rt)
evalset.predict.rt$hsdpa.u <- predict(rt.p.hsdpa.u, evalset.predict.rt)
evalset.predict.rt$hsupa.d <- predict(rt.p.hsupa.d, evalset.predict.rt)
evalset.predict.rt$hsupa.u <- predict(rt.p.hsupa.u, evalset.predict.rt)
evalset.predict.rt$hspa.d <- predict(rt.p.hspa.d, evalset.predict.rt)
evalset.predict.rt$hspa.u <- predict(rt.p.hspa.u, evalset.predict.rt)
evalset.predict.rt$hspap.d <- predict(rt.p.hspap.d, evalset.predict.rt)
evalset.predict.rt$hspap.u <- predict(rt.p.hspap.u, evalset.predict.rt)
evalset.predict.rt$cdma.d <- predict(rt.p.cdma.d, evalset.predict.rt)
evalset.predict.rt$cdma.u <- predict(rt.p.cdma.u, evalset.predict.rt)
evalset.predict.rt$evdo.d <- predict(rt.p.evdo.d, evalset.predict.rt)
evalset.predict.rt$evdo.u <- predict(rt.p.evdo.u, evalset.predict.rt)
evalset.predict.rt$hrpd.d <- predict(rt.p.hrpd.d, evalset.predict.rt)
evalset.predict.rt$hrpd.u <- predict(rt.p.hrpd.u, evalset.predict.rt)
evalset.predict.rt$oos.d <- predict(rt.p.oos.d, evalset.predict.rt)
evalset.predict.rt$oos.u <- predict(rt.p.oos.u, evalset.predict.rt)
evalset.predict.rt$wifi.d <- predict(rt.p.wifi.d, evalset.predict.rt)
evalset.predict.rt$wifi.u <- predict(rt.p.wifi.u, evalset.predict.rt)

summary(evalset.predict.rt)

evalset.predict.rt$download_speed <- evalset$download_speed
evalset.predict.rt$upload_speed <- evalset$upload_speed

#
evalset.predict.rt$download_speed <- as.character(levels(evalset.predict.rt$download_speed))[evalset.predict.rt$download_speed]
table(evalset.predict.rt$download_speed)
evalset.predict.rt$upload_speed <- as.character(levels(evalset.predict.rt$upload_speed))[evalset.predict.rt$upload_speed]
table(evalset.predict.rt$upload_speed)
#

summary(evalset$nw_type)
summary(evalset.predict.rt$nw_type)

#Replacing PREDICTs with speeds
evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "LTE"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "LTE"),]$lte.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "LTE"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "LTE"),]$lte.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "GSM"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "GSM"),]$gsm.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "GSM"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "GSM"),]$gsm.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "UMTS"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "UMTS"),]$umts.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "UMTS"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "UMTS"),]$umts.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSDPA"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSDPA"),]$hsdpa.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSDPA"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSDPA"),]$hsdpa.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSUPA"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSUPA"),]$hsupa.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSUPA"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSUPA"),]$hsupa.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPA"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPA"),]$hspa.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPA"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPA"),]$hspa.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPAP"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPAP"),]$hspap.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPAP"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HSPAP"),]$hspap.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "CDMA"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "CDMA"),]$cdma.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "CDMA"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "CDMA"),]$cdma.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "EVDO"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "EVDO"),]$evdo.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "EVDO"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "EVDO"),]$evdo.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HRPD"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "HRPD"),]$hrpd.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HRPD"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "HRPD"),]$hrpd.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "OOS"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "OOS"),]$oos.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "OOS"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "OOS"),]$oos.u

evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "WIFI"),]$download_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT" & evalset.predict.rt$nw_type == "WIFI"),]$wifi.d
evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "WIFI"),]$upload_speed <- 
  evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT" & evalset.predict.rt$nw_type == "WIFI"),]$wifi.u





#checking missing speeds
length(evalset.predict.rt[which(evalset.predict.rt$download_speed == "PREDICT"),]$download_speed)
length(evalset.predict.rt[which(evalset.predict.rt$download_speed == "SKIP"),]$download_speed)
length(evalset.predict.rt[which(evalset.predict.rt$upload_speed == "PREDICT"),]$upload_speed)
length(evalset.predict.rt[which(evalset.predict.rt$upload_speed == "SKIP"),]$upload_speed)


#Saving outputs

write.table(select(evalset.predict.rt, upload_speed, download_speed), file = "YetAnotherTeam_final_test_2.csv", sep = ",", row.names = F)



#