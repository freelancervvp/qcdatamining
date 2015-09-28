#data loading
setwd("C:\\freelancervvp\\Projects\\2015_Data_Mining")
t <- read.csv("training_set.csv")
str(t)
summary(t)


#inserted_at_app formatting
temp <- t
hist(temp$inserted_at_app)
hist(temp$inserted_at_app[temp$inserted_at_app > 1e+12])
hist(temp$inserted_at_app[temp$inserted_at_app < 1e+12])
temp$inserted_at_app[temp$inserted_at_app < 1e+12] <- 
  temp$inserted_at_app[temp$inserted_at_app < 1e+12] * 1000
temp$inserted_at_app <- as.POSIXct(temp$inserted_at_app/1000, origin="1970-01-01")
hist(temp$inserted_at_app, breaks = 10000, xlim = c(as.POSIXct("2014-11-01 00:00:00 MSK"), 
                                                    as.POSIXct("2015-06-01 00:00:00 MSK")))
library(plyr)
ddply(temp, .(strftime(temp$inserted_at_app, "%Y")), nrow) #by years
ddply(temp, .(strftime(temp$inserted_at_app, "%d")), nrow) #by days of month
ddply(temp, .(strftime(temp$inserted_at_app, "%m")), nrow) #by months
ddply(temp, .(strftime(temp$inserted_at_app, "%u")), nrow) #by weekdays
t <- temp


#udid exploration
temp <- t
head(sort(table(temp$udid), decreasing = T), 10)
t <- temp


#phone_type exploration
temp <- t
table(temp$phone_type)
t <- temp


#network_connection_type formatting
temp <- t
temp$network_connection_type <- as.factor(temp$network_connection_type)
t <- temp


#icmp_ping_time formatting
temp <- t
temp$icmp_ping_time <- as.integer(temp$icmp_ping_time)
hist(temp$icmp_ping_time)
t <- temp


#icmp_ping_packet_loss formatting
temp <- t
temp$icmp_ping_packet_loss <- 
  as.numeric(levels(temp$icmp_ping_packet_loss))[as.integer(temp$icmp_ping_packet_loss)]
summary(temp$icmp_ping_packet_loss)
table(temp$icmp_ping_packet_loss)
temp$icmp_ping_packet_loss[which(temp$icmp_ping_packet_loss == -1)] <- NA
summary(temp$icmp_ping_packet_loss)
table(temp$icmp_ping_packet_loss)
str(temp)
t <- temp


#icmp_ping_range formatting
temp <- t
temp$icmp_ping_range <- 
  as.integer(levels(temp$icmp_ping_range))[as.integer(temp$icmp_ping_range)]
boxplot(temp$icmp_ping_range)
hist(temp$icmp_ping_range[which(temp$icmp_ping_range>1000 & temp$icmp_ping_range<10000)], 
     breaks = 100)
hist(temp$icmp_ping_range[which(temp$icmp_ping_range>1500 & temp$icmp_ping_range<2500)],
     breaks = 100)
temp$icmp_ping_range[which(temp$icmp_ping_range > 2000)] <- NA
hist(temp$icmp_ping_range)
str(temp)
t <- temp


#download_speed formatting
temp <- t
head(levels(temp$download_speed), 20)
temp$download_speed <- 
  as.integer(levels(temp$download_speed))[as.integer(temp$download_speed)]
str(temp$download_speed)
summary(temp$download_speed)
hist(temp$download_speed)
hist(temp$download_speed[which(temp$download_speed > 50000 & temp$download_speed < 150000)],
     breaks = 100)
head(temp[which(temp$download_speed > 100000),], 20)
nrow(temp[which(temp$download_speed > 100000),])
temp$download_speed[which(temp$download_speed > 100000)] <- NA
str(temp)
t <- temp


#upload_speed formatting
temp <- t
head(levels(temp$upload_speed), 20)
temp$upload_speed <- 
  as.integer(levels(temp$upload_speed))[as.integer(temp$upload_speed)]
str(temp$upload_speed)
summary(temp$upload_speed)
hist(temp$upload_speed)
hist(temp$upload_speed[which(temp$upload_speed > 50000 & temp$upload_speed < 150000)],
     breaks = 100)
head(temp[which(temp$upload_speed > 100000),], 20)
nrow(temp[which(temp$upload_speed > 100000),])
temp$upload_speed[which(temp$upload_speed > 100000)] <- NA
str(temp)
t <- temp


#network_type and network_type_int formatting
temp <- t
table(temp$network_type, temp$network_type_int)
sort(summary(temp$network_type))
boxplot(temp$download_speed ~ temp$network_type)
temp$network_type_int <- as.factor(temp$network_type_int)
t <- temp


#network_name and network_name_sim formatting
temp <- t
sort(summary(temp$network_name))
sort(summary(temp$network_name_sim))
#still a lot to clean
t <- temp


#network_id and network_id_sim formatting
temp <- t
summary(temp$network_id)
temp$network_id <- as.integer(levels(temp$network_id))[as.integer(temp$network_id)]
summary(temp$network_id)
summary(temp$network_id_sim)
#Correcting id_sim's
levels(temp$network_id_sim)[lapply(levels(temp$network_id_sim), function(x) {stringi::stri_length(x)}) > 6]
temp$network_id_sim[temp$network_id_sim == "22210 22210"] <- "22210"
temp$network_id_sim[temp$network_id_sim == "23001 23001"] <- "23001"
temp$network_id_sim[temp$network_id_sim == "25001 25002"] <- "25001"
temp$network_id_sim <- factor(temp$network_id_sim)
#NAying ""," ", "- ", "NULL"
levels(temp$network_id_sim)[lapply(levels(temp$network_id_sim), function(x) {stringi::stri_length(x)}) < 5]
temp$network_id_sim[temp$network_id_sim == "" | temp$network_id_sim == "- " | 
                      temp$network_id_sim == " " | temp$network_id_sim == "NULL" ] <- NA
temp$network_id_sim <- factor(temp$network_id_sim)

temp$network_id_sim <- as.integer(levels(temp$network_id_sim))[as.integer(temp$network_id_sim)]
t <- temp


#CID and LAC formatting
temp <- t
head(sort(table(temp$CID), decreasing = T), 20)
temp$CID <- as.integer(levels(temp$CID))[as.integer(temp$CID)]
head(sort(table(temp$LAC), decreasing = T), 20)
temp$LAC <- as.integer(levels(temp$LAC))[as.integer(temp$LAC)]
hist(temp$CID)
summary(temp$CID)
hist(temp$LAC)
summary(temp$LAC)
subset(temp$LAC, lapply(temp$LAC, function(x) {stringi::stri_length(x)}) > 5)
temp$LAC[temp$LAC %in% subset(temp$LAC, lapply(temp$LAC, function(x) {stringi::stri_length(x)}) > 5)] <- NA
hist(temp$LAC)
hist(temp$LAC[temp$LAC<5000], breaks = 100)
length(na.omit(temp$LAC[temp$LAC == 0]))
length(na.omit(temp$LAC[temp$LAC == 1]))
t <- temp


#pci formatting
temp <- t
summary(temp$pci)
hist(temp$pci)
temp$pci[temp$pci>503]
temp$pci[temp$pci>503] <- NA
length(temp$pci[temp$pci == -1])
length(temp$pci[temp$pci == 0])
hist(temp$pci[temp$pci > 0])
summary(temp[which(temp$network_type != "LTE"),]$pci)
summary(temp[which(temp$network_type == "LTE"),]$pci)
table(temp[which(temp$network_type == "LTE"),]$pci)
temp$pci[temp$pci<0] <- NA
table(temp$pci)
hist(temp$pci)
t <- temp


#roaming formatting
temp <- t
summary(temp$roaming)
table(temp$roaming)
temp$roaming <- factor(temp$roaming)
summary(temp$roaming)
str(temp$roaming)
t <- temp


#lat and lng formatting
temp <- t
summary(temp$lat)
summary(temp$lng)
t <- temp


#location_precision formatting
temp <- t
summary(temp$location_precision)
hist(temp$location_precision)
hist(temp$location_precision[temp$location_precision > 0 & temp$location_precision < 10000])
hist(temp$location_precision[temp$location_precision > 1000 & temp$location_precision < 10000])
length(temp$location_precision[temp$location_precision < -1])
length(temp$location_precision[temp$location_precision < 0])
temp$location_precision[temp$location_precision < -1] <- NA
temp$location_precision[temp$location_precision > 5000] <- NA
hist(temp$location_precision)
t <- temp


#rssi formatting
temp <- t
summary(temp$rssi)
str(temp$rssi)
temp$rssi <- as.integer(levels(temp$rssi))[as.integer(temp$rssi)]
hist(temp$rssi)
temp$rssi[temp$rssi > -30] <- NA
table(temp$rssi)
summary(temp$rssi)
t <- temp


#bit_error_rate formatting
temp <- t
table(temp$bit_error_rate)
temp$bit_error_rate <- as.integer(levels(temp$bit_error_rate))[as.integer(temp$bit_error_rate)]
summary(temp$bit_error_rate)
table(temp$bit_error_rate)
temp$bit_error_rate[temp$bit_error_rate < -2] <- NA
table(temp$bit_error_rate)
temp$bit_error_rate[temp$bit_error_rate == -1] <- NA
table(temp$bit_error_rate)
summary(temp$bit_error_rate)
str(temp)
t <- temp


#ec_io formatting
temp <- t
head(subset(temp, ec_io != "-1" & ec_io != "NULL"), 20)
head(subset(temp, ec_io != "-1" & ec_io != "NULL")$phone_type)
table(subset(temp, ec_io != "-1" & ec_io != "NULL")$phone_type)
temp$ec_io <- as.numeric(levels(temp$ec_io))[as.integer(temp$ec_io)]
summary(temp$ec_io)
sort(table(temp$ec_io))
hist(temp$ec_io[temp$ec_io > -200 & temp$ec_io < -1])
temp$ec_io[temp$ec_io == -1] <- NA
table(temp$ec_io)
hist(temp$ec_io[temp$ec_io > -200 & temp$ec_io < 0], breaks = 100)
temp$ec_io[temp$ec_io < -315] <- NA
table(temp$ec_io)
temp$ec_io[which(temp$ec_io < -32)] <- temp$ec_io[which(temp$ec_io < -32)]/10
table(temp$ec_io)
temp$ec_io[which(temp$ec_io > 0)] <- temp$ec_io[which(temp$ec_io > 0)] / (-10)
table(temp$ec_io)
hist(temp$ec_io)
t <- temp


#rsrp formatting 
temp <- t
table(temp$rsrp)
temp$rsrp <- as.integer(levels(temp$rsrp))[as.integer(temp$rsrp)]
table(temp[which(temp$network_type != "LTE"),]$rsrp)
table(temp[which(temp$network_type == "LTE"),]$rsrp)
table(temp[which(is.na(temp$rsrp)),]$network_type)
table(temp[which(!is.na(temp$rsrp)),]$network_type)
temp[which(temp$network_type != "LTE"),]$rsrp <- NA
hist(temp$rsrp)
#plotting download_speed vs rsrp for LTE and network_connection_type==0 (i.e. not Wi-Fi)
ggplot(data = temp[which(temp$network_connection_type==0 & temp$network_type == "LTE"),], 
       aes(rsrp, download_speed)) + geom_point()
t <- temp


#rsrq formatting 
temp <- t
levels(temp$rsrq)
table(temp$rsrq)
table(temp[which(temp$network_type != "LTE"),]$rsrq)
table(temp[which(temp$network_type == "LTE"),]$rsrq)
temp$rsrq <- as.integer(levels(temp$rsrq))[as.integer(temp$rsrq)]
table(temp$rsrq)
temp[which(temp$network_type != "LTE"),]$rsrq <- NA
table(temp[which(temp$network_type != "LTE"),]$rsrq)
ggplot(data = temp[which(temp$network_connection_type==0 & temp$network_type == "LTE"),], 
       aes(rsrq, download_speed)) + geom_point()
str(temp)
t <- temp


#rssnr formatting 
temp <- t
head(table(temp$rssnr),20)
temp$rssnr <- as.numeric(levels(temp$rssnr))[as.integer(temp$rssnr)]
hist(temp$rssnr)
summary(temp$rssnr)
hist(temp[which(temp$rssnr < 1),]$rssnr, breaks = 100)
temp[which(temp$rssnr < 0),]$rssnr <- NA
hist(temp$rssnr)
hist(temp[which(temp$rssnr < 100),]$rssnr, breaks = 100)
hist(temp[which(temp$rssnr < 40),]$rssnr, breaks = 100)
table(temp[which(temp$rssnr < 40),]$rssnr)
temp[which(temp$rssnr >= 30),]$rssnr <- NA
hist(temp$rssnr)
summary(temp$rssnr)
table(temp[which(temp$network_type != "LTE"),]$rssnr)
temp[which(temp$network_type != "LTE"),]$rssnr <- NA
hist(temp$rssnr)
summary(temp$rssnr)
str(temp)
t <- temp


#app_version_code formatting
temp <- t
table(temp$app_version_code)
str(temp)
t <- temp


#api_level formatting
temp <- t
summary(temp$api_level)
temp$api_level <- factor(temp$api_level)
str(temp)
t <- temp


#release_code formatting
temp <- t
summary(temp$release_code)
str(temp)
t <- temp


#phone_model formatting
temp <- t
head(sort(table(temp$phone_model), decreasing = T), 50)
str(temp)
t <- temp



#unreliable formatting
temp <- t
summary(temp$unreliable)
hist(temp$unreliable)
table(temp$unreliable)
temp$unreliable <- factor(temp$unreliable)
table(temp$unreliable)
str(temp)
t <- temp


#ip_remote formatting
temp <- t
summary(temp$ip_remote)
head(sort(table(temp$ip_remote), decreasing = T))
str(temp)
t <- temp





#Plotting maps
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


#data exploration
head(subset(temp, network_type == "LTE"), 15)
head(subset(temp, phone_type == "CDMA"), 15)
head(subset(temp, network_connection_type == 1), 15)
head(temp[c("phone_type", "network_type")], 50)
head(subset(temp[c("phone_type", "network_type")], network_type == "unknown"), 50)
head(subset(temp[c("phone_type", "network_type")], phone_type == "WIFI"), 50)
table(temp$phone_type)
table(temp$network_type)


#graphics
#Comparing boxplots of icmp_ping_time by network_connection_type
boxplot(t$icmp_ping_time ~ t$network_connection_type)
title("Comparing boxplots of icmp_ping_time by network_connection_type")

#Plotting download_speed and upload_speed vs icmp_ping_time
ggplot(data = data.frame(t$icmp_ping_time, t$download_speed), 
       aes(t$icmp_ping_time, t$download_speed)) + geom_point() + geom_smooth()
ggplot(data = data.frame(t$icmp_ping_time, t$upload_speed), 
       aes(t$icmp_ping_time, t$upload_speed)) + geom_point() + geom_smooth()
#Plotting download_speed and upload_speed vs icmp_ping_time grouped by network_connection_type
ggplot(data = t[which(t$network_connection_type==0 | t$network_connection_type == 1),], 
       aes(icmp_ping_time, download_speed)) + geom_point()
+ facet_grid(. ~ network_connection_type)


#Plotting boxplot of download_speed by network_type (also for network_connection_type == 0 only)
temp <- t[which(t$network_type %in% c("CDMA", "EDGE", "GPRS", "HSDPA", "HSPA", "HSPAP", "HSUPA", "LTE", "UMTS")),]
temp$network_type <- factor(temp$network_type)
boxplot(temp$download_speed ~ temp$network_type)
temp <- temp[which(temp$network_connection_type == 0),]
boxplot(temp$download_speed ~ temp$network_type)




#first data models
library(caret)
temp_d <- select(temp, -upload_speed)
trainIndex <- createDataPartition(temp_d$download_speed, p = .8, list = FALSE)
temp_d.train <- temp_d[trainIndex,]
temp_d.test <- temp_d[-trainIndex,]


