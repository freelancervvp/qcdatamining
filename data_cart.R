testset <- t
summary(testset$download_speed)

#temp nw_type

summary(temp$nw_type)
temp$nw_type <- NA
temp[which(temp$network_type == "LTE"),]$nw_type <- "LTE"

temp[which(temp$network_type == "1xRTT" | temp$network_type == "CDMA"),]$nw_type <- "CDMA"
temp[which(temp$network_type == "eHRPD"),]$nw_type <- "HRPD"
temp[which(temp$network_type == "EVDO 0" | temp$network_type == "EVDO A" | 
                temp$network_type == "EVDO B"),]$nw_type <- "EVDO"

temp[which(temp$network_type == "EDGE" | temp$network_type == "GPRS"),]$nw_type <- "GSM"

temp[which(temp$network_type == "HSDPA"),]$nw_type <- "HSDPA"
temp[which(temp$network_type == "HSPA"),]$nw_type <- "HSPA"
temp[which(temp$network_type == "HSPAP"),]$nw_type <- "HSPAP"
temp[which(temp$network_type == "HSUPA"),]$nw_type <- "HSUPA"
temp[which(temp$network_type == "UMTS"),]$nw_type <- "UMTS"

temp[which(temp$network_type == "Out of service"),]$nw_type <- "OOS"

temp[which(!is.na(temp$wifi_rssi)),]$nw_type <- "WIFI"

temp$nw_type <- factor(temp$nw_type)
summary(temp$nw_type)



#testset nw_type

summary(testset$nw_type)
testset$nw_type <- NA
testset[which(testset$network_type == "LTE"),]$nw_type <- "LTE"

testset[which(testset$network_type == "1xRTT" | testset$network_type == "CDMA"),]$nw_type <- "CDMA"
testset[which(testset$network_type == "eHRPD"),]$nw_type <- "HRPD"
testset[which(testset$network_type == "EVDO 0" | testset$network_type == "EVDO A" | 
              testset$network_type == "EVDO B"),]$nw_type <- "EVDO"

testset[which(testset$network_type == "EDGE" | testset$network_type == "GPRS"),]$nw_type <- "GSM"

testset[which(testset$network_type == "HSDPA"),]$nw_type <- "HSDPA"
testset[which(testset$network_type == "HSPA"),]$nw_type <- "HSPA"
testset[which(testset$network_type == "HSPAP"),]$nw_type <- "HSPAP"
testset[which(testset$network_type == "HSUPA"),]$nw_type <- "HSUPA"
testset[which(testset$network_type == "UMTS"),]$nw_type <- "UMTS"

testset[which(testset$network_type == "Out of service"),]$nw_type <- "OOS"

testset[which(!is.na(testset$wifi_rssi)),]$nw_type <- "WIFI"

testset$nw_type <- factor(testset$nw_type)
summary(testset$nw_type)




#_____________________________________________________________________________________________________________________________
# RPART Regression Tree

library(caret)
library(dplyr)
library(rpart)
set.seed(16)

temp$new_network_country <- factor(temp$new_network_country)
temp$network_id <- factor(temp$network_id)
temp$new_year <- factor(temp$new_year)
temp$new_month <- factor(temp$new_month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
temp$new_weekday <- factor(temp$new_weekday)
temp$app_version_code <- factor(temp$app_version_code)
str(temp)

# by technology

temp.lte <- temp[which(temp$nw_type == "LTE"),]
temp.gsm <- temp[which(temp$nw_type == "GSM"),]
temp.umts <- temp[which(temp$nw_type == "UMTS"),]
temp.hsdpa <- temp[which(temp$nw_type == "HSDPA"),]
temp.hsupa <- temp[which(temp$nw_type == "HSUPA"),]
temp.hspa <- temp[which(temp$nw_type == "HSPA"),]
temp.hspap <- temp[which(temp$nw_type == "HSPAP"),]
temp.cdma <- temp[which(temp$nw_type == "CDMA"),]
temp.evdo <- temp[which(temp$nw_type == "EVDO"),]
temp.hrpd <- temp[which(temp$nw_type == "HRPD"),]
temp.oos <- temp[which(temp$nw_type == "OOS"),]
temp.wifi <- temp[which(temp$nw_type == "WIFI"),]
temp.na <- temp[which(is.na(temp$nw_type)),]

temp.lte.d <- temp.lte[which(!is.na(temp.lte$download_speed)),]
temp.lte.u <- temp.lte[which(!is.na(temp.lte$upload_speed)),]
temp.gsm.d <- temp.gsm[which(!is.na(temp.gsm$download_speed)),]
temp.gsm.u <- temp.gsm[which(!is.na(temp.gsm$upload_speed)),]
temp.umts.d <- temp.umts[which(!is.na(temp.umts$download_speed)),]
temp.umts.u <- temp.umts[which(!is.na(temp.umts$upload_speed)),]
temp.hsdpa.d <- temp.hsdpa[which(!is.na(temp.hsdpa$download_speed)),]
temp.hsdpa.u <- temp.hsdpa[which(!is.na(temp.hsdpa$upload_speed)),]
temp.hsupa.d <- temp.hsupa[which(!is.na(temp.hsupa$download_speed)),]
temp.hsupa.u <- temp.hsupa[which(!is.na(temp.hsupa$upload_speed)),]
temp.hspa.d <- temp.hspa[which(!is.na(temp.hspa$download_speed)),]
temp.hspa.u <- temp.hspa[which(!is.na(temp.hspa$upload_speed)),]
temp.hspap.d <- temp.hspap[which(!is.na(temp.hspap$download_speed)),]
temp.hspap.u <- temp.hspap[which(!is.na(temp.hspap$upload_speed)),]
temp.cdma.d <- temp.cdma[which(!is.na(temp.cdma$download_speed)),]
temp.cdma.u <- temp.cdma[which(!is.na(temp.cdma$upload_speed)),]
temp.evdo.d <- temp.evdo[which(!is.na(temp.evdo$download_speed)),]
temp.evdo.u <- temp.evdo[which(!is.na(temp.evdo$upload_speed)),]
temp.hrpd.d <- temp.hrpd[which(!is.na(temp.hrpd$download_speed)),]
temp.hrpd.u <- temp.hrpd[which(!is.na(temp.hrpd$upload_speed)),]
temp.oos.d <- temp.oos[which(!is.na(temp.oos$download_speed)),]
temp.oos.u <- temp.oos[which(!is.na(temp.oos$upload_speed)),]
temp.wifi.d <- temp.wifi[which(!is.na(temp.wifi$download_speed)),]
temp.wifi.u <- temp.wifi[which(!is.na(temp.wifi$upload_speed)),]


#LTE
rt.lte.d <- rpart(download_speed ~ new_month + new_year + new_weekday
                  + new_network_country + network_id + rsrp + rsrq + rssnr
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                  data = temp.lte.d, method = "anova", 
                  control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.lte.d)

rt.lte.u <- rpart(upload_speed ~ new_month + new_year + new_weekday
                  + new_network_country + network_id + rsrp + rsrq + rssnr
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                  data = temp.lte.u, method = "anova", 
                  control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.lte.u)

#GSM
rt.gsm.d <- rpart(download_speed ~ new_month + new_year + new_weekday
                   + new_network_country + network_id + rssi
                   + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                   data = temp.gsm.d, method = "anova", 
                   control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.gsm.d)

rt.gsm.u <- rpart(upload_speed ~ new_month + new_year + new_weekday
                   + new_network_country + network_id + rssi
                   + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                   data = temp.gsm.u, method = "anova", 
                   control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.gsm.u)

#UMTS
rt.umts.d <- rpart(download_speed ~ new_month + new_year + new_weekday
                  + new_network_country + network_id + rssi
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                  data = temp.umts.d, method = "anova", 
                  control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.umts.d)

rt.umts.u <- rpart(upload_speed ~ new_month + new_year + new_weekday
                  + new_network_country + network_id + rssi
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                  data = temp.umts.u, method = "anova", 
                  control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.umts.u)

#HSDPA
rt.hsdpa.d <- rpart(download_speed ~ new_month + new_year + new_weekday
                   + new_network_country + network_id + rssi
                   + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                   data = temp.hsdpa.d, method = "anova", 
                   control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.hsdpa.d)

rt.hsdpa.u <- rpart(upload_speed ~ new_month + new_year + new_weekday
                   + new_network_country + network_id + rssi
                   + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                   data = temp.hsdpa.u, method = "anova", 
                   control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.hsdpa.u)

#HSUPA
rt.hsupa.d <- rpart(download_speed ~ new_month + new_year + new_weekday
                    + new_network_country + network_id + rssi
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                    data = temp.hsupa.d, method = "anova", 
                    control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.hsupa.d)

rt.hsupa.u <- rpart(upload_speed ~ new_month + new_year + new_weekday
                    + new_network_country + network_id + rssi
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                    data = temp.hsupa.u, method = "anova", 
                    control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.hsupa.u)

#HSPA
rt.hspa.d <- rpart(download_speed ~ new_month + new_year + new_weekday
                    + new_network_country + network_id + rssi
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                    data = temp.hspa.d, method = "anova", 
                    control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.hspa.d)

rt.hspa.u <- rpart(upload_speed ~ new_month + new_year + new_weekday
                    + new_network_country + network_id + rssi
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                    data = temp.hspa.u, method = "anova", 
                    control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.hspa.u)

#HSPAP
rt.hspap.d <- rpart(download_speed ~ new_month + new_year + new_weekday
                    + new_network_country + network_id + rssi
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                    data = temp.hspap.d, method = "anova", 
                    control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.hspap.d)

rt.hspap.u <- rpart(upload_speed ~ new_month + new_year + new_weekday
                    + new_network_country + network_id + rssi
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                    data = temp.hspap.u, method = "anova", 
                    control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.hspap.u)

#CDMA
rt.cdma.d <- rpart(download_speed ~ new_month + new_year + new_weekday
                    + new_network_country + network_id + rssi + ec_io
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                    data = temp.cdma.d, method = "anova", 
                    control = rpart.control(minsplit = 2, cp = 0.0001))
printcp(rt.cdma.d)

rt.cdma.u <- rpart(upload_speed ~ new_month + new_year + new_weekday
                    + new_network_country + network_id + rssi + ec_io
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                    data = temp.cdma.u, method = "anova", 
                    control = rpart.control(minsplit = 2, cp = 0.0001))
printcp(rt.cdma.u)

#EVDO
rt.evdo.d <- rpart(download_speed ~ new_month + new_year + new_weekday
                    + new_network_country + network_id + rssi + ec_io
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                    data = temp.evdo.d, method = "anova", 
                    control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.evdo.d)

rt.evdo.u <- rpart(upload_speed ~ new_month + new_year + new_weekday
                    + new_network_country + network_id + rssi + ec_io
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                    data = temp.evdo.u, method = "anova", 
                    control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.evdo.u)

#HRPD
rt.hrpd.d <- rpart(download_speed ~ new_month + new_year + new_weekday
                    + new_network_country + network_id + rssi + ec_io
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                    data = temp.hrpd.d, method = "anova", 
                    control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.hrpd.d)

rt.hrpd.u <- rpart(upload_speed ~ new_month + new_year + new_weekday
                    + new_network_country + network_id + rssi + ec_io
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                    data = temp.hrpd.u, method = "anova", 
                    control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.hrpd.u)

#OOS
rt.oos.d <- rpart(download_speed ~ new_month + new_year + new_weekday
                   + new_network_country + network_id + rssi
                   + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                   data = temp.oos.d, method = "anova", 
                   control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.oos.d)

rt.oos.u <- rpart(upload_speed ~ new_month + new_year + new_weekday
                    + new_network_country + network_id + rssi
                    + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                    data = temp.oos.u, method = "anova", 
                    control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.oos.u)

#WIFI
rt.wifi.d <- rpart(download_speed ~ new_month + new_year + new_weekday
                  + new_network_country + wifi_rssi + wifi_band
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                  data = temp.wifi.d, method = "anova", 
                  control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.wifi.d)

rt.wifi.u <- rpart(upload_speed ~ new_month + new_year + new_weekday
                  + new_network_country + wifi_rssi + wifi_band
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                  data = temp.wifi.u, method = "anova", 
                  control = rpart.control(minsplit = 30, cp = 0.001))
printcp(rt.wifi.u)









#_____________________________________________________________________________________________________________________________
#Predicting speeds by technology

str(testset)
testset$new_month <- factor(testset$new_month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
testset$new_year <- factor(testset$new_year)
testset$new_weekday <- factor(testset$new_weekday)
testset$new_network_country <- factor(testset$new_network_country, levels = levels(temp$new_network_country))
testset$network_id <- factor(testset$network_id, levels = levels(temp$network_id))
str(testset)

testset.predict.rt <- select(testset, download_speed, upload_speed, new_month,
                               new_year, new_weekday, new_network_country, network_id, 
                               rssi, ec_io, rsrp, rsrq, rssnr, wifi_rssi, wifi_band,
                               icmp_ping_time, icmp_ping_packet_loss,
                               icmp_ping_range, app_version_code,
                               nw_type)

str(testset.predict.rt)
testset.predict.rt.backup <- testset.predict.rt
#testset.predict.rt <- testset.predict.rt.backup

#come on!
testset.predict.rt$lte.d <- predict(rt.lte.d, testset.predict.rt)
testset.predict.rt$lte.u <- predict(rt.lte.u, testset.predict.rt)
testset.predict.rt$gsm.d <- predict(rt.gsm.d, testset.predict.rt)
testset.predict.rt$gsm.u <- predict(rt.gsm.u, testset.predict.rt)
testset.predict.rt$umts.d <- predict(rt.umts.d, testset.predict.rt)
testset.predict.rt$umts.u <- predict(rt.umts.u, testset.predict.rt)
testset.predict.rt$hsdpa.d <- predict(rt.hsdpa.d, testset.predict.rt)
testset.predict.rt$hsdpa.u <- predict(rt.hsdpa.u, testset.predict.rt)
testset.predict.rt$hsupa.d <- predict(rt.hsupa.d, testset.predict.rt)
testset.predict.rt$hsupa.u <- predict(rt.hsupa.u, testset.predict.rt)
testset.predict.rt$hspa.d <- predict(rt.hspa.d, testset.predict.rt)
testset.predict.rt$hspa.u <- predict(rt.hspa.u, testset.predict.rt)
testset.predict.rt$hspap.d <- predict(rt.hspap.d, testset.predict.rt)
testset.predict.rt$hspap.u <- predict(rt.hspap.u, testset.predict.rt)
testset.predict.rt$cdma.d <- predict(rt.cdma.d, testset.predict.rt)
testset.predict.rt$cdma.u <- predict(rt.cdma.u, testset.predict.rt)
testset.predict.rt$evdo.d <- predict(rt.evdo.d, testset.predict.rt)
testset.predict.rt$evdo.u <- predict(rt.evdo.u, testset.predict.rt)
testset.predict.rt$hrpd.d <- predict(rt.hrpd.d, testset.predict.rt)
testset.predict.rt$hrpd.u <- predict(rt.hrpd.u, testset.predict.rt)
testset.predict.rt$oos.d <- predict(rt.oos.d, testset.predict.rt)
testset.predict.rt$oos.u <- predict(rt.oos.u, testset.predict.rt)
testset.predict.rt$wifi.d <- predict(rt.wifi.d, testset.predict.rt)
testset.predict.rt$wifi.u <- predict(rt.wifi.u, testset.predict.rt)

summary(testset.predict.rt)

#
testset.predict.rt$download_speed <- as.character(levels(testset.predict.rt$download_speed))[testset.predict.rt$download_speed]
table(testset.predict.rt$download_speed)
testset.predict.rt$upload_speed <- as.character(levels(testset.predict.rt$upload_speed))[testset.predict.rt$upload_speed]
table(testset.predict.rt$upload_speed)
#

summary(testset$nw_type)
summary(testset.predict.rt$nw_type)

#Replacing PREDICTs with speeds
testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "LTE"),]$download_speed <- 
  testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "LTE"),]$lte.d
testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "LTE"),]$upload_speed <- 
  testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "LTE"),]$lte.u

testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "GSM"),]$download_speed <- 
  testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "GSM"),]$gsm.d
testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "GSM"),]$upload_speed <- 
  testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "GSM"),]$gsm.u

testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "UMTS"),]$download_speed <- 
  testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "UMTS"),]$umts.d
testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "UMTS"),]$upload_speed <- 
  testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "UMTS"),]$umts.u

testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "HSDPA"),]$download_speed <- 
  testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "HSDPA"),]$hsdpa.d
testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "HSDPA"),]$upload_speed <- 
  testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "HSDPA"),]$hsdpa.u

testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "HSUPA"),]$download_speed <- 
  testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "HSUPA"),]$hsupa.d
testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "HSUPA"),]$upload_speed <- 
  testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "HSUPA"),]$hsupa.u

testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "HSPA"),]$download_speed <- 
  testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "HSPA"),]$hspa.d
testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "HSPA"),]$upload_speed <- 
  testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "HSPA"),]$hspa.u

testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "HSPAP"),]$download_speed <- 
  testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "HSPAP"),]$hspap.d
testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "HSPAP"),]$upload_speed <- 
  testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "HSPAP"),]$hspap.u

testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "CDMA"),]$download_speed <- 
  testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "CDMA"),]$cdma.d
testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "CDMA"),]$upload_speed <- 
  testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "CDMA"),]$cdma.u

testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "EVDO"),]$download_speed <- 
  testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "EVDO"),]$evdo.d
testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "EVDO"),]$upload_speed <- 
  testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "EVDO"),]$evdo.u

testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "HRPD"),]$download_speed <- 
  testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "HRPD"),]$hrpd.d
testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "HRPD"),]$upload_speed <- 
  testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "HRPD"),]$hrpd.u

testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "OOS"),]$download_speed <- 
  testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "OOS"),]$oos.d
testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "OOS"),]$upload_speed <- 
  testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "OOS"),]$oos.u

testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "WIFI"),]$download_speed <- 
  testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT" & testset.predict.rt$nw_type == "WIFI"),]$wifi.d
testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "WIFI"),]$upload_speed <- 
  testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT" & testset.predict.rt$nw_type == "WIFI"),]$wifi.u





#checking missing speeds
length(testset.predict.rt[which(testset.predict.rt$download_speed == "PREDICT"),]$download_speed)
length(testset.predict.rt[which(testset.predict.rt$download_speed == "SKIP"),]$download_speed)
length(testset.predict.rt[which(testset.predict.rt$upload_speed == "PREDICT"),]$upload_speed)
length(testset.predict.rt[which(testset.predict.rt$upload_speed == "SKIP"),]$upload_speed)


#Saving outputs

write.table(select(testset.predict.rt, upload_speed, download_speed), file = "YetAnotherTeam_initial_test_3.csv", sep = ",", row.names = F)

#

save(rt.cdma.d, rt.cdma.u, 
     rt.evdo.d, rt.evdo.u, 
     rt.gsm.d, rt.gsm.u, 
     rt.hrpd.d, rt.hrpd.u, 
     rt.hsdpa.d, rt.hsdpa.u, 
     rt.hspa.d, rt.hspa.u, 
     rt.hspap.d, rt.hspap.u, 
     rt.hsupa.d, rt.hsupa.u, 
     rt.lte.d, rt.lte.u, 
     rt.oos.d, rt.oos.u, 
     rt.umts.d, rt.umts.u, 
     rt.wifi.d, rt.wifi.u, 
     file = "rt")

#


#pruning the trees

rt.p.cdma.d <- prune(rt.cdma.d, cp = rt.cdma.d$cptable[which.min(rt.cdma.d$cptable[,"xerror"]),"CP"]) 
rt.p.cdma.u <- prune(rt.cdma.u, cp = rt.cdma.u$cptable[which.min(rt.cdma.u$cptable[,"xerror"]),"CP"]) 
rt.p.evdo.d <- prune(rt.evdo.d, cp = rt.evdo.d$cptable[which.min(rt.evdo.d$cptable[,"xerror"]),"CP"]) 
rt.p.evdo.u <- prune(rt.evdo.u, cp = rt.evdo.u$cptable[which.min(rt.evdo.u$cptable[,"xerror"]),"CP"]) 
rt.p.gsm.d <- prune(rt.gsm.d, cp = rt.gsm.d$cptable[which.min(rt.gsm.d$cptable[,"xerror"]),"CP"]) 
rt.p.gsm.u <- prune(rt.gsm.u, cp = rt.gsm.u$cptable[which.min(rt.gsm.u$cptable[,"xerror"]),"CP"])
rt.p.hrpd.d <- prune(rt.hrpd.d, cp = rt.hrpd.d$cptable[which.min(rt.hrpd.d$cptable[,"xerror"]),"CP"]) 
rt.p.hrpd.u <- prune(rt.hrpd.u, cp = rt.hrpd.u$cptable[which.min(rt.hrpd.u$cptable[,"xerror"]),"CP"])
rt.p.hsdpa.d <- prune(rt.hsdpa.d, cp = rt.hsdpa.d$cptable[which.min(rt.hsdpa.d$cptable[,"xerror"]),"CP"]) 
rt.p.hsdpa.u <- prune(rt.hsdpa.u, cp = rt.hsdpa.u$cptable[which.min(rt.hsdpa.u$cptable[,"xerror"]),"CP"])
rt.p.hspa.d <- prune(rt.hspa.d, cp = rt.hspa.d$cptable[which.min(rt.hspa.d$cptable[,"xerror"]),"CP"]) 
rt.p.hspa.u <- prune(rt.hspa.u, cp = rt.hspa.u$cptable[which.min(rt.hspa.u$cptable[,"xerror"]),"CP"])
rt.p.hspap.d <- prune(rt.hspap.d, cp = rt.hspap.d$cptable[which.min(rt.hspap.d$cptable[,"xerror"]),"CP"]) 
rt.p.hspap.u <- prune(rt.hspap.u, cp = rt.hspap.u$cptable[which.min(rt.hspap.u$cptable[,"xerror"]),"CP"])
rt.p.hsupa.d <- prune(rt.hsupa.d, cp = rt.hsupa.d$cptable[which.min(rt.hsupa.d$cptable[,"xerror"]),"CP"]) 
rt.p.hsupa.u <- prune(rt.hsupa.u, cp = rt.hsupa.u$cptable[which.min(rt.hsupa.u$cptable[,"xerror"]),"CP"])
rt.p.lte.d <- prune(rt.lte.d, cp = rt.lte.d$cptable[which.min(rt.lte.d$cptable[,"xerror"]),"CP"]) 
rt.p.lte.u <- prune(rt.lte.u, cp = rt.lte.u$cptable[which.min(rt.lte.u$cptable[,"xerror"]),"CP"])
rt.p.oos.d <- prune(rt.oos.d, cp = rt.oos.d$cptable[which.min(rt.oos.d$cptable[,"xerror"]),"CP"]) 
rt.p.oos.u <- prune(rt.oos.u, cp = rt.oos.u$cptable[which.min(rt.oos.u$cptable[,"xerror"]),"CP"])
rt.p.umts.d <- prune(rt.umts.d, cp = rt.umts.d$cptable[which.min(rt.umts.d$cptable[,"xerror"]),"CP"]) 
rt.p.umts.u <- prune(rt.umts.u, cp = rt.umts.u$cptable[which.min(rt.umts.u$cptable[,"xerror"]),"CP"])
rt.p.wifi.d <- prune(rt.wifi.d, cp = rt.wifi.d$cptable[which.min(rt.wifi.d$cptable[,"xerror"]),"CP"]) 
rt.p.wifi.u <- prune(rt.wifi.u, cp = rt.wifi.u$cptable[which.min(rt.wifi.u$cptable[,"xerror"]),"CP"])

save(rt.p.cdma.d, rt.p.cdma.u, 
     rt.p.evdo.d, rt.p.evdo.u, 
     rt.p.gsm.d, rt.p.gsm.u, 
     rt.p.hrpd.d, rt.p.hrpd.u, 
     rt.p.hsdpa.d, rt.p.hsdpa.u, 
     rt.p.hspa.d, rt.p.hspa.u, 
     rt.p.hspap.d, rt.p.hspap.u, 
     rt.p.hsupa.d, rt.p.hsupa.u, 
     rt.p.lte.d, rt.p.lte.u, 
     rt.p.oos.d, rt.p.oos.u, 
     rt.p.umts.d, rt.p.umts.u, 
     rt.p.wifi.d, rt.p.wifi.u, 
     file = "rt.p")
