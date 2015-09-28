#_____________________________________________________________________________________________________________________________
# LTE RPART Regression Tree

library(caret)
library(dplyr)
library(rpart)
library(rattle)
set.seed(16)
temp$new_network_country <- factor(temp$new_network_country)
temp$network_id <- factor(temp$network_id)
temp.lte <- temp[which(temp$nw_type == "LTE"),]

# LTE DL RPART

summary(temp.lte$download_speed)
# remove NAs in download_speed column
temp.lte.d <- temp.lte[which(!is.na(temp.lte$download_speed)),]
summary(temp.lte.d$download_speed)
str(temp.lte.d)
temp.lte.d$new_year <- factor(temp.lte.d$new_year)
temp.lte.d$new_month <- factor(temp.lte.d$new_month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
temp.lte.d$new_weekday <- factor(temp.lte.d$new_weekday)
temp.lte.d$new_network_country <- factor(temp.lte.d$new_network_country, levels = levels(temp$new_network_country))
temp.lte.d$network_id <- factor(temp.lte.d$network_id, levels = levels(temp$network_id))
temp.lte.d$app_version_code <- factor(temp.lte.d$app_version_code, levels = levels(temp$app_version_code))
# create train and test sets
trainIndex <- createDataPartition(temp.lte.d$download_speed, p = .8, list = FALSE)
temp.lte.d.train <- temp.lte.d[trainIndex,]
temp.lte.d.test <- temp.lte.d[-trainIndex,]
Sys.time()
rt.lte.d <- rpart(download_speed ~ new_month + new_year + new_weekday
                         + new_network_country + network_id + rsrp + rsrq + rssnr
                         + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                         data = temp.lte.d.train, method = "anova", 
                         control = rpart.control(minsplit = 30, cp = 0.001))
Sys.time()
printcp(rt.lte.d)
plotcp(rt.lte.d) 
print(rt.lte.d)
plot(rt.lte.d)
text(rt.lte.d)
par(mfrow=c(1,2))
rsq.rpart(rt.lte.d)

#min.xerror <- rt.lte.d$cptable[which.min(rt.lte.d$cptable[,"xerror"]),"CP"]
#rt.lte.d.pruned <- prune(rt.lte.d,cp = min.xerror) 
#print(rt.lte.d.pruned)
#printcp(rt.lte.d.pruned)

#prediction
temp.lte.d.test.prediction <- cbind(predict(rt.lte.d, temp.lte.d.test), temp.lte.d.test$download_speed)
summary(temp.lte.d.test.prediction)
#temp.lte.d.test.prediction <- na.omit(temp.lte.d.test.prediction)
#summary(temp.lte.d.test.prediction)
rsq.lte.d.2 <- 1 - sum((temp.lte.d.test.prediction[,2] - temp.lte.d.test.prediction[,1])^2) / 
  sum((temp.lte.d.test.prediction[,2] - mean(temp.lte.d.test.prediction[,2]))^2)
rmsle.lte.d.2 <- sqrt(1 / nrow(temp.lte.d.test.prediction) * 
                      sum((log1p(temp.lte.d.test.prediction[,1]) - 
                             log1p(temp.lte.d.test.prediction[,2]))^2))
mse.lte.d.2 <- sqrt(sum((temp.lte.d.test.prediction[,1] - 
                         temp.lte.d.test.prediction[,2])^2) / nrow(temp.lte.d.test.prediction))

# LTE DL CTREE

library(party)

Sys.time()
ct.lte.d <- ctree(download_speed ~ new_month + new_year + new_weekday
                  + new_network_country + network_id + rsrp + rsrq + rssnr
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                  data = temp.lte.d.train, control = ctree_control(minsplit=100))
Sys.time()
printcp(rt.lte.d)
plotcp(rt.lte.d) 
print(rt.lte.d)
plot(rt.lte.d)
text(rt.lte.d)
par(mfrow=c(1,2))
rsq.rpart(rt.lte.d)

#prediction
temp.lte.d.test.prediction <- cbind(predict(rt.lte.d, temp.lte.d.test), temp.lte.d.test$download_speed)
summary(temp.lte.d.test.prediction)
#temp.lte.d.test.prediction <- na.omit(temp.lte.d.test.prediction)
#summary(temp.lte.d.test.prediction)
rsq.lte.d.2 <- 1 - sum((temp.lte.d.test.prediction[,2] - temp.lte.d.test.prediction[,1])^2) / 
  sum((temp.lte.d.test.prediction[,2] - mean(temp.lte.d.test.prediction[,2]))^2)
rmsle.lte.d.2 <- sqrt(1 / nrow(temp.lte.d.test.prediction) * 
                        sum((log10(temp.lte.d.test.prediction[,1] + 1) - 
                               log10(temp.lte.d.test.prediction[,2] + 1))^2))
mse.lte.d.2 <- sqrt(sum((temp.lte.d.test.prediction[,1] - 
                           temp.lte.d.test.prediction[,2])^2) / nrow(temp.lte.d.test.prediction))



#_____________________________________________________________________________________________________________________________
# UMTS RPART

library(caret)
library(dplyr)
library(randomForest)
library(rpart)
library(rattle)
set.seed(16)
temp$new_network_country <- factor(temp$new_network_country)
temp$network_id <- factor(temp$network_id)
temp.umts <- temp[which(temp$nw_type == "UMTS"),]

# UMTS DL RANDOMFOREST

summary(temp.umts$download_speed)
# remove NAs in download_speed column
temp.umts.d <- temp.umts[which(!is.na(temp.umts$download_speed)),]
summary(temp.umts.d$download_speed)
str(temp.umts.d)
temp.umts.d$new_year <- factor(temp.umts.d$new_year)
temp.umts.d$new_month <- factor(temp.umts.d$new_month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
temp.umts.d$new_weekday <- factor(temp.umts.d$new_weekday)
temp.umts.d$new_network_country <- factor(temp.umts.d$new_network_country, levels = levels(temp$new_network_country))
temp.umts.d$network_id <- factor(temp.umts.d$network_id, levels = levels(temp$network_id))
temp.umts.d$app_version_code <- factor(temp.umts.d$app_version_code, levels = levels(temp$app_version_code))
# create train and test sets
trainIndex <- createDataPartition(temp.umts.d$download_speed, p = .8, list = FALSE)
temp.umts.d.train <- temp.umts.d[trainIndex,]
temp.umts.d.test <- temp.umts.d[-trainIndex,]
rt.umts.d <- rpart(download_speed ~ new_month + new_year + new_weekday +
                            new_network_country + network_id + rssi + test_type +
                            icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code +
                            network_type,
                            data = temp.umts.d.train, method = "anova", 
                            control=rpart.control(minsplit = 30, cp = 0.001, maxdepth = 5))
Sys.time()
printcp(rt.umts.d)
plotcp(rt.umts.d) 
print(rt.umts.d)
plot(rt.umts.d)
text(rt.umts.d)
par(mfrow=c(1,2))
rsq.rpart(rt.umts.d)
fancyRpartPlot(rt.umts.d)

#prediction
temp.umts.d.test.prediction <- cbind(predict(rt.umts.d, temp.umts.d.test), temp.umts.d.test$download_speed)
summary(temp.umts.d.test.prediction)
temp.umts.d.test.prediction <- na.omit(temp.umts.d.test.prediction)
summary(temp.umts.d.test.prediction)
rsq.umts.d.2 <- 1 - sum((temp.umts.d.test.prediction[,2] - temp.umts.d.test.prediction[,1])^2) / 
  sum((temp.umts.d.test.prediction[,2] - mean(temp.umts.d.test.prediction[,2]))^2)
rmsle.umts.d.2 <- sqrt(1 / nrow(temp.umts.d.test.prediction) * 
                       sum((log1p(temp.umts.d.test.prediction[,1]) - 
                              log1p(temp.umts.d.test.prediction[,2]))^2))
mse.umts.d.2 <- sqrt(sum((temp.umts.d.test.prediction[,1] - 
                          temp.umts.d.test.prediction[,2])^2) / nrow(temp.umts.d.test.prediction))


# LTE DL LM

summary(temp.lte$download_speed)
# remove NAs in download_speed column
temp.lte.d <- temp.lte[which(!is.na(temp.lte$download_speed)),]
summary(temp.lte.d$download_speed)
str(temp.lte.d)
temp.lte.d$new_year <- factor(temp.lte.d$new_year)
temp.lte.d$new_month <- factor(temp.lte.d$new_month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
temp.lte.d$new_weekday <- factor(temp.lte.d$new_weekday)
temp.lte.d$new_network_country <- factor(temp.lte.d$new_network_country, levels = levels(temp$new_network_country))
temp.lte.d$network_id <- factor(temp.lte.d$network_id, levels = levels(temp$network_id))
temp.lte.d$app_version_code <- factor(temp.lte.d$app_version_code, levels = levels(temp$app_version_code))
# create train and test sets
trainIndex <- createDataPartition(temp.lte.d$download_speed, p = .8, list = FALSE)
temp.lte.d.train <- temp.lte.d[trainIndex,]
temp.lte.d.test <- temp.lte.d[-trainIndex,]
Sys.time()
lm.lte.d <- lm(download_speed ~ new_month + new_year + new_weekday
                  + new_network_country + network_id + rsrp + rsrq + rssnr
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                  data = temp.lte.d.train)
Sys.time()
printcp(rt.lte.d)
plotcp(rt.lte.d) 
print(rt.lte.d)
plot(rt.lte.d)
text(rt.lte.d)
par(mfrow=c(1,2))
rsq.rpart(rt.lte.d)

#min.xerror <- rt.lte.d$cptable[which.min(rt.lte.d$cptable[,"xerror"]),"CP"]
#rt.lte.d.pruned <- prune(rt.lte.d,cp = min.xerror) 
#print(rt.lte.d.pruned)
#printcp(rt.lte.d.pruned)

#prediction
temp.lte.d.test.prediction <- cbind(predict(rt.lte.d, temp.lte.d.test), temp.lte.d.test$download_speed)
summary(temp.lte.d.test.prediction)
#temp.lte.d.test.prediction <- na.omit(temp.lte.d.test.prediction)
#summary(temp.lte.d.test.prediction)
rsq.lte.d.2 <- 1 - sum((temp.lte.d.test.prediction[,2] - temp.lte.d.test.prediction[,1])^2) / 
  sum((temp.lte.d.test.prediction[,2] - mean(temp.lte.d.test.prediction[,2]))^2)
rmsle.lte.d.2 <- sqrt(1 / nrow(temp.lte.d.test.prediction) * 
                        sum((log10(temp.lte.d.test.prediction[,1] + 1) - 
                               log10(temp.lte.d.test.prediction[,2] + 1))^2))
mse.lte.d.2 <- sqrt(sum((temp.lte.d.test.prediction[,1] - 
                           temp.lte.d.test.prediction[,2])^2) / nrow(temp.lte.d.test.prediction))



#_____________________________________________________________________________________________________________________________
# Categorize download and upload speeds
# LTE RPART Classification Tree


#temp$download_speed_split <- cut(temp$download_speed, breaks = seq(0,100000,500), labels = seq(250,99750,500))
#temp$download_speed_split <- cut(temp$download_speed, breaks = seq(0,100000,2000), labels = seq(1000,99000,2000))
temp$download_speed_split <- cut(temp$download_speed, breaks = seq(0,100000,1000), labels = seq(500,99500,1000))
summary(temp$download_speed_split)

library(caret)
library(dplyr)
library(rpart)
library(rattle)
set.seed(16)
temp$new_network_country <- factor(temp$new_network_country)
temp$network_id <- factor(temp$network_id)
temp.lte <- temp[which(temp$nw_type == "LTE"),]

# LTE DL RPART Classification

summary(temp.lte$download_speed_split)
# remove NAs in download_speed column
temp.lte.d <- temp.lte[which(!is.na(temp.lte$download_speed_split)),]
summary(temp.lte.d$download_speed_split)
str(temp.lte.d)
temp.lte.d$new_year <- factor(temp.lte.d$new_year)
temp.lte.d$new_month <- factor(temp.lte.d$new_month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
temp.lte.d$new_weekday <- factor(temp.lte.d$new_weekday)
temp.lte.d$new_network_country <- factor(temp.lte.d$new_network_country, levels = levels(temp$new_network_country))
temp.lte.d$network_id <- factor(temp.lte.d$network_id, levels = levels(temp$network_id))
temp.lte.d$app_version_code <- factor(temp.lte.d$app_version_code, levels = levels(temp$app_version_code))
# create train and test sets
trainIndex <- createDataPartition(temp.lte.d$download_speed_split, p = .8, list = FALSE)
temp.lte.d.train <- temp.lte.d[trainIndex,]
temp.lte.d.test <- temp.lte.d[-trainIndex,]
Sys.time()
rt.lte.d <- rpart(download_speed_split ~ new_month + new_year + new_weekday
                  + new_network_country + network_id + rsrp + rsrq + rssnr
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                  data = temp.lte.d.train, method = "anova", control=rpart.control(minsplit = 30, cp=0.001))
Sys.time()
printcp(rt.lte.d)
plotcp(rt.lte.d) 
print(rt.lte.d)
plot(rt.lte.d)
text(rt.lte.d)
par(mfrow=c(1,2))
rsq.rpart(rt.lte.d)

#prediction
temp.lte.d.test.prediction <- cbind(as.numeric(levels(temp$download_speed_split)[predict(rt.lte.d, temp.lte.d.test)]), as.numeric(temp.lte.d.test$download_speed))
summary(temp.lte.d.test.prediction)
#temp.lte.d.test.prediction <- na.omit(temp.lte.d.test.prediction)
#summary(temp.lte.d.test.prediction)
rsq.lte.d.2 <- 1 - sum((temp.lte.d.test.prediction[,2] - temp.lte.d.test.prediction[,1])^2) / 
  sum((temp.lte.d.test.prediction[,2] - mean(temp.lte.d.test.prediction[,2]))^2)
rmsle.lte.d.2 <- sqrt(1 / nrow(temp.lte.d.test.prediction) * 
                        sum((log1p(temp.lte.d.test.prediction[,1]) - 
                               log1p(temp.lte.d.test.prediction[,2]))^2))
mse.lte.d.2 <- sqrt(sum((temp.lte.d.test.prediction[,1] - 
                           temp.lte.d.test.prediction[,2])^2) / nrow(temp.lte.d.test.prediction))





#_____________________________________________________________________________________________________________________________
# LTE GBM Regression 

library(caret)
library(dplyr)
library(gbm)
set.seed(16)
temp$new_network_country <- factor(temp$new_network_country)
temp$network_id <- factor(temp$network_id)
temp.lte <- temp[which(temp$nw_type == "LTE"),]

# LTE DL RPART

summary(temp.lte$download_speed)
# remove NAs in download_speed column
temp.lte.d <- temp.lte[which(!is.na(temp.lte$download_speed)),]
summary(temp.lte.d$download_speed)
str(temp.lte.d)
temp.lte.d$new_year <- factor(temp.lte.d$new_year)
temp.lte.d$new_month <- factor(temp.lte.d$new_month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
temp.lte.d$new_weekday <- factor(temp.lte.d$new_weekday)
temp.lte.d$new_network_country <- factor(temp.lte.d$new_network_country, levels = levels(temp$new_network_country))
temp.lte.d$network_id <- factor(temp.lte.d$network_id, levels = levels(temp$network_id))
temp.lte.d$app_version_code <- factor(temp.lte.d$app_version_code, levels = levels(temp$app_version_code))
# create train and test sets
trainIndex <- createDataPartition(temp.lte.d$download_speed, p = .8, list = FALSE)
temp.lte.d.train <- temp.lte.d[trainIndex,]
temp.lte.d.test <- temp.lte.d[-trainIndex,]
Sys.time()
gbm.lte.d <- gbm(download_speed ~ new_month + new_year + new_weekday
                  + new_network_country  + rsrp + rsrq + rssnr
                  + icmp_ping_time + icmp_ping_packet_loss + icmp_ping_range + app_version_code, 
                  data = temp.lte.d.train, n.trees = 500)
Sys.time()
summary.gbm(gbm.lte.d)
best.iter <- gbm.perf(gbm.lte.d,method="OOB")
best.iter

#min.xerror <- rt.lte.d$cptable[which.min(rt.lte.d$cptable[,"xerror"]),"CP"]
#rt.lte.d.pruned <- prune(rt.lte.d,cp = min.xerror) 
#print(rt.lte.d.pruned)
#printcp(rt.lte.d.pruned)

#prediction
temp.lte.d.test.prediction <- cbind(predict(gbm.lte.d, temp.lte.d.test, best.iter), temp.lte.d.test$download_speed)
summary(temp.lte.d.test.prediction)
str(temp.lte.d.test.prediction)
#temp.lte.d.test.prediction <- na.omit(temp.lte.d.test.prediction)
#summary(temp.lte.d.test.prediction)
rsq.lte.d.2 <- 1 - sum((temp.lte.d.test.prediction[,2] - temp.lte.d.test.prediction[,1])^2) / 
  sum((temp.lte.d.test.prediction[,2] - mean(temp.lte.d.test.prediction[,2]))^2)
rmsle.lte.d.2 <- sqrt(1 / nrow(temp.lte.d.test.prediction) * 
                        sum((log1p(temp.lte.d.test.prediction[,1]) - 
                               log1p(temp.lte.d.test.prediction[,2]))^2))
mse.lte.d.2 <- sqrt(sum((temp.lte.d.test.prediction[,1] - 
                           temp.lte.d.test.prediction[,2])^2) / nrow(temp.lte.d.test.prediction))




