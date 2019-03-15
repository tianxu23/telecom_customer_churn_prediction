setwd("C:/Users/31221/Desktop/data science  for BUSINESS/FinalProject")

data <- read.csv("Telecom_customer churn.csv")
summary(data)

#converting categorical variables to factors
data$new_cell <- as.factor(data$new_cell)
data$crclscod <- as.factor(data$crclscod)
data$asl_flag <- as.factor(data$asl_flag)
data$prizm_social_one <- as.factor(data$prizm_social_one)
data$area <- as.factor(data$area)
data$dualband <- as.factor(data$dualband)
data$refurb_new <- as.factor(data$refurb_new)
data$hnd_webcap <- as.factor(data$hnd_webcap)
data$dwlltype <- as.factor(data$dwlltype)
data$marital <- as.factor(data$marital)
data$infobase <- as.factor(data$infobase)
data$HHstatin <- as.factor(data$HHstatin)
data$dwllsize <- as.factor(data$dwllsize)
data$ethnic <- as.factor(data$ethnic)
data$kid0_2 <- as.factor(data$kid0_2)
data$kid3_5 <- as.factor(data$kid3_5)
data$kid6_10 <- as.factor(data$kid6_10)
data$kid11_15 <- as.factor(data$kid11_15)
data$kid16_17 <- as.factor(data$kid16_17)
data$creditcd <- as.factor(data$creditcd)

missing_means <- data[which(is.na(data$da_Mean)),]
data1 <- data[-which(is.na(data$da_Mean)),]

#making the missing values in truck and rv column to 2, so that we can identify them separately
data_temp <- data1
data_temp <- data_temp[-which(is.na(data_temp$truck)),]
data_temp$hnd_webcap[is.na(data_temp$hnd_webcap)] <- "UNKW"
data_temp$eqpdays <- ifelse(is.na(data_temp$eqpdays), 0, data_temp$eqpdays)
data_temp$phones <- ifelse(is.na(data_temp$phones), 1, data_temp$phones)
data_temp$models <- ifelse(is.na(data_temp$models), 1, data_temp$models)
data_temp$lor <- NULL
data_temp$numbcars <- NULL
summary(data_temp)

data_temp$income <- NULL

missing_change <- data_temp[which(is.na(data_temp$change_mou) & is.na(data_temp$change_rev)),]
data_use <- data_temp[-which(is.na(data_temp$change_mou) & is.na(data_temp$change_rev)),]
install.packages("ragtop")
library(ragtop)

data_use$prizm_social_one <- NULL
data_use <- data_use[-which(is.na(data_use$hnd_price)),]
data_use <- data_use[-which(is.na(data_use$avg6qty)),]
data_use$infobase <- NULL
data_use$Customer_ID <- NULL
data_use$dwlltype <- NULL
data_use$ownrent <- NULL
data_use$HHstatin <- NULL
data_use$dwllsize <- NULL
data_use$adults <- NULL

summary(data_use)


