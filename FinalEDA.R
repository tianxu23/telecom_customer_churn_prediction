setwd('G:/Fall 1/Data Science/Project')
source("DataAnalyticsFunctions.R")

data <- read.csv('tel_churn.csv')

data <- read.csv('tel_churn.csv',header=T,na.strings=c(""))

data <- data[-which(is.na(data$marital)),]
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
data_temp$rv_fixed <- ifelse(is.na(data_temp$rv), 2, data_temp$rv)
data_temp$truck_fixed <- ifelse(is.na(data_temp$truck), 2, data_temp$truck)
data_temp$hnd_webcap[is.na(data_temp$hnd_webcap)] <- "UNKW"
data_temp$forgntvl <- ifelse(is.na(data_temp$forgntvl), 2, data_temp$forgntvl)
data_temp$eqpdays <- ifelse(is.na(data_temp$eqpdays), 0, data_temp$eqpdays)
data_temp$phones <- ifelse(is.na(data_temp$phones), 1, data_temp$phones)
data_temp$models <- ifelse(is.na(data_temp$models), 1, data_temp$models)
data_temp$lor <- NULL
data_temp$numbcars <- NULL
data_temp$rv <- NULL
data_temp$truck <- NULL
summary(data_temp)

unique(data_temp$income)
missing_income <- data_temp[which(is.na(data_temp$income)),]
nmissing_income <- data_temp[-which(is.na(data_temp$income)),]
missing_income$income <- as.integer(runif(25330, 1,10))
data_temp1 <- rbind(nmissing_income, missing_income)

missing_change <- data_temp1[which(is.na(data_temp1$change_mou) & is.na(data_temp1$change_rev)),]
data_use <- data_temp1[-which(is.na(data_temp1$change_mou) & is.na(data_temp1$change_rev)),]

summary(data_use)

summary(data1)

library(ggplot2)
library(corrplot)

ggplot(data=data1, aes(x=na.omit(rev_Mean)))+geom_histogram(binwidth = 10) +
  scale_x_continuous(limits = c(0,425), "rev_Mean", expand = c(0,0))

ggplot(data = data1, aes(x=mou_Mean, y=rev_Mean))+geom_point()

ggplot(data1, aes(x=rev_Mean)) + geom_histogram() + facet_wrap(~churn) + scale_x_continuous(breaks = seq(0, 350, 50), lim = c(0, 350))

cor(data1$churn, data1$rev_Mean)

data1$rev_Mean[data1$rev_Mean > 1000]

data1$rev_Mean[data$rev_Mean == NA]

100000-sum(!is.na(data1$rev_Mean))
#357 null values in rev_Mean column

summary(data)
colnames(data)
personal <- data[, which( colnames(data)%in%c("uniqsubs", "actvsubs", "new_cell", "asl_flag", "area", "ownrent", "lor", "dwlltype", "marital", "adults", "income", "HHstatin", "dwllsize", "forgntvl", "creditcd", "eqpdays"))]

summary(personal)

pers_cor <- cor(personal$uniqsubs, personal$actvsubs, personal$eqpdays)
str(personal)

hist(data$lor, label=TRUE, col="blue")

counts <- table(data$churn, data$marital)
barplot(counts, main="Customer distribution by marital status and churn",
        xlab="Marital status", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

counts1 <- table(data$churn, data$new_cell)
barplot(counts1, main="Customer distribution by new cellphone and churn",
        xlab="New cellphone", ylab = "Frequency", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

counts3 <- table(data$churn, data$creditcd)
barplot(counts3, main="Employee distribution by Credit Card and Churn",
                xlab="Credit Card", ylab = "Frequency", col=c("darkblue","red"),
                 legend = rownames(counts3), beside=TRUE)

ggplot(data, aes(x=actvsubs, y =uniqsubs, colour =churn))+geom_point()+xlim(0,12)+ylim(0,20)

ggplot(data, aes(x=churn, y = lor)) + geom_boxplot()

ggplot(data, aes(x=lor, colour =churn))+geom_dotplot()

data$area

ggplot(data, aes(x=area)+geom_bar(legend.text = NULL)

install.packages('DataExplorer') 
library(DataExplorer)       
plot_correlation(data, type = 'continuous','Review.Date')

#churn separated through foreign travel
ggplot(data, aes(x=churn)) + geom_bar() + facet_wrap(~forgntvl)

#Similar category variables put in one dataframe
OvCharge <- data[, which( colnames(data)%in%c("totmrc_Mean", "rev_Mean",
                                              "ovrrev_Mean",
                                              "vceovr_Mean",
                                              "datovr_Mean",
                                              "totrev",
                                              "avgrev",
                                              "avg3rev",
                                              "avg6rev",
                                              "change_rev"))]

####################################################################################
#Churn plotted aginst months first and then handset price
ggplot(data_use2, aes(x=months, fill=churn))+geom_density(alpha=0.4)

ggplot(data_use2, aes(x=hnd_price, fill = churn))+geom_density(alpha=0.4)

#####################################################################################
data_use2$churn <- as.factor(data_use2$churn)
data_use2$forgntvl <- as.factor(data_use2$forgntvl)

summary(data_use2$forgntvl)

data_use2 <- data_use

str(data_use)

nmissing_income$churn <- as.factor(nmissing_income$churn)

summary(OvCharge)

str(OvCharge)

#Null values removed
OvCharge1 <- OvCharge[-which(is.na(OvCharge)),]
cor(OvCharge1)

#Correlation plot for charge variables
data_use <- data_use[-which(is.na(data_use$change_rev)),]
summary(data_use)
corrplot(OvCharge1)
plot_correlation(OvCharge1, type = 'continuous','Review.Date')

summary(OvCharge1$change_rev)

install.packages("glmnet")
library(glmnet)

install.packages("Amelia")
library(Amelia)
missmap(OvCharge1, main = "Missing values vs observed")

data1 <- data[-which(is.na(data)),]

missmap(data1, main = "Missing values vs observed")

str(data_use)
summary(data_use)

cor(data_use$drop_vce_Mean, data_use$churn)
cor(data_use$rev_Mean, data_use$churn)

data1 <- na.omit(data1)

#Correlation between data over mean and voice over mean
cor(data$datovr_Mean, data$vceovr_Mean)
summary(data$datovr_Mean)

#Voice and data with churn
qplot(x=log(data$vceovr_Mean), y=log(data$datovr_Mean), color = churn, data = data, geom = "point")

ggplot(data_use2, aes(x=drop_blk_Mean, y=log(custcare_Mean), color=churn)) + geom_point()
cor(data_use$drop_vce_Mean, data_use$hnd_price)
