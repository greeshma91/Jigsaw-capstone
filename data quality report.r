# Data Quality Report

library(dplyr)
tele<-read.csv("/DS Full stack/Graded Assignments/09 - Capstone Project  and Certitication/telecomfinal.csv",header=T)
dim(tele)
names(tele)
str(tele)
summary(tele)

#data quality report
variables<-names(tele)
dqr<-as.data.frame(variables)
rm(variables)

#datatype for each var
dqr$DataType<-sapply(tele,class)
View(dqr)

#no of records for each var
dqr$No.ofRecords<-nrow(tele)

#counting unique values for each var
for(i in 1:ncol(tele))
{
  dqr$UniqueRecords<-length(unique(tele[,i]))
}

#no of observations available for each var and its perc
dqr$Dataavailable<-colSums(!is.na(tele))
dqr$Datapercentage<- round(colMeans(!is.na(tele),4))

#total and perc of missing values for each var
dqr$Missing<- colSums(is.na(tele))
dqr$Missingpercentage<-round(colMeans(is.na(tele),4))

#max,min,mean n quantiles for each var
for(i in 1:ncol(tele))
{
  dqr$Minimum[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",min(tele[,i],na.rm=T),0),2)
  dqr$Maximum[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",max(tele[,i],na.rm=T),0),2)
  dqr$Mean[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",mean(tele[,i],na.rm=T),0),2)
  dqr$Fifthpercentile[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.05,na.rm=T),0),2)
  dqr$tenthpercentile[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.10,na.rm=T),0),2)
  dqr$TwentyFifthpercentile[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.25,na.rm=T),0),2)
  dqr$Fiftypercentile[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.50,na.rm=T),0),2)
  dqr$SeventyFifthpercentile[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.75,na.rm=T),0),2)
  dqr$Ninetypercentile[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.90,na.rm=T),0),2)
  dqr$NinetyFifthpercentile[i]<-round(ifelse(class(tele[,i])=="integer"|class(tele[,i])=="numeric",quantile(tele[,i],p=0.95,na.rm=T),0),2)
  
}
str(dqr)

#exporting data quality rep
write.csv(dqr,"trialdataqualityrep.csv",row.names=T)

# Alternative method to check data quality
library(dataQualityR)
checkDataQuality(data = data,out.file.num = "numeric.csv",out.file.cat = "categorical.csv")

