library(dplyr)
library(dataQualityR)
library(car)
library(caret)
options(scipen = 999)

tele<-read.csv("/DS Full stack/Graded Assignments/09 - Capstone Project  and Certitication/telecomfinal.csv",header=T)
View(tele)
variables<-names(tele)
dqr<-as.data.frame(variables)
rm(variables)
dqr

#recording datatype for each variable
dqr$datatype<-sapply(tele,class)
dqr$no.of.records<-nrow(tele)
#counting no of unique values for each var
for (i in 1:ncol(tele))
     {
      dqr$no.of.uniquevalue<-length(unique(tele[,i]))
      }

for (i in 1:ncol(tele))
{dqr$Minimum<-ifelse(class(tele[,i])=="numeric"|class(tele[,i])=="integer",min(tele[,i],na.rm=T),0)
dqr$Maximum<-ifelse(class(tele[,i])=="numeric"|class(tele[,i])=="integer",max(tele[,i],na.rm=T),0)
dqr$Mean<-ifelse(class(tele[,i])=="numeric"|class(tele[,i])=="integer",mean(tele[,i],na.rm=T),0)
dqr$Fifthpercentile<-ifelse(class(tele[,i])=="numeric"|class(tele[,i])=="integer",quantile(tele[,i],p=0.05,na.rm=T),0)
dqr$Tenthpercentile<-ifelse(class(tele[,i])=="numeric"|class(tele[,i])=="integer",quantile(tele[,i],p=0.10,na.rm=T),0)
dqr$Twentyfifthpercentile<-ifelse(class(tele[,i])=="numeric"|class(tele[,i])=="integer",quantile(tele[,i],p=0.25,na.rm=T),0)
dqr$Seventyfivepercentile<-ifelse(class(tele[,i])=="numeric"|class(tele[,i])=="integer",quantile(tele[,i],p=0.75,na.rm=T),0)
dqr$Ninetyfivepercentile<-ifelse(class(tele[,i])=="numeric"|class(tele[,i])=="integer",quantile(tele[,i],p=0.95,na.rm=T),0)
dqr$Ninetyninepercentile<-ifelse(class(tele[,i])=="numeric"|class(tele[,i])=="integer",quantile(tele[,i],p=0.99,na.rm=T),0)
}  

str(dqr)

write.csv(dqr,file="capstone_data_quality.csv",row.names = F)
checkDataQuality(data = tele,out.file.num = "numeric.csv",out.file.cat = "categorical.csv")


dim(tele)
colnames(tele)

#omitting variables with more than 15% missing values
#var retdays seems to have more than 15% missing but its important variable hence treating it befor handed.
summary(tele$retdays)
sort(unique(tele$retdays),na.last=F)
str(tele$retdays)
summary(tele$retdays)

tele1<-tele[,colMeans(is.na(tele))<=0.15]
View(tele1)
names(tele1)

# drop_blk_Mean=blck_dat_Mean+drop_dat_Mean+drop_vce_Mean,
# hence omitting var drop_blk_Mean 
tele1<-tele1[,-6]

# Data exploration-profiling-continous var as dat & categoriacal var as datc

# deciling continous var on basis of churn :
               `                                                           
#mou_Mean
summary(tele1$mou_Mean)
tele1%>%mutate(dec=ntile(tele1$mou_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat1
dat1$N<-unclass(tele1%>%mutate(dec=ntile(tele1$mou_Mean,10))%>%count(dec)%>%unname())[[2]]
dat1$churnperc<-round(dat1$n/dat1$N,2)
dat1$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$mou_Mean,10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat1$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$mou_Mean,10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat1$varname<-rep("mou_Mean",nrow(dat1))
View(dat1)

#totmrc_Mean
summary(tele1$totmrc_Mean)
tele1%>%mutate(dec=ntile(tele1$totmrc_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat2
dat2$N<-unclass(tele1%>%mutate(dec=ntile(tele1$totmrc_Mean,10))%>%count(dec)%>%unname())[[2]]
dat2$churnperc<-round(dat2$n/dat2$N,2)
dat2$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$totmrc_Mean,10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat2$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$totmrc_Mean,10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat2$varname<-rep("totmrc_Mean",nrow(dat2))
View(dat2)

#rev_Range
summary(tele1$rev_Range)
tele1%>%mutate(dec=ntile(tele1$rev_Range,10))%>%count(churn,dec)%>%filter(churn==1)->dat3
dat3$N<-unclass(tele1%>%mutate(dec=ntile(tele1$rev_Range,10))%>%count(dec)%>%unname())[[2]]
dat3$churnperc<-round(dat3$n/dat3$N,2)
dat3$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$rev_Range,10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat3$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$rev_Range,10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dat3$varname<-rep("rev_Range",nrow(dat3))
View(dat3)

#mou_Range
summary(tele1$mou_Range)
tele1%>%mutate(dec=ntile(tele1$mou_Range,10))%>%count(churn,dec)%>%filter(churn==1)->dat4
dat4$N<-unclass(tele1%>%mutate(dec=ntile(tele1$mou_Range,10))%>%count(dec)%>%unname())[[2]]
dat4$churnperc<-round(dat4$n/dat4$N,2)
dat4$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$mou_Range,10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat4$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$mou_Range,10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dat4$varname<-rep("mou_Range",nrow(dat4))
View(dat4)

#change_mou
summary(tele1$change_mou)
tele1%>%mutate(dec=ntile(tele1$change_mou,10))%>%count(churn,dec)%>%filter(churn==1)->dat5
dat5$N<-unclass(tele1%>%mutate(dec=ntile(tele1$change_mou,10))%>%count(dec)%>%unname())[[2]]
dat5$churnperc<-round(dat5$n/dat5$N,2)
dat5$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$change_mou,10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat5$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$change_mou,10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat5$varname<-rep("change_mou",nrow(dat5))
View(dat5)

#drop_vce_Range
summary(tele1$drop_vce_Range)
tele1%>%mutate(dec=ntile(tele1$drop_vce_Range,10))%>%count(churn,dec)%>%filter(churn==1)->dat6
dat6$N<-unclass(tele1%>%mutate(dec=ntile(tele1$drop_vce_Range,10))%>%count(dec)%>%unname())[[2]]
dat6$churnperc<-round(dat6$n/dat6$N,2)
dat6$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$drop_vce_Range,10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
dat6$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$drop_vce_Range,10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
dat6$varname<-rep("drop_vce_Range",nrow(dat6))
View(dat6)

#owylis_vce_Range
summary(tele1$owylis_vce_Range)
tele1%>%mutate(dec=ntile(tele1$owylis_vce_Range,10))%>%count(churn,dec)%>%filter(churn==1)->dat7
dat7$N<-unclass(tele1%>%mutate(dec=ntile(tele1$owylis_vce_Range,10))%>%count(dec)%>%unname())[[2]]
dat7$churnperc<-round(dat7$n/dat7$N,2)
dat7$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$owylis_vce_Range,10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
dat7$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$owylis_vce_Range,10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
dat7$varname<-rep("owylis_vce_Range",nrow(dat7))
View(dat7)

#mou_opkv_Range
summary(tele1$mou_opkv_Range)
tele1%>%mutate(dec=ntile(tele1$mou_opkv_Range,10))%>%count(churn,dec)%>%filter(churn==1)->dat8
dat8$N<-unclass(tele1%>%mutate(dec=ntile(tele1$mou_opkv_Range,10))%>%count(dec)%>%unname())[[2]]
dat8$churnperc<-round(dat8$n/dat8$N,2)
dat8$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$mou_opkv_Range,10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
dat8$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$mou_opkv_Range,10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
dat8$varname<-rep("owylis_vce_Range",nrow(dat8))
View(dat8)

#months
summary(tele1$months)
tele1%>%mutate(dec=ntile(tele1$months,10))%>%count(churn,dec)%>%filter(churn==1)->dat9
dat9$N<-unclass(tele1%>%mutate(dec=ntile(tele1$months,10))%>%count(dec)%>%unname())[[2]]
dat9$churnperc<-round(dat9$n/dat9$N,2)
dat9$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$months,10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
dat9$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$months,10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
dat9$varname<-rep("months",nrow(dat9))
View(dat9)

#totcalls
summary(tele1$totcalls)
tele1%>%mutate(dec=ntile(tele1$totcalls,10))%>%count(churn,dec)%>%filter(churn==1)->dat10
dat10$N<-unclass(tele1%>%mutate(dec=ntile(tele1$totcalls,10))%>%count(dec)%>%unname())[[2]]
dat10$churnperc<-round(dat10$n/dat10$N,2)
dat10$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$totcalls,10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dat10$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$totcalls,10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dat10$varname<-rep("totcalls",nrow(dat10))
View(dat10)

#eqpdays
summary(tele1$eqpdays)
#only 1 missing observatiom-deleting it
index<-which(is.na(tele1$eqpdays))
tele1<-tele1[-index,]
tele1%>%mutate(dec=ntile(tele1$eqpdays,10))%>%count(churn,dec)%>%filter(churn==1)->dat11
dat11$N<-unclass(tele1%>%mutate(dec=ntile(tele1$eqpdays,10))%>%count(dec)%>%unname())[[2]]
dat11$churnperc<-round(dat11$n/dat11$N,2)
dat11$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$eqpdays,10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
dat11$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$eqpdays,10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
dat11$varname<-rep("eqpdays",nrow(dat11))
View(dat11)

#custcare_Mean
#getting less than 4 deciles so omit
summary(tele1$custcare_Mean)
tele1%>%mutate(dec=ntile(tele1$custcare_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat12
dat12$varname<-rep("custcare_Mean",nrow(dat12))
View(dat12)

#callwait_Mean
summary(tele1$callwait_Mean)
tele1%>%mutate(dec=ntile(tele1$callwait_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat13
dat13$N<-unclass(tele1%>%mutate(dec=ntile(tele1$callwait_Mean,4))%>%count(dec)%>%unname())[[2]]
dat13$churnperc<-round(dat13$n/dat13$N,2)
dat13$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$callwait_Mean,4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
dat13$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$callwait_Mean,4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
dat13$varname<-rep("callwait_Mean",nrow(dat13))
View(dat13)

#iwylis_vce_Mean
summary(tele1$iwylis_vce_Mean)
tele1%>%mutate(dec=ntile(tele1$iwylis_vce_Mean,6))%>%count(churn,dec)%>%filter(churn==1)->dat14
dat14$N<-unclass(tele1%>%mutate(dec=ntile(tele1$iwylis_vce_Mean,6))%>%count(dec)%>%unname())[[2]]
dat14$churnperc<-round(dat14$n/dat14$N,2)
dat14$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$iwylis_vce_Mean,6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
dat14$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$iwylis_vce_Mean,6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
dat14$varname<-rep("iwylis_vce_Mean",nrow(dat14))
View(dat14)

#callwait_Range
#getting less than 4 deciles so omit
summary(tele1$callwait_Range)
tele1%>%mutate(dec=ntile(tele1$callwait_Range,4))%>%count(churn,dec)%>%filter(churn==1)->dat15
dat15$varname<-rep("callwait_Range",nrow(dat15))
View(dat15)

#ccrndmou_Range
#getting less than 4 deciles so omit
summary(tele1$ccrndmou_Range)
tele1%>%mutate(dec=ntile(tele1$ccrndmou_Range,4))%>%count(churn,dec)%>%filter(churn==1)->dat16
dat16$varname<-rep("ccrndmou_Range",nrow(dat16))
View(dat16)

#adjqty
summary(tele1$adjqty)
tele1%>%mutate(dec=ntile(tele1$adjqty,10))%>%count(churn,dec)%>%filter(churn==1)->dat17
dat17$N<-unclass(tele1%>%mutate(dec=ntile(tele1$adjqty,10))%>%count(dec)%>%unname())[[2]]
dat17$churnperc<-round(dat17$n/dat17$N,2)
dat17$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$adjqty,10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dat17$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$adjqty,10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dat17$varname<-rep("adjqty",nrow(dat17))
View(dat17)

#ovrrev_Mean
summary(tele1$ovrrev_Mean)
tele1%>%mutate(dec=ntile(tele1$ovrrev_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat18
dat18$N<-unclass(tele1%>%mutate(dec=ntile(tele1$ovrrev_Mean,4))%>%count(dec)%>%unname())[[2]]
dat18$churnperc<-round(dat18$n/dat18$N,2)
dat18$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$ovrrev_Mean,4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat18$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$ovrrev_Mean,4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat18$varname<-rep("ovrrev_Mean",nrow(dat18))
View(dat18)

#rev_Mean
summary(tele1$rev_Mean)
tele1%>%mutate(dec=ntile(tele1$rev_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat19
dat19$N<-unclass(tele1%>%mutate(dec=ntile(tele1$rev_Mean,10))%>%count(dec)%>%unname())[[2]]
dat19$churnperc<-round(dat19$n/dat19$N,2)
dat19$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$rev_Mean,10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat19$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$rev_Mean,10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat19$varname<-rep("rev_Mean",nrow(dat19))
View(dat19)

#ovrmou_Mean
summary(tele1$ovrmou_Mean)
tele1%>%mutate(dec=ntile(tele1$ovrmou_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat20
dat20$N<-unclass(tele1%>%mutate(dec=ntile(tele1$ovrmou_Mean,4))%>%count(dec)%>%unname())[[2]]
dat20$churnperc<-round(dat20$n/dat20$N,2)
dat20$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$ovrmou_Mean,4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat20$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$ovrmou_Mean,4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
dat20$varname<-rep("ovrmou_Mean",nrow(dat20))
View(dat20)

#comp_vce_Mean
#data transforma nd then delete
summary(tele1$comp_vce_Mean)
tele1%>%mutate(dec=ntile(tele1$comp_vce_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat21
dat21$N<-unclass(tele1%>%mutate(dec=ntile(tele1$comp_vce_Mean,10))%>%count(dec)%>%unname())[[2]]
dat21$churnperc<-round(dat21$n/dat21$N,2)
dat21$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$comp_vce_Mean,10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dat21$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$comp_vce_Mean,10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dat21$varname<-rep("comp_vce_Mean",nrow(dat21))
View(dat21)

#plcd_vce_Mean
#data transforma nd then delete
summary(tele1$plcd_vce_Mean)
tele1%>%mutate(dec=ntile(tele1$plcd_vce_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat22
dat22$N<-unclass(tele1%>%mutate(dec=ntile(tele1$plcd_vce_Mean,10))%>%count(dec)%>%unname())[[2]]
dat22$churnperc<-round(dat22$n/dat22$N,2)
dat22$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$plcd_vce_Mean,10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dat22$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$plcd_vce_Mean,10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dat22$varname<-rep("plcd_vce_Mean",nrow(dat22))
View(dat22)

#avg3mou
summary(tele1$avg3mou)
tele1%>%mutate(dec=ntile(tele1$avg3mou,10))%>%count(churn,dec)%>%filter(churn==1)->dat23
dat23$N<-unclass(tele1%>%mutate(dec=ntile(tele1$avg3mou,10))%>%count(dec)%>%unname())[[2]]
dat23$churnperc<-round(dat23$n/dat23$N,2)
dat23$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$avg3mou,10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
dat23$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$avg3mou,10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
dat23$varname<-rep("avg3mou",nrow(dat23))
View(dat23)

#avgmou
summary(tele1$avgmou)
tele1%>%mutate(dec=ntile(tele1$avgmou,10))%>%count(churn,dec)%>%filter(churn==1)->dat24
dat24$N<-unclass(tele1%>%mutate(dec=ntile(tele1$avgmou,10))%>%count(dec)%>%unname())[[2]]
dat24$churnperc<-round(dat24$n/dat24$N,2)
dat24$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$avgmou,10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dat24$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$avgmou,10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dat24$varname<-rep("avgmou",nrow(dat24))
View(dat24)

#avg3qty
summary(tele1$avg3qty)
tele1%>%mutate(dec=ntile(tele1$avg3qty,10))%>%count(churn,dec)%>%filter(churn==1)->dat25
dat25$N<-unclass(tele1%>%mutate(dec=ntile(tele1$avg3qty,10))%>%count(dec)%>%unname())[[2]]
dat25$churnperc<-round(dat25$n/dat25$N,2)
dat25$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$avg3qty,10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
dat25$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$avg3qty,10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
dat25$varname<-rep("avg3qty",nrow(dat25))
View(dat25)

#avgqty
summary(tele1$avgqty)
tele1%>%mutate(dec=ntile(tele1$avgqty,10))%>%count(churn,dec)%>%filter(churn==1)->dat26
dat26$N<-unclass(tele1%>%mutate(dec=ntile(tele1$avgqty,10))%>%count(dec)%>%unname())[[2]]
dat26$churnperc<-round(dat26$n/dat26$N,2)
dat26$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$avgqty,10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dat26$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$avgqty,10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dat26$varname<-rep("avgqty",nrow(dat26))
View(dat26)

#avg6mou
summary(tele1$avg6mou)
tele1%>%mutate(dec=ntile(tele1$avg6mou,10))%>%count(churn,dec)%>%filter(churn==1)->dat27
dat27$N<-unclass(tele1%>%mutate(dec=ntile(tele1$avg6mou,10))%>%count(dec)%>%unname())[[2]]
dat27$churnperc<-round(dat27$n/dat27$N,2)
dat27$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$avg6mou,10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat27$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$avg6mou,10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat27$varname<-rep("avg6mou",nrow(dat27))
View(dat27)

#avg6qty
summary(tele1$avg6qty)
tele1%>%mutate(dec=ntile(tele1$avg6qty,10))%>%count(churn,dec)%>%filter(churn==1)->dat28
dat28$N<-unclass(tele1%>%mutate(dec=ntile(tele1$avg6qty,10))%>%count(dec)%>%unname())[[2]]
dat28$churnperc<-round(dat28$n/dat28$N,2)
dat28$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$avg6qty,10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat28$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$avg6qty,10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat28$varname<-rep("avg6qty",nrow(dat28))
View(dat28)

#age1
#use as factor
summary(tele1$age1)
tele1%>%mutate(dec=ntile(tele1$age1,6))%>%count(churn,dec)%>%filter(churn==1)->dat29
dat29$N<-unclass(tele1%>%mutate(dec=ntile(tele1$age1,6))%>%count(dec)%>%unname())[[2]]
dat29$churnperc<-round(dat29$n/dat29$N,2)
dat29$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$age1,6))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
dat29$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$age1,6))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
dat29$varname<-rep("age1",nrow(dat29))
View(dat29)

#age2
#getting less than 4 deciles, use as factor var
summary(tele1$age2)
tele1%>%mutate(dec=ntile(tele1$age2,4))%>%count(churn,dec)%>%filter(churn==1)->dat30
dat30$varname<-rep("age2",nrow(dat30))
View(dat30)

#models
#getting less than 4 deciles, use as factor var
summary(tele1$models)
tele1%>%mutate(dec=ntile(tele1$models,4))%>%count(churn,dec)%>%filter(churn==1)->dat31
dat31$varname<-rep("models",nrow(dat31))
View(dat31)

#hnd_price
#use as factor
summary(tele1$hnd_price)
tele1%>%mutate(dec=ntile(tele1$hnd_price,10))%>%count(churn,dec)%>%filter(churn==1)->dat32
dat32$N<-unclass(tele1%>%mutate(dec=ntile(tele1$hnd_price,10))%>%count(dec)%>%unname())[[2]]
dat32$churnperc<-round(dat32$n/dat32$N,2)
dat32$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$hnd_price,10))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
dat32$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$hnd_price,10))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
dat32$varname<-rep("hnd_price",nrow(dat32))
View(dat32)

#actvsubs
#use as factor
summary(tele1$actvsubs)
tele1%>%mutate(dec=ntile(tele1$actvsubs,4))%>%count(churn,dec)%>%filter(churn==1)->dat33
dat33$varname<-rep("actvsubs",nrow(dat33))
View(dat33)

#uniqsubs
#use as factor
summary(tele1$uniqsubs)
tele1%>%mutate(dec=ntile(tele1$uniqsubs,4))%>%count(churn,dec)%>%filter(churn==1)->dat34
dat34$varname<-rep("uniqsubs",nrow(dat34))
View(dat34)

#forgntvl
#use as factor
summary(tele1$forgntvl)
tele1%>%mutate(dec=ntile(tele1$forgntvl,4))%>%count(churn,dec)%>%filter(churn==1)->dat35
dat35$varname<-rep("forgntvl",nrow(dat35))
View(dat35)

#opk_dat_Mean
#omit
summary(tele1$opk_dat_Mean)
tele1%>%mutate(dec=ntile(tele1$opk_dat_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat36
dat36$varname<-rep("opk_dat_Mean",nrow(dat36))
View(dat36)

#mtrcycle
#factor variable
summary(tele1$mtrcycle)
tele1%>%mutate(dec=ntile(tele1$mtrcycle,4))%>%count(churn,dec)%>%filter(churn==1)->dat37
dat37$varname<-rep("mtrcycle",nrow(dat37))
View(dat37)

#truck
#factor variable
summary(tele1$truck)
tele1%>%mutate(dec=ntile(tele1$truck,4))%>%count(churn,dec)%>%filter(churn==1)->dat38
dat38$varname<-rep("truck",nrow(dat38))
View(dat38)

#roam_Mean
#getting less than 4 deciles-omit
summary(tele1$roam_Mean)
tele1%>%mutate(dec=ntile(tele1$roam_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat39
dat39$varname<-rep("roam_Mean",nrow(dat39))
View(dat39)

#recv_sms_Mean
#getting less than 4 deciles-omit
summary(tele1$recv_sms_Mean)
tele1%>%mutate(dec=ntile(tele1$recv_sms_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat40
dat40$varname<-rep("recv_sms_Mean",nrow(dat40))
View(dat40)

#mou_pead_Mean
#getting less than 4 deciles-omit
summary(tele1$mou_pead_Mean)
tele1%>%mutate(dec=ntile(tele1$mou_pead_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat41
dat41$varname<-rep("mou_pead_Mean",nrow(dat41))
View(dat41)

#da_Mean
summary(tele1$da_Mean)
tele1%>%mutate(dec=ntile(tele1$da_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat42
dat42$N<-unclass(tele1%>%mutate(dec=ntile(tele1$da_Mean,4))%>%count(dec)%>%unname())[[2]]
dat42$churnperc<-round(dat42$n/dat42$N,2)
dat42$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$da_Mean,4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat42$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$da_Mean,4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
dat42$varname<-rep("da_Mean",nrow(dat42))
View(dat42)

#da_Range
summary(tele1$da_Range)
tele1%>%mutate(dec=ntile(tele1$da_Range,4))%>%count(churn,dec)%>%filter(churn==1)->dat43
dat43$N<-unclass(tele1%>%mutate(dec=ntile(tele1$da_Mean,4))%>%count(dec)%>%unname())[[2]]
dat43$churnperc<-round(dat43$n/dat43$N,2)
dat43$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$da_Range,4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat43$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$da_Range,4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
dat43$varname<-rep("da_Range",nrow(dat43))
View(dat43)

#datovr_Mean
#getting less than 4 deciles-omit
summary(tele1$datovr_Mean)
tele1%>%mutate(dec=ntile(tele1$datovr_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat44
dat44$varname<-rep("datovr_Mean",nrow(dat44))
View(dat44)

#datovr_Range
#getting less than 4 deciles-omit
summary(tele1$datovr_Range)
tele1%>%mutate(dec=ntile(tele1$datovr_Range,4))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$varname<-rep("datovr_Range",nrow(dat45))
View(dat45)

#drop_dat_Mean
#getting less than 4 deciles-omit
#more than 95%values are 0 so delete
summary(tele1$drop_dat_Mean)
tele1%>%mutate(dec=ntile(tele1$drop_dat_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat46
dat46$varname<-rep("drop_dat_Mean",nrow(dat46))
View(dat46)

#drop_vce_Mean
summary(tele1$drop_vce_Mean)
tele1%>%mutate(dec=ntile(tele1$drop_vce_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat47
dat47$N<-unclass(tele1%>%mutate(dec=ntile(tele1$drop_vce_Mean,10))%>%count(dec)%>%unname())[[2]]
dat47$churnperc<-round(dat47$n/dat47$N,2)
dat47$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$drop_vce_Mean,10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
dat47$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$drop_vce_Mean,10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
dat47$varname<-rep("drop_vce_Mean",nrow(dat47))
View(dat47)

#adjmou
summary(tele1$adjmou)
tele1%>%mutate(dec=ntile(tele1$adjmou,10))%>%count(churn,dec)%>%filter(churn==1)->dat48
dat48$N<-unclass(tele1%>%mutate(dec=ntile(tele1$adjmou,10))%>%count(dec)%>%unname())[[2]]
dat48$churnperc<-round(dat48$n/dat48$N,2)
dat48$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$adjmou,10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dat48$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$adjmou,10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dat48$varname<-rep("adjmou",nrow(dat48))
View(dat48)

#totrev
summary(tele1$totrev)
tele1%>%mutate(dec=ntile(tele1$totrev,10))%>%count(churn,dec)%>%filter(churn==1)->dat49
dat49$N<-unclass(tele1%>%mutate(dec=ntile(tele1$totrev,10))%>%count(dec)%>%unname())[[2]]
dat49$churnperc<-round(dat49$n/dat49$N,2)
dat49$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$totrev,10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat49$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$totrev,10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat49$varname<-rep("totrev",nrow(dat49))
View(dat49)

#adjrev
summary(tele1$adjrev)
tele1%>%mutate(dec=ntile(tele1$adjrev,10))%>%count(churn,dec)%>%filter(churn==1)->dat50
dat50$N<-unclass(tele1%>%mutate(dec=ntile(tele1$adjrev,10))%>%count(dec)%>%unname())[[2]]
dat50$churnperc<-round(dat50$n/dat50$N,2)
dat50$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$adjrev,10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dat50$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$adjrev,10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dat50$varname<-rep("adjrev",nrow(dat50))
View(dat50)

#avgrev
summary(tele1$avgrev)
tele1%>%mutate(dec=ntile(tele1$avgrev,10))%>%count(churn,dec)%>%filter(churn==1)->dat51
dat51$N<-unclass(tele1%>%mutate(dec=ntile(tele1$adjrev,10))%>%count(dec)%>%unname())[[2]]
dat51$churnperc<-round(dat51$n/dat51$N,2)
dat51$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$avgrev,10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat51$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$avgrev,10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat51$varname<-rep("avgrev",nrow(dat51))
View(dat51)

#comp_dat_Mean
#data transform and omit
summary(tele1$comp_dat_Mean)
tele1%>%mutate(dec=ntile(tele1$comp_dat_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat52
dat52$varname<-rep("comp_dat_Mean",nrow(dat52))
View(dat52)

#plcd_dat_Mean
#data transform and omit
summary(tele1$plcd_dat_Mean)
tele1%>%mutate(dec=ntile(tele1$plcd_dat_Mean,4))%>%count(churn,dec)%>%filter(churn==1)->dat53
dat53$varname<-rep("plcd_dat_Mean",nrow(dat53))
View(dat53)

#new var and data transform
tele1$plcd_attempt_Mean<-tele1$plcd_vce_Mean+tele1$plcd_dat_Mean
#plcd_attempt_Mean
summary(tele1$plcd_attempt_Mean)
tele1%>%mutate(dec=ntile(tele1$plcd_attempt_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat54
dat54$N<-unclass(tele1%>%mutate(dec=ntile(tele1$plcd_attempt_Mean,10))%>%count(dec)%>%unname())[[2]]
dat54$churnperc<-round(dat54$n/dat54$N,2)
dat54$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$plcd_attempt_Mean,10))%>%group_by(dec)%>%summarise(min(plcd_attempt_Mean)))[[2]]
dat54$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$plcd_attempt_Mean,10))%>%group_by(dec)%>%summarise(max(plcd_attempt_Mean)))[[2]]
dat54$varname<-rep("plcd_attempt_Mean",nrow(dat54))
View(dat54)

#new var and data transform
tele1$complete_Mean<-(tele1$comp_vce_Mean)+(tele1$comp_dat_Mean)
#complete_Mean
summary(tele1$complete_Mean)
tele1%>%mutate(dec=ntile(tele1$complete_Mean,10))%>%count(churn,dec)%>%filter(churn==1)->dat55
dat55$N<-unclass(tele1%>%mutate(dec=ntile(tele1$complete_Mean,10))%>%count(dec)%>%unname())[[2]]
dat55$churnperc<-round(dat55$n/dat55$N,2)
dat55$Greaterthan<-unclass(tele1%>%mutate(dec=ntile(tele1$complete_Mean,10))%>%group_by(dec)%>%summarise(min(complete_Mean)))[[2]]
dat55$Lesserthan<-unclass(tele1%>%mutate(dec=ntile(tele1$complete_Mean,10))%>%group_by(dec)%>%summarise(max(complete_Mean)))[[2]]
dat55$varname<-rep("complete_Mean",nrow(dat55))
View(dat55)

names(tele1)
tele1<-tele1[,-c(12,15,16,44,47,48,50,56,57,58,21,22,65,66)]
names(tele1)


#categorical variables
#event rate for each var for each level

#crclscod
summary(tele1$crclscod)
tele1%>%count(churn,levels=crclscod)%>%filter(churn==1)->datc24
datc24$N<-unclass(tele1%>%filter(crclscod%in%datc24$levels)%>%count(crclscod))[[2]]
datc24$churnperc<-round(datc24$n/datc24$N,2)
mean(datc24$churnperc)
datc24$varname<-rep("crclscod",nrow(datc24))
View(datc24)

#asl_flag
summary(tele1$asl_flag)
tele1%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datc25
datc25$N<-unclass(tele1%>%filter(asl_flag%in%datc25$levels)%>%count(asl_flag))[[2]]
datc25$churnperc<-round(datc25$n/datc25$N,2)
mean(datc25$churnperc)
datc25$varname<-rep("asl_flag",nrow(datc25))
View(datc25)

#prizm_social_one
summary(tele1$prizm_social_one)
tele1%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datc26
datc26$N<-unclass(tele1%>%filter(prizm_social_one%in%datc26$levels)%>%count(prizm_social_one))[[2]]
datc26$churnperc<-round(datc26$n/datc26$N,2)
mean(datc26$churnperc)
datc26$varname<-rep("prizm_social_one",nrow(datc26))
View(datc26)

#area
summary(tele1$area)
tele1%>%count(churn,levels=area)%>%filter(churn==1)->datc27
datc27$N<-unclass(tele1%>%filter(area%in%datc27$levels)%>%count(area))[[2]]
datc27$churnperc<-round(datc27$n/datc27$N,2)
mean(datc27$churnperc)
datc27$varname<-rep("area",nrow(datc27))
View(datc27)

#refurb_new
summary(tele1$refurb_new)
tele1%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datc28
datc28$N<-unclass(tele1%>%filter(refurb_new%in%datc28$levels)%>%count(refurb_new))[[2]]
datc28$churnperc<-round(datc28$n/datc28$N,2)
mean(datc28$churnperc)
datc28$varname<-rep("refurb_new",nrow(datc28))
View(datc28)

#hnd_webcap
summary(tele1$hnd_webcap)
tele1%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datc29
datc29$N<-unclass(tele1%>%filter(hnd_webcap%in%datc29$levels)%>%count(hnd_webcap))[[2]]
datc29$churnperc<-round(datc29$n/datc29$N,2)
mean(datc29$churnperc)
datc29$varname<-rep("hnd_webcap",nrow(datc29))
View(datc29)

#marital
summary(tele1$marital)
tele1%>%count(churn,levels=marital)%>%filter(churn==1)->datc30
datc30$N<-unclass(tele1%>%filter(marital%in%datc30$levels)%>%count(marital))[[2]]
datc30$churnperc<-round(datc30$n/datc30$N,2)
mean(datc30$churnperc)
datc30$varname<-rep("marital",nrow(datc30))
View(datc30)

#ethnic
summary(tele1$ethnic)
tele1%>%count(churn,levels=ethnic)%>%filter(churn==1)->datc31
datc31$N<-unclass(tele1%>%filter(ethnic%in%datc31$levels)%>%count(ethnic))[[2]]
datc31$churnperc<-round(datc31$n/datc31$N,2)
mean(datc31$churnperc)
datc31$varname<-rep("ethnic",nrow(datc31))
View(datc31)

#car_buy
summary(tele1$car_buy)
tele1%>%count(churn,levels=car_buy)%>%filter(churn==1)->datc32
datc32$N<-unclass(tele1%>%filter(car_buy%in%datc32$levels)%>%count(car_buy))[[2]]
datc32$churnperc<-round(datc32$n/datc32$N,2)
mean(datc32$churnperc)
datc32$varname<-rep("car_buy",nrow(datc32))
View(datc32)

#csa
summary(tele1$csa)
tele1%>%count(churn,levels=csa)%>%filter(churn==1)->datc33
datc33$N<-unclass(tele1%>%filter(csa%in%datc33$levels)%>%count(csa))[[2]]
datc33$churnperc<-round(datc33$n/datc33$N,2)
mean(datc33$churnperc)
datc33$varname<-rep("csa",nrow(datc33))
View(datc33)


#use var as factors: age2,models,actvsubs,uniqsubs,forgntvl,mtrcycle,truck,hnd_price
#age2
summary(tele1$age2)
tele1%>%count(churn,levels=age2)%>%filter(churn==1)->datc34
datc34$N<-unclass(tele1%>%filter(age2%in%datc34$levels)%>%count(age2))[[2]]
datc34$churnperc<-round(datc34$n/datc34$N,2)
mean(datc34$churnperc)
datc34$varname<-rep("age2",nrow(datc34))
View(datc34)

#models
summary(tele1$models)
tele1%>%count(churn,levels=models)%>%filter(churn==1)->datc35
datc35$N<-unclass(tele1%>%filter(models%in%datc35$levels)%>%count(models))[[2]]
datc35$churnperc<-round(datc35$n/datc35$N,2)
mean(datc35$churnperc)
datc35$varname<-rep("models",nrow(datc35))
View(datc35)

#actvsubs
summary(tele1$actvsubs)
tele1%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datc36
datc36$N<-unclass(tele1%>%filter(actvsubs%in%datc36$levels)%>%count(actvsubs))[[2]]
datc36$churnperc<-round(datc36$n/datc36$N,2)
mean(datc36$churnperc)
datc36$varname<-rep("actvsubs",nrow(datc36))
View(datc36)

#uniqsubs
summary(tele1$uniqsubs)
tele1%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datc37
datc37$N<-unclass(tele1%>%filter(uniqsubs%in%datc37$levels)%>%count(uniqsubs))[[2]]
datc37$churnperc<-round(datc37$n/datc37$N,2)
mean(datc37$churnperc)
datc37$varname<-rep("uniqsubs",nrow(datc37))
View(datc37)

#forgntvl
summary(tele1$forgntvl)
tele1%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datc38
datc38$N<-unclass(tele1%>%filter(forgntvl%in%datc38$levels)%>%count(forgntvl))[[2]]
datc38$churnperc<-round(datc38$n/datc38$N,2)
mean(datc38$churnperc)
datc38$varname<-rep("forgntvl",nrow(datc38))
View(datc38)

#mtrcycle
summary(tele1$mtrcycle)
tele1%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datc39
datc39$N<-unclass(tele1%>%filter(mtrcycle%in%datc39$levels)%>%count(mtrcycle))[[2]]
datc39$churnperc<-round(datc39$n/datc39$N,2)
mean(datc39$churnperc)
datc39$varname<-rep("mtrcycle",nrow(datc39))
View(datc39)

#truck
summary(tele1$truck)
tele1%>%count(churn,levels=truck)%>%filter(churn==1)->datc40
datc40$N<-unclass(tele1%>%filter(truck%in%datc40$levels)%>%count(truck))[[2]]
datc40$churnperc<-round(datc40$n/datc40$N,2)
mean(datc40$churnperc)
datc40$varname<-rep("truck",nrow(datc40))
View(datc40)

#hnd_price
summary(tele1$hnd_price)
tele1%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datc41
datc41$N<-unclass(tele1%>%filter(hnd_price%in%datc41$levels)%>%count(hnd_price))[[2]]
datc41$churnperc<-round(datc41$n/datc41$N,2)
mean(datc41$churnperc)
datc41$varname<-rep("hnd_price",nrow(datc41))
View(datc41)

names(tele1)

####data-preparation###
##outlier treatment##
##continuous variables##
#boxplot method#

list<-names(tele1)
#excluding categorical variables
list<-list[-c(24:40,43,44)]
list

#plots
par(mfrow=c(4,10))
for (i in 1:length(list))
{
  boxplot(tele1[,list[i]],main=list[i])
}

for (i in 1:length(list))
{
  plot(tele1[,list[i]],main=list[i])
}

#outlier treatment

for (i in 1:length(list))
{
  x<-boxplot(tel1[,list[i]],main=list[i])
  out<-x$out
  index<-which(tele1[,list[i]])%in%out
  tele1[index.list[i]]<-mean(tele1[,list[i]],narm=T)
  rm(x)
  rm(out)
}

#missing value treatment
#deleting and imputation
names(tele1)
index1<-which(is.na(tele1[,c(1:4)]))
tele1<-tele1[-index1,]
summary(tele1)


index2<-which(is.na(tele1$change_mou))
tele1<-tele1[-index2,]

index3<-which(is.na(tele1$area))
tele1<-tele1[-index3,]

index4<-which(is.na(tele1$marital))
tele1<-tele1[-index4,]

#Mean imputation
tele1$avg6mou[is.na(tele1$avg6mou)]<-mean(tele1$avg6mou,na.rm=T)

tele1$avg6qty[is.na(tele1$avg6qty)]<-mean(tele1$avg6qty,na.rm=T)

tele1$hnd_price[is.na(tele1$hnd_price)]<-mean(tele1$hnd_price,na.rm=T)


#create seperate category "missing" for factor variables

#prizm_social_one
tele1$prizm_social_one_1<-ifelse(is.na(tele1$prizm_social_one),"Missing",as.factor(tele1$prizm_social_one))
str(tele1$prizm_social_one_1)
tele1$prizm_social_one_1<-as.factor(tele1$prizm_social_one_1)
summary(tele1$prizm_social_one_1)
tele1$prizm_social_one_1<-factor(tele1$prizm_social_one_1,labels = c("C","R","S","T","U","Missing"))
summary(tele1$prizm_social_one_1)
names(tele1)
tele1<-tele1[,-26]
names(tele1)

#hnd_webcap
tele1$hnd_webcap_1<-ifelse(is.na(tele1$hnd_webcap),"Missing",as.factor(tele1$hnd_webcap))
str(tele1$hnd_webcap_1)
tele1$hnd_webcap_1<-as.factor(tele1$hnd_webcap_1)
summary(tele1$hnd_webcap_1)
tele1$hnd_webcap_1<-factor(tele1$hnd_webcap_1,labels = c("UNKW","WC","WCMB","Missing"))
summary(tele1$hnd_webcap_1)
names(tele1)
tele1<-tele1[,-28]
names(tele1)

#CHECKING CHURN RATES AFTER IMPUATTIONS
table(tele$churn)/nrow(tele)
table(tele1$churn)/nrow(tele1)

#convert to factors and create dummy var-age1,age2,models,hnd_price,actvsubs,uniqsubs,
#forgntvl,mtrcycle,truck,churn,Customer_ID
#age1
str(tele1$age1)
summary(tele1$age1)
tele1$age1_1<-ifelse(tele1$age1==0,"Default",ifelse(tele1$age1<=30,"Young",
                                                    ifelse(tele1$age1>30 & tele1$age1<=55,"Middle-age","Old")))
str(tele1$age1_1)
tele1$age1_1<-as.factor(tele1$age1_1)
summary(tele1$age1_1)
names(tele1)
tele1<-tele1[,-30]

#age2
str(tele1$age2)
summary(tele1$age2)
tele1$age2_1<-ifelse(tele1$age2==0,"Default",ifelse(tele1$age2<=30,"Young",
                                                    ifelse(tele1$age2>30 & tele1$age2<=55,"Middle-age","Old")))
str(tele1$age2_1)
tele1$age2_1<-as.factor(tele1$age2_1)
summary(tele1$age2_1)
names(tele1)
tele1<-tele1[,-30]

#models
str(tele1$models)
summary(tele1$models)
tele1$models<-as.factor(tele1$models)
summary(tele1$models)

#hnd_price
str(tele1$hnd_price)
summary(tele1$hnd_price)
tele1$hnd_price<-as.factor(tele1$hnd_price)
summary(tele1$hnd_price)

#actvsubs
str(tele1$actvsubs)
summary(tele1$actvsubs)
tele1$actvsubs<-as.factor(tele1$actvsubs)
summary(tele1$actvsubs)

#uniqsubs
str(tele1$uniqsubs)
summary(tele1$uniqsubs)
tele1$uniqsubs<-as.factor(tele1$uniqsubs)
summary(tele1$uniqsubs)

#forgntvl
str(tele1$forgntvl)
summary(tele1$forgntvl)
tele1$forgntvl<-as.factor(tele1$forgntvl)
summary(tele1$forgntvl)

#mtrcycle
str(tele1$mtrcycle)
summary(tele1$mtrcycle)
tele1$mtrcycle<-as.factor(tele1$mtrcycle)
summary(tele1$mtrcycle)

#truck
str(tele1$truck)
summary(tele1$truck)
tele1$truck<-as.factor(tele1$truck)
summary(tele1$truck)


#####logistic regression model building#########
##splitting into train and test samples
set.seed(500)
index<-sample(nrow(tele1),0.70*nrow(tele1),replace=F)
train<-tele1[index,]
test<-tele1[-index,]
#checking churn rate
table(train$churn)/nrow(train)
table(test$churn)/nrow(test)
names(tele1)

#exclude customer_ID and build model
mod<-glm(churn~.,data=train[,-48],family=binomial)
summary(mod)

#creating dummy variables on basis on occurence or no
#asl_flag
summary(tele1$asl_flag)
train$asl_flag_Y<-ifelse(train$asl_flag=="Y",1,0)
test$asl_flag_Y<-ifelse(test$asl_flag=="Y",1,0)

#area
summary(train$area)
train$area_cal_nrth<-ifelse(train$area=="CALIFORNIA NORTH AREA",1,0)
test$area_cal_nrth<-ifelse(test$area=="CALIFORNIA NORTH AREA",1,0)

train$area_texas<-ifelse(train$area=="CENTRAL/SOUTH TEXAS AREA",1,0)
test$area_texas<-ifelse(test$area=="CENTRAL/SOUTH TEXAS AREA",1,0)

train$area_nrthflorida<-ifelse(train$area=="NORTH FLORIDA AREA",1,0)
test$area_nrthflorida<-ifelse(test$area=="NORTH FLORIDA AREA",1,0)

train$area_nrthwst<-ifelse(train$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
test$area_nrthwst<-ifelse(test$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)

train$area_southflorida<-ifelse(train$area=="SOUTH FLORIDA AREA",1,0)
test$area_southflorida<-ifelse(test$area=="SOUTH FLORIDA AREA",1,0)

train$area_southwst<-ifelse(train$area=="SOUTHWEST AREA",1,0)
test$area_southwst<-ifelse(test$area=="SOUTHWEST AREA",1,0)

train$area_tenesse<-ifelse(train$area=="TENNESSEE AREA",1,0)
test$area_tenesse<-ifelse(test$area=="TENNESSEE AREA",1,0)

summary(train$refurb_new)
train$refurb_R<-ifelse(train$refurb_new=="R",1,0)
test$refurb_R<-ifelse(test$refurb_new=="R",1,0)

summary(train$ethnic)
train$ethnic_C<-ifelse(train$ethnic=="C",1,0)
test$ethnic_C<-ifelse(test$ethnic=="C",1,0)

summary(train$ethnic)
train$ethnic_N<-ifelse(train$ethnic=="N",1,0)
test$ethnic_N<-ifelse(test$ethnic=="N",1,0)

summary(train$ethnic)
train$ethnic_O<-ifelse(train$ethnic=="O",1,0)
test$ethnic_O<-ifelse(test$ethnic=="O",1,0)

summary(train$ethnic)
train$ethnic_S<-ifelse(train$ethnic=="S",1,0)
test$ethnic_S<-ifelse(test$ethnic=="S",1,0)

summary(train$ethnic)
train$ethnic_U<-ifelse(train$ethnic=="U",1,0)
test$ethnic_U<-ifelse(test$ethnic=="U",1,0)

summary(train$ethnic)
train$ethnic_Z<-ifelse(train$ethnic=="Z",1,0)
test$ethnic_Z<-ifelse(test$ethnic=="Z",1,0)

summary(train$hnd_price)
train$hnd_price_79.98<-ifelse(train$hnd_price=="79.9899",1,0)
test$hnd_price_79.98<-ifelse(test$hnd_price=="79.9899",1,0)

train$hnd_price_105.08<-ifelse(train$hnd_price=="105.0830",1,0)
test$hnd_price_105.08<-ifelse(test$hnd_price=="105.0830",1,0)

train$hnd_price_129.98<-ifelse(train$hnd_price=="129.9899",1,0)
test$hnd_price_129.98<-ifelse(test$hnd_price=="129.9899",1,0)

train$hnd_price_149.98<-ifelse(train$hnd_price=="149.9899",1,0)
test$hnd_price_149.98<-ifelse(test$hnd_price=="149.9899",1,0)

train$hnd_price_199.98<-ifelse(train$hnd_price=="199.9899",1,0)
test$hnd_price_199.98<-ifelse(test$hnd_price=="199.9899",1,0)

train$hnd_price_249.98<-ifelse(train$hnd_price=="249.9899",1,0)
test$hnd_price_249.98<-ifelse(test$hnd_price=="249.9899",1,0)

summary(train$uniqsubs)
train$uniqsubs_2<-ifelse(train$uniqsubs=="2",1,0)
test$uniqsubs_2<-ifelse(test$uniqsubs=="2",1,0)

train$uniqsubs_3<-ifelse(train$uniqsubs=="3",1,0)
test$uniqsubs_3<-ifelse(test$uniqsubs=="3",1,0)

train$uniqsubs_4<-ifelse(train$uniqsubs=="4",1,0)
test$uniqsubs_4<-ifelse(test$uniqsubs=="4",1,0)

train$uniqsubs_5<-ifelse(train$uniqsubs=="5",1,0)
test$uniqsubs_5<-ifelse(test$uniqsubs=="5",1,0)

train$uniqsubs_6<-ifelse(train$uniqsubs=="6",1,0)
test$uniqsubs_6<-ifelse(test$uniqsubs=="6",1,0)

train$uniqsubs_7<-ifelse(train$uniqsubs=="7",1,0)
test$uniqsubs_7<-ifelse(test$uniqsubs=="7",1,0)

train$uniqsubs_8<-ifelse(train$uniqsubs=="8",1,0)
test$uniqsubs_8<-ifelse(test$uniqsubs=="8",1,0)

train$uniqsubs_9<-ifelse(train$uniqsubs=="9",1,0)
test$uniqsubs_9<-ifelse(test$uniqsubs=="9",1,0)

summary(train$truck)
summary(train$prizm_social_one_1)
train$prizm_social_one_1_C<-ifelse(train$prizm_social_one_1=="C",1,0)
test$prizm_social_one_1_C<-ifelse(test$prizm_social_one_1=="C",1,0)

train$prizm_social_one_1_R<-ifelse(train$prizm_social_one_1=="R",1,0)
test$prizm_social_one_1_R<-ifelse(test$prizm_social_one_1=="R",1,0)

train$prizm_social_one_1_S<-ifelse(train$prizm_social_one_1=="S",1,0)
test$prizm_social_one_1_S<-ifelse(test$prizm_social_one_1=="S",1,0)

train$prizm_social_one_1_T<-ifelse(train$prizm_social_one_1=="T",1,0)
test$prizm_social_one_1_T<-ifelse(test$prizm_social_one_1=="T",1,0)

train$prizm_social_one_1_U<-ifelse(train$prizm_social_one_1=="U",1,0)
test$prizm_social_one_1_U<-ifelse(test$prizm_social_one_1=="U",1,0)

summary(train$age1_1)
train$age1_1_young<-ifelse(train$age1_1=="Young",1,0)
test$age1_1_young<-ifelse(test$age1_1=="Young",1,0)

train$age1_1_midleage<-ifelse(train$age1_1=="Middle-age",1,0)
test$age1_1_midleage<-ifelse(test$age1_1=="Middle-age",1,0)

train$age1_1_old<-ifelse(train$age1_1=="Old",1,0)
test$age1_1_old<-ifelse(test$age1_1=="Old",1,0)

summary(tele1$age2_1)

train$age2_1_young<-ifelse(train$age2_1=="Young",1,0)
test$age2_1_young<-ifelse(test$age2_1=="Young",1,0)

train$age2_1_midleage<-ifelse(train$age2_1=="Middle-age",1,0)
test$age2_1_midleage<-ifelse(test$age2_1=="Middle-age",1,0)

train$age2_1_old<-ifelse(train$age2_1=="Old",1,0)
test$age2_1_old<-ifelse(test$age2_1=="Old",1,0)

# rerun model with sig var
names(train)
mod1<-glm(churn~ mou_Mean + totmrc_Mean +  rev_Range + mou_Range            
         + change_mou + drop_vce_Range + owylis_vce_Range + mou_opkv_Range      
         + months + eqpdays + iwylis_vce_Mean + adjqty + ovrrev_Mean + rev_Mean            
         + ovrmou_Mean + avg3mou + avgmou + avg3qty             
         + avgqty + avg6mou + avg6qty + forgntvl + mtrcycle + truck               
         + totrev + complete_Mean + prizm_social_one_1 + hnd_webcap_1        
         + age1_1 + age2_1 + asl_flag_Y              
         + area_cal_nrth + area_texas + area_nrthflorida + area_nrthwst        
         + area_southflorida + area_southwst + area_tenesse + refurb_R
         + ethnic_C + ethnic_N + ethnic_O + ethnic_S            
         + ethnic_U + ethnic_Z + hnd_price_79.98 + hnd_price_105.08    
         + hnd_price_129.98 + hnd_price_149.98 + hnd_price_199.98 + hnd_price_249.98    
         + uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_5          
         + uniqsubs_6 + uniqsubs_7 + uniqsubs_8 + uniqsubs_9          
         + prizm_social_one_1_C + prizm_social_one_1_R + prizm_social_one_1_S + prizm_social_one_1_T
         + prizm_social_one_1_U + age1_1_young + age1_1_midleage + age1_1_old          
         + age2_1_young + age2_1_midleage + age2_1_old,data=train,family=binomial)

summary(mod1)

#rerunning
mod2<-glm(churn~ totmrc_Mean +  rev_Range + mou_Range            
          + change_mou + drop_vce_Range + owylis_vce_Range    
          + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean + rev_Mean            
          + avg3mou + avgmou + complete_Mean       
          + area_nrthwst + area_southflorida + area_tenesse + refurb_R
          + ethnic_C + ethnic_N + ethnic_O           
          + ethnic_Z + uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_5          
          + uniqsubs_7 + prizm_social_one_1_R + prizm_social_one_1_T
          + age1_1_midleage + age1_1_old + age2_1_young + age2_1_old,data=train,family=binomial)
summary(mod2)
#all variables are significant

#model diagnostics

#1 multicollinearity
vif(mod2)
#removing variables with vif>5
#rev_Range,rev_Mean,avg3mou,avgmou,remove these variables and re running the model
mod3<-glm(churn~ totmrc_Mean + mou_Range            
          + change_mou + drop_vce_Range + owylis_vce_Range    
          + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean + complete_Mean       
          + area_nrthwst + area_southflorida + area_tenesse + refurb_R
          + ethnic_C + ethnic_N + ethnic_O           
          + ethnic_Z + uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_5          
          + uniqsubs_7 + prizm_social_one_1_R + prizm_social_one_1_T
          + age1_1_midleage + age1_1_old + age2_1_young + age2_1_old,data=train,family=binomial)
summary(mod3)

#check confidence interval
confint(mod3)

#model-testing
#predicting the probability of customer churning
pred<-predict(mod3,type="response",newdata = test)
head(pred)
summary(pred)
test$pred1<-pred
View(test)

#assuming cutoff probability as per churn dataset
table(tele1$churn)/nrow(tele1)
s<-seq(0.02,02,0.8)
n<--1
a<-as.vector(length(s))
for(i in s)
{
  print(i)
  test$result<-as.factor(ifelse(test$pred1>i,1,0))
  test$churn<-as.factor(test$churn)
  a[n]<-confusionMatrix(test$result,test$churn,positive="1")$overall[2]
  print(n)
  n=n+1
}
max(a)
#as kappa value goes with 0.23,considering it
pred2<- ifelse(pred>=0.2380,1,0)
table(pred2)

#checking prediction quality
#kappa matrix
library(irr)
kappa2(data.frame(test$churn,pred2))

#confusion matrix
library(caret)
pred2<-as.factor(pred2)
test$churn<-as.factor(test$churn)
confusionMatrix(pred2,test$churn,positive="1")
table(test$churn)

#2627 correct events & 1900 incorrect events
#8390 correct non-events & 6499 incorrect non-events
#model doing an ok job!

#ROCR curve
library(ROCR)
pred2<-as.numeric(pred2)
test$churn<-as.numeric(test$churn)
pred3<-prediction(pred2,test$churn)
pref<-performance(pred3,"tpr","fpr")
plot(pref,col="red")
abline(0,1,lty=8,col="grey")
auc<-performance(pred3,"auc")
auc
unlist(slot(auc,"y.values"))
auc
# auc is 0.5718996 is more than 0.50
# curve seems to be above grey line
#model is doing ok

#gains chart
library(gains)
gains(test$churn,predict(mod3,type="response",newdata=test),groups=10)

#top 40% of probabilities show that 42% of customers are likely to churn
test$prob<-predict(mod3,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#top 40% of probability scores lie between 0.27 to 0.85

#extracting cust ids
targeted<- test[test$prob>0.27 & test$prob<0.85 & test$churn=="1","Customer_ID"]
targeted<-as.data.frame(targeted)
nrow(targeted)
# 4137 customers likely to churn!

### ********** Answering Business Questions ********** ###
### Top Line Questions of Interest to Senior Management: 

#  1.  What are the top five factors driving likelihood of churn at Mobicom?
head(sort(abs(mod3$coefficients),decreasing = T),10)
summary(mod3)

# from the summary of my final model, "mod3".

## The model results show that the top 5 factors affecting churn are:
### a. unq_7            with beta coefficient of 0.735625538
### b. retdays_1        with beta coefficient of 0.670774312
### c. ethnic_O         with beta coefficient of 0.313047896
### d. area_nrthwst     with beta coefficient of 0.283039470
### e. area_southflrda  with beta coefficient of 0.272490954

# The 1st factor explains, with a unit increase in level 7 of variable uniqsubs, there is 0.735625538 unit increase
# in churn.
# The 2nd Factor explains, with a unit increase in variable retdays, there is 0.670774312 unit increase in churn.
# Same explaination applies to the next 3 variables.
# var ethnic_O represents var ethnic with level o, var area_nrthwst is NORTHWEST/ROCKY MOUNTAIN AREA, and 
# var area_southflrda represents var SOUTH FLORIDA AREA, Var retdays_1 represents valid values for var retdays,i.e.
# values more than "0"

# Thus family bundles should be rolled out for families with 7 unique subscribers. Special offers should be given
# to customers who makes retention calls, at the earliest as per their grieviances. Special plans should be rolled out for 
# people with Asian Ethnicity. Special special plans should be rolled out for customers located in NORTHWEST/ROCKY 
# MOUNTAIN AREA and SOUTH FLORIDA AREA. 


#   2.  Validation of survey findings. 
# a) Whether "cost and billing" and "network and service quality" are important factors influencing churn behaviour.  

# The following variables explain "cost and billing" and "network and service quality"

# Variables totmrc_Mean i.e. 'base plan charge' representing cost to customer, 
# var rev_Range i.e. 'Range of Revenue(charge amount)' representing billing amount,
# var ovrrev_Mean = DATOVR_MEAN + VCEOVR_MEAN i.e. 'Mean overage revenue' (It is the sum of data and voice 
# overage revenues) representing the overage revenue earned from customers after billing the same to them.   
# and var  totrev i.e. 'Total revenue' representing total revenue earned from customers.


# var totmrc_Mean has beta coefficient value of -0.005294251 meaning a unit increase in this variable is causing 
# decrease in churn by 0.005294251/unit.

# var rev_Range has beta coefficient value of 0.002095208 meaning a unit increase in this variable is causing 
# increase in churn by 0.002095208/unit

# var ovrrev_Mean has beta coefficient value of 0.007265908 meaning a unit increase in this variable is causing 
# increase in churn by 0.007265908/unit

# var totrev has beta coefficient value of 0.000197018 meaning a unit increase in this variable is causing 
# increase in churn by 0.000197018/unit

# Having said that, if we notice above mentioned beta values, a unit increase in them is having almost 0% impact 
# on churn. SO it seems cost and billing is not very important factors here influencing churn behaviour at Mobicom.



# The following variables explain "network and service quality" 

# VARIABLE          BETA COEFFICIENT

# mou_Range         0.000300765
# change_mou       -0.000653979
# drop_blk_Mean     0.007668757
# drop_vce_Range    0.018691566 
# mou_opkv_Range   -0.001117168 
# iwylis_vce_Mean  -0.015130015
# avgqty            0.001032554 
# avg6mou          -0.000327649
# adjmou            0.000014846
# retdays_1         0.670774312
# complete_Mean    -0.001719650


# From the above statistics, data explains the following:

# Variables mou_Range 1.e. with a unit increase in 'Range of number of minutes of use', 
#       there is increase in Churn by 0.000300765 units.
# var change_mou i.e. with a unit increase in 'Percentage change in monthly minutes of 
#     use vs previous three month average, there is decrease in Churn by -0.000653979 units.
# var drop_blk_Mean i.e. with unit increase in 'Mean number of dropped or blocked calls', 
#     there is an increase in churn by 0.007668757 units
# var drop_vce_Range i.e. with a unit increase in 'Range of number of dropped (failed) voice calls', 
#     there is an increase in Churn by 0.018691566 units.
# var mou_opkv_Range  i.e. with a unit increase in  'Range of unrounded minutes of use of 
#     off-peak voice calls, there is a decrease in Churn by -0.001117168 units.
# var iwylis_vce_Mean i.e. with a unit increase in 'Mean number of inbound wireless to wireless voice calls',
#     there is a decrease in churn by -0.015130015 units.
# var avgqty i.e. with a unit increase in 'Average monthly number of calls over the life of the customer',
#     there is an increase in Churn by 0.001032554 units.
# var avg6mou i.e. with unit increase in 'Average monthly minutes of use over the previous six months',
#     there is a decrease in Churn by -0.000327649 units.
# var adjmou i.e. with unit increase in  'Billing adjusted total minutes of use over the life of the customer',
#     there is an increase in Churn by 0.000014846 units.
# var retdays_1 representing values captured in the variable retdays i.e. with a unit increase in 
#     'Number of days since last retention call', there is an increase in Churn by 0.000014846 units.
#     This variable is probably explaining the service quality of the company.
# var complete_Mean i.e. with unit increase in 'Mean number of completed voice and data calls' 
#     there is a decrease in Churn by -0.001719650 units

# Of the above variables, the beta coefficient of variable retdays_1 is expressing a very important 
# factor influencing Churn behaviour. That is  with the increase in the number of days since a customer 
# makes a retention call, the customer's chances of churning is very high. This could probably be because
# their grieviances are not being catered to properly. These customers should be paid more attention to and 
# special offers should be made to them depending upon their grieviances.


 

#  2b) Are data usage connectivity issues turning out to be costly? In other words, is it leading to churn? 

#   comp_dat_Mean - Mean no. of completed data calls. 
#   plcd_dat_Mean - Mean number of attempted data calls placed
#   opk_dat_Mean - Mean number of off-peak data calls
#   blck_dat_Mean - Mean no. of blocked / failed data calls
#   datovr_Mean - Mean revenue of data overage. 
#   datovr_Range - Range of revenue of data overage
#   drop_dat_Mean - Mean no. of dropped / failed data calls

#   The above variables express data usage connectivity. 
quantile(tele$plcd_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))

#   The Data Quality Report for all the above variables show that only 10% to 15% customers are actualy 
#   making data calls or using the internet. 
#   This could be a matter of concern since the global market survey report shows "Subscribers who 
#   have switched operators in recent months reported two key information sources in their decision:
#   the Internet and recommendation of family and friends.. 
#   In this case it seems customers are not really using the internet. So it would be good to work 
#   towards attaining more customers to use data and also towards proving quality network connectivity
#   and service to provide maximum customer satisfaction and reduce Churn.
#   Since there is not enough usable data for the above variables they are not showing any influence 
#   on the Churn Behaviour at Mobicom.




#   3. Would you recommend rate plan migration as a proactive retention strategy?

#   Variable ovrrev_Mean has beta coefficient of 0.007265908. 
#   var ovrrev_Mean = DATOVR_MEAN + VCEOVR_MEAN i.e. 'Mean overage revenue' 
#   It is the sum of data and voice overage revenues representing the overage revenue earned 
#   from customers after billing the same to them. 
#   The Beta coefficient is not showing a strong impact of overage billing as an influencer 
#   of churn behaviour. 
#   Though this might be a matter of concern for few individual customers and they could be 
#   catered to on case to case basis. But overall rate plan migration as a proactive retention strategy
#   might not help much at Mobicom.


#   4. What would be your recommendation on how to use this churn model for prioritisation
#   of customers for a proactive retention campaigns in the future?

# Solution:
#Gains Chart
library(gains)
gains(test$churn,predict(mod3,type="response",newdata=test),groups = 10)
#the Gains Chart shows that the top 20% of the probabilities contain 29.5% customers that are highly likely to churn.


# Selecting Customers with high churn rate
test$prob<-predict(mod3,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

# Top 20% of the probabilities lie between 0.3042058 and 0.7529329

# Applying cutoff value to predict customers who Will Churn
pred4<-predict(mod3, type="response", newdata=test)
pred4<-ifelse(pred4>=0.3042058 , 1, 0)
table(pred4,test$churn)

Targeted<-test[test$prob>0.3042058 & test$prob<=0.7529329 & test$churn=="1","Customer_ID"]
Targeted<-as.data.frame(Targeted)
nrow(Targeted)

write.csv(Targeted,"Target_Customers.csv",row.names = F)

#   Thus Using the model can be used to predict customers with high probability of Churn and extract the 
#   target list using their "Customer ID". 



# 5. What would be the target segments for proactive retention campaigns? 
# Falling ARPU forecast is also a concern and therefore, Mobicom would like to save their high revenue 
# customers besides managing churn. Given a budget constraint of a contact list of 20% of the subscriber pool, 
# which subscribers should prioritized if "revenue saves" is also a priority besides controlling churn. 
# In other words, controlling churn is the primary objective and revenue saves is the secondary objective.

# Solution:
pred5<-predict(mod3, type="response", newdata=test)
test$prob<-predict(mod3,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
pred6<-ifelse(pred5<0.20,"Low_Score",ifelse(pred5>=0.20 & pred5<0.30,"Medium_Score","High_Score"))
table(pred6,test$churn)

str(test$totrev)
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
Revenue_Levels<-ifelse(test$totrev<670.660,"Low_Revenue",ifelse(test$totrev>=670.660 & 
                                                                  test$totrev<1034.281,"Medium_Revenue","High_Revenue"))

table(Revenue_Levels)

table(pred6,Revenue_Levels)

##  Thus this table can be used to select the levels of customers are to be targeted
##  and the Target list can be extracted as follows:

test$prob_levels<-ifelse(pred5<0.20,"Low_Score",ifelse(pred5>=0.20 & pred5<0.30,"Medium_Score","High_Score"))
test$Revenue_Levels<-ifelse(test$totrev<670.660,"Low_Revenue",ifelse(test$totrev>=670.660 & 
                                                                  test$totrev<1034.281,"Medium_Revenue","High_Revenue"))

Targeted1<-test[test$prob_levels=="High_Score" & test$Revenue_Levels=="High_Revenue","Customer_ID"]
Targeted1<-as.data.frame(Targeted1)
nrow(Targeted1)

write.csv(Targeted1,"High_Revenue_Target_Customers.csv",row.names = F)



























































