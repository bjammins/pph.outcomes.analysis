library(VIF)
library(plyr)
library(reshape)
library(reshape2)
library(MASS)
library(dplyr)
library(car)
library(lmtest)
library(ggplot2)
library(pvclust)

setwd("L:/PPH Project/data")
data_aa <- read.csv("QI Learning Collaborative List for Critical Juncture.csv", header=TRUE)
data <- read.csv("Baseline Assessment Survey responses.csv", header=TRUE)
question <- read.csv("baseline assessment survey question table.csv", header=TRUE)
data <- tbl_df(data)
datac <- subset(data, Q49==1)#only get completed survey data
datac <- arrange(datac,Q1)
datac$Q2[datac$Q2==""] <- NA  #convert spaces to NAs
hosp1 <- as.data.frame(na.omit(datac$Q2))
names(hosp1) <- "hosp_name"
datac$Q3[datac$Q3==""] <- NA
hosp2 <- as.data.frame(na.omit(datac$Q3))
names(hosp2) <- "hosp_name"
datac$Q4[datac$Q4==""] <- NA
hosp3 <- as.data.frame(na.omit(datac$Q4))
names(hosp3) <- "hosp_name"
hosp_name <- rbind(hosp1,hosp2,hosp3) #combine hospital names into one column
datac <- cbind(datac,hosp_name) #combine data frames
datac <- dplyr::select(datac,-c(Q2,Q2.1,Q3,Q3.1,Q4,Q4.1)) #cleanup of hospital names because they have now been combined into hosp_name
datac <- datac %>% mutate(birth_cat = 1*(Q7<1000) + 2 * (Q7 >999 & Q7<3000)+3*(Q7>2999)) #create birth categories
#note original code was datac<-datac[-66,]  I realized that this was an error in the code as I wanted to remove Hackensack UMC because they did not have a birth category.  Instead, I believe I removed hunterdon

datac<-datac[!is.na(datac$birth_cat),] #remove hospital without a birth category Hackensack UMC- Pascack Valley
datac$birth_cat<-as.factor(datac$birth_cat)


#set a student indicator variable to either 1 or 0 if they have a student vs they don't have a student

datac$Q9 <- as.numeric(ifelse(datac$Q9 == "Nursing students", 1, 0))
datac$Q9.1 <- as.numeric(ifelse(datac$Q9.1 == "Medical students", 1, 0))
datac$Q9.2 <- as.numeric(ifelse(datac$Q9.2 == "Midwifery students", 1, 0))
student<-cbind(datac$Q9,datac$Q9.1, datac$Q9.2)
datac$student<-rowSums(student)
datac$student<-ifelse(datac$student>0,1,0)
datac$student<-as.factor(datac$student)

#format cesarean rates
#datac$Q8<-as.numeric(datac$Q8)

datac<-datac %>% mutate(cesarean = 1*(Q8<30) + 2 * (Q8 >29.99 & Q8<40)+3*(Q8 >39.99))
datac$cesarean<-as.factor(datac$cesarean)

#set resident indicator

datac$Q9.3<-as.numeric(ifelse(datac$Q9.3 == "Obstetric residents", 1, 0))
datac$Q9.4<-as.numeric(ifelse(datac$Q9.4 == "Family practice residents who provide OB services", 1, 0))
resident<-cbind(datac$Q9.3,datac$Q9.4)
datac$resident<-rowSums(resident)
datac$resident<-ifelse(datac$resident>0,1,0)
datac$resident<-as.factor(datac$resident)

datac$Q12<-revalue(datac$Q12, c("State and local government hospital"="O"))
datac$Q12<-revalue(datac$Q12, c("Non-government not-for-profit hospital"="NP"))
datac$Q12<-revalue(datac$Q12, c("Non-government investor-owned (for profit) hospital"="FP"))
datac$Q12<-revalue(datac$Q12, c("Other (please specify)"="O"))
datac$Q12<-revalue(datac$Q12, c("Unknown"="O"))

#reordering of the levels to get them in the order that I want
datac$Q14<-factor(datac$Q14, levels=c("Low level of preparedness","Moderate level of preparedness","High level of preparedness","Very high level of preparedness"))
datac$Q14<-revalue(datac$Q14, c("Low level of preparedness"= 0))
datac$Q14<-revalue(datac$Q14, c("Moderate level of preparedness"= 1))
datac$Q14<-revalue(datac$Q14, c("High level of preparedness"= 2))
datac$Q14<-revalue(datac$Q14, c("Very high level of preparedness"= 2))
datac$Q15<-factor(datac$Q15, levels=c("We do not have a standard definition for postpartum hemorrhage","greater than 500 ml of blood loss for all deliveries","greater than 500 ml for vaginal deliveries and greater than 1000 ml for cesarean deliveries","greater than 1000 ml for all deliveries","Other definition (please specify)")) #remove the "" factor
datac$Q16<-factor(datac$Q16, levels=c("Yes","No","Unknown")) #remove the ""factor



#maternal transports indicator
datac$Q10<-as.numeric(ifelse(datac$Q10 == "Yes", 1, 0))
datac$Q10<-as.factor(datac$Q10)

#neonatal transpors indicator
datac$Q11<-as.numeric(ifelse(datac$Q11 == "Yes", 1, 0))
datac$Q11<-as.factor(datac$Q11)

#Magnet
datac$Q13<-as.numeric(ifelse(datac$Q13 == "Yes", 1, 0))
datac$Q13<-as.factor(datac$Q13)

#####create collaborative statistics
#Collaborative only hospitals
collab<-datac[datac$collaborative=="1",]
collab<-collab[collab$hosp_name!="Saint Peter's University Hospital|254 Easton Avenue P.O. Box 591 New Brunswick, NJ 08903-0591",] #remove St. Peters as they weren't part of the final collaborative
summary(collab$Q1) #state

#calculate summary statistics by hospitals in the collaborative
#statistics for calculating hospital birth data
summary(collab$Q7)
collab_sum<-collab%>%
  group_by(Q1)%>%
  summarise(n=n(), mean=mean(Q7, na.rm=TRUE), sd=sd(Q7, na.rm=TRUE), `25%`=quantile(Q7, probs=0.25, na.rm=TRUE), median=median(Q7, na.rm=TRUE), `75%`=quantile(Q7, probs=0.75, na.rm=TRUE), min=min(Q7, na.rm=TRUE), max=max(Q7, na.rm=TRUE))



#birth category data
collab_sum_birth_cat<-collab%>%
  group_by(Q1,birth_cat)%>%
  summarise(n=n())



#cesarean data
collab_sum_cesarean<-collab%>%
  group_by(Q1,cesarean)%>%
  summarise(n=n())


#Student data
collab_sum_students<-collab%>%
  group_by(Q1,student)%>%
  summarise(n=n())



#resident data
collab_sum_resident<-collab%>%
  group_by(Q1,resident)%>%
  summarise(n=n())


#magnet data
collab_sum_magnet<-collab%>%
  group_by(Q1,Q13)%>%
  summarise(n=n())


#maternal transport
collab_sum_mat_trans<-collab%>%
  group_by(Q1,Q10)%>%
  summarise(n=n())

#neonatal transport
collab_sum_neo_trans<-collab%>%
  group_by(Q1,Q11)%>%
  summarise(n=n())


#hospital type
collab_sum_hospital_type<-collab%>%
  group_by(Q1,Q12)%>%
  summarise(n=n())


#calculate summary statistics by hospitals in the collaborative by QBL Implementation fidelity category
#statistics for calculating hospital birth data
summary(collab$Q7)
collab_sum_qbl<-collab%>%
  group_by(qbl.imp.fid)%>%
  summarise(n=n(), mean=mean(Q7, na.rm=TRUE), sd=sd(Q7, na.rm=TRUE), `25%`=quantile(Q7, probs=0.25, na.rm=TRUE), median=median(Q7, na.rm=TRUE), `75%`=quantile(Q7, probs=0.75, na.rm=TRUE), min=min(Q7, na.rm=TRUE), max=max(Q7, na.rm=TRUE))

#cesarean data
collab_sum_cesarean_qbl<-collab%>%
  group_by(qbl.imp.fid,cesarean)%>%
  summarise(n=n())

#magnet data
collab_sum_magnet_qbl<-collab%>%
  group_by(qbl.imp.fid,Q13)%>%
  summarise(n=n())


#maternal transport
collab_sum_mat_trans_qbl<-collab%>%
  group_by(qbl.imp.fid,Q10)%>%
  summarise(n=n())

#hospital type
collab_sum_hospital_type_qbl<-collab%>%
  group_by(qbl.imp.fid,Q12)%>%
  summarise(n=n())



#calculate summary statistics by hospitals in the collaborative by Risk Assessment fidelity category
#statistics for calculating hospital birth data
summary(collab$Q7)
collab_sum_qbl<-collab%>%
  group_by(ra.imp.fid)%>%
  summarise(n=n(), mean=mean(Q7, na.rm=TRUE), sd=sd(Q7, na.rm=TRUE), `25%`=quantile(Q7, probs=0.25, na.rm=TRUE), median=median(Q7, na.rm=TRUE), `75%`=quantile(Q7, probs=0.75, na.rm=TRUE), min=min(Q7, na.rm=TRUE), max=max(Q7, na.rm=TRUE))

#cesarean data
collab_sum_cesarean_ra<-collab%>%
  group_by(ra.imp.fid,cesarean)%>%
  summarise(n=n())

#magnet data
collab_sum_magnet_ra<-collab%>%
  group_by(ra.imp.fid,Q13)%>%
  summarise(n=n())


#maternal transport
collab_sum_mat_trans_ra<-collab%>%
  group_by(ra.imp.fid,Q10)%>%
  summarise(n=n())

#hospital type
collab_sum_hospital_type_ra<-collab%>%
  group_by(ra.imp.fid,Q12)%>%
  summarise(n=n())


#obtain intensity data

intensity <- read.csv("qi_measures_05_05_2016.csv", header = TRUE)
intensity <- intensity[intensity$Hospital.Name!="Saint Peter's University Hospital",]

#calculating mean, sd, 25th, median, 75th percentiles
intensity_hours<-intensity%>%
  summarise(mean=mean(Q30, na.rm=TRUE), sd=sd(Q30, na.rm=TRUE), `25%`=quantile(Q30, probs=0.25, na.rm=TRUE), median=median(Q30, na.rm=TRUE), `75%`=quantile(Q30, probs=0.75, na.rm=TRUE), min=min(Q30, na.rm=TRUE), max=max(Q30, na.rm=TRUE))
