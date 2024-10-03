#######################################################################X
##############   Argentine Black and White Tegu          ##############X
##############   Body condition analysis                 ##############X
##############   Created by: Jenna Cole                  ##############X
##############   Modified by Sergio A. Balaguera-Reina   ##############X
##############               Daniel Bonilla-Liberato     ##############X
##############   Last day modified: Sep 3rd, 2024        ##############X
#######################################################################X

rm(list = ls())
dev.off()

#Libraries----
library(tidyverse)
library(MASS)
library(weathermetrics)

library(FSA)
library(openintro) 
library(e1071)
library(readxl)
library(olsrr)
library(performance)
library(MuMIn)
library(ggpubr)
library(glm.nb)

#### 1. Calling and formating data----
tegu.data <- read.csv("tegudata20211111.csv")

#add column for month and year
tegu.data<-tegu.data %>%
  mutate(year = year(tegu.data$Capture.Date), 
                month.numb = month(tegu.data$Capture.Date), 
                day = yday(tegu.data$Capture.Date))

#convert month.num to name
tegu.data$month<-month.abb[tegu.data$month.numb]
tegu.data$month<-factor(tegu.data$month, levels=month.abb)

##Setting data table up 
tegu.data$mass.g <- with(tegu.data, Intake.Weight.kg*1000)##body mass in grams
tegu.data$percfat <- with(tegu.data, Fat.Wt..g./mass.g*100)##calculating fat percentage
tegu.data$fulton <- with(tegu.data, (mass.g/Intake.SVL^3)*10^2)##multiplied by 10^2 to make mixed number 

#Changing Sex to factor
tegu.data$Analysis.Sex<-factor(tegu.data$Analysis.Sex, levels=c("Female", "Male"), ordered=T)
levels(tegu.data$Analysis.Sex)

##Exploratory graphs
#SVL data
hist(tegu.data$Intake.SVL, breaks=100, main="Tegu SVL")
abline(v=30, col="red",lwd=2)##Vertical line at svl = 30, cutoff for adult size class

#fultons K
#remove data from tegu.data which are clearly a result of typos
tegu.data<-tegu.data[!(tegu.data$UF.Animal.ID=="TuMe2339"|tegu.data$UF.Animal.ID=="TuMe3178"),]

hist(tegu.data$fulton, breaks=100, main="Fulton's K")
abline(v=2.77, col="red",lwd=2)

##Dealing with date, converting date from character to actual date (POSIXct format)
class(tegu.data$Capture.Date)
class(tegu.data$month)
tegu.data$month <-month.abb[tegu.data$month]
levels(tegu.data$month)

str(tegu.data)

range(tegu.data$Intake.SVL, na.rm=TRUE)
summary(tegu.data$Intake.SVL)

hist(tegu.data$Intake.SVL, breaks=100, main="Tegu SVL")#histogram of SVL data
abline(v=30, col="red",lwd=2)##Verticle line at svl = 30, cutoff for adult size class
boxcox(tegu.data$Intake.SVL)

hist(tegu.data$fulton, breaks=100, main="Fulton's K excluding hatchlings and outliers")
boxcox(tegu.data$fulton) #Not normal

#### Add distance and environmental data####
teguhabdat<-read.csv("20220224habdata.csv")
unique(teguhabdat$Hab1)
teguhabdat$Hab.Name1<- ifelse(teguhabdat$Hab1 == 4, "Prairie and Bog",
                              ifelse(teguhabdat$Hab1== 5, " Marshes",
                                     ifelse(teguhabdat$Hab1 == 6, "Freshwater Forrested Wetland",
                                            ifelse(teguhabdat$Hab1 == 7, "Cultural - Lacustrine",
                                                   ifelse(teguhabdat$Hab1 == 9, "Mangrove Swamp",
                                                          ifelse(teguhabdat$Hab1== 10, "Scrub Mangrove",
                                                                 ifelse(teguhabdat$Hab1== 12, "High Intensity Urban", "Cropland")))))))

unique(teguhabdat$Hab2)
teguhabdat$Hab.Name2<- ifelse(teguhabdat$Hab2==4, "mixed scrub shrub wetland",
                              ifelse(teguhabdat$Hab2==5, "Marshes",
                                     ifelse(teguhabdat$Hab2== 6, "Glades Marsh",
                                            ifelse(teguhabdat$Hab2==7, "mixed hardwood wetlands",
                                                   ifelse(teguhabdat$Hab2 ==8, "artificial impoundment",
                                                          ifelse(teguhabdat$Hab2==11, "Mangrove swamp",
                                                                 ifelse(teguhabdat$Hab2==12, "scrub mangrove",
                                                                        ifelse(teguhabdat$Hab2==14, "residential - med",
                                                                               ifelse(teguhabdat$Hab2==15, "institutional",
                                                                                      ifelse(teguhabdat$Hab2 ==17, "field crops", "residential high"))))))))))

teguhabdat$HabCat<-ifelse(teguhabdat$Hab1 == 4, "Prairie and Bog",
                          ifelse(teguhabdat$Hab1== 5, "Marshes",
                                 ifelse(teguhabdat$Hab1 == 6, "Freshwater Forrested Wetland",
                                        ifelse(teguhabdat$Hab1 == 7, "Urban",
                                               ifelse(teguhabdat$Hab1 == 9, "Mangrove Swamp",
                                                      ifelse(teguhabdat$Hab1== 10, "Mangrove Swamp",
                                                             ifelse(teguhabdat$Hab1== 12,"Urban","Urban")))))))
teguhabdat$Habitat<-ifelse(teguhabdat$Hab1 == 4, "1",
                           ifelse(teguhabdat$Hab1== 5, "2",
                                  ifelse(teguhabdat$Hab1 == 6, "3",
                                         ifelse(teguhabdat$Hab1 == 7, "4",
                                                ifelse(teguhabdat$Hab1 == 9, "5",
                                                       ifelse(teguhabdat$Hab1== 10, "5",
                                                              ifelse(teguhabdat$Hab1== 12,"4","4")))))))

names(teguhabdat) <- gsub("_", ".", names(teguhabdat)) 
teguhabdat$Capture.Date<-as.Date(teguhabdat$Capture.Date, "%m/%d/%Y")
teguhabdat$Euth.Date<-as.Date(teguhabdat$Euth.Date, "%m/%d/%Y")
class(teguhabdat$Capture.Date)

#Import FAWN environmental data
env.data<-read.csv("FAWN_report.csv")

#merge teguhabdat with tegu.data
tegu.data<- merge(tegu.data, teguhabdat, all.y=T, incomparables=NULL)

#Need to average environmental data from 2014-2019 for each month
#make full date
env.data$Period<-as.Date(paste("01-", env.data$Period, sep = ""), format = "%d-%b-%y")

#create new columns
env.data<-env.data %>%
  dplyr::mutate(year = lubridate::year(env.data$Period), 
                month.numb = lubridate::month(env.data$Period))

#convert month.num to name
env.data$month<-month.abb[env.data$month.numb]
env.data$month<-factor(env.data$month, levels=month.abb)

#remove unused columns
env.data <- env.data[-c(1,8,10)]

#rename columns
env.data<-rename(env.data, 
                 Avg.Temp.F=X60cm.T.avg..F.,
                 Min.Temp.F=X60cm.T.min..F.,
                 Max.Temp.F= X60cm.T.max..F.,
                 Rain.in=X2m.Rain.tot..in.,
                 Rain.Max.15.in=X2m.Rain.max.over.15min..in.)

#convert F to C, and inches to centimeters
env.data$Avg.Temp.C<-fahrenheit.to.celsius(env.data$Avg.Temp.F)
env.data$Min.Temp.C<-fahrenheit.to.celsius(env.data$Min.Temp.F)
env.data$Max.Temp.C<-fahrenheit.to.celsius(env.data$Max.Temp.F)
env.data$Rain.cm<-env.data$Rain.in*2.54
env.data$Rain.Max.15.cm<-env.data$Rain.Max.15.in*2.54

#add monthly average to each tegu based on capture date
tegu.data<-merge(tegu.data, env.data, by.x=c("month", "year"), by.y=c("month", "year"), all=TRUE)

#Remove environmental data that has no tegus associateed with it
tegu.data<-tegu.data%>% drop_na(UF.Animal.ID)

#get julian date
tegu.data$Capture.Date<- as.Date(tegu.data$Capture.Date)

tegu.data$julian<-NA
tegu.data$julian<-yday(tegu.data$Capture.Date)

#Check slope for fultons K
#take log of SVL and weight
tegu.data$lsvl<-log(tegu.data$Intake.SVL)
tegu.data$ltl<-log(tegu.data$Intake.TL)
tegu.data$lmass<-log(tegu.data$mass.g)

#create groups
#Group 1 = 0-20.2; Group 2 = 20.2-30.0, Group 3 = 30.0+
tegu.data$sizeclass <- factor(ifelse(tegu.data$Intake.SVL <=20.2, "Group1", 
                                     ifelse(tegu.data$Intake.SVL >=30.1, "Group3","Group2")))

#Create dummy variables for habitat, categorical- 5 categories
tegu.data$d1 <- as.numeric(ifelse(tegu.data$Habitat==1, "1", "0"))
tegu.data$d2 <- as.numeric(ifelse(tegu.data$Habitat==2, "1", "0"))
tegu.data$d3 <- as.numeric(ifelse(tegu.data$Habitat==3, "1", "0"))
tegu.data$d4 <- as.numeric(ifelse(tegu.data$Habitat==4, "1", "0"))
tegu.data$d5 <- as.numeric(ifelse(tegu.data$Habitat==5, "1", "0"))

#create dummy variables for year 2014-2019
tegu.data$y14<- as.numeric(ifelse(tegu.data$year==2014, "1","0"))
tegu.data$y15<- as.numeric(ifelse(tegu.data$year==2015, "1","0"))
tegu.data$y16<- as.numeric(ifelse(tegu.data$year==2016, "1","0"))
tegu.data$y17<- as.numeric(ifelse(tegu.data$year==2017, "1","0"))
tegu.data$y18<- as.numeric(ifelse(tegu.data$year==2018, "1","0"))
tegu.data$y19<- as.numeric(ifelse(tegu.data$year==2019, "1","0"))

#create dummy variables for month (february - december)
tegu.data$m2<-as.numeric(ifelse(tegu.data$month.numb==2,"1", "0" ))
tegu.data$m3<-as.numeric(ifelse(tegu.data$month.numb==3,"1", "0" ))
tegu.data$m4<-as.numeric(ifelse(tegu.data$month.numb==4,"1", "0" ))
tegu.data$m5<-as.numeric(ifelse(tegu.data$month.numb==5,"1", "0" ))
tegu.data$m6<-as.numeric(ifelse(tegu.data$month.numb==6,"1", "0" ))
tegu.data$m7<-as.numeric(ifelse(tegu.data$month.numb==7,"1", "0" ))
tegu.data$m8<-as.numeric(ifelse(tegu.data$month.numb==8,"1", "0" ))
tegu.data$m9<-as.numeric(ifelse(tegu.data$month.numb==9,"1", "0" ))
tegu.data$m10<-as.numeric(ifelse(tegu.data$month.numb==10,"1", "0" ))
tegu.data$m11<-as.numeric(ifelse(tegu.data$month.numb==11,"1", "0" ))
tegu.data$m12<-as.numeric(ifelse(tegu.data$month.numb==12,"1", "0" ))

#remove hand captured tegu
tegu.data<- tegu.data[!(tegu.data$UF.Animal.ID=="TuMe2348"),] 

#IQR for outliers in fulton, UF Data
iqr<-IQR(tegu.data$fulton)

lowerq=quantile(tegu.data$fulton)[2]
upperq=quantile(tegu.data$fulton)[4]

mild.threshold.upper = (iqr * 1.5) + upperq
mild.threshold.lower = lowerq - (iqr * 1.5)
mild.threshold.lower #
mild.threshold.upper #
#any K value outside of 2.16-3.95 is a minor outlier

#UF data remove outliers
tegu.data<-tegu.data[which(tegu.data$fulton<3.95& tegu.data$fulton>2.16),]
#write.csv(tegu.data, "tegudatanooutliers2022Aug25.csv", row.names=F)

#remove column
tegu.data <- subset (tegu.data, select = -c(Age.Class))

#Create Data Subsets
Group1<-subset(tegu.data, tegu.data$sizeclass=="Group1")
Group2<-subset(tegu.data, tegu.data$sizeclass== "Group2")
Group3<-subset(tegu.data, tegu.data$sizeclass== "Group3")

#check slope
tegu.data$lsvl<-log(tegu.data$Intake.SVL)
tegu.data$ltl<-log(tegu.data$Intake.TL)
tegu.data$lmass<-log(tegu.data$mass.g)

summary(lm(lmass~lsvl, data=tegu.data))

#check slope
summary(lm(lmass~lsvl, data=Group1)) #3.07+-0.04
summary(lm(lmass~lsvl, data=Group2)) #3.06+-0.3
summary(lm(lmass~lsvl, data=Group3)) #2.97+- 0.07

#create data set excluding tegus with abnormalities

##Remove Tegus having malformed limbs/spine, missing tail or limbs, because that affect weight. 
#need to find a better way to do this? abnormal limbs?
AbnormalTail <- tegu.data[grepl("kinked|Kinked|Regen|regen|Regenerated|regenerated|Broken|Nub|nub|
                                broken|tip of tail|Nubbed|nubbed|tail tip missing| tail regeration|
                                tail regneration|stump|stumpy|stub", 
                                ignore.case=T, tegu.data$Extremities.Notes), ]
AbnormalTail2 <- tegu.data[grepl("kinked|Kinked|Regen|regen|Regenerated|regenerated|Broken|Nub|nub|
                                 broken|tip of tail|nubbed|Nubbed|Kink|kink|kinky|Kinky|dropped|Dropped|
                                 missing tail|Missing tail|no tail| No tail|tail tip missing|
                                 tail regeration| tail regneration|stump|stumpy|stub|
                                 missing part of tail|little piece of tail missing|regneration|rengeration", 
                                 ignore.case= T, tegu.data$General.Notes), ]
AbnormalTail3 <- tegu.data[grepl("kinked|Kinked|Regen|regen|Regenerated|regenerated|Broken|Nub|nub|broken|
                                 tip of tail|nubbed|Nubbed|Kink|kink|kinky|Kinky|dropped|
                                 Dropped|missing tail|Missing tail|no tail| No tail", 
                                 ignore.case=T, tegu.data$Malformations), ]
#combine and remove duplicates
Abnormaltail.All <- rbind(AbnormalTail, AbnormalTail2, AbnormalTail3)
Abnormaltail.AllND <- Abnormaltail.All[!duplicated(Abnormaltail.All$UF.Animal.ID), ]

MissingFeet<-tegu.data[grepl("missing front left foot|Missing front left foot|
                             missing front right foot|Missing front right foot|
                             missing limb|Missing limb|missing left arm|missing right arm|no foot|
                             No foot|missing right forelimb| missing left forelimb|missing rear left leg|
                             Missing rear left leg|Right rear foot gone|Rear right foot is missing|
                             rear foot missing", ignore.case=T, tegu.data$Extremities.Notes), ]
MissingFeet2<-tegu.data[grepl("rear right leg missing|Tip of tail missing|Tip of Tail Missing
                              | Tip of tail is missing|Tail regeneration-|Tail regeneration| Stumpy| Stump| 
                              Stub|Right front foot missing| Rear right leg missing |No tail|
                              Missing tip of the tail|Missing segment of tail| Missing part of tail| 
                              Missing left front foot| Missing front right limb| 
                              Little piece of tail missing| Missing front right leg|
                              missing front left foot|Missing front left foot|missing front right foot|
                              Missing front right foot|missing limb|Missing limb|missing left arm|
                              missing right arm|no foot|No foot|missing right forelimb| 
                              missing left forelimb|missing rear left leg|Missing rear left leg|
                              Right rear foot gone|Rear right foot is missing|rear foot missing|
                              missing rear right foot|missing left front foot|missing front right limb|
                              missing front right leg", ignore.case=T, tegu.data$General.Notes), ]
MissingFeet3<-tegu.data[grepl("Tip of tail missing|Tip of Tail Missing| Tip of tail is missing|
                              Tail regeneration-|Tail regeneration| Stumpy| Stump| Stub|
                              Right front foot missing| Rear right leg missing |No tail|
                              Missing tip of the tail|Missing segment of tail| Missing part of tail| 
                              Missing left front foot| Missing front right limb| 
                              Little piece of tail missing| Missing front right leg|missing front left foot|
                              Missing front left foot|missing front right foot|Missing front right foot|
                              missing limb|Missing limb|missing left arm|missing right arm|no foot|No foot|
                              missing right forelimb| missing left forelimb|missing rear left leg|
                              Missing rear left leg|Right rear foot gone|Rear right foot is missing|
                              rear foot missing", ignore.case=T, tegu.data$Malformations), ]

##Combine and remove duplicates
MissingFeet.All <- rbind(MissingFeet, MissingFeet2, MissingFeet3)
MissingFeet.AllND <- MissingFeet.All[!duplicated(MissingFeet.All$UF.Animal.ID), ]

#combine dataframes
abnormalities.master <- rbind(MissingFeet.AllND, Abnormaltail.AllND)
abnormalities.masterND<-abnormalities.master[!duplicated(abnormalities.master$UF.Animal.ID), ]

#remove abnormalities from tegu.data
tegu.data<-anti_join ( tegu.data , abnormalities.masterND , by = c("UF.Animal.ID"))

#Calculate Fulton's K on tegu.data and compare with Fulton's K of abnormalities.master
tegu.data$mass.g <- tegu.data$Intake.Weight.kg*1000##body mass in grams
tegu.data$fulton <- with(tegu.data, (mass.g/Intake.SVL^3)*10^2)##multiplied by 10^2 to make mixed number 
plot(tegu.data$mass.g ~ tegu.data$Intake.SVL^3)
abnormalities.masterND$mass.g <- abnormalities.masterND$Intake.Weight.kg*1000##body mass in grams
abnormalities.masterND$fulton <- with(abnormalities.masterND, (mass.g/Intake.SVL^3)*10^2)##multiplied by 10^2 to make mixed number 

#check for significance
t.test(tegu.data$fulton, abnormalities.masterND$fulton) 
#not significant, P=0.185

#recombine tegu.data and abnormal data
tegu.data <-merge(tegu.data, abnormalities.masterND, by=intersect(names(tegu.data), names(abnormalities.masterND)), all=T,
                  no.dups = F, incomparables = NULL)

#tegu remove abnormal
tegu.data.ab<-anti_join (tegu.data , abnormalities.masterND , by = c("UF.Animal.ID"))

#remove and outliers
#IQR for outliers in fulton, UF Data
iqr.ab<-IQR(tegu.data.ab$fulton)

lowerq.ab=quantile(tegu.data.ab$fulton)[2]
upperq.ab=quantile(tegu.data.ab$fulton)[4]

mild.threshold.upper.ab = (iqr.ab * 1.5) + upperq.ab
mild.threshold.lower.ab = lowerq.ab - (iqr.ab * 1.5)
mild.threshold.lower.ab #2.16
mild.threshold.upper.ab #3.95
#any K value outside of 2.16-3.95 is a minor outlier

#tegu.data.ab remove outliers
tegu.data.ab<-tegu.data.ab[which(tegu.data.ab$fulton<3.95 & tegu.data.ab$fulton>2.16),]
#write.csv(tegu.data.ab, "tegudata.ab.nooutliers.20220825.csv", row.names=F)

#create groups
Group1.ab<-subset(tegu.data.ab, tegu.data.ab$sizeclass=="Group1")
Group2.ab<-subset(tegu.data.ab, tegu.data.ab$sizeclass== "Group2")
Group3.ab<-subset(tegu.data.ab, tegu.data.ab$sizeclass== "Group3")

#check slope
summary(lm(lmass~lsvl, data=tegu.data.ab)) #3.1 +- 0.01
summary(tegu.data.ab)

#check slope
summary(lm(lmass~lsvl, data=Group1.ab)) #3.04 +- 0.04
summary(lm(lmass~lsvl, data=Group2.ab)) #3.08 +- 0.03
summary(lm(lmass~lsvl, data=Group3.ab)) #2.96 +- 0.09

Group1.ab$slope<-(c(3.04))
Group2.ab$slope<-(c(3.08))
Group3.ab$slope<-(c(2.96))

#save file
#write.csv(tegu.data, "tegudata20220825.csv", row.names=F)
#write.csv(tegu.data.ab, "tegudata.ab20220825.csv", row.names=F)

#### Histogram
hist(tegu.data.ab$fulton, breaks=100, xlab="Fulton's Values", main="Tegu Fulton's Values")#histogram of SVL data
mean(tegu.data.ab$fulton)
abline(v=3.06, col="red",lwd=2)##Verticle line at svl = 30, cutoff for adult size class

####condition index####
unique(Group1.ab$Body.Condition)

#Group 1 - hatchlings
emaciated1<-subset(Group1.ab,Group1.ab$Body.Condition %in% "emaciated")
underweight1<-subset(Group1.ab,Group1.ab$Body.Condition %in% "underweight/lean")
ideal1<-subset(Group1.ab,Group1.ab$Body.Condition %in% "ideal")

mean(emaciated1$fulton) 
sd(emaciated1$fulton)

mean(underweight1$fulton) 
sd(underweight1$fulton) 

mean(ideal1$fulton) 
sd(ideal1$fulton) 

#group 2 - juveniles
emaciated2<-subset(Group2.ab,Group2.ab$Body.Condition %in% "emaciated")
underweight2<-subset(Group2.ab,Group2.ab$Body.Condition %in% "underweight/lean")
ideal2<-subset(Group2.ab,Group2.ab$Body.Condition %in% "ideal")

mean(emaciated2$fulton) #2.422
sd(emaciated2$fulton) #0.24

mean(underweight2$fulton) #2.8
sd(underweight2$fulton) #0.30

mean(ideal2$fulton) #3.01
sd(ideal2$fulton) #0.33

#group 3 - subadults
emaciated3<-subset(Group3.ab,Group3.ab$Body.Condition %in% "emaciated")
underweight3<-subset(Group3.ab,Group3.ab$Body.Condition %in% "underweight/lean")
ideal3<-subset(Group3.ab,Group3.ab$Body.Condition %in% "ideal")

mean(emaciated3$fulton) #2.82
sd(emaciated3$fulton) #0.25

mean(underweight3$fulton) #2.98
sd(underweight3$fulton) #0.24

mean(ideal3$fulton) #3.09
sd(ideal3$fulton)#0.29

#### 3. Histograms of UF Data####

# make histogram of mass, svl and FK
summary (tegu.data.ab)

mean(tegu.data.ab$Intake.SVL)
sd(tegu.data.ab$Intake.SVL)

mean(tegu.data.ab$Intake.TL)
sd(tegu.data.ab$Intake.TL)

mean(tegu.data.ab$mass.g)
sd(tegu.data.ab$mass.g)

mean(tegu.data.ab$fat.g, na.rm=T)
sd(tegu.data.ab$fat.g, na.rm=T)

mean(tegu.data.ab$percfat, na.rm=T)
sd(tegu.data.ab$percfat, na.rm=T)

#split males/females
tegu.ab.male<- subset(tegu.data.ab, tegu.data.ab$Analysis.Sex == "Male")
tegu.ab.female<-subset(tegu.data.ab, tegu.data.ab$Analysis.Sex == "Female")

mean(tegu.ab.male$Intake.SVL)
sd(tegu.ab.male$Intake.SVL)
mean(tegu.ab.female$Intake.SVL)
sd(tegu.ab.female$Intake.SVL)
wilcox.test(tegu.data.ab$Intake.SVL~ tegu.data.ab$Analysis.Sex)

mean(tegu.ab.male$Intake.TL)
sd(tegu.ab.male$Intake.TL)
wilcox.test(tegu.data.ab$Intake.TL~ tegu.data.ab$Analysis.Sex)

mean(tegu.ab.female$Intake.TL)
sd(tegu.ab.female$Intake.TL)

mean(tegu.ab.male$mass.g)
sd(tegu.ab.male$mass.g)
mean(tegu.ab.female$mass.g)
sd(tegu.ab.female$mass.g)
wilcox.test(tegu.data.ab$mass.g~ tegu.data.ab$Analysis.Sex)

mean(tegu.ab.male$fat.g, na.rm=T)
sd(tegu.ab.male$fat.g, na.rm=T)
mean(tegu.ab.female$fat.g, na.rm=T)
sd(tegu.ab.female$fat.g, na.rm=T)
wilcox.test(tegu.data.ab$fat.g~ tegu.data.ab$Analysis.Sex)

mean(tegu.ab.male$percfat, na.rm=T)
sd(tegu.ab.male$percfat, na.rm=T)
mean(tegu.ab.female$percfat, na.rm=T)
sd(tegu.ab.female$percfat, na.rm=T)
wilcox.test(tegu.data.ab$percfat~ tegu.data.ab$Analysis.Sex)
##
par(mar=c(2,2,2,2))
hist(tegu.data.ab$mass.g, breaks = 100, main= "Tegu mass (g)")
shapiro.test(tegu.data.ab$mass.g) #not normal
hist(tegu.data.ab$Intake.TL, breaks = 100, main= "Tegu SVL (cm)")
shapiro.test(tegu.data.ab$Intake.SVL) #not normal
hist(tegu.data.ab$fulton, breaks=100, main="Fulton's K")
shapiro.test(tegu.data.ab$fulton) #not normal
hist(tegu.data.ab$percfat, breaks=100)
hist(tegu.data.ab$fat.g, breaks=100)
####
#### 4. T.Tests ####
#### 4a. Shapiro Test####

#all...nothing is normal
shapiro.test(tegu.data.ab$fulton)#not normal
shapiro.test(tegu.data.ab$Intake.SVL) #not normal
shapiro.test(tegu.data.ab$mass.g)
shapiro.test(tegu.data.ab$percfat)
shapiro.test(tegu.data.ab$fat.g)
shapiro.test(tegu.data.ab$C110111.DIST)
shapiro.test(tegu.data.ab$C110424.DIST)
shapiro.test(tegu.data.ab$JDC.DIST)
shapiro.test(tegu.data.ab$Pond.DIST)
shapiro.test(tegu.data.ab$UP.DIST)
shapiro.test(tegu.data.ab$WCS.DIST)
shapiro.test(tegu.data.ab$Habitat)
shapiro.test(tegu.data.ab$Avg.Temp.C)
shapiro.test(tegu.data.ab$Max.Temp.C)
shapiro.test(tegu.data.ab$Rain.cm)
shapiro.test(tegu.data.ab$Rain.Max.15.cm)
shapiro.test(tegu.data.ab$Min.Temp.C)
shapiro.test(tegu.data.ab$julian)

#Group 1
shapiro.test(Group1.ab$fulton)#not normal
shapiro.test(Group1.ab$Intake.SVL) 
shapiro.test(Group1.ab$mass.g)
shapiro.test(Group1.ab$percfat)
shapiro.test(Group1.ab$fat.g)
shapiro.test(Group1.ab$C110111.DIST)
shapiro.test(Group1.ab$C110424.DIST)
shapiro.test(Group1.ab$JDC.DIST)
shapiro.test(Group1.ab$Pond.DIST)
shapiro.test(Group1.ab$UP.DIST)
shapiro.test(Group1.ab$WCS.DIST)
shapiro.test(Group1.ab$Habitat)
shapiro.test(Group1.ab$Avg.Temp.C)
shapiro.test(Group1.ab$Max.Temp.C)
shapiro.test(Group1.ab$Rain.cm)
shapiro.test(Group1.ab$Rain.Max.15.cm)
shapiro.test(Group1.ab$Min.Temp.C)
shapiro.test(Group1.ab$julian)

#Group 2
shapiro.test(Group2.ab$fulton)#not normal
shapiro.test(Group2.ab$Intake.SVL) 
shapiro.test(Group2.ab$mass.g)
shapiro.test(Group2.ab$percfat)
shapiro.test(Group2.ab$fat.g)
shapiro.test(Group2.ab$C110111.DIST)
shapiro.test(Group2.ab$C110424.DIST)
shapiro.test(Group2.ab$JDC.DIST)
shapiro.test(Group2.ab$Pond.DIST)
shapiro.test(Group2.ab$UP.DIST)
shapiro.test(Group2.ab$WCS.DIST)
shapiro.test(Group2.ab$Habitat)
shapiro.test(Group2.ab$Avg.Temp.C)
shapiro.test(Group2.ab$Max.Temp.C)
shapiro.test(Group2.ab$Rain.cm)
shapiro.test(Group2.ab$Rain.Max.15.cm)
shapiro.test(Group2.ab$Min.Temp.C)
shapiro.test(Group2.ab$julian)

#Group 3
shapiro.test(Group3.ab$fulton)#not normal
shapiro.test(Group3.ab$Intake.SVL) 
shapiro.test(Group3.ab$mass.g)
shapiro.test(Group3.ab$percfat)
shapiro.test(Group3.ab$fat.g)
shapiro.test(Group3.ab$C110111.DIST)
shapiro.test(Group3.ab$C110424.DIST)
shapiro.test(Group3.ab$JDC.DIST)
shapiro.test(Group3.ab$Pond.DIST)
shapiro.test(Group3.ab$UP.DIST)
shapiro.test(Group3.ab$WCS.DIST)
shapiro.test(Group3.ab$Habitat)
shapiro.test(Group3.ab$Avg.Temp.C)
shapiro.test(Group3.ab$Max.Temp.C)
shapiro.test(Group3.ab$Rain.cm)
shapiro.test(Group3.ab$Rain.Max.15.cm)
shapiro.test(Group3.ab$Min.Temp.C)
shapiro.test(Group3.ab$julian)


#### 4.a.1 Box-plot ####

#all...nothing is normal
shapiro.test(tegu.data.ab$fulton)#not normal
shapiro.test(tegu.data.ab$Intake.SVL) #not normal
shapiro.test(tegu.data.ab$mass.g)
shapiro.test(tegu.data.ab$percfat)
shapiro.test(tegu.data.ab$fat.g)
shapiro.test(tegu.data.ab$C110111.DIST)
shapiro.test(tegu.data.ab$C110424.DIST)
shapiro.test(tegu.data.ab$JDC.DIST)
shapiro.test(tegu.data.ab$Pond.DIST)
shapiro.test(tegu.data.ab$UP.DIST)
shapiro.test(tegu.data.ab$WCS.DIST)
shapiro.test(tegu.data.ab$Habitat)
shapiro.test(tegu.data.ab$Avg.Temp.C)
shapiro.test(tegu.data.ab$Max.Temp.C)
shapiro.test(tegu.data.ab$Rain.cm)
shapiro.test(tegu.data.ab$Rain.Max.15.cm)
shapiro.test(tegu.data.ab$Min.Temp.C)
shapiro.test(tegu.data.ab$julian)

#Group 1
shapiro.test(Group1.ab$fulton)#not normal
shapiro.test(Group1.ab$Intake.SVL) 
shapiro.test(Group1.ab$mass.g)
shapiro.test(Group1.ab$percfat)
shapiro.test(Group1.ab$fat.g)
shapiro.test(Group1.ab$C110111.DIST)
shapiro.test(Group1.ab$C110424.DIST)
shapiro.test(Group1.ab$JDC.DIST)
shapiro.test(Group1.ab$Pond.DIST)
shapiro.test(Group1.ab$UP.DIST)
shapiro.test(Group1.ab$WCS.DIST)
shapiro.test(Group1.ab$Habitat)
shapiro.test(Group1.ab$Avg.Temp.C)
shapiro.test(Group1.ab$Max.Temp.C)
shapiro.test(Group1.ab$Rain.cm)
shapiro.test(Group1.ab$Rain.Max.15.cm)
shapiro.test(Group1.ab$Min.Temp.C)
shapiro.test(Group1.ab$julian)

#Group 2
shapiro.test(Group2.ab$fulton)#not normal
shapiro.test(Group2.ab$Intake.SVL) 
shapiro.test(Group2.ab$mass.g)
shapiro.test(Group2.ab$percfat)
shapiro.test(Group2.ab$fat.g)
shapiro.test(Group2.ab$C110111.DIST)
shapiro.test(Group2.ab$C110424.DIST)
shapiro.test(Group2.ab$JDC.DIST)
shapiro.test(Group2.ab$Pond.DIST)
shapiro.test(Group2.ab$UP.DIST)
shapiro.test(Group2.ab$WCS.DIST)
shapiro.test(Group2.ab$Habitat)
shapiro.test(Group2.ab$Avg.Temp.C)
shapiro.test(Group2.ab$Max.Temp.C)
shapiro.test(Group2.ab$Rain.cm)
shapiro.test(Group2.ab$Rain.Max.15.cm)
shapiro.test(Group2.ab$Min.Temp.C)
shapiro.test(Group2.ab$julian)

#Group 3
shapiro.test(Group3.ab$fulton)#normal
shapiro.test(Group3.ab$Intake.SVL) 
shapiro.test(Group3.ab$mass.g)
shapiro.test(Group3.ab$percfat)
shapiro.test(Group3.ab$fat.g)
shapiro.test(Group3.ab$C110111.DIST)
shapiro.test(Group3.ab$C110424.DIST)
shapiro.test(Group3.ab$JDC.DIST)
shapiro.test(Group3.ab$Pond.DIST)
shapiro.test(Group3.ab$UP.DIST)
shapiro.test(Group3.ab$WCS.DIST)
shapiro.test(Group3.ab$Habitat)
shapiro.test(Group3.ab$Avg.Temp.C)
shapiro.test(Group3.ab$Max.Temp.C)
shapiro.test(Group3.ab$Rain.cm)
shapiro.test(Group3.ab$Rain.Max.15.cm)
shapiro.test(Group3.ab$Min.Temp.C)
shapiro.test(Group3.ab$julian)


#### 4b. Fligner-Killeen test####
#all groups fulton variance by group, fulton is not normal
fligner.test(tegu.data.ab$fulton, tegu.data.ab$sizeclass)#no
fligner.test(tegu.data.ab$fulton, tegu.data.ab$Intake.SVL) #equvar
fligner.test(tegu.data.ab$fulton, tegu.data.ab$mass.g)#no
fligner.test(tegu.data.ab$fulton, tegu.data.ab$percfat)#equvar
fligner.test(tegu.data.ab$fulton, tegu.data.ab$fat.g)#equvar
fligner.test(tegu.data.ab$fulton, tegu.data.ab$C110111.DIST)#no
fligner.test(tegu.data.ab$fulton, tegu.data.ab$C110424.DIST)#no
fligner.test(tegu.data.ab$fulton, tegu.data.ab$JDC.DIST)#no
fligner.test(tegu.data.ab$fulton, tegu.data.ab$Pond.DIST)#no
fligner.test(tegu.data.ab$fulton, tegu.data.ab$UP.DIST)#no
fligner.test(tegu.data.ab$fulton, tegu.data.ab$WCS.DIST)#no
fligner.test(tegu.data.ab$fulton, tegu.data.ab$Habitat)#equvar
fligner.test(tegu.data.ab$fulton, tegu.data.ab$Avg.Temp.C)
fligner.test(tegu.data.ab$fulton, tegu.data.ab$Max.Temp.C)
fligner.test(tegu.data.ab$fulton, tegu.data.ab$Rain.cm)
fligner.test(tegu.data.ab$fulton, tegu.data.ab$Min.Temp.C)
fligner.test(tegu.data.ab$fulton, tegu.data.ab$Rain.Max.15.cm)
fligner.test(tegu.data.ab$fulton, tegu.data.ab$julian)#equvar

#group 1 - dist is significant, fulton is normal
fligner.test(Group1.ab$fulton, Group1.ab$Analysis.Sex)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$mass.g)#no
fligner.test(Group1.ab$fulton, Group1.ab$percfat, na.rm= T)
fligner.test(Group1.ab$fulton, Group1.ab$fat.g)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$C110111.DIST)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$C110424.DIST)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$JDC.DIST)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$Pond.DIST)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$UP.DIST)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$WCS.DIST)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$Habitat)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$Avg.Temp.C)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$Max.Temp.C)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$Rain.cm)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$Min.Temp.C)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$Rain.Max.15.cm)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$julian)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$year)#equvar
fligner.test(Group1.ab$fulton, Group1.ab$month.numb)#equvar

#Group 2 - dist significant, fulton is not normal
fligner.test(Group2.ab$fulton, Group2.ab$Analysis.Sex)
fligner.test(Group2.ab$fulton, Group2.ab$Intake.SVL)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$mass.g)#no
fligner.test(Group2.ab$fulton, Group2.ab$percfat)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$fat.g)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$C110111.DIST)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$C110424.DIST)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$JDC.DIST)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$Pond.DIST)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$UP.DIST)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$WCS.DIST)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$Habitat)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$Avg.Temp.C)#no
fligner.test(Group2.ab$fulton, Group2.ab$Max.Temp.C)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$Rain.cm)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$Min.Temp.C)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$Rain.Max.15.cm)#no
fligner.test(Group2.ab$fulton, Group2.ab$julian)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$year)#equvar
fligner.test(Group2.ab$fulton, Group2.ab$month.numb)#equvar

#group 3 -  fulton is normal
fligner.test(Group3.ab$fulton, Group3.ab$Analysis.Sex)
fligner.test(Group3.ab$fulton, Group3.ab$Max.Temp.C)
fligner.test(Group3.ab$fulton, Group3.ab$Rain.cm)
fligner.test(Group3.ab$fulton, Group3.ab$Min.Temp.C)
fligner.test(Group3.ab$fulton, Group3.ab$Rain.Max.15.cm)
fligner.test(Group3.ab$fulton, Group3.ab$mass.g)
fligner.test(Group3.ab$fulton, Group3.ab$Intake.SVL)
fligner.test(Group3.ab$fulton, Group3.ab$percfat)#****
fligner.test(Group3.ab$fulton, Group3.ab$fat.g) #***
fligner.test(Group3.ab$fulton, Group3.ab$C110111.DIST)
fligner.test(Group3.ab$fulton, Group3.ab$C110424.DIST)
fligner.test(Group3.ab$fulton, Group3.ab$JDC.DIST)
fligner.test(Group3.ab$fulton, Group3.ab$Pond.DIST)
fligner.test(Group3.ab$fulton, Group3.ab$UP.DIST)
fligner.test(Group3.ab$fulton, Group3.ab$WCS.DIST)
fligner.test(Group3.ab$fulton, Group3.ab$Habitat)
fligner.test(Group3.ab$fulton, Group3.ab$julian) 
fligner.test(Group3.ab$fulton, Group3.ab$year)
fligner.test(Group3.ab$fulton, Group3.ab$month.numb)


#### 4c. Wilcoxn rank sum test ####

#all tegu differences between Sex
wilcox.test(tegu.data.ab$fulton~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$Intake.SVL~tegu.data.ab$Analysis.Sex )
wilcox.test(tegu.data.ab$mass.g~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$percfat~tegu.data.ab$Analysis.Sex) #sig diff
wilcox.test(tegu.data.ab$fat.g~tegu.data.ab$Analysis.Sex) #sig diff
wilcox.test(tegu.data.ab$C110111.DIST~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$C110424.DIST~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$JDC.DIST~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$Pond.DIST~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$UP.DIST~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$WCS.DIST~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$Habitat~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$Avg.Temp.C~tegu.data.ab$Analysis.Sex)#sig diff
wilcox.test(tegu.data.ab$Max.Temp.C~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$Rain.cm~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$Min.Temp.C~tegu.data.ab$Analysis.Sex)#sig diff
wilcox.test(tegu.data.ab$Rain.Max.15.cm~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$julian~tegu.data.ab$Analysis.Sex) #sig diff

#Group 1 differences between sex
wilcox.test(Group1.ab$fulton~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$Intake.SVL~Group1.ab$Analysis.Sex )
wilcox.test(Group1.ab$mass.g~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$percfat~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$fat.g~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$C110111.DIST~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$C110424.DIST~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$JDC.DIST~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$Pond.DIST~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$UP.DIST~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$WCS.DIST~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$Habitat~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$Avg.Temp.C~Group1.ab$Analysis.Sex)#sig
wilcox.test(Group1.ab$Max.Temp.C~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$Rain.cm~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$Min.Temp.C~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$Rain.Max.15.cm~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$julian~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$year~Group1.ab$Analysis.Sex)
wilcox.test(Group1.ab$month.numb~Group1.ab$Analysis.Sex)


#group 2 differences between sex
wilcox.test(Group2.ab$fulton~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$Intake.SVL~Group2.ab$Analysis.Sex ) 
wilcox.test(Group2.ab$mass.g~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$percfat~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$fat.g~Group2.ab$Analysis.Sex) #sig
wilcox.test(Group2.ab$C110111.DIST~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$C110424.DIST~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$JDC.DIST~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$Pond.DIST~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$UP.DIST~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$WCS.DIST~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$Habitat~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$Avg.Temp.C~Group2.ab$Analysis.Sex)#sig
wilcox.test(Group2.ab$Max.Temp.C~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$Rain.cm~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$Min.Temp.C~Group2.ab$Analysis.Sex) #sig
wilcox.test(Group2.ab$Rain.Max.15.cm~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$julian~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$year~Group2.ab$Analysis.Sex)
wilcox.test(Group2.ab$month.numb~Group2.ab$Analysis.Sex)

#group 3 differences between sex
wilcox.test(Group3.ab$fulton~Group3.ab$Analysis.Sex)
#wilcox.test(Group3.ab$Intake.SVL~Group3.ab$Analysis.Sex )
#wilcox.test(Group3.ab$mass.g~Group3.ab$Analysis.Sex)
#wilcox.test(Group3.ab$percfat~Group3.ab$Analysis.Sex) #sig diff
#wilcox.test(Group3.ab$fat.g~Group3.ab$Analysis.Sex) #sig diff
#wilcox.test(Group3.ab$C110111.DIST~Group3.ab$Analysis.Sex)
wilcox.test(Group3.ab$C110424.DIST~Group3.ab$Analysis.Sex)
wilcox.test(Group3.ab$JDC.DIST~Group3.ab$Analysis.Sex)
wilcox.test(Group3.ab$Pond.DIST~Group3.ab$Analysis.Sex)
wilcox.test(Group3.ab$UP.DIST~Group3.ab$Analysis.Sex)
#wilcox.test(Group3.ab$WCS.DIST~Group3.ab$Analysis.Sex)
#wilcox.test(Group3.ab$Habitat~Group3.ab$Analysis.Sex)
wilcox.test(Group3.ab$Avg.Temp.C~Group3.ab$Analysis.Sex)
wilcox.test(Group3.ab$Max.Temp.C~Group3.ab$Analysis.Sex)
wilcox.test(Group3.ab$Rain.cm~Group3.ab$Analysis.Sex)
wilcox.test(Group3.ab$Min.Temp.C~Group3.ab$Analysis.Sex) #sig diff
wilcox.test(Group3.ab$Rain.Max.15.cm~Group3.ab$Analysis.Sex)
#wilcox.test(Group3.ab$julian~Group3.ab$Analysis.Sex)
wilcox.test(Group3.ab$year~Group3.ab$Analysis.Sex)
#wilcox.test(Group3.ab$month.numb~Group3.ab$Analysis.Sex)


#### Kruskal wallis test ####

#do krushkal walis test on fulton's k vs. sizeclass; all levels
kruskal.test(tegu.data.ab$fulton, tegu.data.ab$sizeclass)
dunnTest(tegu.data.ab$fulton, tegu.data.ab$sizeclass, method = 'bonferroni')

#do krushkal walis test on SVL vs. sizeclass/group; all levels
kruskal.test(tegu.data.ab$Intake.SVL, tegu.data.ab$sizeclass)
dunnTest(tegu.data.ab$Intake.SVL, tegu.data.ab$sizeclass, method = 'bonferroni')


#do krushkal walis test on TL vs. sizeclass/group; all levels
kruskal.test(tegu.data.ab$Intake.TL, tegu.data.ab$sizeclass)
dunnTest(tegu.data.ab$Intake.TL, tegu.data.ab$sizeclass, method = 'bonferroni')

#do krushkal walis test on mass vs. sizeclass; all levels
kruskal.test(tegu.data.ab$mass.g, tegu.data.ab$sizeclass)
dunnTest(tegu.data.ab$mass.g, tegu.data.ab$sizeclass, method = 'bonferroni')


#do krushkal walis test on percent fat vs. sizeclass
kruskal.test(tegu.data.ab$percfat, tegu.data.ab$sizeclass)
dunnTest(tegu.data.ab$percfat, tegu.data.ab$sizeclass, method = 'bonferroni')
#significantly different between 1 & 3*, 2 & 3*

#do krushkal walis test on fat vs. sizeclass; all levels
kruskal.test(tegu.data.ab$fat.g, tegu.data.ab$sizeclass)
dunnTest(tegu.data.ab$fat.g, tegu.data.ab$sizeclass, method = 'bonferroni')

#do krushkal walis test on c110111 dist vs. sizeclass
kruskal.test(tegu.data.ab$C110111.DIST, tegu.data.ab$sizeclass)
dunnTest(tegu.data.ab$C110111.DIST, tegu.data.ab$sizeclass, method = 'bonferroni')
# 1 & 2

#do krushkal walis test on c110424 vs. sizeclass
kruskal.test(tegu.data$C110424.DIST, tegu.data$sizeclass)
dunnTest(tegu.data$C110424.DIST, tegu.data$sizeclass, method = 'bonferroni')
#none

#do krushkal walis test on JDC vs. sizeclass
kruskal.test(tegu.data$JDC.DIST, tegu.data$sizeclass)
dunnTest(tegu.data$JDC.DIST, tegu.data$sizeclass, method = 'bonferroni')
#2 & 3

#do krushkal walis test on pond vs. sizeclass
kruskal.test(tegu.data$Pond.DIST, tegu.data$sizeclass)
dunnTest(tegu.data$Pond.DIST, tegu.data$sizeclass, method = 'bonferroni')
#2&3

#do krushkal walis test on UP vs. sizeclass
kruskal.test(tegu.data$UP.DIST, tegu.data$sizeclass)
dunnTest(tegu.data$UP.DIST, tegu.data$sizeclass, method = 'bonferroni')
#All

#do krushkal walis test on WCS dist vs. sizeclass
kruskal.test(tegu.data$WCS.DIST, tegu.data$sizeclass)
dunnTest(tegu.data$WCS.DIST, tegu.data$sizeclass, method = 'bonferroni')
#1&2

#do krushkal walis test on avg temp vs. sizeclass; all levels
kruskal.test(tegu.data$Avg.Temp.C, tegu.data$sizeclass)
dunnTest(tegu.data$Avg.Temp.C, tegu.data$sizeclass, method = 'bonferroni')


#do krushkal walis test on max temp vs. sizeclass; all levels
kruskal.test(tegu.data$Max.Temp.C, tegu.data$sizeclass)
dunnTest(tegu.data$Max.Temp.C, tegu.data$sizeclass, method = 'bonferroni')


#do krushkal walis test on rain vs. sizeclass; all levels
kruskal.test(tegu.data$Rain.cm, tegu.data$sizeclass)
dunnTest(tegu.data$Rain.cm, tegu.data$sizeclass, method = 'bonferroni')


#do krushkal walis test on min temp vs. sizeclass; all levels
kruskal.test(tegu.data$Min.Temp.C, tegu.data$sizeclass)
dunnTest(tegu.data$Min.Temp.C, tegu.data$sizeclass, method = 'bonferroni')

#do krushkal walis test on rain max vs. sizeclass; all levels
kruskal.test(tegu.data$Rain.Max.15.cm, tegu.data$sizeclass)
dunnTest(tegu.data$Rain.Max.15.cm, tegu.data$sizeclass, method = 'bonferroni')

#do krushkal walis test on Julian vs. sizeclass; all levels
kruskal.test(tegu.data$julian, tegu.data$sizeclass)
dunnTest(tegu.data$julian, tegu.data$sizeclass, method = 'bonferroni')

####
#### 5. Averages ####
# males and females
summary(tegu.data.ab)
males.ab<-subset (tegu.data.ab, tegu.data.ab$Analysis.Sex=="Male")
females.ab<-subset (tegu.data.ab, tegu.data.ab$Analysis.Sex=="Female")
#variability
var(tegu.data.ab$fulton)

summary(tegu.data.ab$Intake.SVL)
summary(tegu.data.ab$mass.g)
summary(tegu.data.ab$fulton)
summary(tegu.data.ab$percfat)
summary(tegu.data.ab$fat.g)


mean(Group3.ab$fulton) #3.16
summary(adults$fulton) #2.26-3.93
sd(Group3.ab$fulton)#0.31
mean(Group3.ab$Intake.SVL) #30.04 cm
sd(adults$Intake.SVL)#3.11
mean(Group3.ab$mass.g)#888.80
sd(adults$mass.g)#337.66
mean(Group3.ab$fat.g, na.rm=T) #14.48
sd(adults$fat.g, na.rm=T) #18.81
mean(adults$percfat, na.rm=T)#1.49
sd(adults$percfat, na.rm=T) #1.68

#native
adults2<-merge(Group4.N2, Group5.N2, all= TRUE)

mean(adults2$fulton) #3.17
summary(adults2$fulton) #2.01-4.41
sd(adults2$fulton)#0.42
mean(adults2$SVL) #38.28 cm
sd(adults2$SVL)#3.77
mean(adults2$mass.g)#1840.77
sd(adults2$mass.g)#616.59
mean(adults2$FatBody, na.rm=T) #55.19
sd(adults2$FatBody, na.rm=T) #50.21
mean(adults2$percfat, na.rm=T)#2.83
sd(adults2$percfat, na.rm=T) #2.28

summary(lm(lmass~lsvl, data=adults)) #3.13, 0.05
summary(lm(lmass~lsvl, data=adults2)) #3.27, 0.05
####
mean()
sd(adults$percfat[!is.na(adults$percfat)])
sd(adults$Intake.SVL, na.rm= T)

#all tegus
mean(tegu.data.ab$fulton) #3.06
summary(tegu.data.ab$fulton)
sd(tegu.data.ab$fulton)#0.33
mean(tegu.data.ab$Intake.SVL) #23.66 cm
length(tegu.data.ab$fat.g[!is.na(tegu.data.ab$fat.g)])
length(tegu.data.ab$percfat[!is.na(tegu.data.ab$percfat)])
sd(tegu.data.ab$Intake.SVL, na.rm= T)

mean(tegu.data.ab$Intake.SVL)
sd(tegu.data.ab$Intake.SVL)

mean(tegu.data.ab$mass.g)
sd(tegu.data.ab$mass.g)

#male vs female svl/tl/mass
wilcox.test(tegu.data.ab$Intake.SVL~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$Intake.TL~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$mass.g~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$percfat~tegu.data.ab$Analysis.Sex)
wilcox.test(tegu.data.ab$fat.g~tegu.data.ab$Analysis.Sex)

#male and female average fat
mean(tegu.ab.female$fat.g, na.rm = T)
sd(tegu.ab.female$fat.g, na.rm = T)

mean(tegu.ab.female$percfat, na.rm = T)
sd(tegu.ab.female$percfat, na.rm = T)

mean(tegu.ab.male$fat.g, na.rm = T)
sd(tegu.ab.male$fat.g, na.rm = T)

mean(tegu.ab.male$percfat, na.rm = T)
sd(tegu.ab.male$percfat, na.rm = T)

#summary of stats
summary(tegu.data.ab)

#average fat mass and SD
sd(Group1.ab$fat.g, na.rm=T) #0.8897111
mean(Group1.ab$fat.g, na.rm=T) #0.6131017

sd(Group2.ab$fat.g, na.rm=T) #3.652193
mean(Group2.ab$fat.g, na.rm=T) #2.50095

sd(Group3.ab$fat.g, na.rm=T) #6.821539
mean(Group3.ab$fat.g, na.rm=T) #5.366864

#average Fulton's K
sd(Group1.ab$fulton, na.rm=T) #0.3735293
mean(Group1.ab$fulton, na.rm=T) #2.93436

sd(Group2.ab$fulton, na.rm=T) # 0.3337599
mean(Group2.ab$fulton, na.rm=T) #2.997839

sd(Group3.ab$fulton, na.rm=T) #0.3027526
mean(Group3.ab$fulton, na.rm=T) #3.084418


#percent fat
sd(Group1.ab$percfat, na.rm=T)#0.9331748%
mean(Group1.ab$percfat, na.rm=T) #0.6567649%

sd(Group2.ab$percfat, na.rm=T) # 1.395278%
mean(Group2.ab$percfat, na.rm=T) #1.015209%

sd(Group3.ab$percfat, na.rm=T) # 1.300657%
mean(Group3.ab$percfat, na.rm=T) #1.123993%

#weight

sd(Group1.ab$mass.g, na.rm=T) #0.3735293
mean(Group1.ab$mass.g, na.rm=T) #2.93436

sd(Group2.ab$mass.g, na.rm=T) # 0.3337599
mean(Group2.ab$mass.g, na.rm=T) #2.997839

sd(Group3.ab$mass.g, na.rm=T) #0.3027526
mean(Group3.ab$mass.g, na.rm=T) #3.084418

#svl

sd(Group1.ab$Intake.SVL, na.rm=T) #0.3735293
mean(Group1.ab$Intake.SVL, na.rm=T) 

sd(Group2.ab$Intake.SVL, na.rm=T) # 0.3337599
mean(Group2.ab$Intake.SVL, na.rm=T) #2.997839

sd(Group3.ab$Intake.SVL, na.rm=T) #0.3027526
mean(Group3.ab$Intake.SVL, na.rm=T) #3.084418

####


#### Native vs. invasive range ####
Native<-read.csv("C:/Users/jcole1/Downloads/R_Project_Tegu/R_Project_Tegu/NativeTeguData.csv")
merianae<-subset(Native, Native$Species %in% "merianae")

merianae$mass.g <- with(merianae, BM*1000)##body mass in grams
merianae$fulton <- with(merianae, (mass.g/SVL^3)*10^2)
merianae$percfat<-with(merianae, (FatBody/mass.g)*100)
merianae$lsvl<-log(merianae$SVL)
merianae$ltl<-log(merianae$TL)
merianae$lmass<-log(merianae$mass.g)

merianae$Date.1 <- as.Date(merianae$Date, "%m/%d/%Y")

#add column for month and year
merianae<-merianae %>%
  dplyr::mutate(year = lubridate::year(merianae$Date.1), 
                month.numb = lubridate::month(merianae$Date.1), 
                day = lubridate::yday(merianae$Date.1))

#convert month.num to name
merianae$month<-month.abb[merianae$month.numb]
merianae$month<-factor(merianae$month, levels=month.abb)

#split into groups
merianae$sizeclass <- factor(ifelse(merianae$SVL <=20.2, "Group1", 
                                    ifelse(merianae$SVL>=30.0, "Group3","Group2")))

#create group
Group1.N<-subset(merianae, merianae$sizeclass=="Group1")
Group2.N<-subset(merianae, merianae$sizeclass== "Group2")
Group3.N<-subset(merianae, merianae$sizeclass== "Group3")

#male and female
males.N3<-subset (Group3.N2, Group3.N2$Sex=="male")
females.N3<-subset (Group3.N2, Group3.N2$Sex=="female")

#with outliers
#mean fulton and SD per group
mean(merianae$fulton, na.rm=T) #3.189
sd(merianae$fulton, na.rm = T)#0.583

#group 1, n = 3
mean(Group1.N$fulton, na.rm=T) #4.908
sd(Group1.N$fulton, na.rm=T) #3.829

#group 2, n = 22
mean(Group2.N$fulton, na.rm=T) #3.382
sd(Group2.N$fulton, na.rm=T) #1.654

#group 3, n = 670
mean(Group3.N$fulton, na.rm=T) #3.175
sd(Group3.N$fulton, na.rm=T) #0.462

#remove outliers
#IQR for outliers in fulton
iqr<-IQR(merianae$fulton, na.rm=T)

lowerq=quantile(merianae$fulton, na.rm=T)[2]
upperq=quantile(merianae$fulton, na.rm=T)[4]

mild.threshold.upper = (iqr * 1.5) + upperq
mild.threshold.lower = lowerq - (iqr * 1.5)
mild.threshold.lower #1.931
mild.threshold.upper #4.4347

#remove outliers
merianae2<-merianae[which(merianae$fulton<4.434758 & merianae$fulton>1.931379),]

#create groups 2
Group1.N2<-subset(merianae2, merianae2$sizeclass=="Group1")
Group2.N2<-subset(merianae2, merianae2$sizeclass== "Group2")
Group3.N2<-subset(merianae2, merianae2$sizeclass== "Group3")

#check amle and female
Group1.male<-subset(Group1.ab, Group1.ab$Analysis.Sex=="Male")
Group2.male<-subset(Group2.ab, Group2.ab$Analysis.Sex== "Male")
Group3.male<-subset(Group3.ab, Group3.ab$Analysis.Sex== "Male")

Group1.female<-subset(Group1.ab, Group1.ab$Analysis.Sex=="Female")
Group2.female<-subset(Group2.ab, Group2.ab$Analysis.Sex== "Female")
Group3.female<-subset(Group3.ab, Group3.ab$Analysis.Sex== "Female")

#check groups
#all, n =673
mean(merianae2$SVL,na.rm=T) #38.21
mean(merianae2$fulton, na.rm=T) #3.168
mean(tegu.data.ab$fulton, na.rm=T) #3.06
sd(merianae2$fulton, na.rm = T)#0.418

#group 1, n = 2
mean(Group1.N2$SVL,na.rm=T) #19.5
sd(Group1.N2$SVL, na.rm=T) #0.103
mean(Group1.N2$fulton, na.rm=T) #2.697
sd(Group1.N2$fulton, na.rm=T) #0.103
mean(Group1.N2$mass.g, na.rm=T) #2.697
sd(Group1.N2$mass.g, na.rm=T) #0.103
mean(Group1.N2$FatBody, na.rm=T) #2.697
sd(Group1.N2$FatBody, na.rm=T) #0.103
summary(Group2.N2$percfat)
sd(Group2.N2$percfat, na.rm=T)

#group 2, n = 18
mean(Group2.N2$SVL,na.rm=T) #28.027
sd(Group2.N2$SVL, na.rm=T)
mean(Group2.N2$fulton, na.rm=T) #2.94
sd(Group2.N2$fulton, na.rm=T) #0.392
mean(Group2.N2$mass.g, na.rm=T) #2.697
sd(Group2.N2$mass.g, na.rm=T) #0.103
mean(Group2.N2$FatBody, na.rm=T) #2.697
sd(Group2.N2$FatBody, na.rm=T) #0.103

#group 3, n = 653
mean(Group3.N2$SVL,na.rm=T) #38.548
sd(Group3.N2$SVL, na.rm=T)
mean(Group3.N2$fulton, na.rm=T) #3.17
sd(Group3.N2$fulton, na.rm=T) #0.417
mean(Group3.N2$mass.g, na.rm=T) #2.697
sd(Group3.N2$mass.g, na.rm=T) #0.103
mean(Group3.N2$FatBody, na.rm=T) #2.697
sd(Group3.N2$FatBody, na.rm=T) #0.103

summary(Group3.N2$FatBody)
summary(Group3.N2$fulton)

summary(Group2.N2$FatBody)
summary(Group2.N2$fulton)

#Invasive

#average Fulton's K, n = 1634
mean(tegu.data.ab$Intake.SVL) #23.662
mean(tegu.data.ab$fulton) #3.06
sd(tegu.data.ab$fulton) #0.33

#group 1, n = 403
mean(Group1.ab$Intake.SVL) #16.97
mean(Group1.ab$fulton, na.rm=T) #2.943
sd(Group1.ab$fulton, na.rm=T) #0.346

#group 2, n = 1081
mean(Group2.ab$Intake.SVL)#24.798
mean(Group2.ab$fulton, na.rm=T) #3.087
sd(Group2.ab$fulton, na.rm=T) # 0.3162

#group 3, n = 150
mean(Group3.ab$Intake.SVL) #33.43
mean(Group3.ab$fulton, na.rm=T) #3.215
sd(Group3.ab$fulton, na.rm=T) #0.285

#only large adults and early adults
tegu.data.adults<-subset(tegu.data.ab, tegu.data.ab$Intake.SVL> 26.7)
merianae.adults<-subset(merianae2, merianae2$SVL>26.7)

wilcox.test(tegu.data.adults$Intake.SVL, merianae.adults$SVL)

mean(tegu.data.adults$percfat, na.rm=T)
sd(tegu.data.adults$percfat, na.rm=T)
merianae.adults$percfat<- (merianae.adults$FatBody/merianae.adults$mass.g)*100
mean(merianae.adults$percfat, na.rm=T)
sd(merianae.adults$percfat, na.rm=T)

mean(tegu.data.adults$mass.g)
sd(tegu.data.adults$mass.g)

mean(merianae.adults$mass.g)
sd(merianae.adults$mass.g)

#check slope
merianae$lsvl<-log(merianae$SVL)
merianae$ltl<-log(merianae$TL)
merianae$lmass<-log(merianae$mass.g)

summary(lm(lmass~lsvl, data=tegu.data.adults))
#check slope
summary(lm(lmass~lsvl, data=merianae.adults)) #yes
summary(lm(lmass~lsvl, data=Group2.N2)) #no
summary(lm(lmass~lsvl, data=Group3.N2)) #no
summary(lm(lmass~lsvl, data=Group1.N2)) #no
summary(lm(lmass~lsvl, data=Group5)) #no

##Invasive 1
summary(Group1.ab$Intake.SVL)
summary(Group1.ab$mass.g)
summary(Group1.ab$fulton)
summary(Group1.ab$percfat)
summary(Group1.ab$fat.g)

##Invasive 2
summary(Group2.ab$Intake.SVL)
summary(Group2.ab$mass.g)
summary(Group2.ab$fulton)
summary(Group2.ab$percfat)
summary(Group2.ab$fat.g)
summary(Group2.ab$Analysis.Sex)

##Invasive 3
summary(Group3.ab$Intake.SVL)
summary(Group3.ab$mass.g)
summary(Group3.ab$fulton)
summary(Group3.ab$percfat)
summary(Group3.ab$fat.g)

#slope
summary(lm(lmass~lsvl, data=Group2.ab)) #no
summary(lm(lmass~lsvl, data=Group3.ab)) #no
summary(lm(lmass~lsvl, data=Group1.ab))


####
#### 6.Plotting Group 1####
#### fulton by month####
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

Group1.abmonth.tab<-with(Group1.ab, aggregate(fulton, list(month), mean))

names(Group1.abmonth.tab)<-c("month", "fulton.mean")

Group1.abmonth.tab$fulton.sd<-with(Group1.ab[!is.na(Group1.ab$fulton),], aggregate(fulton, list(month), sd))[,2]

Group1.abmonth.tab$fulton.se<-with(Group1.ab[!is.na(Group1.ab$fulton),], aggregate(fulton, list(month), se))[,2]

Group1.abmonth.tab$month <- factor(Group1.abmonth.tab$month, levels = month.abb)

#View(Group1.abmonth.tab)
#by month
#Group1.ab - Plotting fulton's K by month
G1fulton.month<-ggplot(Group1.abmonth.tab, aes(month, fulton.mean))

##Group1.ab - Plot with fulton mean and sd by month
G1fulton<-G1fulton.month+geom_errorbar(aes(ymin=fulton.mean-fulton.sd , ymax=fulton.mean+fulton.sd))+
  geom_point(size=6)+
  #facet_wrap(~Sex)+
  ggtitle("")
G1fulton

#SVL
G1.apr<-subset(Group1.ab, Group1.ab$month=="Apr")
mean(G1.apr$Intake.SVL)
mean(G1.apr$mass.g)
mean(G1.apr$fulton)
G1.may<-subset(Group1.ab, Group1.ab$month=="May")
mean(G1.may$Intake.SVL)
mean(G1.may$mass.g)
mean(G1.may$fulton)
G1.jun<-subset(Group1.ab, Group1.ab$month=="Jun")
mean(G1.jun$Intake.SVL)
mean(G1.jun$mass.g)
mean(G1.jun$fulton)
G1.jul<-subset(Group1.ab, Group1.ab$month=="Jul")
mean(G1.jul$Intake.SVL)
mean(G1.jul$mass.g)
mean(G1.jul$fulton)
G1.aug<-subset(Group1.ab, Group1.ab$month=="Aug")
mean(G1.aug$Intake.SVL)
mean(G1.aug$mass.g)
mean(G1.aug$fulton)
G1.Sep<-subset(Group1.ab, Group1.ab$month=="Sep")
mean(G1.Sep$Intake.SVL)
mean(G1.Sep$mass.g)
mean(G1.Sep$fulton)
G1.oct<-subset(Group1.ab, Group1.ab$month=="Oct")
mean(G1.oct$Intake.SVL)
mean(G1.oct$mass.g)
mean(G1.oct$fulton)

#### fulton by year G1####
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

Group1.year.tab<-with(Group1.ab, aggregate(fulton, list(year), mean))

names(Group1.year.tab)<-c("year", "fulton.mean")

Group1.year.tab$fulton.sd<-with(Group1.ab[!is.na(Group1.ab$fulton),], aggregate(fulton, list(year), sd))[,2]

Group1.year.tab$fulton.se<-with(Group1.ab[!is.na(Group1.ab$fulton),], aggregate(fulton, list(year), se))[,2]

Group1.year.tab$year <- factor(Group1.abmonth.tab$year)

View(Group1.year.tab)
#by month
#Group1.ab - Plotting fulton's K by month
G1fulton.year<-ggplot(Group1.year.tab, aes(year, fulton.mean))

##Group1.ab - Plot with fulton mean and sd by month
G1y.fulton<-G1fulton.year+geom_errorbar(aes(ymin=fulton.mean-fulton.sd , ymax=fulton.mean+fulton.sd))+
  geom_point(size=6)+
  #facet_wrap(~Sex)+
  ggtitle("Group 1 Fulton's K by Year with +-SD")
G1y.fulton

####
#### SVL by month ####
G1svl.tab<-with(Group1.ab, aggregate(Intake.SVL, list(month.numb), mean)) #getting mean values

names(G1svl.tab)<-c("month", "svl.mean")

G1svl.tab$sd<-with(Group1.ab, aggregate(Intake.SVL, list(month.numb), sd))[,2] #getting sd 

##also plot with SE, here is the function for calculating SE
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

G1svl.tab$se<-with(Group1.ab, aggregate(Intake.SVL, list(month.numb), se))[,2] #getting SE 

#G1svl.tab$month <- factor(G1svl.tab$month, levels = month.ab)

##Plotting using ggplot2
G1svl.g<-ggplot(G1svl.tab, aes(x=month,y=svl.mean))#this establishes the ggplot

##Plot svl by month and Sex using mean +- SD
G1svl<-G1svl.g+geom_errorbar(aes(ymin=svl.mean-sd, ymax=svl.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  #facet_wrap(~Sex)+##makes separate graphs for Sexes
  ggtitle("")
G1svl



####
#### Mass by month####
##Plotting mass mean +-SD and  +-SE by size class and Sex
G1mass.tab<-with(Group1.ab[!is.na(Group1.ab$mass.g),], aggregate(mass.g, list(month), mean))##remove NA's and get mean

names(G1mass.tab)<-c("month","mass.mean")

G1mass.tab$sd<-with(Group1.ab[!is.na(Group1.ab$mass.g),], aggregate(mass.g, list(month), sd))[,2]##remove NA's and get sd

G1mass.tab$se<-with(Group1.ab, aggregate(mass.g, list(month), se))[,2]##function removes any NA's 

G1mass.tab$month <- factor(G1mass.tab$month, levels = month.abb)
#View(G1mass.tab)

##Plotting using ggplot2
G1mass.g2<-ggplot(G1mass.tab, aes(x=month,y=mass.mean))#this establishes the ggplot

##Plot mass by size class and Sex using mean +- SD
G1mass<-G1mass.g2+geom_errorbar(aes(ymin=mass.mean-sd, ymax=mass.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  #facet_wrap(~Sex)+##makes seperate graphs for Sexes
  ggtitle("")
G1mass

####
#### Fat mass by month ####

##Plotting fat mass mean +-SD and  +-SE by size class and Sex
G1fmass.tab<-with(Group1.ab[!is.na(Group1.ab$fat.g),], aggregate(fat.g, list(month), mean))##remove NA's and get mean

names(G1fmass.tab)<-c("month","fat.mean")

G1fmass.tab$sd<-with(Group1.ab[!is.na(Group1.ab$fat.g),], aggregate(fat.g, list(month), sd))[,2]##remove NA's and get sd

G1fmass.tab$se<-with(Group1.ab, aggregate(fat.g, list(month), se))[,2]##function removes any NA's 

G1fmass.tab$month <- factor(G1fmass.tab$month, levels = month.abb)

##Plotting using ggplot2
G1fmass.g2<-ggplot(G1fmass.tab, aes(x=month,y=fat.mean))#this establishes the ggplot

##Plot fat mass by size class and Sex using mean +- SD
G1fmass<-G1fmass.g2+geom_errorbar(aes(ymin=fat.mean-sd, ymax=fat.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  #facet_wrap(~Sex)+##makes seperate graphs for Sexes
  ggtitle("")
G1fmass

####

#### Percent fat by month####
##Plotting fat mass mean +-SD and  +-SE by size class and Sex
#significantly diff by sex must plot separately
G1perc.tab<-with(Group1.ab[!is.na(Group1.ab$percfat),], aggregate(percfat,list(month), mean))##remove NA's and get mean

names(G1perc.tab)<-c("month", "percfat.mean")
#View(G1perc.tab)

G1perc.tab$sd<-with(Group1.ab[!is.na(Group1.ab$percfat),], aggregate(percfat, list(month), sd))[,2]##remove NA's and get sd

G1perc.tab$se<-with(Group1.ab[!is.na(Group1.ab$percfat),], aggregate(percfat,list(month), se))[,2]##function removes any NA's 

G1perc.tab$month <- factor(G1perc.tab$month, levels = month.abb)

##Plotting using ggplot2
G1perc.g2<-ggplot(G1perc.tab, aes(x=month,y=percfat.mean))#this establishes the ggplot

##Plot percfat by size class and Sex using mean +- SD
G1perc<-G1perc.g2+geom_errorbar(aes(ymin=percfat.mean-sd, ymax=percfat.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+
  #facet_wrap(~sex)+##makes separate graphs for Sexes
  ggtitle("")
G1perc


#### Group 1 Plots ####
Group1.plots<-ggarrange(G1fulton, G1svl, G1mass, G1fmass, G1perc, 
                        labels = c(),
                        ncol = 1, nrow = 5)
Group1.plots
####



#### 7.Plotting Group 2####
#### Fulton ####
Group2.abmonth.tab<-with(Group2.ab, aggregate(fulton, list(month), mean))

names(Group2.abmonth.tab)<-c("month", "fulton.mean")

Group2.abmonth.tab$fulton.sd<-with(Group2.ab[!is.na(Group2.ab$fulton),], aggregate(fulton, list(month), sd))[,2]

Group2.abmonth.tab$fulton.se<-with(Group2.ab[!is.na(Group2.ab$fulton),], aggregate(fulton, list(month), se))[,2]

Group2.abmonth.tab$month <- factor(Group2.abmonth.tab$month, levels = month.abb)

#Group2.ab - Plotting fulton's K by month and Sex
G2fulton.month<-ggplot(Group2.abmonth.tab, aes(month, fulton.mean))

##Group2.ab - Plot with fulton mean and sd by month and Sex
G2fulton<-G2fulton.month+geom_errorbar(aes(ymin=fulton.mean-fulton.sd , ymax=fulton.mean+fulton.sd))+
  geom_point(size=6)+
  #facet_wrap(~Sex)+
  ggtitle("")
G2fulton

####
####plot avg bc by year G2####
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

Group2.year.tab<-with(Group2.ab, aggregate(fulton, list(year), mean))

names(Group2.year.tab)<-c("year", "fulton.mean")

Group2.year.tab$fulton.sd<-with(Group2.ab[!is.na(Group2.ab$fulton),], aggregate(fulton, list(year), sd))[,2]

Group2.year.tab$fulton.se<-with(Group2.ab[!is.na(Group2.ab$fulton),], aggregate(fulton, list(year), se))[,2]

Group2.year.tab$year <- factor(Group2.abmonth.tab$year)

View(Group2.year.tab)
#by month
#Group2.ab - Plotting fulton's K by month
G2fulton.year<-ggplot(Group2.year.tab, aes(year, fulton.mean))

##Group2.ab - Plot with fulton mean and sd by month
G2y.fulton<-G2fulton.year+geom_errorbar(aes(ymin=fulton.mean-fulton.sd , ymax=fulton.mean+fulton.sd))+
  geom_point(size=6)+
  #facet_wrap(~Sex)+
  ggtitle("")
G2y.fulton
####
#### SVL####
G2svl.tab<-with(Group2.ab, aggregate(Intake.SVL, list(month, Analysis.Sex), mean)) #getting mean values

names(G2svl.tab)<-c("month","Sex","svl.mean")

G2svl.tab$sd<-with(Group2.ab, aggregate(Intake.SVL, list(month, Analysis.Sex), sd))[,3] #getting sd 

G2svl.tab$month <- factor(G2svl.tab$month, levels = month.abb)

##also plot with SE, here is the function for calculating SE
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

#G2svl.tab$se<-with(Group2.ab, aggregate(Intake.SVL, list(month), se))[,2] #getting SE 

##Plotting using ggplot2
G2svl.g<-ggplot(G2svl.tab, aes(x=month,y=svl.mean))#this establishes the ggplot
##Plot svl by month and Sex using mean +- SD
G2svl<-G2svl.g+geom_errorbar(aes(ymin=svl.mean-sd, ymax=svl.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  facet_wrap(~Sex)+##makes separate graphs for Sexes
  ggtitle("")
G2svl

##withoutsplit
G2svl.tab2<-with(Group2.ab, aggregate(Intake.SVL, list(month), mean)) #getting mean values

names(G2svl.tab2)<-c("month","svl.mean")

G2svl.tab2$sd<-with(Group2.ab, aggregate(Intake.SVL, list(month), sd))[,2] #getting sd 

G2svl.tab2$month <- factor(G2svl.tab2$month, levels = month.abb)

##also plot with SE, here is the function for calculating SE
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

G2svl.tab$se<-with(Group2.ab, aggregate(Intake.SVL, list(month), se))[,2] #getting SE 

##Plotting using ggplot2
G2svl.g2<-ggplot(G2svl.tab2, aes(x=month,y=svl.mean) + 
                   geom_)#this establishes the ggplot
##Plot svl by month and Sex using mean +- SD
G2svl2<-G2svl.g2+geom_errorbar(aes(ymin=svl.mean-sd, ymax=svl.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  #facet_wrap(~Sex)+##makes separate graphs for Sexes
  ggtitle("")
G2svl2

####
#### Mass####
##Plotting mass mean +-SD and  +-SE by size class and Sex
G2mass.tab<-with(Group2.ab[!is.na(Group2.ab$mass.g),], aggregate(mass.g, list(month, Analysis.Sex), mean))##remove NA's and get mean

names(G2mass.tab)<-c("month","Sex", "mass.mean")
#View(Amass.tab)

G2mass.tab$sd<-with(Group2.ab[!is.na(Group2.ab$mass.g),], aggregate(mass.g, list(month, Analysis.Sex), sd))[,3]##remove NA's and get sd

#G2mass.tab$se<-with(Group2.ab, aggregate(mass.g, list(month), se))[,2]##function removes any NA's 

G2mass.tab$month <- factor(G2mass.tab$month, levels = month.abb)

##Plotting using ggplot2
G2mass.g2<-ggplot(G2mass.tab, aes(x=month,y=mass.mean))#this establishes the ggplot

##Plot mass by size class and Sex using mean +- SD
G2mass<-G2mass.g2+geom_errorbar(aes(ymin=mass.mean-sd, ymax=mass.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  facet_wrap(~Sex)+##makes seperate graphs for Sexes
  ggtitle("")
G2mass

####
#### Fat mass ####

##Plotting fat mass mean +-SD and  +-SE by size class and Sex
G2fmass.tab<-with(Group2.ab[!is.na(Group2.ab$fat.g),], aggregate(fat.g, list(month), mean))##remove NA's and get mean

names(G2fmass.tab)<-c("month","fat.mean")
#View(Amass.tab)

G2fmass.tab$sd<-with(Group2.ab[!is.na(Group2.ab$fat.g),], aggregate(fat.g, list(month), sd))[,2]##remove NA's and get sd

G2fmass.tab$se<-with(Group2.ab[!is.na(Group2.ab$fat.g),], aggregate(fat.g, list(month), se))[,2]##function removes any NA's 

G2fmass.tab$month <- factor(G2fmass.tab$month, levels = month.abb)

##Plotting using ggplot2
G2fmass.g2<-ggplot(G2fmass.tab, aes(x=month,y=fat.mean))#this establishes the ggplot

##Plot fat mass by size class and Sex using mean +- SD
G2fat<-G2fmass.g2+geom_errorbar(aes(ymin=fat.mean-sd, ymax=fat.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  #facet_wrap(~Sex)+##makes seperate graphs for Sexes
  ggtitle("")
G2fat

####
#### Percent fat####
G2perc.tab<-with(Group2.ab[!is.na(Group2.ab$percfat),], aggregate(percfat,list(month), mean))##remove NA's and get mean

names(G2perc.tab)<-c("month","percfat.mean")
#View(Aperc.tab)

G2perc.tab$sd<-with(Group2.ab[!is.na(Group2.ab$percfat),], aggregate(percfat, list(month), sd))[,2]##remove NA's and get sd

#Aperc.tab$se<-with(Group2.ab [!is.na(Group2.ab$percfat),], aggregate(percfat, list(month), se))[,2]##function removes any NA's 

G2perc.tab$month <- factor(G2perc.tab$month, levels = month.abb)

##Plotting using ggplot2
G2perc.g2<-ggplot(G2perc.tab, aes(x=month,y=percfat.mean))#this establishes the ggplot

##Plot percfat by size class and Sex using mean +- SD
G2perc<-G2perc.g2+geom_errorbar(aes(ymin=percfat.mean-sd, ymax=percfat.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+
  #facet_wrap(~Sex)+##makes separate graphs for Sexes
  ggtitle("")
G2perc

####


#### Group2.ab Plots ####
Group2.plots<-ggarrange(G2fulton, G2svl, G2mass, G2fat, G2perc, 
                        labels = c(),
                        ncol = 1, nrow = 5)
####

#### 8.Plotting Group 3####
#### Fulton by month####
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

Group3.abmonth.tab<-with(Group3.ab, aggregate(fulton, list(month), mean))

names(Group3.abmonth.tab)<-c("month", "fulton.mean")

Group3.abmonth.tab$fulton.sd<-with(Group3.ab[!is.na(Group3.ab$fulton),], aggregate(fulton, list(month), sd))[,2]

Group3.abmonth.tab$fulton.se<-with(Group3.ab[!is.na(Group3.ab$fulton),], aggregate(fulton, list(month), se))[,2]

Group3.abmonth.tab$month <- factor(Group3.abmonth.tab$month, levels = month.abb)

#View(Group3.abmonth.tab)

#by month
#Group3.ab - Plotting fulton's K by month
G3fulton.month<-ggplot(Group3.abmonth.tab, aes(month, fulton.mean))

##Group3.ab - Plot with fulton mean and sd by month
G3fulton<-G3fulton.month+geom_errorbar(aes(ymin=fulton.mean-fulton.sd , ymax=fulton.mean+fulton.sd))+
  geom_point(size=6)+
  #facet_wrap(~Sex)+
  ggtitle("")
G3fulton
####
#### Fulton by year####
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

Group3.year.tab<-with(Group3.ab, aggregate(fulton, list(year), mean))

names(Group3.year.tab)<-c("year", "fulton.mean")

Group3.year.tab$fulton.sd<-with(Group3.ab[!is.na(Group3.ab$fulton),], aggregate(fulton, list(year), sd))[,2]

Group3.year.tab$fulton.se<-with(Group3.ab[!is.na(Group3.ab$fulton),], aggregate(fulton, list(year), se))[,2]

Group3.year.tab$year <- factor(Group3.abmonth.tab$year)

View(Group3.year.tab)
#by month
#Group3.ab - Plotting fulton's K by month
G3fulton.year<-ggplot(Group3.year.tab, aes(year, fulton.mean))

##Group3.ab - Plot with fulton mean and sd by month
G3y.fulton<-G3fulton.year+geom_errorbar(aes(ymin=fulton.mean-fulton.sd , ymax=fulton.mean+fulton.sd))+
  geom_point(size=6)+
  #facet_wrap(~Sex)+
  ggtitle("")
G3y.fulton
####
#### SVL by month ####
G3svl.tab<-with(Group3.ab, aggregate(Intake.SVL, list(month), mean)) #getting mean values

names(G3svl.tab)<-c("month", "svl.mean")

G3svl.tab$sd<-with(Group3.ab, aggregate(Intake.SVL, list(month), sd))[,2] #getting sd 

##also plot with SE, here is the function for calculating SE
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

G3svl.tab$se<-with(Group3.ab, aggregate(Intake.SVL, list(month), se))[,2] #getting SE 

G3svl.tab$month <- factor(G3svl.tab$month, levels = month.abb)

##Plotting using ggplot2
G3svl.g<-ggplot(G3svl.tab, aes(x=month,y=svl.mean))#this establishes the ggplot


##Plot svl by month and Sex using mean +- SD
G3svl<-G3svl.g+geom_errorbar(aes(ymin=svl.mean-sd, ymax=svl.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  #facet_wrap(~Sex)+##makes separate graphs for Sexes
  ggtitle("")
G3svl

####
#### Mass by month####
##Plotting mass mean +-SD and  +-SE by size class and Sex
G3mass.tab<-with(Group3.ab[!is.na(Group3.ab$mass.g),], aggregate(mass.g, list(month), mean))##remove NA's and get mean

names(G3mass.tab)<-c("month","mass.mean")
#View(G3mass.tab)

G3mass.tab$sd<-with(Group3.ab[!is.na(Group3.ab$mass.g),], aggregate(mass.g, list(month), sd))[,2]##remove NA's and get sd

G3mass.tab$se<-with(Group3.ab, aggregate(mass.g, list(month), se))[,2]##function removes any NA's 

G3mass.tab$month <- factor(G3mass.tab$month, levels = month.abb)

##Plotting using ggplot2
G3mass.g2<-ggplot(G3mass.tab, aes(x=month,y=mass.mean))#this establishes the ggplot

##Plot mass by size class and Sex using mean +- SD
G3mass<-G3mass.g2+geom_errorbar(aes(ymin=mass.mean-sd, ymax=mass.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  #facet_wrap(~Sex)+##makes seperate graphs for Sexes
  ggtitle("")
G3mass

##Plot mass by size class and Sex using mean +- SE
#Jmass.g2+geom_errorbar(aes(ymin=mass.mean-se, ymax=mass.mean+se))+
#  geom_point(size=6, (aes(x=month)))+
#facet_wrap(~Sex)+
#  ggtitle("Juv Mass by month, +-SE")
####
#### Fat mass by month ####

##Plotting fat mass mean +-SD and  +-SE by size class and Sex
G3fmass.tab<-with(Group3.ab[!is.na(Group3.ab$fat.g),], aggregate(fat.g, list(month, Analysis.Sex), mean))##remove NA's and get mean

names(G3fmass.tab)<-c("month", "Sex", "fat.mean")
#View(G3fmass.tab)

G3fmass.tab$sd<-with(Group3.ab[!is.na(Group3.ab$fat.g),], aggregate(fat.g, list(month, Analysis.Sex), sd))[,3]##remove NA's and get sd

#G3fmass.tab$se<-with(Group3.ab, aggregate(fat.g, list(month), se))[,2]##function removes any NA's 

G3fmass.tab$month <- factor(G3fmass.tab$month, levels = month.abb)

##Plotting using ggplot2
G3fmass.g2<-ggplot(G3fmass.tab, aes(x=month,y=fat.mean))#this establishes the ggplot

##Plot fat mass by size class and Sex using mean +- SD
G3fmass<-G3fmass.g2+geom_errorbar(aes(ymin=fat.mean-sd, ymax=fat.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  facet_wrap(~Sex)+##makes seperate graphs for Sexes
  ggtitle("")
G3fmass

####

#### Percent fat by month####
##Plotting percfat mass mean +-SD and  +-SE by size class and Sex
#significantly diff by sex must plot separately
G3perc.tab<-with(Group3.ab[!is.na(Group3.ab$percfat),], aggregate(percfat,list(month, Analysis.Sex), mean))##remove NA's and get mean

names(G3perc.tab)<-c("month", "sex", "percfat.mean")
#View(G3perc.tab)

G3perc.tab$sd<-with(Group3.ab[!is.na(Group3.ab$percfat),], aggregate(percfat, list(month, Analysis.Sex), sd))[,3]##remove NA's and get sd

G3perc.tab$se<-with(Group3.ab, aggregate(percfat,list(month, Analysis.Sex), se))[,3]##function removes any NA's 

G3perc.tab$month <- factor(G3perc.tab$month, levels = month.abb)

##Plotting using ggplot2
G3perc.g2<-ggplot(G3perc.tab, aes(x=month,y=percfat.mean))#this establishes the ggplot

##Plot percfat by size class and Sex using mean +- SD
G3perc<-G3perc.g2+geom_errorbar(aes(ymin=percfat.mean-sd, ymax=percfat.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+
  facet_wrap(~sex)+##makes separate graphs for Sexes
  ggtitle("")
G3perc

####


#### Group 3 plots ####
Group3.plots<-ggarrange(G3fulton, G3svl, G3mass, G3fmass, G3perc, 
                        labels = c(),
                        ncol = 1, nrow = 5)
Group3.plots

#### 9.Plotting Group 4####
#### Fulton ####
Group4.abmonth.tab<-with(Group4.ab, aggregate(fulton, list(month), mean))

names(Group4.abmonth.tab)<-c("month", "fulton.mean")

Group4.abmonth.tab$fulton.sd<-with(Group4.ab[!is.na(Group4.ab$fulton),], aggregate(fulton, list(month), sd))[,2]

Group4.abmonth.tab$fulton.se<-with(Group4.ab[!is.na(Group4.ab$fulton),], aggregate(fulton, list(month), se))[,2]

Group4.abmonth.tab$month <- factor(Group4.abmonth.tab$month, levels = month.abb)

#Group4.ab - Plotting fulton's K by month and Sex
G4fulton.month<-ggplot(Group4.abmonth.tab, aes(month, fulton.mean))

##Group 4 - Plot with fulton mean and sd by month and Sex
G4fulton<-G4fulton.month+geom_errorbar(aes(ymin=fulton.mean-fulton.sd , ymax=fulton.mean+fulton.sd))+
  geom_point(size=6)+
  #facet_wrap(~Sex)+
  ggtitle("")
G4fulton

####
####plot avg bc by year G1####
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

Group4.year.tab<-with(Group4.ab, aggregate(fulton, list(year), mean))

names(Group4.year.tab)<-c("year", "fulton.mean")

Group4.year.tab$fulton.sd<-with(Group4.ab[!is.na(Group4.ab$fulton),], aggregate(fulton, list(year), sd))[,2]

Group4.year.tab$fulton.se<-with(Group4.ab[!is.na(Group4.ab$fulton),], aggregate(fulton, list(year), se))[,2]

Group4.year.tab$year <- factor(Group4.abmonth.tab$year)

View(Group4.year.tab)
#by month
#Group4.ab - Plotting fulton's K by month
G4fulton.year<-ggplot(Group4.year.tab, aes(year, fulton.mean))

##Group4.ab - Plot with fulton mean and sd by month
G4y.fulton<-G4fulton.year+geom_errorbar(aes(ymin=fulton.mean-fulton.sd , ymax=fulton.mean+fulton.sd))+
  geom_point(size=6)+
  #facet_wrap(~Sex)+
  ggtitle("")
G4y.fulton
####
#### SVL####
G4svl.tab<-with(Group4.ab, aggregate(Intake.SVL, list(month), mean)) #getting mean values

names(G4svl.tab)<-c("month","svl.mean")

G4svl.tab$sd<-with(Group4.ab, aggregate(Intake.SVL, list(month), sd))[,2] #getting sd 

G4svl.tab$month <- factor(G4svl.tab$month, levels = month.abb)

##also plot with SE, here is the function for calculating SE
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

G4svl.tab$se<-with(Group4.ab, aggregate(Intake.SVL, list(month), se))[,2] #getting SE 

##Plotting using ggplot2
G4svl.g<-ggplot(G4svl.tab, aes(x=month,y=svl.mean))#this establishes the ggplot
##Plot svl by month and Sex using mean +- SD
G4svl<-G4svl.g+geom_errorbar(aes(ymin=svl.mean-sd, ymax=svl.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  #facet_wrap(~Sex)+##makes separate graphs for Sexes
  ggtitle("")
G4svl

##Plot svl by month and Sex using mean +- SE
#Asvl.g+geom_errorbar(aes(ymin=svl.mean-se, ymax=svl.mean+se))+
#  geom_point(size=6, (aes(x=month)))+
#facet_wrap(~Sex)+
#  ggtitle("Adult SVL by month, +-SE")
####
#### Mass####
##Plotting mass mean +-SD and  +-SE by size class and Sex
G4mass.tab<-with(Group4.ab[!is.na(Group4.ab$mass.g),], aggregate(mass.g, list(month), mean))##remove NA's and get mean

names(G4mass.tab)<-c("month","mass.mean")
#View(G4mass.tab)

G4mass.tab$sd<-with(Group4.ab[!is.na(Group4.ab$mass.g),], aggregate(mass.g, list(month), sd))[,2]##remove NA's and get sd

G4mass.tab$se<-with(Group4.ab, aggregate(mass.g, list(month), se))[,2]##function removes any NA's 

G4mass.tab$month <- factor(G4mass.tab$month, levels = month.abb)

##Plotting using ggplot2
G4mass.g2<-ggplot(G4mass.tab, aes(x=month,y=mass.mean))#this establishes the ggplot

##Plot mass by size class and Sex using mean +- SD
G4mass<-G4mass.g2+geom_errorbar(aes(ymin=mass.mean-sd, ymax=mass.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  #facet_wrap(~Sex)+##makes seperate graphs for Sexes
  ggtitle("")
G4mass

####
#### Fat mass ####

##Plotting fat mass mean +-SD and  +-SE by size class and Sex
G4fmass.tab<-with(Group4.ab[!is.na(Group4.ab$fat.g),], aggregate(fat.g, list(month, Analysis.Sex), mean))##remove NA's and get mean

names(G4fmass.tab)<-c("month","Sex", "fat.mean")
View(G4fmass.tab)

G4fmass.tab$sd<-with(Group4.ab[!is.na(Group4.ab$fat.g),], aggregate(fat.g, list(month, Analysis.Sex), sd))[,3]##remove NA's and get sd

G4fmass.tab$se<-with(Group4.ab[!is.na(Group4.ab$fat.g),], aggregate(fat.g, list(month, Analysis.Sex), se))[,3]##function removes any NA's 

G4fmass.tab$month <- factor(G4fmass.tab$month, levels = month.abb)

##Plotting using ggplot2
G4fmass.g2<-ggplot(G4fmass.tab, aes(x=month,y=fat.mean))#this establishes the ggplot

##Plot fat mass by size class and Sex using mean +- SD
G4fat<-G4fmass.g2+geom_errorbar(aes(ymin=fat.mean-sd, ymax=fat.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  facet_wrap(~Sex)+##makes seperate graphs for Sexes
  ggtitle("")
G4fat

####
#### Percent fat####
G4perc.tab<-with(Group4.ab[!is.na(Group4.ab$percfat),], aggregate(percfat, list(month, Analysis.Sex), mean))##remove NA's and get mean

names(G4perc.tab)<-c("month", "Sex", "percfat.mean")
#View(G4perc.tab)

G4perc.tab$sd<-with(Group4.ab[!is.na(Group4.ab$percfat),], aggregate(percfat, list(month, Analysis.Sex), sd))[,3]##remove NA's and get sd

#Aperc.tab$se<-with(Adults [!is.na(Adults$percfat),], aggregate(percfat, list(month), se))[,2]##function removes any NA's 

G4perc.tab$month <- factor(G4perc.tab$month, levels = month.abb)

##Plotting using ggplot2
G4perc.g2<-ggplot(G4perc.tab, aes(x=month,y=percfat.mean))#this establishes the ggplot

##Plot percfat by size class and Sex using mean +- SD
G4perc<-G4perc.g2+geom_errorbar(aes(ymin=percfat.mean-sd, ymax=percfat.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+
  facet_wrap(~Sex)+##makes separate graphs for Sexes
  ggtitle("")
G4perc

####

#### Group 4 plots ####
Group4.plots<-
  ggarrange(G4fulton, G4svl, G4mass, G4fat, G4perc, 
            labels = c(),
            ncol = 1, nrow = 5)


#### 10. Plotting Group 5####
#### Fulton ####
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

Group5.abmonth.tab<-with(Group5.ab, aggregate(fulton, list(month), mean))

names(Group5.abmonth.tab)<-c("month", "fulton.mean")

Group5.abmonth.tab$fulton.sd<-with(Group5.ab[!is.na(Group5.ab$fulton),], aggregate(fulton, list(month), sd))[,2]

Group5.abmonth.tab$fulton.se<-with(Group5.ab[!is.na(Group5.ab$fulton),], aggregate(fulton, list(month), se))[,2]

Group5.abmonth.tab$month <- factor(Group5.abmonth.tab$month, levels = month.abb)

#View(Group5.abmonth.tab)

#by month
#Group5.ab - Plotting fulton's K by month
G5fulton.month<-ggplot(Group5.abmonth.tab, aes(month, fulton.mean))

##Group5.ab - Plot with fulton mean and sd by month
G5fulton<-G5fulton.month+geom_errorbar(aes(ymin=fulton.mean-fulton.sd , ymax=fulton.mean+fulton.sd))+
  geom_point(size=6)+
  #facet_wrap(~Sex)+
  ggtitle("")
G5fulton
####
####plot avg bc by year G1####
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

Group5.year.tab<-with(Group5.ab, aggregate(fulton, list(year), mean))

names(Group5.year.tab)<-c("year", "fulton.mean")

Group5.year.tab$fulton.sd<-with(Group5.ab[!is.na(Group5.ab$fulton),], aggregate(fulton, list(year), sd))[,2]

Group5.year.tab$fulton.se<-with(Group5.ab[!is.na(Group5.ab$fulton),], aggregate(fulton, list(year), se))[,2]

Group5.year.tab$year <- factor(Group5.abmonth.tab$year)

View(Group5.year.tab)
#by month
#Group5.ab - Plotting fulton's K by month
G5fulton.year<-ggplot(Group5.year.tab, aes(year, fulton.mean))

##Group5.ab - Plot with fulton mean and sd by month
G5y.fulton<-G5fulton.year+geom_errorbar(aes(ymin=fulton.mean-fulton.sd , ymax=fulton.mean+fulton.sd))+
  geom_point(size=6)+
  #facet_wrap(~Sex)+
  ggtitle("")
G5y.fulton
####
#### SVL by month ####
G5svl.tab<-with(Group5.ab, aggregate(Intake.SVL, list(month), mean)) #getting mean values

names(G5svl.tab)<-c("month", "svl.mean")

G5svl.tab$sd<-with(Group5.ab, aggregate(Intake.SVL, list(month), sd))[,2] #getting sd 

##also plot with SE, here is the function for calculating SE
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

G5svl.tab$se<-with(Group5.ab, aggregate(Intake.SVL, list(month), se))[,2] #getting SE 

G5svl.tab$month <- factor(G5svl.tab$month, levels = month.abb)

##Plotting using ggplot2
G5svl.g<-ggplot(G5svl.tab, aes(x=month,y=svl.mean))#this establishes the ggplot

##Plot svl by month and Sex using mean +- SD
G5svl<-G5svl.g+geom_errorbar(aes(ymin=svl.mean-sd, ymax=svl.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  #facet_wrap(~Sex)+##makes separate graphs for Sexes
  ggtitle("")
G5svl

####
#### Mass by month####
##Plotting mass mean +-SD and  +-SE by size class and Sex
G5mass.tab<-with(Group5.ab[!is.na(Group5.ab$mass.g),], aggregate(mass.g, list(month), mean))##remove NA's and get mean

names(G5mass.tab)<-c("month","mass.mean")

G5mass.tab$sd<-with(Group5.ab[!is.na(Group5.ab$mass.g),], aggregate(mass.g, list(month), sd))[,2]##remove NA's and get sd

G5mass.tab$se<-with(Group5.ab, aggregate(mass.g, list(month), se))[,2]##function removes any NA's 

G5mass.tab$month <- factor(G5mass.tab$month, levels = month.abb)
#View(G5mass.tab)

##Plotting using ggplot2
G5mass.g2<-ggplot(G5mass.tab, aes(x=month,y=mass.mean))#this establishes the ggplot

##Plot mass by size class and Sex using mean +- SD
G5mass<-G5mass.g2+geom_errorbar(aes(ymin=mass.mean-sd, ymax=mass.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  #facet_wrap(~Sex)+##makes seperate graphs for Sexes
  ggtitle("")
G5mass

####
#### Fat mass by month ####

##Plotting fat mass mean +-SD and  +-SE by size class and Sex
G5fmass.tab<-with(Group5.ab[!is.na(Group5.ab$fat.g),], aggregate(fat.g, list(month), mean))##remove NA's and get mean

names(G5fmass.tab)<-c("month","fat.mean")

G5fmass.tab$sd<-with(Group5.ab[!is.na(Group5.ab$fat.g),], aggregate(fat.g, list(month), sd))[,2]##remove NA's and get sd

#G5fmass.tab$se<-with(Group5.ab, aggregate(fat.g, list(month), se))[,2]##function removes any NA's 

G5fmass.tab$month <- factor(G5fmass.tab$month, levels = month.abb)

##Plotting using ggplot2
G5fmass.g2<-ggplot(G5fmass.tab, aes(x=month,y=fat.mean))#this establishes the ggplot

##Plot fat mass by size class and Sex using mean +- SD
G5fmass<-G5fmass.g2+geom_errorbar(aes(ymin=fat.mean-sd, ymax=fat.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  #facet_wrap(~Sex)+##makes seperate graphs for Sexes
  ggtitle("")
G5fmass

####

#### Percent fat by month####
##Plotting fat mass mean +-SD and  +-SE by size class and Sex
#significantly diff by sex must plot separately
G5perc.tab<-with(Group5.ab[!is.na(Group5.ab$percfat),], aggregate(percfat,list(month), mean))##remove NA's and get mean

names(G5perc.tab)<-c("month", "percfat.mean")
#View(G5perc.tab)

G5perc.tab$sd<-with(Group5.ab[!is.na(Group5.ab$percfat),], aggregate(percfat, list(month), sd))[,2]##remove NA's and get sd

G5perc.tab$se<-with(Group5.ab[!is.na(Group5.ab$percfat),], aggregate(percfat,list(month), se))[,2]##function removes any NA's 

G5perc.tab$month <- factor(G5perc.tab$month, levels = month.abb)

##Plotting using ggplot2
G5perc.g2<-ggplot(G5perc.tab, aes(x=month,y=percfat.mean))#this establishes the ggplot

##Plot percfat by size class and Sex using mean +- SD
G5perc<-G5perc.g2+geom_errorbar(aes(ymin=percfat.mean-sd, ymax=percfat.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+
  #facet_wrap(~sex)+##makes separate graphs for Sexes
  ggtitle("")
G5perc


### Group 5 plots ####
Group5.plots<-ggarrange(G5fulton, G5svl, G5mass, G5fmass, G5perc, 
                        labels = c(),
                        ncol = 1, nrow = 5)

Group5.plots

#### all groups plottesd together####
tegu.data.ab$Group<- ifelse(tegu.data.ab$sizeclass == "Group1", "Hatchling/Early Juveniles",
                            ifelse(tegu.data.ab$sizeclass == "Group2", "Late Juvenile/Reproductive Adults",
                                   ifelse(tegu.data.ab$sizeclass == "Group3", "Large Adults",
                                          ifelse(tegu.data.ab$sizeclass == "Group4", "Early Adult", "Large Adult"))))
tegu.data.ab$pop<- ifelse(tegu.data.ab$sizeclass == "Group1", "Invasive",
                            ifelse(tegu.data.ab$sizeclass == "Group2", "Invasive",
                                   ifelse(tegu.data.ab$sizeclass == "Group3", "Invasive",
                                          ifelse(tegu.data.ab$sizeclass == "Group4", "Invasive", "Invasive"))))

#native
merianae$Group<- ifelse(merianae$sizeclass == "Group1", "Hatchling/Early Juveniles",
                            ifelse(merianae$sizeclass == "Group2", "Late Juvenile/Reproductive Adults",
                                   ifelse(merianae$sizeclass == "Group3", "Large Adults",
                                          ifelse(merianae$sizeclass == "Group4", "Early Adult", "Large Adult"))))
#native
merianae$pop<- ifelse(merianae$sizeclass == "Group1", "Native",
                        ifelse(merianae$sizeclass == "Group2", "Native",
                               ifelse(merianae$sizeclass == "Group3", "Native",
                                      ifelse(merianae$sizeclass == "Group4", "Native", "Native"))))

#### Fulton
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

alltegu.abmonth.tab<-with(tegu.data.ab, aggregate(fulton, list(month, Group, pop), mean))

names(alltegu.abmonth.tab)<-c("Month", "Group", "pop", "Fulton.Mean")

alltegu.abmonth.tab$fulton.sd<-with(tegu.data.ab[!is.na(tegu.data.ab$fulton),], aggregate(fulton, list(month, Group), sd))[,3]

alltegu.abmonth.tab$fulton.se<-with(tegu.data.ab[!is.na(tegu.data.ab$fulton),], aggregate(fulton, list(month, Group), se))[,3]

alltegu.abmonth.tab$Month <- factor(alltegu.abmonth.tab$Month, levels = month.abb)

#View(Group1.abmonth.tab)
#by month
# Plotting fulton's K by month
atfulton.month<-ggplot(alltegu.abmonth.tab, aes(Month, Fulton.Mean))

##Group1.ab - Plot with fulton mean and sd by month
atfulton<-atfulton.month+geom_errorbar(aes(ymin=Fulton.Mean-fulton.sd , ymax=Fulton.Mean+fulton.sd))+
  #geom_point(aes(color = ))+
  #facet_wrap(~sizeclass)+
  ggtitle("")
##
atfulton<-atfulton.month+geom_errorbar(aes(ymin=Fulton.Mean-fulton.sd , ymax=Fulton.Mean+fulton.sd, color=Group))

atfulton + geom_point(aes(color=Group), size=3.5) +theme(axis.title=element_text(size = 15)) +
  theme(axis.text=element_text(size = 12))+
  scale_color_manual(breaks = c("Large Adults", "Late Juvenile/Reproductive Adults", "Hatchling/Early Juveniles"), values = c("#00798c", "#d1495b", "#edae49"))+
  theme(legend.text = element_text(size = 12)) 

### NATIVE
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

Nat.month.tab<-with(merianae[!is.na(merianae$fulton),], aggregate(fulton, list(month, Group, pop), mean))

names(Nat.month.tab)<-c("Month", "Group", "pop", "Fulton.Mean")

Nat.month.tab$fulton.sd<-with(merianae[!is.na(merianae$fulton),], aggregate(fulton, list(month, Group), sd))[,3]

Nat.month.tab$fulton.se<-with(merianae[!is.na(merianae$fulton),], aggregate(fulton, list(month, Group), se))[,3]

Nat.month.tab$Month <- factor(merianae$Month, levels = month.abb)

#View(Group1.abmonth.tab)
#by month
# Plotting fulton's K by month
Natfulton.month<-ggplot(Nat.month.tab, aes(Month, Fulton.Mean))

##Group1.ab - Plot with fulton mean and sd by month
Natfulton<-Natfulton.month+geom_errorbar(aes(ymin=Fulton.Mean-fulton.sd , ymax=Fulton.Mean+fulton.sd))+
  #geom_point(aes(color = ))+
  #facet_wrap(~sizeclass)+
  ggtitle("")
##
Natfulton<-Natfulton.month+geom_errorbar(aes(ymin=Fulton.Mean-fulton.sd , ymax=Fulton.Mean+fulton.sd, color=Group))

Natfulton + geom_point(aes(color=Group), size=3.5) +theme(axis.title=element_text(size = 15)) +
  theme(axis.text=element_text(size = 12))+
  scale_color_manual(breaks = c("Large Adults", "Late Juvenile/Reproductive Adults", "Hatchling/Early Juveniles"), values = c("#00798c", "#d1495b", "#edae49"))+
  theme(legend.text = element_text(size = 12)) 

#plot native and invasive adults
both<-full_join(alltegu.abmonth.tab, Nat.month.tab)
both<-na.omit(both)
adult.both<- subset(both, both$Group == "Large Adults")
both.month<-ggplot(adult.both, aes(Month, Fulton.Mean))

both.plot<-both.month+geom_errorbar(aes(ymin=Fulton.Mean-fulton.sd , ymax=Fulton.Mean+fulton.sd, color= pop))

both.plot + geom_point(aes(color= pop), size=3.5) +theme(axis.title=element_text(size = 15)) +
  theme(axis.text=element_text(size = 12))+
  scale_color_manual(breaks = c("Invasive","Native"), values = c("#00798c", "#d1495b", "#edae49"))+
  theme(legend.text = element_text(size = 12))+labs(colour = "Population") 

#SVL

se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

svltegu.abmonth.tab<-with(tegu.data.ab, aggregate(Intake.SVL, list(month, Group), mean))

names(svltegu.abmonth.tab)<-c("Month", "Group", "SVL.Mean")

svltegu.abmonth.tab$svl.sd<-with(tegu.data.ab[!is.na(tegu.data.ab$Intake.SVL),], aggregate(Intake.SVL, list(month, Group), sd))[,3]

svltegu.abmonth.tab$Month <- factor(svltegu.abmonth.tab$Month, levels = month.abb)

#View(Group1.abmonth.tab)
#by month
# Plotting fulton's K by month
svltegu.abmonth<-ggplot(svltegu.abmonth.tab, aes(Month, SVL.Mean))

##Group1.ab - Plot with fulton mean and sd by month
svltegu<-svltegu.abmonth+geom_errorbar(aes(ymin=SVL.Mean-svl.sd , ymax=SVL.Mean+svl.sd))+
  #geom_point(aes(color = ))+
  #facet_wrap(~sizeclass)+
  ggtitle("")
##
svltegu<-svltegu.abmonth+geom_errorbar(aes(ymin=SVL.Mean-svl.sd , ymax=SVL.Mean+svl.sd, color=Group))

svltegu + geom_point(aes(color=Group), size=3.5) +theme(axis.title=element_text(size = 15)) +
  theme(axis.text=element_text(size = 12))+
  scale_color_manual(breaks = c("Large Adults", "Late Juvenile/Reproductive Adults", "Hatchling/Early Juveniles"), values = c("#00798c", "#d1495b", "#edae49", "#a2d729", "#2e4057"))+
  theme(legend.text = element_text(size = 12)) 

#Mass
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

masstegu.abmonth.tab<-with(tegu.data.ab, aggregate(mass.g, list(month, Group), mean))

names(masstegu.abmonth.tab)<-c("Month", "Group", "Mass.Mean")

masstegu.abmonth.tab$mass.sd<-with(tegu.data.ab[!is.na(tegu.data.ab$mass.g),], aggregate(mass.g, list(month, Group), sd))[,3]

masstegu.abmonth.tab$Month <- factor(masstegu.abmonth.tab$Month, levels = month.abb)

#View(Group1.abmonth.tab)
#by month
# Plotting fulton's K by month
masstegu.abmonth<-ggplot(masstegu.abmonth.tab, aes(Month, Mass.Mean))

##Group1.ab - Plot with fulton mean and sd by month
masstegu<-masstegu.abmonth+geom_errorbar(aes(ymin=Mass.Mean-mass.sd , ymax=Mass.Mean+mass.sd))+
  geom_point(aes(color = ))+
  #facet_wrap(~sizeclass)+
  ggtitle("")
##
masstegu<-masstegu.abmonth+geom_errorbar(aes(ymin=Mass.Mean-mass.sd , ymax=Mass.Mean+mass.sd, color=Group))

masstegu +  geom_point(aes(color=Group), size=3.5) +theme(axis.title=element_text(size = 15)) +
  theme(axis.text=element_text(size = 12))+
  scale_color_manual(breaks = c("Large Adults", "Late Juvenile/Reproductive Adults", "Hatchling/Early Juveniles"), values = c("#00798c", "#d1495b", "#edae49", "#a2d729", "#2e4057"))+
  
  theme(legend.text = element_text(size = 12))

#Fat Mass
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

fattegu.abmonth.tab<-with(tegu.data.ab[!is.na(tegu.data.ab$fat.g),], aggregate(fat.g, list(month, Group), mean))

names(fattegu.abmonth.tab)<-c("Month", "Group", "Fat.Mean")

fattegu.abmonth.tab$fat.sd<-with(tegu.data.ab[!is.na(tegu.data.ab$fat.g),], aggregate(fat.g, list(month, Group), sd))[,3]

fattegu.abmonth.tab$Month <- factor(fattegu.abmonth.tab$Month, levels = month.abb)

#View(fattegu.abmonth.tab)
#by month
# Plotting fulton's K by month
fattegu.abmonth<-ggplot(fattegu.abmonth.tab, aes(Month, Fat.Mean))

##Group1.ab - Plot with fulton mean and sd by month
fattegu<-fattegu.abmonth+geom_errorbar(aes(ymin=Fat.Mean-fat.sd , ymax=Fat.Mean+fat.sd))+
  geom_point(aes(color = ))+
  #facet_wrap(~sizeclass)+
  ggtitle("")
##
fattegu<-fattegu.abmonth+geom_errorbar(aes(ymin=Fat.Mean-fat.sd , ymax=Fat.Mean+fat.sd, color=Group))

fattegu +  geom_point(aes(color=Group), size=3.5) +theme(axis.title=element_text(size = 15)) +
  theme(axis.text=element_text(size = 12))+
  scale_color_manual(breaks = c("Large Adults", "Late Juvenile/Reproductive Adults", "Hatchling/Early Juveniles"), values = c("#00798c", "#d1495b", "#edae49", "#a2d729", "#2e4057"))+
  theme(legend.text = element_text(size = 12))

#Percent Fat
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

perctegu.abmonth.tab<-with(tegu.data.ab[!is.na(tegu.data.ab$percfat),], aggregate(percfat, list(month, Group), mean))

names(perctegu.abmonth.tab)<-c("Month", "Group", "PercentFat.Mean")

perctegu.abmonth.tab$perc.sd<-with(tegu.data.ab[!is.na(tegu.data.ab$percfat),], aggregate(percfat, list(month, Group), sd))[,3]

perctegu.abmonth.tab$Month <- factor(perctegu.abmonth.tab$Month, levels = month.abb)

View(perctegu.abmonth.tab)
#by month
# Plotting fulton's K by month
perctegu.abmonth<-ggplot(perctegu.abmonth.tab, aes(Month,PercentFat.Mean))

##Group1.ab - Plot with fulton mean and sd by month
perctegu<-perctegu.abmonth+geom_errorbar(aes(ymin=PercentFat.Mean-perc.sd , ymax=PercentFat.Mean+perc.sd))+
  geom_point(aes(color = ))+
  #facet_wrap(~sizeclass)+
  ggtitle("")
##
perctegu<-perctegu.abmonth+geom_errorbar(aes(ymin=PercentFat.Mean-perc.sd , ymax=PercentFat.Mean+perc.sd, color=Group))

perctegu + geom_point(aes(color=Group), size=3.5) +theme(axis.title=element_text(size = 15)) +
  theme(axis.text=element_text(size = 12))+
  scale_color_manual(breaks = c("Large Adults", "Late Juvenile/Reproductive Adults", "Hatchling/Early Juveniles"), values = c("#00798c", "#d1495b", "#edae49", "#a2d729", "#2e4057"))+
  theme(legend.text = element_text(size = 12))


####by year
#### Fulton
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

alltegu.abyear.tab<-with(tegu.data.ab, aggregate(fulton, list(year, Group), mean))

names(alltegu.abyear.tab)<-c("Year", "Group", "Fulton.Mean")

alltegu.abyear.tab$fulton.sd<-with(tegu.data.ab[!is.na(tegu.data.ab$fulton),], aggregate(fulton, list(year, Group), sd))[,3]

alltegu.abyear.tab$fulton.se<-with(tegu.data.ab[!is.na(tegu.data.ab$fulton),], aggregate(fulton, list(year, Group), se))[,3]

#alltegu.abyear.tab$year <- factor(alltegu.abyear.tab$year, levels = year.abb)

#View(Group1.abyear.tab)
#by year
# Plotting fulton's K by year
atfulton.year<-ggplot(alltegu.abyear.tab, aes(Year, Fulton.Mean))

##Group1.ab - Plot with fulton mean and sd by year
atfulton<-atfulton.year+geom_errorbar(aes(ymin=Fulton.Mean-fulton.sd , ymax=Fulton.Mean+fulton.sd))+
  geom_point(aes(color = ))+
  #facet_wrap(~sizeclass)+
  ggtitle("")
##
atfulton<-atfulton.year+geom_errorbar(aes(ymin=Fulton.Mean-fulton.sd , ymax=Fulton.Mean+fulton.sd, color=Group))+
  scale_color_manual(breaks = c("Large Adults", "Late Juvenile/Reproductive Adults", "Hatchling/Early Juveniles"), values = c("#00798c", "#d1495b", "#edae49", "#a2d729", "#2e4057"))


atfulton + geom_point(aes(color=Group), size=3)

#check for significant differences between FK for each year
kruskal.test(Group1.ab$fulton, Group1.ab$year)
kruskal.test(Group2.ab$fulton, Group2.ab$year)
kruskal.test(Group3.ab$fulton, Group3.ab$year)
kruskal.test(Group4.ab$fulton, Group4.ab$year)
kruskal.test(Group5.ab$fulton, Group5.ab$year)
Group1.ab$YEAR<-as.factor(Group1.ab$year)
Group2.ab$YEAR<-as.factor(Group2.ab$year)
Group3.ab$YEAR<-as.factor(Group3.ab$year)
Group4.ab$YEAR<-as.factor(Group4.ab$year)
Group5.ab$YEAR<-as.factor(Group5.ab$year)

dunnTest(Group1.ab$fulton~Group1.ab$YEAR)
dunnTest(Group2.ab$fulton~Group2.ab$YEAR)
dunnTest(Group3.ab$fulton~Group3.ab$YEAR)#
dunnTest(Group4.ab$fulton~Group4.ab$YEAR)#
dunnTest(Group5.ab$fulton~Group5.ab$YEAR)


#SVL

se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

svltegu.abyear.tab<-with(tegu.data.ab, aggregate(Intake.SVL, list(year, Group), mean))

names(svltegu.abyear.tab)<-c("year", "group", "svl.mean")

svltegu.abyear.tab$svl.sd<-with(tegu.data.ab[!is.na(tegu.data.ab$Intake.SVL),], aggregate(Intake.SVL, list(year, Group), sd))[,3]

#svltegu.abyear.tab$year <- factor(svltegu.abyear.tab$year, levels = year.abb)

#View(Group1.abyear.tab)
#by year
# Plotting fulton's K by year
svltegu.abyear<-ggplot(svltegu.abyear.tab, aes(year, svl.mean))

##Group1.ab - Plot with fulton mean and sd by year
svltegu<-svltegu.abyear+geom_errorbar(aes(ymin=svl.mean-svl.sd , ymax=svl.mean+svl.sd))+
  geom_point(aes(color = ))+
  #facet_wrap(~sizeclass)+
  ggtitle("")
##
svltegu<-svltegu.abyear+geom_errorbar(aes(ymin=svl.mean-svl.sd , ymax=svl.mean+svl.sd, color=group))

svltegu + geom_point(aes(color=group), size=3)

#### 11. Plotting all tegus by Groups ####
#### Fulton's K ####
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

tegusmonth.tab<-with(tegu.data, aggregate(fulton, list(month, sizeclass), mean))

names(tegusmonth.tab)<-c("month", "sizeclass", "fulton.mean")

tegusmonth.tab$fulton.sd<-with(tegu.data[!is.na(tegu.data$fulton),], aggregate(fulton, list(month, sizeclass), sd))[,3]

tegusmonth.tab$fulton.se<-with(tegu.data[!is.na(tegu.data$fulton),], aggregate(fulton, list(month, sizeclass), se))[,3]

#View(tegusmonth.tab)

tegusmonth.tab$month <- factor(tegusmonth.tab$month, levels = month.abb)

#tegus - Plotting fulton's K by month and sizeclass
tfulton.month<-ggplot(tegusmonth.tab, aes(month, fulton.mean))

##tegu - Plot with fulton mean and sd by month and sizeclass
tfulton.month+geom_errorbar(aes(ymin=fulton.mean-fulton.sd , ymax=fulton.mean+fulton.sd))+
  geom_point(size=6)+
  facet_wrap(~sizeclass)+
  ggtitle("Fulton's K by Month with +-SD")

#### Fulton by sizeclass

#### Fulton by year ####
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

tegusy.tab<-with(tegu.data, aggregate(fulton, list(year, sizeclass), mean))

names(tegusy.tab)<-c("year", "sizeclass", "fulton.mean")

tegusy.tab$fulton.sd<-with(tegu.data[!is.na(tegu.data$fulton),], aggregate(fulton, list(year, sizeclass), sd))[,3]

tegusy.tab$fulton.se<-with(tegu.data[!is.na(tegu.data$fulton),], aggregate(fulton, list(year, sizeclass), se))[,3]

View(tegusy.tab)

tegusy.tab$year <- factor(tegusy.tab$year)

#tegus - Plotting fulton's K by month and sizeclass
tfulton.y<-ggplot(tegusy.tab, aes(year, fulton.mean))

##tegu - Plot with fulton mean and sd by month and sizeclass
tfulton.y+geom_errorbar(aes(ymin=fulton.mean-fulton.sd , ymax=fulton.mean+fulton.sd))+
  geom_point(size=6)+
  facet_wrap(~sizeclass)+
  ggtitle("Fulton's K by Year with +-SD")
####
#### SVL ####
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

teguSVL.tab<-with(tegu.data, aggregate(Intake.SVL, list(month, sizeclass), mean))

names(teguSVL.tab)<-c("month", "sizeclass", "SVL.mean")

teguSVL.tab$Intake.SVL.sd<-with(tegu.data[!is.na(tegu.data$Intake.SVL),], aggregate(Intake.SVL, list(month, sizeclass), sd))[,3]

teguSVL.tab$Intake.SVL.se<-with(tegu.data[!is.na(tegu.data$Intake.SVL),], aggregate(Intake.SVL, list(month, sizeclass), se))[,3]

teguSVL.tab$month <- factor(teguSVL.tab$month, levels = month.abb)

#View(teguSVL.tab)

#tegus - Plotting SVL by month and sizeclass
teguSVL.month<-ggplot(teguSVL.tab, aes(month, SVL.mean))

##tegu - Plot with SVL mean and sd by month and sizeclass
teguSVL.month+geom_errorbar(aes(ymin=SVL.mean-Intake.SVL.sd , ymax=SVL.mean+Intake.SVL.sd))+
  geom_point(size=6)+
  facet_wrap(~sizeclass)+
  ggtitle("SVL by Month with +-SD")

####

#### Fat Mass ####
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

tegufat.tab<-with(tegu.data[!is.na(tegu.data$fat.g),], 
                  aggregate(fat.g, list(month, sizeclass), mean))

names(tegufat.tab)<-c("month", "sizeclass", "fat.mean")

tegufat.tab$fat.sd<-with(tegu.data[!is.na(tegu.data$fat.g),], 
                         aggregate(fat.g, list(month, sizeclass), sd))[,3]

tegufat.tab$month <- factor(tegufat.tab$month, levels = month.abb)

ggarrange(G1fmass, G2fat, G3fmass, G4fat, G5fmass, 
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

#tegufat.tab$fat.se<-with(tegu.data[!is.na(tegu.data$fat.g),], 
#                         aggregate(fat.g, list(month, sizeclass), se))[,3]

#View(tegufat.tab)

#tegus - Plotting fat weight by month and sizeclass
tfat.month<-ggplot(tegufat.tab, aes(month, fat.mean))

##tegu - Plot with fat weight mean and sd by month and sizeclass
tfat.month+geom_errorbar(aes(ymin=fat.mean-fat.sd , ymax=fat.mean+fat.sd))+
  geom_point(size=6)+
  facet_wrap(~sizeclass)+
  ggtitle("Fat Weight (g) by Month with +-SD")


#### Mass ####
se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values
  sqrt(var(as.vector(y))/length(y))
}

tegum.tab<-with(tegu.data, aggregate(mass.g, list(month, sizeclass), mean))

names(tegum.tab)<-c("month", "sizeclass", "Weight.Mean")

tegum.tab$m.sd<-with(tegu.data[!is.na(tegu.data$mass.g),], aggregate(mass.g, list(month, sizeclass), sd))[,3]

tegum.tab$m.se<-with(tegu.data[!is.na(tegu.data$mass.g),], aggregate(mass.g, list(month, sizeclass), se))[,3]

tegum.tab$month <- factor(tegum.tab$month, levels = month.abb)

View(tegum.tab)

#tegus - Plotting SVL by month and sizeclass
tegum.month<-ggplot(tegum.tab, aes(month, Weight.Mean))

##tegu - Plot with SVL mean and sd by month and sizeclass
tegum.month+geom_errorbar(aes(ymin=Weight.Mean-m.sd , ymax=Weight.Mean+m.sd))+
  geom_point(size=6)+
  facet_wrap(~sizeclass)+
  ggtitle("Weight by Month with +-SD")
#### Percent Fat ####

tperc.tab<-with(tegu.data[!is.na(tegu.data$percfat),], aggregate(percfat, list(month, sizeclass, Analysis.Sex), mean))##remove NA's and get mean
names(tperc.tab)<-c("month", "sizeclass", "Sex", "percfat.mean")
tperc.tab$sd<-with(tegu.data[!is.na(tegu.data$percfat),], aggregate(percfat, list(month, sizeclass, Analysis.Sex), sd))[,4]##remove NA's and get sd

tperc.tab$se<-with(tegu.data [!is.na(tegu.data$percfat),], aggregate(percfat, list(month, sizeclass, Analysis.Sex), se))[,4]##function removes any NA's 

tperc.tab$month <- factor(tperc.tab$month, levels = month.abb)

##Plotting using ggplot2
tperc.g2<-ggplot(tperc.tab, aes(x=month,y=percfat.mean))#this establishes the ggplot
par(mfrow=c(2,3))
##Plot percfat by size class using mean +- SD
tperc.g2+geom_errorbar(aes(ymin=percfat.mean-sd, ymax=percfat.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+
  facet_wrap(~Sex+sizeclass, ncol=4)+
  #facet_wrap()##makes separate graphs for sizeclass
  ggtitle("Tegu Percent Fat by month, +-SD")

ggarrange(G1perc, G2perc, G3perc, G4perc, G5perc, 
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

####



#### Correlation of variables####

#environmental variables 
tegu.env <-tegu.data.ab[c(37, 65:68)]
TM<-cor(tegu.env, method="spearman")
round(TM, 2)
write.csv(TM, "envmatrix.csv", row.names=F)
#environmental
env.mlr <- lm(fulton ~ Rain.cm + Min.Temp.C + Max.Temp.C, 
              data = tegu.data.ab, na.action = "na.fail")
bc.G2.5 <- boxcox(env.mlr)
LambdaG2.5 <- bc.G2.5$x[which.max(bc.G2.5$y)]
env.mlr2 <- lm((tegu.data.ab$fulton^LambdaG2.5-1/LambdaG2.5)~Rain.cm + 
                 Min.Temp.C + Max.Temp.C, data = tegu.data.ab, na.action = "na.fail")
summary(env.mlr)
#plot(G1.5.mlr)
ols_test_normality(env.mlr2)
ols_vif_tol(env.mlr2)


#distance
tegu.dist <-tegu.data.ab[c(37, 47:52)]
TM<-cor(tegu.dist)
round(TM, 2)
#write.csv(TM, "distmatrix.csv", row.names=F)
#distance
dist.mlr <- lm(fulton ~ UP.DIST + C110424.DIST + C110111.DIST, 
               data = tegu.data.ab, na.action = "na.fail")
summary(dist.mlr)
#plot(G1.4.mlr)
#ols_test_normality(G1.4.mlr)
ols_vif_tol(dist.mlr)

#temporal
tegu.temp <-tegu.data.ab[c(37,2, 79:95)]
TM<-cor(tegu.temp)
round(TM, 2)
#write.csv(TM, "tempmatrix.csv", row.names=F)
#temporal
G1.6.mlr <- lm(fulton ~ julian + y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12, 
               data = Group1.ab, na.action = "na.fail")
summary(G1.6.mlr)
#plot(G1.6.mlr)
ols_test_normality(G1.6.mlr)

ols_vif_tol(G1.6.mlr)

#### Group 1 Regressions... ####
#fulton
#day
plot((Group1.ab$fulton~Group1.ab$julian), main="Group1.ab tegu body condition vs. day of capture")
abline(lm(Group1.ab$fulton~Group1.ab$julian), col="red")
G1day.line<-(lm(Group1.ab$fulton~Group1.ab$julian))
#plot(G5day.line)
summary(G1day.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$julian))

#All adults Fulton's K vs. 
par(mfrow=c(1,1))

#Julian date
plot((Group1.ab$fulton~Group1.ab$julian), main="Group 1 tegu body condition vs. Julian date")
abline(lm(Group1.ab$fulton~Group1.ab$julian), col="red")
G1j.line<-(lm(Group1.ab$fulton~Group1.ab$julian))
#plot(G1j.line)
summary(G1j.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$julian))

#boxcox - sig & normal
bc.G1j<-boxcox(G1j.line)
(lambda.G1j<-bc.G1j$x[which.max(bc.G1j$y)])
new.G1j<-lm(((Group1.ab$fulton^lambda.G1j-1)/lambda.G1j)~Group1.ab$fat.g)
#plot(new.G1j)
summary(new.G1j)
ols_test_normality(new.G1j)

#SVL
plot((Group1.ab$fulton~Group1.ab$Intake.SVL), main="Group1.ab tegu body condition vs. SVL")
abline(lm(Group1.ab$fulton~Group1.ab$Intake.SVL), col="red")
G1svl.line<-(lm(Group1.ab$fulton~Group1.ab$Intake.SVL))
#plot(G1svl.line)
summary(G1svl.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$Intake.SVL))

#boxcox, sig & normal
bc.G1svl<-boxcox(G1svl.line)
(lambda.G1svl<-bc.G1svl$x[which.max(bc.G1svl$y)])
new.G1svl<-lm(((Group1.ab$fulton^lambda.G1svl-1)/lambda.G1svl)~Group1.ab$fat.g)
#plot(new.G1svl)
summary(new.G1svl)
ols_test_normality(new.G1svl)

#percent fat
#tegu.data$percfat <- with(tegu.data, fat.g/mass.g*100)##calculating fat percentage
plot((Group1.ab$fulton~Group1.ab$percfat), main="Group1.ab tegu body condition vs. percent fat")
abline(lm(Group1.ab$fulton~Group1.ab$percfat), col="red")
G1pf.line<-(lm(Group1.ab$fulton~Group1.ab$percfat))
#plot(G1pf.line)
summary(G1pf.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$percfat))

#boxcox, sig & normal
bc.G1pf<-boxcox(G1pf.line)
(lambda.G1pf<-bc.G1pf$x[which.max(bc.G1pf$y)])
new.G1pf<-lm(((Group1.ab$fulton^lambda.G1pf-1)/lambda.G1pf)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1pf)
ols_test_normality(new.G1pf)

#fat mass, sig & normal
#tegu.data$percfat <- with(tegu.data, fat.g/mass.g*100)##calculating fat percentage
plot((Group1.ab$fulton~Group1.ab$fat.g), main="Group1.ab tegu body condition vs. fat weight")
abline(lm(Group1.ab$fulton~Group1.ab$fat.g), col="red")
G1fm.line<-(lm(Group1.ab$fulton~Group1.ab$fat.g))
#plot(G1fm.line)
summary(G1fm.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$fat.g))

#Average monthly temperature
plot((Group1.ab$fulton~Group1.ab$Avg.Temp.C), main="Group1.ab tegu body condition vs. average temperature")
abline(lm(Group1.ab$fulton~Group1.ab$Avg.Temp.C), col="red")
G1temp.line<-(lm(Group1.ab$fulton~Group1.ab$Avg.Temp.C))
#plot(G1temp.line)
summary(G1temp.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$Avg.Temp.C))

#boxcox, sig & normal
bc.G1temp<-boxcox(G1temp.line)
(lambda.G1temp<-bc.G1temp$x[which.max(bc.G1temp$y)])
new.G1temp<-lm(((Group1.ab$fulton^lambda.G1temp-1)/lambda.G1temp)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1temp)
ols_test_normality(new.G1temp)

#average monthly rainfall
plot((Group1.ab$fulton~Group1.ab$Rain.cm), main="Group1.ab tegu body condition vs. average rainfall")
abline(lm(Group1.ab$fulton~Group1.ab$Rain.cm), col="red")
G1rain.line<-(lm(Group1.ab$fulton~Group1.ab$Rain.cm))
#plot(G1rain.line)
summary(G1rain.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$Rain.cm))

#boxcox, sig & normal
bc.G1rain<-boxcox(G1fm.line)
(lambda.G1rain<-bc.G1rain$x[which.max(bc.G1rain$y)])
new.G1rain<-lm(((Group1.ab$fulton^lambda.G1rain-1)/lambda.G1rain)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1rain)
ols_test_normality(new.G1rain)

#minimum monthly temp
plot((Group1.ab$fulton~Group1.ab$Min.Temp.C), main="Group1.ab tegu body condition vs. Min Temp")
abline(lm(Group1.ab$fulton~Group1.ab$Min.Temp.C), col="red")
G1mint.line<-(lm(Group1.ab$fulton~Group1.ab$Min.Temp.C))
#plot(G1mint.line)
summary(G1mint.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$Min.Temp.C))

#boxcox, significant & normal
bc.G1mint<-boxcox(G1mint.line)
(lambda.G1mint<-bc.G1mint$x[which.max(bc.G1mint$y)])
new.G1mint<-lm(((Group1.ab$fulton^lambda.G1mint-1)/lambda.G1mint)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1mint)
ols_test_normality(new.G1mint)

#max monthly temp
plot((Group1.ab$fulton~Group1.ab$Max.Temp.C), main="Group1.ab tegu body condition vs. Max Temp")
abline(lm(Group1.ab$fulton~Group1.ab$Max.Temp.C), col="red")
G1maxt.line<-(lm(Group1.ab$fulton~Group1.ab$Max.Temp.C))
#plot(G1maxt.line)
summary(G1maxt.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$Max.Temp.C))

#boxcox, sig & normal
bc.G1maxt<-boxcox(G1maxt.line)
(lambda.G1maxt<-bc.G1maxt$x[which.max(bc.G1maxt$y)])
new.G1maxt<-lm(((Group1.ab$fulton^lambda.G1maxt-1)/lambda.G1maxt)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1maxt)
ols_test_normality(new.G1maxt)

#rain max
plot((Group1.ab$fulton~Group1.ab$Rain.Max.15.cm), main="Group1.ab tegu body condition vs. Rain Max")
abline(lm(Group1.ab$fulton~Group1.ab$Rain.Max.15.cm), col="red")
G1maxr.line<-(lm(Group1.ab$fulton~Group1.ab$Rain.Max.15.cm))
#plot(G1maxr.line)
summary(G1maxr.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$Rain.Max.15.cm))

#boxcox, sig & normal
bc.G1maxr<-boxcox(G1maxr.line)
(lambda.G1maxr<-bc.G1maxr$x[which.max(bc.G1maxr$y)])
new.G1maxr<-lm(((Group1.ab$fulton^lambda.G1maxr-1)/lambda.G1maxr)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1maxr)
ols_test_normality(new.G1maxr)

####
#### Group 1 Regressions Fat Mass ####
#day
plot((Group1.ab$fat.g~Group1.ab$julian), main="Group1.ab tegu body condition vs. day of capture")
abline(lm(Group1.ab$fat.g~Group1.ab$julian), col="red")
G1day.line<-(lm(Group1.ab$fat.g~Group1.ab$julian))
#plot(G5day.line)
summary(G1day.line)
ols_test_normality(lm(Group1.ab$fat.g~Group1.ab$julian))

#All adults fat.g's K vs. 
par(mfrow=c(1,1))

#Julian date
plot((Group1.ab$fat.g~Group1.ab$julian), main="Group 1 tegu body condition vs. Julian date")
abline(lm(Group1.ab$fat.g~Group1.ab$julian), col="red")
G1j.line<-(lm(Group1.ab$fat.g~Group1.ab$julian))
#plot(G1j.line)
summary(G1j.line)
ols_test_normality(lm(Group1.ab$fat.g~Group1.ab$julian))

#boxcox - sig & normal
bc.G1j<-boxcox(G1j.line)
(lambda.G1j<-bc.G1j$x[which.max(bc.G1j$y)])
new.G1j<-lm(((Group1.ab$fat.g^lambda.G1j-1)/lambda.G1j)~Group1.ab$fat.g)
#plot(new.G1j)
summary(new.G1j)
ols_test_normality(new.G1j)

#SVL
plot((Group1.ab$fat.g~Group1.ab$Intake.SVL), main="Group1.ab tegu body condition vs. SVL")
abline(lm(Group1.ab$fat.g~Group1.ab$Intake.SVL), col="red")
G1svl.line<-(lm(Group1.ab$fat.g~Group1.ab$Intake.SVL))
#plot(G1svl.line)
summary(G1svl.line)
ols_test_normality(lm(Group1.ab$fat.g~Group1.ab$Intake.SVL))

#boxcox, sig & normal
bc.G1svl<-boxcox(G1svl.line)
(lambda.G1svl<-bc.G1svl$x[which.max(bc.G1svl$y)])
new.G1svl<-lm(((Group1.ab$fat.g^lambda.G1svl-1)/lambda.G1svl)~Group1.ab$fat.g)
#plot(new.G1svl)
summary(new.G1svl)
ols_test_normality(new.G1svl)

#percent fat
#tegu.data$percfat <- with(tegu.data, fat.g/mass.g*100)##calculating fat percentage
plot((Group1.ab$fat.g~Group1.ab$percfat), main="Group1.ab tegu body condition vs. percent fat")
abline(lm(Group1.ab$fat.g~Group1.ab$percfat), col="red")
G1pf.line<-(lm(Group1.ab$fat.g~Group1.ab$percfat))
#plot(G1pf.line)
summary(G1pf.line)
ols_test_normality(lm(Group1.ab$fat.g~Group1.ab$percfat))

#boxcox, sig & normal
bc.G1pf<-boxcox(G1pf.line)
(lambda.G1pf<-bc.G1pf$x[which.max(bc.G1pf$y)])
new.G1pf<-lm(((Group1.ab$fat.g^lambda.G1pf-1)/lambda.G1pf)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1pf)
ols_test_normality(new.G1pf)

#fat mass, sig & normal
#tegu.data$percfat <- with(tegu.data, fat.g/mass.g*100)##calculating fat percentage
plot((Group1.ab$fat.g~Group1.ab$fat.g), main="Group1.ab tegu body condition vs. fat weight")
abline(lm(Group1.ab$fat.g~Group1.ab$fat.g), col="red")
G1fm.line<-(lm(Group1.ab$fat.g~Group1.ab$fat.g))
#plot(G1fm.line)
summary(G1fm.line)
ols_test_normality(lm(Group1.ab$fat.g~Group1.ab$fat.g))

#Average monthly temperature
plot((Group1.ab$fat.g~Group1.ab$Avg.Temp.C), main="Group1.ab tegu body condition vs. average temperature")
abline(lm(Group1.ab$fat.g~Group1.ab$Avg.Temp.C), col="red")
G1temp.line<-(lm(Group1.ab$fat.g~Group1.ab$Avg.Temp.C))
#plot(G1temp.line)
summary(G1temp.line)
ols_test_normality(lm(Group1.ab$fat.g~Group1.ab$Avg.Temp.C))

#boxcox, sig & normal
bc.G1temp<-boxcox(G1temp.line)
(lambda.G1temp<-bc.G1temp$x[which.max(bc.G1temp$y)])
new.G1temp<-lm(((Group1.ab$fat.g^lambda.G1temp-1)/lambda.G1temp)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1temp)
ols_test_normality(new.G1temp)

#average monthly rainfall
plot((Group1.ab$fat.g~Group1.ab$Rain.cm), main="Group1.ab tegu body condition vs. average rainfall")
abline(lm(Group1.ab$fat.g~Group1.ab$Rain.cm), col="red")
G1rain.line<-(lm(Group1.ab$fat.g~Group1.ab$Rain.cm))
#plot(G1rain.line)
summary(G1rain.line)
ols_test_normality(lm(Group1.ab$fat.g~Group1.ab$Rain.cm))

#boxcox, sig & normal
bc.G1rain<-boxcox(G1fm.line)
(lambda.G1rain<-bc.G1rain$x[which.max(bc.G1rain$y)])
new.G1rain<-lm(((Group1.ab$fat.g^lambda.G1rain-1)/lambda.G1rain)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1rain)
ols_test_normality(new.G1rain)

#minimum monthly temp
plot((Group1.ab$fat.g~Group1.ab$Min.Temp.C), main="Group1.ab tegu body condition vs. Min Temp")
abline(lm(Group1.ab$fat.g~Group1.ab$Min.Temp.C), col="red")
G1mint.line<-(lm(Group1.ab$fat.g~Group1.ab$Min.Temp.C))
#plot(G1mint.line)
summary(G1mint.line)
ols_test_normality(lm(Group1.ab$fat.g~Group1.ab$Min.Temp.C))

#boxcox, significant & normal
bc.G1mint<-boxcox(G1mint.line)
(lambda.G1mint<-bc.G1mint$x[which.max(bc.G1mint$y)])
new.G1mint<-lm(((Group1.ab$fat.g^lambda.G1mint-1)/lambda.G1mint)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1mint)
ols_test_normality(new.G1mint)

#max monthly temp
plot((Group1.ab$fat.g~Group1.ab$Max.Temp.C), main="Group1.ab tegu body condition vs. Max Temp")
abline(lm(Group1.ab$fat.g~Group1.ab$Max.Temp.C), col="red")
G1maxt.line<-(lm(Group1.ab$fat.g~Group1.ab$Max.Temp.C))
#plot(G1maxt.line)
summary(G1maxt.line)
ols_test_normality(lm(Group1.ab$fat.g~Group1.ab$Max.Temp.C))

#boxcox, sig & normal
bc.G1maxt<-boxcox(G1maxt.line)
(lambda.G1maxt<-bc.G1maxt$x[which.max(bc.G1maxt$y)])
new.G1maxt<-lm(((Group1.ab$fat.g^lambda.G1maxt-1)/lambda.G1maxt)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1maxt)
ols_test_normality(new.G1maxt)

#rain max
plot((Group1.ab$fat.g~Group1.ab$Rain.Max.15.cm), main="Group1.ab tegu body condition vs. Rain Max")
abline(lm(Group1.ab$fat.g~Group1.ab$Rain.Max.15.cm), col="red")
G1maxr.line<-(lm(Group1.ab$fat.g~Group1.ab$Rain.Max.15.cm))
#plot(G1maxr.line)
summary(G1maxr.line)
ols_test_normality(lm(Group1.ab$fat.g~Group1.ab$Rain.Max.15.cm))

#boxcox, sig & normal
bc.G1maxr<-boxcox(G1maxr.line)
(lambda.G1maxr<-bc.G1maxr$x[which.max(bc.G1maxr$y)])
new.G1maxr<-lm(((Group1.ab$fat.g^lambda.G1maxr-1)/lambda.G1maxr)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1maxr)
ols_test_normality(new.G1maxr)

####
#### Group 1 Distance Regressions####
#G1 distance from US1 underpass
plot((Group1.ab$fulton~Group1.ab$UP.DIST), main="Group1.ab tegu body condition vs. distance from the US 1 Underpass")
abline(lm(Group1.ab$fulton~Group1.ab$UP.DIST), col="red")
G1dist.line<-(lm(Group1.ab$fulton~Group1.ab$UP.DIST))
#plot(G1dist.line)
summary(G1dist.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$UP.DIST))

#boxcox, sig & normal
bc.G1up<-boxcox(G1dist.line)
(lambda.G1up<-bc.G1up$x[which.max(bc.G1up$y)])
new.G1up<-lm(((Group1.ab$fulton^lambda.G1up-1)/lambda.G1up)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1up)
ols_test_normality(new.G1up)

#G1 distance from JDC
plot((Group1.ab$fulton~Group1.ab$JDC.DIST), main="Group1.ab tegu body condition vs. distance from detention center ")
abline(lm(Group1.ab$fulton~Group1.ab$JDC.DIST), col="red")
G1dist.jdc.line<-(lm(Group1.ab$fulton~Group1.ab$JDC.DIST))
#plot(G1dist.jdc.line)
summary(G1dist.jdc.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$JDC.DIST))

#boxcox, sig & normal
bc.G1jdc<-boxcox(G1dist.jdc.line)
(lambda.G1jdc<-bc.G1jdc$x[which.max(bc.G1jdc$y)])
new.G1jdc<-lm(((Group1.ab$fulton^lambda.G1jdc-1)/lambda.G1jdc)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1jdc)
ols_test_normality(new.G1jdc)

#Group1.ab distance from Pond
plot((Group1.ab$fulton~Group1.ab$Pond.DIST), main="Group1.ab tegu body condition vs. distance from pond")
abline(lm(Group1.ab$fulton~Group1.ab$Pond.DIST), col="red")
G1dist.pond.line<-(lm(Group1.ab$fulton~Group1.ab$Pond.DIST))
#plot(G1dist.pond.line)
summary(G1dist.pond.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$Pond.DIST))

#boxcox, sig & normal
bc.G1pond<-boxcox(G1dist.pond.line)
(lambda.G1pond<-bc.G1pond$x[which.max(bc.G1pond$y)])
new.G1pond<-lm(((Group1.ab$fulton^lambda.G1pond-1)/lambda.G1pond)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1pond)
ols_test_normality(new.G1pond)

#Group1.ab distance from C-110/424th intersection
plot((Group1.ab$fulton~Group1.ab$C110424.DIST), main="Group1.ab tegu body condition vs. distance from C-110/424th intersection")
abline(lm(Group1.ab$fulton~Group1.ab$C110424.DIST), col="red")
G1dist.x110424.line<-(lm(Group1.ab$fulton~Group1.ab$C110424.DIST))
#plot(G1dist.x110424.line)
summary(G1dist.x110424.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$C110424.DIST))

#boxcox, sig & normal
bc.G1c110<-boxcox(G1dist.x110424.line)
(lambda.G1c110<-bc.G1c110$x[which.max(bc.G1c110$y)])
new.G1c110<-lm(((Group1.ab$fulton^lambda.G1c110-1)/lambda.G1c110)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1c110)
ols_test_normality(new.G1c110)

#Group1.ab distance from water control structure
plot((Group1.ab$fulton~Group1.ab$WCS.DIST), main="Group1.ab tegu body condition vs. distance from water control structure")
abline(lm(Group1.ab$fulton~Group1.ab$WCS.DIST), col="red")
G1dist.wcs.line<-(lm(Group1.ab$fulton~Group1.ab$WCS.DIST))
#plot(G1dist.wcs.line)
summary(G1dist.wcs.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$WCS.DIST))

#boxcox, sig & normal
bc.G1wcs<-boxcox(G1dist.wcs.line)
(lambda.G1wcs<-bc.G1wcs$x[which.max(bc.G1wcs$y)])
new.G1wcs<-lm(((Group1.ab$fulton^lambda.G1wcs-1)/lambda.G1wcs)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1wcs)
ols_test_normality(new.G1wcs)

#Group1.ab distance from c-110/C-111 intersection
plot((Group1.ab$fulton~Group1.ab$C110111.DIST), main="Group1.ab tegu body condition vs. distance from C-110/C-111 intersection")
abline(lm(Group1.ab$fulton~Group1.ab$C110111.DIST), col="red")
G1dist.x110111.line<-(lm(Group1.ab$fulton~Group1.ab$C110111.DIST))
#plot(G1dist.x110111.line)
summary(G1dist.x110111.line)
ols_test_normality(lm(Group1.ab$fulton~Group1.ab$C110111.DIST))

#boxcox, sig & normal
bc.G1111<-boxcox(G1dist.x110111.line)
(lambda.G1111<-bc.G1111$x[which.max(bc.G1111$y)])
new.G1111<-lm(((Group1.ab$fulton^lambda.G1111-1)/lambda.G1111)~Group1.ab$fat.g)
#plot(new.G1)
summary(new.G1111)
ols_test_normality(new.G1111)

####

#### Group 1 multiple linear regressions####

#all
G1.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
               Rain.cm + Min.Temp.C + Max.Temp.C + 
               julian + y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
               d1 + d2 + d3 + d4 +d5, data = Group1.ab, na.action = "na.fail")
summary(G1.mlr)
#plot(G1.mlr)
ols_test_normality(G1.mlr)
ols_vif_tol(G1.mlr)


#continuous
G1.2.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
                 Rain.cm + Min.Temp.C + Max.Temp.C + 
                 julian, data = Group1.ab, na.action = "na.fail")
summary(G1.2.mlr)
#plot(G1.2.mlr)
ols_test_normality(G1.2.mlr)

#dummy
G1.3.mlr <- lm(fulton ~ y14 + y15 + y16 + y17 + y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
                 d1 + d2 + d3 + d4 +d5, data = Group1.ab, na.action = "na.fail")
summary(G1.3.mlr)
#plot(G1.3.mlr)
ols_test_normality(G1.3.mlr)

#distance
G1.4.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST, 
               data = Group1.ab, na.action = "na.fail")
summary(G1.4.mlr)
#plot(G1.4.mlr)
ols_test_normality(G1.4.mlr)

#boxcox
bc.G1.4 <- boxcox(G1.4.mlr)
LambdaG1.4 <- bc.G1.4$x[which.max(bc.G1.4$y)]
new.G1.4.mlr <- lm((Group1.ab$fulton^LambdaG1.4-1/LambdaG1.4)~UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST, 
                   data = Group1.ab, na.action = "na.fail")
#plot(new.12.5.mlr)
ols_test_normality(new.G1.4.mlr)
summary(new.G1.4.mlr)

#environmental
G1.5.mlr <- lm(fulton ~ Rain.cm + Min.Temp.C + Max.Temp.C, 
               data = Group1.ab, na.action = "na.fail")
summary(G1.5.mlr)
#plot(G1.5.mlr)
ols_test_normality(G1.5.mlr)
ols_vif_tol(G1.5.mlr)

#boxcox
bc.G1.5 <- boxcox(G1.5.mlr)
LambdaG1.5 <- bc.G1.5$x[which.max(bc.G1.5$y)]
new.G1.5.mlr <- lm((Group1.ab$fulton^LambdaG1.5-1/LambdaG1.5)~Rain.cm + Min.Temp.C + Max.Temp.C, 
                   data = Group1.ab, na.action = "na.fail")
#plot(new.12.5.mlr)
ols_test_normality(new.G1.5.mlr)
summary(new.G1.5.mlr)

#temporal
G1.6.mlr <- lm(fulton ~ julian + y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12, 
               data = Group1.ab, na.action = "na.fail")
summary(G1.6.mlr)
#plot(G1.6.mlr)
ols_test_normality(G1.6.mlr)

#habitat
G1.7.mlr <- lm(fulton ~ d1 + d2 + d3 + d4 + d5, data = Group1.ab, na.action = "na.fail")
summary(G1.7.mlr)
#plot(G1.7.mlr)
ols_test_normality(G1.7.mlr)

#boxcox
bc.G1.7 <- boxcox(G1.7.mlr)
LambdaG1.7 <- bc.G1.7$x[which.max(bc.G1.7$y)]
new.G1.7.mlr <- lm((Group1.ab$fulton^LambdaG1.7-1/LambdaG1.7)~d1 + d2 + d3 + d4 + d5, 
                   data = Group1.ab, na.action = "na.fail")
#plot(new.12.5.mlr)
ols_test_normality(new.G1.7.mlr)
summary(new.G1.7.mlr)

#no NAs
G1.8.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
                 Rain.cm + Min.Temp.C + Max.Temp.C + 
                 julian + y14 + y15 + y16 + y17 +y18 + m4 + m5 + m6 + m7 + m8 + m9 +
                 d1 + d2 + d4, data = Group1.ab, na.action = "na.fail")
summary(G1.8.mlr)
#plot(G1.8.mlr)
ols_test_normality(G1.8.mlr)

#significant V only
G1.9.mlr <- lm(fulton ~ C110424.DIST , data = Group1.ab, na.action = "na.fail")
summary(G1.9.mlr)
#plot(G1.9.mlr)
ols_test_normality(G1.9.mlr)

#boxcox
bc.G1.9 <- boxcox(G1.9.mlr)
LambdaG1.9 <- bc.G1.9$x[which.max(bc.G1.9$y)]
new.G1.9.mlr <- lm((Group1.ab$fulton^LambdaG1.9-1/LambdaG1.9)~C110424.DIST, 
                   data = Group1.ab, na.action = "na.fail")
#plot(new.12.5.mlr)
ols_test_normality(new.G1.9.mlr)
summary(new.G1.9.mlr)

####

####MLR fat.g ####
#all
G1.mlr <- lm(fat.g ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
               Rain.cm + Min.Temp.C + Max.Temp.C + 
               julian + y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
               d1 + d2 + d3 + d4 +d5, data = Group1.ab, na.action = "na.omit")
summary(G1.mlr)
#plot(G1.mlr)
ols_test_normality(G1.mlr)
ols_vif_tol(G1.mlr)


#continuous
G1.2.mlr <- lm(fat.g ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
                 Rain.cm + Min.Temp.C + Max.Temp.C + 
                 julian, data = Group1.ab, na.action = "na.omit")
summary(G1.2.mlr)
#plot(G1.2.mlr)
ols_test_normality(G1.2.mlr)

#dummy
G1.3.mlr <- lm(fat.g ~ y14 + y15 + y16 + y17 + y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
                 d1 + d2 + d3 + d4 +d5, data = Group1.ab, na.action = "na.omit")
summary(G1.3.mlr)
#plot(G1.3.mlr)
ols_test_normality(G1.3.mlr)

#distance
G1.4.mlr <- lm(fat.g ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST, 
               data = Group1.ab, na.action = "na.omit")
summary(G1.4.mlr)
#plot(G1.4.mlr)
ols_test_normality(G1.4.mlr)

#boxcox
bc.G1.4 <- boxcox(G1.4.mlr)
LambdaG1.4 <- bc.G1.4$x[which.max(bc.G1.4$y)]
new.G1.4.mlr <- lm((Group1.ab$fat.g^LambdaG1.4-1/LambdaG1.4)~UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST, 
                   data = Group1.ab, na.action = "na.omit")
#plot(new.12.5.mlr)
ols_test_normality(new.G1.4.mlr)
summary(new.G1.4.mlr)

#environmental
G1.5.mlr <- lm(fat.g ~ Rain.cm + Min.Temp.C + Max.Temp.C, 
               data = Group1.ab, na.action = "na.omit")
summary(G1.5.mlr)
#plot(G1.5.mlr)
ols_test_normality(G1.5.mlr)
ols_vif_tol(G1.5.mlr)

#boxcox
bc.G1.5 <- boxcox(G1.5.mlr)
LambdaG1.5 <- bc.G1.5$x[which.max(bc.G1.5$y)]
new.G1.5.mlr <- lm((Group1.ab$fat.g^LambdaG1.5-1/LambdaG1.5)~Rain.cm + Min.Temp.C + Max.Temp.C, 
                   data = Group1.ab, na.action = "na.omit")
#plot(new.12.5.mlr)
ols_test_normality(new.G1.5.mlr)
summary(new.G1.5.mlr)

#temporal
G1.6.mlr <- lm(fat.g ~ julian + y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12, 
               data = Group1.ab, na.action = "na.omit")
summary(G1.6.mlr)
#plot(G1.6.mlr)
ols_test_normality(G1.6.mlr)

#habitat
G1.7.mlr <- lm(fat.g ~ d1 + d2 + d3 + d4 + d5, data = Group1.ab, na.action = "na.omit")
summary(G1.7.mlr)
#plot(G1.7.mlr)
ols_test_normality(G1.7.mlr)

#boxcox
bc.G1.7 <- boxcox(G1.7.mlr)
LambdaG1.7 <- bc.G1.7$x[which.max(bc.G1.7$y)]
new.G1.7.mlr <- lm((Group1.ab$fat.g^LambdaG1.7-1/LambdaG1.7)~d1 + d2 + d3 + d4 + d5, 
                   data = Group1.ab, na.action = "na.omit")
#plot(new.12.5.mlr)
ols_test_normality(new.G1.7.mlr)
summary(new.G1.7.mlr)

#no NAs
G1.8.mlr <- lm(fat.g ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
                 Rain.cm + Min.Temp.C + Max.Temp.C + 
                 julian + y14 + y15 + y16 + y17 +y18 + m4 + m5 + m6 + m7 + m8 + m9 +
                 d1 + d2 + d4, data = Group1.ab, na.action = "na.omit")
summary(G1.8.mlr)
#plot(G1.8.mlr)
ols_test_normality(G1.8.mlr)

#significant V only
G1.9.mlr <- lm(fat.g ~ UP.DIST + JDC.DIST + Pond.DIST +  Min.Temp.C +  
                 julian + m4 + m5 + m9 +
                 d2 + d4, data = Group1.ab, na.action = "na.omit")
summary(G1.9.mlr)
#plot(G1.9.mlr)
ols_test_normality(G1.9.mlr)

#boxcox
bc.G1.9 <- boxcox(G1.9.mlr)
LambdaG1.9 <- bc.G1.9$x[which.max(bc.G1.9$y)]
new.G1.9.mlr <- lm((Group1.ab$fat.g^LambdaG1.9-1/LambdaG1.9)~UP.DIST + JDC.DIST + Pond.DIST +  Min.Temp.C +  
                     julian + m4 + m5 + m9 +
                     d2 + d4, 
                   data = Group1.ab, na.action = "na.omit")
#plot(new.12.5.mlr)
ols_test_normality(new.G1.9.mlr)
summary(new.G1.9.mlr)


#### Group 2 Regressions..####

#Average monthly temperature
plot((Group2.ab$fulton~Group2.ab$Avg.Temp.C), main="Group2.ab tegu body condition vs. average temperature")
abline(lm(Group2.ab$fulton~Group2.ab$Avg.Temp.C), col="red")
G2temp.line<-(lm(Group2.ab$fulton~Group2.ab$Avg.Temp.C))
#plot(G2temp.line)
summary(G2temp.line)
ols_test_normality(lm(Group2.ab$fulton~Group2.ab$Avg.Temp.C))

#boxcox, sig & normal
bc.G2temp<-boxcox(G2temp.line)
(lambda.G2temp<-bc.G2temp$x[which.max(bc.G2temp$y)])
new.G2temp<-lm(((Group2.ab$fulton^lambda.G2temp-1)/lambda.G2temp)~Group2.ab$Avg.Temp.C)
#plot(new.G2temp)
summary(new.G2temp)
ols_test_normality(new.G2temp)

#average monthly rainfall
plot((Group2.ab$fulton~Group2.ab$Rain.cm), main="Group2.ab tegu body condition vs. average rainfall")
abline(lm(Group2.ab$fulton~Group2.ab$Rain.cm), col="red")
G2rain.line<-(lm(Group2.ab$fulton~Group2.ab$Rain.cm))
#plot(G2rain.line)
summary(G2rain.line)
ols_test_normality(lm(Group2.ab$fulton~Group2.ab$Rain.cm))

#boxcox, sig & normal
bc.G2rain<-boxcox(G2rain.line)
(lambda.G2rain<-bc.G2rain$x[which.max(bc.G2rain$y)])
new.G2rain<-lm(((Group2.ab$fulton^lambda.G2rain-1)/lambda.G2rain)~Group2.ab$Rain.cm)
#plot(new.G2rain)
summary(new.G2rain)
ols_test_normality(new.G2rain)

#minimum monthly temp
plot((Group2.ab$fulton~Group2.ab$Min.Temp.C), main="Group2.ab tegu body condition vs. Min Temp")
abline(lm(Group2.ab$fulton~Group2.ab$Min.Temp.C), col="red")
G2mint.line<-(lm(Group2.ab$fulton~Group2.ab$Min.Temp.C))
#plot(G2mint.line)
summary(G2mint.line)
ols_test_normality(lm(Group2.ab$fulton~Group2.ab$Min.Temp.C))

#boxcox, sig not normal
bc.G2mint<-boxcox(G2mint.line)
(lambda.G2mint<-bc.G2mint$x[which.max(bc.G2mint$y)])
new.G2mint<-lm(((Group2.ab$fulton^lambda.G2mint-1)/lambda.G2mint)~Group2.ab$Min.Temp.C)
#plot(new.G2mint)
summary(new.G2mint)
ols_test_normality(new.G2mint)

#max monthly temp
plot((Group2.ab$fulton~Group2.ab$Max.Temp.C), main="Group2.ab tegu body condition vs. Max Temp")
abline(lm(Group2.ab$fulton~Group2.ab$Max.Temp.C), col="red")
G2maxt.line<-(lm(Group2.ab$fulton~Group2.ab$Max.Temp.C))
#plot(G2maxt.line)
summary(G2maxt.line)
ols_test_normality(lm(Group2.ab$fulton~Group2.ab$Max.Temp.C))

#boxcox, not sig normal
bc.G2maxt<-boxcox(G2maxt.line)
(lambda.G2maxt<-bc.G2maxt$x[which.max(bc.G2maxt$y)])
new.G2maxt<-lm(((Group2.ab$fulton^lambda.G2maxt-1)/lambda.G2maxt)~Group2.ab$Max.Temp.C)
#plot(new.G2maxt)
summary(new.G2maxt)
ols_test_normality(new.G2maxt)

#rain max, normal not sig
plot((Group2.ab$fulton~Group2.ab$Rain.Max.15.cm), main="Group2.ab tegu body condition vs. Rain Max")
abline(lm(Group2.ab$fulton~Group2.ab$Rain.Max.15.cm), col="red")
G2maxr.line<-(lm(Group2.ab$fulton~Group2.ab$Rain.Max.15.cm))
#plot(G2maxr.line)
summary(G2maxr.line)
ols_test_normality(lm(Group2.ab$fulton~Group2.ab$Rain.Max.15.cm))

#boxcox, sig &  normal
bc.G2maxr<-boxcox(G2maxr.line)
(lambda.G2maxr<-bc.G2maxr$x[which.max(bc.G2maxr$y)])
new.G2maxr<-lm(((Group2.ab$fulton^lambda.G2maxr-1)/lambda.G2maxr)~Group2.ab$Rain.Max.15.cm)
#plot(new.G2maxr)
summary(new.G2maxr)
ols_test_normality(new.G2maxr)

#day
plot((Group2.ab$fulton~Group2.ab$julian), main="Group2.ab tegu body condition vs. day of capture")
abline(lm(Group2.ab$fulton~Group2.ab$julian), col="red")
G2day.line<-(lm(Group2.ab$fulton~Group2.ab$julian))
#plot(G5day.line)
summary(G2day.line)
ols_test_normality(lm(Group2.ab$fulton~Group2.ab$julian))

#boxcox, sig & normal
bc.G2day<-boxcox(G2day.line)
(lambda.G2day<-bc.G2day$x[which.max(bc.G2day$y)])
new.G2day<-lm(((Group2.ab$fulton^lambda.G2day-1)/lambda.G2day)~Group2.ab$julian)
#plot(new.G2maxr)
summary(new.G2day)
ols_test_normality(new.G2day)

####
#### Group 2 Distance Regressions..####
#Group 2 distance from US1 underpass
plot((Group2.ab$fulton~Group2.ab$UP.DIST), main="Group 2 tegu body condition vs. distance from the US 1 Underpass")
abline(lm(Group2.ab$fulton~Group2.ab$UP.DIST), col="red")
G2dist.line<-(lm(Group2.ab$fulton~Group2.ab$UP.DIST))
#plot(G2dist.line)
summary(G2dist.line)
ols_test_normality(lm(Group2.ab$fulton~Group2.ab$UP.DIST))

#boxcox, normal & sig
bc.G2UP<-boxcox(G2dist.line)
(lambda.G2UP<-bc.G2UP$x[which.max(bc.G2UP$y)])
new.G2UP<-lm(((Group2.ab$fulton^lambda.G2UP-1)/lambda.G2UP)~Group2.ab$UP.DIST)
#plot(new.G2UP)
summary(new.G2UP)
ols_test_normality(new.G2UP)

#Group 2 distance from JDC,
plot((Group2.ab$fulton~Group2.ab$JDC.DIST), main="Group 2 tegu body condition vs. distance from detention center ")
abline(lm(Group2.ab$fulton~Group2.ab$JDC.DIST), col="red")
G2dist.jdc.line<-(lm(Group2.ab$fulton~Group2.ab$JDC.DIST))
#plot(G2dist.jdc.line)
summary(G2dist.jdc.line)
ols_test_normality(lm(Group2.ab$fulton~Group2.ab$JDC.DIST))

#boxcox, normal and sig
bc.G2jdc<-boxcox(G2dist.jdc.line)
(lambda.G2jdc<-bc.G2jdc$x[which.max(bc.G2jdc$y)])
new.G2jdc<-lm(((Group2.ab$fulton^lambda.G2jdc-1)/lambda.G2jdc)~Group2.ab$JDC.DIST)
#plot(new.G2jdc)
summary(new.G2jdc)
ols_test_normality(new.G2jdc)

#Group 2 distance from Pond
plot((Group2.ab$fulton~Group2.ab$Pond.DIST), main="Group 2 tegu body condition vs. distance from pond")
abline(lm(Group2.ab$fulton~Group2.ab$Pond.DIST), col="red")
G2dist.pond.line<-(lm(Group2.ab$fulton~Group2.ab$Pond.DIST))
#plot(G2dist.pond.line)
summary(G2dist.pond.line)
ols_test_normality(lm(Group2.ab$fulton~Group2.ab$Pond.DIST))

#boxcox, sig and normal
bc.G2pond<-boxcox(G2dist.pond.line)
(lambda.G2pond<-bc.G2pond$x[which.max(bc.G2pond$y)])
new.G2pond<-lm(((Group2.ab$fulton^lambda.G2pond-1)/lambda.G2pond)~Group2.ab$Pond.DIST)
#plot(new.G2pond)
summary(new.G2pond)
ols_test_normality(new.G2pond)

#Group 2 distance from C-110/424th intersection
plot((Group2.ab$fulton~Group2.ab$C110424.DIST), main="Group 2 tegu body condition vs. distance from C-110/424th intersection")
abline(lm(Group2.ab$fulton~Group2.ab$C110424.DIST), col="red")
G2dist.x110424.line<-(lm(Group2.ab$fulton~Group2.ab$C110424.DIST))
#plot(G2dist.x110424.line)
summary(G2dist.x110424.line)
ols_test_normality(lm(Group2.ab$fulton~Group2.ab$C110424.DIST))

#boxcox, sig and normal
bc.G2x110424<-boxcox(G2dist.x110424.line)
(lambda.G2x110424<-bc.G2x110424$x[which.max(bc.G2x110424$y)])
new.G2x110424<-lm(((Group2.ab$fulton^lambda.G2x110424-1)/lambda.G2x110424)~Group2.ab$C110424.DIST)
#plot(new.G2x110424)
summary(new.G2x110424)
ols_test_normality(new.G2x110424)

#Group 2 distance from water control structure
plot((Group2.ab$fulton~Group2.ab$WCS.DIST), main="Group 2 tegu body condition vs. distance from water control structure")
abline(lm(Group2.ab$fulton~Group2.ab$WCS.DIST), col="red")
G2dist.wcs.line<-(lm(Group2.ab$fulton~Group2.ab$WCS.DIST))
#plot(G2dist.wcs.line)
summary(G2dist.wcs.line)
ols_test_normality(lm(Group2.ab$fulton~Group2.ab$WCS.DIST))

#boxcox, not sig and normal
bc.G2wcs<-boxcox(G2dist.wcs.line)
(lambda.G2wcs<-bc.G2wcs$x[which.max(bc.G2wcs$y)])
new.G2wcs<-lm(((Group2.ab$fulton^lambda.G2wcs-1)/lambda.G2wcs)~Group2.ab$WCS.DIST)
#plot(new.G2wcs)
summary(new.G2wcs)
ols_test_normality(new.G2wcs)

#Group 2 distance from c-110/C-111 intersection
plot((Group2.ab$fulton~Group2.ab$C110111.DIST), main="Group 2 tegu body condition vs. distance from C-110/C-111 intersection")
abline(lm(Group2.ab$fulton~Group2.ab$C110111.DIST), col="red")
G2dist.x110111.line<-(lm(Group2.ab$fulton~Group2.ab$C110111.DIST))
#plot(G2dist.x110111.line)
summary(G2dist.x110111.line)
ols_test_normality(lm(Group2.ab$fulton~Group2.ab$C110111.DIST))

#boxcox, not sig and normal
bc.G2c111<-boxcox(G2dist.x110111.line)
(lambda.G2c111<-bc.G2c111$x[which.max(bc.G2c111$y)])
new.G2c111<-lm(((Group2.ab$fulton^lambda.G2c111-1)/lambda.G2c111)~Group2.ab$C110111.DIST)
#plot(new.G2wcs)
summary(new.G2c111)
ols_test_normality(new.G2c111)

####

#### Group 2 multiple linear regressions####
#all
G2.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
               Rain.cm + Min.Temp.C + Max.Temp.C + 
               julian + y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
               d1 + d2 + d3 + d4 + d5, data = Group2.ab, na.action = "na.fail")
summary(G2.mlr)
#plot(G2.mlr)
ols_test_normality(G2.mlr)

#boxcox
bc.G2 <- boxcox(G2.mlr)
LambdaG2 <- bc.G2$x[which.max(bc.G2$y)]
new.G2.mlr <- lm((Group2.ab$fulton^LambdaG2-1/LambdaG2)~UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
                   Rain.cm + Min.Temp.C + Max.Temp.C + 
                   julian + y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
                   d1 + d2 + d3 + d4 + d5, data = Group2.ab, na.action = "na.fail")
#plot(new.G2.mlr)
ols_test_normality(new.G2.mlr)
summary(new.G2.mlr)

#Continuous
G2.2.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
                 Rain.cm + Min.Temp.C + Max.Temp.C + 
                 julian, data = Group2.ab, na.action = "na.fail")
summary(G2.2.mlr)
#plot(G2.2.mlr)
ols_test_normality(G2.2.mlr)
#boxcox
bc.G2.2 <- boxcox(G2.2.mlr)
LambdaG2.2 <- bc.G2.2$x[which.max(bc.G2.2$y)]
new.G2.2.mlr <- lm((Group2.ab$fulton^LambdaG2.2-1/LambdaG2.2)~UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
                     Rain.cm + Min.Temp.C + Max.Temp.C + 
                     julian, data = Group2.ab, na.action = "na.fail")
#plot(new.G2.mlr)
ols_test_normality(new.G2.2.mlr)
summary(new.G2.2.mlr)

#Dummy
G2.3.mlr <- lm(fulton ~ y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
                 d1 + d2 + d3 + d4 + d5, data = Group2.ab, na.action = "na.fail")
summary(G2.3.mlr)
#plot(G2.3.mlr)
ols_test_normality(G2.3.mlr)

#boxcox
bc.G2.3 <- boxcox(G2.3.mlr)
LambdaG2.3 <- bc.G2.3$x[which.max(bc.G2.3$y)]
new.G2.3.mlr <- lm((Group2.ab$fulton^LambdaG2.3-1/LambdaG2.3)~ y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
                     d1 + d2 + d3 + d4 + d5, data = Group2.ab, na.action = "na.fail")
#plot(new.G2.3.mlr)
ols_test_normality(new.G2.3.mlr)
summary(new.G2.3.mlr)

#Distance
G2.4.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST, 
               data = Group2.ab, na.action = "na.fail")
summary(G2.4.mlr)
#plot(G2.4.mlr)
ols_test_normality(G2.4.mlr)

#boxcox
bc.G2.4 <- boxcox(G2.4.mlr)
LambdaG2.4 <- bc.G2.4$x[which.max(bc.G2.4$y)]
new.G2.4.mlr <- lm((Group2.ab$fulton^LambdaG2.4-1/LambdaG2.4)~UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST, 
                   data = Group2.ab, na.action = "na.fail")
#plot(new.G2.5.mlr)
ols_test_normality(new.G2.4.mlr)
summary(new.G2.4.mlr)

#Environmental
G2.5.mlr <- lm(fulton ~Rain.cm + Min.Temp.C + Max.Temp.C, 
               data = Group2.ab, na.action = "na.fail")
summary(G2.5.mlr)
#plot(G2.5.mlr)
ols_test_normality(G2.5.mlr)

#boxcox
bc.G2.5 <- boxcox(G2.5.mlr)
LambdaG2.5 <- bc.G2.5$x[which.max(bc.G2.5$y)]
new.G2.5.mlr <- lm((Group2.ab$fulton^LambdaG2.5-1/LambdaG2.5)~Rain.cm + 
                     Min.Temp.C + Max.Temp.C, data = Group2.ab, na.action = "na.fail")
#plot(new.G2.5.mlr)
ols_test_normality(new.G2.5.mlr)
summary(new.G2.5.mlr)

#Temporal
G2.6.mlr <- lm(fulton ~ julian + y14 + y15 + y16 + y17 + y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12, 
               data = Group2.ab, na.action = "na.fail")
summary(G2.6.mlr)
#plot(G2.6.mlr)
ols_test_normality(G2.6.mlr)

#boxcox
bc.G2.6 <- boxcox(G2.6.mlr)
LambdaG2.6 <- bc.G2.6$x[which.max(bc.G2.6$y)]
new.G2.6.mlr <- lm((Group2.ab$fulton^LambdaG2.6-1/LambdaG2.6)~julian + y14 + y15 + y16 + y17 + y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12, 
                   data = Group2.ab, na.action = "na.fail")
#plot(new.G2.6.mlr)
ols_test_normality(new.G2.6.mlr)
summary(new.G2.6.mlr)

#Habitat
G2.7.mlr <- lm(fulton ~ d1 + d2 + d3 + d4 + d5, data = Group2.ab, na.action = "na.fail")
summary(G2.7.mlr)
#plot(G2.7.mlr)
ols_test_normality(G2.7.mlr)

#boxcox
bc.G2.7 <- boxcox(G2.7.mlr)
LambdaG2.7 <- bc.G2.7$x[which.max(bc.G2.7$y)]
new.G2.7.mlr <- lm((Group2.ab$fulton^LambdaG2.7-1/LambdaG2.7)~d1 + d2 + d3 + d4 + d5,
                   data = Group2.ab, na.action = "na.fail")
#plot(new.G2.7.mlr)
ols_test_normality(new.G2.7.mlr)
summary(new.G2.7.mlr)

#What is the best?
G2.8.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
                 Rain.cm + Min.Temp.C + Max.Temp.C + 
                 julian + y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
                 d1 + d2 + d3 + d4 + d5, data = Group2.ab, na.action = "na.fail")
summary(G2.8.mlr)
#plot(G2.8.mlr)
ols_test_normality(G2.8.mlr)
#boxcox
bc.G2.8 <- boxcox(G2.8.mlr)
LambdaG2.8 <- bc.G2.8$x[which.max(bc.G2.8$y)]
new.G2.8.mlr <- lm((Group2.ab$fulton^LambdaG2.8-1/LambdaG2.8)~UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
                     Rain.cm + Min.Temp.C + Max.Temp.C + 
                     julian + y14 + y15 + y16 + y17 +y18 + m3 + m4 + m5 + m6 + m7 + m8 + m9 +
                     d1 + d2 + d3 + d4 , data = Group2.ab, na.action = "na.fail")
#plot(new.G2.mlr)
ols_test_normality(new.G2.8.mlr)
summary(new.G2.8.mlr)


#plot best
G2.9.mlr <- lm(fulton ~ UP.DIST + 
                 Max.Temp.C + y14 +y15 + y17 + m4 + m5 + m6 + m7 + m8 + m9,
               data = Group2.ab, na.action = "na.fail")
summary(G2.9.mlr)
#plot(G2.9.mlr)
ols_test_normality(G2.9.mlr)

#boxcox
bc.G2.9 <- boxcox(G2.9.mlr)
LambdaG2.9 <- bc.G2.9$x[which.max(bc.G2.9$y)]
new.G2.9.mlr <- lm((Group2.ab$fulton^LambdaG2.9-1/LambdaG2.9)~UP.DIST + 
                     Max.Temp.C + y14 +y15 + y17 + m4 + m5 + m6 + m7 + m8 + m9,
                   data = Group2.ab, na.action = "na.fail")
#plot(new.G2.9.mlr)
ols_test_normality(new.G2.9.mlr)
summary(new.G2.9.mlr)


####


#### Group 3 Regressions...####

#Average monthly temperature, normal not sig
plot((Group3.ab$fulton~Group3.ab$Avg.Temp.C), main="Group3.ab tegu body condition vs. average temperature")
abline(lm(Group3.ab$fulton~Group3.ab$Avg.Temp.C), col="red")
G3temp.line<-(lm(Group3.ab$fulton~Group3.ab$Avg.Temp.C))
#plot(G3temp.line)
summary(G3temp.line)
ols_test_normality(lm(Group3.ab$fulton~Group3.ab$Avg.Temp.C))

#average monthly rainfall, normal not sig
plot((Group3.ab$fulton~Group3.ab$Rain.cm), main="Group3.ab tegu body condition vs. average rainfall")
abline(lm(Group3.ab$fulton~Group3.ab$Rain.cm), col="red")
G3rain.line<-(lm(Group3.ab$fulton~Group3.ab$Rain.cm))
#plot(G3rain.line)
summary(G3rain.line)
ols_test_normality(lm(Group3.ab$fulton~Group3.ab$Rain.cm))

#minimum monthly temp,normal not sig
plot((Group3.ab$fulton~Group3.ab$Min.Temp.C), main="Group3.ab tegu body condition vs. Min Temp")
abline(lm(Group3.ab$fulton~Group3.ab$Min.Temp.C), col="red")
G3mint.line<-(lm(Group3.ab$fulton~Group3.ab$Min.Temp.C))
#plot(G3mint.line)
summary(G3mint.line)
ols_test_normality(lm(Group3.ab$fulton~Group3.ab$Min.Temp.C))

#max monthly temp, normal not sig
plot((Group3.ab$fulton~Group3.ab$Max.Temp.C), main="Group3.ab tegu body condition vs. Max Temp")
abline(lm(Group3.ab$fulton~Group3.ab$Max.Temp.C), col="red")
G3maxt.line<-(lm(Group3.ab$fulton~Group3.ab$Max.Temp.C))
#plot(G3maxt.line)
summary(G3maxt.line)
ols_test_normality(lm(Group3.ab$fulton~Group3.ab$Max.Temp.C))


#rain max, normal not sig
plot((Group3.ab$fulton~Group3.ab$Rain.Max.15.cm), main="Group3.ab tegu body condition vs. Rain Max")
abline(lm(Group3.ab$fulton~Group3.ab$Rain.Max.15.cm), col="red")
G3maxr.line<-(lm(Group3.ab$fulton~Group3.ab$Rain.Max.15.cm))
#plot(G3maxr.line)
summary(G3maxr.line)
ols_test_normality(lm(Group3.ab$fulton~Group3.ab$Rain.Max.15.cm))


#day, normal not sig
plot((Group3.ab$fulton~Group3.ab$julian), main="Group3.ab tegu body condition vs. day of capture")
abline(lm(Group3.ab$fulton~Group3.ab$julian), col="red")
G3day.line<-(lm(Group3.ab$fulton~Group3.ab$julian))
#plot(G5day.line)
summary(G3day.line)
ols_test_normality(lm(Group3.ab$fulton~Group3.ab$julian))
#

####
#### Group 3 Distance Regressions...####
#Group 3 distance from US1 underpass, normal and sig
plot((Group3.ab$fulton~Group3.ab$UP.DIST), main="Group 3 tegu body condition vs. distance from the US 1 Underpass")
abline(lm(Group3.ab$fulton~Group3.ab$UP.DIST), col="red")
G3dist.line<-(lm(Group3.ab$fulton~Group3.ab$UP.DIST))
#plot(G3dist.line)
summary(G3dist.line)
ols_test_normality(lm(Group3.ab$fulton~Group3.ab$UP.DIST))


#Group 3 distance from JDC, normal and significant
plot((Group3.ab$fulton~Group3.ab$JDC.DIST), main="Group 3 tegu body condition vs. distance from detention center ")
abline(lm(Group3.ab$fulton~Group3.ab$JDC.DIST), col="red")
G3dist.jdc.line<-(lm(Group3.ab$fulton~Group3.ab$JDC.DIST))
#plot(G3dist.jdc.line)
summary(G3dist.jdc.line)
ols_test_normality(lm(Group3.ab$fulton~Group3.ab$JDC.DIST))


#Group 3 distance from Pond, Normal and significant
plot((Group3.ab$fulton~Group3.ab$Pond.DIST), main="Group 3 tegu body condition vs. distance from pond")
abline(lm(Group3.ab$fulton~Group3.ab$Pond.DIST), col="red")
G3dist.pond.line<-(lm(Group3.ab$fulton~Group3.ab$Pond.DIST))
#plot(G3dist.pond.line)
summary(G3dist.pond.line)
ols_test_normality(lm(Group3.ab$fulton~Group3.ab$Pond.DIST))

#Group 3 distance from C-110/424th intersection, Normal and significant
plot((Group3.ab$fulton~Group3.ab$C110424.DIST), main="Group 3 tegu body condition vs. distance from C-110/424th intersection")
abline(lm(Group3.ab$fulton~Group3.ab$C110424.DIST), col="red")
G3dist.x110424.line<-(lm(Group3.ab$fulton~Group3.ab$C110424.DIST))
#plot(G3dist.x110424.line)
summary(G3dist.x110424.line)
ols_test_normality(lm(Group3.ab$fulton~Group3.ab$C110424.DIST))

#Group 3 distance from water control structure, Normal and not significant
plot((Group3.ab$fulton~Group3.ab$WCS.DIST), main="Group 3 tegu body condition vs. distance from water control structure")
abline(lm(Group3.ab$fulton~Group3.ab$WCS.DIST), col="red")
G3dist.wcs.line<-(lm(Group3.ab$fulton~Group3.ab$WCS.DIST))
#plot(G3dist.wcs.line)
summary(G3dist.wcs.line)
ols_test_normality(lm(Group3.ab$fulton~Group3.ab$WCS.DIST))

#Group 3 distance from c-110/C-111 intersection, Normal but not significant
plot((Group3.ab$fulton~Group3.ab$C110111.DIST), main="Group 3 tegu body condition vs. distance from C-110/C-111 intersection")
abline(lm(Group3.ab$fulton~Group3.ab$C110111.DIST), col="red")
G3dist.x110111.line<-(lm(Group3.ab$fulton~Group3.ab$C110111.DIST))
#plot(G3dist.x110111.line)
summary(G3dist.x110111.line)
ols_test_normality(lm(Group3.ab$fulton~Group3.ab$C110111.DIST))

####

#### Group 3 multiple linear regressions####
#all
G3.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
               Rain.cm + Min.Temp.C + Max.Temp.C + 
               julian + y14 + y15 + y16 + y17 + y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
               d1 + d2 + d3 + d4 + d5, data = Group3.ab, na.action = "na.fail")
summary(G3.mlr)
#plot(G3.mlr)
ols_test_normality(G3.mlr)


#continuous
G3.2.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
                 Rain.cm + Min.Temp.C + Max.Temp.C + 
                 julian, data = Group3.ab, na.action = "na.fail")
summary(G3.2.mlr)
#plot(G3.2.mlr)
ols_test_normality(G3.2.mlr)


#Dummy
G3.3.mlr <- lm(fulton ~ y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
                 d1 + d2 + d3 + d4 + d5, data = Group3.ab, na.action = "na.fail")
summary(G3.3.mlr)
#plot(G3.3.mlr)
ols_test_normality(G3.3.mlr)

#Distance
G3.4.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST, 
               data = Group3.ab, na.action = "na.fail")
summary(G3.4.mlr)
#plot(G3.4.mlr)
ols_test_normality(G3.4.mlr)

#Environmental
G3.5.mlr <- lm(fulton ~ Rain.cm + Min.Temp.C + Max.Temp.C, 
               data = Group3.ab, na.action = "na.fail")
summary(G3.5.mlr)
#plot(G3.5.mlr)
ols_test_normality(G3.5.mlr)

#Temporal
G3.6.mlr <- lm(fulton ~ julian + y14 + y15 + y16 + y17 + y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12, 
               data = Group3.ab, na.action = "na.fail")
summary(G3.6.mlr)
#plot(G3.6.mlr)
ols_test_normality(G3.6.mlr)


#Habitat
G3.7.mlr <- lm(fulton ~ d1 + d2 + d3 + d4 + d5, data = Group3.ab, na.action = "na.fail")
summary(G3.7.mlr)
#plot(G3.7.mlr)
ols_test_normality(G3.7.mlr)


#which is best?
G3.8.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
                 Rain.cm + Min.Temp.C + Max.Temp.C + 
                 julian + y14 + y15 + y16 + y17 +y18 + m3 + m4 + m5 + m6 + m7 +
                 d1 + d2 + d3 + d4, data = Group3.ab, na.action = "na.fail")
summary(G3.8.mlr)
#plot(G3.mlr)
ols_test_normality(G3.8.mlr)


#best
G3.9.mlr <- lm(fulton ~ Min.Temp.C +y14 + y15 + m5 + m6 + m7,
               data = Group3.ab, na.action = "na.fail")
summary(G3.9.mlr)
#plot(G3.9.mlr)
ols_test_normality(G3.9.mlr)

#boxcox
bc.G3.9 <- boxcox(G3.9.mlr)
LambdaG3.9 <- bc.G3.9$x[which.max(bc.G3.9$y)]
new.G3.9.mlr <- lm((Group3.ab$fulton^LambdaG3.9-1/LambdaG3.9)~Max.Temp.C +julian+ y14 + y15 + d2 + d3 + d4,
                   data = Group3.ab, na.action = "na.fail")
summary(G3.9.mlr)
#plot(new.G3.9.mlr)
ols_test_normality(new.G3.9.mlr)
summary(new.G3.9.mlr)

####


#### Group 4 Regressions...julian, fat weight, percfat, rainfall####

#Average monthly temperature, data normal, not significant
plot((Group4.ab$fulton~Group4.ab$Avg.Temp.C), main="Group4.ab tegu body condition vs. average temperature")
abline(lm(Group4.ab$fulton~Group4.ab$Avg.Temp.C), col="red")
G4temp.line<-(lm(Group4.ab$fulton~Group4.ab$Avg.Temp.C))
#plot(G4temp.line)
summary(G4temp.line)
ols_test_normality(lm(Group4.ab$fulton~Group4.ab$Avg.Temp.C))

#average monthly rainfall, data normal, not significant
plot((Group4.ab$fulton~Group4.ab$Rain.cm), main="Group4.ab tegu body condition vs. average rainfall")
abline(lm(Group4.ab$fulton~Group4.ab$Rain.cm), col="red")
G4rain.line<-(lm(Group4.ab$fulton~Group4.ab$Rain.cm))
#plot(G4rain.line)
summary(G4rain.line)
ols_test_normality(lm(Group4.ab$fulton~Group4.ab$Rain.cm))

#minimum monthly temp, normal not sig
plot((Group4.ab$fulton~Group4.ab$Min.Temp.C), main="Group4.ab tegu body condition vs. Min Temp")
abline(lm(Group4.ab$fulton~Group4.ab$Min.Temp.C), col="red")
G4mint.line<-(lm(Group4.ab$fulton~Group4.ab$Min.Temp.C))
#plot(G4mint.line)
summary(G4mint.line)
ols_test_normality(lm(Group4.ab$fulton~Group4.ab$Min.Temp.C))

#max monthly temp, normal not sig
plot((Group4.ab$fulton~Group4.ab$Max.Temp.C), main="Group4.ab tegu body condition vs. Max Temp")
abline(lm(Group4.ab$fulton~Group4.ab$Max.Temp.C), col="red")
G4maxt.line<-(lm(Group4.ab$fulton~Group4.ab$Max.Temp.C))
#plot(G4maxt.line)
summary(G4maxt.line)
ols_test_normality(lm(Group4.ab$fulton~Group4.ab$Max.Temp.C))

#rain max, normal not sig
plot((Group4.ab$fulton~Group4.ab$Rain.Max.15.cm), main="Group4.ab tegu body condition vs. Rain Max")
abline(lm(Group4.ab$fulton~Group4.ab$Rain.Max.15.cm), col="red")
G4maxr.line<-(lm(Group4.ab$fulton~Group4.ab$Rain.Max.15.cm))
#plot(G4maxr.line)
summary(G4maxr.line)
ols_test_normality(lm(Group4.ab$fulton~Group4.ab$Rain.Max.15.cm))

#day
plot((Group4.ab$fulton~Group4.ab$julian), main="Group5.ab tegu body condition vs. day of capture")
abline(lm(Group4.ab$fulton~Group4.ab$julian), col="red")
G4day.line<-(lm(Group4.ab$fulton~Group4.ab$julian))
#plot(G4day.line)
summary(G4day.line)
ols_test_normality(lm(Group4.ab$fulton~Group4.ab$julian))
####

#### Group 4 Distance Regressions..all exc. c110/c111 and WCS####
#Group 4 distance from US1 underpass, normal and significant
plot((Group4.ab$fulton~Group4.ab$UP.DIST), main="Group 4 tegu body condition vs. distance from the US 1 Underpass")
abline(lm(Group4.ab$fulton~Group4.ab$UP.DIST), col="red")
G4dist.line<-(lm(Group4.ab$fulton~Group4.ab$UP.DIST))
#plot(G4dist.line)
summary(G4dist.line)
ols_test_normality(lm(Group4.ab$fulton~Group4.ab$UP.DIST))

#Group 4 distance from JDC, normal and significant
plot((Group4.ab$fulton~Group4.ab$JDC.DIST), main="Group 4 tegu body condition vs. distance from detention center ")
abline(lm(Group4.ab$fulton~Group4.ab$JDC.DIST), col="red")
G4dist.jdc.line<-(lm(Group4.ab$fulton~Group4.ab$JDC.DIST))
#plot(G4dist.jdc.line)
summary(G4dist.jdc.line)
ols_test_normality(lm(Group4.ab$fulton~Group4.ab$JDC.DIST))

#Group 4 distance from Pond, Normal and significant
plot((Group4.ab$fulton~Group4.ab$Pond.DIST), main="Group 4 tegu body condition vs. distance from pond")
abline(lm(Group4.ab$fulton~Group4.ab$Pond.DIST), col="red")
G4dist.pond.line<-(lm(Group4.ab$fulton~Group4.ab$Pond.DIST))
#plot(G4dist.pond.line)
summary(G4dist.pond.line)
ols_test_normality(lm(Group4.ab$fulton~Group4.ab$Pond.DIST))

#Group 4 distance from C-110/424th intersection, Normal and significant
plot((Group4.ab$fulton~Group4.ab$C110424.DIST), main="Group 4 tegu body condition vs. distance from C-110/424th intersection")
abline(lm(Group4.ab$fulton~Group4.ab$C110424.DIST), col="red")
G4dist.x110424.line<-(lm(Group4.ab$fulton~Group4.ab$C110424.DIST))
#plot(G4dist.x110424.line)
summary(G4dist.x110424.line)
ols_test_normality(lm(Group4.ab$fulton~Group4.ab$C110424.DIST))

#Group 4 distance from water control structure, Normal and not significant
plot((Group4.ab$fulton~Group4.ab$WCS.DIST), main="Group 4 tegu body condition vs. distance from water control structure")
abline(lm(Group4.ab$fulton~Group4.ab$WCS.DIST), col="red")
G4dist.wcs.line<-(lm(Group4.ab$fulton~Group4.ab$WCS.DIST))
#plot(G4dist.wcs.line)
summary(G4dist.wcs.line)
ols_test_normality(lm(Group4.ab$fulton~Group4.ab$WCS.DIST))

#Group 4 distance from c-110/C-111 intersection, Normal but not significant
plot((Group4.ab$fulton~Group4.ab$C110111.DIST), main="Group 4 tegu body condition vs. distance from C-110/C-111 intersection")
abline(lm(Group4.ab$fulton~Group4.ab$C110111.DIST), col="red")
G4dist.x110111.line<-(lm(Group4.ab$fulton~Group4.ab$C110111.DIST))
#plot(G4dist.x110111.line)
summary(G4dist.x110111.line)
ols_test_normality(lm(Group4.ab$fulton~Group4.ab$C110111.DIST))
####


####



#### Group 4 multiple linear regressions####
#all
G4.mlr <- lm(fulton ~ UP.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
               Rain.cm + Min.Temp.C + Max.Temp.C + 
               julian + y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
               d1 + d2 + d3 + d4 + d5, data = Group4.ab, na.action = "na.fail")
summary(G4.mlr)
#plot(G4.mlr)
ols_test_normality(G4.mlr)

#continuous
G4.2.mlr <- lm(fulton ~ UP.DIST +  C110424.DIST + C110111.DIST +  
                 Rain.cm + Min.Temp.C + Max.Temp.C + 
                 julian, data = Group4.ab, na.action = "na.fail")
summary(G4.2.mlr)
#plot(G4.2.mlr)
ols_test_normality(G4.2.mlr)

#dummy
G4.3.mlr <- lm(fulton ~ y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 +
                 d1 + d2 + d3 + d4 + d5, data = Group4.ab, na.action = "na.fail")
summary(G4.3.mlr)
#plot(G4.3.mlr)
ols_test_normality(G4.3.mlr)

#distance
G4.4.mlr <- lm(fulton ~ UP.DIST + C110424.DIST + C110111.DIST, 
               data = Group4.ab, na.action = "na.fail")
summary(G4.4.mlr)
#plot(G4.4.mlr)
ols_test_normality(G4.4.mlr)

#environmental
G4.5.mlr <- lm(fulton ~ Rain.cm + Min.Temp.C + Max.Temp.C, 
               data = Group4.ab, na.action = "na.fail")
summary(G4.5.mlr)
#plot(G4.5.mlr)
ols_test_normality(G4.5.mlr)

#temporal
G4.6.mlr <- lm(fulton ~ julian + y14 + y15 + y16 + y17 + y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12, 
               data = Group4.ab, na.action = "na.fail")
summary(G4.6.mlr)
#plot(G4.6.mlr)
ols_test_normality(G4.6.mlr)

#habitat
G4.7.mlr <- lm(fulton ~ d1 + d2 + d3 + d4 + d5, data = Group4.ab, na.action = "na.fail")
summary(G4.7.mlr)
#plot(G4.7.mlr)
ols_test_normality(G4.7.mlr)

#which is best?
G4.8.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
                 Rain.cm + Min.Temp.C + Max.Temp.C + 
                 julian + y14+ y15 +y16 +y17 +y18 + m2 + m3 + m4 + m5 + m6 + m7 + m8 +
                 d1 + d2 + d3 + d4, data = Group4.ab, na.action = "na.fail")
summary(G4.8.mlr)
#plot(G4.8.mlr)

#best
G4.9.mlr <- lm(fulton ~ UP.DIST + Max.Temp.C + y14 + y15 + y16 +y17 + m5 + m6 + m7 
               , data = Group4.ab, na.action = "na.fail")
summary(G4.9.mlr)
#plot(G4.9.mlr)
ols_test_normality(G4.9.mlr)


####



#### Group 5 Regressions...percfat, fat.g####

#Average monthly temperature, data normal, not significant
plot((Group5.ab$fulton~Group5.ab$Avg.Temp.C), main="Group5.ab tegu body condition vs. average temperature")
abline(lm(Group5.ab$fulton~Group5.ab$Avg.Temp.C), col="red")
G5temp.line<-(lm(Group5.ab$fulton~Group5.ab$Avg.Temp.C))
#plot(G5temp.line)
summary(G5temp.line)
ols_test_normality(lm(Group5.ab$fulton~Group5.ab$Avg.Temp.C))

#average monthly rainfall, data normal, not significant
plot((Group5.ab$fulton~Group5.ab$Rain.cm), main="Group5.ab tegu body condition vs. average rainfall")
abline(lm(Group5.ab$fulton~Group5.ab$Rain.cm), col="red")
G5rain.line<-(lm(Group5.ab$fulton~Group5.ab$Rain.cm))
#plot(G5rain.line)
summary(G5rain.line)
ols_test_normality(lm(Group5.ab$fulton~Group5.ab$Rain.cm))

#minimum monthly temp, normal not sig
plot((Group5.ab$fulton~Group5.ab$Min.Temp.C), main="Group5.ab tegu body condition vs. Min Temp")
abline(lm(Group5.ab$fulton~Group5.ab$Min.Temp.C), col="red")
G5mint.line<-(lm(Group5.ab$fulton~Group5.ab$Min.Temp.C))
#plot(G5mint.line)
summary(G5mint.line)
ols_test_normality(lm(Group5.ab$fulton~Group5.ab$Min.Temp.C))

#max monthly temp, normal not sig
plot((Group5.ab$fulton~Group5.ab$Max.Temp.C), main="Group5.ab tegu body condition vs. Max Temp")
abline(lm(Group5.ab$fulton~Group5.ab$Max.Temp.C), col="red")
G5maxt.line<-(lm(Group5.ab$fulton~Group5.ab$Max.Temp.C))
#plot(G5maxt.line)
summary(G5maxt.line)
ols_test_normality(lm(Group5.ab$fulton~Group5.ab$Max.Temp.C))

#rain max, normal not sig
plot((Group5.ab$fulton~Group5.ab$Rain.Max.15.cm), main="Group5.ab tegu body condition vs. Rain Max")
abline(lm(Group5.ab$fulton~Group5.ab$Rain.Max.15.cm), col="red")
G5maxr.line<-(lm(Group5.ab$fulton~Group5.ab$Rain.Max.15.cm))
#plot(G5maxr.line)
summary(G5maxr.line)
ols_test_normality(lm(Group5.ab$fulton~Group5.ab$Rain.Max.15.cm))

#day
plot((Group5.ab$fulton~Group5.ab$julian), main="Group5.ab tegu body condition vs. day of capture")
abline(lm(Group5.ab$fulton~Group5.ab$julian), col="red")
G5day.line<-(lm(Group5.ab$fulton~Group5.ab$julian))
#plot(G5day.line)
summary(G5day.line)
ols_test_normality(lm(Group5.ab$fulton~Group5.ab$julian))

####


#### Group 5 Distance Regressions....none####
#Group 5 distance from US1 underpass, normal and significant
plot((Group5.ab$fulton~Group5.ab$UP.DIST), main="Group 5 tegu body condition vs. distance from the US 1 Underpass")
abline(lm(Group5.ab$fulton~Group5.ab$UP.DIST), col="red")
G5dist.line<-(lm(Group5.ab$fulton~Group5.ab$UP.DIST))
#plot(G5dist.line)
summary(G5dist.line)
ols_test_normality(lm(Group5.ab$fulton~Group5.ab$UP.DIST))

#Group 5 distance from JDC, normal and significant
plot((Group5.ab$fulton~Group5.ab$JDC.DIST), main="Group 5 tegu body condition vs. distance from detention center ")
abline(lm(Group5.ab$fulton~Group5.ab$JDC.DIST), col="red")
G5dist.jdc.line<-(lm(Group5.ab$fulton~Group5.ab$JDC.DIST))
#plot(G5dist.jdc.line)
summary(G5dist.jdc.line)
ols_test_normality(lm(Group5.ab$fulton~Group5.ab$JDC.DIST))

#Group 5 distance from Pond, Normal and significant
plot((Group5.ab$fulton~Group5.ab$Pond.DIST), main="Group 5 tegu body condition vs. distance from pond")
abline(lm(Group5.ab$fulton~Group5.ab$Pond.DIST), col="red")
G5dist.pond.line<-(lm(Group5.ab$fulton~Group5.ab$Pond.DIST))
#plot(G5dist.pond.line)
summary(G5dist.pond.line)
ols_test_normality(lm(Group5.ab$fulton~Group5.ab$Pond.DIST))

#Group 5 distance from C-110/424th intersection, Normal and significant
plot((Group5.ab$fulton~Group5.ab$C110424.DIST), main="Group 5 tegu body condition vs. distance from C-110/525th intersection")
abline(lm(Group5.ab$fulton~Group5.ab$C110424.DIST), col="red")
G5dist.x110424.line<-(lm(Group5.ab$fulton~Group5.ab$C110424.DIST))
#plot(G5dist.x110525.line)
summary(G5dist.x110424.line)
ols_test_normality(lm(Group5.ab$fulton~Group5.ab$C110424.DIST))

#Group 5 distance from water control structure, Normal and significant
plot((Group5.ab$fulton~Group5.ab$WCS.DIST), main="Group 5 tegu body condition vs. distance from water control structure")
abline(lm(Group5.ab$fulton~Group5.ab$WCS.DIST), col="red")
G5dist.wcs.line<-(lm(Group5.ab$fulton~Group5.ab$WCS.DIST))
#plot(G5dist.wcs.line)
summary(G5dist.wcs.line)
ols_test_normality(lm(Group5.ab$fulton~Group5.ab$WCS.DIST))

#Group 5 distance from c-110/C-111 intersection, Normal but not significant
plot((Group5.ab$fulton~Group5.ab$C110111.DIST), main="Group 5 tegu body condition vs. distance from C-110/C-111 intersection")
abline(lm(Group5.ab$fulton~Group5.ab$C110111.DIST), col="red")
G5dist.x110111.line<-(lm(Group5.ab$fulton~Group5.ab$C110111.DIST))
#plot(G5dist.x110111.line)
summary(G5dist.x110111.line)
ols_test_normality(lm(Group5.ab$fulton~Group5.ab$C110111.DIST))
####

#### Group 5 multiple linear regressions####
#all
G5.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
               Rain.cm + Min.Temp.C + Max.Temp.C + 
               julian + y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12+
               d1 + d2 + d3 + d4 + d5, data = Group5.ab, na.action = "na.fail")
summary(G5.mlr)
#plot(G5.mlr)
ols_test_normality(G5.mlr)


#continuous
G5.2.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
                 Rain.cm + Min.Temp.C + Max.Temp.C + 
                 julian, data = Group5.ab, na.action = "na.fail")
summary(G5.2.mlr)
#plot(G5.2.mlr)
ols_test_normality(G5.2.mlr)


#Dummy
G5.3.mlr <- lm(fulton ~ y14 + y15 + y16 + y17 +y18 + y19 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12+
                 d1 + d2 + d3 + d4 + d5, data = Group5.ab, na.action = "na.fail")
summary(G5.3.mlr)
plot(G5.3.mlr)
ols_test_normality(G5.3.mlr)


#distance
G5.4.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST, 
               data = Group5.ab, na.action = "na.fail")
summary(G5.4.mlr)
#plot(G5.4.mlr)
ols_test_normality(G5.4.mlr)


#environmental
G5.5.mlr <- lm(fulton ~ Rain.cm + Min.Temp.C + Max.Temp.C, 
               data = Group5.ab, na.action = "na.fail")
summary(G5.5.mlr)
#plot(G5.5.mlr)
ols_test_normality(G5.5.mlr)

#temporal
G5.6.mlr <- lm(fulton ~ julian + y14 + y15 + y16 + y17 +y18 + m2 + m3 + m4 + m5 + m6 + m7 + m8, 
               data = Group5.ab, na.action = "na.fail")
summary(G5.6.mlr)
plot(G5.6.mlr)
ols_test_normality(G5.6.mlr)


#habitat
G5.7.mlr <- lm(fulton ~ d1 + d2 + d3 + d4 +d5, data = Group5.ab, na.action = "na.fail")
summary(G5.7.mlr)
plot(G5.7.mlr)
ols_test_normality(G5.7.mlr)


#which is best?
G5.8.mlr <- lm(fulton ~ UP.DIST + JDC.DIST + Pond.DIST + C110424.DIST + C110111.DIST + WCS.DIST + 
                 Rain.cm + Min.Temp.C + Max.Temp.C + 
                 julian + y14 + y15 + y16 + y17 +y18 + m2 + m3 + m4 + m5 + m6 + m7 + m8 +
                 d1 + d2 + d3 + d4, data = Group5.ab, na.action = "na.fail")
summary(G5.8.mlr)
plot(G5.8.mlr)
ols_test_normality(G5.8.mlr)

#best
G5.9.mlr <- lm(fulton ~ Min.Temp.C +y17, data = Group5.ab, na.action = "na.fail")
summary(G5.9.mlr)
#plot(G5.9.mlr)
ols_test_normality(G5.9.mlr)

####


#### making figures ####


hist(Group1.ab$fulton)
hist(Group2.ab$fulton)
hist(Group3.ab$fulton)
hist(Group4.ab$fulton)
hist(Group5.ab$fulton)
##Plot svl by month and Sex using mean +- SD
juv.svl<-Jsvl.g+geom_errorbar(aes(ymin=svl.mean-sd, ymax=svl.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  facet_wrap(~Sex)+##makes separate graphs for Sexes
  ggtitle("Juvenile SVL by month and Sex, +-SD")
##Plot svl by month and Sex using mean +- SD
aud.svl<-Asvl.g+geom_errorbar(aes(ymin=svl.mean-sd, ymax=svl.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+##points
  facet_wrap(~Sex)+##makes separate graphs for Sexes
  ggtitle("Adult SVL by month and Sex, +-SD")

###Adults - percfat by size class and Sex using mean +- SD
aud.perc<-Aperc.g2+geom_errorbar(aes(ymin=percfat.mean-sd, ymax=percfat.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+
  facet_wrap(~Sex)+##makes separate graphs for Sexes
  ggtitle("Adult Percent Fat by Sex and month, +-SD")

##Juveniles - percfat by size class and Sex using mean +- SD
juv.perc<-Jperc.g2+geom_errorbar(aes(ymin=percfat.mean-sd, ymax=percfat.mean+sd))+##error bars
  geom_point(size=6, (aes(x=month)))+
  facet_wrap(~Sex)+##makes separate graphs for Sexes
  ggtitle("Juvenile Percent Fat by Sex and month, +-SD")

ggarrange(aud.svl, juv.svl, aud.perc, juv.perc,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
#fat weight and FK

##Plot fat mass by size class and Sex using mean +- SD
Ad.jv.month<-ggplot(NULL, aes(month, fat.mean))+
  geom_point(data=Amass.g2+geom_errorbar(aes(ymin=fat.mean-sd, ymax=fat.mean+sd))
             geom_point(Jmass.g2+geom_errorbar(aes(ymin=fat.mean-sd, ymax=fat.mean+sd))+##error bars
                          geom_point(size=6, (aes(x=month))))
             
             
             Adult.fat<-Amass.g2+geom_errorbar(aes(ymin=fat.mean-sd, ymax=fat.mean+sd))+##error bars
               geom_point(size=6, (aes(x=month)))+##points
               facet_wrap(~Sex)+##makes seperate graphs for Sexes
               ggtitle("Adult Fat mass by Sex and month, +-SD")
             ##Plot fat mass by size class and Sex using mean +- SD
             Juv.fat<-Jmass.g2+geom_errorbar(aes(ymin=fat.mean-sd, ymax=fat.mean+sd))+##error bars
               geom_point(size=6, (aes(x=month)))+##points
               facet_wrap(~Sex)+##makes seperate graphs for Sexes
               ggtitle("Juvenile Fat mass by Sex and month, +-SD")
             ##Adults - Plot with fulton mean and sd by month and Sex
             Adult.fulton<-Afulton.month+geom_errorbar(aes(ymin=fulton.mean-fulton.sd , ymax=fulton.mean+fulton.sd))+
               geom_point(size=6)+
               facet_wrap(~Sex)+
               ggtitle("Adults  Fulton's K by Sex and Month with +-SD")
             ##Juveniles - Plot with fulton mean and sd by month and Sex
             Juv.fulton<-Jfulton.month+geom_errorbar(aes(ymin=fulton.mean-fulton.sd , ymax=fulton.mean+fulton.sd))+
               geom_point(size=6)+
               facet_wrap(~Sex)+
               ggtitle("Juveniles  Fulton's K by Sex and Month with +-SD")
             
             ggarrange(Adult.fat, Juv.fat, Adult.fulton, Juv.fulton, 
                       labels = c("A", "B", "C", "D"),
                       ncol = 2, nrow = 2)
             ####
             citation("dplyr")
             citation("RODBC")
             citation("FSA")
             citation("openintro")
             citation("ggplot2")
             citation("e1071")
             citation("readxl")
             citation("tidyr")
             citation("weathermetrics")
             citation("olsrr")
             citation("MASS")
             citation("odbc")
             citation("performance")
             citation("MuMIn")
             citation("lubridate")
             citation("ggpubr")
             
             summary(tegu.data$Analysis.Sex)
             library("psych")
             describe(tegu.data$fat.g)
             
             odbcCloseAll()
             