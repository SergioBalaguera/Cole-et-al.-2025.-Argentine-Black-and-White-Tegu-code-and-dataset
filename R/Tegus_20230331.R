#######################################################################X
##############   Argentine Black and White Tegu          ##############X
##############   Body condition analysis                 ##############X
##############   Created by: Jenna Cole                  ##############X
##############   Modified by Sergio A. Balaguera-Reina   ##############X
##############   Last day modified: Oct 25th, 2024        #############X
#######################################################################X

rm(list = ls())
dev.off()

#Libraries----
library(tidyverse)
library(MASS)
library(Boruta)
library(mgcv)
library(gratia)
library(marginaleffects)
library(patchwork)

#### 1. Calling and exploring invasive data----
tegu.data <- read.csv("tegudata.ab.nooutliers.20220825.csv")

hist(tegu.data$fulton, breaks=100, main="Fulton's K")
abline(v=2.77, col="red",lwd=2)

hist(tegu.data$Intake.SVL, breaks=100, main="Tegu SVL")#histogram of SVL data
abline(v=30, col="red",lwd=2)##Verticle line at svl = 30, cutoff for adult size class

#All
tegu.data %>% 
  group_by(Analysis.Sex) %>%
  summarise(
    n()
  )
tegu.data %>%
  summarise(
    MSVL = mean(Intake.SVL),
    SDSVL = sd(Intake.SVL)
  )
tegu.data %>%
  summarise(
    MMassL = mean(mass.g),
    SDMass = sd(mass.g)
  )
tegu.data %>%
  summarise(
    MFK = mean(fulton),
    SDFK = sd(fulton)
  )
tegu.data %>%
  summarise(
    MFat = mean(fat.g, na.rm = T),
    SDFat = sd(fat.g, na.rm = T)
  )
tegu.data %>%
  summarise(
    MFatPerc = mean(percfat, na.rm = T),
    SDFatPerc = sd(percfat, na.rm = T)
  )
tegu.data %>%
  subset(percfat >=0) %>%
  summarise(
    n()
  )

#Groups
tegu.data %>%
  group_by(sizeclass) %>%
  summarise(
    n()
  )
tegu.data %>%
  group_by(sizeclass, Analysis.Sex) %>%
  summarise(
    n()
  )
tegu.data %>%
  group_by(sizeclass) %>%
  summarise(
    MSVL = mean(Intake.SVL),
    SDSVL = sd(Intake.SVL)
  )
tegu.data %>%
  group_by(sizeclass) %>%
  summarise(
    MMassL = mean(mass.g),
    SDMass = sd(mass.g)
  )
tegu.data %>%
  group_by(sizeclass) %>%
  summarise(
    MFK = mean(fulton),
    SDFK = sd(fulton)
  )
tegu.data %>%
  group_by(sizeclass) %>%
  summarise(
    MFat = mean(fat.g, na.rm = T),
    SDFat = sd(fat.g, na.rm = T)
  )
tegu.data %>%
  group_by(sizeclass) %>%
  summarise(
    MFatPerc = mean(percfat, na.rm = T),
    SDFatPerc = sd(percfat, na.rm = T)
  )
tegu.data %>%
  subset(percfat >=0) %>%
  group_by(sizeclass) %>%
  summarise(
    n()
  )

#### 2. Calling and exploring native data----
Native <- read.csv("NativeTeguDat.csv")
merianae<-subset(Native, Native$Species %in% "merianae")
merianae$mass.g <- with(merianae, BM*1000)##body mass in grams
merianae$fulton <- with(merianae, (mass.g/SVL^3)*10^2)
merianae$percfat<-with(merianae, (FatBody/mass.g)*100)
merianae$lsvl<-log(merianae$SVL)
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
merianae2 <- merianae2[complete.cases(merianae2$Sex),]
merianae2 <- merianae2[complete.cases(merianae2$SVL),]
merianae2 <- merianae2[complete.cases(merianae2$mass.g),]

#All
merianae2 %>% 
  group_by(Sex) %>%
  summarise(
    n()
  )

merianae2 %>%
  summarise(
    MSVL = mean(SVL),
    SDSVL = sd(SVL)
  )
merianae2 %>%
  summarise(
    MMassL = mean(mass.g),
    SDMass = sd(mass.g)
  )
merianae2 %>%
  summarise(
    MFK = mean(fulton),
    SDFK = sd(fulton)
  )
merianae2 %>%
  summarise(
    MFat = mean(FatBody, na.rm = T),
    SDFat = sd(FatBody, na.rm = T)
  )
merianae2 %>%
  summarise(
    MFatPerc = mean(percfat, na.rm = T),
    SDFatPerc = sd(percfat, na.rm = T)
  )
merianae2 %>%
  subset(percfat >=0) %>%
  summarise(
    n()
  )

#Groups
merianae2 %>%
  group_by(sizeclass) %>%
  summarise(
    n()
  )
merianae2 %>%
  group_by(sizeclass, Sex) %>%
  summarise(
    n()
  )
merianae2 %>%
  group_by(sizeclass) %>%
  summarise(
    MSVL = mean(SVL),
    SDSVL = sd(SVL)
  )
merianae2 %>%
  group_by(sizeclass) %>%
  summarise(
    MMassL = mean(mass.g),
    SDMass = sd(mass.g)
  )
merianae2 %>%
  group_by(sizeclass) %>%
  summarise(
    MFK = mean(fulton),
    SDFK = sd(fulton)
  )
merianae2 %>%
  group_by(sizeclass) %>%
  summarise(
    MFat = mean(FatBody, na.rm = T),
    SDFat = sd(FatBody, na.rm = T)
  )
merianae2 %>%
  group_by(sizeclass) %>%
  summarise(
    MFatPerc = mean(percfat, na.rm = T),
    SDFatPerc = sd(percfat, na.rm = T)
  )
merianae2 %>%
  subset(percfat >=0) %>%
  group_by(sizeclass) %>%
  summarise(
    n()
  )

#### 3. Intrapopulation analysis----
  #### 3.1 South Florida----
    #### 3.1.1 Overall----
mean(tegu.data$percfat, na.rm = T); sd(tegu.data$percfat, na.rm = T)
mean(tegu.data$Intake.SVL, na.rm = T); sd(tegu.data$Intake.SVL, na.rm = T)
mean(tegu.data$mass.g, na.rm = T); sd(tegu.data$mass.g, na.rm = T)

    #### 3.1.2 By sex----
a <- subset(tegu.data, Analysis.Sex %in% "Female")
b <- subset(tegu.data, Analysis.Sex %in% "Male")
wilcox.test(a$Intake.SVL, b$Intake.SVL)
wilcox.test(a$mass.g, b$mass.g)
wilcox.test(a$percfat, b$percfat)

a %>%
  summarise(
    mean(percfat, na.rm = T),
    sd(percfat, na.rm = T)
  )
b %>%
  summarise(
    mean(percfat, na.rm = T),
    sd(percfat, na.rm = T)
  )

    #### 3.1.3 By size class----
#SVL
tegu.data %>%
  group_by(sizeclass) %>%
  do(w = wilcox.test(Intake.SVL ~ Analysis.Sex, data = ., paired = F)) %>%
  summarise(sizeclass, 
            Wilcox = w$p.value)

#Mass
tegu.data %>%
  group_by(sizeclass) %>%
  do(w = wilcox.test(mass.g ~ Analysis.Sex, data = ., paired = F)) %>%
  summarise(sizeclass, 
            Wilcox = w$p.value)

#fat percentage
tegu.data %>%
  group_by(sizeclass) %>%
  do(w = wilcox.test(percfat ~ Analysis.Sex, data = ., paired = F)) %>%
  summarise(sizeclass, 
            Wilcox = w$p.value)

    #### 3.1.4 By seasonal periods----
#Going into brumation
a <- subset(tegu.data,month %in% "Oct")
b <- subset(merianae2,month %in% "Mar")
wilcox.test(a$percfat, b$percfat)

#Coming out brumation
a <- subset(tegu.data,month %in% "Feb")
b <- subset(merianae2,month %in% "Sep")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Mar")
b <- subset(merianae2,month %in% "Oct")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Apr")
b <- subset(merianae2,month %in% "Nov")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "May")
b <- subset(merianae2,month %in% "Dec")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Jun")
b <- subset(merianae2,month %in% "Jan")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Jul")
b <- subset(merianae2,month %in% "Feb")
wilcox.test(a$percfat, b$percfat)

  #### 3.2 Argentina----

    #### 3.2.1 Overall----
wilcox.test(tegu.data$percfat, merianae2$percfat)
mean(tegu.data$percfat, na.rm = T); sd(tegu.data$percfat, na.rm = T)
mean(merianae2$percfat, na.rm = T); sd(merianae2$percfat, na.rm = T)

    #### 3.2.2 By sex----
a <- subset(merianae2, Sex %in% "female")
b <- subset(merianae2, Sex %in% "male")
wilcox.test(a$SVL, b$SVL)
a %>%
  summarise(
    mean(SVL),
    sd(SVL)
  )
b %>%
  summarise(
    mean(SVL),
    sd(SVL)
  )

wilcox.test(a$mass.g, b$mass.g)
a %>%
  summarise(
    mean(mass.g),
    sd(mass.g)
  )
b %>%
  summarise(
    mean(mass.g),
    sd(mass.g)
  )

wilcox.test(a$percfat, b$percfat)
a %>%
  summarise(
    mean(percfat, na.rm = T),
    sd(percfat, na.rm = T)
  )
b %>%
  summarise(
    mean(percfat, na.rm = T),
    sd(percfat, na.rm = T)
  )

    #### 3.2.3 By size class----
#SVL
merianae2 %>%
  subset(!sizeclass %in% "Group1") %>%
  group_by(sizeclass) %>%
  do(w = wilcox.test(SVL ~ Sex, data = ., paired = F)) %>%
  summarise(sizeclass, 
            Wilcox = w$p.value)

#Mass
merianae2 %>%
  subset(!sizeclass %in% "Group1") %>%
  group_by(sizeclass) %>%
  do(w = wilcox.test(mass.g ~ Sex, data = ., paired = F)) %>%
  summarise(sizeclass, 
            Wilcox = w$p.value)

#fat percentage
merianae2 %>%
  subset(!sizeclass %in% "Group1") %>%
  group_by(sizeclass) %>%
  do(w = wilcox.test(percfat ~ Sex, data = ., paired = F)) %>%
  summarise(sizeclass, 
            Wilcox = w$p.value)

    #### 3.2.4 By seasonal periods----
#Going into brumation
a <- subset(tegu.data,month %in% "Oct")
b <- subset(merianae2,month %in% "Mar")
wilcox.test(a$percfat, b$percfat)

#Coming out brumation
a <- subset(tegu.data,month %in% "Feb")
b <- subset(merianae2,month %in% "Sep")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Mar")
b <- subset(merianae2,month %in% "Oct")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Apr")
b <- subset(merianae2,month %in% "Nov")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "May")
b <- subset(merianae2,month %in% "Dec")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Jun")
b <- subset(merianae2,month %in% "Jan")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Jul")
b <- subset(merianae2,month %in% "Feb")
wilcox.test(a$percfat, b$percfat)

#### 4. Interpopulation analysis (native vs non-native)----
  #### 4.1 Difference in traits----
    #### 4.1.1 Percent fat----
      #### 4.1.1.1 Overall----
wilcox.test(tegu.data$percfat, merianae2$percfat)
mean(tegu.data$percfat, na.rm = T); sd(tegu.data$percfat, na.rm = T)
mean(merianae2$percfat, na.rm = T); sd(merianae2$percfat, na.rm = T)

      #### 4.1.1.2 By sex----
a <- subset(tegu.data, Analysis.Sex %in% "Female")
b <- subset(merianae2,Sex %in% "female")
wilcox.test(a$percfat, b$percfat)

mean(a$percfat, na.rm = T); sd(a$percfat, na.rm = T)
mean(b$percfat, na.rm = T); sd(b$percfat, na.rm = T)

a <- subset(tegu.data, Analysis.Sex %in% "Male")
b <- subset(merianae2,Sex %in% "male")
wilcox.test(a$percfat, b$percfat)

mean(a$percfat, na.rm = T); sd(a$percfat, na.rm = T)
mean(b$percfat, na.rm = T); sd(b$percfat, na.rm = T)

      #### 4.1.1.3 By size class----
a <- subset(tegu.data, sizeclass %in% "Group1")
b <- subset(merianae2,sizeclass %in% "Group1")# too few recods. Analysis can't be done.
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data, sizeclass %in% "Group2")
b <- subset(merianae2,sizeclass %in% "Group2")
wilcox.test(a$percfat, b$percfat)

mean(a$percfat, na.rm = T); sd(a$percfat, na.rm = T)
mean(b$percfat, na.rm = T); sd(b$percfat, na.rm = T)

a <- subset(tegu.data, sizeclass %in% "Group3")
b <- subset(merianae2,sizeclass %in% "Group3")
wilcox.test(a$percfat, b$percfat)

mean(a$percfat, na.rm = T); sd(a$percfat, na.rm = T)
mean(b$percfat, na.rm = T); sd(b$percfat, na.rm = T)

      #### 4.1.1.4 By seasonal periods----
#Going into brumation
a <- subset(tegu.data,month %in% "Oct")
b <- subset(merianae2,month %in% "Mar")
wilcox.test(a$percfat, b$percfat)

#Coming out brumation
a <- subset(tegu.data,month %in% "Feb")
b <- subset(merianae2,month %in% "Sep")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Mar")
b <- subset(merianae2,month %in% "Oct")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Apr")
b <- subset(merianae2,month %in% "Nov")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "May")
b <- subset(merianae2,month %in% "Dec")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Jun")
b <- subset(merianae2,month %in% "Jan")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Jul")
b <- subset(merianae2,month %in% "Feb")
wilcox.test(a$percfat, b$percfat)

    #### 4.1.2 Size----
      #### 4.1.2.1 Overall---- 
wilcox.test(tegu.data$Intake.SVL, merianae2$SVL)
mean(tegu.data$Intake.SVL, na.rm = T); sd(tegu.data$Intake.SVL, na.rm = T)
mean(merianae2$SVL, na.rm = T); sd(merianae2$SVL, na.rm = T)

      #### 4.1.2.2 By sex----
a <- subset(tegu.data, Analysis.Sex %in% "Female")
b <- subset(merianae2,Sex %in% "female")
wilcox.test(a$Intake.SVL, b$SVL)

mean(a$Intake.SVL, na.rm = T); sd(a$Intake.SVL, na.rm = T)
mean(b$SVL, na.rm = T); sd(b$SVL, na.rm = T)

a <- subset(tegu.data, Analysis.Sex %in% "Male")
b <- subset(merianae2,Sex %in% "male")
wilcox.test(a$Intake.SVL, b$SVL)

mean(a$Intake.SVL, na.rm = T); sd(a$Intake.SVL, na.rm = T)
mean(b$SVL, na.rm = T); sd(b$SVL, na.rm = T)

      #### 4.1.2.3 By size class----
a <- subset(tegu.data, sizeclass %in% "Group1")
b <- subset(merianae2,sizeclass %in% "Group1")# too few recods. Analysis can't be done.
wilcox.test(a$Intake.SVL, b$SVL)

a <- subset(tegu.data, sizeclass %in% "Group2")
b <- subset(merianae2,sizeclass %in% "Group2")
wilcox.test(a$Intake.SVL, b$SVL)

mean(a$Intake.SVL, na.rm = T); sd(a$Intake.SVL, na.rm = T)
mean(b$SVL, na.rm = T); sd(b$SVL, na.rm = T)

a <- subset(tegu.data, sizeclass %in% "Group3")
b <- subset(merianae2,sizeclass %in% "Group3")
wilcox.test(a$Intake.SVL, b$SVL)

mean(a$Intake.SVL, na.rm = T); sd(a$Intake.SVL, na.rm = T)
mean(b$SVL, na.rm = T); sd(b$SVL, na.rm = T)

      #### 4.1.2.4 By seasonal periods----
#Going into brumation
a <- subset(tegu.data,month %in% "Oct")
b <- subset(merianae2,month %in% "Mar")
wilcox.test(a$Intake.SVL, b$SVL)

#Coming out brumation
a <- subset(tegu.data,month %in% "Feb")
b <- subset(merianae2,month %in% "Sep")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Mar")
b <- subset(merianae2,month %in% "Oct")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Apr")
b <- subset(merianae2,month %in% "Nov")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "May")
b <- subset(merianae2,month %in% "Dec")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Jun")
b <- subset(merianae2,month %in% "Jan")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Jul")
b <- subset(merianae2,month %in% "Feb")
wilcox.test(a$percfat, b$percfat)

    #### 4.1.3 Weight----
      #### 4.1.3.1 Overall---- 
wilcox.test(tegu.data$Intake.SVL, merianae2$SVL)
mean(tegu.data$Intake.SVL, na.rm = T); sd(tegu.data$Intake.SVL, na.rm = T)
mean(merianae2$SVL, na.rm = T); sd(merianae2$SVL, na.rm = T)

      #### 4.1.3.2 By sex----
a <- subset(tegu.data, Analysis.Sex %in% "Female")
b <- subset(merianae2,Sex %in% "female")
wilcox.test(a$Intake.SVL, b$SVL)

mean(a$Intake.SVL, na.rm = T); sd(a$Intake.SVL, na.rm = T)
mean(b$SVL, na.rm = T); sd(b$SVL, na.rm = T)

a <- subset(tegu.data, Analysis.Sex %in% "Male")
b <- subset(merianae2,Sex %in% "male")
wilcox.test(a$Intake.SVL, b$SVL)

mean(a$Intake.SVL, na.rm = T); sd(a$Intake.SVL, na.rm = T)
mean(b$SVL, na.rm = T); sd(b$SVL, na.rm = T)

      #### 4.1.3.3 By size class----
a <- subset(tegu.data, sizeclass %in% "Group1")
b <- subset(merianae2,sizeclass %in% "Group1")# too few recods. Analysis can't be done.
wilcox.test(a$Intake.SVL, b$SVL)

a <- subset(tegu.data, sizeclass %in% "Group2")
b <- subset(merianae2,sizeclass %in% "Group2")
wilcox.test(a$Intake.SVL, b$SVL)

mean(a$Intake.SVL, na.rm = T); sd(a$Intake.SVL, na.rm = T)
mean(b$SVL, na.rm = T); sd(b$SVL, na.rm = T)

a <- subset(tegu.data, sizeclass %in% "Group3")
b <- subset(merianae2,sizeclass %in% "Group3")
wilcox.test(a$Intake.SVL, b$SVL)

mean(a$Intake.SVL, na.rm = T); sd(a$Intake.SVL, na.rm = T)
mean(b$SVL, na.rm = T); sd(b$SVL, na.rm = T)

      #### 4.1.3.4 By seasonal periods----
#Going into brumation
a <- subset(tegu.data,month %in% "Oct")
b <- subset(merianae2,month %in% "Mar")
wilcox.test(a$Intake.SVL, b$SVL)

#Coming out brumation
a <- subset(tegu.data,month %in% "Feb")
b <- subset(merianae2,month %in% "Sep")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Mar")
b <- subset(merianae2,month %in% "Oct")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Apr")
b <- subset(merianae2,month %in% "Nov")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "May")
b <- subset(merianae2,month %in% "Dec")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Jun")
b <- subset(merianae2,month %in% "Jan")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Jul")
b <- subset(merianae2,month %in% "Feb")
wilcox.test(a$percfat, b$percfat)

  #### 4.2 Difference in body condition-----  
    #### 4.2.1 Overall----
wilcox.test(tegu.data$fulton, merianae2$fulton)
mean(tegu.data$fulton); sd(tegu.data$fulton)
mean(merianae2$fulton); sd(merianae2$fulton)

    #### 4.2.2 By sex----
a <- subset(tegu.data, Analysis.Sex %in% "Female")
b <- subset(merianae2,Sex %in% "female")
wilcox.test(a$fulton, b$fulton)

mean(a$fulton, na.rm = T); sd(a$fulton, na.rm = T)
mean(b$fulton, na.rm = T); sd(b$fulton, na.rm = T)

a <- subset(tegu.data, Analysis.Sex %in% "Male")
b <- subset(merianae2,Sex %in% "male")
wilcox.test(a$fulton, b$fulton)

mean(a$fulton, na.rm = T); sd(a$fulton, na.rm = T)
mean(b$fulton, na.rm = T); sd(b$fulton, na.rm = T)

    #### 4.2.3 By size class----
a <- subset(tegu.data, sizeclass %in% "Group1")
b <- subset(merianae2,sizeclass %in% "Group1")# too few recods. Analysis can't be done.
wilcox.test(a$fulton, b$fulton)

a <- subset(tegu.data, sizeclass %in% "Group2")
b <- subset(merianae2,sizeclass %in% "Group2")
wilcox.test(a$fulton, b$fulton)

mean(a$percfat, na.rm = T); sd(a$percfat, na.rm = T)
mean(b$percfat, na.rm = T); sd(b$percfat, na.rm = T)

a <- subset(tegu.data, sizeclass %in% "Group3")
b <- subset(merianae2,sizeclass %in% "Group3")
wilcox.test(a$fulton, b$fulton)

mean(a$percfat, na.rm = T); sd(a$percfat, na.rm = T)
mean(b$percfat, na.rm = T); sd(b$percfat, na.rm = T)

    #### 4.2.4 By seasonal period----
#Going into brumation
a <- subset(tegu.data,month %in% "Oct")
b <- subset(merianae2,month %in% "Mar")
wilcox.test(a$fulton, b$fulton)

#Coming out brumation
a <- subset(tegu.data,month %in% "Feb")
b <- subset(merianae2,month %in% "Sep")
wilcox.test(a$fulton, b$fulton)

a <- subset(tegu.data,month %in% "Mar")
b <- subset(merianae2,month %in% "Oct")
wilcox.test(a$fulton, b$fulton)

a <- subset(tegu.data,month %in% "Apr")
b <- subset(merianae2,month %in% "Nov")
wilcox.test(a$fulton, b$fulton)

a <- subset(tegu.data,month %in% "May")
b <- subset(merianae2,month %in% "Dec")
wilcox.test(a$fulton, b$fulton)

a <- subset(tegu.data,month %in% "Jun")
b <- subset(merianae2,month %in% "Jan")
wilcox.test(a$fulton, b$fulton)

a <- subset(tegu.data,month %in% "Jul")
b <- subset(merianae2,month %in% "Feb")
wilcox.test(a$fulton, b$fulton)

#### 5. factors affecting body condition in South Florida----
  #### 5.1 variable selection----
data.model <- dplyr::select(tegu.data, c(fulton, year, julian, Rain.cm, Avg.Temp.C, Min.Temp.C,
                                         Max.Temp.C, HabCat, percfat, Analysis.Sex, sizeclass)) %>%
  mutate(HabCat = as.factor(HabCat)) %>%
  mutate(sizeclass = factor(sizeclass, levels = c("Group1", "Group2", "Group3"))) %>%
  mutate(year = factor(year, levels = c("2014", "2015", "2016", "2017", "2018")))
str(data.model)
summary(data.model)

set.seed(123)
Boruta_Analysis <- data.model %>%
  dplyr::select(-sizeclass) %>%
  drop_na() %>%
  Boruta(fulton ~ ., data = ., doTrace = 2, maxRuns = 1000)
plot(Boruta_Analysis)
attStats(Boruta_Analysis) %>%
  as.data.frame() %>%
  rownames_to_column('var') %>%
  dplyr::select(var, decision, meanImp) %>%
  arrange(desc(meanImp))

  #### 5.2 GAM----
m1 <- bam(fulton ~
            s(julian) +
            s(Rain.cm) + 
            s(Avg.Temp.C) +
            s(Min.Temp.C) +
            s(Max.Temp.C) +
            s(percfat) +
            ti(julian, percfat) +
            ti(Rain.cm, percfat) +
            ti(Avg.Temp.C, percfat) +
            ti(Min.Temp.C, percfat) +
            ti(Max.Temp.C, percfat) +
            s(year, bs = 're') +
            s(sizeclass, bs = 're') +
            s(HabCat, bs = 're'),
          data = data.model, method = 'fREML', select = T, 
          nthreads = c(4,1), discrete = T)
summary(m1) #35.3%
k.check(m1) #Fine
appraise(m1, method = 'simulate') #Looks good

m2 <- bam(fulton ~ 
            s(julian) +
            s(Rain.cm) + 
            s(Avg.Temp.C) +
            s(Min.Temp.C) +
            s(Max.Temp.C) +
            s(percfat) +
            s(julian, HabCat, bs = 'fs') +
            s(Rain.cm, HabCat, bs = 'fs') + 
            s(Avg.Temp.C, HabCat, bs = 'fs') +
            s(Min.Temp.C, HabCat, bs = 'fs') +
            s(Max.Temp.C, HabCat, bs = 'fs') +
            s(percfat, HabCat, bs = 'fs') +
            ti(julian, percfat) +
            ti(Rain.cm, percfat) +
            ti(Avg.Temp.C, percfat) +
            ti(Min.Temp.C, percfat) +
            ti(Max.Temp.C, percfat) +
            s(year, bs = 're') +
            s(sizeclass, bs = 're') ,
          data = data.model, method = 'fREML', select = T, 
          nthreads = c(4,1), discrete = T)
summary(m2) #36.3%
k.check(m2) #Fine
appraise(m2, method = 'simulate') #Looks good

m3 <- bam(fulton ~ 
            s(julian) +
            s(Rain.cm) + 
            s(Avg.Temp.C) +
            s(Min.Temp.C) +
            s(Max.Temp.C) +
            s(percfat) +
            s(julian, year, bs = 'fs') +
            s(Rain.cm, year, bs = 'fs') + 
            s(Avg.Temp.C, year, bs = 'fs') +
            s(Min.Temp.C, year, bs = 'fs') +
            s(Max.Temp.C, year, bs = 'fs') +
            s(percfat, year, bs = 'fs') +
            ti(julian, percfat) +
            ti(Rain.cm, percfat) +
            ti(Avg.Temp.C, percfat) +
            ti(Min.Temp.C, percfat) +
            ti(Max.Temp.C, percfat) +
            s(HabCat, bs = 're') +
            s(sizeclass, bs = 're'),
          data = data.model, method = 'fREML', select = T, 
          nthreads = c(4,1), discrete = T)
summary(m3) #36.4%
k.check(m3) #Fine
appraise(m3, method = 'simulate') #Looks good

m4 <- bam(fulton ~ HabCat +
            s(julian) +
            s(Rain.cm) + 
            s(Avg.Temp.C) +
            s(Min.Temp.C) +
            s(Max.Temp.C) +
            s(percfat) +
            s(julian, year, bs = 'fs') +
            s(Rain.cm, year, bs = 'fs') + 
            s(Avg.Temp.C, year, bs = 'fs') +
            s(Min.Temp.C, year, bs = 'fs') +
            s(Max.Temp.C, year, bs = 'fs') +
            s(percfat, year, bs = 'fs') +
            ti(julian, percfat) +
            ti(Rain.cm, percfat) +
            ti(Avg.Temp.C, percfat) +
            ti(Min.Temp.C, percfat) +
            ti(Max.Temp.C, percfat) +
            s(sizeclass, bs = 're'),
          data = data.model, method = 'fREML', select = T, 
          nthreads = c(4,1), discrete = T)
summary(m4) #37.3%
k.check(m4) #Fine
appraise(m4, method = 'simulate') #Looks good

m5 <- bam(fulton ~ HabCat +
            s(julian) +
            s(Rain.cm) + 
            s(Avg.Temp.C) +
            s(Min.Temp.C) +
            s(Max.Temp.C) +
            s(percfat) +
            s(julian, year, bs = 'fs') +
            s(Rain.cm, year, bs = 'fs') + 
            s(Avg.Temp.C, year, bs = 'fs') +
            s(Min.Temp.C, year, bs = 'fs') +
            s(Max.Temp.C, year, bs = 'fs') +
            s(percfat, year, bs = 'fs') +
            ti(julian, percfat) +
            ti(Rain.cm, percfat) +
            ti(Avg.Temp.C, percfat) +
            ti(Min.Temp.C, percfat) +
            ti(Max.Temp.C, percfat) +
            s(sizeclass, bs = 're'),
          data = data.model, method = 'fREML', select = T, family = Gamma(link = "log"),
          nthreads = c(4,1), discrete = T)
summary(m5) #35.7%
k.check(m5) #Fine
appraise(m5, method = 'simulate') #Looks good

#Model selection
AIC(m1, m2, m3, m4, m5) #m1 and m4 best
anova(m1, m4, test = 'F') #No differences between m1 and m4, choose any.

#Graph m4
#Plotting parametric predictors
termplot(m4) 

#graph marginal effects
a <- plot_predictions(m4, condition = "julian", type = 'response', rug = T) + 
  labs(y = "Expected response", x = "Julian day") +
  theme_bw()

b <- plot_predictions(m4, condition = "Min.Temp.C", type = 'response', rug = T) + 
  labs(y = "Expected response", x = "Minimum Temperature (\u00b0C)") +
  theme_bw()

c <- plot_predictions(m4, condition = "percfat", type = 'response', rug = T) + 
  labs(y = "Expected response", x = "Percent fat") +
  theme_bw()

#Plot best predictor by parametric variable
d <- plot_predictions(m4, condition = c("percfat", 'HabCat'), type = 'response', rug = T) +
  labs(y = "Expected response", x = "Percent fat", color = "Habitat Categories", fill = "Habitat Categories") +
  theme_bw()

#Plot bivariate effects
e <- draw(m4, select = c("ti(Rain.cm,percfat)"), scale = 'free') +
  labs(title = "") +
  theme_bw()

#graph smooth factor effects
f <- draw(m4, select = c("s(Rain.cm,year)",
                    "s(Avg.Temp.C,year)",
                    "s(Max.Temp.C,year)",
                    "s(percfat,year)"), scale = 'free') +
  plot_layout(guides = "collect") &
  labs(title = "") &
  theme_bw()

#### Figure 2----
Months <- data.frame(month = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
merianae3 <- left_join(Months, merianae2, by = "month")
merianae3 <- merianae3 %>%
  mutate(month = factor(month, 
                        levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))
a <- merianae3 %>%
  filter(!is.na(month)) %>%
  ggplot() +
  geom_boxplot(aes(y = percfat, x = as.factor(month), color = sizeclass)) +
  scale_color_manual(values = c("#F8766D", "#7CAE00"), name = "Size class", 
                     labels = c("Hatchling/Early Juvenile", "Late Juvenile/Early adult")) +
  ylim(0, 10) +
  labs(x = "", y = "Percent fat") +
  theme_bw()

tegu.data1 <- left_join(Months, tegu.data, by = 'month')
tegu.data1 <- tegu.data1 %>%
  mutate(month = factor(month, 
                        levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))
b <- ggplot(tegu.data1, aes(y = percfat, x = as.factor(month), color = sizeclass)) +
  geom_boxplot() +
  scale_color_manual(values = c("#F8766D", "#7CAE00", "#00BFC4"), 
                     name = "Size class", labels = c("Hatchling/Early Juvenile",
                                                       "Late Juvenile/Early adult",
                                                       "Large adult")) +
  ylim(0, 10) +
  labs(x = "", y = "") +
  theme_bw()

c <- merianae3 %>%
  filter(!is.na(month)) %>%
  ggplot() +
  geom_boxplot(aes(y = fulton, x = as.factor(month), color = sizeclass)) +
  ylim(2, 4.5) +
  labs(x = "", y = "Fulton's K") +
  scale_color_manual(values = c("#F8766D", "#7CAE00", "#00BFC4"), 
                     name = "Size class", labels = c("Hatchling/Early Juvenile",
                                                       "Lat Juvenile/Early adult",
                                                       "Large adult")) +
  theme_bw()

d <- ggplot(tegu.data1, aes(y = fulton, x = as.factor(month), color = sizeclass)) +
  geom_boxplot() +
  ylim(2, 4.5) +
  labs(x = "", y = "") +
  scale_color_manual(values = c("#F8766D", "#7CAE00", "#00BFC4"), 
                     name = "Size class", labels = c("Hatchling/Early Juvenile",
                                                       "Lat Juvenile/Early adult",
                                                       "Large adult")) +
  theme_bw()

(a + b) / (c + d) + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

ggsave("./Figures/Figure2.jpeg", plot = last_plot(), height = 1500, width = 2000, dpi = 250, units = 'px')



#### Figure 3----
(a + b + c) / (d + e) / f

ggsave("./Figures/Figure3.jpeg", plot = last_plot(), height = 3000, width = 2500, dpi = 270,
       units = 'px')
