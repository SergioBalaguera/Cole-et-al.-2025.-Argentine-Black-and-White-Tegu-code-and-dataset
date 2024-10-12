#######################################################################X
##############   Argentine Black and White Tegu          ##############X
##############   Body condition analysis                 ##############X
##############   Created by: Jenna Cole                  ##############X
##############   Modified by Sergio A. Balaguera-Reina   ##############X
##############   Last day modified: Sep 12th, 2024        #############X
#######################################################################X

rm(list = ls())
dev.off()

#Libraries----
library(tidyverse)
library(MASS)
library(mgcv)
library(gratia)
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

#### 3. comparing native and invasive----
  #### 3.1 Difference in size and fat----
#Invasive
a <- subset(tegu.data, Analysis.Sex %in% "Female")
b <- subset(tegu.data, Analysis.Sex %in% "Male")
wilcox.test(a$Intake.SVL, b$Intake.SVL)
wilcox.test(a$mass.g, b$mass.g)
wilcox.test(a$fat.g, b$fat.g)
a %>%
  summarise(
    MFat = mean(fat.g, na.rm = T),
    SDFat = sd(fat.g, na.rm = T)
  )
b %>%
  summarise(
    MFat = mean(fat.g, na.rm = T),
    SDFat = sd(fat.g, na.rm = T)
  )
wilcox.test(a$percfat, b$percfat)
a %>%
  summarise(
    MFat = mean(percfat, na.rm = T),
    SDFat = sd(percfat, na.rm = T)
  )
b %>%
  summarise(
    MFat = mean(percfat, na.rm = T),
    SDFat = sd(percfat, na.rm = T)
  )

#Native
a <- subset(merianae2, Sex %in% "female")
b <- subset(merianae2, Sex %in% "male")
wilcox.test(a$SVL, b$SVL)
a %>%
  summarise(
    MSVL = mean(SVL),
    SDSVL = sd(SVL)
  )
b %>%
  summarise(
    MSVL = mean(SVL),
    SDSVL = sd(SVL)
  )

wilcox.test(a$mass.g, b$mass.g)
a %>%
  summarise(
    MMass = mean(mass.g),
    SDMass = sd(mass.g)
  )
b %>%
  summarise(
    MFat = mean(mass.g),
    SDFat = sd(mass.g)
  )

wilcox.test(a$FatBody, b$FatBody)
a %>%
  summarise(
    MFat = mean(FatBody, na.rm = T),
    SDFat = sd(FatBody, na.rm = T)
  )
b %>%
  summarise(
    MFat = mean(FatBody, na.rm = T),
    SDFat = sd(FatBody, na.rm = T)
  )

wilcox.test(a$percfat, b$percfat)
a %>%
  summarise(
    MFat = mean(percfat, na.rm = T),
    SDFat = sd(percfat, na.rm = T)
  )
b %>%
  summarise(
    MFat = mean(percfat, na.rm = T),
    SDFat = sd(percfat, na.rm = T)
  )

#By month
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
  labs(x = "", y = "Percent of fat (%)") +
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
  labs(x = "Month", y = "Percent of fat (%)") +
  theme_bw()

ggsave("./Figures/legend.jpeg", plot = last_plot(), height = 1500, width = 2000, dpi = 250, units = 'px')

a / b + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

ggsave("./Figures/Figure2.jpeg", plot = last_plot(), height = 1500, width = 2000, dpi = 250, units = 'px')

a <- subset(tegu.data,month %in% "Feb")
b <- subset(merianae2,month %in% "Feb")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Mar")
b <- subset(merianae2,month %in% "Mar")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Sep")
b <- subset(merianae2,month %in% "Sep")
wilcox.test(a$percfat, b$percfat)

a <- subset(tegu.data,month %in% "Oct")
b <- subset(merianae2,month %in% "Oct")
wilcox.test(a$percfat, b$percfat)

  #### 3.2 Differences by group----
#South Florida
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

#Argentina
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

  #### 3.3 Difference in body condition-----  
a <- subset(tegu.data,sizeclass %in% "Group2")
b <- subset(merianae2,sizeclass %in% "Group2")
wilcox.test(a$fulton, b$fulton)

a <- subset(tegu.data,sizeclass %in% "Group3")
b <- subset(merianae2,sizeclass %in% "Group3")
wilcox.test(a$fulton, b$fulton)

#By month
Months <- data.frame(month = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
merianae3 <- left_join(Months, merianae2, by = "month")
merianae3 <- merianae3 %>%
  mutate(month = factor(month, 
                        levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))
a <- merianae3 %>%
  filter(!is.na(month)) %>%
  ggplot() +
  geom_boxplot(aes(y = fulton, x = as.factor(month), color = sizeclass)) +
  ylim(2, 4.5) +
  labs(x = "", y = "Fulton's K") +
  scale_color_discrete(name = "Size class", labels = c("Hatchling/Early Juvenile",
                                                       "Lat Juvenile/Early adult",
                                                       "Large adult")) +
  theme_bw()

tegu.data1 <- left_join(Months, tegu.data, by = 'month')
tegu.data1 <- tegu.data1 %>%
  mutate(month = factor(month, 
                        levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))
b <- ggplot(tegu.data1, aes(y = fulton, x = as.factor(month), color = sizeclass)) +
  geom_boxplot() +
  ylim(2, 4.5) +
  labs(x = "Month", y = "Fulton's K") +
  scale_color_discrete(name = "Size class", labels = c("Hatchling/Early Juvenile",
                                                       "Lat Juvenile/Early adult",
                                                       "Large adult")) +
  theme_bw()

a / b + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

ggsave("./Figures/Figure3.jpeg", plot = last_plot(), height = 1500, width = 2000, dpi = 250, units = 'px')

a <- subset(tegu.data,month %in% "Feb")
b <- subset(merianae2,month %in% "Feb")
wilcox.test(a$fulton, b$fulton)

a <- subset(tegu.data,month %in% "Mar")
b <- subset(merianae2,month %in% "Mar")
wilcox.test(a$fulton, b$fulton)

a <- subset(tegu.data,month %in% "Sep")
b <- subset(merianae2,month %in% "Sep")
wilcox.test(a$fulton, b$fulton)

a <- subset(tegu.data,month %in% "Oct")
b <- subset(merianae2,month %in% "Oct")
wilcox.test(a$fulton, b$fulton)

#### 4. factors affecting body condition in South Florida----
data.model <- dplyr::select(tegu.data, c(fulton, month, year, julian, Rain.cm, Avg.Temp.C, Min.Temp.C,
                                         Max.Temp.C, HabCat, percfat, UTM.Easting, UTM.Northing, fat.g))
data.model$HabCat <- as.factor(data.model$HabCat)
str(data.model)

summary(data.model)

m1 <- bam(fulton ~ 
            s(julian) +
            s(Rain.cm) + 
            s(Avg.Temp.C) +
            s(Min.Temp.C) +
            s(Max.Temp.C) +
            s(percfat) +
            s(year, bs = 're') +
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
            ti(Max.Temp.C, percfat),
          data = data.model, method = 'fREML', select = F, 
          nthreads = c(4,1), discrete = T)
summary(m1) #34%
k.check(m1) #Fine
appraise(m1, method = 'simulate') #Looks good

m2 <- bam(fulton ~ HabCat +
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
            ti(Max.Temp.C, percfat),
          data = data.model, method = 'fREML', select = F, 
          nthreads = c(4,1), discrete = T)
summary(m2) #36.1%
k.check(m2) #Fine
appraise(m2, method = 'simulate') #Looks good

m3 <- bam(fulton ~ HabCat +
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
            ti(Max.Temp.C, percfat),
          data = tegu.data, method = 'fREML', select = F, family = Gamma(link = "log"),
          nthreads = c(4,1), discrete = T)
summary(m3) #34.8%
k.check(m3) #Fine
appraise(m3, method = 'simulate') #Looks good

#Model selection
AIC(m1, m2, m3) #m1 best
anova(m1, m2, test = 'F')

#Graph m2
draw(m2, select = c("s(Max.Temp.C)",
                    "s(percfat)"), scale = 'free')
ggsave("./Figures/unvariate.jpeg", plot = last_plot(), height = 1000, width = 2000, dpi = 250,
       units = 'px')
draw(m2, select = c("s(Min.Temp.C,year)",
                    "s(Max.Temp.C,year)"), scale = 'free')
ggsave("./Figures/bivariate11.jpeg", plot = last_plot(), height = 1000, width = 2000, dpi = 250,
       units = 'px')
draw(m2, select = c("ti(Rain.cm,percfat)",
                    "ti(Min.Temp.C,percfat)"), scale = 'free')
ggsave("./Figures/bivariate2.jpeg", plot = last_plot(), height = 1000, width = 2000, dpi = 250,
       units = 'px')
