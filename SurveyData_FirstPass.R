library(tidyverse)
library(mapview)
library(effects)

theme_set(theme_bw(20))

setwd("C:/Users/mavolio2/Dropbox/BES Research/SegrationIncome/Data")

data<-read.csv("2021_2022_cleaned.csv") %>% 
  filter(Finished==T) %>% 
  filter(race2!="", income2!="")

# #getting to Baltimore Area - I don't fully understand this
# d22.1<-d22 %>% 
#   filter(Location.Latitude>=38.8, Location.Latitude<=39.5, Location.Longitude>=-77.2, Location.Longitude<=-76.2)

# #map 
# mapview(d22.1, xcol="Location.Longitude", ycol="Location.Latitude",crs=4326, grid=F)


#multiple responses from one IP address - except for Amy's
reps<-data %>%
  group_by(IP.Address) %>%
  summarize(n=length(Progress)) %>%
  filter(n>1, IP.Address!="69.140.25.220") %>%
  select(IP.Address) %>%
  mutate(remove=T)
cleandata<-data %>%
  left_join(reps) %>%
  filter(is.na(remove))

racewealth<-cleandata %>% 
  filter(race2=="Black"|race2=="White", income2<200000) %>% 
  group_by(race2, income2) %>% 
  summarize(n=length(Progress)) %>% 
  pivot_wider(names_from = "race2", values_from = n)

#need to subset white respondents to get even numbers
set.seed(1)

white25<-cleandata %>% 
  filter(race2=="White"&income2==25000) %>% 
  slice_sample(n=10)
white40<-cleandata %>% 
  filter(race2=="White"&income2==40000) %>% 
  slice_sample(n=39)
white60<-cleandata %>% 
  filter(race2=="White"&income2==60000) %>% 
  slice_sample(n=32)
white80<-cleandata %>% 
  filter(race2=="White"&income2==80000) %>% 
  slice_sample(n=26)
white100<-cleandata %>% 
  filter(race2=="White"&income2==100000) %>% 
  slice_sample(n=22)
white120<-cleandata %>% 
  filter(race2=="White"&income2==120000) %>% 
  slice_sample(n=18)
white140<-cleandata %>% 
  filter(race2=="White"&income2==140000) %>% 
  slice_sample(n=22)
white160<-cleandata %>% 
  filter(race2=="White"&income2==160000) %>% 
  slice_sample(n=19)
white180<-cleandata %>% 
  filter(race2=="White"&income2==180000) %>% 
  slice_sample(n=15)
white195<-cleandata %>% 
  filter(race2=="White"&income2==195000) %>% 
  slice_sample(n=9)

White<-white25 %>% 
  bind_rows(white40, white60, white80, white100, white120, white140, white160, white180, white195)

datasubset<-cleandata %>% 
  filter(race2=="Black", income2<200000) %>% 
  bind_rows(White) 

racewealth2<-datasubset%>% 
  group_by(race2, income2) %>% 
  summarize(n=length(Progress)) %>% 
  pivot_wider(names_from = "race2", values_from = n)

#how much do people spend on thier yards?
spendwealth<-datasubset %>% 
  select(race2, income2, YardSpend)

m1<-lm(YardSpend~race2*income2, data=spendwealth)
summary(m1)


#yardwork hours
yardwork<-datasubset %>% 
  select(race2, income2, Yardwork_hours_week, Yardwork_hour_month, YardSatisfy) %>% 
  mutate(MonthHours1=ifelse(Yardwork_hour_month!="", Yardwork_hour_month, Yardwork_hours_week*4),
         MonthHours2=ifelse(is.na(Yardwork_hour_month), Yardwork_hours_week*4, 999), 
         Monthhours=ifelse(MonthHours2==999, MonthHours1, MonthHours2)) %>% 
  mutate(Satisfied=ifelse(YardSatisfy=="Very dissatisfied", -2, ifelse(YardSatisfy=="Mildly dissatisfied", -1, ifelse(YardSatisfy=="Neutral", 0, ifelse(YardSatisfy=="Mildly satisfied", 1, ifelse(YardSatisfy=="Very satisfied", 2, 999)))))) %>% 
  filter(Satisfied!=999)

m1<-lm(Satisfied~race2*income2, data=yardwork)
summary(m1)
m2<-lm(Satisfied~income2+race2, data=yardwork)
anova(m1, m2)#no diff stick with model 1

ggplot(data=yardwork, aes(x=income2, y=Satisfied, color=race2))+
  geom_smooth(aes(group=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("Satified with Yard")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=1.6, label="Race p=0.07\nIncome p=0.08\nRace*Income p=0.21", size=4)+
  scale_y_continuous(limits=c(-2,2))+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.2))
ggsave("Yardsat.jpeg", device=jpeg)

summary(lm(Monthhours~race2*income2, data=yardwork))
ggplot(data=yardwork, aes(x=income2, y=Monthhours, color=race2))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method = "lm")

summary(lm(Satisfied~Monthhours*income2, data=yardwork))
ggplot(data=yardwork, aes(x=income2, y=Monthhours, color=race2))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method = "lm")

##yard components
library(stringr)
yardcomponents<-datasubset %>% 
  select(race2, income2, YardPlants) %>% 
  mutate(Lawns=ifelse(str_detect(YardPlants, "Lawn"), 1, 0),
         Food=ifelse(str_detect(YardPlants, "Food"), 1, 0),
         Flower=ifelse(str_detect(YardPlants, "Flower"), 1,0),
         Trees=ifelse(str_detect(YardPlants, "Trees"), 1, 0),
         Shrubs=ifelse(str_detect(YardPlants, "Shrubs"), 1, 0)) %>% 
  mutate(numtypes=Lawns+Food+Flower+Trees+Shrubs)

#wealthier more likely to grow food
summary(glm(Food~race2*income2, family="binomial", data=yardcomponents))

#wealthier more likely to have trees
summary(glm(Trees~race2*income2, family="binomial", data=yardcomponents))

#no one more likely to have shrubs
summary(glm(Shrubs~race2*income2, family="binomial", data=yardcomponents))

#no one more likely to have lawn
summary(glm(Lawns~race2*income2, family="binomial", data=yardcomponents))

#wealthier more likely to have trees and race difference
summary(glm(Flower~race2*income2, family="binomial", data=yardcomponents))

ggplot(data=yardcomponents, aes(x=income2, y=Flower, color=race2))+
  geom_smooth(aes(group=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("Have flowering plants")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=0.25, label="Race p=0.001*\nIncome p=0.002*\nRace*Income p=0.12", size=4)+
  scale_y_continuous(limits=c(0,1), breaks=c(0,1))+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.2))


#wealthier have more plant types
m1<-(lm(numtypes~race2*income2, data=yardcomponents))
m2<-(lm(numtypes~race2+income2, data=yardcomponents))
anova(m1, m2)
ggplot(data=yardcomponents, aes(x=income2, y=numtypes, color=race2))+
  geom_point()+
  geom_jitter(height=0.05)+
  geom_smooth(method = "lm")

#wellbeing
wellbeing<-datasubset %>% 
  select(Response.ID, race2, income2, Yard_wellbeing, Outdoors_wellbing, Nb_wellbing) %>% 
  pivot_longer(Yard_wellbeing:Nb_wellbing, names_to="scale", values_to = "wb") %>% 
  mutate(wellbeing=ifelse(wb=="Not at all", 0, ifelse(wb=="Not very much", 1, ifelse(wb=='Very much', 2, 999)))) %>% 
  filter(wellbeing!=999) %>% 
  select(-wb) %>% 
  pivot_wider(names_from="scale", values_from = "wellbeing")


summary(lm(Yard_wellbeing~race2*income2, data=wellbeing))
summary(lm(Nb_wellbing~race2*income2, data=wellbeing))
summary(lm(Outdoors_wellbing~race2*income2, data=wellbeing))

ggplot(data=wellbeing, aes(x=income2, y=Nb_wellbing, color=race2))+
  geom_smooth(aes(group=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("Neighborhood contributes\nto wellbeing")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=1.0, label="Race p=0.39\nIncome p<0.001*\nRace*Income p=0.31", size=4)+
  scale_y_continuous(limits=c(0,2), breaks=c(0,1,2))+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.2))



#Satified with NB and parks/trees
Nb<-datasubset %>% 
  select(race2, income2, EnvSatifsy, EnoughParks, CompareParks, EnoughTrees, CompareTrees) %>% 
  mutate(Satisfied=ifelse(EnvSatifsy=="Very dissatisfied", -2, ifelse(EnvSatifsy=="Mildly dissatisfied", -1, ifelse(EnvSatifsy=="Neutral", 0, ifelse(EnvSatifsy=="Mildly satisfied", 1, ifelse(EnvSatifsy=="Very satisfied", 2, NA))))),
         Parknum=ifelse(EnoughParks=="Yes", 1, 0),
         Treenum=ifelse(EnoughTrees=='Yes', 1, 0),
         Parkcomp=ifelse(CompareParks=="About the same", 0, ifelse(CompareParks=="Fewer", -1, ifelse(CompareParks=="More", 1, NA))), 
         Treecomp=ifelse(CompareTrees=="About the same", 0, ifelse(CompareTrees=="Fewer", -1, ifelse(CompareTrees=="More", 1, NA)))) %>% 
  drop_na()

#income Env satisfaction
m1<-(lm(Satisfied~race2*income2, data=Nb))
summary(m1)
plot(allEffects(m1))


ggplot(data=Nb, aes(x=income2, y=Satisfied))+
  geom_smooth(aes(color=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("Satified with Nature\nin Neighborhood")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=1.5, label="Race p=0.70\nIncome p<0.001*\nRace*Income p=0.77", size=4)+
  scale_y_continuous(limits=c(-2,2))+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.2))
ggsave('NbSatify.jpeg', device="jpeg")


m1<-(glm(Parknum~race2*income2, family='binomial', data=Nb))
summary(m1)
m1<-(glm(Treenum~race2*income2, family='binomial', data=Nb))
summary(m1)
plot(allEffects(m1))

ggplot(data=Nb, aes(x=income2, y=Parknum))+
  geom_smooth(aes(color=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("Neigbhorhood has enough parks")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=0.25, label="Race p=0.37\nIncome p=0.02*\nRace*Income p=0.06", size=4)+
  scale_y_continuous(limits=c(0,1), breaks=c(0,1))+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.2))

ggplot(data=Nb, aes(x=income2, y=Treenum))+
  geom_smooth(aes(color=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("Neigbhorhood has enough trees")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=0.25, label="Race p=0.87\nIncome p=0.06\nRace*Income p=0.21", size=4)+
  scale_y_continuous(limits=c(0,1), breaks=c(0,1))+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.2))

m1<-lm(Parkcomp~race2*income2, data=Nb)
summary(m1)
plot(allEffects(m1))

m1<-lm(Treecomp~race2*income2, data=Nb)
summary(m1)
plot(allEffects(m1))

##management
management<-datasubset %>% 
  select(race2, income2, Fertilize, Herbicide, Pesticide) %>% 
  mutate(Fert=ifelse(Fertilize=="No", 0, ifelse(Fertilize=='Yes', 1, NA)),
         Herb=ifelse(Herbicide=='No', 0, ifelse(Herbicide=='Yes', 1, NA)),
         Pest=ifelse(Pesticide=='No', 0, ifelse(Pesticide=='Yes', 1, NA))) %>% 
  drop_na

m1<-glm(Fert~race2*income2, family='binomial', data=management)
summary(m1)
plot(allEffects(m1))
m1<-glm(Herb~race2*income2, family='binomial', data=management)
summary(m1)
plot(allEffects(m1))
m1<-glm(Pest~race2*income2, family='binomial', data=management)
summary(m1)
plot(allEffects(m1))

ggplot(data=management, aes(x=income2, y=Herb))+
  geom_smooth(aes(color=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("Apply Herbicide")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=0.75, label="Race p=0.65\nIncome p<0.001*\nRace*Income p=0.04*", size=4)+
  scale_y_continuous(limits=c(0,1), breaks=c(0,1))+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.2))

#Time spent outdoors
time<-datasubset %>% 
  select(race2, income2, Yard_hours_week, Yard_hours_month, Park_hours_week, Park_hours_month, Outdoors_hours_week, Outdoors_hours_moth) %>% 
  mutate(YardMonthHours=ifelse(!is.na(Yard_hours_month), Yard_hours_month, Yard_hours_week*4),
    ParkMonthHours=ifelse(!is.na(Park_hours_month), Park_hours_month, Park_hours_week*4),
        OutdoorMonthHours=ifelse(!is.na(Outdoors_hours_moth), Outdoors_hours_moth, Outdoors_hours_week*4)) %>%
  select(-Yard_hours_month, -Yard_hours_week, -Park_hours_month, -Park_hours_week, -Outdoors_hours_moth, -Outdoors_hours_week) %>% 
  drop_na()

m1<-lm(ParkMonthHours~race2*income2, data=time)
summary(m1)
plot(allEffects(m1))

m1<-lm(YardMonthHours~race2*income2, data=time)
summary(m1)
plot(allEffects(m1))

m1<-lm(OutdoorMonthHours~race2*income2, data=time)
summary(m1)
plot(allEffects(m1))

ggplot(data=time, aes(x=income2, y=ParkMonthHours))+
  geom_smooth(aes(color=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("Hours spent in local\nparks each month")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=60, label="Race p=0.12\nIncome p=0.66\nRace*Income p=0.02*", size=4)+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.2))


#native plants
np<-datasubset %>% 
  select(race2, income2, NativePlants) %>% 
  mutate(native=ifelse(NativePlants=="Very unimportant", -2, ifelse(NativePlants=='Somewhat unimportant', -1, ifelse(NativePlants=='Neutral', 0, ifelse(NativePlants=='Somewhat important', 1, ifelse(NativePlants=='Very important', 2, NA)))))) %>% 
  drop_na

m1<-lm(native~race2*income2, data=np)
summary(m1)
#nothing here

ggplot(data=np, aes(x=income2, y=native))+
  geom_smooth(aes(color=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("Importance of Native Plants")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=1.5, label="Race p=0.90\nIncome p=0.81\nRace*Income p=0.48", size=4)+
  scale_y_continuous(limits=c(-2,2))+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.2))


#yard uses
yarduse<-datasubset %>% 
  select(race2, income2, YardServices) %>% 
  mutate(Relax=ifelse(str_detect(YardServices, "relax"), 1, 0),
         Gather=ifelse(str_detect(YardServices, "gather"), 1, 0),
         Kids=ifelse(str_detect(YardServices, "kids"), 1,0),
         Food=ifelse(str_detect(YardServices, "foood"), 1, 0),
         Wildlife=ifelse(str_detect(YardServices, "wildlife"), 1, 0),
         Aesthetics=ifelse(str_detect(YardServices, "Aesthetically"), 1, 0), 
         Fun=ifelse(str_detect(YardServices, "fun"), 1, 0))

#interaction
m1<-glm(Relax~race2*income2, family='binomial', data=yarduse)
summary(m1)
plot(allEffects(m1))

ggplot(data=yarduse, aes(x=income2, y=Relax))+
  geom_smooth(aes(color=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("Yard is a place to relax")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=0.25, label="Race p=0.07\nIncome p=0.38\nRace*Income p=0.01*", size=4)+
  scale_y_continuous(limits=c(0,1), breaks=c(0,1))+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.2))

#interaction
m1<-glm(Gather~race2*income2, family='binomial', data=yarduse)
summary(m1)
plot(allEffects(m1))

ggplot(data=yarduse, aes(x=income2, y=Gather))+
  geom_smooth(aes(color=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("Yard is a place to gather\nwith friends and family")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=0.25, label="Race p<0.001*\nIncome p=0.03*\nRace*Income p<0.001*", size=4)+
  scale_y_continuous(limits=c(0,1), breaks=c(0,1))+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.2))

#fun
m1<-glm(Fun~race2*income2, family='binomial', data=yarduse)
summary(m1)
plot(allEffects(m1))

ggplot(data=yarduse, aes(x=income2, y=Fun))+
  geom_smooth(aes(color=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("Yard is a place to have fun")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=0.75, label="Race p<0.02*\nIncome p=0.40\nRace*Income p<0.07", size=4)+
  scale_y_continuous(limits=c(0,1), breaks=c(0,1))+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.2))

meanfun<-yarduse %>% 
  group_by(race2) %>% 
  summarize(mean=mean(Fun))

#kids
m1<-glm(Kids~race2*income2, family='binomial', data=yarduse)
summary(m1)
plot(allEffects(m1))

ggplot(data=yarduse, aes(x=income2, y=Kids))+
  geom_smooth(aes(color=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("Yard is a place to\nplay with kids")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=0.75, label="Race p<0.01*\nIncome p=0.81\nRace*Income p<0.01*", size=4)+
  scale_y_continuous(limits=c(0,1), breaks=c(0,1))+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.2))


#food no good
m1<-glm(Food~race2*income2, family='binomial', data=yarduse)
summary(m1)
plot(allEffects(m1))

#wildlife nothing
m1<-glm(Wildlife~race2*income2, family='binomial', data=yarduse)
summary(m1)
plot(allEffects(m1))

ggplot(data=yarduse, aes(x=income2, y=Wildlife))+
  geom_smooth(aes(color=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("Yard is a place to\nsupport wildlife")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=0.75, label="Race p<0.31\nIncome p=0.07\nRace*Income p<0.09", size=4)+
  scale_y_continuous(limits=c(0,1), breaks=c(0,1))+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.8))


#aesthetics income
m1<-glm(Aesthetics~race2*income2, family='binomial', data=yarduse)
summary(m1)
plot(allEffects(m1))

ggplot(data=yarduse, aes(x=income2, y=Aesthetics))+
  geom_smooth(aes(color=race2), method = 'lm')+
  scale_color_manual(name="Race", values=c("dodgerblue", "dodgerblue4"))+
  xlab("Income")+
  ylab("My yard is aesthetically pleasing")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x=50000, y=0.25, label="Race p<0.23\nIncome p=0.04*\nRace*Income p=0.11", size=4)+
  scale_y_continuous(limits=c(0,1), breaks=c(0,1))+
  scale_x_continuous(breaks=c(25000, 75000, 125000,175000))+
  theme(legend.position = c(.8,.2))

