library(readxl)
library(tidyverse)
library(codyn)
library(vegan)

theme_set(theme_bw(20))
setwd('C:\\Users\\mavolio2\\Dropbox\\BES Research\\SegrationIncome\\Baltimore Yards Project\\DataClean')

houses<-read_xlsx("AllHouses.xlsx", sheet = 'HouseInfo')

incomeclasses<-houses %>% 
  group_by(Income, Race) %>% 
  summarise(n=length(Year))

#&Income!='Less than $30,000'&Income!='$130,000 - $150,000'
houses2<-houses %>% 
  mutate(Income=ifelse(HouseID==2102, '$130,000 - $150,000', Income)) %>% 
  filter(Income!='$180-$200,000'&Income!='More than $200,000'&Income!='$150,000 - $170,000'&Income!='Less than $30,000') %>% 
  mutate(IncomeCat=case_when(
    #Income=="Less than $30,000" ~ 20,
    Income=='$30,000-$50,000' ~ 40
    ,Income=='$50,000-$70,000' ~ 60
    ,Income=='$70,000 - $90,000' ~ 80
    ,Income=='$90,000 - $110,000' ~ 100
    ,Income=='$110,000 - $130,000' ~ 120
    ,Income=='$130,000 - $150,000' ~ 140
    ,Income=='$150,000 - $170,000' ~160
    ,.default=999)) %>% 
  select(-Income, -Year)

design<-houses2 %>% 
  group_by(IncomeCat, Race) %>% 
  summarize(n=length(HouseID))

#cleaned names#cleaneYeard names
names_flower<-read_xlsx("flowernames.xlsx", sheet = 'Sheet1')
names_trees<-read_xlsx("treenames.xlsx", sheet = 'treenames')
names_shrubs<-read_xlsx("shrubnames.xlsx", sheet = 'shrubnames')

#data
flower<-read_xlsx("bysflowers2.xlsx", sheet = 'Sheet1') %>% 
  rename(fArea='Floral Area',
         fstm1='F/stem1',
         fstm2='F/stem2',
         fstm3='F/stem3')

trees<-read_xlsx("bystrees.xlsx", sheet = 'Sheet1')
shrubs<-read_xlsx("bysshrubs.xlsx", sheet = 'Sheet1')

#Tree data
trees2<-trees %>%
  mutate(PlantName=tolower(PlantName)) %>% 
  left_join(names_trees) %>% 
  replace_na(list(DBH2=0, DBH3=0, DBH4=0, DBH5=0, DBH6=0, DBH7=0, DBH8=0)) %>% 
  mutate(DBH=sqrt(DBH1^2+DBH2^2+DBH3^2+DBH4^2+DBH5^2+DBH6^2+DBH7^2+DBH8^2), .after = DBH8) %>% 
  select(Year, HouseID, 'F/B', genus, species, DBH) %>% 
  right_join(houses2) %>% 
  replace_na(list(DBH=0))

#tree abundance
treeabund<-trees2 %>% 
  group_by(IncomeCat, Race, HouseID, Data_T) %>% 
  summarize(n=length(DBH)) %>% 
  mutate(n=ifelse(Data_T==0, 0, n))

ggplot(data=treeabund, aes(x=IncomeCat, y=n, color=Race))+
  geom_point()+
  geom_jitter()+
  geom_smooth(se=F, method = 'lm')

m1<-lm(n~Race*IncomeCat, data=treeabund)
anova(m1)

with(treeabund, cor.test(IncomeCat, n))
with(subset(treeabund, Race=='White'), cor.test(IncomeCat, n))
with(subset(treeabund, Race!='White'), cor.test(IncomeCat, n))


treeave<-treeabund %>% 
  group_by(IncomeCat, Race) %>% 
  summarize(mabund=mean(n))

ggplot(data=treeave, aes(x=IncomeCat, y=mabund, color=Race))+
  geom_point()+
  geom_smooth(se=F, method = 'lm')

with(treeave, cor.test(IncomeCat, mabund))
with(subset(treeave, Race=='White'), cor.test(IncomeCat, mabund))
with(subset(treeave, Race!='White'), cor.test(IncomeCat, mabund))

avetreeabund<-treeabund %>% 
  group_by(IncomeCat, Race) %>% 
  summarize(mabund=mean(n), sd=sd(n), n=length(n)) %>% 
  mutate(se=sd/sqrt(n))

avetreeabundcat<-treeabund %>% 
  group_by(Race) %>% 
  summarize(mabund=mean(n), sd=sd(n), n=length(n)) %>% 
  mutate(se=sd/sqrt(n), IncomeCat=160) %>% 
  bind_rows(avetreeabund)

ggplot(data=avetreeabundcat, aes(x=IncomeCat, y=mabund, color=Race))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mabund-se, ymax=mabund+se), width=5)+
  geom_vline(xintercept=150)+
  scale_x_continuous(name = 'Income', breaks=c(40, 80, 120, 160), labels=c('40K', '80K', '120K', 'Overall'))+
  ylab('Number of Yard Trees')+
  scale_color_manual(values = c('dodgerblue', 'green4'), labels=c('African American\nor Black', 'White'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#tree richness
treeabund_byhouse<-trees2 %>%
  mutate(genus_species=paste(genus, species, sep="_")) %>% 
  group_by(HouseID, genus_species) %>% 
  summarize(abund=length(DBH))

treerich<-community_structure(treeabund_byhouse, abundance.var = 'abund', replicate.var = 'HouseID') %>% 
  left_join(houses2)

ggplot(data=treerich, aes(x=IncomeCat, y=richness, color=Race))+
  geom_point()+
  geom_smooth(se=F, method = 'lm')

m1<-lm(richness~Race*IncomeCat, data=treerich)
anova(m1)

treeaverich<-treerich %>% 
  group_by(IncomeCat, Race) %>% 
  summarize(mrich=mean(richness), sd=sd(richness), n=length(richness)) %>% 
  mutate(se=sd/sqrt(n))

treerichcat<-treerich %>% 
  group_by(Race) %>% 
  summarize(mrich=mean(richness), sd=sd(richness), n=length(richness)) %>% 
  mutate(se=sd/sqrt(n), IncomeCat=160) %>% 
  bind_rows(treeaverich)

w<-mean(subset(treerich, Race=='White')$richness)
b<-mean(subset(treerich, Race!='White')$richness)
(w-b)/w

with(subset(treerich, Race=='White'), cor.test(IncomeCat, richness))

ggplot(data=treerichcat, aes(x=IncomeCat, y=mrich, color=Race))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mrich-se, ymax=mrich+se), width=5)+
  geom_vline(xintercept=150)+
  scale_x_continuous(name = 'Income', breaks=c(40, 80, 120, 160), labels=c('40K', '80K', '120K', 'Overall'))+
  ylab('Yard Tree Richness')+
  scale_color_manual(values = c('dodgerblue', 'green4'), labels=c('African American\nor Black', 'White'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  
##Tree DBH
treeDBH<-trees2 %>% 
  group_by(IncomeCat, Race, HouseID, Data_T) %>% 
  summarize(n=sum(DBH)) %>% 
  mutate(n=ifelse(Data_T==0, 0, n))

ggplot(data=treeDBH, aes(x=IncomeCat, y=n, color=Race))+
  geom_point()+
  geom_smooth(se=F, method = 'lm')

m1<-lm(n~Race*IncomeCat, data=treeDBH)
anova(m1)

with(subset(treeDBH, Race=='White'), cor.test(IncomeCat, n))


treeavedhb<-treeDBH %>% 
  group_by(IncomeCat, Race) %>% 
  summarize(mdbh=mean(n), sd=sd(n), n=length(n)) %>% 
  mutate(se=sd/sqrt(n))

treeavedhbcat<-treeDBH %>% 
  group_by(Race) %>% 
  summarize(mdbh=mean(n), sd=sd(n), n=length(n)) %>% 
  mutate(se=sd/sqrt(n), IncomeCat=160) %>% 
  bind_rows(treeavedhb)

ggplot(data=treeavedhbcat, aes(x=IncomeCat, y=mdbh, color=Race))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mdbh-se, ymax=mdbh+se), width=5)+
  geom_vline(xintercept=150)+
  scale_x_continuous(name = 'Income', breaks=c(40, 80, 120, 160), labels=c('40K', '80K', '120K', 'Overall'))+
  ylab('Total Yard DBH (cm)')+
  scale_color_manual(values = c('dodgerblue', 'green4'), labels=c('African American\nor Black', 'White'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



#commontrees
commontrees<-trees2 %>% 
  filter(species!='sp') %>% 
  mutate(genus_species=paste(genus, species, sep="_")) %>% 
  filter(genus_species!='unknown_unknown'&genus_species!='unkown_unknown') %>% 
  group_by(genus_species) %>% 
  summarise(n=length(DBH)) %>% 
  filter(n>1) 


###tree composition
tlong<-trees2 %>% 
  filter(species!='sp') %>% 
  mutate(genus_species=paste(genus, species, sep="_")) %>% 
  filter(genus_species!='unknown_unknown'&genus_species!='unkown_unknown') %>% 
  group_by(Race, IncomeCat, genus_species) %>% 
  summarize(abund=length(DBH)) %>% 
  pivot_wider(names_from = genus_species, values_from = abund, values_fill = 0)

mds<-metaMDS(tlong[3:109], trymax = 500)
stressplot(mds)

plots<-tlong[1:2]
scores<-data.frame(scores(mds,display = "sites"))
scores2<-cbind(plots,scores)

plot(mds, type='t')

ggplot(data=scores2, aes(x=NMDS1, y=NMDS2, shape=as.factor(IncomeCat), color=Race, group=Race))+
  geom_point(size=3)+
  scale_color_manual(labels=c('African American\nor Black', 'White'), values = c('dodgerblue', 'green4'))+
  scale_shape_manual(name='Income', values=c(8:13))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  stat_ellipse()+
  annotate('text', x=-0.7, y = -0.8, label='Stress = 0.14')

#tests
adonis2(tlong[3:109]~Race, data=tlong)
d<-vegdist(tlong[3:109])
beta<-betadisper(d, tlong$Race, type='centroid')
permutest(beta)

##floral data
flower2<-flower %>% 
  mutate(PlantName=tolower(PlantName)) %>% 
 left_join(names_flower) %>% 
  mutate(genus_species=paste(Genus, Species, sep="_")) %>% 
  filter(plants<900)


flsplist<-flower2 %>% 
  group_by(genus_species) %>% 
  summarize(n=length(Genus)) %>% 
  filter(n>3)

flower3<-flower2 %>% 
  right_join(flsplist)

#there is a lot to clean up - meaning there is a lot of outlier data
ggplot(data=flower3, aes(x=fArea))+
  geom_histogram()+
  facet_wrap(~genus_species, scales='free_x')

flowerabund<-flower2 %>% 
  group_by(HouseID, genus_species) %>% 
  summarize(abund=sum(plants))

#flower abundance
flowerabundbyhouse<-flowerabund %>% 
  group_by(HouseID) %>% 
  summarise(tabund=sum(abund)) %>% 
  right_join(houses2)

ggplot(data=flowerabundbyhouse, aes(x=IncomeCat, y=tabund, color=Race))+
  geom_point()+
  geom_smooth(se=F, method = 'lm')

m1<-lm(tabund~Race*IncomeCat, data=flowerabundbyhouse)
anova(m1)

w<-mean(subset(flowerabundbyhouse, Race=='White')$tabund)
b<-mean(subset(flowerabundbyhouse, Race!='White')$tabund)

(w-b)/w
with(flowerabundbyhouse, cor.test(IncomeCat, tabund))
with(subset(flowerabundbyhouse, Race=='White'), cor.test(IncomeCat, tabund))
with(subset(flowerabundbyhouse, Race!='White'), cor.test(IncomeCat, tabund))

flowerabundmean<-flowerabundbyhouse %>% 
  group_by(IncomeCat, Race) %>% 
  summarize(mfabund=mean(tabund), sd=sd(tabund), n=length(tabund)) %>% 
  mutate(se=sd/sqrt(n))

with(subset(flowerabundmean, Race=='White'), cor.test(IncomeCat, mfabund))
with(subset(flowerabundmean, Race!='White'), cor.test(IncomeCat, mfabund))

flowerabundmeancat<-flowerabundbyhouse %>% 
  group_by(Race) %>% 
  summarize(mfabund=mean(tabund), sd=sd(tabund), n=length(tabund)) %>% 
  mutate(se=sd/sqrt(n), IncomeCat=160) %>% 
  bind_rows(flowerabundmean)

ggplot(data=flowerabundmeancat, aes(x=IncomeCat, y=mfabund, color=Race))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mfabund-se, ymax=mfabund+se), width=5)+
  geom_vline(xintercept=150)+
  scale_x_continuous(name = 'Income', breaks=c(40, 80, 120, 160), labels=c('40K', '80K', '120K', 'Overall'))+
  ylab('Number of Flowering Plants')+
  scale_color_manual(values = c('dodgerblue', 'green4'), labels=c('African American\nor Black', 'White'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#flower richness to genera

flowergenera<-flower2 %>% 
  group_by(HouseID, Genus) %>% 
  summarise(abund=sum(plants))

flowerrich<-community_structure(flowergenera, replicate.var = 'HouseID', abundance.var = 'abund') %>% 
  right_join(houses2)

ggplot(data=flowerrich, aes(x=IncomeCat, y=richness, color=Race))+
  geom_point()+
  geom_smooth(se=F, method = 'lm')

hist(flowerrich$richness)
m1<-lm(richness~Race*IncomeCat, data=flowerrich)
anova(m1)

w<-mean(subset(flowerrich, Race=='White')$richness)
b<-mean(subset(flowerrich, Race!='White')$richness)

with(flowerrich, cor.test(IncomeCat, richness))
with(subset(flowerrich, Race=='White'), cor.test(IncomeCat, richness))
with(subset(flowerrich, Race!='White'), cor.test(IncomeCat, richness))
flowerrichmean<-flowerrich %>% 
  group_by(IncomeCat, Race) %>% 
  summarize(mfrich=mean(richness), sd=sd(richness), n=length(richness)) %>% 
  mutate(se=sd/sqrt(n))

with(subset(flowerrichmean, Race=='White'), cor.test(IncomeCat, mfrich))
with(subset(flowerrichmean, Race!='White'), cor.test(IncomeCat, mfrich))

flowerrichmeancat<-flowerrich %>% 
  group_by(Race) %>% 
  summarize(mfrich=mean(richness), sd=sd(richness), n=length(richness)) %>% 
  mutate(se=sd/sqrt(n), IncomeCat=160) %>% 
  bind_rows(flowerrichmean)

ggplot(data=flowerrichmeancat, aes(x=IncomeCat, y=mfrich, color=Race))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mfrich-se, ymax=mfrich+se), width=5)+
  geom_vline(xintercept=150)+
  scale_x_continuous(name = 'Income', breaks=c(40, 80, 120, 160), labels=c('40K', '80K', '120K', 'Overall'))+
  ylab('Genera Richness\nof Flowering Plants')+
  scale_color_manual(values = c('dodgerblue', 'green4'), labels=c('African American\nor Black', 'White'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#floral area
floralarea<-flower2 %>% 
  pivot_longer(fstm1:fstm3, names_to = 'f.stem', values_to='count') %>% 
  group_by(HouseID, Color1, Color2, plants, stems, fArea,genus_species) %>% 
  summarize(avefstem=mean(count, na.rm=T)) %>% 
  mutate(numflowers=stems*avefstem,
         flowerarea=numflowers*fArea) %>% 
  group_by(HouseID) %>% 
  summarize(totarea=sum(flowerarea, na.rm = T)) %>% 
  right_join(houses2)
  
hist(log(floralarea$totarea))

ggplot(data=floralarea, aes(x=IncomeCat, y=totarea, color=Race))+
  geom_point()+
  geom_smooth(se=F, method = 'lm')

m1<-lm(log(totarea)~Race*IncomeCat, data=floralarea)
anova(m1)

w<-mean(subset(floralarea, Race=='White')$totarea)
b<-mean(subset(floralarea, Race!='White')$totarea)
(w-b)/w

# with(floralarea, cor.test(IncomeCat, totarea))
with(subset(floralarea, Race=='White'), cor.test(IncomeCat, totarea))
with(subset(floralarea, Race!='White'), cor.test(IncomeCat, totarea))

flowerareamean<-floralarea %>% 
  group_by(IncomeCat, Race) %>% 
  summarize(mfarea=mean(totarea, na.rm=T), sd=sd(totarea, na.rm=T), n=length(totarea)) %>% 
  mutate(se=sd/sqrt(n))

with(subset(flowerareamean, Race=='White'), cor.test(IncomeCat, mfarea))

flowerareameancat<-floralarea %>% 
  group_by(Race) %>% 
  summarize(mfarea=mean(totarea), sd=sd(totarea), n=length(totarea)) %>% 
  mutate(se=sd/sqrt(n), IncomeCat=160) %>% 
  bind_rows(flowerareamean)

ggplot(data=flowerareameancat, aes(x=IncomeCat, y=mfarea/10000, color=Race))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=mfarea/10000-se/10000, ymax=mfarea/10000+se/10000), width=5)+
  geom_vline(xintercept=150)+
  scale_x_continuous(name = 'Income', breaks=c(40, 80, 120, 160), labels=c('40K', '80K', '120K', 'Overall'))+
  ylab(expression (paste("Total Floral Area (",~m^2,")")))+
  scale_color_manual(values = c('dodgerblue', 'green4'), labels=c('African American\nor Black', 'White'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth(method = 'lm', se=F, linetype='dashed', color='black')

##floral area bar chart

#floral area
floralareaColor<-flower2 %>% 
  pivot_longer(fstm1:fstm3, names_to = 'f.stem', values_to='count') %>% 
  group_by(HouseID, Color1, Color2, plants, stems, fArea,genus_species) %>% 
  summarize(avefstem=mean(count, na.rm=T)) %>% 
  mutate(numflowers=stems*avefstem,
         flowerarea=numflowers*fArea) %>% 
  group_by(HouseID, Color1) %>% 
  summarize(totarea=sum(flowerarea, na.rm = T)) %>% 
  right_join(houses2)

#summarize per color per race
arearace<-floralareaColor %>%
  mutate(Color=ifelse(Color1 %in% c("black",'brown','light brown','unknown'), 'brown4', 
        ifelse(Color1%in% c('dark red', 'maroon', 'red'), 'red', 
        ifelse(Color1 %in% c('dark purple', 'indigo', 'light purple', 'lilac', 'pueple', 'purple'), 'purple',
        ifelse(Color1 %in% c('hot pink', 'light pink', 'magenta', 'peach', 'pink'), 'pink', Color1))))) %>% 
  group_by(Race, IncomeCat, Color) %>% 
  summarise(totarea=sum(totarea))

#ordering variables so legend is in rainbow color order
arearace$value2<-factor(arearace$Color, levels = c("blue","purple",'pink', "red","orange","yellow","green","white", 'brown4'))

ggplot(arearace, aes(x = as.factor(IncomeCat), y=totarea/10000, fill=value2))+
  geom_bar(color="black", width = 0.75,stat="identity")+
  scale_fill_manual(values=c("blue", "purple", 'pink',"red","orange","yellow","green4", "white", 'brown'))+
  ylab(expression (paste("Total Floral Area (",~m^2,")")))+
  xlab("Income")+
  theme(panel.grid=element_blank(), legend.position = "none")+
  facet_wrap(~Race, nrow=2)

####floral composition
flong<-flowergenera %>% 
  left_join(houses2) %>% 
  filter(Genus!='unknown') %>% 
  group_by(Race, IncomeCat, Genus) %>% 
  summarize(abund=sum(abund)) %>% 
  pivot_wider(names_from = Genus, values_from = abund, values_fill = 0) %>% 
  na.omit()

fmds<-metaMDS(flong[3:199], trymax = 500)

fplots<-flong[1:2]
fscores<-data.frame(scores(fmds,display = "sites"))
fscores2<-cbind(fplots,fscores)

plot(fmds, type='t')

ggplot(data=fscores2, aes(x=NMDS1, y=NMDS2, shape=as.factor(IncomeCat), color=Race, group=Race))+
  geom_point(size=3)+
  scale_color_manual(labels=c('African American\nor Black', 'White'), values = c('dodgerblue', 'green4'))+
  scale_shape_manual(name='Income', values=c(8:13))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  stat_ellipse()+
  annotate('text', x=-0.7, y=-1, label= 'Stress = 0.12' )

#tests
adonis2(flong[3:199]~Race, data=flong)
fd<-vegdist(flong[3:199])
fbeta<-betadisper(fd, flong$Race, type='centroid')
permutest(fbeta)



houses3<-houses2 %>% 
  group_by(IncomeCat, Race) %>% 
  summarise(n=length(HouseID)) %>% 
  pivot_wider(values_from = 'n', names_from = 'Race')
