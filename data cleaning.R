library(tidyverse)
library(readxl)
library(WriteXLS)

#import data\
flowers<-read_excel('C:\\Users\\mavolio2\\OneDrive - Johns Hopkins\\Olivia Avolio_Lab\\Baltimore Yards Project\\DataClean\\bysflowers.xlsx') %>% 
  select(PlantName) %>% 
  mutate(PlantName=tolower(PlantName)) %>% 
  unique() %>% 
  separate(PlantName, into=c('genus', 'species', 'other', 'more', sep=" "), remove = F)

write.csv(flowers, 'C:\\Users\\mavolio2\\OneDrive - Johns Hopkins\\Olivia Avolio_Lab\\Baltimore Yards Project\\DataClean\\flowernames.csv', row.names = F)


write.csv()trees<-read_excel('C:\\Users\\mavolio2\\OneDrive - Johns Hopkins\\Olivia Avolio_Lab\\Baltimore Yards Project\\DataClean\\bystrees.xlsx') %>% 
  select(PlantName) %>% 
  mutate(PlantName=tolower(PlantName)) %>% 
  unique() %>% 
  separate(PlantName, into=c('genus', 'species', 'other', 'more', sep=" "), remove = F)
write.csv(trees, 'C:\\Users\\mavolio2\\OneDrive - Johns Hopkins\\Olivia Avolio_Lab\\Baltimore Yards Project\\DataClean\\treenames.csv', row.names = F)


shrubs<-read_excel('C:\\Users\\mavolio2\\OneDrive - Johns Hopkins\\Olivia Avolio_Lab\\Baltimore Yards Project\\DataClean\\bysshrubs.xlsx') %>% 
  select(PlantName) %>% 
  mutate(PlantName=tolower(PlantName)) %>% 
  unique() %>% 
  separate(PlantName, into=c('genus', 'species', 'other', 'more', sep=" "), remove = F)
write.csv(shrubs, 'C:\\Users\\mavolio2\\OneDrive - Johns Hopkins\\Olivia Avolio_Lab\\Baltimore Yards Project\\DataClean\\shrubnames.csv', row.names = F)
