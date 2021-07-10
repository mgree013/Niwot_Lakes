#knb-lter-newt.157.5.r
#Data Explration NIWOT
#Monday August 24, 2020

#Install packages and load them
library(ggplot2)
library(tidyverse)
library(reshape)

#Lets look at our data
str(dt1)
summary(dt1)
str(dt2)
summary(dt2)

#Data set dt1 is zooplanlton traits
#Data set set dt2 has info on species density

#######################################################################################################################
#1) Zooplankotn Diversity


#1A)Organize Data
#Make Data Site*Species  Data Frame

#Lets reshape the dataset to have our columns as species names and rows as unique location,sites, and date.
site_species <- cast(dt2, date+local_site+location ~ taxon, value='density')
site_species <- as.data.frame(site_species)
site_species[is.na(site_species)] <- 0
str(site_species)

site_species<-site_species%>% unite("Site.loc.date", c(local_site,date,location))%>%
  remove_rownames()%>%
  column_to_rownames('Site.loc.date')


diversity<-dt2%>%
  filter(taxon != "total")%>% #this  removes the  total column  from taxon
  dplyr::select(-c(count,liters_examined,pull_number,LTER_site))%>%
  unite("Site.loc.date", c(local_site,date,location))


  pivot_wider(id_cols=Site.loc.date,names_from = taxon, values_from=density)
  
  column_to_rownames("Site.loc.date")%>%
  group_by(Site.loc.date)%>%
  summarize()  
  
  
dt2%>%
  filter(local_site=="GL4")%>%
    group_by(date, taxon, local_site) %>%
    ggplot(mapping = aes(x = as.factor(date), y = density)) +
    geom_point()+
    facet_wrap(~ taxon)

dt1%>%
  gather(chl_a,pH,temp,std_conduct,conduct,DO,sat,nitrate,secchi,PAR, key = "var", value = "value") %>% 
  ggplot(aes(x = local_site, y = value, color = local_site)) + geom_boxplot()+
  facet_grid(year~ var, scales = "free") + theme_bw()

dt1%>%
  gather(chl_a,pH,temp,std_conduct,conduct,DO,sat,nitrate,secchi,PAR, key = "var", value = "value") %>% 
  ggplot(aes(x = local_site, y = value, color = local_site)) + geom_boxplot()+
  facet_wrap(~ var, scales = "free") + theme_bw()







dt2%>%
  gather(density, key = "var", value = "value") %>% 
  ggplot(aes(x = taxon, y = value, color = local_site)) + geom_boxplot()+
  facet_wrap(~ local_site, scales = "free") + theme_bw()

#knb-lter-newt.161.3.r

dt2%>%
  filter(taxon=="total")%>%
  gather(density, key = "var", value = "value") %>% 
  ggplot(aes(x = as.factor(date), y = value, color = local_site)) + geom_boxplot()+
  facet_wrap(~ local_site, scales = "free") + theme_bw()

#knb-lter-newt.64.1.r
dt1%>%
  ggplot(aes(x=as.factor(year), y=cell_count))+
  geom_boxplot()+
  facet_wrap(~taxon)

#1) species  Diversity
#2) Biomass
#3)
