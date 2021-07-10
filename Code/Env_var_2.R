#xknb-lter-nwt.157.5.r
#Data Explration NIWOT
#Monday August 24, 2020

#Install packages and load them
library(ggplot2)
library(tidyverse)
library(reshape)
library(vegan)
library(adespatial)
library(lubridate)
library(ggbiplot)

#run the other  attatched r script to load the data entitled "knb-lter-newt.161.3.r'

#Lets look at our data
str(dt1)
summary(dt1)


#Data set dt1 is on environemtnal variables


###############################################################################################################################################################################################################################################
#1) Addsome new columns on lake network number and elevation

dt1$lake_network_number<-if_else(dt1$local_site=="GL5",1,if_else(dt1$local_site=="GL4",2,if_else(dt1$local_site=="GL3",3,if_else(dt1$local_site=="GL2",4,if_else(dt1$local_site=="ALB",5,if_else(dt1$local_site=="GL1",4,0))))))
dt1$elevation<-if_else(dt1$local_site=="GL5",3621,if_else(dt1$local_site=="GL4",3563,if_else(dt1$local_site=="GL3",3455,if_else(dt1$local_site=="GL2",3408,if_else(dt1$local_site=="ALB",3357,if_else(dt1$local_site=="GL1",3331,3652))))))


#2)Explore Environmental Patterns
dt1%>%
  gather(chl_a,pH,temp,std_conduct,conduct,DO,sat,nitrate,secchi,PAR,key = "var", value = "value") %>% 
  ggplot(aes(x = as.factor(local_site), y = value,colour=as.factor(year))) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_boxplot()+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

dt1%>%
  gather(chl_a,pH,temp,std_conduct,conduct,DO,sat,nitrate,secchi,PAR,key = "var", value = "value") %>% 
  ggplot(aes(x = local_site, y = value, colour=as.factor(year))) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

#remove varibles missing for most sties
dt1%>%
  gather(chl_a,pH,temp,std_conduct,conduct,DO,sat,nitrate,secchi,PAR,key = "var", value = "value") %>% 
  ggplot(aes(x = elevation, y = value, colour=as.factor(year))) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

dt1%>%
  gather(chl_a,pH,temp,std_conduct,conduct,DO,sat,nitrate,secchi,PAR,key = "var", value = "value") %>% 
  ggplot(aes(x = elevation, y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

dt1%>%
  filter(local_site !="GL1")%>%
  gather(chl_a,pH,temp,std_conduct,conduct,DO,sat,nitrate,secchi,PAR,key = "var", value = "value") %>% 
  ggplot(aes(x = lake_network_number, y = value, colour=as.factor(year))) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

#Chlorphyll a, conductivity,temp decrease moving down in  elevation

#PCA
env_varz<-dt1%>%
  dplyr::select(c(local_site,year,chl_a,pH,temp,conduct,DO, lake_network_number,elevation))%>%
  drop_na()

dog.stream <-prcomp(env_varz[,3:7], center = TRUE,scale. = TRUE) #PCA code
dog.stream #this shows us hpw our data lines up PC1
summary(dog.stream)
ggbiplot(dog.stream, labels=rownames(interaction(env_var$local_site, env_var$year)), groups=(interaction(env_var$local_site, env_var$year)), ellipse=TRUE)

dog.stream
#PERMANOVA
adonis2(env_varz[,3:7]~as.factor(lake_network_number)*local_site*as.factor(year), data=env_varz)

