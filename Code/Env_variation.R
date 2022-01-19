#knb-lter-nwt.10.2
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
source("Data/knb-lter-newt.161.3.r")
#Lets look at our data
str(dt1)
summary(dt1)


#Data set dt1 is on environemtnal variables


###############################################################################################################################################################################################################################################
#1) Addsome new columns on lake network number and elevation

dt1$lake_network_number<-if_else(dt1$local_site=="GL5",1,if_else(dt1$local_site=="GL4",2,if_else(dt1$local_site=="GL3",3,if_else(dt1$local_site=="GL2",4,if_else(dt1$local_site=="ALB",5,0)))))
dt1$elevation<-if_else(dt1$local_site=="GL5",3621,if_else(dt1$local_site=="GL4",3563,if_else(dt1$local_site=="GL3",3455,if_else(dt1$local_site=="GL2",3408,if_else(dt1$local_site=="ALB",3357,3431)))))

cem_var<-(colnames(dt1), sep="\n")
#2)Explore Environmental Patterns
dt1%>%
  gather(pH,conduct ,ANC,H.plus.,NH4.plus.,Ca.plus..plus., Mg.plus..plus., Na.plus., K.plus., Cl.hyphen.,NO3.hyphen., SO4.hyphen..hyphen., PO4.hyphen..hyphen..hyphen.,     Si, cat_sum,  an_sum, chg_bal,    TN,    TDN,   PN,   DON,     IN,   TP,   TDP,   PP,   DOP,    IP, d18O,d18O_sdev, dDeut, dD_sdev, D_excess, Trit, T_sdev, TOC, DOC, POC,key = "var", value = "value") %>% 
  ggplot(aes(x = as.factor(local_site), y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_boxplot()+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

dt1%>%
  gather(cem_var,key = "var", value = "value") %>% 
  ggplot(aes(x = as.factor(local_site), y = value)) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_boxplot()+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

dt1%>%
  gather(pH,conduct ,ANC,H.plus.,NH4.plus.,Ca.plus..plus., Mg.plus..plus., Na.plus., K.plus., Cl.hyphen.,NO3.hyphen., SO4.hyphen..hyphen., PO4.hyphen..hyphen..hyphen.,     Si, cat_sum,  an_sum, chg_bal,    TN,    TDN,   PN,   DON,     IN,   TP,   TDP,   PP,   DOP,    IP, d18O,d18O_sdev, dDeut, dD_sdev, D_excess, Trit, T_sdev, TOC, DOC, POC,key = "var", value = "value") %>% 
  ggplot(aes(x = elevation, y = value, colour=as.factor(year))) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()

#remove varibles missing for most sties
dt1%>%
  gather(pH,conduct ,ANC,H.plus.,NH4.plus.,Ca.plus..plus., Mg.plus..plus., Na.plus., K.plus., Cl.hyphen.,NO3.hyphen., SO4.hyphen..hyphen., PO4.hyphen..hyphen..hyphen.,     Si, cat_sum,  an_sum, chg_bal,    TN,    TDN,   PN,   DON,     IN,   TP,   TDP,   PP,   DOP,    IP,  DOC,key = "var", value = "value") %>% 
  ggplot(aes(x = elevation, y = value, colour=as.factor(year))) + #remove , fill=Network and see what the grpah looks like, are there tredns that both entowrks share together
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  facet_wrap(~ var, scales = "free") +
  theme_bw()


#PCA
env_var<-dt1%>%
  dplyr::select(c(local_site,date,year,lake_network_number,elevation,pH,conduct ,ANC,H.plus.,Ca.plus..plus., Mg.plus..plus., Na.plus., K.plus., Cl.hyphen.,NO3.hyphen., SO4.hyphen..hyphen.,     Si, cat_sum,  an_sum,   TDN,   DON))%>%
  drop_na()

dog.stream <-prcomp(env_var[,6:21], center = TRUE,scale. = TRUE) #PCA code
dog.stream #this shows us hpw our data lines up PC1
summary(dog.stream)
ggbiplot(dog.stream, labels=rownames(interaction(env_var$local_site, env_var$year)), groups=interaction(env_var$local_site, env_var$year), ellipse=TRUE)
ggbiplot(dog.stream, labels=rownames(interaction(env_var$local_site)), groups=interaction(env_var$local_site), ellipse=TRUE)

dog.stream
#PERMANOVA
adonis2(env_var[,6:21]~lake_network_number+local_site+as.factor(year),method="euclidean", data=env_var)

          
                             