library(reshape)
library(readxl)
library(plyr)
library(dplyr)
library(MASS)

#load data
link <-  read.csv("C:/Users/au623804/OneDrive/_Dark diversity/data/fungi_link.csv", encoding="UTF-8", sep=";")
cleaned_link<- read.csv("C:/Users/au623804/OneDrive/_Dark diversity/data/linkage_rasmus_cleaned.csv", encoding="UTF-8", sep=";")
SamletArtsdata <- read_excel("SamletArtsdata.xlsx")
SamletArtsdata$latin=as.factor(SamletArtsdata$latin)


#clean linkage data
fungi_link <- merge(cleaned_link,link,all.x=T)
fungi_link=unique(fungi_link)


#only observed biowide plants 
plants <-  subset(SamletArtsdata, species_group == "Plants")
plants = droplevels(plants)

#only plants and fungi observations
pf.obs <-  subset(SamletArtsdata, species_group == "Fungi"| species_group == "Plants")
pf.obs = droplevels(pf.obs)

#only fungi observations
fungi <-  subset(SamletArtsdata, species_group == "Fungi")
fungi = droplevels(fungi)



###predicted fungi regional pool based on linkage

#plant species linkage
link.sp <-  subset(fungi_link, Genus == 0)
link.sp <- link.sp[,c("Fungi","Plant")]
link.sp=unique(link.sp)
link.sp=droplevels(link.sp)

#plant genus linkage
link.g <-  subset(fungi_link, Genus != 0)
link.g <- link.g[,c("Fungi","Genus")]
link.g=unique(link.g)
link.g=droplevels(link.g)


#merge 
######  
totalg <- merge(link.g,plants,by.x=c("Genus"), by.y=c("genus"),all.y=T)
totals <- merge(link.sp,plants,by.x=c("Plant"), by.y=c("latin"),all.y = T)

total=merge(totalg[,c("Fungi","site_nr")],totals[,c("Fungi","site_nr")],all = T)
total=unique(total)
total = droplevels(total)

#add fungal species observed 

total=merge(fungi,total, by.x=(c("latin", "site_nr")), by.y=(c("Fungi", "site_nr")), all = T)
total$freq=1
total = droplevels(total)


#predicted fungal species matrix

#with counts
fungi.matrix=cast(total, site_nr~latin, value = "freq", add.missing=T)
fungi.matrix[is.na(fungi.matrix)] <- 0

#presence/absence predicted
fungi.matrix[,-1][fungi.matrix[,-1]>0]<-1

rowSums(fungi.matrix[,-1])


###observed fungi
#add site observations with no fungi
total$freq.obs=ifelse(is.na(total$phylum), 0,1)

observed.fungi.matrix=cast(total, site_nr~latin,value = "freq.obs", add.missing=T)
observed.fungi.matrix[is.na(observed.fungi.matrix)] <- 0

#presence/absence observed
observed.fungi.matrix[,-1][observed.fungi.matrix[,-1]>0]<-1
rowSums(observed.fungi.matrix[,-1])


####dark diversity

predicted=rowSums(fungi.matrix[,-1]!=0)
predicted=as.data.frame(predicted)
predicted$site_nr=fungi.matrix$site_nr


observed=rowSums(observed.fungi.matrix[,-1]!=0)
observed=as.data.frame(observed)
observed$site_nr=observed.fungi.matrix$site_nr

darkdiv=merge(observed, predicted)
darkdiv[is.na(darkdiv)] <- 0

darkdiv$DD=darkdiv$predicted - darkdiv$observed
darkdiv$relative_DD=darkdiv$DD/darkdiv$predicted
darkdiv=na.omit(darkdiv)




########no genus linkages for regional pool
total.nogenus <- merge(totals,fungi,by.x=c("Fungi","site_nr"), by.y=c("latin","site_nr"),all=T)
total.nogenus=unique(total.nogenus)
total.nogenus = droplevels(total.nogenus)


#with counts
total.nogenus$freq=1
fungi.matrix.nogenus=cast(total.nogenus, site_nr~Fungi, value = "freq", add.missing=T)
fungi.matrix.nogenus[is.na(fungi.matrix)] <- 0

#presence/absence predicted
fungi.matrix.nogenus[,-1][fungi.matrix.nogenus[,-1]>0]<-1

rowSums(fungi.matrix.nogenus[,-1])



predicted.nogenus=rowSums(fungi.matrix.nogenus[,-1]!=0)
predicted.nogenus=as.data.frame(predicted.nogenus)
predicted.nogenus$site_nr=fungi.matrix.nogenus$site_nr

darkdiv=merge(darkdiv, predicted.nogenus)

darkdiv$DD.nogenus=darkdiv$predicted.nogenus - darkdiv$observed
darkdiv$relative_DD.nogenus=darkdiv$DD.nogenus/darkdiv$predicted.nogenus

write.csv(darkdiv, "darkdiv.csv")
