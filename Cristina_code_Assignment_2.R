#######
#Libraries#####
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(DataExplorer)
library(mgcv)
library(gbm)
library(rpart)
library(caret)
library(MASS)

library(ggpubr)
library (randomForest)

library(sf)
library(sp)
library(tmap)
#Read data####
cars<-fread('https://raw.githubusercontent.com/sinafakhar/Insurance/master/data.csv')
postcodes<-fread("https://raw.githubusercontent.com/sinafakhar/Insurance/master/inspost.csv")
# setwd("/Users/cristina/Dropbox/Data science for non life insurance/Project")
# cars<-fread('Assignment2.csv')
# postcodes<-fread("inspost.csv")



#Exploration, preprocessing####

##Preprocessing###
names(cars)=tolower(colnames(cars))     
names(postcodes)=tolower(colnames(postcodes)) 


#Explore
#DataExplorer::create_report(cars)
cars[chargtot != 0 , plot(table(chargtot))]
cars[duree != 1 , plot(table(duree))]

cars[chargtot>1000000] #1989568 

cars[,lnexpo - log(duree)]

sapply(cars,class)
     

#__Merge postcodes####
cars.new[,summary(postcodes)]
cars<-merge(cars,postcodes, by = "codposs", all.x = TRUE)
cars[is.na(lat),.N]

##factor
sapply(cars,class)
# codposs       ageph       duree      lnexpo     nbrtotc 
# "integer"   "integer"   "numeric"   "numeric"   "integer" 
# nbrtotan    chargtot      agecar        sexp       fuelc 
# "numeric"   "numeric"    "factor"    "factor"    "factor" 
# split        usec      fleetc      sportc      coverp 
# "factor"    "factor"    "factor"    "factor"    "factor" 
# powerc         ins     commune         lat        long 
# "factor"   "integer" "character"   "numeric"   "numeric" 
cars[,agecar:=as.factor(agecar)]                    
cars[,sexp:=as.factor(sexp)]
cars[,agecar:=as.factor(agecar)]
cars[,fuelc:=as.factor(fuelc)]
cars[,split:=as.factor(split)]
cars[,usec:=as.factor(usec)]
cars[,fleetc:=as.factor(fleetc)]
cars[,sportc:=as.factor(sportc)]
cars[,coverp:=as.factor(coverp)]
cars[,powerc:=as.factor(powerc)]



###GAM####
#__GAM frequency####
#__1####
fit1<- gam( nbrtotc  ~ s(codposs) + s(ageph) + agecar + sexp + 
             fuelc + usec + fleetc + split + sportc + coverp + powerc,
           offset = lnexpo, 
           data = cars, 
           family = poisson(link = "log")) 
summary(fit1)

#__2####
fit2<- gam( nbrtotc  ~ s(lat) + s(long) +s(ageph) + agecar + sexp + 
              fuelc + usec + fleetc + split + sportc + coverp + powerc,
            offset = lnexpo, 
            data = cars, 
            family = poisson(link = "log")) 
summary(fit2)

#__3####
fit3<- gam( nbrtotc  ~ s(codposs) +s(ageph) + agecar + sexp + 
              fuelc + usec + fleetc + split + coverp + powerc,
            offset = lnexpo, 
            data = cars, 
            family = poisson(link = "log")) 
summary(fit3)
#__4 *####
fit4<- gam( nbrtotc  ~ s(codposs) +s(ageph) + agecar + sexp + 
              fuelc + fleetc + split + coverp + powerc,
            offset = lnexpo, 
            data = cars, 
            family = poisson(link = "log")) 
summary(fit4)
#__5####
fit5<- gam( nbrtotc  ~ s(codposs) +s(ageph) + agecar + 
              fuelc + fleetc + split + coverp + powerc,
            offset = lnexpo, 
            data = cars, 
            family = poisson(link = "log")) 
summary(fit5)


#__GAM severity####
################# #
#__6####
fit6<- gam( chargtot  ~ s(codposs) + agecar + sexp + 
              fuelc + usec + fleetc + split + sportc + coverp + powerc,
            offset = lnexpo, 
            data = cars[!(nbrtotc == 0)], 
            family = Gamma (link = "log")) 
summary(fit6)

#__7####
fit7<- gam( chargtot  ~ s(codposs) + agecar + sexp + 
              fuelc + usec + fleetc + split + sportc + coverp + powerc,
            offset = lnexpo, 
            data = cars[!(nbrtotc == 0) & chargtot < 1000000], 
            family = Gamma (link = "log")) 
summary(fit7)

#__8 *####
fit8<- gam( chargtot  ~ s(lat) + s(long) + agecar + sexp + 
              fuelc + usec + fleetc + split + sportc + coverp + powerc,
            offset = lnexpo, 
            data = cars[!(nbrtotc == 0) & chargtot < 1000000], 
            family = Gamma (link = "log")) 
summary(fit8)

#__9####
fit9<- gam( chargtot  ~ s(lat) + s(long) + sexp + 
              fuelc + usec + fleetc + split + sportc + coverp + powerc,
            offset = lnexpo, 
            data = cars[!(nbrtotc == 0) & chargtot < 1000000], 
            family = Gamma (link = "log")) 
summary(fit9)

#__10####
fit10<- gam( chargtot  ~  agecar + sexp + 
              fuelc + usec + fleetc + split + sportc + coverp + powerc,
            offset = lnexpo, 
            data = cars[!(nbrtotc == 0) & chargtot < 1000000], 
            family = Gamma (link = "log")) 
summary(fit10)
#__11####




AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit6)
AIC(fit7)
AIC(fit8)
AIC(fit9)
####TREE#####


set.seed(111) # reproducibility
fit_tree <- rpart(formula = nbrtotc  ~ codposs + ageph + agecar + sexp + 
               fuelc + usec + fleetc + split + sportc + coverp + powerc,
                      data = cars, 
                method = 'poisson',
              control = rpart.control(
               maxdepth = 20,
               minsplit = 20,
               minbucket = 10,
               cp = 0,
               xval = 5
             )
)
print(fit_tree)
rpart.plot(fit_tree, digits = 4, cex = 1.5)

###GRADIENT BOOSTING####

set.seed(111) # reproducibility



fit_boost<- gbm(formula = nbrtotc  ~ codposs + ageph + agecar + sexp + 
             fuelc + fleetc + split + coverp + powerc,
          # offset = lnexpo, 
           data = cars, 
          distribution = 'poisson',
           n.trees = 115,
           interaction.depth = 3,)
summary(fit_boost)






#####COMPARISON######

train
valid
test

