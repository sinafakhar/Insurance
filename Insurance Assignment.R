# To do list:
#            1- Adding exploration (histgram for number of claims & expo) and amount
#            2- Adding all continues as smooth function and have their plots like article
#            3- Adding all interactions (of continues factors) separately above the main effects
#            4- For severity use average (amount/nclaim) only nclaims>0 lognormal remove claims above 81,000
#            5- Using severity * number of claims
#            6- Adding exaustive search and based on AIC selecting the best subset
#            7- How is the central of lang and lot is calculated? 
#            8- Group municipalities with similar spatial riskiness together 



library(tidyverse)
library(DataExplorer)
library(MASS)
library(mgcv)
library(sf)
library(sp)
library(tmap)
library(gbm)
KULbg = "#116E8A" 

data= read.table("https://raw.githubusercontent.com/sinafakhar/Insurance/master/data.csv",sep=",",header=T)
inpost=read.table("https://raw.githubusercontent.com/sinafakhar/Insurance/master/inspost.csv", sep=",", header=T)

######################## PREPROCESSING DATASETS #######################################

#DataExplorer::create_report(data)                         #Exploring all the data at a glance
names(data)=tolower(colnames(data))                       #Making all the column names lower
names(inspost)=tolower(colnames(inspost))                 #Making all the column names lower

data1=data %>% inner_join(inspost, by = "codposs")        #Joining lang and lat to data1
data1=data1[,-c(17,18)]                                   #Removing commune and ins from data1

which(data1$chargtot>1000000)
data2=data1[-11749,]
sample <- sample.int(n = nrow(data2), size = floor(.8*nrow(data1)), replace = F)
train <- data2[sample, ]
test  <- data2[-sample, ]
mean(test$chargtot)
mean(train$chargtot)
sum(test$nbrtotc)*4
sum(train$nbrtotc)
####################### EXPLORATION ##################################################



###################### PLOTS FOR FREQUENCY OF CLAIMS WITH EXPOSURE ##############

                              
 ggplot(train, aes(nbrtotc)) + theme_bw() +       #Frequency of total claims
   geom_bar(col = KULbg, fill = KULbg) +
   labs(y = "Abs frequency") +
  ggtitle("number of claims")


 ggplot(train, aes(nbrtotc)) + theme_bw() +       #Frequency of total claims weighted with exposure
   geom_bar(aes(weight = duree), col = KULbg,
           fill = KULbg) +
   labs(y = "Abs freq (in exposure)") +
   ggtitle("number of claims")
 

 ggplot(train, aes(chargtot)) + theme_bw() +       #Severity 
   geom_histogram(col = KULbg, fill = KULbg) +
   labs(y = "Abs frequency") +
   ggtitle("number of claims")+scale_y_log10()

 ggplot(train, aes(x=ageph, y=chargtot)) +    #One outlier 
   geom_point(alpha=.4, size=4, color=KULbg)

 ggplot(train, aes(coverp)) + theme_bw() +       #Frequency of coverp
   geom_bar(col = KULbg, fill = KULbg) +
   labs(y = "Abs frequency") +
   ggtitle("number of claims")
 
  ######################## GLM MODEL ############################################

glm.model1=glm(nbrtotc~ offset(log(duree))+ageph+codposs+agecar+sexp+fuelc+split+
                 fleetc+usec+sportc+coverp+powerc+ageph:sexp,train, 
               family = poisson(link="log"))   #Try GLM family model
summary(glm.model1)




model1=stepAIC(glm.model1,k=2, direction="both", scope=list(upper=~., lower=~1))      #Stepwise for both AIC and BIC to see which model is best 
summary(model1)
model1$anova

data3=data1%>% filter(nbrtotc>0)      #Keeping just positive number of claims (removing 0)
glm.model2=glm(chargtot~offset(log(duree))+ageph+codposs+agecar+
                 sexp+fuelc+split+fleetc+usec+sportc+coverp+
                 powerc+ageph:sexp, data3, family = Gamma (link="log"))   #Try GLM family model
summary(glm.model2)

par(mfrow=c(2,2))
plot(glm.model)

model1=stepAIC(glm.model2,k=2, direction="both", scope=list(upper=~., lower=~1))      #Stepwise for both AIC and BIC to see which model is best 
summary(model1)
par(mfrow=c(2,2))
plot(model1)


####################### GAM MODEL ###########################################
 
set.seed(123)
data.sampled= data1[sample(1:nrow(data1),100, replace=FALSE),]

belgium_shape_sf <- st_read(file.choose(), quiet = TRUE)
belgium_shape_sf <- st_transform(belgium_shape_sf, CRS("+proj=longlat +datum=WGS84"))
belgium_shape_sf= st_simplify(belgium_shape_sf,dTolerance = 0.00001)
class(belgium_shape_sf)
dim(belgium_shape_sf)


ggplot(belgium_shape_sf) +
  geom_sf() +
  ggtitle("Welcome to Belgium!") +
  theme_bw()
  
simple_shp = st_simplify(belgium_shape_sf, dTolerance = 0.00001)
qtm(simple_shp)

tm_shape(simple_shp) +
  tm_borders(col = KULbg, lwd = 0.5) +
  tm_layout(main.title = 'Welcome to Belgium!',
            legend.outside = TRUE, frame = FALSE)

#post_expo= data.sampled %>% group_by(codposs) %>% summarize(num = n(), total_expo = sum(duree))
#belgium_shape_sf= left_join(belgium_shape_sf,post_expo,by= c("POSTCODE"="codposs"))



#gam.model=gam(nbrtotc~s(ageph, sp=1.2,k=5, bs="cr"),family=poisson, data1)       #Manually k,bs
#gam.model1=gam(nbrtotc~s(ageph), method="REML",family=poisson, data1)    #Using REML
#print(gam.model1)





model.gam= gam(nbrtotc~s(ageph)+s(long,lat,bs="tp")+agecar+sexp+fuelc+
              split+fleetc+usec+fleetc+sportc+coverp+powerc,
            offset=log(duree), method= "REML", family=poisson, train)
summary(model.gam)
model.gam$sp
plot(model.gam, pages=1, scheme=0)
plot(model.gam, pages=1, scheme=1)
  
post_dt = st_centroid(belgium_shape_sf)
post_dt$long= do.call(rbind, post_dt$geometry)[,1]
post_dt$lat= do.call(rbind, post_dt$geometry)[,2]


freq_gam_spatial=gam(nbrtotc ~ s(long, lat,bs = "tp"), offset = log(duree),
                    family =poisson(link = "log"), data = train)

pred=predict(freq_gam_spatial, newdata = post_dt, type="terms", terms="s(long,lat)")
dt_pred=data.frame(pc=post_dt$POSTCODE,long=post_dt$long, lat=post_dt$lat, pred)
names(dt_pred)[4] = "fit_spatial"



belgium_shape_sf= left_join(belgium_shape_sf, dt_pred, by=c("POSTCODE"="pc"))
num_bins=5
library(classInt)
classint_fisher=classIntervals(dt_pred$fit_spatial,num_bins,style="fisher")
classint_fisher$brks
min(dt_pred$fit_spatial)
max(dt_pred$fit_spatial)
belgium_shape_sf$class_fisher=cut(belgium_shape_sf$fit_spatial,breaks = classint_fisher$brks,
                                  right = FALSE, include.lowest = TRUE,dig.lab = 2)



mtpl_geo= train%>%dplyr::select(nbrtotc, duree, coverp, fuelc, ageph,codposs)
mtpl_geo=left_join(mtpl_geo, dt_pred,by=c("codposs"="pc"))
mtpl_geo$geo=as.factor(cut(mtpl_geo$fit_spatial,breaks = classint_fisher$brks, right = FALSE,
                           include.lowest = TRUE, dig.lab = 2))
head(mtpl_geo$geo)
geomodel= gam(nbrtotc~ geo+coverp+fuelc+s(ageph), data=mtpl_geo, family = poisson(link="log"))
summary(geomodel)

##### GLM with grouped spatial ##############
geo=mtpl_geo[,c("geo","codposs")]
train1=train[,-c(17,18,4,6)]
train2=left_join(train1, geo, by="codposs")
glm.geo=glm(nbrtotc~offset(log(duree))+ageph+agecar+
                 sexp+fuelc+split+fleetc+usec+sportc+coverp+
                 powerc+ageph:sexp, train1, family = poisson (link="log")) 
summary(glm.geo)
# ggplot(belgium_shape_sf)+geom_sf(aes(fill="fit_spatial"), color=NA)+scale_fill_gradient(low="#99CCFF",high= "#003366")+theme_bw()
# tm_shape(belgium_shape_sf) +
#   tm_borders(col = 'white', lwd = .1) +
#   tm_fill("fit_spatial", style = "cont",
#           palette = "RdBu", legend.reverse = TRUE,
#           auto.palette.mapping = TRUE) +
#   tm_layout(legend.title.size = 1.0,
#             legend.text.size = 1.0)


ggplot(belgium_shape_sf) +
  geom_sf(aes(fill = s.long.lat.), colour = NA) +
  ggtitle("claim frequency data") +
  scale_fill_gradient(low = "#99CCFF",
                      high = "#003366") +
  theme_bw()


############################ Gradient Boosting#################################
# Training control definition
set.seed(123) #For reproducibility
train.control <- trainControl(method = "repeatedcv", savePredictions = "final",
                              number = 5, repeats = 3)
train1=train[,-c()]
model6_max <- train(nbrtotc ~ . , data = train,
                    method = "gbm", trControl = train.control, metric = "MAE")


