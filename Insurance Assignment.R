
library(tidyverse)
library(DataExplorer)
library(MASS)
library(mgcv)
library(sf)
library(sp)
library(tmap)
library(gbm)
library(caret)
library(ggplot2)
library(ggpubr)
KULbg = "#116E8A" 

data= read.table("https://raw.githubusercontent.com/sinafakhar/Insurance/master/data.csv",sep=",",header=T)
inpost=read.table("https://raw.githubusercontent.com/sinafakhar/Insurance/master/inspost.csv", sep=",", header=T)

######################## PREPROCESSING DATASETS #######################################

#DataExplorer::create_report(data)                        #Exploring all the data at a glance
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

population= data2[sample(1:nrow(data2), 1000, replace=FALSE),]
sample1= sample.int(n = nrow(data2), size = floor(.8*nrow(population)), replace = F)

minitrain <- data2[sample1, ]
minitest  <- data2[-sample1, ]


####################### EXPLORATION ##################################################
#Old cars have more number of claims
theme_set(theme_pubr())

ggplot(train, aes(x = sportc, y = nbrtotc))+
  geom_bar(
    aes(fill = agecar), stat = "identity", color = "white",
    position = position_dodge(0.9)
  )+
  facet_wrap(~sexp) + 
  fill_palette("jco")


                              
 ggplot(train, aes(nbrtotc)) + theme_bw() +       #Frequency of total claims
   geom_bar(col = KULbg, fill = KULbg) +
   labs(y = "frequency") +
  ggtitle("number of claims")


 ggplot(train, aes(nbrtotc)) + theme_bw() +       #Frequency of total claims weighted with exposure
   geom_bar(aes(weight = duree), col = KULbg,
           fill = KULbg) +
   labs(y = "frequency (in exposure)") +
   ggtitle("number of weighted claims")
 

 ggplot(train, aes(duree)) + theme_bw() +       #Exposure  
   geom_histogram(col = KULbg, fill = KULbg) +
   labs(y = "Abs frequency") +
   ggtitle("Exposure")

 ggplot(train, aes(x=ageph, y=chargtot)) +        #Big amount of severiries are mainly for youngs 
   geom_point(alpha=.4, size=4, color=KULbg)

 ggplot(train, aes(coverp)) + theme_bw() +       #Frequency of coverp
   geom_bar(col = KULbg, fill = KULbg) +
   labs(y = "Coverage") +
   ggtitle("Type of coverage")
 
  ######################## GLM MODEL ############################################
########## Frequency###################
glm.model1=glm(nbrtotc~ offset(log(duree))+ageph+codposs+agecar+sexp+fuelc+split+
                 fleetc+usec+sportc+coverp+powerc+ageph:sexp,train, 
               family = poisson(link="log"))   #Try GLM family model
summary(glm.model1)

model1=stepAIC(glm.model1,k=2, direction="both", scope=list(upper=~., lower=~1))      
summary(model1)

########## Severity#################
data3=train%>% filter(nbrtotc>0)      #Keeping just positive number of claims (removing 0)
glm.model2=glm(chargtot~offset(log(duree))+ageph+codposs+agecar+
                 sexp+fuelc+split+fleetc+usec+sportc+coverp+
                 powerc+ageph:sexp, data3, family = Gamma (link="log"))  
summary(glm.model2)


model1=stepAIC(glm.model2,k=2, direction="both", scope=list(upper=~., lower=~1))      
summary(model1)



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
########### Runing Parallel Processing ########
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

##########  Cross Validation ##################
set.seed(123) 
train.control <- trainControl(method = "repeatedcv", savePredictions = "final",
                              number = 10, repeats = 3)
minitrain1=minitrain[,-c(3,4,6,7)]

########## Training Frequency Models#####################
model1 <- train(nbrtotc ~ . , data = minitrain1,
                    method = "rf", trControl = train.control, metric = "MAE")
model2 <- train(nbrtotc ~ . , data = minitrain1,
                method = "gbm", trControl = train.control, metric = "MAE")
model3 <- train(nbrtotc ~ . , data = minitrain1,
                method = "glm", trControl = train.control,metric = "MAE")


stopCluster(cl)    #Stoping the Parallel Processing 

###### Ploting performance of models###############
trellis.par.set(caretTheme())
plot(model1)
plot(model2)

##### Ploting Varible Importance##################
plot(varimportance <- varImp(model1, scale = FALSE))
plot(varimportance <- varImp(model2, scale = FALSE))
plot(varimportance <- varImp(model3, scale = FALSE))

##### MAE ########################################
print(min(model1$results$MAE))
print(min(model2$results$MAE))
print(min(model3$results$MAE))

#### Predictions #################################
predict1= predict(model1, minitest)
predict2= predict(model2, minitest)
predict3= predict(model3, minitest)


mean(abs(minitest$nbrtotc-predict1))
mean(abs(minitest$nbrtotc-predict2))
mean(abs(minitest$nbrtotc-predict3))


####Training Severity models#####################
minitrain2=minitrain[,-c(3,4,6,5)]

model11 <- train( chargtot~ . , data = minitrain2,
                method = "rf", trControl = train.control, metric = "MAE")
model22 <- train(chargtot ~ . , data = minitrain2,
                method = "gbm", trControl = train.control, metric = "MAE")
model33 <- train(chargtot ~ . , data = minitrain2,
                method = "glm", trControl = train.control,metric = "MAE")

stopCluster(cl)    #Stoping the Parallel Processing 
# registerDoSEQ()
# unregister <- function() {
#   env <- foreach:::.foreachGlobals
#   rm(list=ls(name=env), pos=env)
# }
###### Ploting performance of models###############
trellis.par.set(caretTheme())
plot(model11)
plot(model22)

##### Ploting Varible Importance##################
plot(varimportance <- varImp(model11, scale = FALSE))
plot(varimportance <- varImp(model22, scale = FALSE))
plot(varimportance <- varImp(model33, scale = FALSE))

##### MAE ########################################
print(min(model11$results$MAE))
print(min(model22$results$MAE))
print(min(model33$results$MAE))

#### Predictions #################################
predict11= predict(model11, minitest)
predict22= predict(model22, minitest)
predict33= predict(model33, minitest)


mean(abs(minitest$nbrtotc-predict11))
mean(abs(minitest$nbrtotc-predict22))
mean(abs(minitest$nbrtotc-predict33))
#### Collected premuim Vs Observed losses########
losses=sum(minitrain$chargtot)

predict111=predict(model11, minitrain)
predict222=predict(model22, minitrain)
predict333=predict(model33, minitrain)

sum(predict111)/losses
sum(predict222)/losses
sum(predict333)/losses     

###### rpart for regression tree #####################################
library(rpart)
tree_cp_1 <- rpart(nbrtotc ~ sexp + ageph + coverp + sportc+powerc+fuelc+agecar+split+fleetc+usec, data = train, method = "poisson", control = rpart.control(cp = 1, maxdepth = 5))
tree_cp_1
summary(tree_cp_1)
sum(train$nbrtotc)
sum(train$nbrtotc)/130925
tree_cp_0 <- rpart(nbrtotc ~ sexp + ageph + coverp + sportc+powerc+fuelc+agecar+split+fleetc+usec, data = train, method = "poisson", control = rpart.control(cp=0.0001, maxdepth = 10))
summary(tree_cp_0)
tree_cp_0$variable.importance
library(partykit)
tree_cp_0_party <- as.party(tree_cp_0)
plot(tree_cp_0_party)
printcp(tree_cp_0)
plotcp(tree_cp_0)
c_opt <- tree_cp_0$cptable[which.min(tree_cp_0$cptable[,"xerror"]),"CP"]
c_opt
tree_opt <- prune(tree_cp_0, cp = c_opt)
tree_opt <- as.party(tree_opt)
plot(tree_opt)

lambda_hat <- predict(tree_opt) 
train$lambda_hat <- lambda_hat
class <- partykit:::.list.rules.party(tree_opt)
train$class <- class[as.character(predict(tree_opt, type = "node"))]
head(train)
s <- subset(train, select = c(lambda_hat, class))

s <- unique(s)

s[order(s$lambda_hat), ]



freq_gam_spatial <- rpart(nbrtotc ~ long+ lat, data = train, method = "poisson", control = rpart.control(cp=0.0001, maxdepth = 10))

pred=predict(freq_gam_spatial, newdata = post_dt)
dt_pred=data.frame(pc=post_dt$POSTCODE,long=post_dt$long, lat=post_dt$lat, pred)
names(dt_pred)[4] = "fit_spatial"



belgium_shape_sf= left_join(belgium_shape_sf, dt_pred, by=c("POSTCODE"="pc"))
num_bins=5
library(classInt)
classint_fisher=classIntervals(dt_pred$fit_spatial,num_bins,style="fisher")
classint_fisher$brks
min(dt_pred$fit_spatial)
max(dt_pred$fit_spatial)
belg  ggtitle("claim frequency data") +
ium_shape_sf$class_fisher=cut(belgium_shape_sf$fit_spatial,breaks = classint_fisher$brks,
                                  right = FALSE, include.lowest = TRUE,dig.lab = 2)



mtpl_geo= train%>%dplyr::select(nbrtotc, duree, coverp, fuelc, ageph,codposs)
mtpl_geo=left_join(mtpl_geo, dt_pred,by=c("codposs"="pc"))
mtpl_geo$geo=as.factor(cut(mtpl_geo$fit_spatial,breaks = classint_fisher$brks, right = FALSE,
                           include.lowest = TRUE, dig.lab = 2))
head(mtpl_geo$geo)
geomodel= gam(nbrtotc~ geo+coverp+fuelc+s(ageph), data=mtpl_geo, family = poisson(link="log"))
summary(geomodel)

ggplot(belgium_shape_sf) +
  geom_sf(aes(fill = fit_spatial), colour = NA) +
  scale_fill_gradient(low = "#99CCFF",
                      high = "#003366") +
  theme_bw()
