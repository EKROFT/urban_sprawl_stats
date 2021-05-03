lst.data<-read.csv("Data/compiled_data_0324.csv")
library(GGally)
library(tidyverse)
library(mgcv)

train.lst<-lst.data%>%filter(Testing=="Training")
test.lst<-lst.data%>%filter(Testing=="Test")

##Initial exploration
data.selected<-lst.data%>%select(LST_mean,BD, X.canopy,
                                 Income,river.distance..meters.,Imp., NDVImean)
GGally::ggpairs(data.selected)

##Testing linear models

#First attempt model
lst.model<-lme(LST_mean~BD+X.canopy+log(Income)+log(river.distance..meters.)+
                Imp2, data=lst.data, random=~1|Borough, method="ML", na.action=na.exclude)


summary(lst.model)
library(car)
vif(lst.model)

#canopy cover and impervious seem a little correlated, same with BD and impervious
#I'm testing a model that removes impervious cover for that reason
lst.model2<-lme(LST_mean~BD+X.canopy+log(Income)+
                 log(river.distance..meters.), data=lst.data,random=~1|Borough, method="ML", na.action=na.exclude)
summary(lst.model2)
vif(lst.model2)

#I want to test if the simpler model was better
anova(lst.model,lst.model2)
#looks like more complex model is better

#since the vif showed nothing above 5 for first model, I will keep impervious

#checking if I can remove any variables
lst.modelf<-lme(LST_mean~BD+X.canopy+log(Income)+log(river.distance..meters.)+Imp2, data=lst.data,
               random=~1|Borough, method="ML", na.action=na.exclude)

anova(lst.model, lst.modelf)
AIC(lst.model)
AIC(lst.modelf)
#can't remove any variables, tried each of them in turn

##checking model assumptions
plot(lst.model, which=2)
plot(lst.model, which=3)

##now testing models with interactions

lst.model5<-lme(LST_mean~BD+(X.canopy*Imp.)+Income+river.distance..meters.,
               data=lst.data, random=~1|Borough, na.action=na.exclude, method="ML")
summary(lst.model5)
#canopy:imp. interaction has p values close to 1

lst.model6<-lme(LST_mean~(BD*X.canopy)+Imp.+Income+river.distance..meters., data=lst.data,
               random=~1|Borough, na.action=na.exclude, method="ML")
summary(lst.model6)
#this BD-canopy cover interaction has a very high p value
anova(lst.model, lst.model6)
AIC(lst.model)
AIC(lst.model6)
#I shouldn't use the interaction term


library(nlme)
library(gstat)
library(sp)

lst.model4<-lm(LST_mean~BD+X.canopy+river.distance..meters.+Imp., data=lst.data)
coordinates(lst.data)<-c('Long','Lat')
resids<-residuals(lst.model4)
lst.data$resids=resids
bubble(lst.data,zcol='resids')
V<-variogram(resids~1, data=lst.data)
plot(V, pch=16, col="black")
#variogram doesn't show much spatial sutocorrelation, but still a little

test.dists <- as.matrix(dist(cbind(lst.data$Long, lst.data$Lat)))

test.dists.inv <- 1/test.dists
diag(test.dists.inv) <- 0

test.dists.inv[1:5, 1:5]

library(ape)

test.dists.inv[is.infinite(test.dists.inv)] <- 0

Moran.I(lst.data$resids, test.dists.inv)
#Moran's I seems to indicate that there is spatial autocorrelation in the model residuals

#Model assumptions of independent observations not being met, need to account for this
##Ended up accounting for this by having Borough as a random effect in the model.
coords<-coordinates(lst.data)

lst.model7<-lm(LST_mean~Lat*Long+BD+X.canopy+Income+
                 river.distance..meters.+Imp., data=lst.data)
summary(lst.model7)

anova(lst.model, lst.model7, test="Chisq")
AIC(lst.model)
AIC(lst.model7)
#model with spatial term is stronger

plot(lst.model7, which=2)
plot(lst.model7, which=3)


#trying things out with NDVI
lst.model8<-lm(LST_mean~Lat*Long+BD+X.canopy+Income+
                 river.distance..meters.+Imp.+NDVImean, data=lst.data)
summary(lst.model8)
lst.model9<-lm(LST_mean~Lat*Long+BD+X.canopy+Income+
                 river.distance..meters.+NDVImean, data=lst.data)
summary(lst.model9)
vif(lst.model8)
plot(lst.data$NDVImean~lst.data$Imp.)
vif(lst.model9)
AIC(lst.model7, lst.model9)
##lowest AIC is model 9 (replaced imp. with NDVI)

lst.model10<-lm(LST_mean~Lat*Long+BD+Income+
                  river.distance..meters.+NDVImean, data=lst.data)
summary(lst.model10)
AIC(lst.model9,lst.model10)
##taking away canopy cover hurt the model

lst.model11<-lm(LST_mean~Lat*Long+BD+X.canopy+
                  river.distance..meters.+NDVImean, data=lst.data)
summary(lst.model11)
AIC(lst.model9, lst.model11)
##taking out income hurt the model

##it seems that adding NDVI into the model doesn't really change anything, 
#the only variable it can replace is impervious cover and it doesn't change r2 or p

plot(X.canopy~BD, data=lst.data)
plot(X.canopy~Imp., data=lst.data)
plot(LST_mean~X.canopy, data=lst.data)
plot(Imp2~Imp., data=lst.data)


#install.packages("qpcR")
library(qpcR)

values<-AIC(lst.model, lst.model2)

akaike.weights(values)

0.377/0.622

##Trying LST for alternate days
alt_data<-read.csv("Data/Alt_LST.csv")
View(alt_data)
alt_may<-lm(May_C~BD, data=alt_data)
summary(alt_may)
plot(May_C~BD, data=alt_data, xlab="% Building Density", ylab="May Land Surface Temperature (C)",
     pch=16, xlim=c(0,100), ylim=c(10,45))
abline(alt_may)
alt_oct<-lm(Oct_C~BD, data=alt_data)
summary(alt_oct)
plot(Oct_C~BD, data=alt_data, xlab="% Building Density", ylab="October Land Surface Temperature (C)",
     pch=16, xlim=c(0,100), ylim=c(10,45))
abline(alt_oct)
july<-lm(LST_mean~BD, data=lst.data)
summary(july)
plot(LST_mean~BD, data=lst.data, xlab="% Building Density", ylab="July Land Surface Temperature (C)",
     pch=16, xlim=c(0,100), ylim=c(10,45))
abline(july)

#trying household relationship as a GAM
library(mgcv)
fil<-filter(lst.data, Households>0)
gam.lst<-gam(LST_mean~s(Households)+s(Income)+X.canopy+s(Imp.)+
               s(river.distance..meters.), data=fil, method="REML")
plot(gam.lst)
summary(gam.lst)

##Figuring out whether household is significant beyond BD (most of this I didn't use)
lm.lst<-lm(LST_mean~Households, data=fil)
summary(lm.lst)
lm.lst2<-lm(LST_mean~Households+BD+X.canopy+Income+river.distance..meters., data=lst.data)
summary(lm.lst2)
library(car)
vif(lm.lst2)
lm.lm<-lm(Households~BD, data=lst.data)
summary(lm.lm)


anova(lst.model, lm.lst2)
AIC(lst.model, lm.lst2)
##model I was using first seems to be better

Boroughs<-as.factor(lst.data$Borough)
gam.lsv<-gam(LST_mean~s(Households)+s(BD)+s(Boroughs, bs="re"), data=lst.data, method="ML")
summary(gam.lsv)
gam.check(gam.lsv)

plot(gam.lsv)
#better with random effect




#Borouugh as random effect
library(nlme)

model = lme(LST_mean~BD+X.canopy+log(river.distance..meters.)+Imp2, data=lst.data, random=~1|Borough,
            method="ML")

library(car)

Anova(model)

model.fixed = gls(LST_mean~BD+X.canopy+log(river.distance..meters.)+Imp2, data=lst.data, method="ML")
anova(model,model.fixed)
summary(model)
summary(model.fixed)

values3<-AIC(model, model.fixed)

akaike.weights(values3)

0.378/0.607
##model with random effect is better

#Test vs. training
library(gghighlight)
theme_set(theme_bw())

plot.train<-ggplot(lst.data, aes(x=BD, y=LST_mean, color=Testing)) +
  labs(x="% Building Density", y="LST (C)")+
  geom_point(size=3)+
  theme_classic()+
  scale_colour_manual(values=c("red", "black"))+
  geom_smooth(method=lm, color="black")
plot.train
