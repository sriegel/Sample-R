#REVIEW------------------
bull <- BullRiders
head(bull)

#scatterplot
plot(bull$YearsPro,bull$BuckOuts14) #xlab,ylab,main
abline(lm(bull$BuckOuts14~bull$YearsPro))
plot(bull$Events14~bull$BuckOuts14)
abline(lm(bull$BuckOuts14~bull$Events14))#terrible, most likely due to 0s from no events

#correlation
cor(bull$YearsPro,bull$BuckOuts14)#weak, .19
cor(bull$Events14,bull$BuckOuts14)#strong, .99
myvars <- c('YearsPro','Events14','BuckOuts14')
cor(bull[,myvars]) #correlation matrix

#correlation testing
res <- TempskiResilience
clerk <- res[res$Group=='Clerkship',]
names(clerk)
vars <- c('BDI','Resilience','State.Anxiety','Trait.anxiety')
cor(clerk[,vars])
library(psych)
corr.test(clerk[,vars])#from psych library for multiple vars
corr.test(clerk[,vars])$r #can also run p for $p values

#simple regression
names(clerk)
linFit(x=clerk$Resilience,y=clerk$BDI)
bdi_mod <- lm(BDI~Resilience,data=clerk) #lots more info than linFit
summary(bdi_mod)
confint(bdi_mod) #confidence intervals
lmBeta(bdi_mod) #standardized Beta for Resilience -How strongly it influences the predictor variable

#Regression Diagnostics Plot
summary(bdi_mod)
plot(bdi_mod,which=1) #should be random, homoskedastic,nums are row #'s
cutoff <- 4/(bdi_mod$df) #standard for Cooks
plot(bdi_mod,which=4,cook.levels=cutoff,id.n=5) #shows high outliers, great contenders for removal

#multiple regression
bdi_mult <- lm(BDI ~ Resilience + State.Anxiety + Trait.anxiety, data=clerk)
summary(bdi_mult)

plot(bdi_mult,which=1)
cutoff2 <- 4/bdi_mult$df
plot(bdi_mult,which=4,cook.levels = cutoff2,id.n=8)
confint(bdi_mult)
lmBeta(bdi_mult) #trait anxiety, furthest from 0, most influential
pCorr(bdi_mult) #Trait Anxiety accounts for 7.8% var uniquely

#Sample Code-----------------------------------------
#Subset into the Clinical Sciences
clin <- res[res$Group == "Clinical Sciences",]

Question One
#Intial Correlations
vars <- c("QoL", "BDI")
cor(clin[,vars])

#RQ1 Model
ov_mod <- lm(QoL ~ BDI, data=clin)
summary(ov_mod)
confint(ov_mod)

#Diagnostics
plot(ov_mod, which=1)
cutoff <- 4/(ov_mod$df) 
plot(ov_mod, which=4, cook.levels=cutoff)

Question Two
#Initial correlations
vars <- c("MS.QoL", "DREEM.S.SP", "DREEM.A.SP", "Resilience", "BDI", "Age")
cor(clin[,vars], use="pairwise.complete.obs")

#Test the initial correlations
library(psych)
corr.test(clin[,vars], use="pairwise.complete.obs")

#RQ2 Model
ms_mod <- lm(MS.QoL ~ DREEM.S.SP + DREEM.A.SP + Resilience + BDI + Age, data=clin)
summary(ms_mod)
confint(ms_mod)

#Diagnostics
library(car)
vif(ms_mod)
plot(ms_mod, which=1)
cutoff <- 4/(ms_mod$df) 
plot(ms_mod, which=4, cook.levels=cutoff)

#Put model into context
lmBeta(ms_mod) 
round(pCorr(ms_mod), 4) 


#Actual Lab--------------------------------------------
bas <- res[res$Group=='Basic Sciences',]
varsbas <- c('MS.QoL','WHOQOL.PH','WHOQOL.PSY','WHOQOL.SOC','WHOQOL.ENV')
cor(bas[,varsbas],use="pairwise.complete.obs")

bas_mod <- lm(MS.QoL ~ WHOQOL.PH + WHOQOL.PSY + WHOQOL.SOC + WHOQOL.ENV,data=bas)
summary(bas_mod)

lmBeta(bas_mod) 
round(pCorr(bas_mod), 4) 

Further
#Q1
q1model <- lm(BDI ~ Female + Age + State.Anxiety + Trait.anxiety, data=clin)
summary(q1model)
lmBeta(q1model)
round(pCorr(q1model), 4) 