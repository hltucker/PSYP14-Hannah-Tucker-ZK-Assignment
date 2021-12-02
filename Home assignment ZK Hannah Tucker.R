#Home assignment ZK - Hannah Tucker

##Packages
library(tidyverse)
library(psych)
library(dplyr)
library(plyr)
library(car)
library(lmtest)
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)
library(ggplot2)
library(lm.beta)

##Custom functions
###Coefficient table
coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

###Standardized beta coefficients in LMMs
stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	

##Part 1
###RQ1
study1data<-read.csv("https://tinyurl.com/ha-dataset1")

view(study1data)

##### Errors detected: ID_34 for STAI_trait and ID_88 for Pain (removed)

study1data_corrected = study1data[-c(34,88),]
view(study1data_corrected)
names(study1data_corrected)



####Model 1: Age and Sex as predictor variables

mod1<-lm(pain ~ age + sex, data=study1data_corrected)


####Check assumptions
#####Normality 
mod1 %>% plot(which=2)
describe(residuals(mod1))

#####Linearity 
mod1 %>% residualPlots()

#####Homoscedasticity 
mod1 %>% bptest()
mod1%>% ncvTest()


#####No multicollinearity 
mod1%>% vif() 

####Reporting Model 1
mod1
summary(mod1)
lm.beta(mod1)
coef_table(mod1)
AIC(mod1)

####Model 2: Age, Sex, STAI, Pain Catastrophizing, Mindfulness and two Cortisol measures as predictor variables 
mod2<-lm(pain ~ age + sex + STAI_trait + cortisol_serum + pain_cat + mindfulness,data=study1data_corrected)


####Check assumptions
#####Normality
mod2 %>% plot(which=2)
describe(residuals(mod2)) 

#####Linearity
mod2 %>% residualPlots()

#####Homoscedasticity
mod2 %>% bptest()
mod2 %>% ncvTest()


#####No multicollinearity
mod2 %>% vif()

####Reporting Model 2
mod2
summary(mod2)
lm.beta(mod2)
coef_table(mod2)
AIC(mod2)

#### Model comparison

##### R^2	
summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared

##### Anova	
anova(mod1, mod2)	

##### AIC	
AIC(mod1)	
AIC(mod2)	


### reporting RQ1
coef_table(mod1)
coef_table(mod2)


##Part 2
###RQ2

#### Baseline model

mod3 <- lm(pain ~ age + sex + STAI_trait + cortisol_serum + pain_cat + mindfulness + weight + IQ +household_income,data=study1data_corrected)
mod3

####Check assumptions

#####Normality
mod3 %>% plot(which=2)
describe(residuals(mod3))

#####Linearity
mod3 %>% residualPlots()

#####Homoscedasticity
mod3 %>% bptest()
mod3 %>% ncvTest()

#####No multicollinearity
mod3 %>% vif()

#### Backwards regression model
mod3back <-step(mod3, direction = "backward")


summary(mod3back)
lm.beta(mod3back)
coef_table(mod3back)
AIC(mod3back)


#### Model 4: results selected model

mod4<-lm(pain ~ age + mindfulness + cortisol_serum + pain_cat, data=study1data_corrected)

####Check assumptions

#####Normality
mod4 %>% plot(which=2)
describe(residuals(mod4))

#####Linearity
mod4 %>% residualPlots()

#####Homoscedasticity
mod4 %>% bptest()
mod4 %>% ncvTest()

#####No multicollinearity
mod4 %>% vif()

####Reporting Model 4
summary(mod4)
lm.beta(mod4)
coef_table(mod4)
AIC(mod4)

#### Model comparison; theory-based and backwards model

AIC(mod2)
AIC(mod4)

anova(mod4, mod2b)

#### Test-set

study2data<-read.csv("https://tinyurl.com/87v6emky")
view(study2data)

####Testing performance on test-set

#####Prediction values
prediction_mod2<-predict(mod2, study2data)
prediction_mod4<-predict(mod4, study2data)

##### Sum of squared residuals

RSS_mod2<-sum((study2data[,"pain"] - prediction_mod2)^2)
RSS_mod4<-sum((study2data[,"pain"] - prediction_mod4)^2)

##### Error comparison
RSS_mod2
RSS_mod4


###Part 3

#### New data sets

study3data <-read.csv("https://tinyurl.com/b385chpu")
study4data<-read.csv("https://tinyurl.com/4f8thztv")

#####Check data for errors
view(study3data)
view(study4data)

####
study3data %>% mutate(hospital=factor(hospital))

#### Linear mixed regression model on study data 3, with hospital as a random effect

mod5<- lmer(pain ~ age + sex + STAI_trait + cortisol_serum + pain_cat + mindfulness + (1|hospital),data=study3data)
mod5

sum(residuals(mod5)^2)
cAIC(mod5)$caic
r2beta(mod5, method="nsj", data=study3data)
r.squaredGLMM(mod5)
confint(mod5)

stdCoef.merMod(mod5)
summary(mod5)


####Testing performance on test-set

#####Prediction values
prediction_mod5<-predict(mod5, study4data, allow.new.levels = TRUE)

##### Sum of squared residuals
RSS_mod5<-sum((study4data[,"pain"] - prediction_mod5)^2)
mod5_mean <- lm(pain ~ 1, data = study4data)
TSS_mod5=sum((study4data$pain - predict(mod5_mean))^2)

R_mod5<-1-(RSS_mod5/TSS_mod5)
R_mod5

####Building new LMM

mod6 <- lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data=study3data)


####Visualizing Model 6

study3datapred = study3data %>% mutate(predmod5=predict(mod5), pred_slope=predict(mod6))

study3datapred = study3data %>% 
  mutate(pred_int = predict(mod5),
         pred_slope = predict(mod6))

ordered <- c("hospital_1", "hospital_2", "hospital_3", "hospital_4", "hospital_5", "hospital_6", "hospital_7", "hospital_8", "hospital_9", "hospital_10")
orderedstudy3datapred <- arrange(mutate(study3datapred, hospital=factor(hospital,levels=ordered)),hospital)

orderedstudy3datapred %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 1) +		
  geom_line(color='black', aes(y=pred_slope, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)

####Comparing Model 5 and 6
#####cAIC
cAIC(mod5)$caic
cAIC(mod6)$caic

##### Marginal R^2
r2beta(mod5, method = "nsj", data = study3data)
r2beta(mod6, method = "nsj", data = study3data)



