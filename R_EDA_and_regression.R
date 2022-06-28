# Delete all old variables.
rm(list=ls())
# libraries needed to use
Packages <- c("dplyr", "ggplot2", "tidyverse", "corrplot", "readxl", "RColorBrewer", "psych", "mlbench", "caret", "lmtest")
lapply(Packages, library, character.only = TRUE)

# Set the directory which has the data
setwd("D:/2nd Year/2nd Semester/ThietKePhanTichThucNghiem/Do An/")
# Load data into 'data' variable
data<- read_excel("Concrete_Data.xls")


# Take a look at the columns' names
names(data)
# names are too long -> shorten them
names(data)<- c("Cement", "Blast", "FlyAsh", "Water", "SupPlas", "CoarseAgg","FineAgg","age", 'strength')

#### Descriptive Statistics
# library(psych) # psych library for using 'describe' function
describe(data)
detach(package:psych) # no longer need this library

#Box plot of Cement
ggplot(data,aes(x=Cement))+geom_boxplot()

#Histogram of cement 
ggplot(data, aes(x=Cement)) +
  geom_histogram(position = "identity", binwidth = 10, color="#01937C", fill="#B6C867") +
  geom_freqpoly(color='red', size = 1)

# Hexbin chart cement vs strength
my_colors=colorRampPalette(rev(brewer.pal(11,'Spectral')))
ggplot(data = data,aes(x=Cement, y=strength))+
  stat_bin2d(bins=35)+ 
  scale_fill_gradientn(colours=my_colors(32), trans="log")


#Box plot of Blast Furnace Slag 
ggplot(data,aes(x=Blast))+geom_boxplot()

#Histogram of Blast Furnace Slag  
ggplot(data, aes(x=Blast)) +
  geom_histogram(position = "identity", binwidth = 10, color="#01937C", fill="#B6C867") +
  geom_freqpoly(color='red', size = 2)



## Caret - Feature selection
# Data has more features than expected, so they need to be reduced.
set.seed(7)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(data[,1:8], 
               as.matrix(data[,9]), 
               sizes=c(1:8), 
               rfeControl=control)
# summarize the results
print(results)
# plot the results
plot(results, type=c("g", "o"))
detach(package:mlbench)
detach(package:caret)

# PHAN TICH ANOVA
# First ANOVA
av1<-aov(strength~age* Cement* Water* FineAgg* Blast, data=data)
summary(av1)

# Second ANOVA
av2<-aov(strength~age+ Cement+ Water+ FineAgg+ Blast
         +age:Cement+ age:Water+ age:FineAgg+ Water:FineAgg+ 
           Cement:Blast+ FineAgg:Blast+age:Cement:FineAgg+ 
           Cement:Water:FineAgg+ age:Water:Blast+ 
           Cement:Water:Blast+ Water:FineAgg:Blast+
           age:Cement:Water:Blast+ 
           Cement:Water:FineAgg:Blast+ age:Water:FineAgg:Blast
         ,data=data)
summary(av2)

# Third ANOVA
av3<-aov(strength~age+ Cement+ Water+ FineAgg+ Blast
         +age:Cement+ age:Water+ age:FineAgg+Water:FineAgg+ 
           Cement:Blast+ FineAgg:Blast+
           age:Cement:FineAgg+ age:Water:Blast
         +Cement:Water:FineAgg:Blast+ age:Water:FineAgg:Blast
         ,data=data)
summary(av3)

# Fourth ANOVA
av4<-aov(strength~age+ Cement+ Water+ FineAgg+ Blast
         + age:Cement+ age:Water+ age:FineAgg+ Water:FineAgg+ Cement:Blast+ FineAgg:Blast
         +age:Cement:FineAgg
         + Cement:Water:FineAgg:Blast+ age:Water:FineAgg:Blast
         ,data=data)
summary(av4)

# Fifth ANOVA
av5<-aov(strength~age+ Cement+ Water+ FineAgg+ Blast
         + age:Cement+ age:Water+ age:FineAgg+ Water:FineAgg+ 
           Cement:Blast+ FineAgg:Blast+age:Cement:FineAgg
         + Cement:Water:FineAgg:Blast
         ,data=data)
summary(av5)
par(mfrow=c(2,2))
plot(av5)

# Check heteroscedasticity 
#lmtest::bptest(av) 
lmtest::bptest(av, 
               ~ age*Cement + age*Water + age*FineAgg + Water*FineAgg + Cement*Blast + FineAgg*Blast +
                 age*Cement*FineAgg + Cement*Water*FineAgg + age*Water*Blast + Cement*Water*Blast + Water*FineAgg*Blast +
                 age*Cement*Water*Blast + age*Water*FineAgg*Blast + Cement*Water*FineAgg*Blast + 
                 I(age^2) + I(Cement^2) + I(Water^2)+ I(FineAgg^2)+ I(Blast^2), 
               data = data)
# Box-Cox transformation
data_new <- subset(data, select = c(age, Cement, Water, FineAgg, Blast, strength) )
distBCMod <- caret::BoxCoxTrans(data_new$strength)
print(distBCMod)
data_new <- cbind(data_new, strength_new=predict(distBCMod, data_new$strength)) # append the transformed variable to cars
av_new<-aov(strength~age*Cement + age*Water + age*FineAgg + Water*FineAgg + Cement*Blast + FineAgg*Blast +
              age*Cement*FineAgg + Cement*Water*FineAgg + age*Water*Blast + Cement*Water*Blast + Water*FineAgg*Blast +
              age*Cement*Water*Blast + age*Water*FineAgg*Blast + Cement*Water*FineAgg*Blast +age+ Cement+ Water+ FineAgg+ Blast, data=data)
bptest(av_new)
plot(av_new)
lmtest::bptest(av_new, 
               ~ age*Cement + age*Water + age*FineAgg + Water*FineAgg + Cement*Blast + FineAgg*Blast +
                 age*Cement*FineAgg + Cement*Water*FineAgg + age*Water*Blast + Cement*Water*Blast + Water*FineAgg*Blast +
                 age*Cement*Water*Blast + age*Water*FineAgg*Blast + Cement*Water*FineAgg*Blast + 
                 I(age^2) + I(Cement^2) + I(Water^2)+ I(FineAgg^2)+ I(Blast^2), 
               data = data_new)

#Basic regression
relation_basic <- lm(formula = strength ~ age + Cement + Water + FineAgg + Blast, data=Train)
summary(relation_basic)
bptest(relation_basic,  ~ age + Cement + Water + FineAgg + Blast,
       data=Train.f)
plot(fitted(relation_basic), resid(relation_basic), xlab='Fitted Values', ylab='Residuals')
abline(0,0)

# Linear Regression
Train <- read.csv("Train.csv")
relation <- lm(formula = strength~age + Cement + Water + FineAgg + Blast + 
                 age:Cement + age:Water + age:FineAgg + Water:FineAgg + Cement:Blast + 
                 FineAgg:Blast + age:Cement:FineAgg + Cement:Water:FineAgg:Blast, data=Train)
summary(relation)

pureErrorAnova(relation)
anova(relation)
bptest(relation, data=Train) # the result shows Heteroscedasticity of the dataset

# Weighted Least Squares Regression
#define weights to use
wt <- 1 / lm(abs(relation$residuals) ~ relation$fitted.values)$fitted.values^2
#perform weighted least squares regression
wls_model <- lm(strength~strength~age + Cement + Water + FineAgg + Blast + 
                  age:Cement + age:Water + age:FineAgg + Water:FineAgg + 
                  Cement:Blast + FineAgg:Blast + age:Cement:FineAgg + Cement:Water:FineAgg:Blast, 
                data = Train, weights=wt)
#view summary of model
summary(wls_model)
anova(wls_model)



#White test on Test
Test<- read.csv("Test.csv")
bptest(wls_model, ~ age + Cement + Water + FineAgg + Blast + 
         age:Cement + age:Water + age:FineAgg + Water:FineAgg + Cement:Blast + 
         FineAgg:Blast + age:Cement:FineAgg + Cement:Water:FineAgg:Blast, data = Test)

# Predict 
Test<- read.csv("Test.csv")
pred_cof = c()
interact = c("age", "Cement","Water", "FineAgg", "Blast", "age:Cement", "age:Water",
             "age:FineAgg", "Water:FineAgg", "age:Water:Blast", 
             "age:Cement:Water:FineAgg","age:Cement:FineAgg:Blast")
for(i in 1:12){
  pred_cof <- c(pred_cof, coef(summary(wls_model))[interact[i], "Estimate"])
}
predicted_values = c()

for(i in 1:nrow(Test)) {       # for-loop over rows
  Cement <-Test[i, 2:10][["Cement"]]
  Blast <-Test[i, 2:10][["Blast"]]
  FlyAsh <-Test[i, 2:10][["FlyAsh"]]
  Water <-Test[i, 2:10][["Water"]]
  SupPlas <-Test[i, 2:10][["SupPlas"]]
  CoarseAgg <-Test[i, 2:10][["CoarseAgg"]]
  FineAgg  <-Test[i, 2:10][["FineAgg"]]
  age <-Test[i, 2:10][["age"]]
  pred =0
  pred = pred_cof[1] * age + pred_cof[2]*Cement +
    pred_cof[3]*Water + pred_cof[4]*FineAgg + pred_cof[5]*Blast + 
    pred_cof[6]*age*Cement + pred_cof[7]*age*Water + 
    pred_cof[8]*age*FineAgg + pred_cof[9]*Water*FineAgg + 
    pred_cof[10]*age*Water*Blast+ pred_cof[11]*age*Cement*Water*FineAgg + 
    pred_cof[12]*age*Cement*FineAgg*Blast
  predicted_values <- c(predicted_values, pred)
}
predicted_values <- predict(wls_model, Test)
ggplot(data = data.frame(predicted_values, Test$strength),
       aes(x=Test.strength, y=predicted_values)) + 
  geom_line() + geom_point() + geom_smooth()
plot(predicted_values, Test$strength)
ggplot(data = data.frame(predicted_values, Test$strength),aes(x=predicted_values, y=Test.strength))+
  stat_bin2d(bins=35)+ 
  scale_fill_gradientn(colours=my_colors(32), trans="log")

###### Outliers detection
install.packages("climtrends", repos="http://R-Forge.R-project.org")
library(climtrends)
FindOutliersTietjenMooreTest(data$Cement,2,alpha=0.05)

# Cook distance
library(olsrr)
ols_plot_cooksd_bar(wls_model)
ols_plot_cooksd_chart(wls_model)
cd <- cooks.distance(wls_model)
plot(cooks.distance(wls_model), pch="*", cex=2, main="Influential Obs by Cooks distance")  
abline(h = 4/1020, col="red")  # add cutoff line
text(x=1:length(cd), y=cd, labels=ifelse(cd>4/1030,names(cd),""), col="red", pos = 4)

# mean of residuals 
mean(wls_model$residuals)

### Outliers elimination
# Plot outliers
plot(relation, 5)
# Loc outliers su dung Leverage
Train.f <- subset(Train, hatvalues(relation) <= 0.05)
nrow(Train.f)
#Build model Regression
relation_m <- lm(strength~age + Cement + Water + FineAgg + Blast + 
                   age:Cement + age:Water + age:FineAgg + Water:FineAgg + Cement:Blast + 
                   FineAgg:Blast + age:Cement:FineAgg + Cement:Water:FineAgg:Blast, data = Train.f)
summary(relation_m)

bptest(relation_m, ~ age + Cement + Water + FineAgg + Blast + 
         age:Cement + age:Water + age:FineAgg + Water:FineAgg + Cement:Blast + 
         FineAgg:Blast + age:Cement:FineAgg + Cement:Water:FineAgg:Blast, data = Test)
#define weights to use
wt_no_outlier <- 1 / lm(abs(relation_m$residuals) ~ relation_m$fitted.values)$fitted.values^2
#perform weighted least squares regression
wls_model_no_outlier <- lm(strength~age + Cement + Water + FineAgg + Blast + 
                  age:Cement + age:Water + age:FineAgg + Water:FineAgg + Cement:Blast + 
                  FineAgg:Blast + age:Cement:FineAgg + Cement:Water:FineAgg:Blast, 
                data = Train.f, weights=wt_no_outlier)
summary(wls_model_no_outlier)
bptest(wls_model_no_outlier, ~ age + Cement + Water + FineAgg + Blast + 
         age:Cement + age:Water + age:FineAgg + Water:FineAgg + Cement:Blast + 
         FineAgg:Blast + age:Cement:FineAgg + Cement:Water:FineAgg:Blast, data = Test)
# predict
predicted_values <- predict(wls_model_no_outlier, Test)
ggplot(data = data.frame(predicted_values, Test$strength),
       aes(x=Test.strength, y=predicted_values)) + 
  geom_line() + geom_point() + geom_smooth()

#create residual vs. fitted plot for 4 model
library(ggpubr)
par(mfrow=c(2,2))
relation_basic_plot <- ggplot(data = data.frame(fitted(relation_basic), resid(relation_basic)),
       aes(x=fitted.relation_basic., y=resid.relation_basic.)) + geom_point() +
  labs(title="Basic model", x="Fitted Values", y="Residuals") +
  geom_hline(aes(yintercept=0))
relation_plot<-ggplot(data = data.frame(fitted(relation), resid(relation)),
       aes(x=fitted.relation., y=resid.relation.)) + geom_point() +
  labs(title="Model with interaction", x="Fitted Values", y="Residuals") +
  geom_hline(aes(yintercept=0))
wls_plot<-ggplot(data = data.frame(fitted(wls_model), resid(wls_model)),
       aes(x=fitted.wls_model., y=resid.wls_model.)) + geom_point() +
  labs(title="Weighted LS Model", x="Fitted Values", y="Residuals") +
  geom_hline(aes(yintercept=0))
wls_noOutlier<-ggplot(data = data.frame(fitted(wls_model_no_outlier), resid(wls_model_no_outlier)),
       aes(x=fitted.wls_model_no_outlier., y=resid.wls_model_no_outlier.)) + geom_point() +
  labs(title="Weighted LS Model + no outlier", x="Fitted Values", y="Residuals") +
  geom_hline(aes(yintercept=0))
ggarrange(relation_basic_plot, 
          relation_plot, 
          wls_plot,
          wls_noOutlier,
          ncol = 2, nrow = 2)

qqnorm(resid(model))
qqline(resid(model))
qqnorm(resid(wls_model))
qqline(resid(wls_model))

# predict with 4 models
# train set
pred_train_vals_basic <- predict(relation_basic, Train)
pred_train_vals_interact <- predict(relation, Train)
pred_train_vals_wls <- predict(wls_model, Train)
pred_train_vals_wlsNoOutlier <- predict(wls_model_no_outlier, Train)
basic_pre_train <- ggplot(data = data.frame(pred_train_vals_basic, Train$strength),
       aes(x=Train.strength, y=pred_train_vals_basic)) + 
  geom_line() + geom_point() + geom_smooth() +
  labs(title="Basic model prediction on Train set", 
       x = "Strength", y="Predicted Strength")
int_pre_train <- ggplot(data = data.frame(pred_train_vals_interact, Train$strength),
                          aes(x=Train.strength, y=pred_train_vals_interact)) + 
  geom_line() + geom_point() + geom_smooth()+
  labs(title="Interaction model prediction on Train set", 
       x = "Strength", y="Predicted Strength")
wls_pre_train <- ggplot(data = data.frame(pred_train_vals_wls, Train$strength),
                          aes(x=Train.strength, y=pred_train_vals_wls)) + 
  geom_line() + geom_point() + geom_smooth()+
  labs(title="WLS model prediction on Train set", 
       x = "Strength", y="Predicted Strength")
wlsNoOutlier_pre_train <- ggplot(data = data.frame(pred_train_vals_wlsNoOutlier, Train$strength),
                          aes(x=Train.strength, y=pred_train_vals_wlsNoOutlier)) + 
  geom_line() + geom_point() + geom_smooth()+
  labs(title="WLS model prediction on Train set (no outlier)", 
       x = "Strength", y="Predicted Strength")
ggarrange(basic_pre_train,
          int_pre_train,
          wls_pre_train,
          wlsNoOutlier_pre_train,
          ncol = 2, nrow = 2)
library(forecast)
accuracy(pred_train_vals_basic, Train$strength)
accuracy(pred_train_vals_interact, Train$strength)
accuracy(pred_train_vals_wls, Train$strength)
accuracy(pred_train_vals_wlsNoOutlier, Train$strength)

# test set
pred_test_vals_basic <- predict(relation_basic, Test)
pred_test_vals_interact <- predict(relation, Test)
pred_test_vals_wls <- predict(wls_model, Test)
pred_test_vals_wlsNoOutlier <- predict(wls_model_no_outlier, Test)
basic_pre_test <- ggplot(data = data.frame(pred_test_vals_basic, Test$strength),
                          aes(x=Test.strength, y=pred_test_vals_basic)) + 
  geom_line() + geom_point() + geom_smooth() +
  labs(title="Basic model prediction on Test set", 
       x = "Strength", y="Predicted Strength")
int_pre_test <- ggplot(data = data.frame(pred_test_vals_interact, Test$strength),
                        aes(x=Test.strength, y=pred_test_vals_interact)) + 
  geom_line() + geom_point() + geom_smooth()+
  labs(title="Interaction model prediction on Test set", 
       x = "Strength", y="Predicted Strength")
wls_pre_test <- ggplot(data = data.frame(pred_test_vals_wls, Test$strength),
                        aes(x=Test.strength, y=pred_test_vals_wls)) + 
  geom_line() + geom_point() + geom_smooth()+
  labs(title="WLS model prediction on Test set", 
       x = "Strength", y="Predicted Strength")
wlsNoOutlier_pre_test <- ggplot(data = data.frame(pred_test_vals_wlsNoOutlier, Test$strength),
                                 aes(x=Test.strength, y=pred_test_vals_wlsNoOutlier)) + 
  geom_line() + geom_point() + geom_smooth()+
  labs(title="WLS model prediction on Test set (no outlier)", 
       x = "Strength", y="Predicted Strength")
ggarrange(basic_pre_test,
          int_pre_test,
          wls_pre_test,
          wlsNoOutlier_pre_test,
          ncol = 2, nrow = 2)
accuracy(pred_test_vals_basic, Test$strength)
accuracy(pred_test_vals_interact, Test$strength)
accuracy(pred_test_vals_wls, Test$strength)
accuracy(pred_test_vals_wlsNoOutlier, Test$strength)


