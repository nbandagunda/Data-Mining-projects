###############Homework-4#################
#Naveen Kumar Reddy Bandagunda
#M14528578
#Taiwan Credit card default case study Part-A


 ######################Part-A######################
 #####################100% data EDA###############
source("http://www.sthda.com/upload/rquery_cormat.r")
credit<-read.csv("credit_default.csv")
dim(credit)
attach(credit)
head(credit)
n<-dim(credit)[1]    #no of observations
p<-dim(credit)[2]-1  #no of predictors

#STEP1: #Renaming the response variable name to simply "default"
library(dplyr)
credit<-rename(credit, default=default.payment.next.month)
colnames(credit)

#STEP2: saving the histograms of all int variables
for (i in 5:24){
  
  mypath<-file.path("/Users/bnkr/Desktop/Main/Data Mining/Data Mining/Week 4",
                    paste("myplot_",names(credit)[i],".jpeg",sep=""))
  jpeg(file = mypath )
  hist(credit[,i],
          main=colnames(credit)[i])
  dev.off()
}

#STEP3: #saving boxplots all at once
names<-colnames(credit)[5:24]
for (i in 5:24){
  
  mypath<-file.path("/Users/bnkr/Desktop/Main/Data Mining/Data Mining/Week 4",
                    paste("myplot_",names[i],".jpeg",sep=""))
  jpeg(file = mypath )
  boxplot(credit[,i],
          main=colnames(credit)[i])
  dev.off()
}
str(credit)
cor(credit[5:24])
rquery.cormat(credit)[5:24]
rquery.cormat(train_cr)[5:24]
#STEP4: Proportion of the default in the response:
sum(default)/n

#STEP5:comparing the response variable to limit_balance and pay_0 variables
table(default)
ptab<-prop.table(table(default))

#pie chart
pie(ptab, main="proportions of default")

#density plot with lim_bal and pay_0

densityplot(~ LIMIT_BAL, groups = default, data = credit)
densityplot(~ PAY_0, groups = default, data = credit)

#ggplot for limit bal to default and pay_0 to default
ggplot(credit, aes(x = LIMIT_BAL, color = as.factor(default))) + 
  geom_density() +
  theme_bw()
ggplot(credit, aes(x = PAY_0, color = as.factor(default))) + 
  geom_density() +
  theme_bw()

#scatter plot for limit_bal~default
plot(jitter(default,0.1) ~ jitter(LIMIT_BAL), data = credit, las = 1,
     xlab = "Limit_bal", ylab = "Default",
     col = adjustcolor(default + 1, alpha.f = 0.3))

###########################PART-B#############################
##########################80% DATA EDA########################

#STEP1: sampling the data into 80 and 20 % datasets

set.seed(14528578)
x<-sample(nrow(credit), nrow(credit)*0.80)
train_cr<-credit[x,]
test_cr<-credit[-x,]

head(train_cr)
attach(train_cr)
train_cr$SEX<-as.factor(SEX)
train_cr$EDUCATION<-as.factor(EDUCATION)
train_cr$MARRIAGE<-as.factor(MARRIAGE)
str(train_cr)

#STEP2: Analyze default with Education and Pay_0 variables:

#For a default of 0 and 1 how many observations of levels exist?
summary1<-data.frame(table(train_cr$PAY_0,train_cr$default)) #creating a dataframe to calculate the freqs and %s
summary1<-rename(summary1,Pay_0_levels=Var1, Default=Var2)   #renaming to generic names
colnames(summary1)

library(tidyverse)                                           #Creating new columns with percentages for ggplot
summary1 %>% 
  mutate(pctg=round(Freq/24000,4), lbl=scales::percent(pctg))->summary1
summary1
#plotting Pay_0 to default
ggplot(train_cr, 
       aes(x=factor(PAY_0),
           
           fill=factor(default)))+
  labs(y="Number of customer with default and not default",
             x="Past month's data",
             fill="Default")+
  geom_bar(position = "dodge")

#Plotting the education to the default
ggplot(train_cr, 
       aes(x=factor(EDUCATION),
           
           fill=factor(default)))+
  labs(y="Number of customer with default and not default",
       x="Education",
       fill="Default")+
  geom_bar(position = "dodge")

##########################PART-3############################
#############LOGISTIC REGRESSION MODEL DEVELOPMENT##########

#model development

#STEP1: FULL MODEL
model_full <- glm(default~., family = binomial, data=train_cr)
summary(model_full)

#AIC< BIC and In-sample mean residual deviance

AIC1<-AIC(model_full)
BIC1<-BIC(model_full) 
deviance1 <- model_full$deviance        
mean_residual_deviance<-deviance1/model_full$df.residual

d1<-data.frame(AIC1, BIC1, mean_residual_deviance)
d1
##STEP 2:NULL MODEL
model_null<-glm(default~1 , family = binomial, data=train_cr)
summary(model_null)       

AIC2<-AIC(model_null)
BIC2<-BIC(model_null) 
deviance2 <- model_null$deviance        
mean_residual_deviance2<-deviance2/model_null$df.residual

d2<-data.frame(AIC2, BIC2, mean_residual_deviance2)
d<-cbind(d1,d2)
d2
#STEP3: model with two variables Pay_0 and Education

model_2var<-glm(default~EDUCATION+PAY_0, family=binomial, data =train_cr)
summary(model_2var)

AIC3<-AIC(model_2var)
BIC3<-BIC(model_2var)
deviance3 <- model_2var$deviance        
mean_residual_deviance3<-deviance3/model_2var$df.residual
d3<-data.frame(AIC3, BIC3, mean_residual_deviance3)
d3
d<-cbind(d1,d2,d3)

####STEP4: Step-wise variable selection using AIC

AIC_model1<-step(model_full, data=train_cr,k=2)
#using the AIC selected variables, building a new model


model3_AIC<-glm(default~PAY_AMT6+PAY_AMT4+PAY_6+PAY_AMT5+
                  BILL_AMT3+SEX+PAY_2+AGE+LIMIT_BAL+EDUCATION+PAY_3+MARRIAGE+BILL_AMT1+PAY_AMT2+PAY_AMT1+PAY_0, 
                family = binomial, 
                data=train_cr)
AIC4<-AIC(model3_AIC)
BIC4<-BIC(model3_AIC)
deviance4 <- model3_AIC$deviance
mean_residual_deviance4<-deviance4/model3_AIC$df.residual
d4<-data.frame(AIC4, BIC4, mean_residual_deviance4)


####STEP5: Step-wise variable selection using BIC
BIC_model1<-step(model_full, data=train_cr,k=log(dim(train_cr)[1]))

#using the BIC selected variables, building a new model:

model4_BIC<-glm(default~ BILL_AMT3+SEX+PAY_2+AGE+LIMIT_BAL+EDUCATION+PAY_3+MARRIAGE+BILL_AMT1+PAY_AMT2+PAY_AMT1+PAY_0, 
                family = binomial, 
                data=train_cr)
AIC5<-AIC(model4_BIC)
BIC5<-BIC(model4_BIC)
deviance5 <- model4_BIC$deviance
mean_residual_deviance5<-deviance5/model4_BIC$df.residual
d5<-data.frame(AIC5, BIC5, mean_residual_deviance5)
d5
colnames(train_cr)

#STEP5: using LASSO variable selection:

library(glmnet)
modA<-glmnet(as.matrix(train_cr[,-c(which(colnames(train_cr)=="default"))]),y=train_cr$default, family=binomial, alpha=1)
summary(modA)
coef(modA,s=0.05)

library(plotmo) 

plot_glmnet(modA,label=TRUE)
##using cv.glmnet to identify best s value
cv_model<-cv.glmnet(as.matrix(train_cr[,-c(which(colnames(train_cr)=="default"))]),
                    y=train_cr$default,
                    family=binomial,
                    type.measure = "class",
                    alpha=1,
                    nfolds=10)
summary(cv_model)

a<-cv_model$lambda.min
b<-cv_model$lambda.1se

#calculating the coefficients now using s values

coef(modA,s=a)
coef(modA, s=b)

#building new model with the lasso selected variables with lambda minimum:

lasso_model1<-glm(default~. -BILL_AMT2 -BILL_AMT4, family = binomial, data=train_cr)
summary(lasso_model1)

AIC6<-AIC(lasso_model1)
BIC6<-BIC(lasso_model1)
deviance6 <- lasso_model1$deviance
mean_residual_deviance6<-deviance6/lasso_model1$df.residual
d6<-data.frame(AIC6, BIC6, mean_residual_deviance6)
d6

#building new model with the lasso selected variables with lambda 1se:

lasso_model2<-glm(default~LIMIT_BAL+MARRIAGE+ AGE+PAY_0+PAY_2+
                    PAY_3+PAY_5+PAY_6+BILL_AMT1+PAY_AMT1+PAY_AMT2+PAY_AMT5, family = binomial, data=train_cr)
summary(lasso_model2)

AIC7<-AIC(lasso_model2)
BIC7<-BIC(lasso_model2)
deviance7 <- lasso_model2$deviance
mean_residual_deviance7<-deviance7/lasso_model2$df.residual
d7<-data.frame(AIC7, BIC7, mean_residual_deviance7)
d7

##Prediction using the lambda min lasso selection model

predict1<-predict(modA,newx = as.matrix(train_cr[,-c(which(colnames(train_cr)=="default"))]), s= a,type = "response")
head(predict1)

table(predict1>0.5)       
table(train_cr$default)


library(caret)
install.packages("ISLR")
install.packages("InformationValue")
library(InformationValue)
library(ISLR)
optimal <- optimalCutoff(train_cr$default, predict1)[1]

confusionMatrix(train_cr$default,predict1)
sensitivity(train_cr$default,predict1)
specificity(train_cr$default,predict1)
misClassError(train_cr$default,predict1,threshold = optimal)



########################Assignment 5#########################

