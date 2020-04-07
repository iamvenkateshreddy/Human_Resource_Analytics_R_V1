getwd()
library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
library(cvTools)

#importing test and train data
hr_train = read.csv('hr_train.csv',stringsAsFactors = F)
hr_test = read.csv('hr_test.csv',stringsAsFactors = F)

#glimpse(hr_train)
#glimpse(hr_test)

#assigning the NA to left(dependent variable) in test
hr_test$left = NA

#hr_test$left = as.factor(hr_test$left)

#adding variable to distinguish test and train data
hr_train$data='train'
hr_test$data='test'

#combining the test and train data
hr_all = rbind(hr_train,hr_test)
#glimpse(hr_all)

#converting the dependent variable to factor since it is a classification problem
#hr_all$left = as.factor(hr_all$left)

###
#hr_all$Work_accident = as.factor(hr_all$Work_accident)
#hr_all$promotion_last_5years = as.factor(hr_all$promotion_last_5years)

glimpse(hr_all)


#unwanted feature
#hr_all$sales = NULL

View(hr_all)
glimpse(hr_all)

table(hr_all$sales)
table(hr_all$salary)
table(hr_all$satisfaction_level)
table(hr_all$last_evaluation)
table(hr_all$number_project)
table(hr_all$average_montly_hours)
table(hr_all$time_spend_company)
table(hr_all$sales)
table(hr_all$Work_accident)
table(hr_all$promotion_last_5years)

#Feature scaling
hr_all$average_montly_hours = scale(hr_all$average_montly_hours)

#table(hr_all$sales)  #not required 

#hr_all = hr_all %>%
#  mutate(salary_low = as.numeric(salary=='low'),
#         salary_medium = as.numeric(salary=='medium')) %>%
#  select(-salary)

hr_all$salary = factor(hr_all$salary,
                       levels = c('low', 'medium', 'high'),
                       labels = c(1, 2, 3))


#Creae Dummies function
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}


#hr_all=CreateDummies(hr_all ,"time_spend_company",200)
hr_all=CreateDummies(hr_all,"sales",100)
#hr_all$sales=NULL

glimpse(hr_all)

lapply(hr_all,function(x) sum(is.na(x)))

for(col in names(hr_all)){
  if(sum(is.na(hr_all[,col]))>0 & !(col %in% c("data","left"))){
    rg[is.na(rg[,col]),col]=mean(hr_all[hr_all$data=='train',col],na.rm=T)
  }
}


#mydata$employee_satisfaction[mydata$satisfaction_level >= 0.9] = '1.Maximum'
#mydata$employee_satisfaction[mydata$satisfaction_level >= 0.8 & mydata$satisfaction_level < 0.9 ] = '2.High'
#mydata$employee_satisfaction[mydata$satisfaction_level >= 0.6 & mydata$satisfaction_level < 0.8 ] = '3.Good'
#mydata$employee_satisfaction[mydata$satisfaction_level >= 0.4 & mydata$satisfaction_level < 0.6 ] = '4.Average'
#mydata$employee_satisfaction[mydata$satisfaction_level >= 0.2 & mydata$satisfaction_level < 0.4 ] = '5.Low'
#mydata$employee_satisfaction[mydata$satisfaction_level <  0.2] = '6.Minimum'

#hr_all$satisfaction_level[hr_all$satisfaction_level >= 0.9 ] = 6
#hr_all$satisfaction_level[hr_all$satisfaction_level >= 0.8 & hr_all$satisfaction_level < 0.9 ] =5
#hr_all$satisfaction_level[hr_all$satisfaction_level >= 0.6 & hr_all$satisfaction_level < 0.8 ] =4
#hr_all$satisfaction_level[hr_all$satisfaction_level >= 0.4 & hr_all$satisfaction_level < 0.6 ] =3
#hr_all$satisfaction_level[hr_all$satisfaction_level >= 0.2 & hr_all$satisfaction_level < 0.4 ] =2
#hr_all$satisfaction_level[hr_all$satisfaction_level < 0.2 ] = 1

#hr_all$satisfaction_level =as.factor(hr_all$satisfaction_level)




hr_train = hr_all %>% filter(data =='train') %>% select(-data)
hr_test = hr_all %>% filter(data =='test') %>% select(-data,-left)

glimpse(hr_train)
glimpse(hr_test)

#taking the sample form train data
set.seed(2)
s=sample(1:nrow(hr_train),0.75*nrow(hr_train))
hr_train1=hr_train[s,]
hr_train2=hr_train[-s,]

library(car)


for_vif=lm(left~.,data=hr_train1)  ##applying the linear model(lm)

sort(vif(for_vif),decreasing = T)[1:3]

log_fit = glm(left~.,
              data=hr_train1,family = "binomial")
##we use step function to find best input with less AIC value
log_fit=step(log_fit)

formula(log_fit)

log_fit = glm(left ~ satisfaction_level + last_evaluation + number_project + 
  average_montly_hours + time_spend_company + Work_accident + 
  salary + sales_hr + sales_accounting + sales_marketing + 
  sales_product_mng + sales_support + sales_technical + sales_sales,
  data=hr_train1,family = "binomial")

summary(log_fit)  

#### performance of score model on validation data
library(pROC)

val.score=predict(log_fit,newdata = hr_train2,type='response')

auc(roc(hr_train2$left,val.score))
