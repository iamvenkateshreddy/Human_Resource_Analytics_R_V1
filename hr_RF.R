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

hr_all$left = as.factor(hr_all$left)

#unwanted feature
#hr_all$sales = NULL

View(hr_all)
glimpse(hr_all)

table(hr_all$salary)
table(hr_all$satisfaction_level)
table(hr_all$last_evaluation)
table(hr_all$number_project)
table(hr_all$average_montly_hours)
table(hr_all$time_spend_company)
table(hr_all$Work_accident)
table(hr_all$promotion_last_5years)
table(hr_all$salary_high)
table(hr_all$salary_medium)
#table(hr_all$sales)  #not required 

#############
hr_all = hr_all %>%
  mutate(salary_low = as.numeric(salary=='low'),
         salary_medium = as.numeric(salary=='medium')) %>%
  select(-salary)
#########################

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

hr_all=CreateDummies(hr_all,"sales",100)
hr_all=CreateDummies(hr_all,"salary",100)

glimpse(hr_all)

hr_train = hr_all %>% filter(data =='train') %>% select(-data)
hr_test = hr_all %>% filter(data =='test') %>% select(-data,-left)

glimpse(hr_train)
glimpse(hr_test)

#taking the sample form train data
#set.seed(2)
#s=sample(1:nrow(hr_train),0.7*nrow(hr_train))
#hr_train1=hr_train[s,]
#hr_train2=hr_train[-s,]

#Parameter choosing
param=list(mtry=c(5,10,15,20,25,35),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50,100),
           nodesize=c(1,2,5,10))

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}



subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  return(subset_para)
}


num_trials=70
my_params=subset_paras(param,num_trials)


myauc=0

library(randomForest)
library(cvTools)



for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  k=cvTuning(randomForest,left~.,
             data =hr_train,
             tuning =params,
             folds = cvFolds(nrow(hr_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  if(score.this>myauc){
    print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    # uncomment the line above to keep track of progress
    best_params=params
  }
  print('DONE')
  # uncomment the line above to keep track of progress
}

############################

############################

best_params
myauc

#0.8410313
# best_params
#mtry ntree maxnodes nodesize
#410   10   500      100        2

hr.rf.final=randomForest(left~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=hr_train)

## Variable IMportance

d=importance(hr.rf.final)
d=as.data.frame(d)
d$VariableName=rownames(d)
d %>% arrange(desc(MeanDecreaseGini))

## Varimp Plot

varImpPlot(hr.rf.final)

## Partial Dependence Plot

var='left'

pred.resp = predict(hr.rf.final,newdata=hr_test,type='prob')[,2]
write.csv(pred.resp,'mysubmission.csv',row.names = F)
myvar = hr_train[,var]

trend.data=data.frame(Response=pred.resp,myvar=myvar)

trend.data %>% ggplot(aes(y=Response,x=myvar))+
  geom_smooth()  
