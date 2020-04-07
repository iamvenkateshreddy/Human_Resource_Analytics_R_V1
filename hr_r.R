library(dplyr)
hr_train = read.csv('hr_train.csv',stringsAsFactors = F)
hr_test = read.csv('hr_test.csv',stringsAsFactors = F)
hr_test$left = NA
glimpse(hr_test)

hr = rbind(hr_test,hr_train)

write.csv(hr,'hr.csv',row.names = F)

getwd()

#hist(normal,probability=T, 
 #    main="Histogram of normal data",
 #    xlab="Approximately normally distributed data") 
#lines(density(normal),col=2)

hist(hr$average_montly_hours,probability=T, 
     main="Histogram of normal data",
     xlab="Approximately normally distributed data") 
lines(density(hr$average_montly_hours),col=4) 

hist(hr$average_montly_hours,breaks=1000)

qqnorm(hr$average_montly_hours,main="QQ plot of normal data",pch=19) 
qqline(hr$average_montly_hours)

library(ggplot2)
df=data.frame(res=hr$average_montly_hours)
ggplot(df,aes(x=res))+geom_density(color="red")+
  stat_function(fun=dnorm ,args = list(mean=mean(df$res),sd=sd(df$res)),color="green")

?cor.test()

#correlation test
cor.test(hr$last_evaluation,hr$average_montly_hours)

qno

table(hr$promotion_last_5years)

var(hr$satisfaction_level)
?var

median(hr$time_spend_company) %>% 
  filter(hr$left==1)

filter(hr$left==1)

filter(hr$left=1)

?filter


hr %>% median(time_spend_company) %>%
  filter(left==1)

hr %>% group_by(left) %>%
  summarise(med = median(time_spend_company,na.rm = T))
   # median(time_spend_company),na.rm = T)


hr %>% group_by(sales) %>%
  summarise(med = median(average_montly_hours,na.rm = T))

hr %>% group_by(left) %>%
  summarise(prj_count=n()) %>%
  arrange(prj_count)

library("ggpubr")
ggdensity(hr$average_montly_hours,
          main = "Density plot of Distance length",
          xlab = "Tooth length")


View(hr)


hr %>% group_by(left) %>%
  summarise(var = var(satisfaction_level))

hr %>% group_by(salary) %>%
  %
  summarise(left_c = n())
  


hr %>% select(salary,left) %>%
  group_by(salary) %>%
  filter(left==1) %>%
  summarise(left_co = n()) %>%
  arrange(-left_co)

cor.test(hr$last_evaluation,hr$average_montly_hours)
cor.test(hr$average_montly_hours,hr$last_evaluation)  

hr %>% group_by(left) %>%
  summarise(prj_count=n()) %>%
  arrange(prj_count)
names(hr)

hr %>% select(number_project) %>% 
  summarise(prj_co = n())
%>%
  
  hr %>% select(number_project) %>%
  summarise(prj_c =n())

hr %>% select(left) %>%
  summarise(l_c = n())

  7424+ 3075 + 4500
  
  getwd()
