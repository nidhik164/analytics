#create dframe of these columns for 1000 students
#roll, name, gender, age, marks1, marks2, passfail
#age-between 20 to 30 (); gender-65% females; 
#marks1- normally distributed, mean = 65, sd=10; marks2- normally distributed, mean = 70, sd=8
#placement result: pass - 0, fail - 1



n=1000
rollno=1:n
name=paste('student',rollno,sep='_')
gender=sample(x=c('M','F'),size=n,replace=T, prob = c(.35,.65))
age=trunc(runif(n, min = 20, max = 30)) #truncate converts to integer
marks1=round(x=rnorm(n, mean=65, sd=10),digits=2)
marks2=round(x=rnorm(n, mean = 70, sd = 8),digits=2)
passfail=sample(x=c(0,1),size=n,replace=T)
dataiitb=data.frame(rollno,name,gender,age,marks1,marks2,passfail)
dim(dataiitb)
nrow(dataiitb);ncol(dataiitb)
class(dataiitb)
str(dataiitb)
summary(dataiitb)
prop.table(table(dataiitb$gender))


#subdirectory data in current git repository
write.csv(dataiitb,'./iitb/dataiitb.csv')
#open the above csv in raw format in my git repository, copy the link and use it to import the same csv in any program
data_import=read.csv('https://raw.githubusercontent.com/nidhik164/analytics/master/iitb/dataiitb.csv')

library(dplyr)
#divide data in 2 groups genderwise and calculate mean, min, max, sd for each gender
dataiitb %>% group_by(gender) %>% summarise(meanM1=mean(marks1), meanM2=mean(marks2), minA=min(age), sdA=sd(marks1))
#arrange data in inc order of marks1 and dec order of marks2
dataiitb %>% arrange(marks1,desc(marks2))

#divide data in 2 groups genderwise and within each gender further group by passfail and calculate mean, min, max, sd for passes and fails
d1<-dataiitb %>% group_by(gender, passfail) %>% summarise(meanM1=mean(marks1), meanM2=mean(marks2),minA=min(age), sdM1=sd(marks1))
d1
#arrange data in order of sd of marks1
d1 %>% arrange(sdM1)
#who are top students from each gender from marks2
dataiitb %>% group_by(gender) %>% arrange(gender, marks2) %>% top_n(2,marks2)

#barplot of gender distribution
#syntax-barplot(height = c(10,20))
table(dataiitb$gender)
barplot(height = table(dataiitb$gender), col = 1:2) #col is colour (colours are mapped by numbers)
barplot(height = table(dataiitb$passfail))
hist(dataiitb$marks1)
hist(dataiitb$marks1, breaks=5, col = 1:5)
plot(density(dataiitb$marks1))
plot(density(dataiitb$age))



#create a linear model of predicting marks1 from age
fit1 = lm(marks2 ~ age + marks1, data=dataiitb)
summary(fit1)

#create a logistic model to predict passfail from marks1, marks2, gender
fit2 = glm(passfail ~ marks1 + marks2 + gender + course, data=dataiitb, family='binomial')

summary(fit2)

#create a decision tree using previous case of logistic model. Predict passfail for sample case of gender, marks1, marks2
library(rpart);library(rpart.plot)
fit3 = rpart(passfail ~ marks1 + marks2 + gender, data=dataiitb,method='class' )
summary(fit3)
rpart.plot(fit3)
#clustering
fit4 = kmeans(data2[, c('age','marks1','marks2')], centers = 4)
fit4$centers
a
