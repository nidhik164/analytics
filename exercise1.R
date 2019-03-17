#create dframe of these columns for 1000 students
#roll, name, gender, age, marks1, marks2, passfail
#age-between 20 to 30 (); gender-65% females; 
#marks1- normally distributed, mean = 65, sd=10; marks2- normally distributed, mean = 70, sd=8
#placement result: pass - 0, fail - 1



n=1000
rollno=1:n
name=paste('student',rollno,sep='_')
gender=sample(x=c('M','F'),size=rollno,replace=T, prob = c(.35,.65))
age=trunc(runif(n, min = 20, max = 30))
marks1=round(x=rnorm(n, mean=65, sd=10),digits=2)
marks2=round(x=rnorm(n, mean = 70, sd = 8),digits=2)
passfail=sample(x=c(0,1),size=n,replace=T)
dataiitb=data.frame(rollno,name,gender,age,marks1,marks2,passfail)
dim(dataiitb)
nrow(dataiitb)
ncol(dataiitb);str(dataiitb);
class(dataiitb)
