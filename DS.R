#Data Structures----



#matrix----


#vectors----
v1=1:100    #create vector from 1 to 100
v1
V2=c(1,4,5,10)
V2
class(v1)
class(v2)
v3=c('a','Dhiraj','Sachin')
v3   # to print the vector
class(v3)
V4=c(TRUE, FALSE, T, F, T)
class(V4)

#Mean----
mean(v1)
median(v1)
sd(v1)
hist(v1)
hist(women$height)
V2[V2>=5]


x=rnorm(60, mean=60, sd=10)
x
plot(x)
hist(x)
plot(density(x))
abline(v=60)
#rectangles and density line together
hist(x, freq = F)
lines(density(x))

hist(x,breaks = 10, col=1:10)
length(x)
sd(x)
?sample
x1=LETTERS[5:20]
x1

set.seed(1234)  # to retain the order of output

y1=sample(x1)
y1


(y2=sample(x1, size = 5))


set.seed(5)

(gender=sample(c('M','F'), size=10000000, replace=TRUE, prob = c(.6,.4)))
t1=table(gender)
prop.table(t1)
pie(t1)
barplot(t1,col = 1:2,horiz = T)


#matrix-----
(m1=matrix(1:24, nrow = 4))
(m2=matrix(1:24, nrow = 4, byrow = T))
(m3=matrix(1:24, ncol=4, byrow=T))
(x=trunc(runif(60, 60, 100)))
(x=trunc(runif(60,60,100)))
plot(density(x))
(m4 = matrix(x, ncol=6))
?runif
colSums(m4)
rowSums(m4)
rowMeans(m4)
colMeans(m4)
m4[m4>67 & m4<86]
m4[8:10, c(1,3,5)]
rowSums(m4[8:10, c(1,3,5)])

#Array---similar to matrix

#data.frame
#rollno, name, gender, course, marks1, marks2
(rollno = 1:60)
#student1, student2
(name=paste('student1', 1:60, sep='-'))
paste('IIM', paste('student1', 1:60, sep='-'), sep = ' & ')
name[1:10]
name[15:20]
name[c(10,20,37)]
name[-c(1:10)]
rev(name)
?rev

(gender=sample(c('Male', 'Female'), size=60, replace=T,prob = c(.3,.7)))
(course=sample(c("BBA", "MBA", "FPM"), size=60, replace=T, prob = c(.2,.3,.5)))
(marks1=ceiling(rnorm(60, mean=65, sd=7)))
(marks2=ceiling(rnorm(60, mean=60, sd=11)))
(grades=sample(c('A','B','C'), size = 60, replace=T))
(students=data.frame(rollno, name ,gender, course, marks1, marks2, grades, stringsAsFactors = F))
class
summary(students)
students[, c('name')]
students[students$gender  =='Male', c('rollno','gender', 'marks1')]
students[students$gender=='Male' & students$grades == 'C', c('rollno', 'gender', 'marks1')]
#students[students$marks1 > 55 | students$marks1 < 75, c('name', 'marks')]
students$gender
t1=table(students$gender)
barplot(table(students$course, ylim=c(0,50), col=1:3))
studenstr(students)
nrow(students)
names(students)
dim(students)
head(students)
tail(students)
head(students,n=7)
barplot(table(students$course, ylim=c(0,60), col=1:3))
barplot(table(students$course))
students[students$gender]

#avg marks scored by each gender
#gender, marks1
aggregate(students$marks1, by=list(students$gender), FUN=mean)
aggregate(students$marks2, by=list(students$course), FUN=max)
aggregate(students$marks2, by=list(students$course, students$gender), FUN=mean)
aggregate(students$marks2, by=list(students$course, students$gender, students$grades), FUN=mean)

#dplyr
library(dplyr)
students %>% group_by(course,gender) %>% summarise(mean(marks1), min(marks2), max(marks2))

students %>% group_by(course,gender) %>% summarise(meanmarks1=mean(marks1), min(marks2), max(marks2)) %>% arrange(desc(meanmarks1))
students %>% arrange(desc(marks1)) %>% filter(gender=='Male') %>% top_n(5)

x=sample_frac(students, size = 0.1, replace = FALSE, weight = NULL, .env = parent.frame() )

students %>% sample_frac(.1) %>% arrange(course)
students %>% sample_n(2)
students %>% arrange(desc(course), gender, marks1)
students %>% arrange(course, grades,marks1) %>% select(course, grades, marks1)
?sample_frac
summary(students$gender)
summary(students$course)
students$course = factor(students$course, order=T, levels=c('FPM', 'MBA', 'BBA'))
summary(students$course)

#factor

names(students)
students$gender=factor(students$gender)#for ordinal type of data like grades(excellent, sat, poor) for which mode and median is available but no mean
#it helps us convert a clum into an ordered category
#C, A, B C<A<B
students$grades = factor(students$grades, ordered=T, levels=c('C','A','B'))
summary(students$gender)
students$grades
table(students$grades)
barplot(table(students$grades))

students
write.csv(students,'./data/iimtrichy.csv')
students2=read.csv('./data/iimtrichy.csv')
head(students2)
students3 = read.csv(file.choose())
head(students3)
students3=students3[,-1]
head(students3)
library(gsheet)
url= 'https://docs.google.com/spreadsheets/d/1h7HU0X_Q4T5h5D1Q36qoK40Tplz94x_HZYHOJJC_edU/edit#gid=989407334'
denco=as.data.frame(gsheet2tbl(url))
str(denco)
head(denco)

library(dplyr)
denco %>% count(custname, sort=TRUE)

denco %>% group_by(custname) %>% summarise(n=n() %>% arrange(desc(n))

df3a = aggregate(denco$revenue, by=list(denco$partnum, FUN=sum))
head(df3a)

denco %>% group_by(partnum) %>% summarise(n=n()) %>% arrange(desc(n))

##Neeta's code
denco %>% count(custname, sort=TRUE)

denco %>% group_by(custname) %>% summarise(n=n()) %>% arrange(desc(n))


#summarise by part num

df3a= aggregate(denco$revenue, by= list(denco$partnum),FUN=sum)
head(df3a)

denco %>% group_by(partnum) %>% summarise(n=n()) %>% arrange(desc(n)) # summarise(n=n()) is count


names(denco)
df4a = aggregate(margin ~partnum, data= denco, FUN= sum)
aggregate(margin ~ partnum, data= denco, FUN=sum)
head(df4a[order(df4a$margin, decreasing =T),])

sales %>% group_by (partnum) %>% summarise(TotalMargin= sum(margin)) %>% arrange

##done
                                           