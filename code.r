#exporting libraries in r after installing the packages
library(datasets)  
library(dplyr)
library(explore)
library(GGally)
library(ggplot2)
library(car)
library(gtools)

data('mtcars') #export dataset mtcars
?mtcars
mtcars
nrow(mtcars)
ncol(mtcars)
names(mtcars)
rownames(mtcars)

#task1-->display automatic and manual separatly
task1.1=select(mtcars,'am')    #select the whole column
head(filter(task1.1,am==1))          #filter value 1-->manual
head(filter(task1.1,am==0))          #filter value 0-->automatic
head(filter(mtcars,am==1))           #for the whole dataset
head(filter(mtcars,am==0))           #for the whole dataset

#task3--->display above average mpg
task1.2=select(mtcars,'mpg')
value<-mean(mtcars$mpg)
filter(task1.2,mpg>value)   #for the exact column
filter(mtcars,mpg>value)    #for the whole dataset

#top 10 hp / disp / drat
# method 1 for each


ord_hp<- mtcars[order(-mtcars['hp']),]
ord_hp[1:10,]

ord_disp<- mtcars[order(-mtcars['disp']),]
ord_disp[1:10,]

ord_drat<- mtcars[order(-mtcars['drat']),]
ord_drat[1:10,]
#method 2 for each

ord_hp<- mtcars[order(-mtcars$hp),]
head(ord_hp,10)

ord_disp<- mtcars[order(-mtcars$disp),]
head(ord_disp,10)

ord_drat<- mtcars[order(-mtcars$drat),]
head(ord_drat,10)
#method 3

head(sort(mtcars$disp,decreasing=TRUE),n=10)
head(sort(mtcars$hp,decreasing=TRUE),n=10)
head(sort(mtcars$drat,decreasing=TRUE),n=10)

#task4-->graphs

cylinder<- c(nrow(filter(mtcars,cyl==4)),nrow(filter(mtcars,cyl==6)),nrow(filter(mtcars,cyl==8))) #vector of number of cars with each value
lable<- c("4","6","8")
pie(cylinder,labels = lable,col = rainbow(length(cylinder)),main = "Number of cylinders")

carbb<- c(nrow(filter(mtcars,carb==1)),nrow(filter(mtcars,carb==2)),nrow(filter(mtcars,carb==3)),nrow(filter(mtcars,carb==4)),nrow(filter(mtcars,carb==6)),nrow(filter(mtcars,carb==8)))
lable<- c("1","2","3","4","6","8")
pie(carbb,labels = lable, col = rainbow(length(carbb)),main = "Number of carburetors")

amm<- c(nrow(filter(mtcars,am==0)),nrow(filter(mtcars,am==1)))
lable<- c("0","1")
pie(amm,labels = lable, col = rainbow(length(amm)),main = "Transmittion (0 = Manual,1 = Automatic)")

vss<- c(nrow(filter(mtcars,v==0)),nrow(filter(mtcars,vs==1))) #vector of number of cars with each value
lable<- c("0","1")
pie(vss,labels = lable,col = rainbow(length(vss)),main = "Engine (0 = V-shaped, 1 = straight)")


gearr<- c(nrow(filter(mtcars,gear==3)),nrow(filter(mtcars,gear==4)),nrow(filter(mtcars,gear==5))) #vector of number of cars with each value
lable<- c("3","4","5")
pie(gearr,labels = lable,col = rainbow(length(gearr)),main = "Number of forward gears")

hist(
  mtcars$hp,
  main = "Gross horsepower",
  xlab = "hp value",
  ylab = "PMF value",
  border = "blue",
  col = "yellow",
  freq = F,
)

hist(
  mtcars$mpg,
  main = "Miles/(US) gallon",
  xlab = "mpg value",
  ylab = "PMF value",
  border = "blue",
  col = "yellow",
  freq = F,
)

hist(
  mtcars$disp,
  main = "Displacement (cu.in.)",
  xlab = "disp value",
  ylab = "PMF value",
  border = "blue",
  col = "yellow",
  freq = F,
)

hist(
  mtcars$drat,
  main = "Rear axle ratio",
  xlab = "drat value",
  ylab = "PMF value",
  border = "blue",
  col = "yellow",
  freq = F,
)

hist(
  mtcars$wt,
  main = "Weight (1000 lbs)",
  xlab = "wt value",
  ylab = "PMF value",
  border = "blue",
  col = "yellow",
  freq = F,
)

hist(
  mtcars$qsec,
  main = "1/4 mile time",
  xlab = "qsec value",
  ylab = "PMF value",
  border = "blue",
  col = "yellow",
  freq = F,
)

#boxPlot and three main percentiles of each variable
boxplot( mtcars$hp, main = "Gross horsepower")
quantile(mtcars$hp,c(0.25,0.5,0.75))

boxplot(mtcars$disp,main = "Displacement (cu.in.)")
quantile(mtcars$disp,c(0.25,0.5,0.75))

boxplot(mtcars$qsec,main = "1/4 mile time")
quantile(mtcars$qsec,c(0.25,0.5,0.75))

#///////////////////////////////////////////////////////////////////////////////
#3.a
100*(1-sum(dnorm(0:3.4,mean(mtcars$wt),sd(mtcars$wt)))) 

# result of each should be 1 so dnorm is more accurate
sum(dnorm(0:max(mtcars$wt),mean(mtcars$wt),sd(mtcars$wt)))
pnorm(max(mtcars$wt),mean(mtcars$wt),sd(mtcars$wt))

# 3.b
x<-filter(mtcars,am==1)
pbinom(18,nrow(mtcars),nrow(x)/nrow(mtcars) ) 

#3.c
sum(dbinom(0:4,12,0.2))

#///////////////////////////////////////////////////////////////////////////////

#4.a all permutations for ternary digits
#method 1

for(i in 0:2)
  for(j in 0:2)
    for (k in 0:2){
      y<-c(i,j,k)
      print(y)
    }
#method 2

permutations(n=3,r=3,repeats.allowed=T)

#4.b select 3 numbers
#method 1 -->built in function in R combinations

nrow(combinations(n=1,r=1))*nrow(combinations(n=1,r=1))*nrow(combinations(n=2,r=1)) 
#method 2-->built in factorial function in r

y<-(factorial(1)/(factorial(1)*factorial(1-1)))
y*y*(factorial(2)/(factorial(2-1)*factorial(1)))

#method 3-->basic operations
y<- 1/(1*1)
y*y*((2*1)/(1*1))

#///////////////////////////////////////////////////////////////////////////////

#q-q graph ( library(ggplot2) must be exported for qplot() and library(gtools) for qqplot() )

qqPlot(mtcars$disp)
qqPlot(mtcars$wt)
qqPlot(mtcars$hp)
qqPlot(mtcars$mpg)
qqPlot(mtcars$drat)
qqPlot(mtcars$qsec)
qplot(sample = am, data = mtcars, color=cyl,ylab = "am value", xlab = "0 indicates 50% of cars")+theme_bw()
qplot(sample = vs, data = mtcars, color=cyl, ylab = "vs value", xlab = "0 indicates 50% of cars")+theme_bw()
qplot(sample = gear, data = mtcars, color=cyl, ylab = "gear value", xlab = "0 indicates 50% of cars")+theme_bw()
qplot(sample = cyl, data = mtcars, color=cyl,ylab = "cyl", xlab = "0 indicates 50% of cars")+theme_bw()
qplot(sample = carb, data = mtcars, color=cyl, ylab = "carb value", xlab = "0 indicates 50% of cars")+theme_bw()

#code to symmarize relation between wech two variables
ggpairs(data = mtcars %>% select(mpg,hp,disp,vs,am,cyl,drat,gear,wt,carb,qsec))  


