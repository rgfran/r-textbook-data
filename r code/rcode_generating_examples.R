#functions to generate random data
random_cor <- function(nobs, ncors){
  a = 1:ncors
  for (i in 1:ncors) {
    x = rnorm(nobs)
    b = rnorm(nobs)
    a[i] = cor(x,b)
  }
  return(a)
}

#generating standard deviation materials
testa = round(rnorm(10,75,8))
testb = round(rnorm(10,75,2))
testc = round(rnorm(10,75,15))
sd.test = data.frame(testa,testb,testc)

#randomly generating bivariate data with a certain correlation

nobs = 10
m1 = 8
s1 = 3
m2 = 8
s2 = 4
corr = -.6
rounda = 0
roundb = 0

#need to make a function for mean filling data
meanfill = function(df){
  c = 0
  for (i in 1:nrow(test2)){
    for (j in 1:ncol(test2)){
      if(is.na(test2[i,j]))
      {
        test2[i,j] = mean(test2[,j], na.rm = T)
        c = c+1                    
      }
      
      
    }
  }
  
}

randcorr = function(nobs,m1=0,s1=1,m2=0,s2=1,corr=0,rounda=0,roundb=0,tol=.01){
  count = 0
  if(abs(corr) > 1){print('enter a correlation between -1 and 1');break}
  repeat
  { 
    count = count+1
    a = round(rnorm(nobs,m1,s1), rounda)
    b = round(rnorm(nobs,m2,s2), roundb)
    if(abs((cor(a,b)) - corr) < tol)
      {
        return(data.frame(a,b))
        break
      }

  
  }
}
randsec = function(data,m=0,s=1,corr=0,round=0,tol=.01,mina = 0){
  if(abs(corr) > 1){print('enter a correlation between -1 and 1');break}
repeat
{ 
  nobs = length(data)
  a = round(rnorm(nobs,m,s), round)
  if(abs((cor(a,data)) - corr) < tol & min(a) > mina)
  {
    return(a)

    break
  }
  
  
}
}
  
  
#height example lecture 4

height = round(rnorm(100,68,5))

plot PDP
hist(height, breaks = c(58,60,62,64,66,68,70,72,74,76,78,80))
hist(height, breaks = c(58,60,62,64,66,68,70,72,74,76,78,80),freq = FALSE)

#Plot CDP
t = sort(height)
plot(t, 1:100/100, type = 's', ylim = c(0, 1), xlab = 'height', ylab = '', main = "Cumulative density plot")
t = sort(height, decreasing = TRUE)
plot(t, 1:100/100, type = 's', ylim = c(0, 1), xlab = 'height', ylab = '', main = "Survivor plot")

#regression between weight and mile time Lecture 6
milereg = randcorr(25,160,30,480,40,.6)
colnames(milereg)[1:2] = c("weight","miletime")
milereg$milemins = milereg$miletime/60
milereg$person = 1:nrow(milereg)

them = theme(text=element_text(size=24, family="Arial"))
s = ggplot(milereg, aes(x = milemins, y = weight))
s + geom_point(size = 3) + labs(x = "Mile Time (mins)", y = "Weight LBS") + them
them = theme(text=element_text(size=24, family="Arial"))
s + geom_point(size = 3) + geom_smooth(method="lm", se = F) + labs(x = "Mile Time (mins)", y = "Weight LBS") + them

milereg$hrsexercise = randsec(milereg$weight,8,4,-.6)
s = ggplot(milereg, aes(x = milemins, y = hrsexercise))
s + geom_point(size = 3) + labs(x = "Mile Time (mins)", y = "Hours exercised") + them
them = theme(text=element_text(size=24, family="Arial"))
s + geom_point(size = 3) + geom_smooth(method="lm", se = F) + labs(x = "Mile Time (mins)", y = "hours exercised") + them



#random data for IQ and sampling, doesn't work well
IQ = round(rnorm(1000,100,15), 0)
observation = 1:1000
IQsample = data.frame(observation)

me2 = 1
sd2 = 1
for (i in 1:1000){
  
  for (j in 1:2){
      
    s2 = IQ[sample(1:1000,2,replace=F)]
    me2[j] = mean(s2)
    
    IQsample$m2[i] = mean(me2)
    IQsample$s2[i] = sd(me2)
  }
  
  for (j in 1:4){
    
    s2 = IQ[sample(1:1000,4,replace=F)]
    me2[j] = mean(s2)
    IQsample$m4[i] = mean(me2)
    IQsample$s4[i] = sd(me2)
  }
  
  for (j in 1:9){
    
    s2 = IQ[sample(1:1000,9,replace=F)]
    me2[j] = mean(s2)
    IQsample$m9[i] = mean(me2)
    IQsample$s9[i] = sd(me2)
  }
    

}
library(psych)
describe(IQ)
describe(IQsample[2:7])
rm(s2,s4,s9,sd2,m2,m4,m9,me2)

#death data for distributions
library(ggplot2,fonts)
maledeath = read.csv(file.choose())
femaledeath = read.csv(file.choose())
maledeath$pd2011 = maledeath$X2011 / sum(maledeath$X2011)
maledeath$cd2011 = 0
for (i in 1:nrow(maledeath)){
  maledeath$cd2011[i] = sum(maledeath$pd2011[1:i])
}
maledeath$sd2011 = 1 - maledeath$cd2011

femaledeath$pd2011 = femaledeath$X2011 / sum(femaledeath$X2011)
femaledeath$cd2011 = 0
for (i in 1:nrow(femaledeath)){
  femaledeath$cd2011[i] = sum(femaledeath$pd2011[1:i])
}
femaledeath$sd2011 = 1 - femaledeath$cd2011
femaledeath$Age[106] = 105
femaledeath$Age = as.numeric(as.character(femaledeath$Age))
combdata = data.frame(maledeath[,1],maledeath[,39:42])
colnames(combdata)[1] = "Age"
combdata$sex = "male"
x = data.frame(femaledeath[,1],femaledeath[,39:42])
x$sex = "female"
colnames(x) = colnames(combdata)
combdata = rbind(combdata,x)
colnames(combdata) = c("Age","maledeaths","malePD","maleCD","maleSD","femaledeaths","femalePD","femaleCD","femaleSD")
combdata$Age = as.numeric(as.character(combdata$Age))
combdata$Age[212] = 105
them = theme(text=element_text(size=24, family="Arial"))
      
s = ggplot(combdata, aes(x = Age, y = X2011, group = sex, color = sex))
s + geom_line(size = 3) + labs(x = "Age", y = "Number of Deaths") + them
them = theme(text=element_text(size=24, family="Arial"))

s = ggplot(combdata, aes(x = Age, y = pd2011, group = sex, color = sex))
s + geom_line(size = 3) + labs(x = "Age", y = "Number of Deaths") + them

s = ggplot(combdata, aes(x = Age, y = cd2011, group = sex, color = sex))
s + geom_line(size = 3) + labs(x = "Age", y = "Number of Deaths") + them
s = ggplot(combdata, aes(x = Age, y = sd2011, group = sex, color = sex))
s + geom_line(size = 3) + labs(x = "Age", y = "Number of Deaths") + them


#make a whole dataset of pdfs
malepdf = maledeath
femalepdf = femaledeath
for (j in 2:37){
  for (i in 1:nrow(maledeath)){
    malepdf[i,j] = maledeath[i,j] /sum(maledeath[,j])
}}
for (j in 2:37){
  for (i in 1:nrow(femaledeath)){
    femalepdf[i,j] = femaledeath[i,j] /sum(femaledeath[,j])
  }}

t = ggplot(malepdf, aes(x = Age, y = X2002))
t + geom_line(size = 3) + labs(x = "Age", y = "Number of Deaths") + them

#make distribution data for  baseball
bat2014 = read.csv(file.choose())

