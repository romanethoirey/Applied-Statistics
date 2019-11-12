# Calculate with the normal distribution 
# t 1-a/2, a étant => 0,05 quand on a 95% d'intervalle de confidence
qnorm(0.9)
qnorm(0.96)
qnorm(0.99)

## Formula to compute confidence interval [a,b] of 1 - alpha confidence level 
## of the population mean of a variable x following the normal distribution with a known sigma
# N sample size, 
# Xn sample mean, 
# Sigma population standard deviation, 
# t quantile of standard normal distribution of order 1 - alpha / 2

b = xn + t * sigma / sqrt(n)
a = xn - t * sigma / sqrt(n)

# 3.

#fonction pour calculer le quantil
quantil<-function(I){
  a=1-I
  x= 1-(a/2)
  rslt<-qnorm(x,0,1)
  rslt
}

ci_norm<- function(I,mn,sg,n){
  t<-quantil(I)
  sq<-sqrt(n)
  minus<-(mn -t*(sg/sq))
  plus<-(mn +t*(sg/sq))
  paste(" [ ",minus,";",plus," ]")
  
}

# 4.
ci_norm(0.95,45,5,25)
ci_norm(0.99,45,5,25)

# 5.
ci_norm(0.95,1150,25, 36)

# 6.
ci_norm(0.95,65,22,12)
ci_norm(0.95,185,10,22)
ci_norm(0.95,19,30,50)

# 12.
quantil_stu<-function(I,f){
  a=1-I
  x= 1-(a/2)
  rslt=qt(x,f)
  rslt
}

ci_stu <- function(I,mn,n,s){
  t<-quantil_stu(I,n-1)
  sq<-sqrt(n)
  minus<-(mn -t*(s/sq))
  plus<-(mn +t*(s/sq))
  paste(" [ ",minus,";",plus," ]")
}
 
ci_stu(0.95, 26, 10, 9)  
ci_stu(0.95, 132, 18, 20)
ci_stu(0.95, 52, 25, 12)

# 13.

ci_stu(0.99, 26, 10, 9)  
ci_stu(0.99, 132, 18, 20)
ci_stu(0.99, 52, 25, 12)


