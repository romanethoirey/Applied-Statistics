# loi binomiale 
s1 = (rbinom(120,5,0.8))
s1
summary(s1)
hist(s1)

# loi uniforme
s2 = runif(5000,-1,2)
s2
hist(s2)
summary(s2)
var(s2)

# loi normale 
s3 = rnorm(3000,2,3)
s3
summary(s3)
sd(s3)
hist(s3,20)
rnorm(300,2,3)
rnorm(1524,2,3)

# loi exponentielle
s4 = rexp(1500,2)
s4
summary(s4)
var(s4)
hist(s4,15)
