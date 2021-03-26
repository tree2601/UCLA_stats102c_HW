




q1a<-function(){
  #This function generates one value from the distribution X in problem 1
  #it has no arguments
  #it returns this value
  u<-runif(1)
  
  if(u<=0.1){
    return (0);
  }
  else if(u<=0.25){
    return (1);
  }
  else if (u<=0.45){
    return (2);
  }
  else if (u<=0.7){
    (3);
  }
  else {
    return (4);
  }
}


q1b<-c()
for(i in 1:10000){
  q1b<-c(q1b,q1a())
}



q2b<-function(lambda){
  #This function generates a value from a poisson distribution with lambda
  #the argument lambda gives the value of the parameter
  #the function returns the value generated
  
}



q2b<-function(lambda){
  #This function generates a value from a poisson distribution with lambda
  #the argument lambda gives the value of the parameter
  #the function returns the value generated
  u<-runif(1)
  x<-0
  cdf<-((lambda^x)*exp(-lambda))/factorial(x)
  while(u >=cdf){
    x<-x+1
    px<-((lambda^x)*exp(-lambda))/factorial(x)
    cdf<-cdf+px
  }
  return (x)
  
  
  
}

q2b(3)

q2c<-c()
for(i in 1:10000){
  q2c<-c(q2c,q2b(3))
}

q2c2<-rpois(n=10000,lambda=3)


my_beta<-function(x,a,b){
  y <-(x^(a-1))*((1-x)^(b-1))
  z<-(factorial(a-1))*(factorial(b-1))/factorial(a+b-1)
  y/z
}

q5b<-function(n){
  M<-16/9
  result<-c()
  
for(i in 1:n){
  
x<-runif(1)  
U<-runif(1)
if (U < my_beta(x,3,2)/M){
  result <-c(result,x)
}

}

  result
}



q5b2<-q5b(100000)
hist(q5b2)


