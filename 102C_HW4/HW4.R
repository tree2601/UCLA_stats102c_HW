






q1<-function(x){
  2*exp(-2*x)
}
integrate(q1,0,1)
integrate(q,0,1)

q<-function(x){
  exp(-2*x)
}




{
curve(q3a2)
curve(q3a1,add=T)
}




plot(q3a1)

plot(q3a2)

x<-seq(0,1,by=0.1)
y1<-q3a1(x)
y2<-q3a2(x)
{
plot(x,y2)
lines(x,y1,col="red")

}




1-pnorm(0.5)

1-pnorm(1)
1-pnorm(2)
1-pnorm(3)


n <- 1000
MC1 <- MC2 <- numeric(n)
for(i in 1:n){
  MC1[i] <- q1a(10000)
  MC2[i] <- q1b(10000)
}
(var(MC1) - var(MC2)) / var(MC1)

var(MC1)
var(MC2)



q1c1<-function(x){
  2*exp(-2*x)
}
integrate(q1c2,0,1)

q1c2<-function(x){
  exp(-2*x)
}

  u <- runif(10000)
  B <- q1c1(u)
  A <- q1c2(u)
  cor(A, B)
  a <- -cov(A, B) / var(B)
  
  
  
  T1 <- q1c1(u) 
  T2 <- T1 + a * (q1c2(u) - 0.4323324)
  mean(T2)












