x<-rexp(n=1000,rate =0.1)
ecdf(x)
plot(ecdf(x), ylab = expression(P(X <= x)), main = "EDF", las = 1)

q3e<-ecdf(x)
summary(xx)




q5c <-function(lambda,eta,n){
#This function generates random numbers from the two-parameter exponential distribution.
#lambda and eta are parameters and n specifies how many numbers to generate.
#The function gives a number or a vector of numbers generated from the distribution. 
  x <- runif(n)
  y <- eta-(log(1-x)/lambda)
  y
}

runif(1)
q5c(0.4,0,20)
xx

z <-seq(0,80,0.1)

sqrt((q3e(z)*(1-q3e(z)))/1000)*1.96



# **(d))**
#   Let N be the number of calls in 5 seconds. 
# \[
#   \begin{eqnarray}
#   N&\sim& poi(\lambda'),\lambda'=6/12=0.5\\
#                P(N >0) &=& 1-P(N=0)=1-\frac{0.5^0e^{-0.5}}{0!} \\
#                &=&0.3935
#                
#                \end{eqnarray}
#                \]
# 

zc<-sqrt((q3e(z)*(1-q3e(z)))/1000)*1.96 
U<- q3e(z)+zc
L<-q3e(z)-zc

plot(z,q3e(z),ylab = expression(P(X <= x)), main = "EDF", las = 1)
lines(L)
lines(U)








