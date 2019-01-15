#AZMIARDHY ZULKIFLI F
#17523225

#NO.1
x <- c(50, 51, 52, 53, 54)
y <- c(40, 46, 44, 55, 49)
relation <- lm(y~x)
print(relation)
     
#2
f<-function(x){
  return(-93.6+2.7*x)
}

#3
xi=c(0:4)
yi=c(1,1.25,3.75,4.25,5.65)
poly.calc(xi,yi)

#4
f1<-function(x){
  return(1 - 4.079167*x + 6.527083*x^2 - 2.495833*x^3 + 0.2979167*x^4)
}
f1(2.75)

#NO.5 
plot(xi,yi)
curve(f1,add=TRUE)

#NO.6 bi <- function(a,b) {
re <- 3
pn <- (a + b)/2
while (re >= 0.0001){
  print(paste(a,b,pn,fx(pn),fx(a), re,sep=" "))
  p <-pn
  if (sign(g(p))== sign(g(a))) {
    a<-p
  }else {
    b<-p
  }
  pn <- (a + b)/2
  re <- abs(pn-p) /abs(pn)
  
}
"C"

#NO.7
"D"


#NO.8 
"C"

#No.9 
"B"

#NO.10
#No.11
"B"

#NO.12
"D"

#NO.13
h <- 0.1
x <- seq(0,1, by=h)
f <- function(x) {
  return(x^2)
}
f0 <- f(x[1])
fi <- sapply(x[2:10], f)
fn <- f(x[length(x)])

trap <- function(f0, fi, fn, h){
  L<- h*(f0 + 2*sum(fi)+fn)/2
  return(L)
}
trap(f0, fi, fn, h)


#NO.14
0.335


#NO.15
h <- 0.2
x <- seq(0,1, by=h)
f <- function(x) {
  return(x^2)
}
f0 <- f(x[1])
fi <- sapply(x[2:4], f)
fn <- f(x[length(x)])

trap <- function(f0, fi, fn, h){
  L<- h*(f0 + 2*sum(fi)+fn)/2
  return(L)
}
trap(f0, fi, fn, h)





