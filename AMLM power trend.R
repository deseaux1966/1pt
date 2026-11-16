#Power of the AMLM test with no trend and with the estimated trend, using the model in GameSimAMLM.R
#(the leading team's make probabilities are multiplied by alpha), 2020-21 regular season probabilities.
amlm<-function(m1,dm,s2){k<-rowSums(m1*dm)
  (k<0)*k^2/(s2*rowSums(m1^2))+rowSums(m1^2*(dm^2-s2))^2/(2*s2*(2*rowSums(m1^4*dm^2)-s2*rowSums(m1^4)))}
esttrend<-function(M){n<-ncol(M); t<-1:n
  beta1<-(M[,n]-M[,1])/(n-1); md<-M-outer(beta1,t)
  rho<-rowSums(md[,1:(n-1)]*md[,2:n])/rowSums(md[,1:(n-1)]^2)
  y<-M[,2:n]-rho*M[,1:(n-1)]; x<-outer(1-rho,2:n)+rho
  betahat<-rowSums(x*y)/rowSums(x^2); mar<-M-outer(betahat,t)
  m1<-mar[,1:(n-1)]; dm<-mar[,2:n]-m1; amlm(m1,dm,rowMeans(dm^2))}
notrend<-function(M){n<-ncol(M); y<-M[,1:(n-1)]; x<-M[,2:n]; dm<-x-y; yc<-y-rowMeans(y); xc<-x-rowMeans(x)
  s2<-(rowSums(yc^2)-rowSums(xc*yc)^2/rowSums(xc^2))/(n-3); amlm(y,dm,s2)}
p<-c(0.074,0.331,0.133); trials<-100000; n<-101
sim<-function(alpha){
  M<-matrix(0,trials,n)
  draw<-function(a){u<-runif(trials); pm<-a*sum(p)
    ifelse(u<1-pm,0,ifelse(u<1-pm+a*p[2],2,ifelse(u<1-pm+a*p[2]+a*p[3],3,1)))}
  for(k in 2:n){lead<-M[,k-1]>0; a1<-ifelse(lead,alpha,1); a2<-ifelse(lead,1,alpha)
    M[,k]<-M[,k-1]+draw(a1)-draw(a2)}
  M}
set.seed(20261106)
M0<-sim(1); a0<-notrend(M0); t0<-esttrend(M0)
cra<-quantile(a0,c(.99,.95,.90)); crt<-quantile(t0,c(.99,.95,.90))
cat("critical values no trend:",round(cra,2)," estimated trend:",round(crt,2),"\n")
res<-sapply(c(0.5,0.6,0.7,0.8,0.9),function(al){M<-sim(al); a<-notrend(M); t<-esttrend(M)
  c(sapply(cra,function(c) mean(a>c)),sapply(crt,function(c) mean(t>c)))})
colnames(res)<-c(0.5,0.6,0.7,0.8,0.9); rownames(res)<-c("none .01","none .05","none .10","est .01","est .05","est .10")
print(round(res,2))
