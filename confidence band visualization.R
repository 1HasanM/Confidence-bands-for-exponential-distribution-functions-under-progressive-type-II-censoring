x=c(0.19,0.78,0.96,1.31,2.78,4.85,6.50,7.35)
r=c(0,0,3,0,3,0,0,5)
m=length(r)
n=sum(r)+m

cdf=function(x,par){
  mu=par[1]
  sigma=par[2]
  res=1-exp(-(x-mu)/sigma)
  return(res)
}
j1=2:m
j2=1:(m-1)
gam=NULL
for(j in 1:m)
{
  gam[j]=sum(r[j:m]+1)
}

muhat=min(x)
sigmahat=1/m*sum(gam[2:m]*(x[j1]-x[j2]))
p=0.05
q1=(1-sqrt(1-p))/2
q2=1-q1
s_q1=2*m*sigmahat/qchisq(q1,2*m-2)
s_q2=2*m*sigmahat/qchisq(q2,2*m-2)
sigmaset=c(s_q2,s_q1)
data=NULL
for(s in seq(s_q2,s_q1,.1))
{
  mu_alt=muhat+s*log(q1)/n
  mu_ust=muhat+s*log(q2)/n  
  data=rbind(data,c(s,mu_alt,mu_ust))
}

for(i in 1:100)  
{
  y=runif(1,s_q2,s_q1)
  x=runif(1,muhat+y*log(q1)/n,muhat+y*log(q2)/n)
  xxlim=80
  xx=seq(0,xxlim,.1)
  df=cdf(xx,c(x,y))
  plot(xx,df,xlim=c(0,xxlim),ylim=c(0,1),cex=.5,
       type = "l",xlab = "",ylab = "",axes = F)
  par(new=T)
}
plot(xx,cdf(xx,c(muhat,sigmahat)),xlim=c(0,xxlim),ylim=c(0,1),cex=2,
     col="red",type = "l",xlab = "x",ylab = "cdf") 