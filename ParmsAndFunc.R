#source this file before everything else
#script for parameters and functions
set.seed(2021)

alpha = .05
nrep = 2000 # number of replications
N=c(500,1000,1500) # sample sizes

cols = c("green","purple","orange")

#cols= c("green","purple4")

methnames=c("GOODMAN/BONFERRONI INTERVAL","FITZPATRICK-SCOTT QUICK INTERVAL")

meth=c("GB","FS","GB.logratio")

p_pt1a=c(.5,.3,.2)
p_pt1b=c(.5,.48,.02)

p_pt2=c(.5,.25,.25)
r=c(.1,.45,.45)

q=c(.1,.2,.3,.4,.5)

V=matrix(0,3,2000)
NV=matrix(0,3,2000)


CI.GB=function(M,alpha,N,p,i,j)
{
  pij=p[i]-p[j]
  m=dim(M)
  nrep=m[2]
  m=m[1]
  n=m*(m-1)/2
  
  pi=M[i,]
  pj=M[j,]
  
  A = qchisq(1-alpha/n,n-1)
  deltaij=pi-pj
  dij=pi+pj-deltaij^2
  
  ConfInt=matrix(0,nrep,2)
  
  Conf.lower = deltaij - sqrt(A*dij/N)
  Conf.upper = deltaij + sqrt(A*dij/N)
  
  #a=mean(Conf.lower)
  #b=mean(Conf.upper)
  
  ConfInt[,1]=Conf.lower
  ConfInt[,2]=Conf.upper
  Diff=Conf.upper - Conf.lower
  Conf.width = mean(Diff)
  ci.width.se = sd(Diff)/sqrt(nrep)
  
  CI.out = Conf.upper < pij | Conf.lower > pij
  #CI.in = Conf.upper > pij & Conf.lower < pij
  #p.out =sum(CI.out)/nrep
  
  list(ConfInt=ConfInt,CI.out=CI.out, Conf.width=Conf.width,ci.width.se=ci.width.se )
  
}


CI.FS=function(M,alpha,N,p,i,j)
{
  pij=p[i]-p[j]
  m=dim(M)
  nrep=m[2]
  m=m[1]
  
  pi=M[i,]
  pj=M[j,]
  
  deltaij=pi-pj
  
  ConfInt=matrix(0,nrep,2)
  
  x = seq(1,4,length=1000)
  y = 1 - 2*(1 - pnorm(x)) - 4*(m-2)*(1-pnorm(x*sqrt(2)))
  a = min(x[y >= 1 - alpha])
  

  #plot to show this
  # plot(x,y,type="l")
  # abline(h = 1-alpha,col="red")
  # abline(v = a,col="red")
  
  
  Conf.lower = deltaij - a/sqrt(N)
  Conf.upper = deltaij + a/sqrt(N)
  
  ConfInt[,1]=Conf.lower
  ConfInt[,2]=Conf.upper
  Diff=Conf.upper - Conf.lower
  Conf.width = mean(Diff)
  ci.width.se = sd(Diff)/sqrt(nrep)
  
  CI.out = Conf.upper < pij | Conf.lower > pij
  #CI.in = Conf.upper > pij & Conf.lower < pij
  
  #p.out =sum(CI.out)/nrep
  
  list(ConfInt=ConfInt,CI.out=CI.out, Conf.width=Conf.width,ci.width.se=ci.width.se )
  
}




CI.GB.logratio=function(M,alpha,N,p,i,j)
{
  pij=log(p[i])-log(p[j])
  m=dim(M)
  nrep=m[2]
  m=m[1]
  n=m*(m-1)/2
  
  pi=M[i,]
  pj=M[j,]
  
  Xi=pi*N
  Xj=pj*N
  
  A = qchisq(1-alpha/n,n-1)
  deltaij=log(pi)-log(pj)
  dij=pi+pj-deltaij^2
  
  ConfInt=matrix(0,nrep,2)
  
  Conf.lower = deltaij - sqrt(A*(1/Xi + 1/Xj))
  Conf.upper = deltaij + sqrt(A*(1/Xi + 1/Xj))
  
  #a=mean(Conf.lower)
  #b=mean(Conf.upper)
  
  ConfInt[,1]=Conf.lower
  ConfInt[,2]=Conf.upper
  Diff=Conf.upper - Conf.lower
  Conf.width = mean(Diff)
  ci.width.se = sd(Diff)/sqrt(nrep)
  
  CI.out = Conf.upper < pij | Conf.lower > pij
  #CI.in = Conf.upper > pij & Conf.lower < pij
  #p.out =sum(CI.out)/nrep
  
  list(ConfInt=ConfInt,CI.out=CI.out, Conf.width=Conf.width,ci.width.se=ci.width.se )
  
}

















CI.out.se=function(a,b,c)
{
  d=a|b|c
  
  p.out=mean(d)
  
  #cover=1-d
  #p.se = sd(d)/sqrt(nrep)
  
  list(p.out=p.out,d=d)
}
#doHist=FALSE, mainHist=NULL, doQQ=FALSE, mainQQ=NULL,mainCI=NULL,
plot.conf.int = function(Confint,ci.out,target,text,n,me,qvalue=0,conf.int.colors=c("cyan","red"), alpha=.05,  Y.lim=NULL, a=FALSE,b=FALSE,q=FALSE,clean=FALSE) {
  
  cmat = Confint
  wd = mean(cmat[,2] - cmat[,1])
  
  N = dim(cmat)[1]
  cnt.out = sum(ci.out) #number of intervals that missed target
  conf.col = rep(conf.int.colors[1],N)
  conf.col[ci.out] = conf.int.colors[2]
  if(is.null(Y.lim)) Y.lim=c(min(range(cmat)),max(range(cmat),target))
 
  
  plot(0,0,xlim=c(0,N),ylim=Y.lim,xlab="",ylab="",type="n")
  if(a)
    mainCI=paste("Simulation of ",paste(round(100*(1-alpha)),"%",sep="")," Confidence Intervals for ",text," Part 1 Item a",sep="")
  if(b)
    mainCI=paste("Simulation of ",paste(round(100*(1-alpha)),"%",sep="")," Confidence Intervals for ",text," Part 1 Item b",sep="")
  #if(q)
  #   mainCI=paste("Simulation of ",paste(round(100*(1-alpha)),"%",sep="")," Confidence Intervals for ",text," Part 2",sep="")
  # if(clean)
  #   mainCI=paste("Simulation of ",paste(round(100*(1-alpha)),"%",sep="")," Confidence Intervals for ",text," Part 2",sep="")
  
  #title(mainCI)
  for(k in seq(N)) {
    #rect(k-1,cmat[k,1],k,cmat[k,2],col=conf.col[k],border = NA )
    segments(k,cmat[k,1],k,cmat[k,2],col=conf.col[k])
  }
  if(clean|q)
  mtext(paste(paste(round(100*(1-cnt.out/N),1),"%",sep=""),"of these intervals contain:",round(target, digits = 3)),side=3,line=.25,cex=.8)
  #abline(h=target,col="black")
  segments(0, target, x1 = N, y1 = target,col="black")
  if(a|b){
    title(mainCI)
    mtext(paste(paste(round(100*(1-cnt.out/N),1),"%",sep=""),"of these intervals contain:",target),side=3,line=.25)
    mtext(paste("Sample size = ",n,"    Mean width = ",round(wd,3),"    Method: ", me),side=1,line=2.5)}
  if(q)
    mtext(paste("Mean width = ",round(wd,3),"    Mixing: ", qvalue*100,"%",sep="" ),side=1,line=2.5,cex=.8)
  if(clean)
    mtext(paste("Mean width = ",round(wd,3),sep="" ),side=1,line=2.5,cex=.8)
  
}
save.image("Data/Parms.RData")