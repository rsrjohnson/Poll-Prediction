load("Data/Parms.RData")

nmeths = length(meth)
meths.pch = 1:nmeths

ind=list(c(1,2),c(1,3),c(2,3))




###Confidence Intervals

# ##### Part 1 Item a
for(i in seq_along(meth))
{
  for(j in seq_along(N))
  {
    for(k in seq_along(ind))
    {
      s=ind[[k]][1]
      t=ind[[k]][2]

      target=p_pt1a[s]-p_pt1a[t]

      text=paste("p",s,"-p",t,sep="")

      datatoload=paste(meth[i],text,sep = "_")

      data=get(load(paste("Out/Part 1/CIa",datatoload,N[j],"Rdata",sep=".")))



      png(paste("Graphics/Part 1/Part1-a_",paste(text,N[j],meth[i],"png",sep="."),sep=""),width=1600,height=800,res=120)

      plot.conf.int(data$ConfInt,data$CI.out,target,text,N[j],meth[i],a=TRUE)


      graphics.off()
    }

  }

}

##### Part 1 Item b

for(i in seq(length(meth)))
{
  for(j in seq(length(N)))
  {
    for(k in seq(length(ind)))
    {
      s=ind[[k]][1]
      t=ind[[k]][2]

      target=p_pt1b[s]-p_pt1b[t]

      text=paste("p",s,"-p",t,sep="")

      datatoload=paste(meth[i],text,sep = "_")

      data=get(load(paste("Out/Part 1/CIb",datatoload,N[j],"Rdata",sep=".")))



      png(paste("Graphics/Part 1/Part1-b_",paste(text,N[j],meth[i],"png",sep="."),sep=""),width=1600,height=800,res=120)

      plot.conf.int(data$ConfInt,data$CI.out,target,text,N[j],meth[i],b=TRUE)


      graphics.off()
    }

  }

}


####Part 2


for(k in seq_along(ind))
{
  s=ind[[k]][1]
  t=ind[[k]][2]
  
  text=paste("p",s,"-p",t,sep="")
  
  logtext=paste("log(p",s,")-log(p",t,")",sep="")
  
  target=p_pt2[s]-p_pt2[t]
 
  for(i in seq_along(meth))
  {
    if(i==3) target=log(p_pt2[s])-log(p_pt2[t])
    for(j in seq_along(N))
    {
      
      datatoload=paste(meth[i],text,sep = "_")
      
      data=get(load(paste("Out/Part 2/CI.clean",datatoload,N[j],"Rdata",sep=".")))
      
      y=range(data$ConfInt)



      for(l in q)
      {


        datatoload=paste(meth[i],text,sep = "_")

        data=get(load(paste("Out/Part 2/CI",datatoload,N[j],l,"Rdata",sep=".")))

        yt=range(data$ConfInt)

        y=c(min(yt,y),max(yt,y))



      }

      datatoload=paste(meth[i],text,sep = "_")

      data=get(load(paste("Out/Part 2/CI.clean",datatoload,N[j],"Rdata",sep=".")))
      
      png(paste("Graphics/Part 2/Part2_",paste(text,N[j],meth[i],"png",sep="."),sep=""),width=1600,height=800,res=120)
      par(mfrow=c(2,3))
      
      #plot.conf.int(data$ConfInt,data$CI.out,target,text,N[j],meth[i],clean=TRUE)
      plot.conf.int(data$ConfInt,data$CI.out,target,text,N[j],meth[i],clean=TRUE,Y.lim=y)
      if(i==3)
        title(paste("Simulation of ",paste(round(100*(1-alpha)),"%",sep="")," Confidence Intervals for ",logtext," = ",round(target, digits = 3),"  Sample size = ",N[j],"    Method: ", meth[i],sep=""),outer = TRUE,cex=1.33,font=2,line=-2)
      else
        title(paste("Simulation of ",paste(round(100*(1-alpha)),"%",sep="")," Confidence Intervals for ",text," = ",round(target, digits = 3),"  Sample size = ",N[j],"    Method: ", meth[i],sep=""),outer = TRUE,cex=1.33,font=2,line=-2)
      #mtext(paste("    Sample size = ",N[j],"    Method: ", meth[i]),outer=TRUE,line=-1.5,cex=.8)
        
        for(l in q)
        {
        
        
        datatoload=paste(meth[i],text,sep = "_")
        
        data=get(load(paste("Out/Part 2/CI",datatoload,N[j],l,"Rdata",sep=".")))
        
        
        
        #png(paste("Graphics/Part 2/Part2_",paste(text,N[j],meth[i],l,"png",sep="."),sep=""),width=1600,height=800,res=120)
        
        #plot.conf.int(data$ConfInt,data$CI.out,target,text,N[j],meth[i],qvalue=l,q=TRUE)
        plot.conf.int(data$ConfInt,data$CI.out,target,text,N[j],meth[i],qvalue=l,q=TRUE,Y.lim=y)
        
        
        #graphics.off()
        }
      
      graphics.off()
      }
      
    }
    
  }



###########################################################
#miscoverage probability

load(paste("Out/Part 1/pa.out","RData",sep="."))

png("Graphics/ Part 1 Item a.png",width=1600,height=800,res=120)

yLim = c(0,max(c(2*alpha,pa.out)))

plot(N,pa.out[1,],xaxt="none",type="n",pch=meths.pch[1],ylim=yLim,ylab="Miscoverage Probability",
     xlab="Sample size")
axis(1, N)
for(k in seq(nmeths)) {
  #lines(N,pa.out[k,],type="b",pch=meths.pch[k])
  lines(N,pa.out[k,],type="p",pch=meths.pch[k])
  lines(N,pa.out[k,],type="c",col=cols[k],lwd=2)
}
abline(h=alpha,col="red",lwd=2)
abline(h=0,col="blue",lwd=2)

legend(max(N),yLim[2],legend=meth,pch=meths.pch,xjust=1,yjust=1,cex=.9,text.col=cols,title="Methods",title.col="black")
mtext("Miscoverage Probability vs Sample Size",outer=TRUE,cex=1.33,font=2,line=-2)
mtext("Part 1 Item a",outer=TRUE,line=-3.25,cex=1.2)

graphics.off()

load(paste("Out/Part 1/pb.out","RData",sep="."))

png("Graphics/ Part 1 Item b.png",width=1600,height=800,res=120)

yLim = c(0,max(c(2*alpha,pa.out)))

plot(N,pb.out[1,],xaxt="none",type="n",pch=meths.pch[1],ylim=yLim,ylab="Miscoverage Probability",
     xlab="Sample size")
axis(1, N)
for(k in seq(nmeths)) {
  #lines(N,pb.out[k,],type="b",pch=meths.pch[k])
  lines(N,pb.out[k,],type="p",pch=meths.pch[k])
  lines(N,pb.out[k,],type="c",col=cols[k],lwd=2)
}
abline(h=alpha,col="red",lwd=2)
abline(h=0,col="blue",lwd=2)

legend(max(N),yLim[2],legend=meth,pch=meths.pch,xjust=1,yjust=1,cex=.9,text.col=cols,title="Methods",title.col="black")
mtext("Miscoverage Probability vs Sample Size",outer=TRUE,cex=1.33,font=2,line=-2)
mtext("Part 1 Item b",outer=TRUE,line=-3.25,cex=1.2)

graphics.off()



for(k in seq_along(N))
{
  ContamCover=matrix(0,5,3)
  
  for(i in seq_along(q))
  {
    load(paste("Out/Part 2/p.out",q[i],"RData",sep="."))
    
    for(j in seq_along(meth))
    {
      ContamCover[i,j]=p.out[j,k]
    }
  }
  
  require("xtable")
  nDigits = 4
  
  Coverage=1-ContamCover
  Coverage=t(Coverage)
  
  dimnames(Coverage)=list(meth,paste("q=",q,sep=""))
  
  sink(paste("Tables/Coverage",N[k],"tex",sep="."))
  text=paste("Coverage Probabilities Sample Size = ",N[k])
  #cat("\\section*{Coverage Probabilities}\n")
  
  Coverage.tbl = xtable(Coverage)
  digits(Coverage.tbl) = nDigits
  cat(paste("\n\n",text,"\n\n"))
  print(Coverage.tbl,floating=FALSE)
  cat("\\vspace{0.2in}\n")
  
  sink()
  
  p.cl=get(load(paste("Out/Part 2/p.clean","RData",sep=".")))

  #png(paste(paste("Graphics/ Part 2_",N[k],sep=""), "png",sep="."),width=1600,height=800,res=120)
  
  png(paste(paste("Graphics/ Miscoverage and Width_",N[k],sep=""), "png",sep="."),width=1600,height=800,res=120)
  
  par(mfrow=c(1,2))
  
  plot(c(0,q)*100,c(p.cl[1,k],ContamCover[,1]),xaxt="none",type="n",pch=meths.pch[1],ylim=c(0,1),ylab="Miscoverage Probability",
       xlab="Mixing %")
  axis(1, c(0,q)*100)
    
    
    for(m in seq_along(meth))
    {
      lines(c(0,q)*100,c(p.cl[m,k],ContamCover[,m]),type="p",pch=meths.pch[m])
      lines(c(0,q)*100,c(p.cl[m,k],ContamCover[,m]),type="c",col=cols[m],lwd=2)
      
    }
  
  abline(h=alpha,col="red",lwd=2)
  abline(h=0,col="blue",lwd=2)
  
  legend(max(q*100),0.8,legend=meth,pch=meths.pch,xjust=1,yjust=1,cex=.9,text.col=cols,title="Methods",title.col="black")
  #mtext("Miscoverage Probability vs Mixing %",outer=TRUE,cex=1.33,font=2,line=-2)
  #mtext(paste("Sample size N = ",N[k],sep=""),outer=TRUE,line=-3.25,cex=1.2)
  
  mtext("Miscoverage Probability and Interval Boundaries vs Mixing %",
        outer=TRUE,cex=1.25,font=2,line=-2)
  mtext(paste("Sample size N = ",N[k],sep=""),outer=TRUE,side=1,line=-2.2)
  
  
  

pointtype=seq(nmeths)

colw=c("red","green","blue")


WL=WU=matrix(0,3,5)
for(i in seq_along(q))
{
  WUL=get(load(paste("Out/Part 2/ContamUL",meth[3],q[i],"Rdata",sep=".")))
  WL[,i]=WUL[[k]][,1]
  WU[,i]=WUL[[k]][,2]
  
}

CleanUL=get(load(paste("Out/Part 2/CleanUL",meth[3],"Rdata",sep=".")))

CleanL=CleanUL[[k]][,1]
CleanU=CleanUL[[k]][,2]

Width.Clean.Contam.L=cbind(CleanL,WL)
Width.Clean.Contam.U=cbind(CleanU,WU)


#mtext("Interval Width vs Mixing %",outer=TRUE,cex=1.33,font=2,line=-2)
#mtext(paste("Sample size N = ",N[k],sep=""),outer=TRUE,line=-3.25,cex=1.2)

plot(c(0,q)*100,Width.Clean.Contam.L[1,],xaxt="none",type="n",pch=pointtype[3],ylim=range(c(Width.Clean.Contam.L,Width.Clean.Contam.U)),ylab="Interval Boundaries",
     xlab="Mixing %")

axis(1, c(0,q)*100)

legend(40,max(range(c(Width.Clean.Contam.L,Width.Clean.Contam.U))),legend=meth,pch=pointtype,title="Methods",title.col="black",xjust=1,yjust=1,cex=.9)
legend(50,max(range(c(Width.Clean.Contam.L,Width.Clean.Contam.U))),legend=c("p1-p2","p1-p3","p2-p3"),text.col=colw,title="Differences",title.col="black",xjust=1,yjust=1,cex=.9)

for(it in seq(3))
{
  lines(c(0,q)*100,Width.Clean.Contam.L[it,],type="p",pch=pointtype[3])
  lines(c(0,q)*100,Width.Clean.Contam.L[it,],type="c",col=colw[it])
  lines(c(0,q)*100,Width.Clean.Contam.U[it,],type="p",pch=pointtype[3])
  lines(c(0,q)*100,Width.Clean.Contam.U[it,],type="c",col=colw[it])
}






 for( wi in seq(1,nmeths-1))
 {

  #png(paste(paste("Graphics/ Contam.Width_",N[k],sep=""), "png",sep="."),width=1600,height=800,res=120)
  
  WL=WU=matrix(0,3,5)
  for(i in seq_along(q))
  {
    WUL=get(load(paste("Out/Part 2/ContamUL",meth[wi],q[i],"Rdata",sep=".")))
    WL[,i]=WUL[[k]][,1]
    WU[,i]=WUL[[k]][,2]
    
    
  }
  
  CleanUL=get(load(paste("Out/Part 2/CleanUL",meth[wi],"Rdata",sep=".")))
  
  CleanL=CleanUL[[k]][,1]
  CleanU=CleanUL[[k]][,2]
  
  Width.Clean.Contam.L=cbind(CleanL,WL)
  Width.Clean.Contam.U=cbind(CleanU,WU)
  
  
  #mtext("Interval Width vs Mixing %",outer=TRUE,cex=1.33,font=2,line=-2)
  #mtext(paste("Sample size N = ",N[k],sep=""),outer=TRUE,line=-3.25,cex=1.2)
  
  for(it in seq(3))
  {
    lines(c(0,q)*100,Width.Clean.Contam.L[it,],type="p",pch=pointtype[wi])
    lines(c(0,q)*100,Width.Clean.Contam.L[it,],type="c",col=colw[it])
    lines(c(0,q)*100,Width.Clean.Contam.U[it,],type="p",pch=pointtype[wi])
    lines(c(0,q)*100,Width.Clean.Contam.U[it,],type="c",col=colw[it])
  }
  
  
}  
   
  
  
  graphics.off()
  
  
  
}






  
    
  

