#source after genData.r
load("Data/Parms.RData")

set.seed(2021)

pa.out = pa.se = pb.out = pb.se =matrix(0,length(meth),length(N))

#CI=matrix(0,length(p_pt1a),nrep)
#row.names(CI)=c("p1-p2","p1-p3","p2-p3")


#Part 1

for(i in seq_along(meth))
{
  me=meth[i]
  methfunct = get(paste("CI.",me,sep=""))
  
  for(j in seq_along(N))
  {
    Nsamp=N[j]
    
    X=get(load(paste("Data/M.a",Nsamp,"Rdata",sep=".")))
    
    p12=methfunct(X,alpha,Nsamp,p_pt1a,1,2)
    p13=methfunct(X,alpha,Nsamp,p_pt1a,1,3)
    p23=methfunct(X,alpha,Nsamp,p_pt1a,2,3)
    
    save(p12,file=paste("Out/Part 1/CIa",paste(me,"p1-p2",sep = "_"),Nsamp,"RData",sep="."))
    save(p13,file=paste("Out/Part 1/CIa",paste(me,"p1-p3",sep = "_"),Nsamp,"RData",sep="."))
    save(p23,file=paste("Out/Part 1/CIa",paste(me,"p2-p3",sep = "_"),Nsamp,"RData",sep="."))
   
    CIs=CI.out.se(p12$CI.out,p13$CI.out,p23$CI.out)
    save(CIs,file=paste("Out/Part 1/CIa_mix",Nsamp,"Rdata",sep="."))
    pa.out[i,j]=CIs$p.out
    #pa.se[i,j]=CIs$p.se
    
    
    
    Y=get(load(paste("Data/M.b",Nsamp,"RData",sep=".")))
    
    p12=methfunct(Y,alpha,Nsamp,p_pt1b,1,2)
    p13=methfunct(Y,alpha,Nsamp,p_pt1b,1,3)
    p23=methfunct(Y,alpha,Nsamp,p_pt1b,2,3)
    
    save(p12,file=paste("Out/Part 1/CIb",paste(me,"p1-p2",sep = "_"),Nsamp,"RData",sep="."))
    save(p13,file=paste("Out/Part 1/CIb",paste(me,"p1-p3",sep = "_"),Nsamp,"RData",sep="."))
    save(p23,file=paste("Out/Part 1/CIb",paste(me,"p2-p3",sep = "_"),Nsamp,"RData",sep="."))
    
    CIs=CI.out.se(p12$CI.out,p13$CI.out,p23$CI.out)
    pb.out[i,j]=CIs$p.out
    #pb.se[i,j]=CIs$p.se
    
  }
  
}


dimnames(pa.out) = dimnames(pa.se)=dimnames(pb.out)=dimnames(pb.se)=list(meth,paste("N=",N,sep=""))


save(pa.out,file=paste("Out/Part 1/pa.out","RData",sep="."))
save(pb.out,file=paste("Out/Part 1/pb.out","RData",sep="."))
save(pa.se,file=paste("Out/Part 1/pa.se","RData",sep="."))
save(pb.se,file=paste("Out/Part 1/pb.se","RData",sep="."))

require("xtable")
nDigits = 4 #only used here for xtable

sink("Tables/Part1.tex") #redirect the output to specific file
cat("\\section*{Part 1}\n")

#pa.tbl = xtable(pa.out)
pa.tbl = xtable(1-pa.out)
digits(pa.tbl) = nDigits
#cat("\n\nMiscoverage probabilities Item a)\n\n")
cat("\n\nCoverage probabilities Item a)\n\n")
print(pa.tbl,floating=FALSE)
cat("\\vspace{0.2in}\n")

# pa.tbl = xtable(pa.se)
# digits(pa.tbl) = nDigits
# cat("\n\nStandard error of miscoverage probabilities a)\n\n")
# print(pa.tbl,floating=FALSE)
# cat("\\vspace{0.2in}\n")

#pb.tbl = xtable(pb.out)
pb.tbl = xtable(1-pb.out)
digits(pb.tbl) = nDigits
#cat("\n\nMiscoverage probabilities Item b)\n\n")
cat("\n\nCoverage probabilities Item b)\n\n")
print(pb.tbl,floating=FALSE)
cat("\\vspace{0.2in}\n")

# pb.tbl = xtable(pb.se)
# digits(pb.tbl) = nDigits
# cat("\n\nStandard error of miscoverage probabilities Item b)\n\n")
# print(pb.tbl,floating=FALSE)

sink()


###############################
#Part 2


for(k in q)
{
  p.out= p.se = matrix(0,length(meth),length(N))
  
  for(i in seq_along(meth))
  {
    me=meth[i]
    methfunct = get(paste("CI.",me,sep=""))
    #contam.width=matrix(0,3,length(N))
    MW=c()
    for(j in seq_along(N))
    {
      
      Nsamp=N[j]
      
      Z=get(load(paste("Data/M",Nsamp,k,"RData",sep=".")))
      
      p12=methfunct(Z,alpha,Nsamp,p_pt2,1,2)
      p13=methfunct(Z,alpha,Nsamp,p_pt2,1,3)
      p23=methfunct(Z,alpha,Nsamp,p_pt2,2,3)
      
      save(p12,file=paste("Out/Part 2/CI",paste(me,"p1-p2",sep = "_"),Nsamp,k,"RData",sep="."))
      save(p13,file=paste("Out/Part 2/CI",paste(me,"p1-p3",sep = "_"),Nsamp,k,"RData",sep="."))
      save(p23,file=paste("Out/Part 2/CI",paste(me,"p2-p3",sep = "_"),Nsamp,k,"RData",sep="."))
      
      
      ContamL=c(mean(p12$ConfInt[,1]),mean(p13$ConfInt[,1]),mean(p23$ConfInt[,1]))
      ContamU=c(mean(p12$ConfInt[,2]),mean(p13$ConfInt[,2]),mean(p23$ConfInt[,2]))
      MW[[j]]=cbind(ContamL,ContamU)
      
      
      #contam.width[,j]=c(p12$Conf.width,p13$Conf.width,p23$Conf.width)
      
      
      CIs=CI.out.se(p12$CI.out,p13$CI.out,p23$CI.out)
      
      p.out[i,j]=CIs$p.out
      #p.se[i,j]=CIs$p.se
      
      
    }
    save(MW,file=paste("Out/Part 2/ContamUL",me,k,"Rdata",sep="."))
    #save(contam.width,file=paste("Out/Part 2/Width_Contam",me,k,"Rdata",sep="."))
  }
  
  
  dimnames(p.out) = dimnames(p.se) = list(meth,paste("N=",N,sep=""))
  
  save(p.out,file=paste("Out/Part 2/p.out",k,"RData",sep="."))
  save(p.se,file=paste("Out/Part 2/p.se",k,"RData",sep="."))
  
  require("xtable")
  nDigits = 4 #only used here for xtable
  
  sink(paste("Tables/Part2",k,"tex",sep=".")) #redirect the output to specific file
  cat(paste("\\section*{Part 2 q = ",k,"}\n",sep=""))
  
  p.out.tbl = xtable(p.out)
  digits(p.out.tbl) = nDigits
  cat("\n\nMiscoverage probabilities\n\n")
  print(p.out.tbl,floating=FALSE)
  cat("\\vspace{0.2in}\n")
  
  # p.se.tbl = xtable(p.se)
  # digits(p.se.tbl) = nDigits
  # cat("\n\nStandard error of miscoverage probabilities\n\n")
  # print(p.se.tbl,floating=FALSE)
  # cat("\\vspace{0.2in}\n")
  sink()
  
}





############### Part 2 Clean

p.clean=matrix(0,length(meth),length(N))



for(i in seq_along(meth))
{
  me=meth[i]
  methfunct = get(paste("CI.",me,sep=""))
  #clean.width=matrix(0,3,length(N))
  MW=c()
  
  for(j in seq_along(N))
  {
    Nsamp=N[j]
    
    X=get(load(paste("Data/M.2.Clean",Nsamp,"Rdata",sep=".")))
    
    p12=methfunct(X,alpha,Nsamp,p_pt2,1,2)
    p13=methfunct(X,alpha,Nsamp,p_pt2,1,3)
    p23=methfunct(X,alpha,Nsamp,p_pt2,2,3)
    
    save(p12,file=paste("Out/Part 2/CI.clean",paste(me,"p1-p2",sep = "_"),Nsamp,"RData",sep="."))
    save(p13,file=paste("Out/Part 2/CI.clean",paste(me,"p1-p3",sep = "_"),Nsamp,"RData",sep="."))
    save(p23,file=paste("Out/Part 2/CI.clean",paste(me,"p2-p3",sep = "_"),Nsamp,"RData",sep="."))
    
    #clean.width[,j]=c(p12$Conf.width,p13$Conf.width,p23$Conf.width)
    
    CleanL=c(mean(p12$ConfInt[,1]),mean(p13$ConfInt[,1]),mean(p23$ConfInt[,1]))
    CleanU=c(mean(p12$ConfInt[,2]),mean(p13$ConfInt[,2]),mean(p23$ConfInt[,2]))
    MW[[j]]=cbind(ContamL,ContamU)
    
    CIs=CI.out.se(p12$CI.out,p13$CI.out,p23$CI.out)
    #save(CIs,file=paste("Out/Part 1/CIa_mix",Nsamp,"Rdata",sep="."))
    p.clean[i,j]=CIs$p.out
    
  }
  #save(clean.width,file=paste("Out/Part 2/Width_Clean",meth[i],"Rdata",sep="."))
  save(MW,file=paste("Out/Part 2/CleanUL",me,"Rdata",sep="."))
}

save(p.clean,file=paste("Out/Part 2/p.clean","RData",sep="."))


