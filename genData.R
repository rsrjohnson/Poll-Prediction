load("Data/Parms.RData")
set.seed(2021)
#Part 1
for(i in N)
{
  M=rmultinom(nrep,i,p_pt1a)/i
  
  save(M,file=paste("Data/M.a",i,"RData",sep="."))
  
}


for(i in N)
{
  M=rmultinom(nrep,i,p_pt1b)/i
  
  save(M,file=paste("Data/M.b",i,"RData",sep="."))
  
}


#Part 2
for(j in q)
{
  for(i in N)
  {
    nq=rbinom(nrep,i,j) #this is for the proportions in r since q does not vote
    np=i-nq # this is for the proportions in p, (1-q)
    
    Vc=V
    NVc=NV
    for (k in seq(nrep))
    {
      
      Vc[,k]=rmultinom(1,np[k],p_pt2)
      NVc[,k]=rmultinom(1,nq[k],r)
      
    }
    M=(Vc+NVc)/i
    save(M,file=paste("Data/M",i,j,"RData",sep="."))
    
  }
  
  
}



#Part 2 Clean

for(i in N)
{
  M=rmultinom(nrep,i,p_pt2)/i
  
  save(M,file=paste("Data/M.2.Clean",i,"RData",sep="."))
  
}