library(FLAdapt)
library(plyr)

dirK2SM="/home/laurie/Desktop/Dropbox/ICCAT/SCRS/BFT/2012/VPA/Projections/k2sm_V2"
         
scn=expand.grid(Selectivity   =c("2010","2012"),
                Catch         =c("Inflated","Reported"),
                Recruitment   =c("low","med","high"),stringsAsFactors=TRUE)

drs=paste(dirK2SM,apply(scn,1,paste,collapse="/"),sep="/")

  m_ply(drs,function(x) {
                          setwd(x)
                          
                          file.copy("/home/laurie/Desktop/Dropbox/ICCAT/SCRS/BFT/2012/VPA/Projections/k2sm_V2/quotas.txt","quotas.txt",overwrite=T)
                          file.copy(system.file("bin", "linux", package="FLAdapt", "pro-2box", mustWork=TRUE),"pro-2box",overwrite=T)
                          
                          system("./pro-2box")})


m_ply(scn, function(Catch,Recruitment,dir){
                from=paste(dirK,Catch,            "sel.txt",sep="/")
                to  =paste(dirK,Catch,Recruitment,"sel.txt",sep="/")
                
                file.copy(from,to,overwrite=T)
                file.copy("/home/laurie/Desktop/flr/pkg/FLAdapt/inst/bin/linux/pro-2box",
                          paste(dirK,Catch,Recruitment,"pro-2box",sep="/"))
                
                file.copy(paste(dirK,Catch,Recruitment,"Prj12.ctl",sep="/"),
                          paste(dirK,Catch,Recruitment,"proj.ctl", sep="/"))
                
                },dir=dirK)

m_ply(scn, function(Catch,Recruitment,Year,dir=""){
  from="/home/laurie/Desktop/Dropbox/ICCAT/SCRS/BFT/2012/VPA/Projections/quotas.txt"
  to  =paste(dir,Year,Catch,Recruitment,"quotas.txt", sep="/")
  print(to)
  file.copy(from,to,overwrite=TRUE)})


m_ply(scn, function(Catch,Recruitment,dir){
  
  setwd(paste(dirK,Catch,Recruitment,sep="/"))
  
  system("./pro-2box.exe")
  
  },dir=dirK)






