library(FLAdapt)

scn=expand.grid(Selectivity   =c("2010","2012")[2],
                Catch         =c("Inflated","Reported"),
                Recruitment   =c("low","med","high"),
                stringsAsFactors=FALSE)

drs=paste(dirPrj,adply(scn,1,paste,collapse="/")[,4],sep="/")
smry=mdply(drs, function(x) as.data.frame(apply(readPro2box(x,type="ref",data.frame=F)["f0.1",c("ssb","harvest","yield")],1:2,median),drop=T))

cast(cbind(scn[smry$X1,],smry[,-1]),quantity+Recruitment~Catch,value="data")


