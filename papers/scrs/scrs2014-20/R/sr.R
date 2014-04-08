#### set up paths and scenarios etc.
## FLR packages
#install.packages(repos="flr", c("FLCore","ggplotFL","FLash","kobe","FLBRP","diags"))

library(FLCore)
library(FLBRP)
library(kobe)
library(diags)
library(plyr)

## Dirs for paper 
paste.dir=function(dir,file) paste(dir,file,sep="/")
dirKobe="/home/laurie/Desktop/ICCAT/SCRS/kobe/Inputs"
dirMy  ="/home/laurie/Desktop/ICCAT/SCRS/2014/bft"

dirDat =paste.dir(dirMy,"data")
dirInp =paste.dir(dirKobe,"bfte/vpa/2012")
scen=c("reported","inflated")

source('~/Desktop/flr/git/FLR3.0/FLCore/R/io.VPA2Box.R')

##Get Data
stks=FLStocks(mlply(scen, function(scen,dir,m)
                         readVPA2Box(paste(dir,"bfte/2012/vpa",scen,"med/bfte2012.c1",sep="/"),m=m),
    dir=dirKobe,
    m  =c(0.49, 0.24, 0.24, 0.24, 0.24, 0.20,0.175,0.15, 0.125,0.10)))
      
names(stks)=scen
scen=expand.grid(catch=scen,model=c("bevholt","ricker","geomean"),stringsAsFactors=FALSE)

srrs=FLSRs(mlply(scen,function(catch,model,stks){ 
                        if (model!="geomean")
                            res=fmle(as.FLSR(iter(stks[[catch]],1),model=model))
                        else
                            res=fmle(as.FLSR(iter(stks[[catch]],1),model="bevholt"),fixed=list(b=0))
                        plot(res)
                        res},
      stks=stks))

brs=FLBRPs(mlply(seq(length(srrs)),
              function(x,stks=stks,srrs=srrs) brp(FLBRP(iter(stks[[scen[x,"catch"]]],1),srrs[[x]])),
      stks=stks,srrs=srrs))
