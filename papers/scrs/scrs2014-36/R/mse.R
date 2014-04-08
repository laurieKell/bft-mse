library(FLCore)
library(FLBRP)
library(biodyn)

dirMy  ="/home/laurie/Desktop/scrs2014/scrs2014-bft-mp"
dirKobe="/home/laurie/Desktop/ICCAT/SCRS/kobe/Inputs"

dirDat=paste(dirMy,"data",sep="/")
dirInp=paste(dirKobe,"/bfte/2012/vpa",sep="")

scn=expand.grid(Catch         =c("inflated","reported"),
                Recruitment   =c("low","med","high")[2],
                stringsAsFactors=FALSE)

load(paste(dirDat, "OMs.RData",  sep="/"))
load(paste(dirDat, "BRPs.RData", sep="/"))             
load(paste(dirDat, "srDev.RData",sep="/"))     

mse  =FLStocks()

brp  =BRPs[[2]]
om   =OMs[[2]]
srDev=srDevs[[3]]

om   =fwdWindow(om,end=2020,brp)
om   =fwd(om,catch=FLQuant(13900*1000,dimnames=list(year=2012:2020)),sr=brp,sr.residuals=srDev)

mse[["1"]]=mseEmpirical1(om,brp,srDev,start=2020,end=2040,k1=1.5,k2=3,gamma=1)$om

plot(mse[[1]])+scale_x_continuous(limits=c(2000,2030))

yrAdult=1950:1980
yrJuve =1990:2000

mse[["2"]]=mseEmpirical2(om,brp,srDev,yrAdult,yrJuve,start=2020,end=2040,tarCatch=30000*1000)$om

plot(window(mse,start=2010))+scale_x_continuous(limits=c(2010,2040))