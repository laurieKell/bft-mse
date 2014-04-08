library(FLAdapt)
library(kobe)
library(ggplotFL)
library(reshape)
library(LaF)

dirBft="/home/laurie/Desktop/gcode/gbyp-sam/data/ICCAT/VPA/BFT/east"
dirTex=paste(dirBft,"tex",sep="/")
dirDat=paste(dirBft,"data",sep="/")
dirRet=paste(dirBft,"VPA/Retros",sep="/")
dirPrj=paste(dirBft,"VPA/Projections/k2sm_V2",sep="/")

scn=expand.grid(Selectivity   =c("2010","2012"),
                Catch         =c("Inflated","Reported"),
                Recruitment   =c("low","med","high"),
                stringsAsFactors=FALSE)

TAC=c(seq(0,30000,2000),c(12900,13500))
