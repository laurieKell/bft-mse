library(FLCore)
library(FLBRP)
library(biodyn)

#dirInp="/home/laurie/Desktop/ICCAT/SCRS/kobe/Inputs/bfte/2012/vpa" 
dirInp="http://rscloud.iccat.int/kobe/bfte/2012/vpa"
dirMy ="/home/laurie/Desktop/scrs2014/scrs2014-36"

scn=expand.grid(Catch         =c("inflated","reported"),
                Recruitment   =c("low","med","high")[2],
                stringsAsFactors=FALSE)

m  =c(0.49, 0.24, 0.24, 0.24, 0.24, 0.20,0.175,0.15, 0.125,0.10)
fls=mdply(scn,function(Catch,Recruitment) paste(dirInp,Catch,Recruitment,"bfte2012.c1",sep="/"))[,3]
OMs=FLStocks(mlply(fls,readVPA2Box,m=m))

names(OMs)=unlist(scn[1])

## Reference Points**
BRPs=FLBRPs(llply(OMs,FLBRP))
names(BRPs)=names(OMs)

## Create Recruitment Deviates
## Scenarios
ops=cbind(expand.grid(catch=c("inflated","reported"),
                      min  =c(1950,1950,1991)),
          max  =rep(c(1985,2010,2010),each=2))

## Get deviates
dev=FLQuants(mlply(ops,function(catch,min,max,OM) 
  rec(OM[[catch]])[,ac(min:max)],
  OM=OMs))

## Get random samples
srDevs=FLQuants(llply(dev,function(x) 
  FLQuant(sample(c(x),prod(unlist(dim(x))),TRUE),
          dimnames=list(age=1,year=2010:2050,iter=1:500))))

attributes(srDevs)$scn=ops

## set up 6 scenarios
OMs=FLStocks(mlply(1:6, function(i){
                    scn=rep(c("inflated","reported"),each=3)
                    om =OMs[[scn[i]]]
                    brp=BRPs[[scn[i]]]
                    
                    om=fwdWindow(om,end=2020,brp)
                    om=fwd(om,catch=FLQuant(13900*1000,dimnames=list(year=2012:2020)),sr=brp,sr.residuals=srDevs[[i]])
                    om}))

#### MSEs #################################################### 
library(doParallel)
library(foreach)

## set parallel stuff
cl=makeCluster(4)
registerDoParallel(cl)

hcr1=foreach(i=1:6, 
            .combine     =FLStocks,
            .multicombine=TRUE,
            .maxcombine  =8,
            .packages    =c("biodyn","FLash","FLBRP")) %dopar% {
       
              mseSBT1(OMs[[i]],
                            BRPs[[rep(c("inflated","reported"),each=3)[i]]],
                            srDevs[[i]],
                            start=2020,end=2040,
                            k1=1.5,k2=3,gamma=1)}

yrAdult=1950:1980
yrJuve =1990:2000

hcr2=foreach(i=1:6, 
             .combine     =FLStocks,
             .multicombine=TRUE,
             .maxcombine  =8,
             .packages    =c("biodyn","FLash","FLBRP")) %dopar% {
               
               mseSBT2(OMs[[i]],
                       BRPs[[rep(c("inflated","reported"),each=3)[i]]],
                       srDevs[[i]],
                       yrAdult,yrJuve,
                       start=2020,end=2040,
                       tarCatch=30000*1000)}

names(hcr1)=1:6
names(hcr2)=1:6

save(hcr1,file=paste(dirMy,"/data/hcr1.RData",sep=""))
save(hcr2,file=paste(dirMy,"/data/hcr2.RData",sep=""))

res=rbind(cbind(HCR=factor(1),ldply(hcr1,function(x) as.data.frame(x[["catch","fbar","rec","ssb","stock"]],drop=T))),
          cbind(HCR=factor(2),ldply(hcr2,function(x) as.data.frame(x[["catch","fbar","rec","ssb","stock"]],drop=T))))
res=transform(res,Quantity=factor(qname,labels=c("Yield","Fishing Mortality","Recruits","SSB","Biomass"),
                                        levels=c("Yield","Fishing Mortality","Recruits","SSB","Biomass")))

names(res)[2:3]=c("Scenario","Year")
qtl=ddply(res,.(HCR,Scenario,Quantity,Year), with, quantile(data,prob=c(.25,.5,.75)))
its=subset(res, iter %in% c(12,244,56,323,195,444))

names(qtl)[5:7]=c("q25","q50","q75")

p1=ggplot(subset(qtl,Quantity!="Biomass"))+
  geom_ribbon(aes(Year,y=q50,ymin=q25,ymax=q75,fill=HCR,group=HCR),alpha=.4)+
  geom_line(  aes(Year,y=q50,col=HCR,group=HCR))+
  facet_grid(Quantity~Scenario,scale="free")+
  scale_x_continuous(limits=c(2010,2040))+theme_ms(6,legend.position="none")+ylab("")
ggsave(filename=paste(dirMy,"/tex/ts.png",sep=""),p1,width=8,height=4,units="in")

p2=ggplot(subset(qtl,Quantity=="SSB"))+
  geom_ribbon(aes(Year,y=q50,ymin=q25,ymax=q75,fill=HCR,group=HCR),alpha=.4)+
  geom_line(aes(Year,y=q50,col=HCR,group=HCR))+
  geom_line(aes(Year,data,col=iter,group=iter),data=subset(its,Quantity=="SSB"))+  
  facet_grid(Scenario~HCR,scale="free")+
  scale_x_continuous(limits=c(2010,2040))+theme_ms(8,legend.position="none")+ylab("")
ggsave(filename=paste(dirMy,"/tex/iS.png",sep=""),p2,width=6,height=8,units="in")

p3=ggplot(subset(qtl,Quantity=="Biomass"))+
  geom_ribbon(aes(Year,y=q50,ymin=q25,ymax=q75,fill=HCR,group=HCR),alpha=.4)+
  geom_line(aes(Year,y=q50,col=HCR,group=HCR))+
  geom_line(aes(Year,data,col=iter,group=iter),data=subset(its,Quantity=="Biomass"))+  
  facet_grid(Scenario~HCR,scale="free")+
  scale_x_continuous(limits=c(2010,2040))+theme_ms(8,legend.position="none")+ylab("")
ggsave(filename=paste(dirMy,"/tex/iB.png",sep=""),p3,width=6,height=8,units="in")

p4=ggplot(subset(qtl,Quantity=="Recruits"))+
  geom_ribbon(aes(Year,y=q50,ymin=q25,ymax=q75,fill=HCR,group=HCR),alpha=.4)+
  geom_line(aes(Year,y=q50,col=HCR,group=HCR))+
  geom_point(aes(Year,data,col=iter,group=iter),data=subset(its,Quantity=="Recruits"),size=1.0)+  
  geom_line( aes(Year,data,col=iter,group=iter),data=subset(its,Quantity=="Recruits"),size=.1)+  
  facet_grid(Scenario~HCR,scale="free")+
  scale_x_continuous(limits=c(2010,2040))+theme_ms(8,legend.position="none")+ylab("")
ggsave(filename=paste(dirMy,"/tex/iR.png",sep=""),p4,width=6,height=8,units="in")

p5=ggplot(subset(qtl,Quantity=="Yield"))+
  geom_ribbon(aes(Year,y=q50,ymin=q25,ymax=q75,fill=HCR,group=HCR),alpha=.4)+
  geom_line(aes(Year,y=q50,col=HCR,group=HCR))+
  geom_line(aes(Year,data,col=iter,group=iter),data=subset(its,Quantity=="Yield"))+  
  facet_grid(Scenario~HCR,scale="free")+
  scale_x_continuous(limits=c(2010,2040))+theme_ms(8,legend.position="none")+ylab("")
ggsave(filename=paste(dirMy,"/tex/iY.png",sep=""),p5,width=6,height=8,units="in")

p6=ggplot(subset(qtl,Quantity=="Fishing Mortality"))+
  geom_ribbon(aes(Year,y=q50,ymin=q25,ymax=q75,fill=HCR,group=HCR),alpha=.4)+
  geom_line(aes(Year,y=q50,col=HCR,group=HCR))+
  geom_line(aes(Year,data,col=iter,group=iter),data=subset(its,Quantity=="Fishing Mortality"))+  
  facet_grid(Scenario~HCR,scale="free")+
  scale_x_continuous(limits=c(2010,2040))+theme_ms(8,legend.position="none")+ylab("")
ggsave(filename=paste(dirMy,"/tex/iF.png",sep=""),p6,width=6,height=8,units="in")
      