library(FLAdapt)
library(kobe)
library(ggplotFL)
library(reshape)
library(LaF)

dirPrj="/home/laurie/Desktop/gcode/gbyp-sam/data/ICCAT/VPA/BFT/east"

scn=expand.grid(Selectivity   =c("2010","2012")[2],
                Catch         =c("Inflated","Reported"),
                Recruitment   =c("low","med","high"),
                stringsAsFactors=FALSE)

TAC=c(seq(0,30000,2000),c(12900,13500))

dirs=paste(dirPrj,adply(scn,1,paste,collapse="/")[,4],sep="/")

testOut =mdply(paste(dirPrj,adply(scn,1,paste,collapse="/")[,4],sep="/")[1], readPro2box,type="out")
testRef =mdply(paste(dirPrj,adply(scn,1,paste,collapse="/")[,4],sep="/")[1], readPro2box,type="ref")
testKobe=kobe2box(dirs,what="trks",proxy="f0.1")
tK      =transform(testKobe,TAC=TAC[tac],Year=as.numeric(as.character(year))+1949)
tK      =cbind(tK,scn[tK$X1,])[,c("stock","harvest","TAC","Year","Selectivity","Catch","Recruitment")]
                     
#### Kobe Phase Plot #############################################################################################################
kobe2012=mdply(dirs,kobe2box,proxy="f0.1",what="sims")

kobe2012.=transform(cbind(scn[kobe2012$X1,],kobe2012[,-1]),
                     year       =as.numeric(ac(kobe2012$year))+1949, 
                     Recruitment=factor(kobe2012$Recruitment, labels=c("High","Low","Medium")),
                     Selectivity=factor(kobe2012$Selectivity, labels=c("Selectivity from 2012")),
                     TAC        =TAC[kobe2012$tac])
#save(kobe2012,file=paste(dirDat,"kobe2012.RData",sep="/"))

## kobe phase plots
kobe2012Mn=ddply(subset(kobe2012,year %in% 2008:2011 & TAC==12900), .(Catch,Recruitment,Selectivity,year,TAC), 
                 function(x) data.frame(ssb=median(x$ssb,na.rm=T),harvest=median(x$harvest,na.rm=T)))

kobe2012Mn$Recruitment=c("High","Low","Medium")[as.numeric(kobe2012Mn$Recruitment)]
kobe2012$Recruitment=c("High","Low","Medium")[as.numeric(kobe2012$Recruitment)]
kobe2012Mn$Recruitment=factor(kobe2012Mn$Recruitment,levels=c("Low","Medium","High"))
kobe2012$Recruitment=factor(kobe2012$Recruitment,levels=c("Low","Medium","High"))
kobeP2012=kobe(kobe2012Mn) + 
  facet_grid(Catch~Selectivity) +
  geom_point(aes(ssb,harvest,col=Recruitment),data=subset(kobe2012,year==2011 & tac==1),size=1) +
  scale_y_continuous(limits=c(0,2.0)) +
  geom_path(aes(ssb,harvest,group=Recruitment,col=Recruitment),size=1.1) +
  geom_point(aes(ssb,harvest,col=Recruitment),data=subset(kobe2012Mn,year %in% 2011 & TAC==12900),size=4,col="black") +
  theme_ms(10)
save(kobeP2012,file=paste(dirDat,"kobeP2012.RData",sep="/"))

ggsave(kobeP2012, height=7, width=6, file=paste(dirTex,"/6_1_9.png",sep="/"))
##################################################################################################################################

## Pies ##########################################################################################################################
kobe2012=transform(kobe2012.,ssb=stock.,harvest=harvest.)
k2012=subset(kobe2012,year %in% c(2011) & tac==1, select=c(Catch,Recruitment,Selectivity,year,ssb,harvest))
k2012=data.frame(k2012,kobeP(k2012$ssb,k2012$harvest))
k2012=melt(k2012[,c("Catch","Recruitment","Selectivity","year","red","green","yellow")], id.vars=c("Catch","Recruitment","Selectivity","year"))
pie2012 <- ggplot(subset(k2012,value>0), aes(x = factor(1), fill = factor(variable))) +  geom_bar(width = 1) + coord_polar(theta = "y") +
  labs(fill='Kobe Quadrant') + xlab('') + ylab('') +
  scale_fill_manual(values=c("red","green","yellow")) + 
  facet_grid(Catch~Selectivity) + 
  theme_ms(10)+
  scale_x_discrete(breaks=NULL)+
  scale_y_continuous(breaks=NULL)
#ggsave(pie2012, height=6, width=6,  file=paste(dirTex,"/7_1_2.png",sep="/"))

k2012.=subset(kobe2012,year %in% 2010:2022 & TAC==12900, select=c(Selectivity,year,ssb,harvest))
k2012.$Selectivity=c("S10","S12")[as.numeric(k2012.$Selectivity)]
k2012.=data.frame(k2012.,kobeP(k2012.$ssb,k2012.$harvest))
k2012.=melt(k2012.[,c("Selectivity","year","red","green","yellow")], id.vars=c("Selectivity","year"))
pie2012 <- ggplot(subset(k2012.,value>0), aes(x = factor(1), fill = factor(variable))) +  geom_bar(width = 1) + coord_polar(theta = "y") +
  xlab('') + ylab('') +
  labs(fill='Kobe Quadrant') + scale_fill_manual(values=c("red","green","yellow")) + 
  facet_grid(Selectivity~year) + 
  theme_ms(8)+
  scale_x_discrete(breaks=NULL)+
  scale_y_continuous(breaks=NULL)
#ggsave(pie2012, height=3, width=8,  file=paste(dirTex,"/7_1_3.png",sep="/"))

#### 2012 ##################################################################################################
load("/home/laurie/Desktop/Dropbox/ICCAT/SCRS/2012/bfte/data/kobe2012.RData")

kobe2012=transform(subset(kobe2012.,year %in% 2013:2022),stock=stock., harvest=harvest.)

t.=ddply(kobe2012,.(year,TAC), function(x) kobeSmry(x[,c("stock","harvest")]))

1-cast(subset(t., year %in% 2013:2022),TAC~year,value="overFishing")
1-cast(subset(t., year %in% 2013:2022),TAC~year,value="overFished")
  cast(subset(t., year %in% 2013:2022),TAC~year,value="green")

k2sm2012$year=as.numeric(as.character(k2sm2012$year))+1949
k2sm2012.=transform(subset(k2sm2012, (year %in% 2013:2022), # & !(TAC %in% c(12900, 13500)), 
                          select=c(year,TAC,green,overFished,overFishing)),
                   TAC =as.numeric(as.character(TAC)),
                   Year=as.numeric(as.character(year)))
k2smTab=k2sm(k2sm2012)

library(tables)
latex(kobeShade(k2smTab[[1]],pct=""),file=paste(dirTex,"k2smF.tex",sep="/"), rowlabel="TAC",rowname=dimnames(k2smTab[[1]])$TAC,caption="Kobe II Strategy Matrix, $P(F\\leq F_{MSY})$.")
latex(kobeShade(k2smTab[[2]],pct=""),file=paste(dirTex,"k2smB.tex",sep="/"), rowlabel="TAC",rowname=dimnames(k2smTab[[1]])$TAC,caption="Kobe II Strategy Matrix, $P(SSB\\geq B_{MSY})$).")
latex(kobeShade(k2smTab[[3]],pct=""),file=paste(dirTex,"k2sm.tex", sep="/"), rowlabel="TAC",rowname=dimnames(k2smTab[[1]])$TAC,caption="Kobe II Strategy Matrix, $P(F\\leq F_{MSY})$ and $P(SSB\\geq B_{MSY})$.")

#### Rate of increase ######################################################################
incr=ddply(subset(tK,Year %in% 2009:2022), .(TAC,Year), function(x) data.frame(stock=median(x$stock)))
incr=merge(incr,transform(incr,Year=Year+1),by=c("TAC","Year"))
incr=transform(incr,r=as.integer(100*(stock.x-stock.y)/stock.y))[,c(1:2,5)]
cast(incr,TAC~Year,value="r")


