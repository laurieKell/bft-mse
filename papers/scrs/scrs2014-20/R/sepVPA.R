library(FLAssess)

om     =OM[["reported"]]
vpa    =om
m(vpa)[]=.2
vpa    =VPA(vpa, fit.plusgroup=TRUE)

plot(harvest(om)[9]/harvest(om)[10])
plot(harvest(vpa)[9]/harvest(vpa)[10])

plot(apply(stock.n(om)*stock.wt(om),2,sum), type="l")
plot(apply(stock.n(vpa)*stock.wt(om),2,sum),type="l")
