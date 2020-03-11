
#Make sure your tree and your data names are in the same order: 
shapedata <- shapedata[,,treeSVP$tip.label]

#designate the regions 
region.names=c("nasal","premax","maxilla","jugal","frontal","parietal","zygomatic","squamosal","mandibular process","supraoccipital",
               "occipital condyle","basioccipital","basisphenoid","palate","pterygoid")


cbbPalette=c("palegreen", "#D55E00", "#6A3D9A", "#0072B2", "#A6CEE3", "#000000",
             "#E69F00", "#696969", "#009E73", "#CC79A7", "#00EEEE", "limegreen", "#FF1493", "#F0E442", "#B2DF8A")


####### RATES AND DISPARITY PER LM #######
source("./code from lab/utilitiesfromgeomorph.r")
right.array<-two.d.array(shapedata) ## shape data
x<-right.array
phy<-treeSVP
phy.parts <- phylo.mat(x, phy)
invC <- phy.parts$invC
D.mat <- phy.parts$D.mat
C = phy.parts$C
global<-sig.calc(x,invC,D.mat,Subset=TRUE)
#global.array1<-arrayspecs(global$R,p=757,k=3)
rates.vector<-colSums(matrix(diag(global$R), nrow=3))
module.id.1<-bones15

variances<-rowSums(apply(shapedata,c(1,2),var))
#variances<-rowSums(apply(Y.gpa.rhs[,,-1],c(1,2),var)) # for no_atreto
per.lm.rates<-cbind(rates.vector,module.id.1, variances)

#simulate:
library(geiger)
cov.mat<-matrix(0,nrow=length(diag(global$R)),ncol=length(diag(global$R)))
diag(cov.mat)<-diag(global$R)
iter=100
k=3
system.time(
  simdat <- sim.char(phy=phy,par=cov.mat, nsim=iter,model="BM")
)
expectedmat<-matrix(nrow=(dim(simdat)[2]/k),ncol=iter)
for (i in 1:iter){
  temp<-arrayspecs(simdat[,,i],p=1021,k=3) #number of landmakrs and curves + dimensions 
  expected<-rowSums(apply(temp,c(1,2),var))
  expectedmat[,i]<-expected
}
colnames(expectedmat)<-paste(c(1:iter))
library(tidyverse)
library(reshape2)
simulatedvariance<-tbl_df(cbind(rates.vector,expectedmat))
simvarclean<-gather(simulatedvariance, "simnumber", "variance", 2:(iter+1))
# Create prediction interval
m1<-lm(variance~rates.vector, data=simvarclean)
newx <- seq(min(simvarclean$rates.vector), max(simvarclean$rates.vector), length.out = 100)
pred_interval <- predict(m1, newdata=data.frame(rates.vector=newx), interval="prediction", level = 0.95)
pred_interval <- as.data.frame(pred_interval)
pred_interval$rates = newx
ratesandvars<-as.data.frame(per.lm.rates)

ratesandvars$module.id.1<-factor(ratesandvars$module.id.1,levels=c(1:15),labels=region.names) #designate the module ids
ratesandvars$bones<-factor(ratesandvars$module.id.1,levels=c(1:15),labels=region.names) #desginate the bone names 
#colour=cbbPalette_modules_vo_grey

colour=cbbPalette

p <-ggplot(ratesandvars, aes(x=rates.vector, y=variances, color=bones))
p<- p + geom_point(size=2) + labs(x="Evolutionary rate",y="Disparity")
p= p+ stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ x,colour="blue")
p= p+scale_color_manual(values = colour)
p=p+theme(panel.background = element_blank())
p=p + theme(axis.line = element_line(colour = "black"))
p= p +  stat_smooth(method="lm", se=TRUE, fill="grey", formula=y ~ x,colour="red")+
  stat_smooth(mapping=aes(x=rates.vector,y=variance),se=TRUE,data=simvarclean,method=lm,
              fill="grey", formula=y ~ x, inherit.aes = FALSE)+geom_ribbon(mapping=aes(x=rates, ymin = lwr, ymax = upr),
                                                                           data=pred_interval, fill = "blue",
                                                                           alpha = 0.1,inherit.aes = FALSE)

p
