

####### RATES AND DISPARITY PER LM #######
source("./code from lab/utilitiesfromgeomorph.r")
right.array<-two.d.array(Y.gpa.rhs_173) ## shape data
x<-right.array
phy<-tree_173
phy.parts <- phylo.mat(x, phy)
invC <- phy.parts$invC
D.mat <- phy.parts$D.mat
C = phy.parts$C
global<-sig.calc(x,invC,D.mat,Subset=TRUE)

#global.array1<-arrayspecs(global$R,p=757,k=3)
rates.vector<-colSums(matrix(diag(global$R), nrow=3))
module.id.19<-M19
module.id.13<-M13
module.id.15<-M15
variances<-rowSums(apply(Y.gpa.rhs_173,c(1,2),var))

#variances<-rowSums(apply(Y.gpa.rhs[,,-1],c(1,2),var)) # for no_atreto
per.lm.rates<-cbind(rates.vector,module.id.19,module.id.13,module.id.15, variances)

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
  temp<-arrayspecs(simdat[,,i],p=995,k=3)
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
ratesandvars$modules19<-factor(ratesandvars$module.id.19,levels=c(1:19),labels=region.names)
ratesandvars$Modules<-factor(ratesandvars$module.id.13,levels=c(1:13),labels=module.names)
ratesandvars$bones<-factor(ratesandvars$module.id.15,levels=c(1:15),labels=bone.names)

#colour=cbbPalette_vo_grey
#colour=cbbPalette_modules_vo_grey
colour=cbbPalette_15_bones_vo_grey
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
