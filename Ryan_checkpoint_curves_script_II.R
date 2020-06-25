

rm(list=ls())
library(tidyverse)
library(Morpho)
library(geomorph)
library(Rvcg)
library(paleomorph)
library(EMMLi)
library(qgraph)
library(ape)
library(geiger)
library(abind)
library("devtools")
devtools::install_github("rnfelice/SURGE")

install.packages("remotes")
remotes::install_github("rnfelice/SURGE")

library(SURGE)
library(RColorBrewer) # for color palettes
library(magick)

source("./checkLM.mod.R")

#Set path to PTS final LHS - this is where all of the curves excels etc are 

#import table defining curves
curve_table <- read_csv('new curves_44_61.csv')

#identify the folder where your pts files are, INCLUDING TEMPLATE
ptsfolder <- "./Raw_Data/pts"

#import the pts file names
ptslist <- dir(ptsfolder, pattern='.pts', recursive=F)

my_curves <- create_curve_info(curve_table, n_fixed = 66)
setwd(ptsfolder)
subsampled.lm <- import_chkpt_data(ptslist, my_curves, subsampl = TRUE, verbose=TRUE)

#RESET WD TO PROJECT DIRECTORY
setwd("C:\\Users\\Anjali Goswami\\Documents\\GitHub\\Eutheria")

#if you have any missing points, Checkpoint will set them to x,y,z=9999
#this makes for bad plots in checkLM, so switch them to NA
subsampled.lm[subsampled.lm == 9999] <- NA

#check to make sure your curves look okay on each specimen
checkLM(subsampled.lm,begin=1,pt.size = .2,path="./Raw_Data/ply/",suffix=".ply",render="s")

#FILL MISSING DATA (do after patching, before sliding)
#subsampled.lm.2 <- fixLMtps(subsampled.lm, comp = 3)
#newpts <- subsampled.lm.2$out #this is the resulting landmark configuration

# SEPARATION OF TEMPLATE AND SPECIMENS
newpts <- subsampled.lm[,,-which(dimnames(subsampled.lm)[[3]]=="template")]
template.lm <- subsampled.lm[,,which(dimnames(subsampled.lm)[[3]]=="template")]

save(newpts, file="./Data/subsampled_data.R")
#load("./Data/subsampled_data.R")



##########Adding new or fixed specimens
#import table defining curves
curve_table <- read_csv("./Raw_Data/placental_curves.csv")

#identify the folder where your pts files are, INCLUDING TEMPLATE
ptsfolder <- "./Test/pts"

#import the pts file names
ptslist <- dir(ptsfolder, pattern='.pts', recursive=F)
my_curves <- create_curve_info(curve_table, n_fixed = 66)
setwd(ptsfolder)
fixed.lm <- import_chkpt_data(ptslist, my_curves, subsampl = TRUE, verbose=TRUE)


#RESET WD TO PROJECT DIRECTORY
setwd("C:\\Users\\Anjali Goswami\\Documents\\GitHub\\Eutheria")

#if you have any missing points, Checkpoint will set them to x,y,z=9999
#this makes for bad plots in checkLM, so switch them to NA
fixed.lm[fixed.lm == 9999] <- NA

#check to make sure your curves look okay on each specimen
checkLM(fixed.lm,begin=13,pt.size = 1,path="./Test/ply/",suffix=".ply",render="s")

#replace bad specimens in newpts
newpts[,,141]<- hyracodon.lm

ictops.lm<-fixed.lm[,,8]
newpts[,,144]<-ictops.lm



#check LM again
checkLM(newpts,atlas=atlas,begin=1,pt.size = .5,path="./Raw_Data/ply/",suffix=".ply",render="s")


#OPTIONAL
#define which curves belong to which modules for a pretty plot
curvemodules<-c()
for (i in 1:nrow(curve_table)) {
x<-rep(curve_table$Bone[i],curve_table$ptswanted[i])
curvemodules<-c(curvemodules,x)
}

curvemodules<-c(rep(0,length(my_curves$Fixed)),curvemodules)
curvemodules<-as.factor(curvemodules)
levels(curvemodules)<- c("black", "#6a3d9a","dimgrey","#fb9a99",  "gold", "#009E73",  "#D55E00", "#CC79A7", "cyan2",  "#e31a1c", "#0072B2", "#b2df8a", "#E69F00",  "whitesmoke" ,  "deeppink",   "#a6cee3",   "#F0E442","blue","red","brown")


#import the mesh of specimen 1 for visualizing
mesh1<-vcgImport("./Raw_Data/ply/Acinonyx_jubatus.ply")

#open a 3d window with two viewing frames
open3d()
mfrow3d(nr=1,nc=2)

#plot the landmarks and curves on the specimen
shade3d(mesh1,col="white")
spheres3d(newpts[my_curves$Fixed,,1],col="red")
spheres3d(newpts[my_curves$Sliding.LMs,,1],col="gold")


#plot the landmarks and curves colored by module
next3d()
shade3d(mesh1,col="white")
spheres3d(newpts[,,1],col=curvemodules)

#export list of taxa 
write.csv(as.matrix(dimnames(newpts)[[3]]),file="./Raw_Data/mytaxonomytable.csv",quote=FALSE)


#change names to match names in trees
taxonomy <- read.csv("xxxx")
for (i in 1:nrow(taxonomy)){
 dimnames(slid)[[3]][which(dimnames(slid)[[3]]==taxonomy$Filename[i])]<-taxonomy$Tip_label[i]
}


#plot the landmarks and curves colored by module
module.ids<-read_csv("./Raw_Data/module.color.table.csv")
mod_colors<-module.ids
module_colors<-as.factor(mod_colors$Color)
color.palette <- c( "#6a3d9a","dimgrey","#fb9a99",  "gold", "#009E73",  "#D55E00", "#CC79A7", "cyan2",  "#e31a1c", "#0072B2", "#b2df8a", "#E69F00",  "whitesmoke" ,  "deeppink",   "#a6cee3",   "#F0E442","blue","red","brown", "black")
levels(module_colors)<-color.palette


open3d()
shade3d(mesh.acinonyx,col="#E4D1C0")
spheres3d(patched_all[,,1],col=module_colors,radius=.5)
rgl.snapshot("./Figures/acinonyx_modules.png")


#Creating 'missing' landmarks to be able to slide
misslist<-createMissingList(dim(newpts)[3])
for (j in 1:dim(newpts)[[3]]){
  misslist[[j]]<-which(is.na(newpts[,1,j]))
} 
 newpts2<-fixLMtps(newpts)
 {
   slided4.all <- slider3d(newpts2$out, SMvector= SMvector1,
                                     outlines = outlines1, surp =surp1,
                                     sur.path = "./ply", sur.name = NULL, 
                                     meshlist = paste("./ply/",dimnames(all.specs3$out)[[3]],".ply",sep=""), ignore = NULL,
                                     sur.type = "ply", tol = 1e-10, deselect = FALSE, inc.check = FALSE,
                                     recursive = TRUE, iterations = 3, initproc = TRUE,
                                     pairedLM = 0, mc.cores = 1, bending=TRUE,
                                     fixRepro = FALSE,stepsize=0.2,
                           missingList=misslist)
   dimnames(slided4.all[["dataslide"]])[3]<-dimnames(newpts2$out)[3]
   #save(slided4.all,file="~/Google Drive/NHM/crocs/data/slid.crocs.all.apr25.R")
   #load("~/Google Drive/NHM/crocs/data/slid.crocs.sept23.R")
 }


#########################################################
#                                                       #
#                 SLIDING LANDMARKS                     #
#                                                       #
# #######################################################                                                 

slided4.all <- slider3d(newpts2$out, SMvector= my_curves$Sliding.LMs,
                                     outlines = my_curves$Curves, sur.path = "./ply", sur.name = NULL, 
                                     meshlist = paste("./ply/",dimnames(newpts2$out)[[3]],".ply",sep=""), ignore = NULL,
                                     sur.type = "ply", tol = 1e-10, deselect = FALSE, inc.check = FALSE,
                                     recursive = TRUE, iterations = 3, initproc = TRUE,
                                     pairedLM = 0, mc.cores = 1, bending=TRUE,
                                     fixRepro = FALSE,stepsize=0.2,
                           missingList=misslist)



#re-estimate missing post sliding
slided4.all$dataslide[which(is.na(newpts))]<-NA
slid.lms<-fixLMtps(slided4.all$dataslide)


#THEN MIRROR 
#THEN PROCRUSTES



                            
