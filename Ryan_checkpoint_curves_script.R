
#rm(list=ls())
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
#load the script for resampling functions
source("R:/Ellen/WHOLE skull analyses/pts/resamplingsV3.R")

#curvedata must be a csv with columns 'curves' 'lm1' 'lm2' and 'ptswanted'
#If you get Unicode UTF-8 BOM (ï..) then use curvedata<-read.csv('new curves.csv',fileEncoding="UTF-8-BOM")
curvedata<-read_csv("R:/Ellen/WHOLE skull analyses/new curves.csv")

#set the working directory
setwd("R:/Ellen/WHOLE skull analyses/pts")

#list which landmarks are fixed and will not slide

fixed <- c(1:123)
#load in the list of pts files from the current working directory
ptslist <- dir(pattern='.pts', recursive=F)  
pts_tibble <- as_tibble()#initialize an empty tibble to store the pts data
filenames <- ptslist
ntaxa <- length(filenames) #define the number of taxa based on how many pts files are in the working directory

#define the curves using the 'curvedata' object
subsampled.curve<-sapply(paste("SC",c(1:nrow(curvedata)),sep=""),function(x) NULL)
subsampled.curve[[1]]<-c(curvedata$lm1[1],((length(fixed)+1):(length(fixed)+curvedata$ptswanted[1])),curvedata$lm2[1])
for (i in 2:length(subsampled.curve)){
  subsampled.curve[[i]]<-c(curvedata$lm1[i],((max(unlist(subsampled.curve))+1):(max(unlist(subsampled.curve))+curvedata$ptswanted[i])),curvedata$lm2[i])
}

subsampled.curve.in<-subsampled.curve
for(i in 1:length(subsampled.curve.in)){
  subsampled.curve.in[[i]]<-tail(head(subsampled.curve[[i]],-1),-1)
}


slidings.sub<-c((max(fixed)+1):max(unlist(subsampled.curve)))

## IMPORT DATA ##
{
  for(i in 1:length(ptslist))
  {
    specimen.tmp <- as_tibble(read.table(file=ptslist[i],skip=2,header=F,sep="")) #import a single specimen
    specimen.tmp <- specimen.tmp %>% mutate(., V1 = as.character(V1)) #convert the first row, the lm names, to characters instead of factors
    specimen.tmp <- specimen.tmp %>% mutate(.,spec.id=filenames[i]) #add a column with the specimen name
    pts_tibble<-bind_rows(pts_tibble,specimen.tmp) #paste it to the end of the tibble with the rest of the specimens 
  }
  #this will give a warning message but its nothing to worry about
  pts_tibble <- pts_tibble %>% separate(., V1, into = c("class", "id","sub_lm"), remove = FALSE)
  
  pts_tibble <- pts_tibble %>% mutate(., id = as.factor(id)) %>%
    rename(., index = V1, X = V2, Y = V3, Z = V4) #rename the coordinate data columns
  print("SERIOUSLY DONT WORRY ABOUT THE WARNINGS BELOW")
}

#make a list of how many sliding semilandmarks on curves there are
curvepoints <- sum(curvedata$ptswanted)
#convert the tibble to a 3D array compatable with geomorph
pts_tibble_tmp <- pts_tibble%>%filter(.,class=="S")%>%group_by(spec.id)%>%select(.,X,Y,Z)%>%nest()%>%transpose()
ptsarray_tmp <- array(dim=c(length(fixed),3,ntaxa))

for(i in 1:length(pts_tibble_tmp))
{
  ptsarray_tmp[,,i] <- as.matrix(select(pts_tibble_tmp[[i]]$data, c(X,Y,Z)))
}



##############################################################
####making all the curves have the correct number of landmarks
##############################################################
#make an empty array with the correct number of landmarks and specimens 
newpts <- array(data = NA, dim = c(length(fixed),3,ntaxa))
#give it dimension names based on your specimen list
dimnames(newpts)[3] <- list(substr(filenames,1,(nchar(filenames)-4)))
#fill in the fixed landmarks 
newpts[fixed,,] <- ptsarray_tmp

#OPTIONAL:
#check for outliers in your anatomical landmarks before subsampling
#find.outliers(newpts)



for (which.curve in 1:nrow(curvedata)){
  this.curve <- array(data=NA, dim=c(curvedata$ptswanted[which.curve],3,ntaxa))
  for (which.spec in 1:length(filenames)){
    orig.curve <- pts_tibble %>% filter(.,spec.id==filenames[which.spec])%>%filter(., class=="C")%>%filter(., id==which.curve) %>% select(., X,Y,Z)
    orig.curve.anchors <- pts_tibble %>% filter(.,spec.id==filenames[which.spec])%>%slice(c(subsampled.curve[[which.curve]][1],last(subsampled.curve[[which.curve]]))) %>% select(., X,Y,Z)
    orig.curve <- rbind(orig.curve.anchors[1,],orig.curve,orig.curve.anchors[2,])
    new.curve <- cursub.interpo(orig.curve, curvedata$ptswanted[which.curve])
    #this bit checks if you had ANY 9999s in this curve before subsampling.
    if(9999 %in% (orig.curve %>% pull(X))){ 
      #If TRUE, fill those values back in with 9999, just in case the code accidentilly subsampled a missing curve, making nonsense variables
      new.curve[c(2:dim(new.curve)[1]-1),]<-9999
    }
    this.curve[,,which.spec] <- as.matrix(new.curve)[2:(dim(new.curve)[1]-1),]
  }
  newpts <- abind::abind(newpts, this.curve, along=1)
}

#which.curve

#class(as.integer(sprintf('%0.3d', 1:480)))
#class(which.curve)

#which.curve2=sprintf('%0.3d', 1:480)
#which.curve2[1]

### trial for one spec one curve- can ignore ### 
#which.curve=2
#this.curve <- array(data=NA, dim=c(curvedata$ptswanted[which.curve],3,ntaxa))

#which.spec=1

#orig.curve <- pts_tibble %>% filter(.,spec.id==filenames[which.spec])%>%filter(., class=="C")%>%filter(., id==which.curve) %>% select(., X,Y,Z)
#orig.curve.anchors <- pts_tibble %>% filter(.,spec.id==filenames[which.spec])%>%slice(c(subsampled.curve[[which.curve]][1],last(subsampled.curve[[which.curve]]))) %>% select(., X,Y,Z)
#orig.curve <- rbind(orig.curve.anchors[1,],orig.curve,orig.curve.anchors[2,])
#new.curve <- cursub.interpo(orig.curve, curvedata$ptswanted[which.curve])
#this.curve[,,which.spec] <- as.matrix(new.curve)[2:(dim(new.curve)[1]-1),]
#newpts <- abind::abind(newpts, this.curve, along=1)




#########################################
#                                       #
#    CHECK WHICH CURVES ARE WRONG       #
#                                       #
#########################################


check_curves <- function(tibble1){
  uniques <- tibble1 %>% group_by(spec.id) %>% filter(class == "C") %>% summarise(Unique_Elements = n_distinct(id))
  max_curves <- max(uniques$Unique_Elements)
  too_few <- which(uniques$Unique_Elements != max_curves)
  if (length(too_few)==0){
    writeLines("You have no missing curves! Great job!") 
  }
  else{
    for (i in 1:length(too_few)){
      name <- uniques$spec.id[too_few[i]]
      present <- pts_tibble %>% group_by(spec.id) %>% filter(class == "C") %>% filter(spec.id == name) %>% distinct(id) %>% pull(id) %>% as.vector()
      absent <- setdiff(as.character(1:max_curves), present)
      writeLines(paste(name, "is missing curve number", absent))
    }
  }
}


#which.curve


#class(as.integer(sprintf('%0.3d', 1:480)))
#class(which.curve)

#which.curve2=sprintf('%0.3d', 1:480)
#which.curve2[1]


##############################
#                            #  
#      MISSING DATA          #
#                            #
##############################


subsampled.lm <- newpts 

subsampled.lm[which(subsampled.lm==9999)]<-NA
subsampled.lm[which(is.nan(subsampled.lm))]<-NA
subsampled.lm[which(subsampled.lm==-Inf)]<-NA

geomorph::estimate.missing(subsampled.lm,method="TPS")

subsampled.lm2<-abind(subsampled.lm[,,2],subsampled.lm)
newnewpts<-estimate.missing(subsampled.lm2)

#Remove the extra specimen added by subsampled 
ptsbatch <- newnewpts[,,-c(1)]

spheres3d(ptsbatch[,,156], radius = 2)

#ptsbatch without RHS 
ptsbatchLHS <- ptsbatch[-c(67:123),,]                         
save(ptsbatchLHS, file="F:/PTS FINAL LHS/ptsbatchLHS.R")
                                               
save(ptsbatch, file="F:/PTS FINAL LHS/ptsbatch.R")


#orig.curve <- pts_tibble %>% filter(.,spec.id==filenames[which.spec])%>%filter(., class=="C")%>%filter(., id==which.curve) %>% select(., X,Y,Z)
#orig.curve.anchors <- pts_tibble %>% filter(.,spec.id==filenames[which.spec])%>%slice(c(subsampled.curve[[which.curve]][1],last(subsampled.curve[[which.curve]]))) %>% select(., X,Y,Z)
#orig.curve <- rbind(orig.curve.anchors[1,],orig.curve,orig.curve.anchors[2,])
#new.curve <- cursub.interpo(orig.curve, curvedata$ptswanted[which.curve])
#this.curve[,,which.spec] <- as.matrix(new.curve)[2:(dim(new.curve)[1]-1),]
#newpts <- abind::abind(newpts, this.curve, along=1)


#This is for checking the lengths of files to see if they are the same 
files <- list.files( path = "R:/Ellen/WHOLE skull analyses/pts/pts", pattern = ".pts" )

for ( i in seq(1:length(files)) ){
  cat( "Lines for file ", files[i], "are: ", "\n" )
  cat( length(readLines( paste( files[i], sep = "" )) ), "\n" )
}


#Checking what goes wrong 
#Load single pts 
file=as.matrix(read.table(file="F:/PTS FINAL LHS/missing data test/Agorophiid USNM 205491.pts",skip=2,header=F,sep="",row.names=1))
#Show numbers on the landmarks (this is landmarks 1:123)
text3d(ptsbatch1[c(1:123),,2], text=1:123)
#This is how the data look before estimate.missing 
subsampled.lm[,,2] # should have NAs for missing data 
#This is how it looks after estimate.missing 
ptsbatch1[,,2]
