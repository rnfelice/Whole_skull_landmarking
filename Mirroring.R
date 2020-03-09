library(paleomorph)


###################################
#                                 #
#      MIRRORING LANDMARKS        #
#                                 #
###################################

########## SYMETRISATION TO IMPROVE THE SHAPE ANALYSES #########################
#Ellen's code 
#DATA SHOULD NOT BE PROCRUSTED 
#Data are ptsbatch 
#None of the lms are mirrored - just the curve data 

################################### SWAP REAL LHS LMS FOR MIRRORED ONES FOR PROCRUSTES
ActualLMs=ptsbatch[1:123,,] ## 53 landmarks, but 3 of these are midline, so total LMs will be 53+50
ActualLMsmatrix=as.matrix(ActualLMs)
midline<-as.integer(c(38,40,48,49,51,54,55,56,61))
left.lm <- c(1:37,39,41:47,50,52,53,57:60,62:66)
right.lm <- c(67:123)
bilat.landmarks <- cbind(left.lm,right.lm)
ActualLMs[bilat.landmarks[,2],,] <- NA
MirroredLMs=mirrorfill(A=ActualLMs,  l1=midline, l2=bilat.landmarks)
MirroredLMs
texts3d(MirroredLMs[,,1], texts=c(1:123))
specimens=abind::abind(MirroredLMs, ptsbatch[-c(1:123),,], along = 1)
dim(specimens)
###################################################################################################
######################## NOW MIRROR ALL CURVES- HAVENT REMOVED ANY CURVES YET.. #########
#fixed<-as.integer(c(1:123))
#subsampling=read.csv("E:/LOOK HERE/Caecilians/Caecilian Patching/Caecilian resampling curves.csv", header=FALSE)
#rec1=subsampling$V2
#rec1=rec1T1[-c(16,17,19,21,22,42,45)] ##made this in T1 code, =resampled curve lengths
#curve<-list(SC1,SC2,SC3,SC4,SC5,SC6,SC7,SC8,SC9,SC10,SC11,SC12,SC13,SC14,SC15,SC16,SC17,SC18,SC19,SC20,
            #SC21,SC22,SC23,SC24,SC25,SC26,SC27,SC28,SC29,SC30,SC31,SC32,SC33,SC34,SC35,SC36,SC37,SC38,
            #SC39,SC40,SC41,SC42,SC43,SC44,SC45,SC46,SC47,SC48,SC49,SC50,SC51,SC52,SC53,SC54,SC55,SC56,SC57) ##maybe do 51 curves not 57  if relevant
#slidings.sub<-c((tail(fixed,1)+1):(tail(fixed,1)+sum(rec1)))
#subsampled.curve<-list()
#subsampled.curve.in<-list()
#curve.last<-tail(fixed,1)
#j=0
#for (i in 1:length(curve)){
  #first<-curve[[i]][1]
  #last<-tail(curve[[i]],1)
  #curve.first<-(curve.last+1)
  #curve.last<-curve.first+rec1[i]-1
  #S.curve<-c(first,curve.first:curve.last,last)
  #S.curve.in<-c(curve.first:curve.last)
  #subsampled.curve[[i]]<-S.curve
  #subsampled.curve.in[[i]]<-S.curve.in
  #j=j+curve.last
#}


left.curves<-c(1:64)
#left.curves<-c(2:34, 36:44, 46:47, 49:51, 53:64)
left.curve.list<-unlist(subsampled.curve.in[left.curves])
leftside<-c(left.lm,left.curve.list) # RHS LMs+ RHS curves+all patch points
num.missing<-(length(leftside)-length(right.lm)) # number of LMs to create= total RHS-current LHS LMs
blanks<-c((dim(ptsbatch)[1]+1):(dim(ptsbatch)[1]+num.missing))
# to fill in blanks from one row past the last current point, for the number of rows needed (num.missing)
rightside<-c(right.lm,blanks)
add_col_or_row = function(x, n = 1, add_col = T, fill = 0)
{
  m1 = matrix(x, ncol = if(add_col) nrow(x) * ncol(x) else nrow(x), byrow = T)
  m2 = matrix(fill, nrow = if(add_col) dim(x)[3] else prod(dim(x)[-1]),
              ncol = if(add_col) nrow(x) * n else n)
  array(t(cbind(m1, m2)),
        c(nrow(x) + ((!add_col) * n), ncol(x) + (add_col * n), dim(x)[3]))
}
specimens2<-add_col_or_row(ptsbatch,n=num.missing,add_col=FALSE,fill=NA)
dimnames(specimens2)[3]<-dimnames(ptsbatch)[3]
bilats<-cbind(leftside,rightside)
newarray<-mirrorfill(specimens2,l1=midline,l2=bilats)
dimnames(newarray)[3]<-dimnames(ptsbatch)[3]

spheres3d(newarray[c(67:123),,1], radius=2, color="red")
spheres3d(newarray[c(1079:2033),,1], radius=2)
dim(newarray)

              
###################################
#                                 #
#      Procrustes the data        #
#                                 #
#                                 #
###################################


Y.gpa=gpagen(newarray) #Remove non-shape aspects 
data=Y.gpa$coords #Subset out the coords 
size=Y.gpa$Csize

shapedata=data[c(1:66, 124:1078),,]

#This is procrustes so the radius is tiny 
spheres3d(shapedata[,,44], radius = 0.0001)

#define what the RHS data are 
shapedataRHS=data[c(67:123, 1079:2033),,]

spheres3d(shapedataRHS[,,44], radius = 0.0001, color = "red")
text3d(shapedataRHS[,,4], text=1:dim(mirrordata)[1])


col=rainbow(length(1:dim(shapedataRHS)[1]))
shapes3d(shapedataRHS[,,4], joinline=c(1:dim(shapedataRHS)[1]), color=col)

#Check the new shape data alignment 
spheres3d(shapedata[,,1], radius = 0.0001)
spheres3d(shapedata[,,2], radius = 0.0001)
spheres3d(shapedata[,,3], radius = 0.0001)
spheres3d(shapedata[,,4], radius = 0.0001)

#Have a look at the original array 
spheres3d(ptsbatch[,,1], radius = 2)
spheres3d(ptsbatch[,,2], radius = 2)
spheres3d(ptsbatch[,,3], radius = 2)
spheres3d(ptsbatch[,,4], radius = 2)

#data 
col=rainbow(length(1:dim(data)[1]))
spheres3d(data[,,1], color=col, radius = 0.001)


spheres3d(newarray[,,1], color=col, radius = 1)
              
#How does the morphospace look? Exactly like with landmarks only 
#Play around with colours/groups later (i.e., add species data) 
PCA=plotTangentSpace(shapedata, axis1=1, axis2=2)
              
              
              
