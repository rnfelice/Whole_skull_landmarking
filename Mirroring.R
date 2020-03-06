

###################################
#                                 #
#      MIRRORING LANDMARKS        #
#                                 #
###################################

########## SYMETRISATION TO IMPROVE THE SHAPE ANALYSES #########################
#Ellen's code 
#DATA SHOULD NOT BE PROCRUSTED 

###################################################################################################
##########################

#Data comes from read in and estimate.missing added data 

###################################################################################################
##########################

#Data comes from read in and estimate.missing added data 

library(paleomorph)
#Remove the RHS lms if using the ptsbatch data 
mirrordata <- ptsbatch[-c(67:123),,]

#check with a plot to see the lms have gone 
spheres3d(mirrordata[,,1], radius = 2)

#This allows us to see the numbered landmarks 
text3d(ptsbatch[,,3], text=1:dim(mirrordata)[1])


#Define left lm 
left.lm <- c(1:37,39,41:47,50,52,53,57:60,62:66) 
midline<-as.integer(c(38,40,48,49,51,54,55,56,61))

#Define fixed left hand curves 
left.curves<-c(1:64) 
left.curve.list<-unlist(subsampled.curve.in[left.curves])  ##subsampled.curve.in from T1
leftside<-c(left.lm,left.curve.list) # RHS LMs+ RHS curves+all patch points
num.missing<-(length(leftside)) # number of LMs to create= total RHS-current LHS LMs

blanks<-c((dim(mirrordata)[1]+1):(dim(mirrordata)[1]+num.missing)) 
# to fill in blanks from one row past the last current point, for the number of rows needed (num.missing)
rightside<-c(blanks)

add_col_or_row = function(x, n = 1, add_col = T, fill = 0)
{
  m1 = matrix(x, ncol = if(add_col) nrow(x) * ncol(x) else nrow(x), byrow = T)
  m2 = matrix(fill, nrow = if(add_col) dim(x)[3] else prod(dim(x)[-1]),
              ncol = if(add_col) nrow(x) * n else n)
  array(t(cbind(m1, m2)),
        c(nrow(x) + ((!add_col) * n), ncol(x) + (add_col * n), dim(x)[3]))
}


specimens2<-add_col_or_row(mirrordata,n=num.missing,add_col=FALSE,fill=NA)
dimnames(specimens2)[3]<-dimnames(mirrordata)[3]

#Bind the left and right side together 
bilats<-cbind(leftside, rightside)
#This has all the mirrored data 
Mirrored_data<-mirrorfill(specimens2,l1=midline,l2=bilats)
dimnames(Mirrored_data)[3]<-dimnames(mirrordata)[3]


spheres3d(Mirrored_data[,,16], radius = 1, color = "black")
spheres3d(Mirrored_data[-c(1021:1958),,16], radius = 10, color = "red")  #1976 

col=rainbow(length(1:dim(Mirrored_data)[1]))
shapes3d(Mirrored_data[,,15], color=col)

save(Mirrored_data, file="F:/PTS FINAL LHS/Mirrored_data.R")


###################################
#                                 #
#      Procrustes the data        #
#                                 #
#                                 #
###################################


Y.gpa=gpagen(Mirrored_data) #Remove non-shape aspects 
data=Y.gpa$coords #Subset out the coords 
size=Y.gpa$Csize

shapedata=data[c(1:1021),,]

spheres3d(shapedata[,,16], radius = 0.0001)

shapedataRHS=data[-c(1:1021),,]

spheres3d(shapedataRHS[,,16], radius = 0.0001)
col=rainbow(length(1:dim(shapedataRHS)[1]))
shapes3d(shapedataRHS[,,1], joinline=c(1:dim(shapedataRHS)[1]), color=col)



#data 
col=rainbow(length(1:dim(data)[1]))
spheres3d(data[,,1], color=col, radius = 0.001)


spheres3d(newarray[,,1], color=col, radius = 1)
