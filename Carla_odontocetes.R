#This code mirrors all curves and landmarks and then replaces the mirrored LHS curves with the asymmetric data 
#The remaining curves are asumed to be symmetric 
#This is why they might not entirely line up with the landmarks

#Use ptsbatch as before 
#define the variables and check how they look

#midline landmarks 
midline.lm<-c(38,40,48,49,51,54,55,56,61)
#midline curves 
midline.curves <- c(629:648,764:783,824:838,884:903)
#Extra landmarks placed on the RHS of odontocetes 
lmextra <- c(67:79,83:86,120:123)
#Extra curves placed on the RHS of odontocetes 
curveextra <- c(1084:1408) #1084 is curve #65 

#Remove corresponding LHS landmarks and curves 
corressLHS <- c(1:15,19:24) #LMs 16-18 are the jugal which I'm ignoring
corresscurves <- c(124:448) #curves #i.e. curve 1-21 (inlcusive) 

#check the midline 
spheres3d(ptsbatch[lmextra,,3],radius=2) 
#check the midline 
spheres3d(ptsbatch[midline.curves,,3], radius = 2, color = 'red') 
#check the additional RHS landmarks and RHS curves
##Check corressponding landmarks and curves on LHS (original) 
spheres3d(ptsbatch[lmextra,,3],radius=2, color = 'red') 
spheres3d(ptsbatch[curveextra,,3],radius=2, color = 'black') 
spheres3d(ptsbatch[corressLHS,,3],radius=2, color ='red')
spheres3d(ptsbatch[corresscurves,,3],radius=2, color = 'green') 

################################
#                              #
#       LANDMARKS ONLY         #
#                              #
################################

#Landmarks only - note the midline 
#NOTE THE MIDLINE
ActualLMs=ptsbatch[1:123,,] ##landmarks like normal
ActualLMsmatrix=as.matrix(ActualLMs)
midline<-as.integer(c(38,40,48,49,51,54,55,56,61)) 
left.lm <- c(16,17,18,25:37,39,41:47,50,52,53,57:60,62:66)
right.lm <- c(80,81,82,87:119)
bilat.landmarks <- cbind(left.lm,right.lm)
ActualLMs[bilat.landmarks[,2],,] <- NA
MirroredLMs=mirrorfill(A=ActualLMs,  l1=midline, l2=bilat.landmarks)
MirroredLMs
texts3d(MirroredLMs[,,4], texts=c(1:123))
specimens=abind::abind(MirroredLMs, ptsbatch[-c(1:123),,], along = 1)
dim(specimens)
################
spheres3d(MirroredLMs[,,3], radius = 2)


###############################################
#                                             #
#                                             #
#    ADD NEW MIDLINE WITH MIDLINE CURVES      # 
#                                             #
#                                             #
###############################################

C_35=unlist(subsampled.curve.in[35])
C_45=unlist(subsampled.curve.in[45])
C_48=unlist(subsampled.curve.in[48])
C_52=unlist(subsampled.curve.in[52])

curves_combined=c(C_35,C_45,C_48,C_52)
#spheres3d(curves_combined[,,1], radius=2, colour = 'red')

midline<-as.integer(c(38,40,48,49,51,54,55,56,61,curves_combined))

curveextra <- c(1084:1408) #remove the extra curves so we are juts mirroring like normal 
ptsbatch_left=ptsbatch[-curveextra,,]

#Now work with ptsbatch_left 

left.curves<-c(1:34,36:44,46:47,49:51,53:64)
#left.curves<-c(2:34, 36:44, 46:47, 49:51, 53:64)
left.curve.list<-unlist(subsampled.curve.in[left.curves])
leftside<-c(left.lm,left.curve.list) # RHS LMs+ RHS curves+all patch points
num.missing<-(length(leftside)-length(right.lm)) # number of LMs to create= total RHS-current LHS LMs
blanks<-c((dim(ptsbatch_left)[1]+1):(dim(ptsbatch_left)[1]+num.missing))
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
specimens<-add_col_or_row(ptsbatch_left,n=num.missing,add_col=FALSE,fill=NA)
dimnames(specimens)[3]<-dimnames(ptsbatch_left)[3]
bilats<-cbind(leftside,rightside)
newarray<-mirrorfill(specimens,l1=midline,l2=bilats)
dimnames(newarray)[3]<-dimnames(ptsbatch_left)[3]


#Now work with newarray 
#check the whole thing 
spheres3d(newarray[,,1], radius=2, color="red")
dim(newarray)

#check parts of it
spheres3d(newarray[lmextra,,3],radius=2, color = 'red')
spheres3d(newarray[curveextra,,3],radius=2, color = 'black')
spheres3d(newarray[corressLHS,,3],radius=2, color ='red')
spheres3d(newarray[corresscurves,,3],radius=2, color = 'green')


###################################
#Pull out what we need and replace the RHS mirrored landmarks with curveextra (replace, not delete)

left_side=newarray[c(1:1083),,] ## the left lms, right lms and left curves
manual_right_curves=ptsbatch[curveextra,,] ## replace the mirrored curves with the data from ptsbatch(curveextra) 
automatic_right_curves=newarray[c(1409:1968),,] ## the curves we mirrored that we want to keep

#final_mirrored is now our dataset bind the above 
final_mirrored=abind::abind(left_side,manual_right_curves,automatic_right_curves, along = 1)

#checks
spheres3d(final_mirrored[,,3],radius=2, color = 'yellow')
spheres3d(final_mirrored[curveextra,,3],radius=2, color = 'green')
spheres3d(final_mirrored[curveextra,,3],radius=2, color = 'red')
spheres3d(final_mirrored[curveextra,,3],radius=2, color = 'black')
spheres3d(final_mirrored[corressLHS,,3],radius=2, color ='red')
spheres3d(final_mirrored[corresscurves,,3],radius=2, color = 'green')


#Later.....
final_dataset=abind::abind(odontocetes, mysticetes_and_archeocetes, along = 3)
gpa=gpagen(final_mirrored)
shape_data=gpa$coords
left_side_shape_data=shape_data[c(1:66,124:1083),,]





