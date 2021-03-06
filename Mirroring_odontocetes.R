
library(rgl)
library(abind)
#Mirroring in the odontocetes 
#Dealing with asymmetry

#normal specimen landmarks = 1:123
#normal specimen curves = 1:64 (lines 124:1083)
#asymmetric specimens landmarks = 1:123 
#asymmetric spcimens curves = 1:85 (lines 1084:1408)

#LHS = original 
#RHS = asymmetric 

ptsbatch <- load('ptsbatch.R') #Just 4 specimens to play with. Used estimate.missing on one of them to test it works ok

#midline landmarks 
midline.lm<-ptsbatch[c(38,40,48,49,51,54,55,56,61),,] 
#midline curves 
midline.curves <- ptsbatch[c(629:648,764:783,824:838,884:903),,] 
#Extra landmarks placed on the RHS of odontocetes 
lmextra <- ptsbatch[c(67:79,83:86,120:123),,]
#Extra curves placed on the RHS of odontocetes 
curveextra <- ptsbatch[c(1084:1408),,] #1084 is curve #65 

#Remove corresponding LHS landmarks and curves 
corressLHS <- ptsbatch[c(1:15,19:24),,] #LMs 16-18 are the jugal which I'm ignoring
corresscurves <- ptsbatch[c(124:448),,] #curves #i.e. curve 1-21 (inlcusive) 

#check the midline 
spheres3d(midline.lm[,,3],radius=2, color = 'red') 
#check the midline 
spheres3d(midline.curves[,,3], radius = 2, color = 'red') 
#check the additional RHS landmarks and RHS curves
##Check corressponding landmarks and curves on LHS (original) 
spheres3d(lmextra[,,3],radius=2, color = 'red') 
spheres3d(curveextra[,,3],radius=2, color = 'black') 
spheres3d(corressLHS[,,3],radius=2, color ='red')
spheres3d(corresscurves[,,3],radius=2, color = 'green') 

#Not sure why I couldn't get 'corressLHS' to work so I just put in the LM numbers again...
#take away the extra lms and curves 
lm_to_mirror <- ptsbatch[-c(1:15,19:24,67:123,83:86,120:123,124:1408,38,40,48,49,51,54,55,56,61),,]
#lm_to_mirror2 <- lm_to_mirror[-c(corressLHS),,] #this takes away the corresponding lms on the LHS - doesn't work so have put the numbers in
#take away the extra curves and landmarks (corresponding curves) and the odont curves
curves_to_mirror <- ptsbatch[-c(1:123, 124:448,1084:1408,629:648,764:783,824:838,884:903),,] 

#check this is correct 
spheres3d(lm_to_mirror[,,3],radius=2, color ='red') #the face and midline line points have been removed 
spheres3d(curves_to_mirror[,,3],radius=2, color ='blue') #the face and midline have been removed 


#bind the landmarks and curves that we want to mirror 
to_mirror <- abind(lm_to_mirror, curves_to_mirror, along = 1)


#check this looks right 
spheres3d(to_mirror[,,3],radius=2, color ='blue')
spheres3d(midline.lm[,,3],radius=2, color = 'yellow') #check the midline 
spheres3d(midline.curves[,,3], radius = 2, color = 'red') 


#bind midline curves and landmarks 
midline <- abind(midline.lm, midline.curves, along = 1)
#double check 
spheres3d(midline[,,3],radius=2, color = 'yellow') 


#CHECK IT ALL - PLOT TOGETHER 
#red = the extra asymmetric data on odonts
#blue = landmarks/curves to mirror across
#green = corresponding curves/landmarks (to the asymmetric ones) 
#yellow = midline 

spheres3d(lmextra[,,3],radius=2, color = 'red') 
spheres3d(curveextra[,,3],radius=2, color  = 'red') 
spheres3d(to_mirror[,,3],radius=2, color ='blue')
spheres3d(corressLHS[,,3],radius=2, color ='green')
spheres3d(corresscurves[,,3],radius=2, color = 'green') 
spheres3d(midline[,,3],radius=2, color = 'yellow') 
