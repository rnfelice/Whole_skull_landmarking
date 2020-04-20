
#Mirroring in the odontocetes 
#Delaing with asymmetry

midline.lm<-as.integer(c(38,40,48,49,51,54,55,56,61)) #midline landmarks 
midline.curves <- ptsbatch[c(629:648,764:783,824:838,884:903),,] #midline curves 
lmextra <- as.integer(c(67:79,83:86,120:123)) #left hand landmarks 
curveextra <- ptsbatch[c(1084:1408),,]

#Remove corresponding RHS landmarks and curves 
corressLHS <- as.integer(c(1:15,19:24)) 
corresscurves <- ptsbatch[c(124:448),,]
                             

spheres3d(ptsbatch[midline.lm,,3],radius=2, color = 'red') #check the midline 
spheres3d(midline.curves[,,3], radius = 2, color = 'red') #check the midline 
spheres3d(ptsbatch[lmextra,,3],radius=2) #check the additional LHS landmarks
spheres3d(curveextra[,,3],radius=2) #check the additional LHS curves 

#Check corressponding landmarks and curves 
spheres3d(ptsbatch[corressLHS,,3],radius=2, color ='green')
spheres3d(corresscurves[,,3],radius=2, color = 'green') 


lm_to_mirror <- ptsbatch[-c(1:15,19:24,67:79,83:86,120:123,124:1408,38,40,48,49,51,54,55,56,61),,] #take away the extra lms and curves 
#lm_to_mirror2 <- lm_to_mirror[-c(corressLHS),,] #this takes away the corresponding lms on the LHS
curves_to_mirror <- ptsbatch[-c(1:123, 124:448,1084:1408,629:648,764:783,824:838,884:903),,] #take away the extra curves and landmarks (corresponding curves) and the odont curves


#check this is correct 
spheres3d(lm_to_mirror[,,3],radius=2, color ='blue')
spheres3d(curves_to_mirror[,,3],radius=2, color ='blue')


#bind the landmarks and curves that we want to mirror 

to_mirror <- abind(lm_to_mirror, curves_to_mirror, along = 1)


#check this looks right 
spheres3d(to_mirror[,,3],radius=2, color ='blue')
spheres3d(ptsbatch[midline.lm,,3],radius=2, color = 'red') #check the midline 
spheres3d(midline.curves[,,3], radius = 2, color = 'red') 

