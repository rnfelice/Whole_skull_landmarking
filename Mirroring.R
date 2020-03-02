

###################################
#                                 #
#      MIRRORING LANDMARKS        #
#                                 #
###################################

########## SYMETRISATION TO IMPROVE THE SHAPE ANALYSES #########################
#Ellen's code 
#DATA SHOULD NOT BE PROCRUSTED 


#Remove landmarks from the RHS (so that we can mirror the whole set of curves and landmarks)
newptsnoRHS <- newnewpts[-c(67:123),,]
#This allows us to see the numbered landmarks 
text3d(newptsnoRHS[,,3], text=1:dim(newptsnoRHS)[1])


#Set the midline 
midline<-as.integer(c(38,40,48,49,51,54,55,56,61)) # LM that are on the midline + parasphenoid curve points + NO patch point
#got length(midline)= 9 points on the midline

left.lm <- c(1:37,39,41:47,50,52,53,57:60,62:66,124:536) 
#left.lm <- c(2,3,5:18,21:37,39,41:47,50,52,53,57:60,62:66)
#exclude midline points. Last number = last number of newpts 


lengmatrice=dim(newptsnoRHS)[1]*2-length(midline)#-length(nasalfrontal) #should be the length with the both sides, 1 is the column and 2 
#just means that we are duplicating the data to be on both sides of the skull 


Matrice=array(NA,dim = c(lengmatrice,3,188)) #3 is the dimensions (x, y, z), 2 is specimen number 
Matrice[1:dim(newptsnoRHS)[1],,]=newptsnoRHS


#left.lm <- c(1:37,39,41:47,50,52,53,57:60,62:66)
#left.lm <- c(2,3,5:18,21:37,39,41:47,50,52,53,57:60,62:66)
#exclude midline points. Last number = last number of newpts 

#Check left.lm and midline [left.lm,,x] = species number
spheres3d(newptsnoRHS[left.lm,,36],radius=2.5)
spheres3d(newptsnoRHS[midline,,36],radius=3,col='red')

right.lm <- c(67:123,537:1063) #left.lm +1:lenmatrice

bilat.landmarks <- cbind(left.lm, right.lm) #one column is the rows of the right side LM and the other column the rows of the left side

Mirroredpts=mirrorfill(A=Matrice,  l1=midline, l2=bilat.landmarks) # the NA rows are now filled so that the numbers are the same on both
#sides of the skull 
Mirroredpts
deformGrid3d(Mirroredpts[537:1063,,2], Matrice[,,2], ngrid=0) #This shows you the new mirroed landmarks 

#can't do with Procrusted data
#This shows the original landmarks
spheres3d(Mirroredpts[c(1:67),,67],col=2,radius=3)

spheres3d(Mirroredpts[,,1],col=2,radius=3)
