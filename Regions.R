

#regions
regionsLHS <- read.csv('regionsLHS.csv')

levels(regionsLHS$bone)
regionsLHS$bone

#double check the data 
spheres3d(shapedata[c(1:66),,4], radius =  0.0002)
#double check curve lengths are correct 
sum(regionsLHS$lm == "c52")

nas <- which(regionsLHS$bone=="nasal")
#plot them to check 
spheres3d(shapedata[nas,,2], radius = 0.0002)
text3d(shapedata[nas,,2], text=nas)


#pull out the modules 
premax <- which(regionsLHS$bone=="premax")
#plot them to check 
spheres3d(shapedata[premax,,2], radius =  0.0002)
text3d(shapedata[premax,,2], text=premax)


#pull out the modules 
max <- which(regionsLHS$bone=="maxilla")
#plot them to check 
spheres3d(shapedata[max,,2], radius =  0.0002)
text3d(shapedata[max,,2], text=max)


#pull out the modules 
frontal <- which(regionsLHS$bone=="frontal")
#plot them to check 
spheres3d(shapedata[frontal,,2], radius =  0.0002)
text3d(shapedata[frontal,,2], text=frontal)


#pull out the modules 
pteryg <- which(regionsLHS$bone=="pterygoid")
#plot them to check 
spheres3d(shapedata[pteryg,,2], radius =  0.0002)
text3d(shapedata[pteryg,,2], text=pteryg)


#pull out the modules 
pal <- which(regionsLHS$bone=="palate")
#plot them to check 
spheres3d(shapedata[pal,,2], radius =  0.0002)
text3d(shapedata[pal,,2], text=pal)

#pull out the modules 
occipital <- which(regionsLHS$bone=="occipital")
#plot them to check 
spheres3d(shapedata[occipital,,2], radius =  0.0002)
text3d(shapedata[occipital,,2], text=occipital)

#pull out the modules 
basiocc <- which(regionsLHS$bone=="basioccipital")
#plot them to check 
spheres3d(shapedata[basiocc,,2], radius =  0.0002)
text3d(shapedata[basiocc,,2], text=basiocc)

#pull out the modules 
basisphen <- which(regionsLHS$bone=="basisphenoid")
#plot them to check 
spheres3d(shapedata[basisphen,,2], radius =  0.0002)
text3d(shapedata[basisphen,,2], text=basisphen)


#pull out the modules 
glen <- which(regionsLHS$bone=="glenoid")
#plot them to check 
spheres3d(shapedata[glen,,2], radius =  0.0002)
text3d(shapedata[glen,,2], text=glen)


#pull out the modules 
occipcon <- which(regionsLHS$bone=="occipitalcondyle")
#plot them to check 
spheres3d(shapedata[occipcon,,2], radius =  0.0002)
text3d(shapedata[occipcon,,2], text=occipcon)


#pull out the modules 
jug <- which(regionsLHS$bone=="jugal")
#plot them to check 
spheres3d(shapedata[jug,,2], radius =  0.0002)
text3d(shapedata[jug,,2], text=jug)


#pull out the modules 
parietal <- which(regionsLHS$bone=="parietal")
#plot them to check 
spheres3d(shapedata[parietal,,2], radius =  0.0002)
text3d(shapedata[parietal,,2], text=parietal)


#pull out the modules 
squa <- which(regionsLHS$bone=="squamosal")
#plot them to check 
spheres3d(shapedata[squa,,2], radius =  0.0002)
text3d(shapedata[squa,,2], text=squa)


spheres3d(shapedata[nas,,2], radius = 0.0002, color = "red")
spheres3d(shapedata[premax,,2], radius =  0.0002, color = "darkblue")
spheres3d(shapedata[max,,2], radius =  0.0002, color = "lightgreen")
spheres3d(shapedata[frontal,,2], radius =  0.0002, color = "pink")
spheres3d(shapedata[pteryg,,2], radius =  0.0002, color = "purple")
spheres3d(shapedata[pal,,2], radius =  0.0002, color = "yellow")
spheres3d(shapedata[occipital,,2], radius =  0.0002, colour = "orange")
spheres3d(shapedata[basiocc,,2], radius =  0.0002, color = "lightblue")
spheres3d(shapedata[basisphen,,2], radius =  0.0002, color = "darkgreen")
spheres3d(shapedata[glen,,2], radius =  0.0002, color = "brown")
spheres3d(shapedata[jug,,2], radius =  0.0002, color = "coral")
spheres3d(shapedata[occipcon,,2], radius =  0.0002, color = "turquoise")
spheres3d(shapedata[parietal,,2], radius =  0.0002, color = "black")
spheres3d(shapedata[squa,,2], radius =  0.0002, color = "darkred")

