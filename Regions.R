
#species data 
species_data <- read.csv('Species data.csv')

#make sure the names are the phylo names
Full_names_phylo=species_data$phylo.name
dimnames(shapedata)[[3]]<-Full_names_phylo 


############################
#                          #
#         Regions          #
#                          #
############################

regionsLHS <- read.csv('regionsLHS.csv')

levels(regionsLHS$bone)
regionsLHS$bone

#makes ure the specimen name is the same 
Full_names_phylo=species_data$phylo.name
dimnames(shapedata)[[3]]<-Full_names_phylo 

#double check the data - are these lms correct? i.e., no RHS lms
spheres3d(shapedata[c(1:66),,4], radius =  0.0002)
#double check curve lengths are correct 
sum(regionsLHS$lm == "c52")

nas <- which(regionsLHS$bone=="nasal")
#plot them to check 
spheres3d(shapedata[nas,,42], radius = 0.0002)
text3d(shapedata[nas,,42], text=nas)


#pull out the modules 
premax <- which(regionsLHS$bone=="premax")
#plot them to check 
spheres3d(shapedata[premax,,42], radius =  0.0002)
text3d(shapedata[premax,,42], text=premax)


#pull out the modules 
max <- which(regionsLHS$bone=="maxilla")
#plot them to check 
spheres3d(shapedata[max,,42], radius =  0.0002)
text3d(shapedata[max,,42], text=max)


#pull out the modules 
frontal <- which(regionsLHS$bone=="frontal")
#plot them to check 
spheres3d(shapedata[frontal,,42], radius =  0.0002)
text3d(shapedata[frontal,,42], text=frontal)


#pull out the modules 
pteryg <- which(regionsLHS$bone=="pterygoid")
#plot them to check 
spheres3d(shapedata[pteryg,,42], radius =  0.0002)
text3d(shapedata[pteryg,,42], text=pteryg)


#pull out the modules 
pal <- which(regionsLHS$bone=="palate")
#plot them to check 
spheres3d(shapedata[pal,,42], radius =  0.0002)
text3d(shapedata[pal,,42], text=pal)

#pull out the modules 
supocc <- which(regionsLHS$bone=="supraoccipital")
#plot them to check 
spheres3d(shapedata[supocc,,42], radius =  0.0002)
text3d(shapedata[supocc,,42], text=occipital)

#pull out the modules 
basiocc <- which(regionsLHS$bone=="basioccipital")
#plot them to check 
spheres3d(shapedata[basiocc,,42], radius =  0.0002)
text3d(shapedata[basiocc,,42], text=basiocc)

#pull out the modules 
basisphen <- which(regionsLHS$bone=="basisphenoid")
#plot them to check 
spheres3d(shapedata[basisphen,,42], radius =  0.0002)
text3d(shapedata[basisphen,,42], text=basisphen)


#pull out the modules 
mandp <- which(regionsLHS$bone=="mandibular process")
#plot them to check 
spheres3d(shapedata[mandp,,42], radius =  0.0002)
text3d(shapedata[mandp,,42], text=mandp)


#pull out the modules 
occipcon <- which(regionsLHS$bone=="occipital condyle")
#plot them to check 
spheres3d(shapedata[occipcon,,42], radius =  0.0002)
text3d(shapedata[occipcon,,42], text=occipcon)


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


#pull out the modules 
zygo <- which(regionsLHS$bone=="zygomatic")
#plot them to check 
spheres3d(shapedata[zygo,,2], radius =  0.0002)
text3d(shapedata[zygo,,2], text=squa)


#check
spheres3d(shapedata[nas,,42], radius = 0.0002, color = "red")
spheres3d(shapedata[premax,,42], radius =  0.0002, color = "darkblue")
spheres3d(shapedata[max,,42], radius =  0.0002, color = "lightgreen")
spheres3d(shapedata[frontal,,42], radius =  0.0002, color = "pink")
spheres3d(shapedata[pteryg,,42], radius =  0.0002, color = "purple")
spheres3d(shapedata[pal,,42], radius =  0.0002, color = "yellow")
spheres3d(shapedata[supocc,,42], radius =  0.0002, colour = "orange")
spheres3d(shapedata[basiocc,,42], radius =  0.0002, color = "lightblue")
spheres3d(shapedata[basisphen,,42], radius =  0.0002, color = "darkgreen")
spheres3d(shapedata[mandp,,42], radius =  0.0002, color = "brown")
spheres3d(shapedata[jug,,42], radius =  0.0002, color = "coral")
spheres3d(shapedata[occipcon,,42], radius =  0.0002, color = "turquoise")
spheres3d(shapedata[parietal,,42], radius =  0.0002, color = "black")
spheres3d(shapedata[squa,,42], radius =  0.0002, color = "darkred")
spheres3d(shapedata[zygo,,42], radius =  0.0002, color = "magenta")


#pulling out the data for each region from all specimens 
nas_data=shapedata[nas,,]
#check 
#spheres3d(shapedata[squa,,40], radius = 0.0002, color = "red")
premax_data=shapedata[premax,,]
max_data=shapedata[max,,]
frontal_data=shapedata[frontal,,]
pteryg_data=shapedata[pteryg,,]
pal_data=shapedata[pal,,]
supocc_data=shapedata[supocc,,]
basiocc_data=shapedata[basiocc,,]
basisphen_data=shapedata[basisphen,,]
mandp_data=shapedata[mandp,,]
jug_data=shapedata[jug,,]
occipcon_data=shapedata[occipcon,,]
parietal_data=shapedata[parietal,,]
squa_data=shapedata[squa,,]
zygo_data=shapedata[zygo,,]

#makes ure names match shapedata 
dimnames(nas_data)[[3]]<-Full_names_phylo 
dimnames(premax_data)[[3]]<-Full_names_phylo 
dimnames(max_data)[[3]]<-Full_names_phylo 
dimnames(frontal_data)[[3]]<-Full_names_phylo 
dimnames(pteryg_data)[[3]]<-Full_names_phylo 
dimnames(supocc_data)[[3]]<-Full_names_phylo 
dimnames(basiocc_data)[[3]]<-Full_names_phylo 
dimnames(basisphen_data)[[3]]<-Full_names_phylo 
dimnames(mandp_data)[[3]]<-Full_names_phylo 
dimnames(occipcon_data)[[3]]<-Full_names_phylo 
dimnames(parietal_data)[[3]]<-Full_names_phylo 
dimnames(squa_data)[[3]]<-Full_names_phylo 
dimnames(zygo_data)[[3]]<-Full_names_phylo 
dimnames(pal_data)[[3]]<-Full_names_phylo 
dimnames(jug_data)[[3]]<-Full_names_phylo 




species_data <- read.csv('Species data.csv', fileEncoding="UTF-8-BOM")


#pPCA bone by bone 
#### nasal bone PCA
PCA=plotTangentSpace(nas_data, label= species_data$suborder, axis1=1, axis2=2)
PCA$pc.summary ## 
nas_var=PCA$pc.scores[,1:3] ## PC3 96%
write.table(nas_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/nasal/nas_var.txt",
            quote = FALSE, col.names = FALSE)


#pPCA bone by bone 
#### premaxilla bone PCA
PCA=plotTangentSpace(premax_data, label= species_data$suborder, axis1=1, axis2=2)
PCA$pc.summary ## 
premax_var=PCA$pc.scores[,1:7]
write.table(premax_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/premax/premax_var.txt",
            quote = FALSE, col.names = FALSE)


#pPCA bone by bone 
#### maxilla bone PCA
PCA=plotTangentSpace(max_data, label= species_data$suborder, axis1=1, axis2=2)
PCA$pc.summary ## 
max_var=PCA$pc.scores[,1:16] 
write.table(max_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/maxilla/max_var.txt",
            quote = FALSE, col.names = FALSE)


#pPCA bone by bone 
#### frontal bone PCA
PCA=plotTangentSpace(frontal_data, label= species_data$suborder, axis1=1, axis2=2)
PCA$pc.summary ## 
frontal_var=PCA$pc.scores[,1:8] 
write.table(frontal_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/frontal/frontal_var.txt",
            quote = FALSE, col.names = FALSE)


#pPCA bone by bone 
#### pterygoid bone PCA
PCA=plotTangentSpace(pteryg_data, label= species_data$suborder, axis1=1, axis2=2)
PCA$pc.summary ## 
pteryg_var=PCA$pc.scores[,1:7] 
write.table(pteryg_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/pterygoid/pteryg_var.txt",
            quote = FALSE, col.names = FALSE)



#pPCA bone by bone 
#### palate bone PCA
PCA=plotTangentSpace(pal_data, label= species_data$suborder, axis1=1, axis2=2)
PCA$pc.summary ## PCA 96%
pal_var=PCA$pc.scores[,1:8] 
write.table(pal_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/palate/pal_var.txt",
            quote = FALSE, col.names = FALSE)



#pPCA bone by bone 
#### supraoccipital bone PCA
PCA=plotTangentSpace(supocc_data, label= species_data$suborder, axis1=1, axis2=2)
PCA$pc.summary ## 
supocc_var=PCA$pc.scores[,1:11] 
write.table(supocc_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/supraoccipital/supocc_var.txt",
            quote = FALSE, col.names = FALSE)



#pPCA bone by bone 
#### basioccipital bone PCA
PCA=plotTangentSpace(basiocc_data, label= species_data$suborder, axis1=1, axis2=2)
PCA$pc.summary ## 
basiocc_var=PCA$pc.scores[,1:8] 
write.table(basiocc_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/basioccipital/basiocc_var.txt",
            quote = FALSE, col.names = FALSE)



#pPCA bone by bone 
#### basisphenoid bone PCA
PCA=plotTangentSpace(basisphen_data, label= species_data$species, axis1=1, axis2=2)
PCA$pc.summary ## 
basisphen_var=PCA$pc.scores[,1:5] 
write.table(basisphen_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/basisphenoid/basisphen_var.txt",
            quote = FALSE, col.names = FALSE)



#pPCA bone by bone 
#### mandibular bone PCA
PCA=plotTangentSpace(mandp_data, label= species_data$species, axis1=1, axis2=2)
PCA$pc.summary ## 
mandp_var=PCA$pc.scores[,1:5] 
write.table(mandp_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/mandibular process/mandp_var.txt",
            quote = FALSE, col.names = FALSE)




#pPCA bone by bone 
#### jug bone PCA
PCA=plotTangentSpace(jug_data, label= species_data$species, axis1=1, axis2=2)
PCA$pc.summary ## 
jug_var=PCA$pc.scores[,1:4] 
write.table(jug_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/jugal/jug_var.txt",
            quote = FALSE, col.names = FALSE)


#pPCA bone by bone 
#### occipital bone PCA
PCA=plotTangentSpace(occipcon_data, label= species_data$suborder, axis1=1, axis2=2)
PCA$pc.summary ## PC6 96%
occipcon_var=PCA$pc.scores[,1:6] 
write.table(occipcon_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/occipital condyle/occipcon_var.txt",
            quote = FALSE, col.names = FALSE)


#pPCA bone by bone 
#### occipital bone PCA
PCA=plotTangentSpace(parietal_data, label= species_data$suborder, axis1=1, axis2=2)
PCA$pc.summary ##
parietal_var=PCA$pc.scores[,1:8] 
write.table(parietal_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/parietal/parietal_var.txt",
            quote = FALSE, col.names = FALSE)



#pPCA bone by bone 
#### occipital bone PCA
PCA=plotTangentSpace(squa_data, label= species_data$suborder, axis1=1, axis2=2)
PCA$pc.summary ## 
squa_var=PCA$pc.scores[,1:9] 
write.table(squa_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/squamosal/squa_var.txt",
            quote = FALSE, col.names = FALSE)



#pPCA bone by bone 
#### zygomatic bone PCA
PCA=plotTangentSpace(zygo_data, label= species_data$suborder, axis1=1, axis2=2)
PCA$pc.summary ## 
zygo_var=PCA$pc.scores[,1:10] 
write.table(zygo_var*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/zygomatic/zygo_var.txt",
            quote = FALSE, col.names = FALSE)



#Comparing regions - this doesn't have ecological data yet 
C_19=compare.multi.evol.rates(A=Y.gpa.rhs_172, phy=tree_172, gp=M19)
PS_disp=morphol.disparity(Y.gpa.rhs_173[PS,,]~1)/length(PS)
Sq_disp=morphol.disparity(Y.gpa.rhs_173[Sq,,]~1)/length(Sq)



library(geomorph)

#Increase your R memory 
#memory.size() reports the current or maximum memory allocation of the malloc function used in this version of R.
#memory.limit() reports or increases the limit in force on the total allocation.
# eg. memory.limit(size = 4000000) 


#Need shapedata + SVP tree 
bones15 <- regionsLHS$bone
B_15=compare.multi.evol.rates(A=shapedata, phy=treeSVP, gp=bones15)

#Bone by bone (no ecology data)
nas_disp=morphol.disparity(shapedata[nas,,]~1)/length(nas)
premax_disp=morphol.disparity(shapedata[premax,,]~1)/length(premax)
max_disp=morphol.disparity(shapedata[max,,]~1)/length(max)
frontal_disp=morphol.disparity(shapedata[frontal,,]~1)/length(frontal)
pteryg_disp=morphol.disparity(shapedata[pteryg,,]~1)/length(pteryg)
supocc_disp=morphol.disparity(shapedata[supocc,,]~1)/length(supocc)
basiocc_disp=morphol.disparity(shapedata[basiocc,,]~1)/length(basiocc)
basisphen_disp=morphol.disparity(shapedata[basisphen,,]~1)/length(basisphen)
mandp_disp=morphol.disparity(shapedata[mandp,,]~1)/length(mandp)
occipcon_disp=morphol.disparity(shapedata[occipcon,,]~1)/length(occipcon)
parietal_disp=morphol.disparity(shapedata[parietal,,]~1)/length(parietal)
squa_disp=morphol.disparity(shapedata[squa,,]~1)/length(squa)
zygo_disp=morphol.disparity(shapedata[zygo,,]~1)/length(zygo)
pal_disp=morphol.disparity(shapedata[pal,,]~1)/length(pal)
jug_disp=morphol.disparity(shapedata[jug,,]~1)/length(jug)

save(B_15, file="F:/PTS FINAL LHS/B_15.R")
PCA=plotTangentSpace(shapedata, label= species_data$species, axis1=1, axis2=2) 
