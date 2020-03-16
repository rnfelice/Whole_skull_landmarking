###############################
#                             #
#        phylo MANOVAs        #
#                             #
#                             #
###############################

#Running the MANOVA with the known data only 
#Plus specimen data 

#cut tree to knowns data (extants only)
#tree is the SVP tree 

species_data_knowns <- read.csv('Species data_knowns.csv', fileEncoding="UTF-8-BOM")

Full_names_phylo=species_data_knowns$phylo.name
dimnames(shapedata_extant)[[3]]<-Full_names_phylo #adds the phylo names to the data

tmp <- name.check(treeSVP, two.d.array(shapedata_extant)) ## (Y.gpa is your 3D array of landmark data)
tmp
treeSVP_extant <- drop.tip(treeSVP, tip=tmp$tree_not_data)  ## drop species in tree but NOT from data
#subtree is now the tree I use for procD.pgls 
treeSVP_extant$tip.label #shows you which tips you have 
name.check(treeSVP, two.d.array(shapedata)) ## check the same
#Should run 'character 0' 


#Make sure tree is subtree knowns 
phy <- treeSVP_extant

#Habitat MANOVA 

#subset out habitat data 
habitat<-species_data_knowns$habitat
habitat<-as.factor(habitat)

diet <- species_data_knowns$diet
diet <-as.factor(diet)

#MANOVA with phy correction 

K.gpa=gpagen(shapedata_extant) #ptsarray is the coords data (i.e., an array of pts data)

#Change for diet and habitat 
gdf<-geomorph.data.frame(K.gpa, phy=treeSVP_extant, ecol=diet)
manova<-procD.pgls(coords~ecol, phy,data = gdf, iter = 5000)
summary(manova)

#Compare with non-phylogenetically corrected data 
gdf <- geomorph.data.frame(K.gpa, diet = species_data_knowns$diet)
procD.lm(coords ~ diet, iter=999, data = gdf)
procD.lm(coords ~ habitat, iter=999, data = gdf, RRPP = TRUE)


