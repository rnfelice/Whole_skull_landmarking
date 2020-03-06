
library(geiger)
library(geomorph)
library(phytools)

#Read in specimen data (if not already done above)

species_data <- read.csv('Species_data_asymmetry.csv')
#Read asymmetry tree in 

tree<- read.nexus('t10.nexus')
plotTree(tree,ftype="i", cex = 0.2, edge.width = 0.1)

Full_names_phylo=species_data$phylo.name
dimnames(Y.gpa$coords)[[3]]<-Full_names_phylo #adds the phylo names to the data


tmp <- name.check(tree, two.d.array(Y.gpa$coords)) ## (Y.gpa is your 3D array of landmark data)
tmp
treeSVP <- drop.tip(tree, tip=tmp$tree_not_data)  ## drop species in tree but NOT from data
#subtree is now the tree I use for procD.pgls 
treeSVP$tip.label #shows you which tips you have 
name.check(treeSVP, two.d.array(Y.gpa$coords)) ## check the same
#Should run 'character 0' or 'OK' 

#drop.tip - drop tip