###############################
#                             #
#        phylo MANOVAs        #
#                             #
#                             #
###############################


#Running the MANOVA with the known data only 
#Plus specimen data 

tree <- read.tree('subtree_knowns.tre')
load('F:/K.gpa.RData')
ecology <- read.csv('knowns_data.csv')

#Make sure tree is subtree knowns 
tree <- subtree_knowns


#Habitat MANOVA 

#subset out habitat data 
habitat<-knowns$habitat
habitat<-as.factor(habitat)

#MANOVA with phy correction 

K.gpa=gpagen(ptsarray_morpho) #ptsarray is the coords data (i.e., an array of pts data)

gdf<-geomorph.data.frame(K.gpa,phy=subtree_knowns,ecol=habitat)
manova<-procD.pgls(coords~ecol, phy,data = gdf, iter = 5000)
summary(manova)

#Diet MANOVA 

#subset out diet data 
diet<-knowns$diet
diet<-as.factor(diet)

gdf<-geomorph.data.frame(K.gpa,phy=subtree_knowns,ecol=diet)
manova<-procD.pgls(coords~ecol, phy, data = gdf, iter = 5000)
summary(manova)
