

library(phytools)

pPCA=phyl.pca(treeSVP, Y=two.d.array(shapedata))
summary(pPCA)
LHS95_pPCA=pPCA$S[,c(1:36)] ## 54 is 95%
write.table(LHS95_pPCA*1000,
            file = "F:/PTS FINAL LHS/BayesTraits/LHS95_pPCA.txt",
            quote = FALSE, col.names = FALSE)


write.nexus(treeSVP, file = 'treeSVP.nexus')


#Now put the tree and the txt file in one folder
#Also add: bayes_traits_script, BayesTraitsV3, Script_BM, ScriptBM2
