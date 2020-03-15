
#--------------------------------------------------#
# PLOTS EVOLUTIONARY RATES OF INDIVIDUAL LANDMARKS #
#--------------------------------------------------#
#assign each landmark to a 'group'
rate.groups <- c(1:66, 124:1021)# %>% as.character(.))
#work out evolutionary rate for each landmark (insert appropriate tree)
per.land.rates <- compare.multi.evol.rates(shapedata[c(1:66, 124:1021),,],gp=rate.groups, phy=treeSVP)
#same, but for allometry-corrected data
#per.land.rates <- compare.multi.evol.rates(adj.shape[1:799,,],gp=rate.groups, phy=treePhy)
# 1. Create colour palette (rainbow spectrum colours)
col.rate <- colorRampPalette( c("violet", "blue","cyan","yellow","red" ) )
# 2. Calculate variances for each individual landmark
rate.land <- per.land.rates$sigma.d.gp
# 3. Log variance values to give more continuous values
log.rate.land <- log( rate.land )
# 4. Create colour gradient of variance values based on colour palette col.rate
rateCol.land <- col.rate( 30 )[as.numeric( cut( log.rate.land, breaks = 30 ) )]
# 5. Check output by plotting points as per-point variance
plot( log.rate.land, col = rateCol.land, cex = 2, pch = 19 )
