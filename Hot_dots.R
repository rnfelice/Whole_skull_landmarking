

#### HOT DOTS PLOT #######
  cols1<-colorRampPalette(c("#6E016B","#0C2C84","#225EA8","#005A32","#FFFF00","#FE9929","#FC4E2A","red"))
  cols<-cols1(100)
  {
    #x=log10(variances) #### do variances here, or raes.vector
    x=log10(rates.vector)
    xlims<-NULL
    tol <- 1e-06
    xlims <- range(x) + c(-tol, tol)
    nbin=100
    breaks <- 0:nbin/nbin * (xlims[2] - xlims[1]) + xlims[1]
    whichColor <- function(p, cols, breaks) {
      i <- 1
      while (p >= breaks[i] && p > breaks[i + 1]) i <- i +
          1
      cols[i]
    }
    colors <- sapply((x), whichColor, cols = cols, breaks = breaks)
  }
  open3d()
  spheres3d(RHS_data[,,3],radius = .1,col=colors)
  shade3d(Adenomus,col=bone1)
rgl.snapshot("./Frogs/PATCHING/RESULTS/Adenomus_hot_dots_ant_rates_173.png")
