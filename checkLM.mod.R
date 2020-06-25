


# >>> Modified checkLM function of "Morpho" package <<< #
# Scales the landmark size to centroid size if:
# @pt.size = NULL
# @pt.size.scale: number to scale centroid size to sphere radius (0.0001 works well)

checkLM.mod <- function (dat.array, path = NULL, prefix = "", suffix = ".ply", 
          col = "white", pt.size = NULL, pt.size.scale=NULL, alpha = 1, begin = 1,
          render = c("w", "s"), point = c("s", "p"), add = FALSE, Rdata = FALSE, 
          atlas = NULL, text.lm = FALSE) 
{
  k <- NULL
  marked <- NULL
  j <- 1
  if (!Rdata) 
    load <- file2mesh
  outid <- NULL
  point <- point[1]
  radius <- pt.size
  if (is.null(radius)) {
    if (point == "s") 
      
      # ! modified section start ! #
      
      # radius <- (cSize(dat.array[, , 1])/sqrt(nrow(dat.array[,,1]))) * (1/30) # original code in checkLM
      radius <- NULL
      for(i in 1:dim(dat.array)[3]) {
        radius[i] <- cSize(dat.array[,,i])*pt.size.scale
      }
      # ! modified section end ! #
    }
    else { radius <- 10
  }
  size <- radius
  render <- render[1]
  arr <- FALSE
  point <- point[1]
  if (point == "s") {
    rendpoint <- spheres3d
  }
  else if (point == "p") {
    rendpoint <- points3d
  }
  #else {
    #stop("argument \\"point\\" must be \\"s\\" for spheres or \\"p\\" for points")
  #}
  dimDat <- dim(dat.array)
  if (length(dimDat) == 3) {
    n <- dim(dat.array)[3]
    name <- dimnames(dat.array)[[3]]
    arr <- TRUE
  }
  else if (is.list(dat.array)) {
    n <- length(dat.array)
    name <- names(dat.array)
  }
  else {
    stop("data must be 3-dimensional array or a list")
  }
  i <- begin
  if (render == "w") {
    back <- front <- "lines"
    rend <- wire3d
  }
  else {
    back <- front <- "filled"
  }
  if (!add || rgl.cur() == 0) 
    open3d()
  if (!is.null(atlas)) {
    k <- dim(atlas$landmarks)[1]
  }
  meshnames <- paste(path, prefix, name, suffix, sep = "")
  while (i <= n) {
    rgl.bringtotop()
    tmp.name <- meshnames[i]
    if (arr) 
      landmarks <- dat.array[, , i]
    else landmarks <- dat.array[[i]]
    if (is.null(atlas)) {
      outid <- rendpoint(landmarks, radius = radius[i], size = size[i])
      if (text.lm) 
        outid <- c(outid, text3d(landmarks, texts = paste(1:dim(landmarks)[1], 
                                                          sep = ""), cex = 1, adj = c(1, 1.5)))
      if (!is.null(path)) {
        if (!Rdata) {
          tmpmesh <- file2mesh(tmp.name)
        }
        else {
          input <- load(tmp.name)
          tmp.name <- gsub(path, "", tmp.name)
          tmpmesh <- get(input)
        }
        outid <- c(outid, shade3d(tmpmesh, col = col, 
                                  alpha = alpha, back = back, front = front))
        rm(tmpmesh)
        if (Rdata) 
          rm(list = input)
        gc()
      }
    }
    else {
      atlas.tmp <- atlas
      atlas.tmp$mesh <- NULL
      atlas.tmp$landmarks <- landmarks[1:k, ]
      atlas.tmp$patch <- landmarks[-c(1:k), ]
      if (!is.null(path)) {
        if (!Rdata) {
          atlas.tmp$mesh <- file2mesh(tmp.name)
        }
        else {
          input <- load(tmp.name)
          tmp.name <- gsub(path, "", tmp.name)
          atlas.tmp$mesh <- get(input)
        }
      }
      outid <- plotAtlas(atlas.tmp, add = TRUE, alpha = alpha, 
                         pt.size = radius[i], render = render, point = point, 
                         meshcol = col, legend = FALSE)
    }
    answer <- readline(paste("viewing #", i, "(return=next | m=mark current | s=stop viewing)\\n"))
    if (answer == "m") {
      marked[j] <- i
      j <- j + 1
    }
    else if (answer == "s") {
      i <- n + 1
    }
    else i <- i + 1
    rgl.pop(id = outid)
  }
  invisible(marked)
}
