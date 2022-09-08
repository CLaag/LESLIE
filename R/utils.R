#'@title Colour enhancer
#'
#'@description Enhances the colour values of a matrix
#'
#'@param m [matrix] (**required**): input matrix with three columns
#'
#'@param single_run [logical] (*with default**): stops enhancement after a single run
#'
#'@author Christian Laag (Institut de Physique du Globe de Paris, Paris, France), Sebastian Kreutzer (Institute of Geography, Ruprecht-Karl University of Heidelberg, Germany)
#'
#'@example #TODO
#'
#'@md
#'@noRd
.colour_enhancer <- function(m, single_run = FALSE){
  ##calculate boundary values
  min <- matrix(matrixStats::colMins(m), ncol = 3, nrow = nrow(m), byrow = TRUE)
  max <- matrix(matrixStats::colMaxs(m), ncol = 3, nrow = nrow(m), byrow = TRUE)
  mean <- matrix(matrixStats::colMeans2(m), ncol = 3, nrow = nrow(m), byrow = TRUE)

  ## calculate tuning values
  min_tun <- min + (min - mean)
  max_tun <- max + (max - mean)

  ## replace matrix in place
  m[] <- min_tun + (m - min) * (max_tun - min_tun) / (max - min)

  ##make sure we do not exceed boundaries
  m[m < 0] <- 0
  m[m > 1] <- 1

  ##we don't want to have an endless loop
  if (single_run || sum(matrixStats::colMaxs(m)) == 3 || sum(matrixStats::colMaxs(m) == 0))
    return(m)

  ##recall
  .colour_enhancer(m)
}
#'
#'@title Convert colours
#'
#'@description Hard-coded version to convert colours from Lab to sRGB
#'
#'@param data [matrix] (**required**): input matrix with rows defining colours
#'
#'@author Christian Laag (Institut de Physique du Globe de Paris, Paris, France), Sebastian Kreutzer (Institute of Geography, Ruprecht-Karl University of Heidelberg, Germany)
#'
#'@return 3-column [matrix] with converted values
#'
#'@examples
#'data <- matrix(c(65.29, 4.22, 15.62), ncol = 3)
#'.convertColor(data)
#'
#'@md
#'@noRd
.convertColor <- function(
  data
){
grDevices::convertColor(
  color = data,
  from.ref.white = "D65",
  from = "Lab",
  to = "sRGB",
  clip = TRUE)
}
