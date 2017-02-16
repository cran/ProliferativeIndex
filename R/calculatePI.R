#calculatePI:
#' A function for calculating proliferative index from variance stabalized RNA-seq data in the ProliferativeIndex package
#'
#' This function allows the user to read in data for subsequent proliferative index calculation and analysis
#' @param userObject Output from ProliferativeIndex readDataForPI function
#' @export
#' @examples
#' calculatePI(exReadDataObj)

calculatePI<-function(userObject){
  dataframePIvst<-subset(userObject$vstData, rownames(userObject$vstData) %in% metaPCNA2)
  vstPI<-apply(userObject$vstData, 2, stats::median)
  print(paste("vstData contained ", nrow(dataframePIvst), "/131 of the PI-associated genes", sep=""))
  invisible(vstPI)
  }

