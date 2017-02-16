#comparePI:
#' A function for comparing proliferative indices from variance stabalized RNA-seq data in the ProliferativeIndex package
#'
#' This function allows the user to examine a summary and plot of their PIs
#' @param vstPIinput Output from ProliferativeIndex calculatePI function (PI of user vst data)////////////////////////////////////////////////////////////////////////
#' @export
#' @examples
#' comparePI(exVSTPI)

comparePI<-function(vstPIinput){
  print(summary(vstPIinput))
  graphics::plot(sort(vstPIinput), xlab="Samples", ylab="PI (vst counts)", main="PI")
  #add plots comparing user data to general GTEx and TCGA data
  #add plots comparing user data to specific tissue match in public data
}
