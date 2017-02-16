#compareModeltoPI:
#' A function for comparing proliferative indices from variance stabalized RNA-seq data to model principal componenets in the ProliferativeIndex package
#'
#' This function allows the user to calculate a correlation between their model and PI
#' @param userObject Output from ProliferativeIndex readDataForPI function (user data)
#' @param vstPI Output from ProliferativeIndex calculatePI function
#' @export
#' @examples
#' compareModeltoPI(exReadDataObj, exVSTPI)

compareModeltoPI<-function(userObject, vstPI){
  dataframePIvst<-subset(userObject$vstData, rownames(userObject$vstData) %in% metaPCNA2)
  userModelPCA<-stats::prcomp(t(dataframePIvst), center=TRUE, scale=TRUE)
  corrTable<-data.frame(matrix(ncol=3, nrow=10))
  rownames(corrTable)<-c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")
  colnames(corrTable)<-c("SpearmanRho", "SpearmanPvalue", "PCAPropOfVariance")
  corrTable[,3]<-summary(userModelPCA)$importance[2,1:10]
  for (i in 1:10){
    corrTest<-stats::cor.test(userModelPCA$x[,i], vstPI, method="spearman")
    corrTable[i,1]<-corrTest$estimate
    corrTable[i,2]<-corrTest$p.value
  }
  graphics::plot(userModelPCA$x[,1], vstPI, xlab="model PC1", ylab="PI")
  return(corrTable)
}