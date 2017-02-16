#readDataForPI:
#' A function for reading data in for use with the ProliferativeIndex package
#'
#' This function allows the user to read in variance stabalized RNA-seq data and gene model names for subsequent proliferative index calculation and analysis
#' @param vstData Dataframe of user variance stabalized count data (from DESeq2) with samples in columns and genes in rows. Rownames must be genes.
#' @param modelIDs Genes in user identified model for comparison to proliferative index
#' @export
#' @examples
#' readDataForPI(vstTCGA_ACCData_sub, c("AIFM3", "ATP9B", "CTRC", "MCL1", 
#' "MGAT4B", "ODF2L", "SNORA65", "TPPP2"))


readDataForPI<-function(vstData, modelIDs){
  #Error checking for each element

  #vst data
  userVSTData<-vstData
  #make sure there is a vstData object
  if (missing(userVSTData))
    stop("You must include a dataframe of vst data")
  #check if vstData elements are numeric
  if(all(sapply(userVSTData, is.numeric)==TRUE) != TRUE)
    stop("The vst dataframe contains non-numeric values")
  #check that vstData is a dataframe
  if(class(userVSTData)!="data.frame")
    stop("The vst data entered is not a dataframe")

  #model gene IDs
  userModelIDs<-modelIDs
  #make sure there are modelIDs
  if (missing(userModelIDs))
    stop("You must include gene names for a model of interest")
  #check that modelIDs are characters
  if(all(sapply(userModelIDs, is.character)==TRUE) != TRUE)
    stop("The gene model names are not characters")
  #check that modelIDs are a character
  if(class(userModelIDs)!="character")
    stop("The gene model names entered are not a character string")
  #check that the modelIDs are in the rownames of vstData
  if(all(userModelIDs %in% rownames(userVSTData)) != TRUE)
    stop("ModelIDs contain gene name(s) not present in vstData")

  #return list of elements (vstData and modelIDs)
  dataList<-list("vstData"=userVSTData, "modelIDs"=userModelIDs)
  return(dataList)
}

