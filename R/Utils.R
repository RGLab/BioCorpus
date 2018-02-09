# helper methods

#' @importFrom Rlabkey labkey.selectRows
#' @import stopwords
#' @export
getISdata <- function(tableName, colName){
  newNm <- paste0(tableName, "-", colName)
  message(newNm)
  tmp <- labkey.selectRows(baseUrl = "https://www.immunespace.org",
                           folderPath = "/Studies/",
                           schemaName = "ImmPort",
                           queryName = tableName,
                           colSelect = c(colName, "study_accession"),
                           colNameOpt = "fieldname")
  tmp$tblCol <- newNm
  colnames(tmp)[[1]] <- "value" # standardize to allow rbind
  return(tmp)
}
