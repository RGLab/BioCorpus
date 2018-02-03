# helper methods

# turn a character vector into a named list of words with the
# original being the name/key and the value being the lowered / non-punctuation version
#' @export
vec2Words <- function(charVec){
  charVec <- unique(charVec) # de-dupe elements
  charVec <- charVec[ !is.na(charVec)  ] # remove NA
  charVec <- gsub("_|\\(|\\)|:|\\.|=|,|;|<|>|\\*|\\[|\\]|\\?|\\/|&|'|~|\\?", " ", charVec) # rm most punct
  charVec <- gsub("\\s{2,}", " ", charVec) # rm extra spaces
  words <- unlist(strsplit(charVec, " ")) # make list of words
  words <- tolower(words) # lower case
  
  # Want to keep certain likely biological info with mix of alpha and digits, specifically
  # terms like CD4 and IL-2, but remove others. Easiest to do in multiple steps and then recombine.
  words <- words[ grepl("[[:alpha:]]", words) ] # remove those that are only digits
  alphas <- words[ grep("\\d+", words, invert = T) ]
  alphas <- gsub("-|+", " ", alphas) # breaking apart compound words
  alphas <- unlist(strsplit(alphas, " ")) 
  mixed <- words[ grep("\\d+", words) ] # allow only mixed words with prefixes that appear bio-related
  mixed <- mixed[ grep("cd|il", mixed) ] 
  output <- c(alphas, mixed)
  output <- output[ nchar(output) > 1 ] # rm single char modifiers
}

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
