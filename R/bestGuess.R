
#' @importFrom stringdist stringdistmatrix
#' @importFrom SnowballC wordStem
#' @export
bestGuess <- function(incorrectWords, corpusFreqTbl){
    cft <- data.frame(corpusFreqTbl, stringsAsFactors = F)
    colnames(cft) <- c("word","Freq") # Rename since not sure what they may be coming in

    tmp <- stringdist::stringdistmatrix(incorrectWords, cft$word)
    rownames(tmp) <- incorrectWords
    colnames(tmp) <- cft$word

    result <- sapply(seq(1:length(rownames(tmp))), function(y){
        word <- rownames(tmp)[[y]]
        gWord <- gsub("\\+", "\\\\+", word) #grep needs escape char
        rowMinus <- tmp[y,][ -(grep(gWord, colnames(tmp))) ] # remove the rowname from options
        mostSimilar <- rowMinus[ min(rowMinus) == rowMinus] # find most similar
        nm <- names(mostSimilar)[[1]]

        if(mostSimilar[[1]] > 2){ return(word) } # return word if outside of stringdist boundary
        if(wordStem(nm) == wordStem(word)){ return(word)}   # return word if root is exactly the same

        # return most similar if frequency is higher
        msFreq <- cft$Freq[ cft$word == nm ]
        currFreq <- cft$Freq[ cft$word == word ]
        ret <- ifelse(msFreq > currFreq, nm, word)
    })

    return(result)
}

