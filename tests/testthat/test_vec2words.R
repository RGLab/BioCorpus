context("vec2words")

test_that("Keeps biological terms but not other digits", {
  tmp <- c("CD4", 
           "CD8+", 
           "A/California", 
           "IL-2", 
           "JAWS II CELL, LPS, 45.5hr",
           "BS_Kepler_017_70_22818_blood",
           "subjects: MLN_Bris_M5, MLN_Bris_M6",
           "and/or<br></br>",
           "[gwas] *mark //br//& ?identifying' ",
           "-word compound-word t-word -b")
  res <- vec2Words(tmp)
  expect_true( all(c("cd4", "cd8+", "il-2", "compound") %in% res) )
  expect_false( any(grepl("_|70|\\.|,|;|:|<|\\[|\\?|\\&|\\*", res)) )
  expect_true( all(nchar(res) > 1) )
})