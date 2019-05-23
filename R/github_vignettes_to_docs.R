github_vignettes_to_docs <- function(){
  arqs <- dir('./vignettes/')

arqs <- arqs[grep(pattern = '.Rmd', x = arqs, ignore.case = T)]
vinhetas <- lapply(X = arqs, FUN = function(x) readLines(con = paste0('./vignettes/', x)))

gsub_output_rmd <- function(x){
  x <- gsub(pattern = "output: rmarkdown::html_vignette",
            replacement = "output: \n  md_document: \n    variant: gfm",
            x = x)
  x
}

docs <- lapply(X = vinhetas, FUN = gsub_output_rmd)

for(i in 1:length(arqs)){
  writeLines(text = docs[[i]], con = paste0('./docs/', arqs[i]))
  knitr::knit(input = paste0('./docs/', arqs[i]),
              output = paste0('./docs/', gsub('.Rmd', '.md', arqs[i])))
}
}
