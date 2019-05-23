read.blm.form <- function(file){
  blm <- readLines(con = file)
  check_form <- substr(blm, 1, 5) == ">FORM"
  check_grou <- substr(blm, 1, 5) == ">GROU"
  check_stru <- substr(blm,1,1) =="("
  inicio <- which(check_form == TRUE)
  fim_grou <- which(check_grou == TRUE)[1]
  fim_stru <- which(check_stru == TRUE)[1]
  fim <- c(which(check_form == TRUE)[-1], ifelse(is.na(fim_grou),fim_stru,fim_grou))-1
  forms <- list()
  for(i in 1:length(inicio)){
    aux <- blm[inicio[i]:fim[i]]
    forms[[i]] <- paste0(aux,collapse = '')
  }
  forms <- unlist(forms)
  forms <- lapply(strsplit(forms,'[(]|[)]'), '[', i = 2)
  forms <- unlist(forms)
  forms <- strsplit(forms,',')
  for(i in 1:length(forms)){
    mode(forms[[i]]) <- 'integer'
  }
  forms
}
