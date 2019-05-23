# gab = gabparBra1605H$gab
# bib = bibBradesco
# nitform = 36
# widths = c(30,3,2,10,45)
# cadini=18
# grpini=6


write.gabarito <- function(gab,bib,nitform,cadini,grpini,...){
  nitbl <- nitform / ncol(bib)
  gabbl <- list()
  for(i in 1:max(bib)){
    aux <- c(((i-1)*nitbl+1):((i)*nitbl))
    gabbl[[i]] <- gab[aux]
  }
  
  gabaritos <- list()
  
  for(i in 1:nrow(bib)){
    vec <- gabbl[[bib[i,1]]]
    for(j in 2:ncol(bib)){
      vec <- c(vec,gabbl[[bib[i,j]]])
  }
    gabaritos[[i]] <- vec
  }  
  gabaritos <- as.data.frame(do.call(rbind,lapply(gabaritos,paste0,collapse = '')), stringsAsFactors = FALSE)
  colnames(gabaritos) <- 'gabarito'
  gabaritos$cad = (cadini+1):(cadini+nrow(bib))
  gabaritos$grp = grpini+1
  gabaritos$id = ''
  gabaritos$peso = ''
  
  write.fwf(x = gabaritos[,c('id','cad','grp','peso','gabarito')], ...)
  
}


