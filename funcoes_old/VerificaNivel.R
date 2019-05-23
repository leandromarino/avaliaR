VerificaNivel <- function(EmpPerc){
  dados <- EmpPerc[[3]]
  aux <- as.numeric(colnames(dados)[-c(1,2,(ncol(dados)-3):ncol(dados))])
  by_nivel <- aux[2] - aux[1]
  result <- dados[,c('It','Gab','level','aban','aban_motivo')]
  result$niv1 <- result$niv0  <- NA
  for(i in 1:nrow(dados)){
    aux <- dados[i,]
    aux0 <- gsub("<","",aux$level)
    if(aux0 != "N.A."){
      x <- aux[,c(as.numeric(aux0)-by_nivel, aux0)]  
      result[i,c('niv0','niv1')] <- unlist(x)
    }else{
      result[i,c('niv0','niv1')] <- 0
    }
  }
  result <- result[result$niv0 > 0.60,]
  result
}