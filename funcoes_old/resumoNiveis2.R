
resumoNiveis2 <- function(dados,turmas,colniv,colsresu,ndec=1,pontosdecorte = seq(75,450,25))
{
  nomesniv   = paste0('n',c('000',sprintf("%03d",pontosdecorte)),"_",c(sprintf("%03d",pontosdecorte),'Inf'))
  nomesnivac = c('Tot',paste("M",sprintf("%03d",pontosdecorte),sep=''))
  
  (nalu <- nrow(dados))
  (nvari <- length(colsresu))
  (colsresu <- rev(colsresu))
  (auxcols <- 1:(nvari+1))
  
  lista <- as.list(dados[,colsresu])
  result <- list()
  for(i in 1:(nvari+1)){
    print(paste("Gerando resultados por: ",c(colsresu,'total')[i],sep=''))
    if(i>1) lista[[i-1]] <- rep('*Total',nalu)
    result[[i]] <- aggregate(dados[,colniv],lista,function(x) c(length(x),table(x)))
    result[[i]] <- as.data.frame(as.list(result[[i]]))
  }
  
  resultniv <- do.call(rbind,result)
  colnames(resultniv) <- c(colsresu,'pres',nomesniv)
  
  # percentual de alunos nos nÃ?veis de desempenho
  resultpercniv <- resultniv
  resultpercniv[,-auxcols] <- round(resultniv[,-auxcols]/resultniv$pres*100,ndec)
  
  # numero de alunos acima dos niveis de desempenho
  resultnivac <- resultniv
  resultnivac[,-c(auxcols)] <- t(apply(t(apply(apply(resultniv[,-c(auxcols)],1,rev),2,cumsum)),1,rev))
  colnames(resultnivac)[-c(auxcols)] <- nomesnivac
  
  # percentual de alunos acima dos niveis de desempenho
  resultpercnivac <- resultniv
  resultpercnivac[,-auxcols] <- round(resultnivac[,-auxcols]/resultniv$pres*100,1)
  colnames(resultpercnivac)[-c(auxcols)] <- nomesnivac
  
  result <- list()
  result[[1]] <- merge(turmas,resultniv      ,by=rev(colsresu),all=T)
  result[[2]] <- merge(turmas,resultpercniv  ,by=rev(colsresu),all=T)
  result[[3]] <- merge(turmas,resultnivac    ,by=rev(colsresu),all=T)
  result[[4]] <- merge(turmas,resultpercnivac,by=rev(colsresu),all=T)
  names(result) <- c('Numero de alunos nos niveis',
                     'Percentual de alunos no niveis',
                     'Numero de alunos acima dos niveis',
                     'Percentual de alunos acima dos niveis')
  result
}


