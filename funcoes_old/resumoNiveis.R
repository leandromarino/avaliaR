#' Sum of vector elements.
#'
#' \code{sum} returns the sum of all the values present in its arguments.
#'
#' This is a generic function: methods can be defined for it directly
#' or via the \code{\link{Summary}} group generic. For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.
#'
#' @param ... Numeric, complex, or logical vectors.
#' @param na.rm A logical scalar. Should missing values (including NaN)
#'   be removed?
#' @return If all inputs are integer and logical, then the output
#'   will be an integer. If integer overflow
#'   \url{http://en.wikipedia.org/wiki/Integer_overflow} occurs, the output
#'   will be NA with a warning. Otherwise it will be a length-one numeric or
#'   complex vector.
#'
#'   Zero-length vectors have sum 0 by definition. See
#'   \url{http://en.wikipedia.org/wiki/Empty_sum} for more details.
#' @examples
#' sum(1:10)
#' sum(1:5, 6:10)
#' sum(F, F, F, T, T)
#'
#' sum(.Machine$integer.max, 1L)
#' sum(.Machine$integer.max, 1)
#'
#' \dontrun{
#' sum("a")
#' }

# dados = profiParc1604M
# turmas = baseturmasParc16[baseturmasParc16$anoescolar=='EF04',]
# colniv = 'niv'
# colsresu = c('anoescolar','codesc','turma')
# disc = 'M'
# nomesniv   = paste('n',c('000',sprintf("%03d",seq(75,450,25))),"_",c(sprintf("%03d",seq(75,450,25)),'Inf'),sep='')
# nomesnivac = namesnivac=c('Tot',paste("M",sprintf("%03d",seq(75,450,25)),sep=''))
# ndec=1



resumoNiveis <- function(dados,turmas,colniv,colsresu,ndec=1,
                         nomesniv   = paste('n',c('000',sprintf("%03d",seq(75,450,25))),"_",c(sprintf("%03d",seq(75,450,25)),'Inf'),sep=''),
                         nomesnivac = c('Tot',paste("M",sprintf("%03d",seq(75,450,25)),sep='')))
{
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
  
  # percentual de alunos nos niveis de desempenho
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





# x <- resumoNiveis(
# dados = profiParc1604M,
# turmas = baseturmasParc16[baseturmasParc16$anoescolar=='EF04',],
# colniv = 'niv',
# colsresu = c('anoescolar','codesc','turma'))
# 
# x[[1]][1:10,]
# rm(x)
