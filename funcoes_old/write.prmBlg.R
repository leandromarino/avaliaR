# dados = parmatBRA15CHV2[102:181,c('It','aorig','borig','c')]
# file = paste0(dirBrad16,'Blg\\CH\\','ItensFixBra15HumV2.PRM')
# add = TRUE

write.prmBlg <- function(dados,file, add = FALSE)
{
  colnames(dados) <- c('it','a','b','c')
  dados[,c('a','b','c')] <- format(round(dados[,c('a','b','c')],5),ndigits =5, nsmall = 5)
  
  if(add == FALSE){
    # dados <- itfixBra1502M[,c('itemblg','a','b','c')]
    # file <- paste0(dirBlgBraPT1602M,'FixparmatBRA14M02V2.PRM')
    write.table(nrow(dados),file,col.names=F,row.names=F,quote=F)
    write.table(dados,file,col.names=F,row.names=F,quote=F,append = T, sep='\t')
  }
  if(add == TRUE){
    aux = readLines(con = file)
    aux[1] <- length(aux)-1 + nrow(dados)
    write.table(aux,file,col.names=F,row.names=F,quote=F,append = F, sep='\t')
    write.table(dados,file,col.names=F,row.names=F,quote=F,append = T, sep='\t')
  }
}


