

# temp
# 
busca_resposta_item <- function(caderno, respostas, itempos, nao_apres){

  #funcao para nomear coluna dos cadernos
  nome_col_cad <- function(cad){
    paste0('cad',sprintf('%03d', cad))
  }
  #funcao para recuperar as respostas ao item_atual nos diversos cadernos
  #essa funcao sera usada dentro do apply
  recupera_resposta <- function(x){
    vetor = rep(nao_apres, length(respostas))
    vetor[caderno == x[1]] <- substr(x = respostas, start = x[2], stop = x[2])[caderno == x[1]]
    vetor
  }
  #recuperando as respostas para o item atual nos diversos cadernos
  saida <- data.frame(apply(itempos, 1, recupera_resposta), stringsAsFactors = F)
  colnames(saida) <- nome_col_cad(itempos[,'cad'])
  
  #transformando a matriz "saida" em um vetor de respostas
  aux_caderno <- data.frame(seq = 1:length(caderno), caderno = caderno)
  resposta_item_atual <- apply(aux_caderno, 1, function(x) saida[x[1], nome_col_cad(x[2])])
  rm(saida, aux_caderno, recupera_resposta)
  
  #convertendo NULL para nao_apres
  resposta_item_atual <- lapply(resposta_item_atual, FUN = function(x) ifelse(is.null(x), nao_apres, x))
  
  resposta_item_atual <- unlist(resposta_item_atual)
  
  resposta_item_atual
  
  
  
}
