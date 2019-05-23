#' Posicao dos itens
#' 
#' @description 
#'   Funcao para construir uma lista onde cada elemento eh um item na ordem 
#'   sequencial e indica a posicao do item nos diferentes cadernos de prova.
#'  
#'  @param itens_caderno objeto da classe \code{itens_caderno}.
#'  @param nit numero de itens total.
#'  @seealso estrutura_prova, gera_itens_caderno
#'  @export

posicao_itens <- function(itens_caderno, nit){
  
  if(sum(class(itens_caderno) %in% c('itens_caderno')) == 0){
    stop("O objeto de 'itens_caderno' não pertence a classe 'itens_caderno'. Ver: '?gera_itens_caderno'")
  }
  
  if(max(unlist(itens_caderno)) != nit){
    stop(paste("O número de itens informado em 'nit':", nit,
               "eh diferente do maior sequencial de itens no 'itens caderno':", max(unlist(itens_caderno))))
  }
  
  ### criando lista com a posicao dos itens para cada um dos cadernos e blocos
  itempos <- list()
  
  seq_caderno <- function(x){
    x <- gsub('cad','', names(x))
    x <- as.integer(x)
    x
  }
  for(i in 1:nit){
    
    #verificando em quais cadernos o item 'i' aparece
    x <- unlist(lapply(itens_caderno, function(x) which(x == i)))
    itempos[[i]] <- cbind.data.frame(cad = seq_caderno(x), pos = x)
    
  }
  names(itempos) <- paste0('it', sprintf("%03d", 1:nit))
  itempos
}



itens_caderno <- gera_itens_caderno(1:10, c(11:20,1:10), 15:30)
nit = 30
tipo = 'integer'

posicao_itens(itens_caderno = gera_itens_caderno(1:10, c(11:20,1:10), 15:30), nit = 30)

