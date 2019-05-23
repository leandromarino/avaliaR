#' Construtor de Itens por Caderno
#' 
#' @description 
#'   Funcao para construir o objeto da classe \code{'itens_caderno'} que 
#'   sera utilizado na geracao da estrutura da prova \code{estrutura_prova}.
#'  
#'  @param ... vetores com o sequencial unico do item. Assume-se que os 
#'  cadernos estão sendo dispostos na ordem em que serao utilizados. 
#'  A ordenacao dos itens deve ser a mesma utilizada nos cadernos
#'  de prova fisicos.
#'  @seealso estrutura_prova
#' @export

gera_itens_caderno <- function(...){
  result <- list(...)
  class(result) <- c('itens_caderno', class(result))
  names(result) <- paste0('cad',sprintf("%03d", 1:length(result)))
  result
}

