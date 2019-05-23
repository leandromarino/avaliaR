#' Calcula o Ponto Bisserial por alternativa
#'
#' \code{sum} returns the sum of all the values present in its arguments.
#'
#' This is a generic function: methods can be defined for it directly
#' or via the \code{\link{Summary}} group generic. For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.
#'
#' @param respostas Objeto do tipo \code{data.frame} com duas colunas
#'    nesta ordem: caderno e respostas
#' @param gabarito Objeto do tipo \code{data.frame} com duas colunas
#'    nesta ordem: caderno e gabarito
#' @param scores Vetor numerico com os escores dos estudantes
#' @param itempos Objeto do tipo \code{ItemPos(...,tipo='integer')}
#' @param resposta_possivel Respostas possiveis (por exemplo:
#'   \code{resp_possible = c(LETTERS[1:5],' ','*')}).
#' @param itematual indice do numero do item atual
#' @param CodNaoAp Codigo para o item nao apresentado
#'  
#' @return If all inputs are integer and logical, then the output
#'   will be an integer. If integer overflow
#'   \url{http://en.wikipedia.org/wiki/Integer_overflow} occurs, the output
#'   will be NA with a warning. Otherwise it will be a length-one numeric or
#'   complex vector.
#'
#'   Zero-length vectors have sum 0 by definition. See
#'   \url{http://en.wikipedia.org/wiki/Empty_sum} for more details.
#'   
#'   
#' @examples
#' data("quest_dados")
#' data("gabpar05P")
#' 
#' item_prova <- matrix(c(1:36,13:36,1:12,25:36,1:24),byrow = T,ncol=36)
#' colnames(item_prova) <- paste('it',sprintf("%02d",1:36),sep='')
#' rownames(item_prova) <- 1:nrow(item_prova)
#' 
#' dadosP <- quest_dados[nchar(quest_dados$rsp_por)==36,]
#' dadosP <- quest_dados[nchar(quest_dados$rsp_por)==36 & !is.na(quest_dados$cad_por),]
#'
#' Gabarito <- data.frame(caderno = 1:nrow(item_prova),
#'                        gabarito=apply(matrix(gabpar05P$gab[item_prova],nrow=nrow(item_prova)),1,paste0,collapse=''),
#'                        stringsAsFactors = F)
#'                        
#' Respostas <- data.frame(caderno = dadosP$cad_por,respostas = dadosP$rsp_por,stringsAsFactors = F)
#' 
#' escore_por <- Escore(respostas = Respostas,
#'                      gabarito = Gabarito,
#'                      NumCad = 3,
#'                      NumItens = 36,
#'                      CodAcer = '1',
#'                      CodErro = '0',
#'                      CodNaoAp = '9',
#'                      nbl = 3 ,
#'                      tbl = 12)
#'                      
#' posicao_item_por <- ItemPos(itens = itens_teste, nitens = 36,tipo='integer')  
#' 
#' pbise_por <- PontoBisserial(respostas = Respostas, gabarito = Gabarito, scores = escore_por$nacer,
#' itempos = posicao_item_por[[i]],resposta_possivel = c(LETTERS[1:4]," ","*"),  itematual = 5,
#' CodNaoAp = '9')
#' 
#' 


PontoBisserial <- function(respostas, gabarito, scores, itempos, resposta_possivel, itematual, CodNaoAp) {
  .Call('avaliaR_PontoBisserial', PACKAGE = 'avaliaR', respostas, gabarito, scores, itempos, resposta_possivel, itematual, CodNaoAp)
}
