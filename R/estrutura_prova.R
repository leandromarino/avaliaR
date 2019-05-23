#' Estrutura da Prova
#' 
#' @description 
#'    Esta função tem por objetivo gerar um objeto da classe 'estrutura_teste'
#'    \code{gera_gabarito} e \code{ItemPos}.
#'  
#' @param itens_caderno objeto da classe 'itens_caderno', gerado a partir da funcao 
#'     \code{gera_itens_caderno}.
#' @param coditem vetor com códigos dos itens.
#' @param respostas objeto do tipo \code{list} onde cada elemento é o conjunto
#'     de opções de respostas possíveis. \code{Ex.: c(LETTER[1:4], '*', 'c')}.
#' @param tipo_item objeto do tipo \code{vector} indicando para cada item se ele é 
#'     \code{'dicotomico'} ou \code{'politomico'}.
#' @param pontuacao objeto do tipo \code{list} indicando a pontuacao para os casos de 
#'     itens politômicos. Em caso de item dicotômico declarar NULL.
#'     Exemplo: \code{list(NULL, c(0.0, 0.5, 1.0))}
#' @param gabarito objeto do tipo \code{vetor} indicando os gabaritos para os itens 
#'     dicotômicos. Em caso de item politômico declarar NULL. Exemplo: \code{c("A", NULL)}
#' @param nit Número de itens que estão sendo adicionados ou criados com essa função.
#'     o nit deve ser igual ao número de elementos em cada um dos parâmetros anteriores. 
#'     Observação: caso só existam itens dicotômicos então \code{pontuacao} 
#'     pode não ser declarada. Default: \code{NULL}
#'     Observação: caso só existam itens politômicos então \code{gabarito} 
#'     pode não ser declarada. Default: \code{NULL}
#' @param nit numero de itens total
#' @seealso gera_itens_caderno
#' @export


estrutura_prova <- function(itens_caderno, coditem, respostas, tipo_item, pontuacao, gabarito, nit){
  
  
  itempos <- posicao_itens(itens_caderno = itens_caderno, nit = nit)
  
  gabarito <- gera_gabarito(coditem = coditem,
                            respostas = respostas,
                            tipo_item = tipo_item,
                            pontuacao = pontuacao,
                            gabarito = gabarito,
                            nit = 36,
                            data = NULL)
  
  if(length(gabarito$gabarito) != length(itempos)){
    stop('O número de itens em gabarito é diferente do número de itens em itempos')
  }
  
  result <- vector("list", length(itempos))
  
  for(item_atual in 1:length(itempos)){
    (result[[item_atual]] <- list(coditem = NULL, tipo_item = NULL, 
                                  gabarito = NULL, posicao = NULL))
    (result[[item_atual]]['coditem']   <- gabarito$coditem[item_atual])
    (result[[item_atual]]['tipo_item'] <- gabarito$tipo_item[item_atual])
    (result[[item_atual]]['gabarito']  <- gabarito$gabarito[item_atual])
    (result[[item_atual]]['posicao']   <- itempos[item_atual])
    
  }
  
  names(result) <- names(itempos)
  
  class(itens_caderno) <- 'list'
  
  result <- list(itens_caderno = itens_caderno, info_item = result)

  class(result) <- c("estrutura_prova", class(result))
  
  result
  
}

