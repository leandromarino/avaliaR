#' Calcula o Escore Total e por Blocos
#'
#' \code{Escore} calcula o escore por blocos e total
#'
#' Funcao em Cpp que calcula o escore total, por blocos e o percentual de acertos
#' para um dado conjunto de itens e um dado vetor de respostas
#'
#' @param respostas Objeto do tipo \code{data.frame} com duas colunas nesta ordem: caderno e respostas
#' @param gabarito  Objeto do tipo \code{data.frame} com duas colunas nesta ordem: caderno e gabarito
#' @param NumCad  Numero de cadernos total
#' @param NumItens Numero de itens no caderno
#' @param CodAcer Codigo para o acerto
#' @param CodErro Codigo para o erro
#' @param CodNaoAp Codigo para o item nao apresentado
#' @param nblform Numero de blocos no caderno
#' @param tbl Tamanho dos blocos (qtd de itens no bloco)
#' 
#' @return \code{data.frame} contendo: padrao de respostas com a codificacao 
#'    Acerto = \code{CodAcer}, Erro = \code{CodErro}, Nao Apresentado = 
#'    \code{CodNaoAp}; nacer = numero de acertos total; ntried = numero de itens
#'    tentados (ou seja, \code{NumItens - soma(itens_nao_apresentados)}); pacer = 
#'    percentual de acertos = \code{nacer/ntried}; nbl(02i) = numero de acertos
#'    por bloco de itens
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

