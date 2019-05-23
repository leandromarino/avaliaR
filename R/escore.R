#' Escore
#' 
#' @description 
#'    Esta função tem por objetivo calcular o escore dos participantes.
#'  
#'  
#' @param respostas vetor de caracteres com as respostas dos alunos em cada um 
#'     dos itens do caderno de provas.
#' @param caderno vetor com o número do caderno que o participante respondeu.
#' @param estrutura_prova objeto resultante da função \code{estrutura_prova}.
#' @param nao_apres caracter de tamanho 1 contendo o código para os itens não 
#'    apresentados, ex.: \code{'9'}
#'  
#' @return
#' ATENÇÃO: O resultado da função está ordenado por item.
#' A saída da função é um \code{data.frame} com 7 colunas:
#' \describe{
#'   \item{respostas}{string com as resposta dos alunos em cada um dos itens da prova (todos os cadernos),
#'   respeitando o sequencial do item em \code{estrutura_prova}.}
#'   \item{padrao}{padrão de respostas considerando a codificação definida no \code{estrutura_prova}.}
#'   \item{ntot}{Número total de itens (considerando todos os possíveis cadernos).}
#'   \item{nnao_apres}{Número de itens não apresentados ao participante.}
#'   \item{ntried}{Quantidade de itens que foi respondida pelo partipante.}
#'   \item{score}{Soma da pontuação do participante em cada item.}
#'   \item{pacer}{percentual de acertos}
#' }
#' @seealso estrutura_prova
#' @export


escore <- function(respostas, caderno, estrutura_prova, nao_apres = NULL){
  
  # checando a classe de itemgab
  if(sum(class(estrutura_prova) %in% 'estrutura_prova') == 0){
    stop("O objeto informado em 'estrutura_prova' nao pertence a classe 'estrutura_prova'.")
  }
  
  itemgab <- estrutura_prova$info_item
  
  #criando arquivo de saida (result)
  result <- data.frame(matrix(NA, ncol = 7, nrow = length(respostas)))
  colnames(result) <- c('respostas', 'padrao', 'ntot', 'nnao_apres', 'ntried', 'score', 'pacer')
  
  
  #contando o número de itens apresentados ao estudante
  result$score <- 0 
  
  pb_1 <- txtProgressBar(min = 0, max = length(itemgab), style = 3, width = 70)
  
  respo <- list()
  score <- list()
  codif <- list()
  
  for(i in seq_len(length.out = length(itemgab))){
    
    setTxtProgressBar(pb_1, i)
    
    item_atual <- itemgab[[i]]
    
    resposta_item_atual <- busca_resposta_item(caderno = caderno,
                                               respostas = respostas,
                                               itempos = item_atual$posicao,
                                               nao_apres = nao_apres)
    
    #separando objeto item_atual$gabarito e adicionando a linha do item nao apresentado
    gab_item_atual <- item_atual$gabarito
    gab_item_atual <- gab_item_atual[1:(nrow(gab_item_atual)+1),]
    gab_item_atual[nrow(gab_item_atual),'respostas'] <- nao_apres
    gab_item_atual[nrow(gab_item_atual),'pontuacao'] <- 0
    gab_item_atual[nrow(gab_item_atual),'codificacao'] <- nao_apres
    
    #respostas 
    respo[[i]] <- resposta_item_atual
    
    #score
    score[[i]] <- as.numeric(plyr::mapvalues(x = resposta_item_atual, 
                                             from = gab_item_atual$respostas, 
                                             to = gab_item_atual$pontuacao,
                                             warn_missing = FALSE))
    #codificacao
    codif[[i]] <- plyr::mapvalues(x = resposta_item_atual, 
                                  from = gab_item_atual$respostas, 
                                  to = gab_item_atual$codificacao,
                                  warn_missing = FALSE)
    
  }
  codif <- as.data.frame(codif)
  colnames(codif) <- names(itemgab)
  #armazenando os padroes e os scores
  result$respostas <- apply(as.data.frame(respo), 1, paste0, collapse = '')
  result$padrao <- apply(codif, 1, paste0, collapse = '')
  result$score <- apply(as.data.frame(score), 1, sum)
  
  #contabilizando itens total, nao apresentados e tentados pelo participante.
  result$ntot <- nchar(result$padrao)
  result$nnao_apres <- rowSums(codif == nao_apres)
  result$ntried <- result$ntot - result$nnao_apres 
  result$pacer <- (result$score / result$ntried)*100
  rm(respo, score, codif)
  
  class(result) <- c('escore', class(result))

  result
  
}

