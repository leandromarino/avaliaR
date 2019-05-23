#' Funcao que gera lista de gabaritos e pontuacao para o calculo do escore e estatisticas classicas.
#' 
#' @description 
#'    Esta funcao tem por objetivo gerar um objeto do tipo list que sera utilizado
#'    dentro da funcao escore e tambem da funcao de tct. 
#'  
#'  @param coditem vetor com códigos dos itens.
#'  @param respostas objeto do tipo \code{list} onde cada elemento é o conjunto
#'     de opções de respostas possíveis. \code{Ex.: c(LETTER[1:4], '*', 'c')}.
#'  @param tipo_item objeto do tipo \code{vector} indicando para cada item se ele é 
#'     \code{'dicotomico'} ou \code{'politomico'}.
#'  @param pontuacao objeto do tipo \code{list} indicando a pontuacao para os casos de 
#'     itens politômicos. Em caso de item dicotômico declarar NULL.
#'     Exemplo: \code{list(NULL, c(0.0, 0.5, 1.0))}
#'  @param gabarito objeto do tipo \code{vetor} indicando os gabaritos para os itens 
#'     dicotômicos. Em caso de item politômico declarar NULL. Exemplo: \code{c("A", NULL)}
#'  @param nit Número de itens que estão sendo adicionados ou criados com essa função.
#'     o nit deve ser igual ao número de elementos em cada um dos parâmetros anteriores. 
#'     Observação: caso só existam itens dicotômicos então \code{pontuacao} 
#'     pode não ser declarada. Default: \code{NULL}
#'     Observação: caso só existam itens politômicos então \code{gabarito} 
#'     pode não ser declarada. Default: \code{NULL}
#'  @param data parâmetro opcional. Default: \code{NULL}. Caso esteja adicionando 
#'     novos itens à um gabarito já existente então o gabarito existente pode
#'     ser definido em \code{data = gabarito_existente}. Assim, os itens serão 
#'     acrescentados após o \code{gabarito_existente}.
#'  
#' @examples 
#' gera_gabarito(coditem = paste0("CODIGO",sprintf("%05d", 1:5)),
#'               respostas = c(list(c(LETTERS[1:3])), rep(list(c(LETTERS[1:4], '*', '.')), 3), list(c(LETTERS[3:1]))),
#'               tipo_item = c('politomico',rep('dicotomico', 3), 'politomico'), 
#'               pontuacao = list(c(0.0,0.5,1.0), c(0, 1), c(0, 0.5), NULL, c(0,1,2)),
#'               gabarito = c(NA,"A","C","B",NA),
#'               nit = 5,
#'               data = NULL)
#' @export



gera_gabarito <- function(coditem, respostas, tipo_item, pontuacao = NULL, gabarito = NULL, nit, data = NULL){

  #verificacoes de seguranca
  gera_gabarito_check()
  
  #definindo itens politomicos e dicotomicos
  (item_politomico <- which(tipo_item == 'politomico'))
  (item_dicotomico <- which(tipo_item == 'dicotomico'))
  
  (respostas_dicotomico <- respostas[item_dicotomico])
  (gabarito_dicotomico <- gabarito[item_dicotomico])
  (pontuacao_dicotomico <- pontuacao[item_dicotomico])
  
  (respostas_politomico <- respostas[item_politomico])
  (pontuacao_politomico <- pontuacao[item_politomico])
  
  if(is.null(data)){
    data <- list()
    nit_data <- 0
  }else{
    nit_data <- length(data)  
  }
  
  
  if(length(item_dicotomico > 0)){
    
    data <- pontuacao.dicotomico(respostas = respostas_dicotomico, 
                                 gabarito = gabarito_dicotomico,
                                 pontuacao = pontuacao_dicotomico,
                                 seq_item = nit_data + item_dicotomico, 
                                 data = data)
    
  }
  
  
  if(length(item_politomico > 0)){
    
    data <- pontuacao.politomico(respostas = respostas_politomico, 
                                 pontuacao = pontuacao_politomico, 
                                 seq_item = nit_data + item_politomico, 
                                 data = data)
    
    
  }
  
  
  data <- data[order(names(data))]
  
  cat('\n')
  
  data <- list(gabarito = data, coditem = coditem, tipo_item = tipo_item)
  
  data
  
}
# 
# gera_gabarito(coditem = paste0("CODIGO_",sprintf("%05d", 1:5)),
#               respostas = c(list(c(LETTERS[1:3])), rep(list(c(LETTERS[1:4], '*', '.')), 3), list(c(LETTERS[3:1]))),
#               tipo_item = c('politomico',rep('dicotomico', 3), 'politomico'),
#               pontuacao = list(c(0.0,0.5,1.0), c(0, 1), c(0, 0.5), NULL, c(0,1,2)),
#               gabarito = c(NA,"A","C","B",NA),
#               nit = 5,
#               data = NULL)
