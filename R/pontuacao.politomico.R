#' Funcao que gera \code{data.frame} de pontuacao para itens politômicos
#' 
#' @description 
#'    Esta funcao tem por objetivo gerar um objeto do tipo list que sera utilizado
#'    dentro da funcao escore e tambem da funcao de TCT. Função apenas para a geração 
#'    pontuação de itens dicotômicos.
#'  
#'  @param respostas objeto do tipo \code{list} onde cada elemento é o conjunto
#'     de opções de respostas possíveis. \code{Ex.: c(LETTER[1:4], '*', 'c')}.
#'  @param gabarito objeto do tipo \code{vetor} indicando os gabaritos para os itens 
#'     dicotômicos. Em caso de item politômico declarar NULL. Exemplo: \code{c("A", NULL)}]
#'  @param pontuacao Se \code{NULL} então será considerada a pontuacao \code{0} para a 
#'     alternativa errada e \code{1} para a alternativa certa. Podem ser atribuídas pontuações
#'     diferentes para itens dicotômicos distintos. \code{list(c(0,1),c(-1,1),c(0,0.5), NULL} para um 
#'     caso de quatro itens onde o primeiro errado = 0 e acerto = 1; o segundo errado = -1 e
#'     acerto = 1; o terceiro errado = 0 e acerto = 0.5; o último item com pontuacao NA será tratado
#'     como 0 erro e 1 acerto.
#'  @param seq_item Indica o sequencial do item caso o item tenha um sequencial 
#'     diferente de \code{1:length(gabarito)} \code{Default: NULL}. Argumento opcional e
#'     somente declarado dentro da funcao \code{gera_gabarito}.
#'  @param data parâmetro opcional. Default: \code{NULL}. Caso esteja adicionando 
#'     novos itens à um gabarito já existente então o gabarito existente pode
#'     ser definido em \code{data = gabarito_existente}. Assim, os itens serão 
#'     acrescentados após o \code{gabarito_existente}.
#'  
#' @export

pontuacao.politomico <- function(respostas, pontuacao, seq_item = NULL, data = NULL){
  
  #criando data, caso necessário
  if(is.null(data)) data <- list()
  nit_data <- length(data)
  
  # se seq_item não é definido então assume 1:tamanho(gabarito)
  if(is.null(seq_item)) seq_item <- 1:length(respostas)
  
  for(item_atual in 1:length(respostas)){
    
    (pontuacao_item_atual <- pontuacao[[item_atual]])
    (respostas_item_atual <- respostas[[item_atual]])
    
    if(is.null(pontuacao_item_atual)) stop(paste0('pontuacao.dicotomico - Erro 1: O item: ', seq_item[item_atual], ' possui pontuacao: NULL'))
    
    if(length(pontuacao_item_atual) != length(respostas_item_atual)){
      stop(paste0('pontuacao.dicotomico - Erro 2: O item: ', seq_item[item_atual], ' possui length(pontuacao) != length(respostas)'))
    }
    cat(paste0("\r Criando matriz de gabarito e pontuação para o item politomico: ", item_atual, '\r'))
    
    (nit_data_atual <- nit_data + item_atual)
    
    #criando matriz de respostas com pontuacao zerada
    (matriz_base <- data.frame(respostas = respostas_item_atual, pontuacao = pontuacao_item_atual, stringsAsFactors = FALSE))
    
    #criando coluna de codificacao
    matriz_base$codificacao <- as.integer(factor(matriz_base$pontuacao),
                                          levels = unique(matriz_base$pontuacao),
                                          labels = 1:length(unique(matriz_base$pontuacao)))-1
    
    if(max(matriz_base$codificacao) > 8){ stop( 'Nao pode haver codificacao maior do que 8')}
    
    #salvando matriz de pontuacao
    (data[[nit_data_atual]] <- matriz_base)
    
    #nomeando o item
    names(data)[nit_data_atual] <- paste0('it', sprintf("%03d", seq_item[item_atual]))
  }
  data
}


