


gera_gabarito_check <- function(){
  
  respostas <- get('respostas', envir = parent.frame())
  tipo_item <- get('tipo_item', envir = parent.frame())
  pontuacao <- get('pontuacao', envir = parent.frame())
  gabarito  <- get('gabarito' , envir = parent.frame())
  nit       <- get('nit'      , envir = parent.frame())
  
  #verificando se existem tipo_item não definidos
  (aux_tipo_item <- unique(tipo_item))
  (checagem = !(aux_tipo_item %in% c('dicotomico','politomico')))
  if(sum(checagem) > 0){
    stop(paste0("Erro 1: Existem elementos em 'tipo_item' nao definidos: '",
                paste(aux_tipo_item[checagem], sep = '', collapse = ', '),
                "'."))
  }
  rm(aux_tipo_item, checagem)
  
  #verificar se o número de elementos em cada uma das listas é compatível com o resultado.
  (tam_respostas <- length(respostas))
  (tam_tipo_item <- length(tipo_item))
  (tam_pontuacao <- length(pontuacao))
  (tam_gabarito  <- length(gabarito))
  
  #criando vetores de indicacao de itens politomicos e dicotomicos
  (item_politomico <- which(tipo_item == 'politomico'))
  (item_dicotomico <- which(tipo_item == 'dicotomico'))
  
  #verificando se precisa de pontuacao para o caso do item ser politomico
  if(length(item_politomico) > 0 & tam_pontuacao != nit){
    stop('Erro 2: Existem itens politomicos e length(pontuacao) ', tam_pontuacao, " != ", nit, " número de itens (nit).")
  }
  
  #verificando se precisa de gabarito para o caso do item ser dicotomico
  if(length(item_dicotomico) > 0 & tam_gabarito != nit){
    stop('Erro 3: Existem itens dicotomicos e length(gabarito) ', tam_gabarito, " != ", nit, " número de itens (nit).")
  }
  
  #verificando se precisa de gabarito para o caso do item ser dicotomico
  if(tam_respostas != nit){
    stop('Erro 4: Existe diferenca entre o numero de elementos de respostas e nit - length(respostas) ', tam_respostas, " != ", nit, " número de itens (nit).")
  }
  
}

