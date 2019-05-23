#' Print Values
#' 
#' @description 
#'   Função para exibir os objetos da classe info_itemgab
#'  
#'  @param x objeto da classe \code{info_itemgab}
#'  @param out vetor com o sequencial dos itens que deseja exibir 
#'     \code{Default: NULL}
#'  @param len quantidade de itens que deseja exibir. \code{Default: 5}. 
#'     Para exibir todos os itens pode utilzar \code{len = Inf}.
#'  @method print info_itemgab
#'  @S3method print info_itemgab
#' @export

print.info_itemgab <- function(x, out = NULL, len = 5){
  
  #out numero de itens a serem exibidos
  (n <- length(x))
  
  if(is.null(out)){
    itens_exib <- seq_len(min(len,n))
    if(len < n) msg <- paste0('Exibidos apenas os ',len,' primeiros itens!')
    if(len >= n) msg <- paste0('Exibidos TODOS os itens!')
  }else{
    itens_exib <- out
    msg <- paste0('Exibidos apenas os itens: ', 
                  paste0(itens_exib, sep = '', collapse = ', '),
                  '.')
  }
  
  for(i in itens_exib){
    
    info_itematual <- x[[i]]
    
    cat(paste0(c('***',rep("-", 60),'***'), sep = '', collapse = ''),
        paste0(c('\n***',rep("-", 60),'***'), sep = '', collapse = ''), 
        paste0(c('\n***',rep("-", 60),'***'), sep = '', collapse = ''),
        '\n', 'Item: ', sprintf('%03d', i), '  |  Codigo:  ', info_itematual$coditem, '\n',
        '\n', 'Tipo: ', info_itematual$tipo_item, '\n')
    
    if(!(info_itematual$tipo_item %in% c('dicotomico','politomico'))){
      cat('* * * * * * * BUG * * * * * *',
          '\n* TIPO DE ITEM NAO DEFINIDO *',
          '\n* * * * * * * * * * * * * * *')
    }
    
    if(info_itematual$tipo_item == 'dicotomico'){
      cat(' Gabarito: ', info_itematual$gabarito$respostas[info_itematual$gabarito$codificacao == 1],'\n')
      cat( '\n Esquema de pontuacao:',
           '\n   Acerto:', max(info_itematual$gabarito$pontuacao),
           '\n   Erro  :', min(info_itematual$gabarito$pontuacao))
    }
    
    
    if(info_itematual$tipo_item == 'politomico'){
      cat('\n','Esquema de pontuacao:\n')
      print(info_itematual$gabarito)
    }
    
    cat('\n\n\n')
    
  }
  cat('\n', msg, '\n')
}




# print(x, out = c(1,4))
# print(x)
# print(x, len = 36)
# print(x, len = Inf)
