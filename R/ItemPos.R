#' Cria lista para informar a posicao do item nos cadernos
#'
#' @param itens Objeto do tipo matriz com \code{rownames} e \code{colnames} definido.
#' @param nitens Numero de itens total.
#' @param tipo determina o tipo de output \code{integer} ou \code{character}
#' @return Objeto do tipo \code{list} de tamanho \code{nitens} onde cada elemento eh um item.
#'   Cada objeto da lista eh um objeto do tipo \code{numeric matrix}.
#'   Cada linha desta matriz refere-se ao caderno em que o
#'   item aparece e contem a informacao da posicao deste item no caderno.
#' @examples
#' itens_teste <- matrix(c(1:30,11:30,1:10,21:30,1:20),ncol=30,byrow = TRUE)
#' rownames(itens_teste) <- 1:nrow(itens_teste)
#' colnames(itens_teste) <- paste('it',sprintf('%02d',1:30),sep='')
#' #saida numerica
#' ItemPos(itens = itens_teste, nitens = 30,tipo='integer')
#' 
#' #saida como character
#' ItemPos(itens = itens_teste, nitens = 30,tipo='character')
#' 


ItemPos <- function(itens,nitens,tipo){


  ### criando lista com a posicao dos itens para cada um dos cadernos e blocos
  itempos <- list()

  for(i in 1:nitens){
    (aux <- itens[rowSums(itens==i)==1,,drop=FALSE])
    (itempos[[i]] <- cbind(form = rownames(aux)[1],pos=which(aux[1,]==i)))
    (it = itempos[[i]][1,'pos'])
    if(sum(unique(as.vector(itens)[duplicated(as.vector(itens))])==i)!=0){
      for(j in 2:nrow(aux)){
        itempos[[i]] <- rbind(itempos[[i]],cbind(form = rownames(aux)[j],pos=which(aux[j,]==i)))
        it = itempos[[i]][j,'pos']
      }
    }
    if(tipo=='integer') mode(itempos[[i]]) <- 'integer'
  }

  names(itempos) <- paste0('it', sprintf('%03d', 1:length(itempos)))

  itempos

}
