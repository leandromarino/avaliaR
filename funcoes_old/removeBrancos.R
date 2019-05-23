#' Resumo de medias
#' 
#' @description 
#'     Funcao que remove os brancos à esquerda ou à direita, equivalente ao trim... 
#' 
#' @usage 
#'    removeBrancos(x)
#' 
#' @param x character ou vetor que deseja remover os espacos em branco
#' 
#' @examples 
#' removeBrancos(" a  asda asd sss " )
#' removeBrancos(c(" a  asda asd sss "," asd2      344        ")
#' 

removeBrancos <- function(x){
  x <- sub("^ +", "", x)
  x <- sub(" +$", "", x)
  x
}
