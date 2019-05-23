#' Probabilidade de resposta sob o modelo logistico de 3 parametros
#'
#' \code{Pij} Retorna a probabilidade de um item dicotômico correto em função da proficiencia do indivíduo,
#' discriminação, dificuldade e probabilidade de acerto casual do item
#'
#' @param theta Proficiência do indivíduo
#' @param a Discriminação do item
#' @param b Dificuldade do item
#' @param ci Parãmetro de acerto casual
#'
#' @return Probabilidade de acerto
#'
#' @description Probabilidade de acerto sob o M3PL, utilizada nas demais funções desse pacote
#'
#' @export
#'
#'
#' @encoding utf8


plogis3 <- function(theta,a,b,ci){ #3PL
  aux <- a*(theta-b)
  aux <- 1/(1+exp(-aux))
  aux <- ci + (1-ci)*aux
  return(aux)
}
