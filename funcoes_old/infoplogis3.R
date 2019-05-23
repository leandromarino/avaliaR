#' Informacao do item sob o modelo logistico de 3 parametros
#'
#' \code{Ii} Retorna a informação de Fisher do item com relação a proficiencia, para o modelo logístico de três parâmetros
#'
#' @param theta Proficiência do indivíduo
#' @param a Discriminação do item
#' @param b Dificuldade do item
#' @param ci Parãmetro de acerto casual
#'
#' @return Informação do item
#'
#' @description Informação do item utilizada nas demais funções do pacote
#'
#' @export
#'
#' @encoding utf8


infoplogis3 <- function(theta,a,b,ci){
  aux <- plogis3(theta,a,b,ci)
  aux <- (1-aux)/(aux)
  aux <- a^2*aux*((plogis3(theta,a,b,ci)-ci)/(1-ci))^2
  return(aux)
}
