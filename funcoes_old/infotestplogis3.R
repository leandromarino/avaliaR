#' Informacao do teste para uma grade de proficiencias
#'
#' \code{\link{Iteste}} Retorna uma matriz de niveis de proficiencia e informacoes segundo o modelo
#' logistico de tres parametros
#'
#' @param a Vetor de discriminações dos itens do teste
#' @param b vetor de dificuldades dos itens do teste
#' @param ci vetor de probabilidades de acerto casual dos itens do teste
#' @param min.theta inicio da grade (default = -4)
#' @param max.theta final da grade (default = 4)
#'
#' @return data.frame com duas colunas, theta com os valores de referencia da grade
#' e Info com as informações
#'
#' @description Informação do teste, utilizada internamente na ATA
#'
#' @export
#'
#' @encoding utf8

infotestplogis3 <- function(a,b,ci,min.theta=-4,max.theta=4){
  grid <- seq(from=min.theta,to=max.theta,by=0.1)
  Saida <- matrix(0,nrow=length(grid),ncol=1)
  for(i in 1:length(a)){
    Saida <- Saida + infoplogis3(grid,a[i],b[i],ci[i])
  }
  return(data.frame("theta"=grid,"Info"=Saida))
}
