#' Calculo da Idade
#' 
#' @description 
#' Funcao para calcular a idade dos estudantes considerando uma data de deferencia
#' 
#' @usage 
#' CalculaIdade(dia,mes,ano,diaref,mesref,anoref)
#' 
#' @param dia Vetor com os dias de nascimento das pessoas (formato: dd)
#' @param mes Vetor com os meses de nascimento das pessoas (formato: mm)
#' @param ano Vetor com os anos de nascimento das pessoas (formato: aaaa)
#' @param diaref  Dia de referencia (formato: dd)
#' @param mesref Mes de referencia (formato: mm)
#' @param anoref Ano de referencia (formato: aaaa)
#' 
#' @examples 
#' 
#' CalculaIdade(dia = c(24,30), mes = c(03,03), ano = c(1986,1986), diaref=25,mesref=03,anoref=2016)

CalculaIdade <- function(dia,mes,ano,diaref,mesref,anoref){
  
  ano = as.integer(as.character(ano))
  mes = as.integer(as.character(mes))
  dia = as.integer(as.character(dia))
  
  idade = anoref-ano
  idade = ifelse(mes>mesref,idade - 1,idade)
  idade = ifelse(mes==mesref & dia>diaref,idade - 1,idade)
  idade
}
