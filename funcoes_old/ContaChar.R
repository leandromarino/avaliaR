#' Conta o numero de vezes em que um conjunto de caracteres aparece
#' 
#' @description 
#' Conta o numero de vezes que qualquer um dos caracteres de \code{vecchar} aparece em
#'   uma string de texto
#'   
#' @usage ContaChar(resp,vecchar=c(" ","*","."))
#' @param resp \code{string} ou vetor de \code{strings} que sera avaliado
#' @param vecchar Vetor com os elementos que devem ser contabilizados.
#' 
#' 
#' @examples 
#' ContaChar(" ABCD*    ....ADEDD")
#' ContaChar(" ABCD*    ....ADEDD",vecchar=" ")
#' ContaChar(" ABCD*    ....ADEDD",vecchar="*")
#' ContaChar(" ABCD*    ....ADEDD",vecchar=".")
#' 
ContaChar <- function(resp,vecchar=c(" ","*",".")){
  n <- length(resp)
  nresp = nchar(resp)[1]
  contabrancos <- rep(0,n)
  for(j in 1:length(vecchar)){
    for (i in 1:nresp) contabrancos <- contabrancos + as.numeric( substring(resp,i,i) == rep(vecchar[j],n ) )  
  }
  contabrancos
}
