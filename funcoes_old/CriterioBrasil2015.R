#' Sum of vector elements.
#'
#' \code{sum} returns the sum of all the values present in its arguments.
#'
#' This is a generic function: methods can be defined for it directly
#' or via the \code{\link{Summary}} group generic. For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.
#'
#' @param ... Numeric, complex, or logical vectors.
#' @param na.rm A logical scalar. Should missing values (including NaN)
#'   be removed?
#' @return If all inputs are integer and logical, then the output
#'   will be an integer. If integer overflow
#'   \url{http://en.wikipedia.org/wiki/Integer_overflow} occurs, the output
#'   will be NA with a warning. Otherwise it will be a length-one numeric or
#'   complex vector.
#'
#'   Zero-length vectors have sum 0 by definition. See
#'   \url{http://en.wikipedia.org/wiki/Empty_sum} for more details.
#' @examples
#' sum(1:10)
#' sum(1:5, 6:10)
#' sum(F, F, F, T, T)
#'
#' sum(.Machine$integer.max, 1L)
#' sum(.Machine$integer.max, 1)
#'
#' \dontrun{
#' sum("a")
#' }


CriterioBrasil2015 <- function(data,banheiroq,empregadaq,carroq,computadorq,lavaloucaq,geladeiraq,freezerq,lavaroupaq,dvdq,microondasq,motoq,
         secadoraroupaq,ensinomae,ensinopai,aguaq,pavimentoq){
  
  cols = c(banheiroq,empregadaq,carroq,computadorq,lavaloucaq,geladeiraq,freezerq,lavaroupaq,dvdq,microondasq,motoq,
           secadoraroupaq,ensinomae,ensinopai,aguaq,pavimentoq)
  
  critbr <- data[,cols]
  for(i in 1:length(cols)) critbr[,i] <- as.character(critbr[,i])
  
  ### pontuacao 
  pontuacao2015 = list(
    banheiro      = c(0,3,7,10,14),
    empregada     = c(0,3,7,10,13),
    carro         = c(0,3,5,8 ,11),
    computador    = c(0,3,6,8 ,11),
    lavalouca     = c(0,3,6,6 ,6 ),
    geladeira     = c(0,2,3,5 ,5 ),
    freezer       = c(0,2,4,6 ,6 ),
    lavaroupa     = c(0,2,4,6 ,6 ),
    dvd           = c(0,1,3,4 ,6 ),
    microondas    = c(0,2,4,4 ,4 ),
    moto          = c(0,1,3,3 ,3 ),
    secadoraroupa = c(0,2,2,2 ,2 ),
    ensinomae     = c(0,1,2,4 ,7 ),
    ensinopai     = c(0,1,2,4 ,7 ),
    agua          = c(0,4),
    pavimento     = c(0,2))
  
  
  for(i in 1:length(cols)){
    aux = pontuacao2015[[i]]
    for(j in 1:length(aux)){
      critbr[,i] <- replace(critbr[,i],critbr[,i]==LETTERS[j],aux[j])
    }
    critbr[,i] <- as.integer(critbr[,i])
  }
  
  critbr$ensino <- pmax(critbr[,ensinomae],critbr[,ensinopai])
  critbr <- critbr[,!is.element(colnames(critbr),c(ensinomae,ensinopai))]
  
  cbr_int <- rowSums(critbr)
  cbr_cat <- as.character(cbr_int )
  cbr_cat <- replace(cbr_cat,cbr_int>=45,"A")
  cbr_cat <- replace(cbr_cat,cbr_int>=38 & cbr_int <= 44,"B1")
  cbr_cat <- replace(cbr_cat,cbr_int>=29 & cbr_int <= 37,"B2")
  cbr_cat <- replace(cbr_cat,cbr_int>=23 & cbr_int <= 28,"C1")
  cbr_cat <- replace(cbr_cat,cbr_int>=17 & cbr_int <= 22,"C2")
  cbr_cat <- replace(cbr_cat,cbr_int>=0  & cbr_int <= 16,"D-E")
  
  critbr <- cbind(cbr_int,cbr_cat)
  critbr
  
}

