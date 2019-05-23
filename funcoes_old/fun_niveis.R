
Nivel <- function(profi, pontosdecorte = seq(075,450,25)){
  maxlabels = length(pontosdecorte)
  pontosdecorte <- c(-Inf,pontosdecorte,Inf)
  profiniveis <- cut(profi, pontosdecorte, labels=0:maxlabels, right=FALSE)
  profiniveis
}
