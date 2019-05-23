# gabpar = gabpar1805d
# colsgabpar = c('it','codit','nomeblg','atran','btran','c','gab')
# percemp = percemp1805d
# percpr = percpr1805d
# modo = 'normal'
# aesc = amat
# besc = bmat
# int = 25
# ndec = 5


quali_ajuste <- function(gabpar, colsgabpar, percemp, percpr, modo, aesc, besc, int, ndec = NULL){
  
gabpar <- gabpar[,colsgabpar]
colnames(gabpar) <- c('it','codit','nomeblg','atran','btran','c','gab')
cols <- as.character(seq(from = percpr[c(2)] %/% int * int, to = percpr[c(8)] %/% int * int, by = int))
df = data.frame(matrix(vector(), 0, length(cols)+length(colsgabpar)+4,
                       dimnames=list(c(), c(colsgabpar,'aescdivulg','bescdivulg',paste0('ponto', cols),'AjuMin','AjuMax'))),
                  stringsAsFactors=F)


  for(item in 1:nrow(gabpar)){
    
    (nmblg <- gabpar[item, 'nomeblg'])
    (codit <- gabpar[item, 'codit'])
    (gab  <- gabpar[item, "gab"])
    (para <- gabpar[item, "atran"])
    (parb <- gabpar[item, "btran"])
    (parc <- gabpar[item, "c"])
    
    D = 1
    if(modo == 'normal'){
      D = 1.7
    }
    
    par_at <- para * D / aesc
    par_bt <- aesc * parb + besc
    
    print(paste0("Analisando item - It",sprintf('%03d',item),"_",nmblg))
    
    data.frame(tag = c(cols, 'minimo','maximo'))
    df[item, 'it'] <- item
    df[item, 'nomeblg'] <- nmblg
    df[item, 'codit' ] <- codit
    df[item, 'gab'] <- gab
    df[item, c('atran','btran','c','aescdivulg','bescdivulg')] <- c(para, parb, parc, par_at, par_bt)
    pts <- as.integer(colnames(percemp[[1]][[1]]))
    
    df[item,paste0('ponto', cols)] <- abs(avaliaR::plogis3(pts, par_at, par_bt, parc) - percemp[[2]][[item]][gab,])[cols]
    df[item,c('AjuMin','AjuMax')] <- range(df[item,paste0('ponto', cols)])
    
  }
  rm(item, para, parb, parc, par_at, par_bt, nmblg, codit, pts, D, gab)
  
  df$statusAju <- ''
  df$statusAju[df$AjuMax > 0.15] <- '> 0.15 *'
  df$statusAju[df$AjuMax > 0.17] <- '> 0.17 **'
  df$statusAju[df$AjuMax > 0.20] <- '> 0.20 ***'
  
  if(!is.null(ndec)){
    df[,c(paste0('ponto', cols),'AjuMin','AjuMax')] <- round(df[,c(paste0('ponto', cols),'AjuMin','AjuMax')], ndec)
  }
  
  rm(cols)
  
  df
}

#Store(quali_ajuste, lib = funcoes, lib.loc = dirNovaEscola18)
