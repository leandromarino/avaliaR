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

GrafItemAlt <- function(PercEmp,limx,nomeprojeto,dirgraf,
         gab,it,bl,ob,itblg,nomeblg,codigo,percpr,titulo,
         item,mincategoria = 30,back='transparent'){
  PercEmp1 = PercEmp[[1]][[item]]
  PercEmp2 = PercEmp[[2]][[item]]
  gab = gab[item]
  it = it[item]
  bl = bl[item]
  ob = ob[item]
  itblg = itblg[item]
  nomeblg = nomeblg[item]
  codigo = codigo[item]
  nalt = nrow(PercEmp1)-2
  if(rowSums(PercEmp1[nalt,,drop=F])==0) nalt = nalt-1
  
  colsgraf = c(1:ncol(PercEmp1))[colSums(PercEmp1) >= mincategoria]
  fpath = paste(dirgraf,'Alt_',nomeprojeto,sprintf('%03d',it),"_",nomeblg,"_",codigo,'.png',sep='')
  
  
  png(fpath,width = 480*3, height = 480*3, bg = back, res = 72*3,pointsize=13.8)
  par(mar=c(5, 4, 5, 2) + 0.1)
  plot(0,0,xlim=limx,ylim=c(0,1),type='n',axes=F,ylab='Proporcao de Resposta',xlab='Proficiencia')
  #rect(-1000,-1000,1000,1000,col='gray90')
  title(main = titulo, line = 3,cex.main=1)
  title(main = paste('It:',it,' Bl:',bl,' Ob:',ob,' Ibg:',itblg,' Gab:',gab,sep=''),line=2,cex.main=1)
  title(main = paste('Código: ',codigo, sep=''),line=1,font.main=1,cex.main=1)
  for(i in 1:nalt){
    lines(x=as.integer(colnames(PercEmp1)[colsgraf]),y=PercEmp2[i,colsgraf],type="b", pch=LETTERS[i])
  }
  axis(1,seq(0,500,50))
  axis(2,seq(0,1,.2)) 
  abline(h=.65,lty=5)
  legend('bottomright',c('5 e 95','10 e 90','50'),title='Percentis:',lty = c(2:4),box.col='transparent',bg='transparent')
  abline(v=percpr[c(2,8)],lty=2)
  abline(v=percpr[c(3,7)],lty=3)
  abline(v=percpr[c(5)],lty=4)
  box()
  dev.off()
}
