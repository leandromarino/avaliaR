GrafItemCom4 <- function(dirgrf, nomgrf, grupos, itcom, colsitcom,
                         gabpar1, colsgabpar1, gabpar2, colsgabpar2,
                         rj1, rj2, percpr1, percpr2,
                         proj, anodisc, modo = 'normal', rect = TRUE)
{
  
  ## 
  # dirgrf = paste0(dirBra16PT,'Resultados\\GraficosV1\\M02\\')
  # nomgrf = 'CCI'
  # grupos = c('BRA15','PT16')
  # itcom = itcomBra15PT1602M
  # colsitcom = c('codigoItem','nomeblg','itemblg','gab','BRA15','PT16')
  # gabpar1 = gabparBra1502M
  # colsgabpar1 <- c('codigo','nomeblg','it','bl','ob','aorig','borig','c')
  # gabpar2 <- gabparPT1602M
  # colsgabpar2 <- c('codigoItem','nomeblg','seq','bl','ob')
  # rj1 = expBra1502Mv1
  # rj2 = expBraPT1602Mv1
  # percpr1 = percprBra1502Mv1
  # percpr2 = percprBraPT1602Mv1
  # proj = 'Pré-teste Fund. Bradesco 2016'
  # anodisc = '2º ano EF - Matemática'
  # modo = 'normal'
  # rect = TRUE
  
  gabpar1 <- gabpar1[,colsgabpar1]
  colnames(gabpar1) <- c('codigo','nomeblg','it','bl','ob','a','b','c')
  rownames(gabpar1) <- gabpar1$nomeblg
  
  gabpar2 <- gabpar2[,colsgabpar2]
  colnames(gabpar2) <- c('codigo','nomeblg','it','bl','ob')
  rownames(gabpar2) <- gabpar2$nomeblg
  
  itcom <- itcom[,colsitcom]
  colnames(itcom) <- c('codigo','nomeblg','itemblg','gab','itg1','itg2')
  rownames(itcom) <- itcom$nomeblg
  D = 1
  if(modo == 'normal'){
    D = 1.7
  }
  xq <- rj1[1,]
  rj1 <- rj1[-1,]
  rj2 <- rj2[-1,]
  analiseDif <- list()
  
  liminf <- max(percpr1[2],percpr2[2])
  limsup <- min(percpr1[8],percpr2[8])
  compara <- colnames(xq)[xq>=liminf & xq<=limsup]
  analiseDif[[1]] <- xq[,compara]
  
  for(i in 2:4) analiseDif[[i]] <- list()
  names(analiseDif) <- c('Quadraturas','InfoItem','Proporcoes','Dif')
  
  nitemcomum <- nrow(itcom)
  
  for(item in 1:nitemcomum)
  {
    nmblg <- itcom[item,'nomeblg']
    codit <- itcom[nmblg,'codigo']
    (para <- gabpar1[nmblg,"a"])
    (parb <- gabpar1[nmblg,"b"])
    (parc <- gabpar1[nmblg,"c"])
    
    print(paste0("Analisando item - It",sprintf('%03d',item),"_",nmblg))
    #criando arquivo .png
    
    
    png(paste0(dirgrf,nomgrf,'_',sprintf("%03d",item),"_",nmblg,"_",codit,".png"), width = 480*3, height = 480*3, pointsize = 12*2.5)
    plot(0,0,ylim = c(0,1), xlim = c(-5,5), ylab = 'Probabilidade', xlab = 'Proficiencia', type='n')
    rect(xleft = -10, ybottom = -1, xright = 10, ytop = 2, col = 'gray90')
    curve(avaliaR::plogis3(x, para*D, parb, parc), add = TRUE)
    title(paste(proj,'-',anodisc), outer = T, line = -1, cex=1.5)
    title(substitute(bold('It: ')~Cit~bold('  Ibg:')~CIbg~bold('  Nbg:')~CNbg~bold('  Gab:')~Cgab~bold('  Cod: ')~CCod,
                     list(Cit=sprintf("%03d",item),
                          CIbg=sprintf("%03d",as.integer(itcom[nmblg,'itemblg'])),
                          CNbg=itcom[nmblg,'nomeblg'],
                          Cgab=itcom[nmblg,'gab'],
                          CCod=itcom[nmblg,'codigo'])), line = -2, outer = T)
    title(substitute(bold(~g1)~bold('  It:')~it1~bold('  Bl:')~bl1~bold('  Ob:')~ob1~"   |-----|   "~
                       bold(~g2)~bold('  It:')~it2~bold('  Bl:')~bl2~bold('  Ob:')~ob2,
                     list(g1 = grupos[1],
                          it1 = sprintf('%03d',gabpar1[nmblg,'it']),
                          bl1 = gabpar1[nmblg,'bl'],
                          ob1 = gabpar1[nmblg,'ob'],
                          g2 = grupos[2],
                          it2 = sprintf('%03d',gabpar2[nmblg,'it']),
                          bl2 = gabpar2[nmblg,'bl'],
                          ob2 = gabpar2[nmblg,'ob'])),
          line = -3, outer = T)
    points(xq[paste("V",1:40,sep='')],rj1[nmblg,paste("V",1:40,sep='')],pch=1)
    points(xq[paste("V",1:40,sep='')],rj2[nmblg,paste("V",1:40,sep='')],pch=16)
    abline(v=percpr1[2],lty=5)
    abline(v=percpr1[8],lty=5)
    abline(v=percpr2[2],lty=3)
    abline(v=percpr2[8],lty=3)
    abline(h=0.65,lty=2)
    legend('topleft',pch = c(1,16),grupos,bty="n")
    box()
    dev.off()
    
    
    taux <- intersect(colnames(gabpar1),colnames(gabpar2))
    analiseDif[[2]][[item]] <- rbind(gabpar1[nmblg,taux],gabpar2[nmblg,taux])
    analiseDif[[2]][[item]]$a <- analiseDif[[2]][[item]]$b <- analiseDif[[2]][[item]]$c <- NA
    analiseDif[[2]][[item]][,c('a','b','c')] <- gabpar1[nmblg,c('a','b','c')]
    analiseDif[[2]][[item]]$grupo <- grupos
    rownames(analiseDif[[2]][[item]]) <- grupos
    
    
    ### separa proporcoes
    props <- rbind(avaliaR::plogis3(unlist(analiseDif[[1]]), para*D, parb, parc),
                   rj1[nmblg,compara],
                   rj2[nmblg,compara])
    rownames(props) <- c('Modelo',grupos)
    analiseDif[[3]][[item]] <- props
    
    difs <- rbind(abs(props[1,] - props[2,]),
                  abs(props[1,] - props[3,]),
                  abs(props[2,] - props[3,]))
    difs$maxDif <- apply(difs,1,max)
    
    analiseDif[[2]][[item]][,paste0('Mod_x_',grupos[1])] <- NA
    analiseDif[[2]][[item]][,paste0('Mod_x_',grupos[2])] <- NA
    analiseDif[[2]][[item]][,paste0(grupos[1],'_x_',grupos[2])] <- NA
    
    analiseDif[[2]][[item]][1,c(paste0('Mod_x_',grupos[1]),paste0('Mod_x_',grupos[2]),paste0(grupos[1],'_x_',grupos[2]))] <- as.vector(apply(difs,1,max))
    analiseDif[[2]][[item]][2,c(paste0('Mod_x_',grupos[1]),paste0('Mod_x_',grupos[2]),paste0(grupos[1],'_x_',grupos[2]))] <- as.vector(apply(difs,1,max))
    
    difs$comparacao <- c(paste0('Modelo x ',grupos[1]),paste0('Modelo x ',grupos[2]),paste0(grupos[1],' x ',grupos[2]))
    rownames(difs) <- 1:nrow(difs)
    difs <- difs[,c('comparacao','maxDif',compara)]
    analiseDif[[4]][[item]]  <- difs
  }
  
  for(i in 2:4) names(analiseDif[[i]]) <- paste0('it',sprintf('%03d',1:nitemcomum),"_",itcom$nomeblg)
  
  temp <- do.call(rbind,analiseDif$InfoItem)
  temp1 <- temp[temp$grupo == grupos[1],colnames(temp)!='grupo']
  temp2 <- temp[temp$grupo == grupos[2],colnames(temp)!='grupo']
  colnames(temp1)[3:5] <- paste0(grupos[1],c("it","bl","ob"))
  colnames(temp2)[3:5] <- paste0(grupos[2],c("it","bl","ob"))
  
  temp <- cbind(temp1[,c(intersect(colnames(temp1),colnames(temp2)),paste0(grupos[[1]],c("it","bl","ob")))],
                temp2[,paste0(grupos[2],c("it","bl","ob"))])
  rownames(temp) <- 1:nrow(temp)
  analiseDif$InfoItem <- temp
  analiseDif
}
