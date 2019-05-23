
Graficos1ppB <- function(percemp, gabpar, colsgabpar, percmodelo, percpr, item, titulo, aesc, besc,
                         s12, m12,
                         modo = 'normal', mincategoria = 30, limx = c(0,500), int  = 100, back = 'transparent',
                         retangulo = F, plot = 'all'){

    if(!(plot %in% c('all','alt','tri'))){
    stop("Os tipos permitidos para plot são: \n 'all'  = (2 graf), \n 'alt' = (para o grafico de alternativas) \n 'tri' = (para o grafico com a curva tri)")
  }
  
  gabpar <- gabpar[,colsgabpar]
  colnames(gabpar) <- c('it','bl','ob','itemblg','nomeblg','codigo','gab','a','b','c','aban')
  
  PercEmp1 = percemp[[1]][[item]]
  PercEmp2 = percemp[[2]][[item]]
  
  # selecionando as colunas validas, ou seja com mais de 30 respondentes
  colsgraf = c(1:ncol(PercEmp1))[colSums(PercEmp1) >= mincategoria]
  
  gab     = gabpar[item,'gab'    ]
  it      = gabpar[item,'it'     ]
  bl      = gabpar[item,'bl'     ]
  ob      = gabpar[item,'ob'     ]
  itemblg = gabpar[item,'itemblg']
  nomeblg = gabpar[item,'nomeblg']
  codigo  = gabpar[item,'codigo' ]
  aban    = gabpar[item,'aban'   ]
  nalt    = nrow(PercEmp1) - 2
  
  if(is.null(percmodelo)){
    xq = 0
    rj = NA
  }else{
    xq = percmodelo[1,paste("V",1:40,sep='')]
    rj = percmodelo[item + 1,paste("V",1:40,sep='')]
    xq <- aesc * (s12*xq + m12) + besc
  }
  
  parm = gabpar[item,letters[1:3]]
  D = 1
  D = ifelse(modo=='normal',1.7,1)
  
  rt <- PercEmp2[ifelse(gab=='X',1,gab),colsgraf,drop=F]
  
  # retirando a ultima linha valida do percEmp caso a soma de escolha seja igual a 0
  if(rowSums(PercEmp1[nalt,,drop = F]) == 0) nalt = nalt - 1
  
  # zerando rj e rt caso gabarito seja nulo!
  if(gab=="X") rt[1,] <- 0
  if(gab=="X") rj[1,] <- 0
  
  # a e b na escala original
  par_a <- gabpar[item,'a']
  par_b <- gabpar[item,'b']
  par_c <- gabpar[item,'c']
  
  # a e b na escala
  par_at <- gabpar[item,'a'] * D / aesc
  par_bt <- aesc * gabpar[item,'b'] + besc
  
  fun_rect <- function(corfundo='gray90',corlinha='white'){
    rect(-1000,-1000,1000,1000,col=corfundo)
    abline(v=seq(limx[1],limx[2],int),lty=3,col=corlinha,lwd=1)
    abline(h=seq(0,1,.2),lty=3,col=corlinha,lwd=1)
  }
  
  
  # definindo layout dos graficos de saida
  
  if(plot == 'all'){
    layout(matrix(c(1,1,2,3,4,4), 3, 2, byrow = TRUE),widths=c(1,1), heights=c(2,9.5,1.2))
  }
  if(plot %in% c('tri','alt')){
    layout(matrix(c(1,2,3), 3, 1, byrow = TRUE),widths=c(1,1), heights=c(2,9.5,1.2))
  }


    #grafico 1 = nada (titulo)
    par(mar=c(0,0,0,0))
    plot(0,0,xlim = c(limx[1]-int/5,limx[1]+int/5), ylim = c(0,1), type = 'n', axes = F, ylab = '', xlab = '')
    
    if(plot == 'tri'| plot == 'all'){
      #grafico 2 = tri
      par(mar = c(4,4,0,0)+0.1)
      plot(0,0,xlim=limx,ylim=c(0,1),type='n',axes=F,ylab='Proporcao de Resposta',xlab='Proficiencia')
      #colocando o retangulo cinza
      if(retangulo==T) fun_rect()
      if(par_a!=0) curve(avaliaR::infoplogis3((x-besc)/aesc,par_a*D,par_b,par_c),type="l",add=T,col='gray50')
      curve(avaliaR::plogis3(x, par_at, par_bt, par_c),add=T)
      points(xq,rj,pch=0)
      points(as.numeric(colnames(rt)),rt,pch=15)
      abline(h=.65,lty=5)
      axis(1,seq(limx[1],limx[2],int))
      axis(2,seq(0,1,.2))
      abline(v=percpr[c(2,8)],lty=2)
      abline(v=percpr[c(3,7)],lty=3)
      abline(v=percpr[c(5)],lty=4)
      if(aban == 1){
        text(x = sum(limx)/2, y = 0.5, 'X', cex = 8, col = 'red', font = 2)
      }
      box()
    }
    
    if(plot == 'alt' | plot == 'all'){
      #grafico 3 = alternativa
      par(mar = c(4,4,0,0)+0.1)
      plot(0,0,xlim = limx, ylim = c(0,1), type = 'n', axes = F, ylab = 'Proporcao de Resposta', xlab = 'Proficiencia')
      #colocando o retangulo cinza
      if(retangulo==T) fun_rect()
      for(i in 1:nalt){
        lines(x=as.numeric(colnames(PercEmp1)[colsgraf]),y=PercEmp2[i,colsgraf],type="b", pch=LETTERS[i])
      }
      axis(1,seq(limx[1],limx[2],int))
      axis(2,seq(0,1,.2))
      abline(h=.65,lty=5)
      abline(v=percpr[c(2,8)],lty=2)
      abline(v=percpr[c(3,7)],lty=3)
      abline(v=percpr[c(5)],lty=4)
      box()
    }
    
    
    ##grafico 4 = nada
    par(mar=c(0,0,0,0))
    plot(0,0,xlim = limx, ylim = c(0,1), type = 'n', axes = F, ylab = '', xlab = '')
    legend('center',c('5 e 95','10 e 90','50'),title='Percentis:',lty = c(2:4),box.col='transparent',bg='gray95' ,ncol=3)
    
    
    ### titulo do grafico
    title(titulo, outer=TRUE ,line =-1.5 , cex.main=1.5,font=4)
    
    title(substitute(bold('It: ')~Cit~bold('  Bl:')~Cbl~
                       bold('  Ob:')~Cob~bold('  Ibg:')~CIbg~
                       bold('  Nbg:')~CNbg~bold('  Gab:')~Cgab~bold('  Cod: ')~CCod,
                     list(Cit=sprintf("%03d",it),
                          Cbl=sprintf("%02d",bl),
                          Cob=sprintf('%02d',ob),
                          CIbg=sprintf("%03d",as.integer(itemblg)),
                          CNbg=nomeblg,
                          Cgab=gab,
                          CCod=codigo)),
          outer=T,line=-3,cex.main=1.5)
    
    formata <- function(valor,dig,dec=',') format(round(valor,digits=dig),ndigits=dig,nsmall=dig,dec=dec)
    
    title(substitute(bold("a=")~Cat~bold("  b=")~Cbt~bold("  c=")~Cct~
                       italic("  Parametros originais (modo logistico):")~
                       bold("  a=")~Ca~bold("  b=")~Cb~bold("  c=")~Cc,
                     list(Cat= formata(par_at,2),
                          Cbt= formata(par_bt,2),
                          Cct= formata(par_c,2),
                          Ca = formata(par_a*D,5),
                          Cb = formata(par_b,5),
                          Cc = formata(par_c,5))),
          line=-4.8,font.main=1, cex.main=1.5,outer=T)
  }
  
#Store(Graficos1ppB, lib = funcoes, lib.loc = dirNovaEscola18)


