# require("gdtools")
# font_list <- sys_fonts()
# font_list
# font_list$family[grep(pattern = 'Narrow', font_list$family )]


#ft_ctt(ctt = cbind(ctt_ENCCEJA_1811001_EM_CH_20180921_idprova201812[[1]][1,],
#                   BL = as.integer(1), OB = as.integer(1), codbni  = as.integer(50000)),
#       numalt = 5,
#       ndec = 2,
#       sep = ',',
#       cores = paleta_encceja2018)


ft_ctt <- function(ctt, numalt = 5, ndec = 2, sep = ',', cores){
  resp_possible <- c(LETTERS[1:numalt],"*",".")
  resp_possible2 <- c(LETTERS[1:numalt],"*"," ")
  
  #definindo colunas
  cols <- c('IT','BL','OB','codbni','GAB','DIFI','DISCR','ABAI','ACIM','BISE',
            paste('Perc',resp_possible,sep = ''),
            paste('Bise',resp_possible,sep = ''))
  
  .temp <- setdiff(cols, names(ctt))
  if(length(.temp) > 0){
    stop(paste0('Faltam colunas em CTT: ', paste0(.temp, collapse = ', ')))
  }
  
  ctt <- ctt[,cols]
  npossible = length(resp_possible2)
  
  titulos <- data.frame(chave = cols,  
                        colB = toupper(c(rep('INDICES',10), 
                                 rep('PERCENTUAIS DE RESPOSTAS',npossible), 
                                 rep('COEFICIENTES BISSERIAIS',npossible))),
                        colA = toupper(c('IT', 'BL', 'OB', 'CÓD.', 'GAB', 'DIFI',
                                         'DISC', 'ABAI', 'ACIM', 'BISE',
                                         resp_possible2, resp_possible2)),
                        stringsAsFactors = FALSE)

  formata_ctt <- function(x){
  x = avaliaR::formataNum(valor = x, dig = ndec, dec = sep)
  x[x == "NaN"] <- '-'
  x
  }
cols_num <- c('DIFI','DISCR','ABAI','ACIM','BISE',
              paste('Perc',resp_possible,sep = ''),
              paste('Bise',resp_possible,sep = ''))

ctt[,cols_num] <- formata_ctt(ctt[,cols_num])

  ft_ctt <- flextable::regulartable(ctt)
  ft_ctt <- flextable::set_header_df(x = ft_ctt, mapping = titulos, key = 'chave')
  ft_ctt <- flextable::merge_h(ft_ctt, part = "header")
  ft_ctt <- flextable::theme_zebra(ft_ctt, 
                        odd_header = cores[7], odd_body = cores[1], 
                        even_header = cores[5], even_body = cores[2])
  ft_ctt <- flextable::hline(ft_ctt, border = fp_border(width = 1.5, color = 'white'), part = 'all')
  ft_ctt <- flextable::vline(ft_ctt, border = fp_border(width = 1.5, color = 'white'), part = 'all')
  ft_ctt <- flextable::align(ft_ctt, align = "center", part = "all" )
  ft_ctt <- flextable::fontsize(ft_ctt, size = 9, part = 'all')
  ft_ctt <- flextable::fontsize(ft_ctt, i = 2, size = 8, part = 'header')
  ft_ctt <- flextable::font(ft_ctt, fontname = 'Arial Narrow', part = "all")
  ft_ctt <- flextable::color(ft_ctt, color = 'white', part = "header")
  ft_ctt <- flextable::padding(ft_ctt, padding.top = 2, padding.bottom = 2, padding.left = 0, padding.right = 0, part = "all" )
  ft_ctt <- flextable::width(ft_ctt, j = c(1:3), width = 0.6/2.54)
  ft_ctt <- flextable::width(ft_ctt, j = c(5), width = 0.7/2.54)
  ft_ctt <- flextable::width(ft_ctt, j = c(4), width = 1./2.54)
  ft_ctt <- flextable::width(ft_ctt, j = c(6:10), width = 0.75/2.54)
  ft_ctt <- flextable::width(ft_ctt, j = c(11:ncol(ctt)), width = 0.75/2.54)
  ft_ctt
}


