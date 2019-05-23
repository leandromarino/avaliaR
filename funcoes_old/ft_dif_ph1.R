
# ft_dif_ph1(
#   dif_ph1 = difph1Encc18CH_EMv1[1,],
#   ndec = 3,
#   sep = ',',
#   cores = paleta_encceja2018)

ft_dif_ph1 <- function(dif_ph1, ndec = 4, sep = ',', cores){
  
  #definindo colunas
  cols <- c('coditem','itemblg', 'nomeblg', 'tipo_dif', 
            'bise_G1', 'bise_G2', 'pct_G1', 'pct_G2', 
            'dif_G1_x_G2')
  
  #filtrando arquivo
  aux_dif_ph1 <- dif_ph1[,cols]
  
  titulos <- data.frame(chave = cols,  
                        colB = toupper(c('CÓD', 'ITEMBLG', 'NOMEBLG',  'GRUPO DIF',
                                         rep('ESTATÍSTICAS', 4), rep('DIF PCT', 1))),
                        colA = toupper(c('CÓD', 'ITEMBLG', 'NOMEBLG', 'GP1 x GP2', 
                                         'BISE GP1', 'BISE GP2', 'PCT GP1', 'PCT GP2', 'GP1 - GP2')),
                        stringsAsFactors = FALSE)
  
  formata_ctt <- function(x, ...){
    x = avaliaR::formataNum(valor = x, ...)
    x[x == "NaN"] <- '-'
    x
  }
  
  cols_num <- c('bise_G1', 'bise_G2', 'pct_G1', 'pct_G2', 'dif_G1_x_G2')
  aux_dif_ph1[,cols_num] <- formata_ctt(aux_dif_ph1[,cols_num], dig = ndec, dec = sep)
  rm(cols_num)
  
  aux_dif_ph1$coditem <- as.character(aux_dif_ph1$coditem)
  aux_dif_ph1$itemblg <- as.character(aux_dif_ph1$itemblg)
  
  ft_dif_ph1 <- flextable::regulartable(aux_dif_ph1)
  ft_dif_ph1 <- flextable::set_header_df(x = ft_dif_ph1, mapping = titulos, key = 'chave')
  ft_dif_ph1 <- flextable::merge_h(ft_dif_ph1, part = "header")
  ft_dif_ph1 <- flextable::merge_v(ft_dif_ph1, part = "header")
  ft_dif_ph1 <- flextable::theme_zebra(ft_dif_ph1, 
                                          odd_header = cores[5], odd_body = cores[1], 
                                          even_header = cores[5], even_body = cores[2])
  ft_dif_ph1 <- flextable::hline(ft_dif_ph1, border = fp_border(width = 1.5, color = 'white'), part = 'all')
  ft_dif_ph1 <- flextable::vline(ft_dif_ph1, border = fp_border(width = 1.5, color = 'white'), part = 'all')
  ft_dif_ph1 <- flextable::align(ft_dif_ph1, align = "center", part = "all" )
  ft_dif_ph1 <- flextable::fontsize(ft_dif_ph1, size = 9, part = 'header')
  ft_dif_ph1 <- flextable::fontsize(ft_dif_ph1, size = 8, part = 'body')
  ft_dif_ph1 <- flextable::font(ft_dif_ph1, fontname = 'Arial Narrow', part = "all")
  ft_dif_ph1 <- flextable::color(ft_dif_ph1, color = 'white', part = "header")
  ft_dif_ph1 <- flextable::padding(ft_dif_ph1, padding.top = 0.1, padding.bottom = 0.1, padding.left = 0, padding.right = 0, part = "all" )
  ft_dif_ph1 <- flextable::width(ft_dif_ph1, width = 1.85/2.54)
  ft_dif_ph1
}



