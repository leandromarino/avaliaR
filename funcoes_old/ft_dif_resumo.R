# require("gdtools")
# font_list <- sys_fonts()
# font_list
# font_list$family[grep(pattern = 'Narrow', font_list$family )]


#ft_dif_resumo(
#  dif_resumo =difResumoEncc18CH_EFv1[1,],
#  ndec = 4,
#  sep = ',',
#  cores = paleta_encceja2018)


ft_dif_resumo <- function(dif_resumo, ndec = 4, sep = ',', cores){

  #definindo colunas
  cols <- c('codbni', 'nomeblg', 'a', 'b', 'c', 'tipodif_G1_x_G2', 'Mod_x_G1', 'Mod_x_G2', 'G1_x_G2')
  
  #filtrando arquivo
  aux_dif_resumo <- dif_resumo[,cols]

  titulos <- data.frame(chave = cols,  
                        colB = toupper(c('CÓD', 'NOMEBLG',
                                         rep('PARÂMETROS',3), 'GRUPO DIF',
                                         rep('DIFERENÇAS', 3))),
                        colA = toupper(c('CÓD', 'NOMEBLG', 'A', 'B', 'C', 
                                         'GP1 x GP2', 'MOD x GP1', 'MOD x GP2', 'GP1 x GP2')),
                        stringsAsFactors = FALSE)
  
  formata_ctt <- function(x, ...){
    x = avaliaR::formataNum(valor = x, ...)
    x[x == "NaN"] <- '-'
    x
  }
  
  cols_num <- c('Mod_x_G1', 'Mod_x_G2', 'G1_x_G2')
  aux_dif_resumo[,cols_num] <- formata_ctt(aux_dif_resumo[,cols_num], dig = ndec, dec = sep)
  rm(cols_num)
  cols_par <- c('a','b','c')
  aux_dif_resumo[,cols_par] <- formata_ctt(aux_dif_resumo[,cols_par], dig = ndec, dec = sep)
  rm(cols_par)
  aux_dif_resumo$codbni <- as.integer(aux_dif_resumo$codbni) 
    
  ft_dif_resumo <- flextable::regulartable(aux_dif_resumo)
  ft_dif_resumo <- flextable::set_header_df(x = ft_dif_resumo, mapping = titulos, key = 'chave')
  ft_dif_resumo <- flextable::merge_h(ft_dif_resumo, part = "header")
  ft_dif_resumo <- flextable::merge_v(ft_dif_resumo, part = "header")
  ft_dif_resumo <- flextable::theme_zebra(ft_dif_resumo, 
                                   odd_header = cores[5], odd_body = cores[1], 
                                   even_header = cores[5], even_body = cores[2])
  ft_dif_resumo <- flextable::hline(ft_dif_resumo, border = fp_border(width = 1.5, color = 'white'), part = 'all')
  ft_dif_resumo <- flextable::vline(ft_dif_resumo, border = fp_border(width = 1.5, color = 'white'), part = 'all')
  ft_dif_resumo <- flextable::align(ft_dif_resumo, align = "center", part = "all" )
  ft_dif_resumo <- flextable::fontsize(ft_dif_resumo, size = 9, part = 'header')
  ft_dif_resumo <- flextable::fontsize(ft_dif_resumo, size = 8, part = 'body')
  ft_dif_resumo <- flextable::font(ft_dif_resumo, fontname = 'Arial Narrow', part = "all")
  ft_dif_resumo <- flextable::color(ft_dif_resumo, color = 'white', part = "header")
  ft_dif_resumo <- flextable::padding(ft_dif_resumo, padding.top = .1, padding.bottom = 0.1, padding.left = 0, padding.right = 0, part = "all" )
  ft_dif_resumo <- flextable::width(ft_dif_resumo, width = 1.85/2.54)
  ft_dif_resumo
}


