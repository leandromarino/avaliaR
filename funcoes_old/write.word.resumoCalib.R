# library(officer)
# library(flextable)

# #inputs
# gabpar = gabparEncc18CH_EMv1
# ctt = cbind(ctt_ENCCEJA_1811001_EM_CH_20180921_idprova201812[[1]], 
#             BL = as.integer(1), OB = as.integer(1:30), 
#             codbni = as.integer(gabparEncc18CH_EMv1$codbni))
# difph1 = difph1Encc18CH_EMv1
# difResumo = difResumoEncc18CH_EFv1
# dirgraf = paste0(dirEncc18,'Resultados/GraficosItens/CH_EMv1/')
# titulo = 'ENCCEJA 2018 - Ciências Humanas - Ensino Médio - BR REG - Versão v1'
# sufixo = 'EM_CHv1'
# dirsaida = paste0(dirEncc18,'Resultados/GraficosItens/')
# cores = paleta_encceja2018
# numalt = 5

#cinza <- gray(level = seq(1, 0,-.10))[-c(1,6,7,11)]

write.word.resumoCalib <- function(
  gabpar, ctt, difph1, difResumo, titulo, sufixo, 
  numalt, dirgraf, dirsaida,
  cores = c("#E6E6E6", "#CCCCCC", "#B3B3B3", "#999999", "#4C4C4C", "#333333", "#191919")
  ){
  
  grafs <- dir(dirgraf)
  grafs_codbni <- as.integer(substring(grafs, nchar(grafs) - 8, nchar(grafs) - 4))
  
  output <- officer::read_docx(path = paste0(dirsaida, 'Resumo_Template.docx')) 
  officer::styles_info(output)
  output <- officer::body_add_par(x = output, 
                                  value = titulo, 
                                  style = "heading 1",
                                  pos = 'on')
  
  output <- officer::body_add_par(x = output, value = '', style = "Normal")
  output <- officer::body_add_par(x = output, value = '', style = "Normal")
  
  output <- officer::body_add_par(x = output, value = "Arquivo gerado em:", style = "Normal")
  output <- officer::body_add_par(x = output, value = Sys.time(), style = "Normal")
  
  output <- officer::body_add_par(x = output, value = '', style = "Normal")
  
  output <- officer::body_add_break(output)
  

  itematual = 22
  for(itematual in 1:nrow(gabpar)){
    
    cat(paste0('Exportando Item : ', sprintf("%02d", itematual), ' de ', sprintf('%02d', nrow(gabpar)), '\r'))
    
    (.aux_ctt <- ctt[itematual, ])
    (.aux_gabpar <- gabpar[itematual, ])
    (.aux_grafs <- grafs[grafs_codbni == .aux_gabpar$codbni])
    
    (.aux_grafcci <- .aux_grafs[substr(.aux_grafs,1,3) == "CCI"])
    (.aux_grafcomuns <- .aux_grafs[substr(.aux_grafs,1,3) == "Com"])
    
    (.aux_difph1 <- difph1[difph1$coditem == .aux_gabpar$codbni,])
    (.aux_difResumo <- difResumo[difResumo$codbni == .aux_gabpar$codbni,])
    
    if(nrow(.aux_difResumo) > 0){
      (.verificaItem <- sum(.aux_difResumo[, c('Mod_x_G1', 'Mod_x_G2', 'G1_x_G2')] > .12) > 0)
    }else{
      .verificaItem <- FALSE
    }
    
    # inicio do item
    output <- officer::body_add_par(x = output, 
                                    value = paste0("Item: ", sprintf('%02d', itematual),
                                                   " de ", nrow(gabpar),
                                                   " | Código: ", .aux_gabpar$codbni,
                                                   ifelse(.verificaItem, ' (ITEM COM POTENCIAL PROBLEMA)', '')), 
                                    style = "heading 2")
    
    output <- officer::body_add_par(x = output, value = '', style = "Normal")
    
    ### adicionar tirinha da estatística classica
    output <- flextable::body_add_flextable(x = output,
                                            value = ft_ctt(ctt = .aux_ctt, numalt = numalt, cores = cores), 
                                            align = 'center')
    output <- officer::body_add_par(x = output, value = '', style = "Normal")
    
    ### curva caracteristica do item 
    output <- officer::body_add_img(x = output, 
                                    src = paste0(dirgraf, .aux_grafcci), 
                                    width = 16.36 / 2.54,
                                    height = 8.18 / 2.54)
    output <- officer::body_add_par(x = output, value = '', style = "Normal")
    
    if(length(.aux_grafcomuns) > 0){
      
      ### dif tct
      output <- officer::body_add_par(x = output,
                                      value = 'DIF - Teoria Clássica de Testes', 
                                      style = "Normal")
      
      output <- flextable::body_add_flextable(x = output,
                                              value = ft_dif_ph1(dif_ph1 = .aux_difph1, cores = cores), 
                                              align = 'center')
      output <- officer::body_add_par(x = output, value = '', style = "Normal")
      
      ### dif tri
      output <- officer::body_add_par(x = output, value = 'DIF - TRI', style = "Normal")
      
      output <- flextable::body_add_flextable(x = output,
                                              value = ft_dif_resumo(dif_resumo = .aux_difResumo, cores = cores), 
                                              align = 'center')
      output <- officer::body_add_par(x = output, value = '', style = "Normal")
      
      
      for(i in 1:length(.aux_grafcomuns)){
        aux <- unlist(strsplit(x = .aux_grafcomuns[i], "_"))
        if(i == 1){
          output <- officer::body_add_img(x = output, 
                                          src = paste0(dirgraf, .aux_grafcomuns[i]), 
                                          width =  7.0 / 2.54,
                                          height = 7.0 / 2.54, 
                                          style = "centered")
        }else{
          output <- officer::slip_in_img(x = output, 
                                          src = paste0(dirgraf, .aux_grafcomuns[i]), 
                                          width =  7.0 / 2.54,
                                          height = 7.0 / 2.54, 
                                          pos = 'after')
        }
      }
    }
    output <- officer::body_add_break(output)
    
  }
  
  ### adicionar graficos de itens comuns
  ### adicionar tabela de dif pct
  ### adicionar tabela de dif tri
  
  
  print(output, target = paste0(dirsaida, 'ResumoCalib_',sufixo,'.docx')) 
}

# write.word.resumoCalib(
#   gabpar = gabparEncc18CH_EMv1,
#   ctt = cbind(ctt_ENCCEJA_1811001_EM_CH_20180921_idprova201812[[1]], 
#               BL = as.integer(1), OB = as.integer(1:30), 
#               codbni = as.integer(gabparEncc18CH_EMv1$codbni)),
#   difph1 = difph1Encc18CH_EMv1,
#   difResumo = difResumoEncc18CH_EFv1, 
#   titulo = 'ENCCEJA 2018 - Ciências Humanas - Ensino Médio - BR REG',
#   sufixo = 'EM_CHv1',
#   cores = paleta_encceja2018,
#   numalt = 5,
#   dirgraf = paste0(dirEncc18,'Resultados/GraficosItens/CH_EMv1/'),
#   dirsaida = paste0(dirEncc18,'Resultados/GraficosItens/'))
