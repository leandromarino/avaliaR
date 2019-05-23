
write.ctt1 <- function (ctt, gabpar = NULL, colsgabpar = NULL, etapa = NULL, area = NULL, exporta_dir) 
{
  nmctt <- ctt
  
  ctt <- get(ctt)
  if (!is.null(gabpar)) {
    if (!is.null(colsgabpar)) {
      if (sum(is.element(colnames(gabpar), colsgabpar)) != 
          length(colsgabpar)) 
        stop("Alguma coluna de gabpar não está corretamente definida em colsgabpar")
    }
    if (is.null(colsgabpar)) {
      warning("Nao foram informadas as colunas de gabpar - retornando CTT com todas as colunas de gabpar")
      colsgabpar <- colnames(gabpar)
    }
  }
  
  cols_remove <- c("PBISE", "PBiseA", "PBiseB", "PBiseC", "PBiseD", "PBise ", "PBise*")
  for (i in c(1, 3)) ctt[[i]] <- ctt[[i]][, !is.element(colnames(ctt[[i]]), cols_remove)]
  
  
  
  # nao me lembro o pq disso. verificar no encceja17         
  # if (is.null(gabpar)) {
  #   ctt[[1]] <- ctt[[1]]
  #   ctt[[3]] <- cbind(gabpar[, colsgabpar], ctt[[1]])
  # }

    if (!is.null(gabpar)) {
    ctt[[1]] <- cbind("Etapa" = etapa, "Área conhecimento" = area, gabpar[, colsgabpar], ctt[[1]])
    ctt[[3]] <- cbind("Etapa" = etapa, "Área conhecimento" = area, gabpar[as.integer(ctt[[3]]$IT), colsgabpar], ctt[[3]])
  }
  xlsx::write.xlsx(ctt[[1]], paste0(exporta_dir, nmctt, ".xlsx"), 
                   sheetName = "Estatisticas", showNA = FALSE, row.names = FALSE)
  xlsx::write.xlsx(ctt[[3]], paste0(exporta_dir, nmctt, ".xlsx"), 
                   sheetName = "VerificarItens", showNA = FALSE, row.names = FALSE, 
                   append = TRUE)
}


