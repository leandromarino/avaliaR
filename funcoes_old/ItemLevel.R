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

ItemLevel <- function (EmpPerc, gabpar, colsgabpar, num_min = 50, min_break = NULL, max_break = NULL, 
                       ndec = 2) 
{
  
  gab <- gabpar[,colsgabpar[1]]
  aban <- gabpar[,colsgabpar[2]]
  aban_motivo <- gabpar[,colsgabpar[3]]
  
  (aux <- EmpPerc[[1]][[1]])
  min_breakb <- as.numeric(colnames(aux)[1])
  max_breakb <- as.numeric(colnames(aux)[ncol(aux)])
  (if (!is.null(min_break)) {
    min_breakb <- min_break
  })
  (if (!is.null(max_break)) {
    max_breakb <- max_break
  })
  (by_break <- as.numeric(colnames(aux)[2]) - as.numeric(colnames(aux)[1]))
  (breaks <- seq(min_breakb, max_breakb, by_break))
  (qtd_breaks <- length(breaks))
  (nitems <- length(gab))
  (qtd_cols <- ncol(EmpPerc[[1]][[1]]))
  tmpItemLevel <- list()
  tmpItemLevel[[1]] <- tmpItemLevel[[2]] <- tmpItemLevel[[3]] <- data.frame(matrix(NA, 
                                                                                   ncol = (qtd_cols + 2), nrow = nitems))
  colnames(tmpItemLevel[[1]]) <- colnames(tmpItemLevel[[3]]) <- colnames(tmpItemLevel[[2]]) <- c("It", 
                                                                                                 "Gab", colnames(aux))
  tmpItemLevel[[3]]$It <- tmpItemLevel[[1]]$It <- tmpItemLevel[[2]]$It <- 1:nitems
  tmpItemLevel[[3]]$Gab <- tmpItemLevel[[1]]$Gab <- tmpItemLevel[[2]]$Gab <- gab
  auxcols = colnames(tmpItemLevel[[1]][, -c(1:2)])
  for (i in 1:nitems) {
    tmpItemLevel[[1]][i, -c(1:2)] <- colSums(EmpPerc[[1]][[i]][, 
                                                               auxcols])
    if (gab[i] == "X") {
      tmpItemLevel[[2]][i, -c(1:2)] <- tmpItemLevel[[3]][i, 
                                                         -c(1:2)] <- 0
    }
    else {
      tmpItemLevel[[2]][i, -c(1:2)] <- EmpPerc[[1]][[i]][match(gab[i], 
                                                               LETTERS), auxcols]
      tmpItemLevel[[3]][i, -c(1:2)] <- EmpPerc[[2]][[i]][match(gab[i], 
                                                               LETTERS), auxcols]
    }
  }
  for (i in 1:(qtd_breaks + 2)) {
    tmpItemLevel[[1]][is.nan(tmpItemLevel[[1]][, i]), i] <- NA
    tmpItemLevel[[2]][is.nan(tmpItemLevel[[2]][, i]), i] <- NA
    tmpItemLevel[[3]][is.nan(tmpItemLevel[[3]][, i]), i] <- NA
  }
  tmpItemLevel[[4]] <- tmpItemLevel[[3]]
  tmpItemLevel[[4]][, -c(1:2)] <- data.frame(matrix(as.numeric(tmpItemLevel[[3]][, 
                                                                                 -c(1:2)] >= 0.65), nrow = nitems))
  tmpItemLevel[[1]]$level <- tmpItemLevel[[2]]$level <- tmpItemLevel[[3]]$level <- tmpItemLevel[[4]]$level <- NA
  for (i in 1:nitems) {
    (a1 <- tmpItemLevel[[1]][i, -c(1, 2)])
    (level_min <- as.integer((names(a1)[a1 >= num_min])[1]))
    (level_max <- as.integer((names(a1)[a1 >= num_min])[sum(a1 >= 
                                                              num_min, na.rm = T)]))
    (if (!is.null(max_break)) {
      level_max = max_break
    })
    (if (!is.null(min_break)) {
      level_min = min_break
    })
    (level_breaks <- seq(level_min, level_max, by = by_break))
    (auxItemLevel1 <- tmpItemLevel[[1]][i, paste(level_breaks, 
                                                 sep = "")])
    (auxItemLevel2 <- tmpItemLevel[[2]][i, paste(level_breaks, 
                                                 sep = "")])
    (auxItemLevel3 <- tmpItemLevel[[3]][i, paste(level_breaks, 
                                                 sep = "")])
    (auxItemLevel4 <- tmpItemLevel[[4]][i, paste(level_breaks, 
                                                 sep = "")])
    (aux <- which(auxItemLevel4 == 0) + 1)
    if (length(aux[length(aux)]) == 0) {
      tmpItemLevel[[1]]$level[i] <- tmpItemLevel[[2]]$level[i] <- tmpItemLevel[[3]]$level[i] <- tmpItemLevel[[4]]$level[i] <- paste("<", 
                                                                                                                                    level_min, sep = "")
    }
    else {
      if (aux[length(aux)] > length(level_breaks)) {
        tmpItemLevel[[1]]$level[i] <- tmpItemLevel[[2]]$level[i] <- tmpItemLevel[[3]]$level[i] <- tmpItemLevel[[4]]$level[i] <- "N.A."
      }
      else {
        tmpItemLevel[[1]]$level[i] <- tmpItemLevel[[2]]$level[i] <- tmpItemLevel[[3]]$level[i] <- tmpItemLevel[[4]]$level[i] <- level_breaks[aux[length(aux)]]
      }
    }
  }
  tmpItemLevel[[3]][, colnames(EmpPerc[[1]][[1]])] <- round(tmpItemLevel[[3]][, 
                                                                              colnames(EmpPerc[[1]][[1]])], ndec)
  tmpItemLevel[[4]]$level
  ItemLevel <- list()
  ItemLevel[[1]] <- cbind(tmpItemLevel[[1]],aban,aban_motivo)
  ItemLevel[[2]] <- cbind(tmpItemLevel[[2]],aban,aban_motivo)
  ItemLevel[[3]] <- cbind(tmpItemLevel[[3]],aban,aban_motivo)
  ItemLevel
}

