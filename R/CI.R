CI <- function(data, sigma = NA, conf = 0.95, type = "T") {
  # Esta funcao encontra os typeos de confianca uni e multivariado
  # desenvolvida por Paulo Cesar Ossani em 20/06/2026
  
  # Entrada:
  # data  - Dados dados com as variaveis para encontrar o IC
  # sigma - matriz de variancia e covariancia, caso contrario o IC sera pela variancia amostral (default sigma = NA)
  # conf  - Nivel de confianca do IC (default conf = 95%)
  # type  - "T" = T^2 de Hotelling e "B" = Bonferroni
  
  # Retorna:
  # icm - Intervalo de confianca multivariado com "sign" de significancia
  # icu - Intervalo de confianca univariado com "sign" de significancia
  
  if (!is.data.frame(data) && !is.matrix(data))
     stop("Entrada 'data' esta incorreta, deve ser do tipo dataframe ou matrix. Verifique!")
  
  type = toupper(type) # torna minusculo
  if (!(type %in% c("T", "B")))
     stop("Entrada para 'type' esta incorreta, deve ser: 'T' ou 'B'. Verifique!")
  
  if (all(!is.na(sigma))) {
    if (!is.matrix(sigma) && (nrow(sigma) != ncol(sigma)) && !isSymmetric(as.matrix(sigma))) 
       stop("Entrada para 'sigma' esta incorreta, deve ser uma matriz de variancia e covariancia. Verifique!")
    
    if (any(eigen(sigma, only.values = TRUE)$values < - 1e-8)) 
       stop("Entrada para 'sigma' esta incorreta, deve ser uma matriz de variancia e covariancia. Verifique!")
  }
  
  if (conf <= 0 || conf > 1) 
     stop("Entrada para 'conf' esta incorreta, deve ser valores entre 0 e 1. Verifique!")
  
  data <- as.matrix(data[, sapply(data, is.numeric), drop = FALSE]) # seleciona apenas as colunas numericas
  
  sign <- 1 - conf # nivel de significancia
  n <- nrow(data) # numero de observacoes 
  m <- ncol(data) # numero de variaveis
  mi.amo <- colMeans(data) # vetor media amostral

  ll <- diag(rep(1,ncol(data))) # vetores l's
  p <- ncol(data) # grau de liberdade

  CIm <- matrix(NA, ncol = 3, nrow = ncol(data)) # matriz com os IC's multivariado
  colnames(CIm) <- c("Medias","Lim.Inferior","Lim.Superior")
  rownames(CIm) <- colnames(data)
  CIm[,1] <- mi.amo
  
  CIu <- CIm # matriz com os IC's uniivariado
  
  if (!all(is.na(sigma))) { 
     
     text <- "- Variancia Conhecida"
    
     ### Inicio - Variancia Conhecida Inivariada ###
     for(i in 1:ncol(data)) {
       erro.media <- sqrt(sigma[i,i] / n) 
       z <- qnorm(1 - sign / 2) 
       CIu[i,2] <- CIu[i,1] - z * erro.media 
       CIu[i,3] <- CIu[i,1] + z * erro.media 
     }
     ### Fim - Variancia Conhecida Inivariada ###
    
     ### Inicio - Variancia Conhecida Multivariada ###
     if (type == "T") { # T^2 Hotelling
        QQ = qchisq(1 - sign, df = p)
        for(i in 1:ncol(data)) {
          erro.media <- sqrt(QQ) * sqrt((t(ll[,i]) %*% sigma %*% ll[,i]) / n)
          CIm[i,2] <- t(ll[,i]) %*% mi.amo - erro.media
          CIm[i,3] <- t(ll[,i]) %*% mi.amo + erro.media
        }
     }
    
     if (type == "B") { # Bonferroni
        for(i in 1:ncol(data)) {
          erro.media <- qnorm(1 - sign / (2 * m)) * sqrt((t(ll[,i]) %*% sigma %*% ll[,i]) / n)
          CIm[i,2] <- t(ll[,i]) %*% mi.amo - erro.media
          CIm[i,3] <- t(ll[,i]) %*% mi.amo + erro.media
        }
     } 
     ### Fim - Variancia Conhecida Multivariada ###
    
  } else { 
    
     text <- "- Variancia Desconhecida"
     
     sigma <- cov(data)
     
     ### Inicio - Variancia Desconhecida Univariada ###
     for(i in 1:ncol(data)) {
       erro.media <- sqrt(sigma[i,i] / n) 
       tcrit <- qt(1 - sign / 2, df = n - 1) 
       CIu[i,2] <- CIu[i,1] - tcrit * erro.media 
       CIu[i,3] <- CIu[i,1] + tcrit * erro.media 
     }
     ### Fim - Variancia Desconhecida Univariada ###
     
     ### Inicio - Variancia Desconhecida Multivariada ###
     v <- nrow(data) - 1 # grau de liberdade
     
     if (type == "T") { # T^2 Hotelling
         TB = v * p / (v - p + 1) * qf(1 - sign, df1 = p, df2 = v - p + 1, ncp = 0)
         for(i in 1:ncol(data)) {
           erro.media <- sqrt(TB) * sqrt((t(ll[,i]) %*% sigma %*% ll[,i]) / n)
           CIm[i,2] <- t(ll[,i]) %*% mi.amo - erro.media
           CIm[i,3] <- t(ll[,i]) %*% mi.amo + erro.media
        }
      }
    
     if (type == "B") { # Bonferroni
        for(i in 1:ncol(data)) {
          erro.media <- qt(1 - sign / (2 * m), df = v ) * sqrt((t(ll[,i]) %*% sigma %*% ll[,i]) / n)
          CIm[i,2] <- t(ll[,i]) %*% mi.amo - erro.media
          CIm[i,3] <- t(ll[,i]) %*% mi.amo + erro.media
        }
     }
     ### Fim - Variancia Desconhecida Multivariada ###
  }
  
  resu <- list(title = paste("IC Univariado", text), CIu = CIu)  
  resm <- list(title = paste("IC Multivariado", text), CIm = CIm)
  
  lista <- list(cim = resm, ciu = resu)
  
  return(lista)
}
