NormData <- function(data, type = 1) {
  # Padroniza os dados, ou seja, torna os dados com Distribuicao
  # Normal com media zero e variancia 1 desenvolvida por 
  # Paulo Cesar Ossani em 07/2013
  
  # data - Dados a serem a normalizados
  # type - 1 Normaliza global (default)
  #        2 Normaliza por coluna
  
  # Retorna:
  # dataNorm  - Dados Normalizados
  
  if (!is.data.frame(data) && !is.matrix(data)) 
     stop("Entrada 'data' esta incorreta, deve ser do tipo dataframe ou matriz. Verifique!")
  
  if (type!=1 && type!=2) 
     stop("Entrada para 'type' esta incorreta, deve ser numerica, sendo 1 ou 2. Verifique!")
  
  dataNorm = as.matrix(data)  # Dados a serem Normalizados
  
  if (type==1) { # normaliza globalmente
     Media  <- mean(dataNorm)
     Desvio <- sd(dataNorm)
     for (i in 1:ncol(dataNorm))
        dataNorm[,i] = (dataNorm[,i]- Media)/Desvio # Matriz de dados normalizados
  }

  if (type==2) { # normaliza os dados por coluna
     # Media    <- apply(dataNorm, 2, mean) # encontra as medias por colunas
     # dataNorm <- sweep(dataNorm, 2, Media, FUN = "-")   # Centraliza na media
     # Desvio   <- sqrt(colSums(dataNorm^2)/(nrow(dataNorm)-1)) # raiz da soma do quadrado - desvio padrao amostral
     # dataNorm <- sweep(dataNorm, 2, Desvio, FUN = "/")  # Divide pelo desvio padrao
     ## Modo mais simples e rapido
     dataNorm <- scale(dataNorm, center = TRUE, scale = TRUE)
  }
  
  return(dataNorm)
}

