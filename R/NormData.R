NormData <- function(Data, Type = 1) {
  # Padroniza os dados, ou seja, torna os dados com Distribuicao
  # Normal com media zero e variancia 1 desenvolvida por 
  # Paulo Cesar Ossani em 07/2013
  
  # Data - Dados a serem a normalizados
  # Type - 1 Normaliza global (default)
  #        2 Normaliza por coluna
  
  # Retorna:
  # DataNorm  - Dados Normalizados
  
  if (!is.data.frame(Data) && !is.matrix(Data)) 
     stop("Entrada 'Data' esta incorreta, deve ser do tipo dataframe ou matriz. Verifique!")
  
  if (Type!=1 && Type!=2) 
     stop("Entrada para 'Type' esta incorreta, deve ser numerica, sendo 1 ou 2. Verifique!")
  
  DataNorm = as.matrix(Data)  # Dados a serem Normalizados
  
  if (Type==1) { # normaliza globalmente
     Media  <- mean(DataNorm)
     Desvio <- sd(DataNorm)
     for (i in 1:ncol(DataNorm))
        DataNorm[,i] = (DataNorm[,i]- Media)/Desvio # Matriz de dados normalizados
  }

  if (Type==2) { # normaliza os dados por coluna
     # Media    <- apply(DataNorm, 2, mean) # encontra as medias por colunas
     # DataNorm <- sweep(DataNorm, 2, Media, FUN = "-")   # Centraliza na media
     # Desvio   <- sqrt(colSums(DataNorm^2)/(nrow(DataNorm)-1)) # raiz da soma do quadrado - desvio padrao amostral
     # DataNorm <- sweep(DataNorm, 2, Desvio, FUN = "/")  # Divide pelo desvio padrao
     ## Modo mais simples e rapido
     DataNorm <- scale(DataNorm, center = TRUE, scale = TRUE)
  }
  
  return(DataNorm)
}

