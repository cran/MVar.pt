CoefVar <- function(Data, Type = 1) {
  # Encontra o Coeficiente de Variacao dos dados, 
  # funcao desenvolvida por Paulo Cesar Ossani em 05/2016
  
  # Data - Dados a serem analizados
  # Type - 1 Coefiente de variacao global (default)
  #        2 Coefiente de variacao por coluna
  
  # Retorna:
  # CVar - Coeficiente de variacao
  
  if (!is.data.frame(Data) && !is.matrix(Data)) 
     stop("Entrada 'Data' esta incorreta, deve ser do tipo dataframe ou matriz. Verifique!")
  
  if (Type!=1 && Type!=2) 
     stop("Entrada para 'Type' esta incorreta, deve ser numerica, sendo 1 ou 2. Verifique!")
  
  Data <- as.matrix(Data)  # Dados a serem analizados
  
  if (Type==1) { # Coeficiente de variacao global
    CVar <- as.matrix(sd(Data)/mean(Data) * 100)
    colnames(CVar) <- c("C.V. em %")
  }
  
  if (Type==2) { # Coeficiente de variacao por coluna
    Media  <- apply(Data, 2, mean) # encontra as medias por colunas
    Data   <- sweep(Data, 2, Media, FUN = "-") # Centraliza na media
    Desvio <- sqrt(colSums(Data^2)/(nrow(Data)-1)) # raiz da soma do quadrado - desvio padrao amostral
    CVar   <- as.matrix(Desvio/Media * 100)
    colnames(CVar) <- c("C.V. em %")
  }
  
  return(CVar)
}
