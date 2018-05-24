MDS <- function(Data, Distance = "euclidean", Eixos = TRUE, 
                Title = NA, xlabel = NA, ylabel = NA, 
                Color = TRUE, LinLab = NA) {
  # Esta funcao executa a Escalonamento Multidimensional
  # desenvolvida por Paulo Cesar Ossani em 07/2016
  
  # Entrada:
  # Data - Dados a serem a analizados.
  # Distance - Metrica das distancias: "euclidean" (default), "maximum", 
  #            "manhattan", "canberra", "binary" ou "minkowski".
  # Color  - Graficos coloridos (default = TRUE).
  # Eixos  - Coloca eixos no grafico (default = TRUE).
  # Title  - Titulo do grafico, se nulo retorna padrao.
  # xlabel - Nomeia o eixo X, se nulo retorna padrao.
  # ylabel - Nomeia o eixo Y, se nulo retorna padrao.
  # LinLab - Vetor com o rotulo para as linhas, se nao
  #          informado retorna o padrao dos dados.
  
  # Retorna:
  # Grafico de escalonamento multidimensional.
  # MatrixD - Matriz das distancias.
  
  if (!is.data.frame(Data)) 
     stop("Entrada 'Data' esta incorreta, deve ser do tipo dataframe. Verifique!")
 
  DISTANCE <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  if (is.na(pmatch(Distance, DISTANCE))) 
     stop("Entrada para 'Metodo' esta incorreta, deve ser: 'euclidean', 
          'maximum', 'manhattan', 'canberra', 'binary' ou 'minkowski'. Verifique!")

  if (!is.logical(Eixos)) 
     stop("Entrada para 'Eixos' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.character(Title) && !is.na(Title))
     stop("Entrada para 'Title' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(xlabel) && !is.na(xlabel))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.logical(Color))
     stop("Entrada para 'Color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.na(LinLab) && length(LinLab)!=nrow(Data))
     stop("O numero elementos do rotulo para linhas 'LinLab' difere do numero de linhas da base de dados. Verifique!")
  
  if (is.na(Title))
     Title = "Escalonamento multidimensional" # Titulo
  
  if (is.na(xlabel))
     xlabel = "Eixo x" # Nomeia Eixo X  
  
  if (is.na(ylabel))
     ylabel = "Eixo y" # Nomeia Eixo Y  
  
  if (is.na(LinLab))
     LinLab <- rownames(Data)
  
  if (!is.na(LinLab) && !is.character(LinLab))
     stop("Entrada para 'LinLab' esta incorreta, deve ser do tipo caracter. Verifique!")

  Md <- dist(Data, method = Distance) # matrix das distancias
  
  fit <- cmdscale(Md) # gera dos dados para o grafico

  x <- fit[,1] # valores eixo x
  y <- fit[,2] # valores eixo y
  
  plot(x,y, # cria grafico para as coordenadas linhas x e colunas y
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Title,  # Titulo
       asp  = 1,  # Aspecto do Grafico
       pch  = 19, # Formato dos pontos 
       cex  = 1,  # Tamanho dos pontos
       xlim = c(min(x)-0.5,max(x)+0.5), # Dimensao para as linhas do grafico
       ylim = c(min(y)-0.5,max(y)+0.5), # Dimensao para as colunas do grafico
       col  = ifelse(Color,"red","black"))  # Cor dos pontos
  
  if (Eixos) # coloca eixos no grafico
     abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  #text(fit, cex = 1, pos = 3, labels = LinLab)  # Coloca os nomes dos pontos das coordenadas
  LocLab(x, y, cex = 1, LinLab)

  Lista <- list(MatrixD = Md)
  
  return(Lista)
}