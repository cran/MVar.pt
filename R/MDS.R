MDS <- function(Data, Distance = "euclidean", Title = NA, xlabel = NA,
                ylabel = NA, PosLeg = 2, BoxLeg = TRUE, Axis = TRUE,
                Color = TRUE, LinLab = NA, Class = NA) {
  
  # Esta funcao executa a Escalonamento Multidimensional
  # desenvolvida por Paulo Cesar Ossani em 07/2016
  
  # Entrada:
  # Data - Dados a serem a analizados.
  # Distance - Metrica das distancias: "euclidean" (default), "maximum", 
  #            "manhattan", "canberra", "binary" ou "minkowski".
  # Title  - Titulo do grafico, se nulo retorna padrao.  
  # xlabel - Nomeia o eixo X, se nulo retorna padrao.
  # ylabel - Nomeia o eixo Y, se nulo retorna padrao. 
  # PosLeg - 0 sem legenda,
  #            1 para legenda no canto superior esquerdo,
  #            2 para legenda no canto superior direito (default),
  #            3 para legenda no canto inferior direito,
  #            4 para legenda no canto inferior esquerdo.  
  # BoxLeg - Colocar moldura na legenda (default = TRUE).  
  # Axis   - Coloca eixos no grafico (default = TRUE).
  # Color  - Graficos coloridos (default = TRUE).
  # LinLab - Vetor com o rotulo para as linhas, se nao
  #          informado retorna o padrao dos dados.  
  # Class  - Vetor com os nomes das classes dos dados.
  
  # Retorna:
  # Grafico de escalonamento multidimensional.
  # MatrixD - Matriz das distancias.

  if (!is.data.frame(Data)) 
     stop("Entrada 'Data' esta incorreta, deve ser do tipo dataframe. Verifique!")
 
  if (!is.na(Class[1])) {
    
    Class <- as.matrix(Class)
    
    if (nrow(Data) != length(Class))
       stop("Entrada 'Class' ou 'Data' esta incorreta, devem conter o mesmo numero de linhas. Verifique!")
  }
  
  Distance <- tolower(Distance) # torna minuscula
  
  DISTANCE <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
 
  if (!(Distance %in% DISTANCE))
     stop("Entrada para 'Distance' esta incorreta, deve ser: 'euclidean', 
          'maximum', 'manhattan', 'canberra', 'binary' ou 'minkowski'. Verifique!")

  if (!is.logical(Axis)) 
     stop("Entrada para 'Axis' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.character(Title) && !is.na(Title[1]))
     stop("Entrada para 'Title' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.numeric(PosLeg) || PosLeg < 0 || PosLeg > 4 || (floor(PosLeg)-PosLeg) != 0)
     stop("Entrada para posicao da legenda 'PosLeg' esta incorreta, deve ser um numero inteiro entre [0,4]. Verifique!")
  
  if (!is.logical(Color))
     stop("Entrada para 'Color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(BoxLeg)) 
     stop("Entrada para moldura da legenda 'BoxLeg' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.na(LinLab[1]) && length(LinLab)!=nrow(Data))
     stop("O numero elementos do rotulo para linhas 'LinLab' difere do numero de linhas da base de dados. Verifique!")
  
  if (is.na(Title[1]))
     Title = "Escalonamento multidimensional" # Titulo
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")

  if (!is.na(LinLab[1]) && !is.character(LinLab))
     stop("Entrada para 'LinLab' esta incorreta, deve ser do tipo caracter ou string. Verifique!")

  ##### INICIO - Informacoes usadas nos Graficos #####
  if (is.na(xlabel[1]))
     xlabel = "Eixo X" 
  
  if (is.na(ylabel[1]))
     ylabel = "Eixo Y"

  if (PosLeg==1) PosLeg = "topleft"     # posicao das legendas nos graficos
  if (PosLeg==2) PosLeg = "topright"
  if (PosLeg==3) PosLeg = "bottomright"
  if (PosLeg==4) PosLeg = "bottomleft"
  
  BoxLeg = ifelse(BoxLeg,"o","n") # moldura nas legendas, "n" sem moldura, "o" com moldura
  
  Num.Class = 0
  if (!is.na(Class[1])) {
     Class.Table <- table(Class)       # cria tabela com as quantidade dos elementos das classes
     Class.Names <- names(Class.Table)  # nomes das classses
     Num.Class   <- length(Class.Table) # numero de classes
     NomeLinhas  <- as.matrix(Class)
  } 

  cor <- 1 # cor inicial dos pontos e legendas
  ##### FIM - Informacoes usadas nos Graficos #####
  
  Md <- dist(Data, method = Distance) # matrix das distancias
  
  fit <- cmdscale(Md) # gera dos dados para o grafico

  x <- fit[,1] # valores eixo x
  y <- fit[,2] # valores eixo y
  
  if (Num.Class == 0) {
    
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
    
  } else {
    
    plot(0,0, # cria grafico para as coordenadas linhas x e colunas y
         xlab = xlabel, # Nomeia Eixo X
         ylab = ylabel, # Nomeia Eixo Y
         main = Title,  # Titulo
         asp  = 1,  # Aspecto do Grafico
         cex  = 0,  # Tamanho dos pontos
         xlim = c(min(x)-0.5,max(x)+0.5), # Dimensao para as linhas do grafico
         ylim = c(min(y)-0.5,max(y)+0.5), # Dimensao para as colunas do grafico
         col  = ifelse(Color,"red","black"))  # Cor dos pontos

    NewData <- cbind(x,y)
    
    Init.Form <- 14 # formato inicial dos pontos
    
    for (i in 1:Num.Class) {
      
      Point.Form <- Init.Form + i # fomato dos pontos de cada classe
      
      cor1 <- ifelse(Color, cor + i, "black")
 
      Point.Data <- NewData[which(Class == Class.Names[i]),]

      points(Point.Data,
             pch = Point.Form, # Formato dos pontos
             cex = 1.2,  # Tamanho dos pontos
             col = cor1) # adiciona ao grafico as coordenadas principais das colunas
    }
    
  }
  
  if (PosLeg != 0 && Num.Class > 0) {
    
     if (Color) cor <- 2
    
     Init.Form <- 15
    
     Color_b <- cor # colore as letras das legendas e suas representacoes no grafico
    
     if (Color) Color_b = cor:(cor + Num.Class)
    
     legend(PosLeg, Class.Names, pch = (Init.Form):(Init.Form + Num.Class), col = Color_b,
            text.col = Color_b, bty = BoxLeg, text.font = 6, y.intersp = 0.8, xpd = TRUE) # cria a legenda
  }
  
  if (Axis) # coloca Axis no grafico
     abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  #text(fit, cex = 1, pos = 3, labels = LinLab)  # Coloca os nomes dos pontos das coordenadas
  if (!is.na(LinLab[1])) LocLab(x, y, cex = 1, LinLab)

  Lista <- list(MatrixD = Md)

  return(Lista)
}