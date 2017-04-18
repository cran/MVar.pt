MDS <- function(Data, Distance = "euclidean", Eixos = "S", 
                LabelX = NULL, LabelY = NULL, Title = NULL,
                Color = "S", LinLab = NULL) {
  # Esta funcao executa a Escalonamento Multidimensional
  # desenvolvida por Paulo Cesar Ossani em 07/2016
  
  # Entrada:
  # Data - Dados a serem a analizados.
  # Distance - Metrica das distancias: "euclidean" (default), "maximum", 
  #            "manhattan", "canberra", "binary" ou "minkowski".
  # Color  - "s" para grafico colorido (default),
  #          "n" para grafico em preto e branco.
  # Eixos  - "s" coloca eixos no grafico (default),
  #          "n" nao coloca eixos no grafico,
  # LabelX - Nomeia o eixo X, se nulo retorna padrao.
  # LabelY - Nomeia o eixo Y, se nulo retorna padrao.
  # Title - Titulo do grafico, se nulo retorna padrao.
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
  
  Eixos <- toupper(Eixos) # transforma em maiusculo
  
  if (Eixos!="S" && Eixos!="N") 
     stop("Entrada para 'Eixos' esta incorreta, deve ser do tipo caracter, sendo 's' ou 'n'. Verifique!")
  
  Eixos <- ifelse(Eixos=="S", TRUE, FALSE)
  
  if (is.null(LabelX))
     LabelX = "Eixo x"  # Nomeia Eixo X  
  
  if (is.null(LabelY))
     LabelY = "Eixo y"  # Nomeia Eixo Y
  
  if (is.null(Title))
     Title = "Escalonamento Multidimensional" # Titulo
  
  Color <- toupper(Color) # transforma em maiusculo
  
  if (Color!="S" && Color!="N")
     stop("Entrada para 'Color' esta incorreta, deve ser do tipo caracter, sendo 's' ou 'n'. Verifique!")
  
  if (!is.null(LinLab) && length(LinLab)!=nrow(Data))
     stop("O numero elementos do rotulo para linhas 'LinLab' difere do numero de linhas da base de dados. Verifique!")
  
  if (is.null(LinLab))
     LinLab <- rownames(Data)
  
  if (!is.null(LinLab) && !is.character(LinLab))
     stop("Entrada para 'LinLab' esta incorreta, deve ser do tipo caracter. Verifique!")
  
  Md <- dist(Data, method = Distance) # matrix das distancias
  
  fit <- cmdscale(Md) # gera dos dados para o grafico

  x <- fit[,1] # valores eixo x
  y <- fit[,2] # valores eixo y
  
  plot(x,y,  # cria grafico para as coordenadas linhas x e colunas y
       xlab = LabelX,  # Nomeia Eixo X
       ylab = LabelY,  # Nomeia Eixo Y
       main = Title,  # Titulo
       asp = 1,  # Aspecto do Grafico
       pch = 19, # Formato dos pontos 
       cex = 1,  # Tamanho dos pontos
       xlim=c(min(x)-0.5,max(x)+0.5), # Dimensao para as linhas do grafico
       ylim=c(min(y)-0.5,max(y)+0.5), # Dimensao para as colunas do grafico
       col = ifelse(Color=="S","red","black"))  # Cor dos pontos
  
  if (Eixos) # coloca eixos no grafico
     abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  #text(fit, cex = 1, pos = 3, labels = LinLab)  # Coloca os nomes dos pontos das coordenadas
  LocLab(x, y, cex = 1, LinLab)

  Lista <- list(MatrixD = Md)
  
  return(Lista)
}