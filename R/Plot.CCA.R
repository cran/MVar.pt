Plot.CCA <- function(CCA, Titles = NA, xlabel = NA, ylabel = NA, 
                     Color = TRUE, Casc = TRUE) {
  # Rotina para Plotar Graficos do Metodo CCA desenvolvida 
  # por Paulo Cesar Ossani em 09/04/2016
  
  # CCA    - Dados da funcao CCA.
  # Titles - Titulos para os graficos. Se nao for definido assume texto padrao.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # Color  - Graficos coloridos (default = TRUE).
  # Casc    - Efeito cascata na apresentacao dos graficos (default = TRUE).

  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(Titles[1]) || is.na(Titles[1])) Titles[1] = c("Scree-plot das correlacoes das cargas canonicas")
  if (!is.character(Titles[2]) || is.na(Titles[2])) Titles[2] = c("Circulo de correlacoes")
  if (!is.character(Titles[3]) || is.na(Titles[3])) Titles[3] = c("Grafico com os scores do grupo X")
  if (!is.character(Titles[4]) || is.na(Titles[4])) Titles[4] = c("Grafico com os scores do grupo Y")
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (!is.character(xlabel) && !is.na(xlabel))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.logical(Color))
     stop("Entrada para 'Color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(Casc))
     stop("Entrada para 'Casc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (is.na(xlabel))
     xlabel = "Eixo X"
  
  if (is.na(ylabel))
     ylabel = "Eixo Y"
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Scree-plot dos fatores #####
  plot(1:length(CCA$Var.UV[,1]), CCA$Var.UV[,1], type = "b", 
       xlab = "Ordem dos pares canonicos", 
       ylab = "Variancias dos pares canonicos",
       main = Titles[1])
  ##### FIM - Scree-plot dos fatores #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem Correlacoes entre as variaveis canonicas e as variaveis originais #####
  plot(0,0, 
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[2], # Titulo
       asp  = 1, # Aspecto do grafico
       cex  = 0, # Tamanho dos pontos
       xlim = c(-1.1,1.1), # Dimensao para as linhas do grafico
       ylim = c(-1.1,1.1)) # Dimensao para as colunas do grafico
  
  symbols(0, 0, circles = 1, inches = FALSE, fg = 1, add = TRUE) # cria um circulo
  
  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  ## Grupo X
  arrows(0,0,CCA$Coor.X[,1], CCA$Coor.X[,2], lty = 2, code = 2, angle = 10, col = ifelse(Color,"red","black")) # cria a seta apontando para cada ponto do grupo X
  LocLab(CCA$Coor.X, rownames(CCA$Coor.X), col = ifelse(Color,"red","black"))  # Coloca os nomes dos pontos das coordenadas principais das linhas
  #text(CCA$Coor.X,cex=1, rownames(CCA$Coor.X), pos=3, xpd = TRUE)  # Coloca os nomes dos pontos das coordenadas principais das linhas

  ## Grupo Y
  arrows(0,0,CCA$Coor.Y[,1], CCA$Coor.Y[,2], lty = 1, code = 2, angle = 10, col = ifelse(Color,"blue","black")) # cria a seta apontando para cada ponto do grupo Y
  LocLab(CCA$Coor.Y, rownames(CCA$Coor.Y), col = ifelse(Color,"Blue","black"))  # Coloca os nomes dos pontos das coordenadas principais das linhas
  #text(CCA$Coor.Y,cex=1, rownames(CCA$Coor.Y), pos=3, xpd = TRUE)  # Coloca os nomes dos pontos das coordenadas principais das linhas
  ##### FIM - Plotagem Correlacoes entre as variaveis canonicas e as variaveis originais #####

  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos scores dos grupos X e Y #####
  plot(CCA$Score.X, # grafico para os scores do grupo X
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[3], # Titulo
       asp  = 2,  # Aspecto do Grafico
       pch  = 15, # Formato dos pontos 
       cex  = 1,  # Tamanho dos pontos
       xlim = c(min(CCA$Score.X[,1])-0.1,max(CCA$Score.X[,1])+0.1), # Dimensao para as linhas do grafico
       ylim = c(min(CCA$Score.X[,2]-0.1),max(CCA$Score.X[,2])+0.1), # Dimensao para as colunas do grafico
       col  = ifelse(Color,"red","black")) # Cor dos pontos
  
  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  if (is.na(rownames(CCA$Score.X)[1])) LineNames <- as.character(1:nrow(CCA$Score.X))
  
  if (!is.na(rownames(CCA$Score.X)[1])) LineNames <- rownames(CCA$Score.X)
  
  LocLab(CCA$Score.X, LineNames)  # Coloca os nomes dos pontos das coordenadas principais das linhas
  #text(CCA$Coor.Y,cex=1, rownames(CCA$Coor.Y), pos=3, xpd = TRUE)  # Coloca os nomes dos pontos das coordenadas principais das linhas
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  plot(CCA$Score.Y, # grafico para os scores do grupo Y
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[4], # Titulo
       asp  = 2,  # Aspecto do Grafico
       pch  = 15, # Formato dos pontos 
       cex  = 1,  # Tamanho dos pontos
       xlim = c(min(CCA$Score.Y[,1])-0.1,max(CCA$Score.Y[,1])+0.1), # Dimensao para as linhas do grafico
       ylim = c(min(CCA$Score.Y[,2]-0.1),max(CCA$Score.Y[,2])+0.1), # Dimensao para as colunas do grafico
       col  = ifelse(Color,"red","black")) # Cor dos pontos
    
  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
    
  if (is.na(rownames(CCA$Score.Y)[1])) LineNames <- as.character(1:nrow(CCA$Score.Y))
    
  if (!is.na(rownames(CCA$Score.Y)[1])) LineNames <- rownames(CCA$Score.Y)
 
  LocLab(CCA$Score.Y, LineNames)  # Coloca os nomes dos pontos das coordenadas principais das linhas
  #text(CCA$Coor.Y,cex=1, rownames(CCA$Coor.Y), pos=3, xpd = TRUE)  # Coloca os nomes dos pontos das coordenadas principais das linhas
  ##### FIM - Plotagem dos scores dos grupos X e Y #####
  
}