Plot.CA <- function(CA, Titles = NA, xlabel = NA, ylabel = NA,
                    Color = TRUE, LinLab = NA, Casc = TRUE) {
  # Rotina para Plotar Graficos do Metodo AC desenvolvida 
  # por Paulo Cesar Ossani em 11/2014
  
  # Entrada:
  # CA     - Dados da funcao CA.
  # Titles - Titulos para os graficos.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # Color  - Graficos coloridos (default = TRUE).
  # LinLab - Vetor com o rotulo para as linhas para dados de frequencia,
  #          se nao informado retorna padrao.
  # Casc    - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(Titles[1]) || is.na(Titles[1])) Titles[1] = c("Scree-plot das variancias dos componentes")
  if (!is.character(Titles[2]) || is.na(Titles[2])) Titles[2] = c("Grafico correspondente as Linhas (Observacoes)")
  if (!is.character(Titles[3]) || is.na(Titles[3])) Titles[3] = c("Grafico correspondente as Colunas (Variaveis)")
  if (!is.character(Titles[4]) || is.na(Titles[4])) Titles[4] = c("Grafico correspondente as Observacoes e Variaveis")
  
  if (!is.character(xlabel) && !is.na(xlabel))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.logical(Color))
     stop("Entrada para 'Color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.na(LinLab) && length(LinLab)!=nrow(CA$MatrixX) && CA$TypData=="F")
     stop("O numero elementos do rotulo para linhas 'LinLab' difere do numero de linhas da base de dados. Verifique!")
  
  if (!is.logical(Casc))
     stop("Entrada para 'Casc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (is.na(LinLab) && CA$TypData=="F")
     LinLab <- rownames(CA$MatrixX)
  
  if (is.na(xlabel))
     xlabel = paste("Primeira coordenada (",round(CA$MatrixAutoVlr[1,2],2),"%)",sep="")
  
  if (is.na(ylabel))
     ylabel = paste("Segunda coordenada (",round(CA$MatrixAutoVlr[2,2],2),"%)",sep="")
  
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Autovalores #####
  mp <- barplot(CA$MatrixAutoVlr[,1],names.arg=paste(round(CA$MatrixAutoVlr[,2],2),"%",sep=""),main = "Variancias dos componentes")
  ##### FIM - Plotagem dos Autovalores #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Scree-plot dos componentes #####
  plot(1:length(CA$MatrixAutoVlr[,1]), CA$MatrixAutoVlr[,1], type = "b", 
       xlab = "Ordem dos componentes", 
       ylab = "Variancia",
       main = Titles[1])
  ##### FIM - Scree-plot dos componentes #####
  
  ##### INICIO - Plotagem dos Dados das linhas #####
  if (CA$TypData=="F") { # plota se nao for analise de correspondencia multipla
    
    if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
    
    plot(CA$MatrixX, # cria grafico para as coordenadas principais das linhas
         xlab = xlabel, # Nomeia Eixo X
         ylab = ylabel, # Nomeia Eixo Y
         main = Titles[2], # Titulo
         asp  = 1,  # Aspecto do Grafico
         pch  = 17, # Formato dos pontos 
         cex  = 1,  # Tamanho dos pontos
         xlim = c(min(CA$MatrixX[,1])-0.1,max(CA$MatrixX[,1])+0.1), # Dimensao para as linhas do grafico
         ylim = c(min(CA$MatrixX[,2]-0.1),max(CA$MatrixX[,2])+0.1), # Dimensao para as colunas do grafico
         col  = ifelse(Color,"red","black")) # Cor dos pontos
    
    abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
    
    #text(CA$MatrixX,cex=1, pos=3, LinLab)  # Coloca os nomes dos pontos das coordenadas principais das linhas
    LocLab(CA$MatrixX,cex=1, LinLab)
  }
  ##### FIM - Plotagem dos Dados das linhas #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem Dados das colunas #####
  plot(CA$MatrixY, # cria grafico para as coordenadas principais das linhas
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[3], # Titulo
       asp  = 1,   # Aspecto do Grafico
       pch  = ifelse(CA$TypData=="C",17,16), # Formato dos pontos 
       cex  = 1.2, # Tamanho dos pontos
       xlim = c(min(CA$MatrixY[,1])-0.1,max(CA$MatrixY[,1])+0.1), # Dimensao para as linhas do grafico
       ylim = c(min(CA$MatrixY[,2]-0.1),max(CA$MatrixY[,2])+0.1), # Dimensao para as colunas do grafico
       col  = ifelse(Color,ifelse(CA$TypData=="C","red","blue"),"black"))             # Cor dos pontos
  
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  #text(CA$MatrixY, cex=1, pos=3, rownames(CA$MatrixY))  # Coloca os nomes dos pontos das coordenadas principais das colunas
  LocLab(CA$MatrixY, cex=1, rownames(CA$MatrixY))
  ##### FIM - Plotagem Dados das colunas #####
  
  ##### INICIO - Plotagem dos Dados das linhas e colunas conjuntamente #####
  if (CA$TypData=="F") { # plota se nao for analise de correspondencia multipla
    
     if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
    
     plot(CA$MatrixX,    # cria grafico para as coordenadas principais das linhas
          xlab = xlabel, # Nomeia Eixo X
          ylab = ylabel, # Nomeia Eixo Y
          main = Titles[4], # Titulo
          asp  = 1,  # Aspecto do Grafico
          pch  = 17, # Formato dos pontos 
          cex  = 1,  # Tamanho dos pontos
          xlim = c(min(CA$MatrixX[,1],CA$MatrixY)-0.1,max(CA$MatrixX[,1],CA$MatrixY)+0.1), # Dimensao para as linhas do grafico
          ylim = c(min(CA$MatrixX[,2],CA$MatrixY)-0.1,max(CA$MatrixX[,2],CA$MatrixY)+0.1), # Dimensao para as colunas do grafico
          col  = ifelse(Color,"red","black")) # Cor dos pontos
    
     points(CA$MatrixY, pch = 16, cex = 1.2, col = ifelse(Color,"blue","black")) # adiciona ao grafico as coordenadas principais das colunas
    
     abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
    
     #text(CA$MatrixX, cex=1,  pos=3, LinLab)  # Coloca os nomes dos pontos das coordenadas principais das linhas
     #LocLab(CA$MatrixX, cex=1,LinLab)
    
     #text(CA$MatrixY, cex=1, pos=3, rownames(CA$MatrixY))  # Coloca os nomes dos pontos das coordenadas principais das colunas
     #LocLab(CA$MatrixY, cex=1, rownames(CA$MatrixY))

     LocLab(rbind(CA$MatrixX[,1:2],CA$MatrixY[,1:2]), cex=1, rbind(as.matrix(LinLab),as.matrix(rownames(CA$MatrixY))))
  }
  ##### FIM - Plotagem dos Dados das linhas e colunas conjuntamente #####
}