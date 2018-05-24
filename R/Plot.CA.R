Plot.CA <- function(AC, Titles = matrix(NA,1,4), xlabel = NA, ylabel = NA,
                    Color = TRUE, LinLab = NA) {
  # Rotina para Plotar Graficos do Metodo AC desenvolvida 
  # por Paulo Cesar Ossani em 11/2014
  
  # Entrada:
  # AC     - Dados da funcao CA.
  # Titles - Titulos para os graficos.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # Color  - Graficos coloridos (default = TRUE).
  # LinLab - Vetor com o rotulo para as linhas para dados de frequencia,
  #          se nao informado retorna padrao.
  
  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(Titles[1]) || is.na(Titles[1])) Titles[1] = c("Scree-plot das variancias dos componentes")
  if (!is.character(Titles[2]) || is.na(Titles[2])) Titles[2] = c("Grafico Correspondente as Linhas (Observacoes)")
  if (!is.character(Titles[3]) || is.na(Titles[3])) Titles[3] = c("Grafico Correspondente as Colunas(Variaveis)")
  if (!is.character(Titles[4]) || is.na(Titles[4])) Titles[4] = c("Grafico Correspondente as Observacoes e Variaveis")
  
  if (!is.character(xlabel) && !is.na(xlabel))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.logical(Color))
     stop("Entrada para 'Color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.na(LinLab) && length(LinLab)!=nrow(AC$MatrixX) && AC$TypData=="F")
     stop("O numero elementos do rotulo para linhas 'LinLab' difere do numero de linhas da base de dados. Verifique!")
  
  if (is.na(LinLab) && AC$TypData=="F")
     LinLab <- rownames(AC$MatrixX)
  
  if (is.na(xlabel))
     xlabel = paste("Primeira coordenada principal (",round(AC$MatrixAutoVlr[1,2],2),"%)",sep="")
  
  if (is.na(ylabel))
     ylabel = paste("Segunda coordenada principal (",round(AC$MatrixAutoVlr[2,2],2),"%)",sep="")
  
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  ##### INICIO - Plotagem dos Autovalores #####
  mp <- barplot(AC$MatrixAutoVlr[,1],names.arg=paste(round(AC$MatrixAutoVlr[,2],2),"%",sep=""),main = "Variancias dos componentes")
  ##### FIM - Plotagem dos Autovalores #####
  
  ##### INICIO - Scree-plot dos componentes #####
  plot(1:length(AC$MatrixAutoVlr[,1]), AC$MatrixAutoVlr[,1], type = "b", 
       xlab = "Ordem dos componentes", 
       ylab = "Variancia",
       main = Titles[1])
  ##### FIM - Scree-plot dos componentes #####
  
  ##### INICIO - Plotagem dos Dados das linhas #####
  if (AC$TypData=="F") { # plota se nao for analise de correspondencia multipla
    plot(AC$MatrixX, # cria grafico para as coordenadas principais das linhas
         xlab = xlabel, # Nomeia Eixo X
         ylab = ylabel, # Nomeia Eixo Y
         main = Titles[2], # Titulo
         asp  = 1,  # Aspecto do Grafico
         pch  = 17, # Formato dos pontos 
         cex  = 1,  # Tamanho dos pontos
         xlim = c(min(AC$MatrixX[,1])-0.1,max(AC$MatrixX[,1])+0.1), # Dimensao para as linhas do grafico
         ylim = c(min(AC$MatrixX[,2]-0.1),max(AC$MatrixX[,2])+0.1), # Dimensao para as colunas do grafico
         col  = ifelse(Color,"red","black")) # Cor dos pontos
    
    abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
    
    #text(AC$MatrixX,cex=1, pos=3, LinLab)  # Coloca os nomes dos pontos das coordenadas principais das linhas
    LocLab(AC$MatrixX,cex=1, LinLab)
  }
  ##### FIM - Plotagem dos Dados das linhas #####
  
  ##### INICIO - Plotagem Dados das colunas #####
  plot(AC$MatrixY, # cria grafico para as coordenadas principais das linhas
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[3], # Titulo
       asp  = 1,   # Aspecto do Grafico
       pch  = ifelse(AC$TypData=="C",17,16), # Formato dos pontos 
       cex  = 1.2, # Tamanho dos pontos
       xlim = c(min(AC$MatrixY[,1])-0.1,max(AC$MatrixY[,1])+0.1), # Dimensao para as linhas do grafico
       ylim = c(min(AC$MatrixY[,2]-0.1),max(AC$MatrixY[,2])+0.1), # Dimensao para as colunas do grafico
       col  = ifelse(Color,ifelse(AC$TypData=="C","red","blue"),"black"))             # Cor dos pontos
  
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  #text(AC$MatrixY, cex=1, pos=3, rownames(AC$MatrixY))  # Coloca os nomes dos pontos das coordenadas principais das colunas
  LocLab(AC$MatrixY, cex=1, rownames(AC$MatrixY))
  ##### FIM - Plotagem Dados das colunas #####
  
  ##### INICIO - Plotagem dos Dados das linhas e colunas conjuntamente #####
  if (AC$TypData=="F") { # plota se nao for analise de correspondencia multipla
     plot(AC$MatrixX,    # cria grafico para as coordenadas principais das linhas
          xlab = xlabel, # Nomeia Eixo X
          ylab = ylabel, # Nomeia Eixo Y
          main = Titles[4], # Titulo
          asp  = 1,  # Aspecto do Grafico
          pch  = 17, # Formato dos pontos 
          cex  = 1,  # Tamanho dos pontos
          xlim = c(min(AC$MatrixX[,1],AC$MatrixY)-0.1,max(AC$MatrixX[,1],AC$MatrixY)+0.1), # Dimensao para as linhas do grafico
          ylim = c(min(AC$MatrixX[,2],AC$MatrixY)-0.1,max(AC$MatrixX[,2],AC$MatrixY)+0.1), # Dimensao para as colunas do grafico
          col  = ifelse(Color,"red","black")) # Cor dos pontos
    
     points(AC$MatrixY, pch = 16, cex = 1.2, col = ifelse(Color,"blue","black")) # adiciona ao grafico as coordenadas principais das colunas
    
     abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
    
     #text(AC$MatrixX, cex=1,  pos=3, LinLab)  # Coloca os nomes dos pontos das coordenadas principais das linhas
     #LocLab(AC$MatrixX, cex=1,LinLab)
    
     #text(AC$MatrixY, cex=1, pos=3, rownames(AC$MatrixY))  # Coloca os nomes dos pontos das coordenadas principais das colunas
     #LocLab(AC$MatrixY, cex=1, rownames(AC$MatrixY))

     LocLab(rbind(AC$MatrixX[,1:2],AC$MatrixY[,1:2]), cex=1, rbind(as.matrix(LinLab),as.matrix(rownames(AC$MatrixY))))
  }
  ##### FIM - Plotagem dos Dados das linhas e colunas conjuntamente #####
}