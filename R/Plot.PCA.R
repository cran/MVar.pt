Plot.PCA <- function(PC, Titles = NA, xlabel = NA, ylabel = NA,
                     Color = TRUE, LinLab = NA, Casc = TRUE) {
  # Rotina para Plotar Graficos do Metodo PCA desenvolvida 
  # por Paulo Cesar Ossani em 11/2014
  
  # Entrada:
  # PC     - Dados da funcao PCA.
  # Titles - Titulos para os graficos.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # Color  - Graficos coloridos (default = TRUE).
  # LinLab - Vetor com o rotulo para as linhas, se nao
  #          informado retorna padrao.
  # Casc    - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(Titles[1]) || is.na(Titles[1])) Titles[1] = c("Scree-plot das variancias dos componentes")
  if (!is.character(Titles[2]) || is.na(Titles[2])) Titles[2] = c("Grafico correspondente as linhas (observacoes)")
  if (!is.character(Titles[3]) || is.na(Titles[3])) Titles[3] = c("Grafico correspondente as colunas (variaveis)")
  
  if (!is.character(xlabel) && !is.na(xlabel))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.logical(Color))
     stop("Entrada para 'Color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.na(LinLab) && length(LinLab)!=nrow(PC$MatrixEsc))
     stop("O numero elementos do rotulo para linhas 'LinLab' difere do numero de linhas da base de dados. Verifique!")
  
  if (!is.logical(Casc))
     stop("Entrada para 'Casc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (is.na(LinLab))
     LinLab <- rownames(PC$MatrixEsc)
  
  if (is.na(xlabel))
     xlabel = paste("Primeira coordenada (",round(PC$MatrixAutoVlr[1,2],2),"%)",sep="")
  
  if (is.na(ylabel))
     ylabel = paste("Segunda coordenada (",round(PC$MatrixAutoVlr[2,2],2),"%)",sep="")
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Autovalores #####
  mp <- barplot(PC$MatrixAutoVlr[,1],names.arg=paste(round(PC$MatrixAutoVlr[,2],2),"%",sep=""),main = "Variancias dos componentes")
  ##### FIM - Plotagem dos Autovalores #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Scree-plot dos componentes #####
  plot(1:length(PC$MatrixAutoVlr[,1]), PC$MatrixAutoVlr[,1], type = "b", 
       xlab = "Ordem dos componentes", 
       ylab = "Variancia",
       main = Titles[1])
  ##### FIM - Scree-plot dos componentes #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Dados das linhas #####
  plot(PC$MatrixEsc, # cria grafico para as coordenadas principais das linhas
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[2], # Titulo
       asp  = 1,  # Aspecto do Grafico
       pch  = 15, # Formato dos pontos 
       cex  = 1,  # Tamanho dos pontos
       xlim = c(min(PC$MatrixEsc[,1])-0.05,max(PC$MatrixEsc[,1])+0.05), # Dimensao para as linhas do grafico
       ylim = c(min(PC$MatrixEsc[,2])-0.05,max(PC$MatrixEsc[,2])+0.05), # Dimensao para as colunas do grafico
       col  = ifelse(Color,"red","black"))  # Cor dos pontos
  
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  #text(PC$MatrixEsc, cex = 1, pos = 3, LinLab)  # Coloca os nomes dos pontos das coordenadas principais das linhas
  LocLab(PC$MatrixEsc, cex = 1, LinLab)
  ##### FIM - Plotagem dos Dados das linhas #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem das Correlacoes dos Componentes Principais com as Variaveis Originais #####
  plot(0,0, # cria grafico para as coordenadas das Correlacoes dos Componentes Principais com as Variaveis Originais
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[3], # Titulo
       asp  = 1, # Aspecto do Grafico
       cex  = 0, # Tamanho dos pontos
       xlim = c(-1.1,1.1), # Dimensao para as linhas do grafico
       ylim = c(-1.1,1.1)) # Dimensao para as colunas do grafico
  
  symbols(0, 0, circles = 1, inches = FALSE, fg = 1, add = TRUE) # cria um circulo
  
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  arrows(0,0,PC$MatrixCCP[1,],PC$MatrixCCP[2,], lty=1, code = 2, length = 0.08, angle = 25, col = ifelse(Color,"Red","Black")) # cria a seta apontando para cada coordenada principal
  
  #text(t(PC$MatrixCCP), cex=1, colnames(PC$MatrixCCP) , col = ifelse(Color,"Blue","Black"), pos = 3, xpd = TRUE)  # Coloca os nomes dos pontos das coordenadas principais
  LocLab(t(PC$MatrixCCP), cex=1, colnames(PC$MatrixCCP) , col = ifelse(Color,"Blue","Black"), xpd = TRUE)
  ##### FIM - Plotagem das Correlacoes dos Componentes Principais com as Variaveis Originais #####
}