Plot.FA <- function(FA, Titles = NA, xlabel = NA, ylabel = NA,
                    Color = TRUE, LinLab = NA, Casc = TRUE) {
  # Rotina para Plotar Graficos do Metodo FA desenvolvida 
  # por Paulo Cesar Ossani em 02/2017
  
  # Entrada:
  # FA     - Dados da funcao FA
  # Titles - Titulos para os graficos
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
  if (!is.character(Titles[1]) || is.na(Titles[1])) Titles[1] = c("Scree-plot das variancias\n das cargas fatoriais")
  if (!is.character(Titles[2]) || is.na(Titles[2])) Titles[2] = c("Escores das observacoes dos\n dois primeiros fatores")
  if (!is.character(Titles[3]) || is.na(Titles[3])) Titles[3] = c("Cargas fatoriais")
  if (!is.character(Titles[4]) || is.na(Titles[4])) Titles[4] = c("Biplot")
  
  if (!is.character(xlabel) && !is.na(xlabel))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.logical(Color))
     stop("Entrada para 'Color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.na(LinLab) && length(LinLab)!=nrow(FA$MatrixScores))
     stop("O numero elementos do rotulo para linhas 'LinLab' difere do numero de linhas da base de dados. Verifique!")
  
  if (!is.logical(Casc))
     stop("Entrada para 'Casc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (is.na(LinLab[1])) LinLab <- rownames(FA$MatrixScores)
  
  if (is.na(xlabel))
     xlabel = "Primeiro fator"
  
  if (is.na(ylabel))
     ylabel = "Segundo fator"
  
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Autovalores #####
  mp <- barplot(FA$MatrixVar[,1],names.arg=paste(round(FA$MatrixVar[,2],2),"%",sep=""),main = "Variancias dos Fatores")
  ##### FIM - Plotagem dos Autovalores #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Scree-plot dos Fatores #####
  plot(1:length(FA$MatrixVar[,1]), FA$MatrixVar[,1], type = "b", 
       xlab = "Ordem dos Fatores", 
       ylab = "Variancia",
       main = Titles[1])
  ##### FIM - Scree-plot dos Fatores #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem Escores das observacoes #####
  plot(FA$MatrixScores,  # cria grafico para os Escores das observacoes 
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[2], # Titulo
       asp  = 1,  # Aspecto do Grafico
       pch  = 15, # Formato dos pontos
       cex  = 1,  # Tamanho dos pontos
       xlim = c(min(FA$MatrixScores[,1])-0.05,max(FA$MatrixScores[,1])+0.05), # Dimensao para as linhas do grafico
       ylim = c(min(FA$MatrixScores[,2])-0.05,max(FA$MatrixScores[,2])+0.05), # Dimensao para as colunas do grafico
       col  = ifelse(Color,"red","black")) # Cor dos pontos

  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central

  #text(FA$MatrixScores, cex = 1, pos = 3, LinLab)  # Coloca os nomes dos pontos das coordenadas principais das linhas
  LocLab(FA$MatrixScores, cex = 1, LinLab)
  ##### FIM - Plotagem Escores das observacoes #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Cargas fatoriais #####
  HpMat <- rbind(c(0,0),FA$MatrixCarga[,1:2])
  MaxX  <- max(HpMat[,1]) + 0.05 # Dimenssoes maximas das linhas
  MinX  <- min(HpMat[,1]) - 0.05 # Dimenssoes minimas das linhas
  MaxY  <- max(HpMat[,2]) + 0.05 # Dimenssoes maximas das colunas
  MinY  <- min(HpMat[,2]) - 0.05 # Dimenssoes minimas das colunas
  
  plot(0,0, # Plota as variaveis
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[3], # Titulo
       asp  = 1, # Aspecto do grafico
       cex  = 0, # Tamanho dos pontos
       xlim = c(MinX,MaxX), # Dimensao para as linhas do grafico
       ylim = c(MinY,MaxY)) # Dimensao para as colunas do grafico
       # xlim=c(min(FA$MatrixCarga[,1])-0.05,max(FA$MatrixCarga[,1])+0.05), # Dimensao para as linhas do grafico
       # ylim=c(min(FA$MatrixCarga[,2])-0.05,max(FA$MatrixCarga[,2])+0.05)) # Dimensao para as colunas do grafico
       
  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  arrows(0,0,FA$MatrixCarga[,1],FA$MatrixCarga[,2], lwd = 1, code = 2, length = 0.08, angle = 25, col = ifelse(Color,"Red","Black")) # cria a seta apontando para cada variavel  
  
  NomeVar <- rownames(FA$MatrixCarga) # nomes das variaveis
  LocLab(FA$MatrixCarga[,1:2], NomeVar, col = ifelse(Color,"Blue","Black"))  # Coloca os nomes das variaveis
  ##### FIM - Cargas fatoriais #####

  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Biplot ##### 
  HpMat <- rbind(c(0,0),FA$MatrixCarga[,1:2],FA$MatrixScores[,1:2])
  MaxX  <- max(HpMat[,1]) + 0.05 # Dimenssoes maximas das linhas
  MinX  <- min(HpMat[,1]) - 0.05 # Dimenssoes minimas das linhas
  MaxY  <- max(HpMat[,2]) + 0.05 # Dimenssoes maximas das colunas
  MinY  <- min(HpMat[,2]) - 0.05 # Dimenssoes minimas das colunas
  # MaxX <- max(FA$MatrixCarga[,1],FA$MatrixScores[,1])+0.05 # Dimenssoes maximas das linhas
  # MinX <- min(FA$MatrixCarga[,1],FA$MatrixScores[,1])-0.05 # Dimenssoes minimas das linhas
  # MaxY <- max(FA$MatrixCarga[,2],FA$MatrixScores[,2])+0.05 # Dimenssoes maximas das colunas
  # MinY <- min(FA$MatrixCarga[,2],FA$MatrixScores[,2])-0.05 # Dimenssoes minimas das colunas
  
  plot(0,0, # Plota as variaveis
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[4], # Titulo
       asp  = 1, # Aspecto do grafico
       cex  = 0, # Tamanho dos pontos
       xlim = c(MinX,MaxX), # Dimensao para as linhas do grafico
       ylim = c(MinY,MaxY)) # Dimensao para as colunas do grafico

  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  arrows(0,0,FA$MatrixCarga[,1],FA$MatrixCarga[,2], lwd = 1, code = 2, length = 0.08, angle = 25, col = ifelse(Color,"Black","Black")) # cria a seta apontando para cada variavel  
  
  NomeVar <- rownames(FA$MatrixCarga) # nomes das variaveis
  LocLab(FA$MatrixCarga[,1:2], NomeVar, col = ifelse(Color,"Blue","Black"))  # Coloca os nomes das variaveis
  
  points(FA$MatrixScores,    # Coloca pontos nas posicoes dos individuos
         asp = 1,  # Aspecto do grafico
         pch = 15, # Formato dos pontos 
         cex = 1,  # Tamanho dos pontos         
         col = ifelse(Color,"Red","Black"))
  
  LocLab(FA$MatrixScores, cex = 1, LinLab)
  ##### FIM - Biplot #####
}