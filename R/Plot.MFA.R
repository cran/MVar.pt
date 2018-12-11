Plot.MFA <- function(MFA, Titles = NA, xlabel = NA, ylabel = NA,
                     PosLeg = 2, BoxLeg = TRUE, Color = TRUE,  
                     NamArr = FALSE, LinLab = NA, Casc = TRUE) {
  
  # Rotina para Plotar Graficos do Metodo MFA desenvolvida 
  # por Paulo Cesar Ossani em 09/2013 a 01/2014
  
  # Entrada:
  # MF - Dados da funcao MFA
  # Titles - Titulos para os graficos. Se nao for definido assume texto padrao.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # PosLeg - 1 para legenda no canto superior esquerdo
  #          2 para legenda no canto superior direito (default)
  #          3 para legenda no canto inferior direito
  #          4 para legenda no canto inferior esquerdo
  # BoxLeg - Coloca moldura na legenda (default = TRUE).
  # Color  - Graficos coloridos (default = TRUE).
  # NamArr - Colocar nomes nos pontos na nuvem ao redor do
  #          centroide no Grafico Correspondente a Analise 
  #          Global dos Individuos e Variaveis (default = FALSE).
  # LinLab - Nomes dos centroides, se omitido retorna os rotulos das linhas.
  # Casc    - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(Titles[1]) || is.na(Titles[1])) Titles[1] = c("Scree-plot das variancias dos componentes")
  if (!is.character(Titles[2]) || is.na(Titles[2])) Titles[2] = c("Grafico correspondente a analise global dos individuos")
  if (!is.character(Titles[3]) || is.na(Titles[3])) Titles[3] = c("Grafico correspondente a analise\n global dos individuos e variaveis")
  if (!is.character(Titles[4]) || is.na(Titles[4])) Titles[4] = c("Grafico das inercias dos Grupos de variaveis")
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (PosLeg < 1 || PosLeg > 4)
     stop("Entrada para posicao da legenda 'PosLeg' esta incorreta. Verifique!")
  
  if (!is.logical(BoxLeg)) 
     stop("Entrada para moldura da legenda 'BoxLeg' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(Color))
     stop("Entrada para 'Color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(NamArr)) 
     stop("Entrada para 'NamArr' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(Casc))
     stop("Entrada para 'Casc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  Groups     = MFA$VectorG  # tamanho de cada grupo
  NameGroups = MFA$VectorNG # nomes de cada grupo
  
  if (!is.na(LinLab[1])) {
    if (length(LinLab) != nrow(MFA$MatrixF))
       stop("Entrada para 'LinLab' esta incorreta, deve ter o mesmo numero de linhas que os dados de entrada em 'MFA'. Verifique!")
    NomeLinhas = as.matrix(LinLab) # nomes das linhas que formam os dados
  } else {
    NomeLinhas = rownames(MFA$MatrixF) # nomes das linhas que formam os dados
  }
  
  NumGroups  = length(NameGroups) # Numero de Grupos
  
  cor = 1 # cor inicial
  
  if (is.na(xlabel[1]))
     xlabel  = paste("Primeira coordenada (",round(MFA$MatrixA[1,2],2),"%)",sep="")
  
  if (is.na(ylabel[1]))
     ylabel  = paste("Segunda coordenada (",round(MFA$MatrixA[2,2],2),"%)",sep="")
  
  if (PosLeg==1) PosLeg = "topleft"     # posicao das legendas nos graficos
  if (PosLeg==2) PosLeg = "topright"
  if (PosLeg==3) PosLeg = "bottomright"
  if (PosLeg==4) PosLeg = "bottomleft"
  
  BoxLeg = ifelse(BoxLeg,"o","n") # moldura nas legendas, FALSE sem moldura, "o" com moldura
  
  Color_a = ifelse(Color,"red","black") # cores nos pontos dos graficos
  Color_b = cor # coreas para letras das legendas e suas representacoes no grafico
  if (Color) Color_b = (cor+1):(cor+NumGroups)
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Autovalores #####
  mp <- barplot(MFA$MatrixA[,1],names.arg=paste(round(MFA$MatrixA[,2],2),"%",sep=""),main = "Variancias dos componentes")
  ##### FIM - Plotagem dos Autovalores #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Scree-plot dos componentes #####
  plot(1:length(MFA$MatrixA[,1]), MFA$MatrixA[,1], type = "b", 
       xlab = "Ordem dos componentes", 
       ylab = "Variancia",
       main = Titles[1])
  ##### FIM - Scree-plot dos componentes #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem da Analise Global #####
  plot(MFA$MatrixF, # cria grafico para as coordenadas principais da Analise Global
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[2], # Titulo
       asp  = 2,         # Aspecto do Grafico
       pch  = 15,        # Formato dos pontos 
       cex  = 1,         # Tamanho dos pontos
       xlim = c(min(MFA$MatrixF[,1])-0.1,max(MFA$MatrixF[,1])+0.1), # Dimensao para as linhas do grafico
       ylim = c(min(MFA$MatrixF[,2]-0.1),max(MFA$MatrixF[,2])+0.1), # Dimensao para as colunas do grafico
       col  = ifelse(Color,"red","black")) # Cor dos pontos
  
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  LocLab(MFA$MatrixF[,1:2], NomeLinhas)  # Coloca os nomes dos pontos das coordenadas principais das linhas
  #text(MFA$MatrixF, cex=1, NomeLinhas, pos=3, xpd = TRUE)  # Coloca os nomes dos pontos das coordenadas principais das linhas
  ##### FIM - Plotagem da Analise Global #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem da Analise por Grupo Juntamente com a Analise Global #####
  ## INICIO - Encontra as dimensoes maximas e minimas para as colunas e linhas ##
  MLC <- MFA$MatrixF[,1:2]
  for (i in 1:length(MFA$MatrixEFG)) 
    MLC <- rbind(MLC,MFA$MatrixEFG[[i]][,1:2])
  maxX = max(MLC[,1]) # Dimenssoes maximas das linhas do grafico
  minX = min(MLC[,1]) # Dimenssoes minimas das linhas do grafico
  maxY = max(MLC[,2]) # Dimenssoes maximas das colunas do grafico
  minY = min(MLC[,2]) # Dimenssoes minimas das colunas do grafico
  ## FIM - Encontra as dimensoes maximas e minimas para as colunas e linhas ##
  
  plot(MFA$MatrixF, # cria grafico para as coordenadas principais da Analise por Grupo
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[3], # Titulo
       asp  = 1,         # Aspecto do grafico
       pch  = 15,        # Formato dos pontos 
       cex  = 1,         # Tamanho dos pontos
       xlim = c(minX,maxX), # Dimensao para as linhas do grafico
       ylim = c(minY,maxY), # Dimensao para as colunas do grafico
       col  = Color_a)   # Cor dos pontos
  
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  LocLab(MFA$MatrixF[,1:2], NomeLinhas)  # Coloca os nomes dos pontos das coordenadas principais da analise global
  #text(MFA$MatrixF, cex=1,NomeLinhas, pos=3, xpd = TRUE)  # Coloca os nomes dos pontos das coordenadas principais da analise global
  ## Acrescenta no grafico da Analise Global as coordenadas principais da Analise por Grupo
  NumObserv = 4 # numero de centroides a considerar para plotagem das orbitas
  NumLinhas = nrow(MFA$MatrixEFG[[1]]) # numero de linhas
  if (NumObserv<NumLinhas) {
    Position = floor(NumLinhas/NumObserv)
    Observ = as.vector(c(rep(1,NumObserv))) # nomes dos centroides
    for (i in 1:(length(Observ)-2)) {
      Observ[i+1] = Position*i
    }     
    Observ[length(Observ)]=NumLinhas # nomes dos centroides
  }
  
  if (NumObserv>=NumLinhas)
     Observ = 1:NumLinhas  # nomes dos centroides
  
  for (i in 1:length(MFA$MatrixEFG)) {
    if (NamArr==FALSE) 
       points(MFA$MatrixEFG[[i]][Observ,1:2], pch = (2 + ifelse(Color,i,0)), cex = 1.2, col = 1 + ifelse(Color,i,0)) # adiciona ao grafico as coordenadas principais dos Grupos
    else
      LocLab(MFA$MatrixEFG[[i]][Observ,1:2],NameGroups[i], col = 1 + ifelse(Color,i,0)) # Coloca os nomes dos pontos das coordenadas principais dos Grupos
    #text(MFA$MatrixEFG[[i]][Observ,1:2], pos=3, cex=1, NameGroups[i], col = 1 + ifelse(Color,i,0),xpd = TRUE) # Coloca os nomes dos pontos das coordenadas principais dos Grupos
  }
  
  ## liga os pontos de cada Analise Global com cada ponto da Analise por Grupo
  for (j in 1:length(MFA$MatrixEFG)) 
     segments(MFA$MatrixF[Observ,1], MFA$MatrixF[Observ,2], MFA$MatrixEFG[[j]][Observ,1], MFA$MatrixEFG[[j]][Observ,2], lty = cor + j, col = ifelse(Color,cor + j,cor), lwd=1.5)
  
  if (NamArr==FALSE)
     legend(PosLeg, NameGroups, lty = (cor+1):(cor+NumGroups), col = Color_b, text.col = Color_b,
            bty=BoxLeg, text.font = 6, y.intersp = 0.9, xpd = TRUE) # cria a legenda
  ##### FIM - Plotagem de Analise por Grupo Juntamento com a Analise Global #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem das Correlacoes dos Componentes Principais com as Variaveis Originais #####
  plot(0,0, # cria grafico para as coordenadas das Correlacoes dos Componentes Principais com as Variaveis Originais
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = "Circulo de Correlacao", # Titulo
       asp  = 1,         # Aspecto do grafico
       cex  = 0,         # Tamanho dos pontos
       xlim = c(-1.1,1.1), # Dimensao para as linhas do grafico
       ylim = c(-1.1,1.1)) # Dimensao para as colunas do grafico
  
  symbols(0, 0, circles = 1, inches = FALSE, fg = 1, add = TRUE) # cria um circulo
  
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  j  <- 1         # coluna inicial do Grupo de variaveis
  k  <- Groups[1] # coluna final do Grupo de variaveis
  for (i in 1:NumGroups) {  # foi necessario criar este for para poder colocar cores diferentes para cada Grupo de variaveis
    
    arrows(0,0,MFA$MatrixCCP[1,j:k],MFA$MatrixCCP[2,j:k], lty=i, code = 2, angle = 10, col = ifelse(Color,cor + i,cor)) # cria a seta apontando para cada coordenada principal
    
    if (is.na(colnames(MFA$MatrixCCP[,j:k]))[1])
      NomeVar<- paste("Comp.", 1:Groups[i], sep = "") # Nomeia as colunas
    else
      NomeVar<- colnames(MFA$MatrixCCP[,j:k])
    
    LocLab(t(MFA$MatrixCCP[,j:k]), NomeVar, col = ifelse(Color,cor + i,cor)) # Coloca os nomes dos pontos das coordenadas principais
    #text(t(MFA$MatrixCCP[,j:k]), cex=1, pos=3, NomeVar, col = ifelse(Color,cor + i,cor), xpd = TRUE)  # Coloca os nomes dos pontos das coordenadas principais
    
    j <- j + Groups[i]  # coluna inicial do Grupo de variaveis
    
    k <- k + Groups[i+ifelse(i!=NumGroups,1,0)]  # coluna final do Grupo de variaveis  
    
  }
  
  legend(PosLeg, NameGroups, lty = cor:(cor+NumGroups), col = Color_b, text.col = Color_b,
         bty=BoxLeg, text.font = 6, y.intersp = 0.9, xpd = TRUE) # cria a legenda
  ##### FIM - Plotagem das Correlacoes dos Componentes Principais com as Variaveis Originais #####
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem das Inercias Parciais/Escores das Variareis #####
  VlrMinX = ifelse(min(MFA$MatrixEscVar[,1])>0,-0.01, min(MFA$MatrixEscVar[,1])) # Valor minimo para a linha X
  VlrMinY = ifelse(min(MFA$MatrixEscVar[,2])>0,-0.01, min(MFA$MatrixEscVar[,2])) # Valor minimo para a linha Y
  VlrMaxX = 1.01 # Valor maximo para a linha X
  VlrMaxY = 1.01 # Valor maximo para a linha Y
  plot(MFA$MatrixEscVar, # cria grafico para as coordenadas Inercias Parciais/Escores das Variareis
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = Titles[4], # Titulo
       asp  = 1,         # Aspecto do grafico
       pch  = 15,        # Formato dos pontos 
       cex  = 1,         # Tamanho dos pontos
       xlim = c(VlrMinX,VlrMaxX), # Dimensao para as linhas do grafico
       ylim = c(VlrMinY,VlrMaxY), # Dimensao para as colunas do grafico
       col  = Color_a)   # Cor dos pontos
  
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  LocLab(MFA$MatrixEscVar[,1:2],rownames(MFA$MatrixEscVar))  # Coloca os nomes dos pontos das coordenadas principais das linhas
  #text(MFA$MatrixEscVar,cex=1, rownames(MFA$MatrixEscVar), pos=3, xpd = TRUE)  # Coloca os nomes dos pontos das coordenadas principais das linhas
  ##### FIM - Plotagem das Inercias Parciais/Escores das Variareis #####
}