Plot.MFA <- function(MFA, titles = NA, xlabel = NA, ylabel = NA, posleg = 2, 
                     boxleg = TRUE, size = 1.1, grid = TRUE, color = TRUE, 
                     namarr = FALSE, linlab = NA, casc = TRUE) {
  
  # Rotina para Plotar Graficos do Metodo MFA desenvolvida 
  # por Paulo Cesar Ossani em 09/2013 a 01/2014
  
  # Entrada:
  # MF - Dados da funcao MFA
  # titles - Titulos para os graficos. Se nao for definido assume texto padrao.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # posleg - 1 para legenda no canto superior esquerdo
  #          2 para legenda no canto superior direito (default)
  #          3 para legenda no canto inferior direito
  #          4 para legenda no canto inferior esquerdo
  # boxleg - Coloca moldura na legenda (default = TRUE).
  # size     - Tamanho dos pontos nos graficos.
  # grid     - Coloca grade nos graficos.
  # color  - Graficos coloridos (default = TRUE).
  # namarr - Colocar nomes nos pontos na nuvem ao redor do
  #          centroide no Grafico Correspondente a Analise 
  #          Global dos Individuos e Variaveis (default = FALSE).
  # linlab - Nomes dos centroides, se omitido retorna os rotulos das linhas.
  # casc    - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(titles[1]) || is.na(titles[1])) titles[1] = c("Scree-plot das variancias dos componentes")
  if (!is.character(titles[2]) || is.na(titles[2])) titles[2] = c("Grafico correspondente a analise global dos individuos")
  if (!is.character(titles[3]) || is.na(titles[3])) titles[3] = c("Grafico correspondente a analise\n global dos individuos e variaveis")
  if (!is.character(titles[4]) || is.na(titles[4])) titles[4] = c("Grafico das inercias dos grupos de variaveis")
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (posleg < 1 || posleg > 4)
     stop("Entrada para posicao da legenda 'posleg' esta incorreta. Verifique!")
  
  if (!is.logical(boxleg)) 
     stop("Entrada para moldura da legenda 'boxleg' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.numeric(size) || size < 0)
     stop("Entrada para 'size' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.logical(grid))
     stop("Entrada para 'grid' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(color))
     stop("Entrada para 'color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(namarr)) 
     stop("Entrada para 'namarr' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(casc))
     stop("Entrada para 'casc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  Groups     = MFA$vtrG  # tamanho de cada grupo
  NameGroups = MFA$vtrNG # nomes de cada grupo
  
  if (!is.na(linlab[1])) {
    if (length(linlab) != nrow(MFA$mtxF))
       stop("Entrada para 'linlab' esta incorreta, deve ter o mesmo numero de linhas que os dados de entrada em 'MFA'. Verifique!")
    NomeLinhas = as.matrix(linlab) # nomes das linhas que formam os dados
  } else {
    NomeLinhas = rownames(MFA$mtxF) # nomes das linhas que formam os dados
  }
  
  NumGroups  = length(NameGroups) # Numero de Grupos
  
  cor = 1 # cor inicial
  
  if (is.na(xlabel[1]))
     xlabel  = paste("Primeira coordenada (",round(MFA$mtxA[1,2],2),"%)",sep="")
  
  if (is.na(ylabel[1]))
     ylabel  = paste("Segunda coordenada (",round(MFA$mtxA[2,2],2),"%)",sep="")
  
  if (posleg==1) posleg = "topleft" # posicao das legendas nos graficos
  if (posleg==2) posleg = "topright"
  if (posleg==3) posleg = "bottomright"
  if (posleg==4) posleg = "bottomleft"

  boxleg = ifelse(boxleg,"o","n") # moldura nas legendas, FALSE sem moldura, "o" com moldura
  
  color_a = ifelse(color,"red","black") # cores nos pontos dos graficos
  color_b = cor # coreas para letras das legendas e suas representacoes no grafico
  if (color) color_b = (cor+1):(cor+NumGroups)
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Autovalores #####
  mp <- barplot(MFA$mtxA[,1],names.arg=paste(round(MFA$mtxA[,2],2),"%",sep=""),
                main = "Variancias dos componentes")
  ##### FIM - Plotagem dos Autovalores #####
  
  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Scree-plot dos componentes #####
  plot(1:length(MFA$mtxA[,1]), MFA$mtxA[,1], 
       type = "n", # nao plota pontos
       xlab = "Ordem dos componentes", 
       ylab = "Variancia",
       main = titles[1])
  
  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  points(1:length(MFA$mtxA[,1]), MFA$mtxA[,1], type = "b")
  ##### FIM - Scree-plot dos componentes #####
  
  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem da Analise Global #####
  plot(MFA$mtxF, # cria grafico para as coordenadas principais da Analise Global
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       type = "n",   # nao plota pontos
       main = titles[2], # Titulo
       # asp  = 2,     # Aspecto do Grafico
       xlim = c(min(MFA$mtxF[,1])-0.1,max(MFA$mtxF[,1])+0.1), # Dimensao para as linhas do grafico
       ylim = c(min(MFA$mtxF[,2]-0.1),max(MFA$mtxF[,2])+0.1)) # Dimensao para as colunas do grafico

  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  points(MFA$mtxF, # cria grafico para as coordenadas principais da Analise Global
         pch = 15,    # Formato dos pontos 
         cex = size,  # Tamanho dos pontos   
         col = ifelse(color,"red","black")) # Cor dos pontos 
    
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  LocLab(MFA$mtxF[,1:2], NomeLinhas)  # Coloca os nomes dos pontos das coordenadas principais das linhas
  ##### FIM - Plotagem da Analise Global #####
  
  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem da Analise por Grupo Juntamente com a Analise Global #####
  ## INICIO - Encontra as dimensoes maximas e minimas para as colunas e linhas ##
  MLC <- MFA$mtxF[,1:2]
  for (i in 1:length(MFA$mtxEFG)) 
    MLC <- rbind(MLC,MFA$mtxEFG[[i]][,1:2])
  maxX = max(MLC[,1]) # Dimenssoes maximas das linhas do grafico
  minX = min(MLC[,1]) # Dimenssoes minimas das linhas do grafico
  maxY = max(MLC[,2]) # Dimenssoes maximas das colunas do grafico
  minY = min(MLC[,2]) # Dimenssoes minimas das colunas do grafico
  ## FIM - Encontra as dimensoes maximas e minimas para as colunas e linhas ##
  
  plot(MFA$mtxF, # cria grafico para as coordenadas principais da Analise por Grupo
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       type = "n",    # nao plota pontos
       main = titles[3], # Titulo
       # asp  = 1,         # Aspecto do grafico
       xlim = c(minX,maxX), # Dimensao para as linhas do grafico
       ylim = c(minY,maxY)) # Dimensao para as colunas do grafico

  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  points(MFA$mtxF,   # cria grafico para as coordenadas principais da Analise por Grupo
         pch = 15,      # Formato dos pontos 
         cex = size,    # Tamanho dos pontos  
         col = color_a) # Cor dos pontos
    
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  LocLab(MFA$mtxF[,1:2], NomeLinhas)  # Coloca os nomes dos pontos das coordenadas principais da analise global
  ## Acrescenta no grafico da Analise Global as coordenadas principais da Analise por Grupo
  NumObserv = 4 # numero de centroides a considerar para plotagem das orbitas
  NumLinhas = nrow(MFA$mtxEFG[[1]]) # numero de linhas
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
  
  for (i in 1:length(MFA$mtxEFG)) {
    if (namarr==FALSE) 
       points(MFA$mtxEFG[[i]][Observ,1:2], pch = (2 + ifelse(color,i,0)), cex = 1.2, col = 1 + ifelse(color,i,0)) # adiciona ao grafico as coordenadas principais dos Grupos
    else
      LocLab(MFA$mtxEFG[[i]][Observ,1:2],NameGroups[i], col = 1 + ifelse(color,i,0)) # Coloca os nomes dos pontos das coordenadas principais dos Grupos
  }
  
  ## liga os pontos de cada Analise Global com cada ponto da Analise por Grupo
  for (j in 1:length(MFA$mtxEFG)) 
     segments(MFA$mtxF[Observ,1], MFA$mtxF[Observ,2], MFA$mtxEFG[[j]][Observ,1], MFA$mtxEFG[[j]][Observ,2], lty = cor + j, col = ifelse(color,cor + j,cor), lwd=1.5)
  
  if (namarr==FALSE)
     legend(posleg, NameGroups, lty = (cor+1):(cor+NumGroups), col = color_b, text.col = color_b,
            bty=boxleg, text.font = 6, y.intersp = 0.9, xpd = TRUE) # cria a legenda
  ##### FIM - Plotagem de Analise por Grupo Juntamento com a Analise Global #####
  
  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem das Correlacoes dos Componentes Principais com as Variaveis Originais #####
  plot(0,0, # cria grafico para as coordenadas das Correlacoes dos Componentes Principais com as Variaveis Originais
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = "Circulo de Correlacao", # Titulo
       asp  = 1,   # Aspecto do grafico
       axes = F,
       type = "n", # nao plota pontos
       xlim = c(-1.1,1.1), # Dimensao para as linhas do grafico
       ylim = c(-1.1,1.1)) # Dimensao para as colunas do grafico

  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  symbols(0, 0, circles = 1, inches = FALSE, fg = 1, add = TRUE) # cria um circulo
  
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  j <- 1         # coluna inicial do Grupo de variaveis
  k <- Groups[1] # coluna final do Grupo de variaveis
  for (i in 1:NumGroups) {  # foi necessario criar este for para poder colocar cores diferentes para cada Grupo de variaveis
    
    arrows(0,0,MFA$mtxCCP[1,j:k],MFA$mtxCCP[2,j:k], lty=i, code = 2, angle = 10, col = ifelse(color,cor + i,cor)) # cria a seta apontando para cada coordenada principal
    
    if (is.na(colnames(MFA$mtxCCP[,j:k]))[1])
      NomeVar<- paste("Comp.", 1:Groups[i], sep = "") # Nomeia as colunas
    else
      NomeVar<- colnames(MFA$mtxCCP[,j:k])
    
    LocLab(t(MFA$mtxCCP[,j:k]), NomeVar, col = ifelse(color,cor + i,cor)) # Coloca os nomes dos pontos das coordenadas principais
 
    j <- j + Groups[i]  # coluna inicial do Grupo de variaveis
    
    k <- k + Groups[i+ifelse(i!=NumGroups,1,0)]  # coluna final do Grupo de variaveis  
    
  }
  
  legend(posleg, NameGroups, lty = cor:(cor+NumGroups), col = color_b, text.col = color_b,
         bty=boxleg, text.font = 6, y.intersp = 0.9, xpd = TRUE) # cria a legenda
  ##### FIM - Plotagem das Correlacoes dos Componentes Principais com as Variaveis Originais #####
  
  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem das Inercias Parciais/Escores das Variareis #####
  VlrMinX = ifelse(min(MFA$mtxEV[,1])>0,-0.01, min(MFA$mtxEV[,1])) # Valor minimo para a linha X
  VlrMinY = ifelse(min(MFA$mtxEV[,2])>0,-0.01, min(MFA$mtxEV[,2])) # Valor minimo para a linha Y
  VlrMaxX = 1.01 # Valor maximo para a linha X
  VlrMaxY = 1.01 # Valor maximo para a linha Y
  
  plot(MFA$mtxEV, # cria grafico para as coordenadas Inercias Parciais/Escores das Variareis
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       type = "n",    # nao plota pontos
       main = titles[4], # Titulo
       # asp  = 1,         # Aspecto do grafico
       xlim = c(VlrMinX,VlrMaxX), # Dimensao para as linhas do grafico
       ylim = c(VlrMinY,VlrMaxY)) # Dimensao para as colunas do grafico

  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  points(MFA$mtxEV, # cria grafico para as coordenadas Inercias Parciais/Escores das Variareis
         pch = 15,      # Formato dos pontos 
         cex = size,    # Tamanho dos pontos  
         col = color_a) # Cor dos pontos
    
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  LocLab(MFA$mtxEV[,1:2],rownames(MFA$mtxEV))  # Coloca os nomes dos pontos das coordenadas principais das linhas
  ##### FIM - Plotagem das Inercias Parciais/Escores das Variareis #####
}