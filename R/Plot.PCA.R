Plot.PCA <- function(PC, titles = NA, xlabel = NA, ylabel = NA, size = 1.1,
                     grid = TRUE, color = TRUE, linlab = NA, savptc = FALSE,
                     width = 3236, height = 2000, res = 300, casc = TRUE) {
  # Rotina para Plotar Graficos do Metodo PCA desenvolvida 
  # por Paulo Cesar Ossani em 11/2014
  
  # Entrada:
  # PC     - Dados da funcao PCA.
  # titles - Titulos para os graficos.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # size   - Tamanho dos pontos nos graficos.
  # grid   - Coloca grade nos graficos.
  # color  - Graficos coloridos (default = TRUE).
  # linlab - Vetor com os rotulos das observacoes.
  # savptc - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width  - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res    - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).
  # casc   - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(titles[1]) || is.na(titles[1])) titles[1] = c("Scree-plot das variancias dos componentes")
  if (!is.character(titles[2]) || is.na(titles[2])) titles[2] = c("Grafico correspondente as linhas (observacoes)")
  if (!is.character(titles[3]) || is.na(titles[3])) titles[3] = c("Grafico correspondente as colunas (variaveis)")
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.logical(color))
     stop("Entrada para 'color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.numeric(size) || size < 0)
     stop("Entrada para 'size' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.logical(grid))
     stop("Entrada para 'grid' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
   
  if (!is.na(linlab[1]) && length(linlab)!=nrow(PC$mtxscores))
     stop("O numero elementos do rotulo para linhas 'linlab' difere do numero de linhas da base de dados. Verifique!")
  
  if (!is.logical(savptc))
     stop("Entrada para 'savptc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.numeric(width) || width <= 0)
     stop("Entrada para 'width' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.numeric(height) || height <= 0)
     stop("Entrada para 'height' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.numeric(res) || res <= 0)
     stop("Entrada para 'res' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.logical(casc && !savptc))
     stop("Entrada para 'casc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (is.na(xlabel[1]))
     xlabel = paste("Primeira coordenada (",round(PC$mtxAutvlr[1,2],2),"%)",sep="")
  
  if (is.na(ylabel[1]))
     ylabel = paste("Segunda coordenada (",round(PC$mtxAutvlr[2,2],2),"%)",sep="")
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (savptc) {
     cat("\014") # limpa a tela
     cat("\n\n Salvando graficos em disco. Aguarde o termino!")
  }
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos

  ##### INICIO - Plotagem dos Autovalores #####
  if (savptc) png(filename = "Figure PCA Variances.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  mp <- barplot(PC$mtxAutvlr[,1],names.arg=paste(round(PC$mtxAutvlr[,2],2),"%",sep=""),
                main = "Variancias dos componentes")
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Plotagem dos Autovalores #####
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Scree-plot dos componentes #####
  if (savptc) png(filename = "Figure PCA Scree Plot.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  plot(1:length(PC$mtxAutvlr[,1]), PC$mtxAutvlr[,1],
       type = "n", # nao plota pontos
       xlab = "Ordem dos componentes", 
       ylab = "Variancia",
       xaxt = "n", # tira o eixo x
       main = titles[1])
  
  axis(1, c(1:length(PC$mtxAutvlr[,1])), c(1:length(PC$mtxAutvlr[,1])))
  
  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  points(1:length(PC$mtxAutvlr[,1]), PC$mtxAutvlr[,1], type = "b")
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Scree-plot dos componentes #####

  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos Dados das linhas #####
  if (savptc) png(filename = "Figure PCA Observations.png", width = width, height = height, res = res) # salva os graficos em arquivo
  
  plot(PC$mtxscores, # cria grafico para as coordenadas principais das linhas
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       type = "n", # nao plota pontos
       main = titles[2], # Titulo
       # asp  = 1,  # Aspecto do Grafico
       xlim = c(min(PC$mtxscores[,1])-0.05,max(PC$mtxscores[,1])+0.05), # Dimensao para as linhas do grafico
       ylim = c(min(PC$mtxscores[,2])-0.05,max(PC$mtxscores[,2])+0.05)) # Dimensao para as colunas do grafico

  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }

  points(PC$mtxscores, # cria grafico para as coordenadas principais das linhas
         pch = 15,   # Formato dos pontos 
         cex = size, # Tamanho dos pontos  
         col = ifelse(color,"red","black"))  # Cor dos pontos
    
  abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
  
  if (!is.na(linlab[1])) LocLab(PC$mtxscores, cex = 1, linlab)
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Plotagem dos Dados das linhas #####
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem das Correlacoes dos Componentes Principais com as Variaveis Originais #####
  if (savptc) png(filename = "Figure PCA Correlations.png", width = width, height = height, res = res) # salva os graficos em arquivo
  
  plot(0,0, # cria grafico para as coordenadas das Correlacoes dos Componentes Principais com as Variaveis Originais
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = titles[3], # Titulo
       asp  = 1, # Aspecto do Grafico
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
  
  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  arrows(0,0,PC$mtxCCP[1,],PC$mtxCCP[2,], lty=1, code = 2, length = 0.08, angle = 25, col = ifelse(color,"Red","Black")) # cria a seta apontando para cada coordenada principal
  
  LocLab(t(PC$mtxCCP), cex=1, colnames(PC$mtxCCP) , col = ifelse(color,"Blue","Black"), xpd = TRUE)
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Plotagem das Correlacoes dos Componentes Principais com as Variaveis Originais #####
  
  if (savptc) cat("\n \n Fim!")
  
}