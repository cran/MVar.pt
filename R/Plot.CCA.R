Plot.CCA <- function(CCA, titles = NA, xlabel = NA, ylabel = NA, 
                     size = 1.1, grid = TRUE, color = TRUE, savptc = FALSE, 
                     width = 3236, height = 2000, res = 300, casc = TRUE) {
  # Rotina para Plotar Graficos do Metodo CCA desenvolvida 
  # por Paulo Cesar Ossani em 09/04/2016
  
  # CCA    - Dados da funcao CCA.
  # titles - Titulos para os graficos. Se nao for definido assume texto padrao.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # size   - Tamanho dos pontos nos graficos.
  # grid   - Coloca grade nos graficos.
  # color  - Graficos coloridos (default = TRUE).
  # savptc - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width  - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res    - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).
  # casc   - Efeito cascata na apresentacao dos graficos (default = TRUE).

  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(titles[1]) || is.na(titles[1])) titles[1] = c("Scree-plot das correlacoes das cargas canonicas")
  if (!is.character(titles[2]) || is.na(titles[2])) titles[2] = c("Circulo de correlacoes")
  if (!is.character(titles[3]) || is.na(titles[3])) titles[3] = c("Grafico com os scores do grupo X")
  if (!is.character(titles[4]) || is.na(titles[4])) titles[4] = c("Grafico com os scores do grupo Y")
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.numeric(size) || size < 0)
     stop("Entrada para 'size' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.logical(grid))
     stop("Entrada para 'grid' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(color))
     stop("Entrada para 'color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
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
  
  if (is.na(xlabel[1])) xlabel = "Eixo X"
  
  if (is.na(ylabel[1])) ylabel = "Eixo Y"
  
  if (savptc) {
     message("\014") # limpa a tela
     message("\n\n Salvando graficos em disco. Aguarde o termino!")
  }
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos

  ##### INICIO - Scree-plot dos fatores #####
  if (savptc) png(filename = "Figure CCA Scree Plot.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  plot(1:length(CCA$var.UV[,1]), CCA$var.UV[,1], 
       type = "n", # nao plota pontos
       xlab = "Ordem dos pares canonicos", 
       ylab = "Variancias dos pares canonicos",
       xaxt = "n", # tira o eixo x
       main = titles[1])
  
  axis(1, c(1:length(CCA$var.UV[,1])), c(1:length(CCA$var.UV[,1])))
  
  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  } 
  
  points(1:length(CCA$var.UV[,1]), CCA$var.UV[,1], type = "b")
  
 if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Scree-plot dos fatores #####
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem Correlacoes entre as variaveis canonicas e as variaveis originais #####
  if (savptc) png(filename = "Figure CCA Correlations.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  plot(0,0, 
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = titles[2], # Titulo
       asp  = 1, # Aspecto do grafico
       type = "n", # nao plota pontos
       axes = F,
       xlim = c(-1.1,1.1), # Dimensao para as linhas do grafico
       ylim = c(-1.1,1.1)) # Dimensao para as colunas do grafico

  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = F)
    
  }
  
  symbols(0, 0, circles = 1, inches = FALSE, fg = 1, add = TRUE) # cria um circulo
  
  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  ## Grupo X
  arrows(0,0,CCA$corr.X[,1], CCA$corr.X[,2], lty = 2, code = 2, angle = 10, col = ifelse(color,"red","black")) # cria a seta apontando para cada ponto do grupo X
  LocLab(CCA$corr.X, rownames(CCA$corr.X), col = ifelse(color,"red","black"))  # Coloca os nomes dos pontos das coordenadas principais das linhas
 
  ## Grupo Y
  arrows(0,0,CCA$corr.Y[,1], CCA$corr.Y[,2], lty = 1, code = 2, angle = 10, col = ifelse(color,"blue","black")) # cria a seta apontando para cada ponto do grupo Y
  LocLab(CCA$corr.Y, rownames(CCA$corr.Y), col = ifelse(color,"Blue","black"))  # Coloca os nomes dos pontos das coordenadas principais das linhas
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Plotagem Correlacoes entre as variaveis canonicas e as variaveis originais #####

  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem dos scores dos grupos X e Y #####
  if (savptc) png(filename = "Figure CCA Scores X.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  plot(CCA$score.X, # grafico para os scores do grupo X
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       type = "n", # nao plota pontos
       main = titles[3], # Titulo
       # asp  = 2, # Aspecto do Grafico
       xlim = c(min(CCA$score.X[,1])-0.1,max(CCA$score.X[,1])+0.1), # Dimensao para as linhas do grafico
       ylim = c(min(CCA$score.X[,2]-0.1),max(CCA$score.X[,2])+0.1)) # Dimensao para as colunas do grafico

  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  points(CCA$score.X, # grafico para os scores do grupo X
         pch = 15,    # Formato dos pontos 
         cex = size,  # Tamanho dos pontos
         col = ifelse(color,"red","black")) # Cor dos pontos
    
  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  if (is.null(rownames(CCA$score.X)[1])) LineNames <- as.character(1:nrow(CCA$score.X))
  
  if (!is.null(rownames(CCA$score.X)[1])) LineNames <- rownames(CCA$score.X)
  
  LocLab(CCA$score.X, LineNames)  # Coloca os nomes dos pontos das coordenadas principais das linhas

 if (savptc) { box(col = 'white'); dev.off() }
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  if (savptc) png(filename = "Figure CCA Scores Y.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  plot(CCA$score.Y, # grafico para os scores do grupo Y
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       type = "n", # nao plota pontos
       main = titles[4], # Titulo
       # asp  = 2,  # Aspecto do Grafico
       xlim = c(min(CCA$score.Y[,1])-0.1,max(CCA$score.Y[,1])+0.1), # Dimensao para as linhas do grafico
       ylim = c(min(CCA$score.Y[,2]-0.1),max(CCA$score.Y[,2])+0.1)) # Dimensao para as colunas do grafico

  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  points(CCA$score.Y, # grafico para os scores do grupo Y
         pch = 15, # Formato dos pontos 
         cex = size,  # Tamanho dos pontos
         col = ifelse(color,"red","black")) # Cor dos pontos
  
  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  if (is.null(rownames(CCA$score.Y)[1])) LineNames <- as.character(1:nrow(CCA$score.Y))
    
  if (!is.null(rownames(CCA$score.Y)[1])) LineNames <- rownames(CCA$score.Y)
 
  LocLab(CCA$score.Y, LineNames)  # Coloca os nomes dos pontos das coordenadas principais das linhas
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Plotagem dos scores dos grupos X e Y #####
  
  if (savptc) message("\n \n Fim!")
  
}