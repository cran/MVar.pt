Plot.FA <- function(FA, titles = NA, xlabel = NA, ylabel = NA, size = 1.1,
                    grid = TRUE, color = TRUE, linlab = NA, axes = TRUE, class = NA, 
                    classcolor = NA, posleg = 2, boxleg = TRUE, savptc = FALSE,
                    width = 3236, height = 2000, res = 300, casc = TRUE) {

  # Rotina para Plotar Graficos do Metodo FA desenvolvida 
  # por Paulo Cesar Ossani em 02/2017
  
  # Entrada:
  # FA     - Dados da funcao FA
  # titles - Titulos para os graficos
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # size   - Tamanho dos pontos nos graficos.
  # grid   - Coloca grade nos graficos.
  # color  - Graficos coloridos (default = TRUE).
  # linlab - Vetor com os rotulos das observacoes.
  # axes   - Coloca eixos no grafico (default = TRUE).
  # class  - Vetor com os nomes das classes dos dados.
  # classcolor - Vetor com as cores das classes.
  # posleg  - 0 sem legenda,
  #           1 para legenda no canto superior esquerdo,
  #           2 para legenda no canto superior direito (default),
  #           3 para legenda no canto inferior direito,
  #           4 para legenda no canto inferior esquerdo. 
  # boxleg - Colocar moldura na legenda (default = TRUE). 
  # savptc - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width  - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res    - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).
  # casc   - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(titles[1]) || is.na(titles[1])) titles[1] = c("Scree-plot das variancias\n das cargas fatoriais")
  if (!is.character(titles[2]) || is.na(titles[2])) titles[2] = c("Escores das observacoes dos\n dois primeiros fatores")
  if (!is.character(titles[3]) || is.na(titles[3])) titles[3] = c("Cargas fatoriais")
  if (!is.character(titles[4]) || is.na(titles[4])) titles[4] = c("Biplot")
  
  if (!is.na(class[1])) {
    
    class <- as.matrix(class)
    
    if (nrow(FA$mtxscores) != length(class))
       stop("Entrada 'class' ou 'data' esta incorreta, devem conter o mesmo numero de linhas. Verifique!")
  }
  
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
  
  if (!is.na(linlab[1]) && length(linlab) != nrow(FA$mtxscores))
     stop("O numero elementos do rotulo para linhas 'linlab' difere do numero de linhas da base de dados. Verifique!")
  
  if (!is.numeric(posleg) || posleg < 0 || posleg > 4 || (floor(posleg)-posleg) != 0)
     stop("Entrada para posicao da legenda 'posleg' esta incorreta, deve ser um numero inteiro entre [0,4]. Verifique!")
  
  if (!is.logical(boxleg)) 
     stop("Entrada para moldura da legenda 'boxleg' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(axes)) 
     stop("Entrada para 'axes' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
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
     xlabel = paste("Primeiro fator (", round(FA$mtxvar[1,2],2),"%)",sep="")
  
  if (is.na(ylabel[1]))
     ylabel = paste("Segundo fator (", round(FA$mtxvar[2,2],2),"%)",sep="")
  
  if (posleg==1) posleg = "topleft"     # posicao das legendas nos graficos
  if (posleg==2) posleg = "topright"
  if (posleg==3) posleg = "bottomright"
  if (posleg==4) posleg = "bottomleft"
  
  boxleg = ifelse(boxleg,"o","n") # moldura nas legendas, "n" sem moldura, "o" com moldura
  
  num.class = 0
  if (!is.na(class[1])) {
    class.Table <- table(class)        # cria tabela com as quantidade dos elementos das classes
    class.Names <- names(class.Table)  # nomes das classses
    num.class   <- length(class.Table) # numero de classes
    NomeLinhas  <- as.matrix(class)
  } 
  
  if (num.class != 0 && length(classcolor) != num.class && !is.na(classcolor) ||
      num.class == 0 && length(classcolor) != 1 && !is.na(classcolor))
      stop("Entrada para 'classcolor' esta incorreta, deve ser em quantidade igual ao numero de classes em 'class'. Verifique!")
  
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (savptc) {
     cat("\014") # limpa a tela
     cat("\n\n Salvando graficos em disco. Aguarde o termino!")
  }
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos

  ##### INICIO - Plotagem dos Autovalores #####
  if (savptc) png(filename = "Figure FA Variances.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  mp <- barplot(FA$mtxvar[,1],names.arg=paste(round(FA$mtxvar[,2],2),"%",sep=""),
                main = "Variancias dos Fatores")
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Plotagem dos Autovalores #####
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Scree-plot dos Fatores #####
  if (savptc) png(filename = "Figure FA Scree Plot.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  plot(1:length(FA$mtxvar[,1]), FA$mtxvar[,1],
       type = "n", # nao plota pontos
       xlab = "Ordem dos Fatores", 
       ylab = "Variancia",
       xaxt = "n", # tira o eixo x
       main = titles[1])
  
  axis(1, c(1:length(FA$mtxvar[,1])), c(1:length(FA$mtxvar[,1])))
  
  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  points(1:length(FA$mtxvar[,1]), FA$mtxvar[,1], type = "b")
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Scree-plot dos Fatores #####

  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem Escores das observacoes #####
  if (savptc) png(filename = "Figure FA Observations.png", width = width, height = height, res = res) # salva os graficos em arquivo
  
  plot(FA$mtxscores,  # cria grafico para os Escores das observacoes 
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       type = "n", # nao plota pontos
       main = titles[2], # Titulo
       # asp  = 1,  # Aspecto do Grafico
       xlim = c(min(FA$mtxscores[,1])-0.05,max(FA$mtxscores[,1])+0.05), # Dimensao para as linhas do grafico
       ylim = c(min(FA$mtxscores[,2])-0.05,max(FA$mtxscores[,2])+0.05)) # Dimensao para as colunas do grafico

  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  if (num.class == 0) {
    
    points(FA$mtxscores,  # cria grafico para os Escores das observacoes 
           pch = 16, # Formato dos pontos
           cex = size,  # Tamanho dos pontos
           col = ifelse(color,"red","black")) # Cor dos pontos
    
  } else {
    
    if (!is.na(classcolor[1])) {
      cor.classe <- classcolor
    }
    else { cor.classe <- c("red") }
    
    newdata <- FA$mtxscores
    
    init.form <- 14 # formato inicial dos pontos
    
    cor <- 1 # cor inicial
    
    for (i in 1:num.class) {
      
      point.form <- init.form + i # fomato dos pontos de cada classe
      
      if (!is.na(classcolor[1])) {
        cor1 <- ifelse(color, cor.classe[i], "black")
      }
      else { cor1 <- ifelse(color, cor + i, "black") }
      
      point.data <- newdata[which(class == class.Names[i]),] 
      
      points(point.data,
             pch = point.form, # Formato dos pontos
             cex = size,  # Tamanho dos pontos
             col = cor1) # adiciona ao grafico as coordenadas principais das colunas
    }
    
    if (posleg != 0 && num.class > 0) {
      
      if (color) cor <- 2
      
      init.form <- 15
      
      cor <- ifelse(color, 2, 1)
      
      if (color) {
        if (!is.na(classcolor[1])) {
          color_b <- classcolor
        }
        else { color_b <- cor:(cor + num.class) }
      }
      else { color_b <- cor }
      
      legend(posleg, class.Names, pch = (init.form):(init.form + num.class), col = color_b,
             text.col = color_b, bty = boxleg, text.font = 6, y.intersp = 0.8, xpd = TRUE) # cria a legenda
    }
    
  }
  
  if (axes) abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  if (!is.na(linlab[1])) LocLab(FA$mtxscores, cex = 1, linlab)
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Plotagem Escores das observacoes #####
  
  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Cargas fatoriais #####
  if (savptc) png(filename = "Figure FA Loadings.png", width = width, height = height, res = res) # salva os graficos em arquivo
  
  HpMat <- rbind(c(0,0),FA$mtxcarga[,1:2])
  MaxX  <- max(HpMat[,1]) + 0.05 # Dimenssoes maximas das linhas
  MinX  <- min(HpMat[,1]) - 0.05 # Dimenssoes minimas das linhas
  MaxY  <- max(HpMat[,2]) + 0.05 # Dimenssoes maximas das colunas
  MinY  <- min(HpMat[,2]) - 0.05 # Dimenssoes minimas das colunas
  
  plot(0,0, # Plota as variaveis
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = titles[3], # Titulo
       # asp  = 1, # Aspecto do grafico
       type = "n", # nao plota pontos
       xlim = c(MinX,MaxX), # Dimensao para as linhas do grafico
       ylim = c(MinY,MaxY)) # Dimensao para as colunas do grafico

  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  if (axes) abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  arrows(0,0,FA$mtxcarga[,1],FA$mtxcarga[,2], lwd = 1, code = 2, length = 0.08, angle = 25, col = ifelse(color,"Red","Black")) # cria a seta apontando para cada variavel  
  
  NomeVar <- rownames(FA$mtxcarga) # nomes das variaveis
  LocLab(FA$mtxcarga[,1:2], NomeVar, col = ifelse(color,"Blue","Black"))  # Coloca os nomes das variaveis
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Cargas fatoriais #####

  if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Biplot ##### 
  if (savptc) png(filename = "Figure FA Biplot.png", width = width, height = height, res = res) # salva os graficos em arquivo
  
  HpMat <- rbind(c(0,0),FA$mtxcarga[,1:2],FA$mtxscores[,1:2])
  MaxX  <- max(HpMat[,1]) + 0.05 # Dimenssoes maximas das linhas
  MinX  <- min(HpMat[,1]) - 0.05 # Dimenssoes minimas das linhas
  MaxY  <- max(HpMat[,2]) + 0.05 # Dimenssoes maximas das colunas
  MinY  <- min(HpMat[,2]) - 0.05 # Dimenssoes minimas das colunas

  plot(0,0, # Plota as variaveis
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = titles[4], # Titulo
       # asp  = 1, # Aspecto do grafico
       type = "n", # nao plota pontos
       xlim = c(MinX,MaxX), # Dimensao para as linhas do grafico
       ylim = c(MinY,MaxY)) # Dimensao para as colunas do grafico

  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  if (num.class == 0) {
    
    points(FA$mtxscores,  # cria grafico para os Escores das observacoes 
           pch = 16, # Formato dos pontos
           cex = size,  # Tamanho dos pontos
           col = ifelse(color,"red","black")) # Cor dos pontos
    
  } else {
    
    if (!is.na(classcolor[1])) {
      cor.classe <- classcolor
    }
    else { cor.classe <- c("red") }
    
    newdata <- FA$mtxscores
    
    init.form <- 14 # formato inicial dos pontos
    
    cor <- 1 # cor inicial
    
    for (i in 1:num.class) {
      
      point.form <- init.form + i # fomato dos pontos de cada classe
      
      if (!is.na(classcolor[1])) {
        cor1 <- ifelse(color, cor.classe[i], "black")
      }
      else { cor1 <- ifelse(color, cor + i, "black") }
      
      point.data <- newdata[which(class == class.Names[i]),] 
      
      points(point.data,
             pch = point.form, # Formato dos pontos
             cex = size,  # Tamanho dos pontos
             col = cor1) # adiciona ao grafico as coordenadas principais das colunas
    }
    
    if (posleg != 0 && num.class > 0) {
      
      if (color) cor <- 2
      
      init.form <- 15
      
      cor <- ifelse(color, 2, 1)
      
      if (color) {
        if (!is.na(classcolor[1])) {
          color_b <- classcolor
        }
        else { color_b <- cor:(cor + num.class) }
      }
      else { color_b <- cor }
      
      legend(posleg, class.Names, pch = (init.form):(init.form + num.class), col = color_b,
             text.col = color_b, bty = boxleg, text.font = 6, y.intersp = 0.8, xpd = TRUE) # cria a legenda
    }
    
  }
  
  if (axes) abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  arrows(0,0,FA$mtxcarga[,1],FA$mtxcarga[,2], lwd = 1, code = 2, length = 0.08, angle = 25, col = ifelse(color,"Red","Black")) # cria a seta apontando para cada variavel  
  
  NomeVar <- rownames(FA$mtxcarga) # nomes das variaveis
  LocLab(FA$mtxcarga[,1:2], NomeVar, col = ifelse(color,"Blue","Black")) # Coloca os nomes das variaveis
  
  if (!is.na(linlab[1])) LocLab(FA$mtxscores, cex = 1, linlab)
  
  if (savptc) { box(col = 'white'); dev.off() }
  ##### FIM - Biplot #####
  
  if (savptc) cat("\n \n Fim!")
  
}