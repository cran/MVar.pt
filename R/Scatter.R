Scatter <- function(data, ellipse = TRUE, ellipse.level = 0.95, rectangle = FALSE,  
                title = NA, xlabel = NA, ylabel = NA,  posleg = 2, boxleg = TRUE, 
                axes = TRUE, size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
                class = NA, classcolor = NA, savptc = FALSE, width = 3236, 
                height = 2000, res = 300) {
  
  # Esta funcao executa a scatterplot
  # desenvolvida por Paulo Cesar Ossani em 12/2020
  
  # Entrada:
  # data    - Dados com as coordenadas x e y.
  # ellipse - Coloca uma elipse envolta das classes (default = TRUE). 
  # ellipse.level - Nivel de significancia da elipse (defaul = 0.95).
  # rectangle - Coloca retangulo para diferenciar as classes (default = FALSE).
  # title   - Titulo do grafico, se nulo retorna padrao.  
  # xlabel  - Nomeia o eixo X, se nulo retorna padrao.
  # ylabel  - Nomeia o eixo Y, se nulo retorna padrao. 
  # posleg  - 0 sem legenda,
  #          1 para legenda no canto superior esquerdo,
  #          2 para legenda no canto superior direito (default),
  #          3 para legenda no canto inferior direito,
  #          4 para legenda no canto inferior esquerdo.  
  # boxleg - Colocar moldura na legenda (default = TRUE).  
  # axes   - Coloca eixos no grafico (default = TRUE).
  # size   - Tamanho dos pontos no grafico (default size = 1.1).
  # grid   - Coloca grade nos graficos.
  # color  - Graficos coloridos (default = TRUE).
  # linlab - Vetor com o rotulo para as linhas, se nao
  #          informado retorna o padrao dos dados.  
  # class  - Vetor com os nomes das classes dos dados.
  # classcolor - Vetor com as cores das classes.
  # savptc - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width  - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res    - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).
  
  # Retorna:
  # Scatter plot.

  if (!is.na(class[1])) {
    
    class <- as.matrix(class)
    
    if (nrow(data) != length(class))
       stop("Entrada 'class' ou 'data' esta incorreta, devem conter o mesmo numero de linhas. Verifique!")
  }
  
  if (!is.logical(ellipse)) 
     stop("Entrada para 'ellipse' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(rectangle)) 
     stop("Entrada para 'rectangle' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(axes)) 
     stop("Entrada para 'axes' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.character(title) && !is.na(title[1]))
     stop("Entrada para 'title' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.numeric(posleg) || posleg < 0 || posleg > 4 || (floor(posleg)-posleg) != 0)
     stop("Entrada para posicao da legenda 'posleg' esta incorreta, deve ser um numero inteiro entre [0,4]. Verifique!")
  
  if (!is.logical(grid))
     stop("Entrada para 'grid' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.numeric(size) || size < 0)
     stop("Entrada para 'size' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.logical(color))
     stop("Entrada para 'color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(boxleg)) 
     stop("Entrada para moldura da legenda 'boxleg' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.na(linlab[1]) && length(linlab) != nrow(data))
     stop("O numero elementos do rotulo para linhas 'linlab' difere do numero de linhas da base de dados. Verifique!")
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")

  if (!is.na(linlab[1]) && !is.character(linlab))
     stop("Entrada para 'linlab' esta incorreta, deve ser do tipo caracter ou string. Verifique!")

  if (!is.numeric(ellipse.level) || ellipse.level <= 0 || ellipse.level >= 1)
     stop("Entrada para 'ellipse.level' esta incorreta, deve ser numerica entre (0,1). Verifique!")
  
  if (!is.logical(savptc))
     stop("Entrada para 'savptc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.numeric(width) || width <= 0)
     stop("Entrada para 'width' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.numeric(height) || height <= 0)
     stop("Entrada para 'height' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.numeric(res) || res <= 0)
     stop("Entrada para 'res' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  
  fellipse <- function(data, ellipse.level, color) { # funcao que retorna coordenadas da elipse
    # funcao desenvolvida em 08/12/2020
    dec <- eigen(var(data)) # decompoe a matriz de covariancia
    
    lambda <- (dec$values)  # autovalores

    nivel <- qchisq(ellipse.level, df = 2)

    ctd <- colMeans(data) # centroides
    
    a   <- sqrt(nivel * lambda[1]) # comprimento do maior eixo
    b   <- sqrt(nivel * lambda[2]) # comprimento do menor eixo
    
    phi <- atan(b/a) # angulo de inclinacao da elipse
    
    if (phi < 0) phi <- phi + 2 * pi
    
    t <- seq(0, 2 * pi, 0.001) # coordenadas
    
    ## coordenadas da elipse
    cdelp <- cbind(a * cos(t), b * sin(t))
 
    ## matriz de rotacao
    mr <- matrix(c(cos(phi), sin(phi), -sin(phi), cos(phi)), ncol = 2, nrow = 2)
    
    ## rotaciona dos dados
    rtc <- cdelp %*% t(mr)
    
    ## coordenadas da elipse baseadas no centroide
    crd <- cbind(rtc[,1] + ctd[1], rtc[,2] + ctd[2])

    return(crd) 
  }
  
  if (savptc) {
     message("\014") # limpa a tela
     message("\n\n Salvando graficos em disco. Aguarde o termino!")
  }
  
  if (is.na(title[1]))  title = "Grafico de dispersao" # Titulo
  
  if (is.na(xlabel[1])) xlabel = "Eixo X" 
  
  if (is.na(ylabel[1])) ylabel = "Eixo Y"

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
  
  ##### FIM - Informacoes usadas nos Graficos #####

  if (savptc) png(filename = "Figure_Scatter_Plot.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  x <- data[,1] # valores eixo x
  y <- data[,2] # valores eixo y
  
  a <- 0.1
  if (ellipse && num.class > 0) { # necessario para ajustar coordenadas do grafico caso tenha elipse
      newdata <- cbind(x,y)
      cord <- NULL
      for (i in 1:num.class) {
        point.data <- newdata[which(class == class.Names[i]),]
        elip <- fellipse(data = point.data, ellipse.level = ellipse.level)
        cord <- rbind(cord, cbind(min(elip[,1]), min(elip[,2]))) # valores minimos
        cord <- rbind(cord, cbind(max(elip[,1]), max(elip[,2]))) # valores maximos
      }
      axes.x <- c(min(c(x, cord[,1])) - a, max(c(x, cord[,1])) + a)
      axes.y <- c(min(c(y, cord[,2])) - a, max(c(y, cord[,2])) + a)
  } else {
      axes.x <- c(min(x) - a, max(x) + a)
      axes.y <- c(min(y) - a, max(y) + a)
  }

  if (!is.na(classcolor[1])) {
     cor.classe <- classcolor
  }
  else { cor.classe <- c("blue") }
  
  plot(0,0, # cria grafico para as coordenadas linhas x e colunas y
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = title,  # Titulo
       asp  = 0,      # Aspecto do Grafico
       type = "n",    # nao plota pontos
       xlim = axes.x, # Dimensao para as linhas do grafico
       ylim = axes.y) # Dimensao para as colunas do grafico 
  
  if (grid) {
    
    args <- append(as.list(par('usr')), c('gray93','gray93'))
    
    names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
    do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
    grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  if (num.class == 0) {
    
     points(x, y, # cria grafico para as coordenadas principais das linhas
            pch = 16,   # Formato dos pontos 
            cex = size, # Tamanho dos pontos  
            col = ifelse(color, cor.classe, "Black"))
    
  } else {
    
     newdata <- cbind(x,y)
     
     init.form <- 14 # formato inicial dos pontos
  
     cor <- 1 # cor inicial
     
     for (i in 1:num.class) {
       
       point.form <- init.form + i # fomato dos pontos de cada classe
       
       if (!is.na(classcolor[1])) {
          cor1 <- ifelse(color, cor.classe[i], "black")
       }
       else { cor1 <- ifelse(color, cor + i, "black") }
  
       point.data <- newdata[which(class == class.Names[i]),] 

       if (ellipse && num.class > 0) { # desenha a elipse das classes
          elip <- fellipse(data = point.data, ellipse.level = ellipse.level)
          lines(elip, col = cor1)
       } 
      
       if (rectangle && num.class > 0) { # desenha o retangunlo das classes
          retan <- chull(point.data) 
          retan <- c(retan, retan[1])
          lines(point.data[retan,], col = cor1)
       }
      
       points(point.data,
              pch = point.form, # Formato dos pontos
              cex = size,  # Tamanho dos pontos
              col = cor1) # adiciona ao grafico as coordenadas principais das colunas
     }
    
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
  
  if (axes) abline(h = 0, v = 0, cex = 1.5, lty = 2)
  
  if (!is.na(linlab[1])) LocLab(x, y, cex = 1, linlab)

  if (savptc) {
     box(col = 'white')
     dev.off()
     message("\n \n Fim!")
  }
  
}
