Plot.PP <- function(PP, titles = NA, xlabel = NA, ylabel = NA, posleg = 2, 
                    boxleg = TRUE, size = 1.1, grid = TRUE, color = TRUE, 
                    classcolor = NA, linlab = NA, axesvar = TRUE, axes = TRUE, 
                    casc = TRUE) {
  
  # Rotina para plotar graficos da Projecao Pursuit desenvolvida 
  # por Paulo Cesar Ossani em 2017/02/27
  
  # Entrada:
  # PP       - Dados da funcao Optimizer.
  # titles   - Titulos para os graficos. Se nao for definido assume texto padrao.
  # xlabel   - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel   - Nomeia o eixo Y, se nao definido retorna padrao.
  # posleg   - 0 sem legenda,
  #            1 para legenda no canto superior esquerdo,
  #            2 para legenda no canto superior direito (default),
  #            3 para legenda no canto inferior direito,
  #            4 para legenda no canto inferior esquerdo.
  # boxleg   - Colocar moldura na legenda (default = TRUE).
  # size     - Tamanho dos pontos nos graficos.
  # grid     - Coloca grade nos graficos.
  # color    - Graficos coloridos (default = TRUE).
  # classcolor - Vetor com as cores das classes.
  # linlab   - Vetor com os rotulos das observacoes.
  # axesvar  - Coloca eixos de rotacao das variaveis, somente quando DimProj > 1 (default = TRUE).
  # axes     - Plot os eixos X e Y (default = TRUE).
  # casc     - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Grafico da evolucao dos indices, e graficos cujos dados 
  # foram reduzidos em duas dimensoes.
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.numeric(posleg) || posleg < 0 || posleg > 4 || (floor(posleg)-posleg) != 0)
     stop("Entrada para posicao da legenda 'posleg' esta incorreta, deve ser um numero inteiro entre [0,4]. Verifique!")
  
  if (!is.numeric(size) || size < 0)
     stop("Entrada para 'size' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.logical(grid))
     stop("Entrada para 'grid' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(color))
     stop("Entrada para 'color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(boxleg)) 
     stop("Entrada para moldura da legenda 'boxleg' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(axesvar))
     stop("Entrada para 'axesvar' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(axes)) 
     stop("Entrada para 'axes' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.na(linlab[1]) && length(linlab) != nrow(PP$proj.data)) 
      stop("Entrada para 'linlab' esta incorreta, deve ter o mesmo numero de linhas que os dados de entrada em 'Data'. Verifique!")

  if (is.na(PP$findex[1])) PP$findex <- "Not Available"
  
  if (!is.logical(casc))
     stop("Entrada para 'casc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  
  if (is.na(xlabel[1]))
     xlabel = "Eixo X" 

  if (is.na(ylabel[1]))
     ylabel = "Eixo Y"

  if (posleg==1) posleg = "topleft" # posicao das legendas nos graficos
  if (posleg==2) posleg = "topright"
  if (posleg==3) posleg = "bottomright"
  if (posleg==4) posleg = "bottomleft"
  
  boxleg = ifelse(boxleg,"o","n") # moldura nas legendas, "n" sem moldura, "o" com moldura

  if (!is.na(PP$num.class[1])) {
     Data <- as.matrix(PP$proj.data[,1:(ncol(PP$proj.data)-1)])
  } else Data <- PP$proj.data
  
  num.class = ifelse(is.na(PP$num.class), 0, PP$num.class)
  
  class.names <- PP$class.names # nomes das classses
  
  if (num.class != 0 && length(classcolor) != num.class && !is.na(classcolor) ||
      num.class == 0 && length(classcolor) != 1 && !is.na(classcolor))
     stop("Entrada para 'classcolor' esta incorreta, deve ser em quantidade igual ao numero de classes em 'class'. Verifique!")
  
  if (num.class == 0) {
     Data <- PP$proj.data
     NomeLinhas = rownames(PP$proj.data)
  } else {
     Data <- as.matrix(PP$proj.data[,1:(ncol(PP$proj.data)-1)])
     NomeLinhas <- as.matrix(PP$proj.data[,ncol(PP$proj.data)])
  }

  cor <- 1 # cor inicial dos pontos e legendas
  ##### FIM - Informacoes usadas nos Graficos #####
  
  if (!is.character(titles[1]) || is.na(titles[1])) titles[1] = c("Evolucao do indice")
  if (!is.character(titles[2]) || is.na(titles[2])) titles[2] = paste("Funcao indice:", PP$findex)

  #### INICIO - Plota os indices das projecoes ####
  
  linCol <- c('blue') # cor da funcao plotada
  
  Cood.xy = round(PP$index,4)
  
  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  plot(Cood.xy,
       xlab = "Simulacao",
       ylab = "Valor do Indice",
       main = titles[1], # Titulo
       type = "n", # tipo de grafico  
       bty  = "l", # tipo de caixa do grafico
       cex.axis = 1, # tamanho do 'tick' dos eixos
       cex.lab  = 1) # tamanho dos nomes dos eixos
  
  if (grid) {

     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
     
  }
  
  lines(Cood.xy, col = linCol)
  #### FIM - Plota os indices das projecoes ####


  #### Plotas as projecoes 2D
  if (ncol(Data) == 2) {
    
     maxX = max(Data[,1], PP$vector.opt[,1]) 
     minX = min(Data[,1], PP$vector.opt[,1]) 
     maxY = max(Data[,2], PP$vector.opt[,2])
     minY = min(Data[,2], PP$vector.opt[,2])
    
     if (casc) dev.new() # efeito cascata na apresentacao dos graficos
     
     if (!is.na(classcolor[1])) {
        cor.classe <- classcolor
     }
     else { cor.classe <- c("red") }
     
     if (num.class == 0) {
       
        if (color && !is.na(classcolor[1])) {
          cor1 <- classcolor
        }
        else { cor1 <- ifelse(color, "blue", "Black") }
       
        plot(Data[,1:2], # coordenadas do grafico
             xlab = xlabel, # Nomeia Eixo X
             ylab = ylabel, # Nomeia Eixo Y
             main = titles[2], # Titulo para o grafico
             type = "n", # tipo de grafico
             axes = F,   # elimina os eixos
             xlim = c(minX,maxX), # dimensao eixo X
             ylim = c(minY,maxY)) # dimensao eixo Y

       if (grid) {
         
          args <- append(as.list(par('usr')), c('gray93','gray93'))
         
          names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
         
          do.call(rect, args) # chama a funcao rect com os argumentos (args)
         
          grid(col = "white", lwd = 2, lty = 7, equilogs = T)
         
       }
       
       points(Data[,1:2], # coordenadas do grafico
              pch  = 16,  # formato dos pontos
              cex  = size,  # Tamanho dos pontos
              col  = cor1)
       
     } else {
       
       plot(0,0, # cria grafico para as coordenadas principais das linhas
            xlab = xlabel, # Nomeia Eixo X
            ylab = ylabel, # Nomeia Eixo Y
            main = titles[2], # Titulo
            type = "n", # nao plota pontos
            xlim = c(minX, maxX), # Dimensao para as linhas do grafico
            ylim = c(minY, maxY)) # Dimensao para as colunas do grafico
 
       if (grid) {
         
          args <- append(as.list(par('usr')), c('gray93','gray93'))
         
          names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
         
          do.call(rect, args) # chama a funcao rect com os argumentos (args)
         
          grid(col = "white", lwd = 2, lty = 7, equilogs = T)
         
       }
       
       Init.Form <- 14 # formato inicial dos pontos
       
       for (i in 1:num.class) {
         
         Point.Form <- Init.Form + i # fomato dos pontos de cada classe

         if (!is.na(classcolor[1])) {
           cor1 <- ifelse(color, cor.classe[i], "black")
         }
         else { cor1 <- ifelse(color, cor + i, "black") }
         
         Point.Data <- Data[which(PP$proj.data[,ncol(PP$proj.data)] == class.names[i]),]

         points(Point.Data,
                pch = Point.Form, # Formato dos pontos
                cex = size,  # Tamanho dos pontos  
                col = cor1) # adiciona ao grafico as coordenadas principais das colunas
       }
       
       if (color) cor <- 2
       
       Init.Form <- 15

     }
     
  }
  
  
  #### Plotas as projecoes 1D
  if (ncol(Data) == 1) {  
    
     if (casc) dev.new() # efeito cascata na apresentacao dos graficos

     if (num.class == 0) {
               
        if (color && !is.na(classcolor[1])) {
          cor1 <- classcolor
        }
        else { cor1 <- ifelse(color, "blue", "Black") }
        
        
        plot(Data, # coordenadas do grafico
             xlab = xlabel, # Nomeia Eixo X
             ylab = ylabel, # Nomeia Eixo Y
             type = "n",    # tipo de grafico
             main = titles[2], # Titulo
             axes = F)      # Elimina os eixos
             
        if (grid) {
          
           args <- append(as.list(par('usr')), c('gray93','gray93'))
          
           names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
          
           do.call(rect, args) # chama a funcao rect com os argumentos (args)
          
           grid(col = "white", lwd = 2, lty = 7, equilogs = T)
          
        }
        
        lines(Data, # coordenadas do grafico
              type = "o",
              pch  = 16,   # formato dos pontos
              cex  = size, # Tamanho dos pontos  
              col  = cor1)
       
     } else {
       
        minX = 5
        maxX = length(Data[,1]) + minX
        maxY = max(Data[, 1]) 
        minY = min(Data[, 1]) 
        
        Init.Form <- 15 # formato inicial dos pontos
        
        if (color && !is.na(classcolor[1])) {
           cor1 <- c(classcolor)[as.factor(NomeLinhas)]
        }
        else { 
           if (color) { cor1 <- c(cor:(cor + num.class))[as.factor(NomeLinhas)]
          } else { cor1 <- c("black") }
        }

        Point.Data <- cbind((1:nrow(Data)) + minX, Data)   

        plot(Point.Data, # cria grafico para as coordenadas principais das linhas
             xlab = xlabel, # Nomeia Eixo X
             ylab = ylabel, # Nomeia Eixo Y
             type = "n",    # tipo de grafico
             main = titles[2], # Titulo
             axes = F,      # Elimina os eixos  
             xlim = c(minX, maxX), # Dimensao para as linhas do grafico
             ylim = c(minY, maxY)) # Dimensao para as colunas do grafico

        if (grid) {
          
           args <- append(as.list(par('usr')), c('gray93','gray93'))
          
           names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
          
           do.call(rect, args) # chama a funcao rect com os argumentos (args)
          
           grid(col = "white", lwd = 2, lty = 7, equilogs = T)
          
        }
        
        lines(Point.Data,  # cria grafico para as coordenadas principais das linhas
              type = "o",  # tipo de grafico
              cex  = size, # Tamanho dos pontos
              pch  = c((Init.Form):(Init.Form + num.class))[as.factor(NomeLinhas)], # Formato dos pontos
              col  = cor1)
        
     }

  }
  
  if (ncol(Data) <= 2) {
    
    if (posleg != 0 && num.class > 0) {

       if (color && !is.na(classcolor[1])) {
         color_b <- classcolor
       }
       else { 
         if (color) { color_b <- cor:(cor + num.class)
         } else { color_b <- c("black") }
       }
        
       legend(posleg, class.names, pch = (Init.Form):(Init.Form + num.class), col = color_b,
              text.col = color_b, bty = boxleg, text.font = 6, y.intersp = 0.8, xpd = TRUE) # cria a legenda
    }
      
    if (color && !is.na(classcolor[1])) {
       cor1 <- c(classcolor)[as.factor(NomeLinhas)]
    }
    else { 
      if (color) { cor1 <- c(cor:(cor + num.class))[as.factor(NomeLinhas)]
      } else { cor1 <- c("black") }
    }
      
    if (!is.na(linlab[1])) LocLab(Data, cex = 1, linlab, col = c(cor1))

    if (axes) abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central 
   
    if (axesvar && ncol(Data) == 2 ) { # plota os eixos das variaveis
      
       Ajuste <- c(diff(range(Data[,1])) / 2 + min(Data[,1]),
                   diff(range(Data[,2])) / 2 + min(Data[,2]))
      
       PosVar <- cbind(PP$vector.opt[,1] + Ajuste[1], PP$vector.opt[,2] + Ajuste[2]) # Posicao para as variaveis no grafico
      
       arrows(Ajuste[1], Ajuste[2], PosVar[,1], PosVar[,2],
              lty = 1, code = 2, length = 0.08, angle = 25,
              col = ifelse(color, "Red", "Black"))
      
       LocLab(PosVar, cex = 1, rownames(PP$vector.opt), xpd = TRUE)
      
    }
    
  }
  
}
