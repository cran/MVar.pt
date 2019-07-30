GrandTour <- function(data, method = "Interpolation", title = NA, xlabel = NA, ylabel = NA,
                      size = 1.1, grid = TRUE, color = TRUE, linlab = NA, class = NA, 
                      posleg = 2, boxleg = TRUE, axesvar = TRUE, axes = FALSE, numrot = 200, 
                      choicerot = NA, savepicture = FALSE) {
  
  # Esta funcao executa a rotacao dos dados multivariados em baixa dimensao
  # basea-se nos artigos de: Asimov, D. . The Grand Tour: A Tool for Viewing
  # Multidimensional data, SIAM Journal of Scientific and Statistical Computing
  # 6(1), 128-143, 1985., Asimov, D. and A. Buja. 1994. The grand tour via 
  # geodesic interpolation of 2-frames,. in Visual data Exploration and Analysis,
  # Symposium on Electronic Imaging Science and Technology, IS&T/SPIE, E Wegman, E.
  # J. and J. Shen. 1993. Three-dimensional Andrews plots and the grand
  # tour, Proceedings of the 25th Symposium on the Interface, 284-288.
  # by Paulo Cesar Ossani in 2017/03/31

  # Entrada:
  # data   - Conjunto de dados numericos.
  # method - Metodo usado para as rotacoes:
  #          "Interpolation" - Metodo Interpolation (default),
  #          "Torus" - Metodo Torus,
  #          "Pseudo" - Metodo Pseudo Grand Tour.
  # title   - Titulo para os graficos, se omitido retorna default.
  # xlabel  - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel  - Nomeia o eixo Y, se nao definido retorna padrao.
  # size    - Tamanho dos pontos nos graficos.
  # grid    - Coloca grade nos graficos.
  # color   - Graficos coloridos (default = TRUE).
  # linlab  - Vetor com os rotulos para as observacoes.
  # class   - Vetor com os nomes das classes dos dados.
  # posleg  - 0 sem legenda,
  #           1 para legenda no canto superior esquerdo,
  #           2 para legenda no canto superior direito (default),
  #           3 para legenda no canto inferior direito,
  #           4 para legenda no canto inferior esquerdo.  
  # boxleg  - Colocar moldura na legenda (default = TRUE).
  # axesvar - Coloca eixos de rotacao das variaveis (default = TRUE).
  # axes    - Plota os eixos X e Y (default = FALSE).
  # numrot  - Numero de rotacoes (default = 200).
  #           Se method = "Interpolation", numrot representara o angulo de rotacao.
  # choicerot - Escolhe rotacao especifica e apresenta na tela,
  #             ou salva a imagem se savepicture = TRUE.
  # savepicture - Salva as imagens dos graficos em arquivos (default = FALSE).

  # Retorna:
  # Graficos com as rotacoes.
  # proj.data  - Dados projetados.
  # vector.opt - Vetor projecao.
  # method     - Metodo usado no Grand Tour.

  if (!is.data.frame(data))
     stop("Entrada 'data' esta incorreta, deve ser do tipo dataframe. Verifique!")
  
  if (!is.na(class[1])) {
    
    class <- as.matrix(class)
    
    if (nrow(data) != length(class))
       stop("Entrada 'class' ou 'data' esta incorreta, devem conter o mesmo numero de linhas. Verifique!")
  }
  
  method <- toupper(method) # transforma em maiusculo

  if (!(method %in% c("TORUS", "INTERPOLATION", "PSEUDO")))
     stop("Entrada para 'method' esta incorreta, deve ser: 'Interpolation', 'Torus' or 'Pseudo'. Verifique!")
  
  if (!is.character(title) && !is.na(title[1]))
     stop("Entrada para 'title' esta incorreta, deve ser do tipo caracter ou string. Verifique!")

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

  if (!is.na(linlab[1]) && length(linlab)!=nrow(data))
     stop("Entrada para 'linlab' esta incorreta, deve ter o mesmo numero de linhas que os dados de entrada em 'data'. Verifique!")

  if (!is.logical(axesvar))
     stop("Entrada para 'axesvar' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(axes)) 
     stop("Entrada para 'axes' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.numeric(numrot) || numrot < 1)
     stop("'numrot' deve ser um numero inteiro maior que zero. Verifique!")
      
  if (!is.na(choicerot[1])) {
     if (!is.numeric(choicerot) ||  choicerot < 1)
        stop("'choicerot' deve ser um numero inteiro maior que zero. Verifique!")
    
     if(numrot < choicerot)
       stop("numrot < choicerot, numrot tem que ser maior ou igual choicerot. Verifique!")
  }

  if (!is.logical(savepicture))
     stop("Entrada para 'savepicture' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (is.na(xlabel[1]))
     xlabel = "Eixo X" 
  
  if (is.na(ylabel[1]))
     ylabel = "Eixo Y"

  if (posleg==1) posleg = "topleft"   # posicao das legendas nos graficos
  if (posleg==2) posleg = "topright"
  if (posleg==3) posleg = "bottomright"
  if (posleg==4) posleg = "bottomleft"
  
  boxleg = ifelse(boxleg,"o","n") # moldura nas legendas, "n" sem moldura, "o" com moldura
  
  d <- 2 # dimensao de projecao

  Num.class = 0
  if (!is.na(class[1])) {
     class.Table <- table(class)       # cria tabela com as quantidade dos elementos das classes
     class.Names <- names(class.Table)  # nomes das classses
     Num.class   <- length(class.Table) # numero de classes
     NomeLinhas  <- as.matrix(class)
  } 
  
  if (method == "TORUS") {
    
    # O codigo seguinte foi transladado do codigo em Matlab encontrado no Livro de
    # Wendy L. Martinez, Angel Martinez, Jeffrey Solka-Exploratory data Analysis
    # with MATLAB, Second Edition-CRC Press (2010) paginas 128-129 Example 4.1
    
    n <- nrow(data)
    
    p <- ncol(data)
    
    N <- 2 * p - 3 # configurar o vetor de frequencias
    
    DivRest <- exp(1:N) %% 1 # encontra o resto da divisao
    
    NumIrr = exp(-5) # pequeno numero irracional
    
    # indice de rotacao
    J <- 2:p;
    I <- rep(1,length(J))
    I <- c(I, 2*rep(1,length(J)-1))
    J <- c(J, 3:p)
    
    BasicVector <- diag(1, p, d) # vetor basico para a rotacao
    
    numrot <- ifelse(is.na(choicerot[1]), numrot, choicerot)
    
    if (savepicture) {
       cat("\014") # limpa a tela
       cat("\n\n Salvando graficos em disco. Aguarde o termino!")
    }
    
    i <- 1
    while (i <= numrot) { # Inicializa o Grand Tour
      
      cor <- 1 # cor inicial dos pontos e legendas
      
      IndVector <- diag(1, p) # vetor identidade
      
      for (j in 1:N) { # encontra a matriz de rotacao
        
        MP <- diag(1, p)
        MP[I[j],J[j]] <-  cos(DivRest[j] * i * NumIrr)
        MP[J[j],I[j]] <-  cos(DivRest[j] * i * NumIrr)
        MP[I[j],J[j]] <- -sin(DivRest[j] * i * NumIrr)
        MP[J[j],I[j]] <-  sin(DivRest[j] * i * NumIrr)
        
        IndVector <- IndVector %*% MP
        
      }
      
      A <- IndVector %*% BasicVector # rotaciona o vetor base
      
      proj.data <- as.matrix(data) %*% A # projecao em direcao a nova base
      
      if (is.na(choicerot[1]) || choicerot == i) {
        
        if (savepicture) png(filename = paste("Picture ", i," - Metodo", method,".png",step="")) # salva os graficos em arquivos
        
        maxX = max(proj.data[, 1], A[,1])
        minX = min(proj.data[, 1], A[,1])
        maxY = max(proj.data[, 2], A[,2])
        minY = min(proj.data[, 2], A[,2])
        
        Tit <- ifelse(!is.character(title) || is.na(title[1]), paste("Rotacao:", i), title)
        
        if (Num.class == 0) {
          
           plot(proj.data, # coordenadas do grafico
                xlab = xlabel, # Nomeia Eixo X
                ylab = ylabel, # Nomeia Eixo Y
                type = "n", # nao plota pontos
                main = Tit, # Titulo para o grafico
                asp  = 1,   # Aspecto do Grafico
                # axes = F,   # elimina os eixos
                xlim = c(minX,maxX), # dimensao eixo X
                ylim = c(minY,maxY)) # dimensao eixo Y

           if (grid) {
            
              args <- append(as.list(par('usr')), c('gray95','gray95'))
            
              names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
            
              do.call(rect, args) # chama a funcao rect com os argumentos (args)
            
              grid(col = "white", lwd = 1, lty = 7, equilogs = T)
            
           }
          
           points(proj.data, # coordenadas do grafico
                  pch  = 16,  # formato dos pontos
                  col = ifelse(color, "Blue", "Black"))
          
        } else {
          
           plot(0,0, # cria grafico para as coordenadas linhas x e colunas y
                xlab = xlabel, # Nomeia Eixo X
                ylab = ylabel, # Nomeia Eixo Y
                main = Tit, # Titulo
                asp  = 1,   # Aspecto do Grafico
                type = "n", # nao plota pontos
                xlim = c(minX,maxX), # dimensao eixo X
                ylim = c(minY,maxY), # dimensao eixo Y
                col  = ifelse(color,"red","black"))  # Cor dos pontos

           if (grid) {
            
              args <- append(as.list(par('usr')), c('gray95','gray95'))
            
              names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
            
              do.call(rect, args) # chama a funcao rect com os argumentos (args)
            
              grid(col = "white", lwd = 1, lty = 7, equilogs = T)
            
           } 
          
           Init.Form <- 14 # formato inicial dos pontos
           
           for (k in 1:Num.class) {
             
             Point.Form <- Init.Form + k # fomato dos pontos de cada classe
            
             cor1 <- ifelse(color, cor + k, "black")
             
             Point.data <- proj.data[which(class == class.Names[k]),]
            
             points(Point.data,
                    pch = Point.Form, # Formato dos pontos
                    cex = size,  # Tamanho dos pontos  
                    col = cor1) # adiciona ao grafico as coordenadas principais das colunas
           }
          
        }  
        
        if (!is.na(linlab[1])) LocLab(proj.data, cex = 1, linlab)
        
        if (axes) # coloca os eixos
           abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
        
        if (axesvar) { # plota os eixos das variaveis
          
           Ajuste <- c(diff(range(proj.data[,1])) / 2 + min(proj.data[,1]),
                       diff(range(proj.data[,2])) / 2 + min(proj.data[,2]))
          
           PosVar <- cbind(A[,1] + Ajuste[1], A[,2] + Ajuste[2]) # Posicao para as variaveis no grafico
           
           arrows(Ajuste[1], Ajuste[2], PosVar[,1], PosVar[,2],
                  lty = 1, code = 2, length = 0.08, angle = 25,
                  col = ifelse(color, "Red", "Black"))
           
           LocLab(PosVar, cex = 1, colnames(data), xpd = TRUE)
          
        }
        
        if (posleg != 0 && Num.class > 0) {
          
           if (color) cor <- 2
          
           Init.Form <- 15 # codigo formato ponto inicial
          
           color_b <- cor # colore as letras das legendas e suas representacoes no grafico
          
           if (color) color_b = cor:(cor + Num.class)
          
           legend(posleg, class.Names, pch = (Init.Form):(Init.Form + Num.class), col = color_b,
                  text.col = color_b, bty = boxleg, text.font = 6, y.intersp = 0.8, xpd = TRUE) # cria a legenda
        }
        
        if (savepicture) dev.off()
        
        Sys.sleep(0.05) # tempo entre as plotagens dos graficos
        
      }
      
      i <- i + 1
      
    }
    
    if (savepicture) cat("\n \n Fim!")
    
  }
  
  
  if (method == "PSEUDO") {
    
    # O codigo seguinte foi transladado do codigo em Matlab encontrado no Livro de
    # Wendy L. Martinez, Angel Martinez, Jeffrey Solka-Exploratory data Analysis
    # with MATLAB, Second Edition-CRC Press (2010) paginas 131-132 Example 4.2
    
    n <- nrow(data)
    
    p <- ncol(data)

    DivRest <- exp(1:p) %% 1 # encontra o resto da divisao
    
    NumIrr = exp(-5)   # pequeno numero irracional
    
    coefic = sqrt(2/p) # pequeno numero irracional
    
    a <- matrix(0, p, 1) # matriz de rotacao
    b <- matrix(0, p, 1) # matriz de rotacao
 
    if (savepicture) {
       cat("\014") # limpa a tela
       cat("\n\n Salvando graficos em disco. Aguarde o termino!")
    }
    
    numrot <- ifelse(is.na(choicerot[1]), numrot, choicerot)
    
    t <- (1:numrot) * NumIrr
    
    i <- 1
    while (i <= length(t)) { # Inicializa o Grand Tour

      cor <- 1 # cor inicial dos pontos e legendas
      
      for(j in 1:(p/2)) { # encontra a matriz de rotacao
        a[2*(j-1)+1] = coefic * sin(DivRest[j] * t[i]);
        a[2*j]       = coefic * cos(DivRest[j] * t[i]);
        b[2*(j-1)+1] = coefic * cos(DivRest[j] * t[i]);
        b[2*j]       = coefic * (-sin(DivRest[j] * t[i]));
      }
      
      A <- cbind(a, b) # vetor projecao
      
      proj.data <- as.matrix(data) %*% A; # projecao
 
      if (is.na(choicerot[1]) || choicerot == i) {
        
        if (savepicture) png(filename = paste("Picture ", i," - Metodo", method,".png",step="")) # salva os graficos em arquivos
        
        maxX = max(proj.data[, 1], A[,1])
        minX = min(proj.data[, 1], A[,1])
        maxY = max(proj.data[, 2], A[,2])
        minY = min(proj.data[, 2], A[,2])
        
        Tit <- ifelse(!is.character(title) || is.na(title[1]), paste("Rotacao:", i), title)
        
        if (Num.class == 0) {

           plot(proj.data, # coordenadas do grafico
                xlab = xlabel, # Nomeia Eixo X
                ylab = ylabel, # Nomeia Eixo Y
                type = "n", # nao plota pontos
                main = Tit, # Titulo para o grafico
                asp  = 1,   # Aspecto do Grafico
                # axes = F,   # elimina os eixos
                xlim = c(minX,maxX), # dimensao eixo X
                ylim = c(minY,maxY)) # dimensao eixo Y
          
           if (grid) {
            
              args <- append(as.list(par('usr')), c('gray95','gray95'))
            
              names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
            
              do.call(rect, args) # chama a funcao rect com os argumentos (args)
            
              grid(col = "white", lwd = 1, lty = 7, equilogs = T)
            
           }
          
           points(proj.data, # coordenadas do grafico
                  pch  = 16,  # formato dos pontos
                  col = ifelse(color, "Blue", "Black"))
          
        } else {
          
           plot(0,0, # cria grafico para as coordenadas linhas x e colunas y
                xlab = xlabel, # Nomeia Eixo X
                ylab = ylabel, # Nomeia Eixo Y
                main = Tit,  # Titulo
                asp  = 1,  # Aspecto do Grafico
                type = "n", # nao plota pontos
                xlim = c(minX,maxX), # dimensao eixo X
                ylim = c(minY,maxY), # dimensao eixo Y
                col  = ifelse(color,"red","black"))  # Cor dos pontos
          
           if (grid) {
            
              args <- append(as.list(par('usr')), c('gray95','gray95'))
            
              names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
            
              do.call(rect, args) # chama a funcao rect com os argumentos (args)
            
              grid(col = "white", lwd = 1, lty = 7, equilogs = T)
            
           } 
          
           Init.Form <- 14 # formato inicial dos pontos
           
           for (k in 1:Num.class) {
             
             Point.Form <- Init.Form + k # fomato dos pontos de cada classe
            
             cor1 <- ifelse(color, cor + k, "black")
            
             Point.data <- proj.data[which(class == class.Names[k]),]
            
             points(Point.data,
                    pch = Point.Form, # Formato dos pontos
                    cex = size, # Tamanho dos pontos  
                    col = cor1) # adiciona ao grafico as coordenadas principais das colunas
           }
          
        }

        if (!is.na(linlab[1])) LocLab(proj.data, cex = 1, linlab)
        
        if (axes) # coloca os eixos
           abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
        
        if (axesvar) { # plota os eixos das variaveis
          
           Ajuste <- c(diff(range(proj.data[,1])) / 2 + min(proj.data[,1]),
                       diff(range(proj.data[,2])) / 2 + min(proj.data[,2]))
          
           PosVar <- cbind(A[,1] + Ajuste[1], A[,2] + Ajuste[2]) # Posicao para as variaveis no grafico
          
           arrows(Ajuste[1], Ajuste[2], PosVar[,1], PosVar[,2],
                  lty = 1, code = 2, length = 0.08, angle = 25,
                  col = ifelse(color, "Red", "Black"))
           
           LocLab(PosVar, cex = 1, colnames(data), xpd = TRUE)
          
        }
        
        if (posleg != 0 && Num.class > 0) {
          
           if (color) cor <- 2
          
           Init.Form <- 15 # codigo formato ponto inicial
          
           color_b <- cor # colore as letras das legendas e suas representacoes no grafico
          
           if (color) color_b = cor:(cor + Num.class)
          
           legend(posleg, class.Names, pch = (Init.Form):(Init.Form + Num.class), col = color_b,
                  text.col = color_b, bty = boxleg, text.font = 6, y.intersp = 0.8, xpd = TRUE) # cria a legenda
        }

        if (savepicture) dev.off()
        
        Sys.sleep(0.05) # tempo entre as plotagens dos graficos
        
      }
      
      i <- i + 1
      
    }
    
    if (savepicture) cat("\n \n Fim!")
    
  }  
  
  
  if (method == "INTERPOLATION") {
    
    # O codigo seguinte foi transladado do codigo em Matlab encontrado no Livro de 
    # Wendy L. Martinez, Angel Martinez, Jeffrey Solka-Exploratory data Analysis 
    # with MATLAB, Second Edition-CRC Press (2010) paginas 134 Example 4.3
    
    n <- nrow(data) 
    
    p <- ncol(data)
    
    v1 <- 1:floor(p/2)  
    
    v2 <- (max(v1) + 1):p
 
    dataNorm <- scale(data, center = TRUE, scale = TRUE) # normaliza os dados
 
    Newdata <- rbind(dataNorm, diag(p))
     
    theta = (0:numrot) * pi/180 # rotacoes
    
    PC <- eigen(cov(dataNorm)) # Encontra os componentes principais
  
    AutVec <- PC$vectors[,p:1] # veja que os autovetores sao os dos menores autovalores para os maiores
 
    Vector1 <- Newdata %*% AutVec[,v1] # projecao
    Vector2 <- Newdata %*% AutVec[,v2] # projecao
    
    proj.data  <- cbind(Vector1, Vector2) # dados projetados
   
    # iguala o numero de colunas dos vetores caso sejam diferentes
    if (length(v1) != length(v2)) {
       if (length(v1) < length(v2)) {
          Vector1 <- cbind(Vector1,Vector2[,1])
       } else Vector2 <- cbind(Vector2,Vector1[,1])
    }
    
    cp.v <- ncol(Vector1) 
    
    numrot <- ifelse(is.na(choicerot[1]), numrot, choicerot)
      
    if (savepicture) {
       cat("\014") # limpa a tela 
       cat("\n\n Salvando graficos em disco. Aguarde o termino!")
    }
    
    i <- 1  
    while (i <= numrot) { # Inicializa o Grand Tour
      
      cor <- 1 # cor inicial dos pontos e legendas
      
      if (i > 1) # novas projecoes
         proj.data = Vector1 %*% diag(1,cp.v) * cos(theta[i]) + Vector2 %*% diag(1,cp.v) * sin(theta[i])
         
      if (is.na(choicerot[1]) || choicerot == i) {
        
        if (savepicture) png(filename = paste("Picture ", i," - Metodo", method,".png",step="")) # salva os graficos em arquivos
        
        maxX = max(proj.data[, 1])
        minX = min(proj.data[, 1])
        maxY = max(proj.data[, 2])
        minY = min(proj.data[, 2])
        
        Tit <- ifelse(!is.character(title) || is.na(title[1]), paste("Rotacao:", i), title)
        
        if (Num.class == 0) {
          
           plot(proj.data, # coordenadas do grafico
                xlab = xlabel, # Nomeia Eixo X
                ylab = ylabel, # Nomeia Eixo Y
                type = "n", # nao plota pontos
                main = Tit, # Titulo para o grafico
                asp  = 1,   # Aspecto do Grafico
                # axes = F,   # elimina os eixos
                xlim = c(minX,maxX), # dimensao eixo X
                ylim = c(minY,maxY)) # dimensao eixo Y
          
           if (grid) {
            
              args <- append(as.list(par('usr')), c('gray95','gray95'))
            
              names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
            
              do.call(rect, args) # chama a funcao rect com os argumentos (args)
            
              grid(col = "white", lwd = 1, lty = 7, equilogs = T)
            
           }
          
           points(proj.data, # coordenadas do grafico
                  pch  = 16,  # formato dos pontos
                  col = ifelse(color, "Blue", "Black"))
          
        } else {
           
           plot(0,0, # cria grafico para as coordenadas linhas x e colunas y
                xlab = xlabel, # Nomeia Eixo X
                ylab = ylabel, # Nomeia Eixo Y
                main = Tit, # Titulo
                asp  = 1,   # Aspecto do Grafico
                type = "n", # nao plota pontos
                xlim = c(minX,maxX), # dimensao eixo X
                ylim = c(minY,maxY), # dimensao eixo Y
                col  = ifelse(color,"red","black"))  # Cor dos pontos
          
           if (grid) {
            
              args <- append(as.list(par('usr')), c('gray95','gray95'))
            
              names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
            
              do.call(rect, args) # chama a funcao rect com os argumentos (args)
            
              grid(col = "white", lwd = 1, lty = 7, equilogs = T)
            
           } 
          
           Init.Form <- 14 # formato inicial dos pontos
          
           for (k in 1:Num.class) {
             
             Point.Form <- Init.Form + k # fomato dos pontos de cada classe
             
             cor1 <- ifelse(color, cor + k, "black")
             
             Point.data <- proj.data[which(class == class.Names[k]),]
             
             points(Point.data,
                    pch = Point.Form, # Formato dos pontos
                    cex = size, # Tamanho dos pontos
                    col = cor1) # adiciona ao grafico as coordenadas principais das colunas
           }
           
        }
        
        if (!is.na(linlab[1])) LocLab(proj.data, cex = 1, linlab)
        
        if (axes) # coloca os eixos
           abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
        
        if (axesvar) { # plota os eixos das variaveis
          
           PosVar <- cbind(proj.data[(n + 1):(n + p),1],proj.data[(n + 1):(n + p),2]) # vetor projecao - coordenadas para os nomes das variaveis
          
           arrows(0,0, PosVar[,1], PosVar[,2],
                  lty = 1, code = 2, length = 0.08, angle = 25,
                  col = ifelse(color, "Red", "Black"))
          
           LocLab(PosVar, cex = 1, colnames(data), xpd = TRUE)
          
        }
         
        if (posleg != 0 && Num.class > 0) {
          
           if (color) cor <- 2
          
           Init.Form <- 15 # codigo formato ponto inicial
           
           color_b <- cor # colore as letras das legendas e suas representacoes no grafico
          
           if (color) color_b = cor:(cor + Num.class)
          
           legend(posleg, class.Names, pch = (Init.Form):(Init.Form + Num.class), col = color_b,
                  text.col = color_b, bty = boxleg, text.font = 6, y.intersp = 0.8, xpd = TRUE) # cria a legenda
        }
      
        if (savepicture) dev.off()
        
        Sys.sleep(0.05) # tempo entre as plotagens dos graficos
      
      }
      
      i <- i + 1
      
    }
    
    A <- cbind(proj.data[(n + 1):(n + p),1], proj.data[(n + 1):(n + p),2]) # vetor projecao
    
    proj.data <- proj.data[1:n,1:d] # dados projetados
    
    if (savepicture) cat("\n \n Fim!")
      
  }
  
  rownames(proj.data)  <- rownames(data)
  
  colnames(proj.data) <- c(paste("Projecao", 1:(ncol(proj.data))))
  
  rownames(A) <- colnames(data)
  
  colnames(A) <- c(paste("Eixo", 1:(ncol(A))))
  
  Lista <- list(proj.data = proj.data, vector.opt = A, method = method)
   
  return(Lista)
   
}
