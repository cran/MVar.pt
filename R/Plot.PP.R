Plot.PP <- function(PP, Titles = NULL, PosLeg = 2, BoxLeg = TRUE, Color = TRUE,
                    Label = FALSE, LabNames = NULL, AxisVar = TRUE, Axis = TRUE) {
  
  # Rotina para plotar graficos da Projecao Pursuit desenvolvida 
  # por Paulo Cesar Ossani em 2017/02/27
  
  # Entrada:
  # PP       - Dados da funcao Optimizer.
  # Titles   - Titulo para o graficos. Se nao for definido assume texto padrao.
  # PosLeg   - 0 sem legenda,
  #            1 para legenda no canto superior esquerdo,
  #            2 para legenda no canto superior direito (default),
  #            3 para legenda no canto inferior direito,
  #            4 para legenda no canto inferior esquerdo.
  # BoxLeg   - Colocar moldura na legenda (default = TRUE).
  # Color    - Graficos coloridos (default = TRUE).
  # Label    - Coloca os rotulos das observacoes (default = FALSE).
  # LabNames - Nomes dos rotulos das observacoes, se omitido retorna a numeracao default.
  # AxisVar  - Coloca eixos de rotacao das variaveis, somente quando DimProj > 1 (default = TRUE).
  # Axis     - Plot os eixos X e Y (default = TRUE).
  
  # Retorna:
  # Grafico da evolucao dos indices, e graficos cujos dados 
  # foram reduzidos em duas dimensoes.
  
  if (!is.numeric(PosLeg) || PosLeg < 0 || PosLeg > 4 || (floor(PosLeg)-PosLeg) != 0)
     stop("Entrada para posicao da legenda 'PosLeg' esta incorreta, deve ser um numero inteiro entre [0,4]. Verifique!")

  if (!is.logical(Color))
     stop("Entrada para 'Color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(BoxLeg)) 
     stop("Entrada para moldura da legenda 'BoxLeg' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(AxisVar))
     stop("Entrada para 'AxisVar' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(Axis)) 
     stop("Entrada para 'Axis' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.null(LabNames) && length(LabNames) != nrow(PP$Proj.Data)) 
      stop("Entrada para 'LabNames' esta incorreta, deve ter o mesmo numero de linhas que os dados de entrada em 'Data'. Verifique!")

  if (!is.logical(Label)) 
     stop("Entrada para 'Label' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (is.null(PP$Findex)) PP$Findex <- "Not Available"
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  DescEixo1 <- ifelse(Axis,"Eixo X","")
  DescEixo2 <- ifelse(Axis,"Eixo Y","")
    
  if (PosLeg==1) PosLeg = "topleft"     # posicao das legendas nos graficos
  if (PosLeg==2) PosLeg = "topright"
  if (PosLeg==3) PosLeg = "bottomright"
  if (PosLeg==4) PosLeg = "bottomleft"
  
  BoxLeg = ifelse(BoxLeg,"o","n") # moldura nas legendas, "n" sem moldura, "o" com moldura
  
  if (!is.null(LabNames)) {
     Class.Table <- table(LabNames)     # cria tabela com as quantidade dos elementos das classes
     Class.Names <- names(Class.Table)  # nomes das classses
     Num.Class   <- length(Class.Table) # numero de classes
     NomeLinhas  <- as.matrix(LabNames)
  } else {
     Class.Names <- ifelse(is.na(PP$Class.Names), "", PP$Class.Names) # nomes das classses
     Num.Class   <- ifelse(is.na(PP$Num.Class), 0, PP$Num.Class) # numero de classes
     
     if (Num.Class == 0) {
        NomeLinhas = rownames(PP$Proj.Data)
     } else {
       NomeLinhas <- as.matrix(PP$Proj.Data[,ncol(PP$Proj.Data)])
     }
  }
  
  if (!is.na(PP$Num.Class)) {
    Data <- as.matrix(PP$Proj.Data[,1:(ncol(PP$Proj.Data)-1)])
  } else Data <- PP$Proj.Data
  
  cor <- 1 # cor inicial dos pontos e legendas
  ##### FIM - Informacoes usadas nos Graficos #####
  
  if (!is.character(Titles[1]) || is.na(Titles[1])) Titles[1] = c("Evolucao do indice")
  if (!is.character(Titles[2]) || is.na(Titles[2])) Titles[2] = paste("Funcao indice:", PP$Findex)

  #### INICIO - Plota os indices das projecoes ####
  bg <- c('gray95') # background - cor de fundo
  
  linCol <- c('blue') # cor da funcao plotada
  
  Cood.xy = round(PP$Index,4)
  
  plot(Cood.xy,
       xlab = "Simulacao",
       ylab = "Valor do Indice",
       main = Titles[1], # Titulo
       type = "n",   # tipo de grafico (pontos, linhas, ambos)
       bty = "l",    # tipo de caixa do grafico
       # asp = 40,   # aspecto do grafico
       cex.axis = 1, # tamanho do 'tick' dos eixos
       cex.lab = 1)  # tamanho dos nomes dos eixos
  
  args <- append(as.list(par('usr')), c(bg,bg))
  
  names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
  
  do.call(rect, args) # chama a funcao rect com os argumentos (args)
  
  grid(col = "white", lwd = 1, lty = 7, equilogs = T)#, lty = "dotted", lwd = par("lwd"), equilogs = T)
  
  lines(Cood.xy, col = linCol)
  #### FIM - Plota os indices das projecoes ####


  #### Plotas as projecoes 2D
  if (ncol(Data) == 2) {
    
     maxX = max(Data[, 1], PP$Vector.Opt[,1]) 
     minX = min(Data[, 1], PP$Vector.Opt[,1]) 
     maxY = max(Data[, 2], PP$Vector.Opt[,2])
     minY = min(Data[, 2], PP$Vector.Opt[,2])
    
     if (Num.Class == 0) {
       
        plot(Data[,1:2], # coordenadas do grafico
            xlab = DescEixo1, # Nomeia Eixo X
            ylab = DescEixo2, # Nomeia Eixo Y
            main = Titles[2], # Titulo para o grafico
            pch  = 16,  # formato dos pontos
            axes = F,   # elimina os eixos
            xlim = c(minX,maxX), # dimensao eixo X
            ylim = c(minY,maxY), # dimensao eixo Y
            col  = ifelse(Color, "Blue", "Black"))
       
     } else {
       
       plot(0,0, # cria grafico para as coordenadas principais das linhas
            xlab = DescEixo1,  # Nomeia Eixo X
            ylab = DescEixo2,  # Nomeia Eixo Y
            main = Titles[2],  # Titulo
            asp = 1,           # Aspecto do Grafico
            cex = 0,           # Tamanho dos pontos
            xlim = c(minX, maxX), # Dimensao para as linhas do grafico
            ylim = c(minY, maxY)) # Dimensao para as colunas do grafico
 
       Init.Form <- 14 # formato inicial dos pontos
       
       for (i in 1:Num.Class) {
         
         Point.Form <- Init.Form + i # fomato dos pontos de cada classe
         
         cor1 <- ifelse(Color, cor + i, "black")
         
         if (is.null(LabNames)) {
            Point.Data <- Data[which(PP$Proj.Data[,ncol(PP$Proj.Data)] == Class.Names[i]),]
         } else {
            Point.Data <- Data[which(LabNames == Class.Names[i]),]
         }                        
         
         points(Point.Data,
                pch = Point.Form, # Formato dos pontos
                cex = 1.2,  # Tamanho dos pontos
                col = cor1) # adiciona ao grafico as coordenadas principais das colunas
       }
       
       if (Color) cor <- 2
       
       Init.Form <- 15

     }
     
  }
  
  
  #### Plotas as projecoes 1D
  if (ncol(Data) == 1) {  
    
     if (Num.Class == 0) {
       
        cor1 <- ifelse(Color, "Blue", "Black")
          
        plot(Data, # coordenadas do grafico
             xlab = DescEixo1, # Nomeia Eixo X
             ylab = DescEixo2, # Nomeia Eixo Y
             type = "o",
             main = Titles[2], # Titulo para o grafico
             pch  = 16,  # formato dos pontos
             axes = F,   # elimina os eixos
             cex  = 0.9, # Tamanho dos pontos
             col = cor1)
       
     } else {
       
        maxX = length(Data[,1])
        minX = 5
        maxY = max(Data[, 1])
        minY = min(Data[, 1])
        
        Init.Form <- 15 # formato inicial dos pontos
        
        if (Color) {
           cor1 <- c(cor:(cor + Num.Class))[as.factor(NomeLinhas)]
        } else {
           cor1 <- c("black")
        }
        
        Point.Data <- cbind((1:nrow(Data)) + minX, Data)   

        plot(Point.Data, # cria grafico para as coordenadas principais das linhas
             xlab = DescEixo1,  # Nomeia Eixo X
             ylab = DescEixo2,  # Nomeia Eixo Y
             type = "o", # tipo de grafico
             main = Titles[2], # Titulo
             axes = F,   # Elimina os eixos
             cex  = 0.9, # Tamanho dos pontos
             xlim = c(minX, maxX), # Dimensao para as linhas do grafico
             ylim = c(minY, maxY), # Dimensao para as colunas do grafico
             pch  = c((Init.Form):(Init.Form + Num.Class))[as.factor(NomeLinhas)], # Formato dos pontos
             col  = c(cor1))

     }

  }
  
  if (PosLeg != 0 && Num.Class > 0) {
      
    Color_b <- cor # colore as letras das legendas e suas representacoes no grafico
      
    if (Color) Color_b = cor:(cor + Num.Class)
      
    legend(PosLeg, Class.Names, pch = (Init.Form):(Init.Form + Num.Class), col = Color_b,
           text.col = Color_b, bty = BoxLeg, text.font = 6, y.intersp = 0.8, xpd = TRUE) # cria a legenda
  }
    
  if (Color) {
     cor1 <- c(cor:(cor + Num.Class))[as.factor(NomeLinhas)]
  } else {
     cor1 <- c("black")
  }
    
  if (Label) LocLab(Data, cex = 1, NomeLinhas, col = c(cor1))
  
  if (Axis) abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central 
 
  if (AxisVar && ncol(Data) == 2 ) { # plota os eixos das variaveis
    
     Ajuste <- c(diff(range(Data[,1])) / 2 + min(Data[,1]),
                 diff(range(Data[,2])) / 2 + min(Data[,2]))
    
     PosVar <- cbind(PP$Vector.Opt[,1] + Ajuste[1], PP$Vector.Opt[,2] + Ajuste[2]) # Posicao para as variaveis no grafico
    
     arrows(Ajuste[1], Ajuste[2], PosVar[,1], PosVar[,2],
            lty = 1, code = 2, length = 0.08, angle = 25,
            col = ifelse(Color, "Red", "Black"))
    
     LocLab(PosVar, cex = 1, rownames(PP$Vector.Opt), xpd = TRUE)
    
  }

}