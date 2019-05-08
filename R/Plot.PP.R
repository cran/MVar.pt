Plot.PP <- function(PP, Titles = NA, xlabel = NA, ylabel = NA, PosLeg = 2, 
                    BoxLeg = TRUE, Color = TRUE, LinLab = NA, AxisVar = TRUE,
                    Axis = TRUE, Casc = TRUE) {
  
  # Rotina para plotar graficos da Projecao Pursuit desenvolvida 
  # por Paulo Cesar Ossani em 2017/02/27
  
  # Entrada:
  # PP       - Dados da funcao Optimizer.
  # Titles   - Titulos para os graficos. Se nao for definido assume texto padrao.
  # xlabel   - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel   - Nomeia o eixo Y, se nao definido retorna padrao.
  # PosLeg   - 0 sem legenda,
  #            1 para legenda no canto superior esquerdo,
  #            2 para legenda no canto superior direito (default),
  #            3 para legenda no canto inferior direito,
  #            4 para legenda no canto inferior esquerdo.
  # BoxLeg   - Colocar moldura na legenda (default = TRUE).
  # Color    - Graficos coloridos (default = TRUE).
  # LinLab   - Nomes para os rotulos das observacoes.
  # AxisVar  - Coloca eixos de rotacao das variaveis, somente quando DimProj > 1 (default = TRUE).
  # Axis     - Plot os eixos X e Y (default = TRUE).
  # Casc    - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Grafico da evolucao dos indices, e graficos cujos dados 
  # foram reduzidos em duas dimensoes.
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
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

  if (!is.na(LinLab[1]) && length(LinLab) != nrow(PP$Proj.Data)) 
      stop("Entrada para 'LinLab' esta incorreta, deve ter o mesmo numero de linhas que os dados de entrada em 'Data'. Verifique!")

  if (is.na(PP$Findex[1])) PP$Findex <- "Not Available"
  
  if (!is.logical(Casc))
     stop("Entrada para 'Casc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  
  if (is.na(xlabel[1]))
     xlabel = "Eixo X" 

  if (is.na(ylabel[1]))
     ylabel = "Eixo Y"

  if (PosLeg==1) PosLeg = "topleft"     # posicao das legendas nos graficos
  if (PosLeg==2) PosLeg = "topright"
  if (PosLeg==3) PosLeg = "bottomright"
  if (PosLeg==4) PosLeg = "bottomleft"
  
  BoxLeg = ifelse(BoxLeg,"o","n") # moldura nas legendas, "n" sem moldura, "o" com moldura

  if (!is.na(PP$Num.Class[1])) {
     Data <- as.matrix(PP$Proj.Data[,1:(ncol(PP$Proj.Data)-1)])
  } else Data <- PP$Proj.Data
  
  Num.Class = ifelse(is.na(PP$Num.Class), 0, PP$Num.Class)
  
  Class.Names <- PP$Class.Names #names(Class.Table)  # nomes das classses
  
  if (Num.Class == 0) {
     Data <- PP$Proj.Data
     NomeLinhas = rownames(PP$Proj.Data)
  } else {
     Data <- as.matrix(PP$Proj.Data[,1:(ncol(PP$Proj.Data)-1)])
     NomeLinhas <- as.matrix(PP$Proj.Data[,ncol(PP$Proj.Data)])
  }

  cor <- 1 # cor inicial dos pontos e legendas
  ##### FIM - Informacoes usadas nos Graficos #####
  
  if (!is.character(Titles[1]) || is.na(Titles[1])) Titles[1] = c("Evolucao do indice")
  if (!is.character(Titles[2]) || is.na(Titles[2])) Titles[2] = paste("Funcao indice:", PP$Findex)

  #### INICIO - Plota os indices das projecoes ####
  bg <- c('gray95') # background - cor de fundo
  
  linCol <- c('blue') # cor da funcao plotada
  
  Cood.xy = round(PP$Index,4)
  
  if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  plot(Cood.xy,
       xlab = "Simulacao",
       ylab = "Valor do Indice",
       main = Titles[1], # Titulo
       type = "n",   # tipo de grafico (pontos, linhas, ambos)
       bty  = "l",   # tipo de caixa do grafico
       cex.axis = 1, # tamanho do 'tick' dos eixos
       cex.lab  = 1) # tamanho dos nomes dos eixos
  
  args <- append(as.list(par('usr')), c(bg,bg))
  
  names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
  
  do.call(rect, args) # chama a funcao rect com os argumentos (args)
  
  grid(col = "white", lwd = 1, lty = 7, equilogs = T)#, lty = "dotted", lwd = par("lwd"), equilogs = T)
  
  lines(Cood.xy, col = linCol)
  #### FIM - Plota os indices das projecoes ####


  #### Plotas as projecoes 2D
  if (ncol(Data) == 2) {
    
     maxX = max(Data[,1], PP$Vector.Opt[,1]) 
     minX = min(Data[,1], PP$Vector.Opt[,1]) 
     maxY = max(Data[,2], PP$Vector.Opt[,2])
     minY = min(Data[,2], PP$Vector.Opt[,2])
    
     if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
     
     if (Num.Class == 0) {
       
        plot(Data[,1:2], # coordenadas do grafico
            xlab = xlabel, # Nomeia Eixo X
            ylab = ylabel, # Nomeia Eixo Y
            main = Titles[2], # Titulo para o grafico
            pch  = 16,  # formato dos pontos
            axes = F,   # elimina os eixos
            xlim = c(minX,maxX), # dimensao eixo X
            ylim = c(minY,maxY), # dimensao eixo Y
            col  = ifelse(Color, "Blue", "Black"))
       
     } else {
       
       plot(0,0, # cria grafico para as coordenadas principais das linhas
            xlab = xlabel,  # Nomeia Eixo X
            ylab = ylabel,  # Nomeia Eixo Y
            main = Titles[2],  # Titulo
            asp = 1,           # Aspecto do Grafico
            cex = 0,           # Tamanho dos pontos
            xlim = c(minX, maxX), # Dimensao para as linhas do grafico
            ylim = c(minY, maxY)) # Dimensao para as colunas do grafico
 
       Init.Form <- 14 # formato inicial dos pontos
       
       for (i in 1:Num.Class) {
         
         Point.Form <- Init.Form + i # fomato dos pontos de cada classe
         
         cor1 <- ifelse(Color, cor + i, "black")
         
         Point.Data <- Data[which(PP$Proj.Data[,ncol(PP$Proj.Data)] == Class.Names[i]),]

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
    
     if (Casc) dev.new() # efeito cascata na apresentacao dos graficos
    
     if (Num.Class == 0) {
       
        cor1 <- ifelse(Color, "Blue", "Black")
          
        plot(Data, # coordenadas do grafico
             xlab = xlabel, # Nomeia Eixo X
             ylab = ylabel, # Nomeia Eixo Y
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
             xlab = xlabel,  # Nomeia Eixo X
             ylab = ylabel,  # Nomeia Eixo Y
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
  
  if (ncol(Data) <= 2) {
    
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
      
    if (!is.na(LinLab[1])) LocLab(Data, cex = 1, LinLab, col = c(cor1))

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
  
}
