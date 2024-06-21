Biplot <- function(data, alpha = 0.5, title = NA, xlabel = NA, ylabel = NA,
                   size = 1.1, grid = TRUE, color = TRUE, var = TRUE,
                   obs = TRUE, linlab = NA, class = NA, classcolor = NA,
                   posleg = 2, boxleg = TRUE, axes = TRUE, savptc = FALSE,
                   width = 3236, height = 2000, res = 300) {
  
  # Rotina para gerar Biplot desenvolvida 
  # por Paulo Cesar Ossani em 20/06/2015
  
  # Entrada:
  # data   - Dados para plotagem.
  # alpha  - Representatividade dos individuos (alpha), 
  #         representatividade das variaveis (1-alpha). 
  #         Sendo 0.5 o default.
  # title  - Titulo para o grafico. Se nao for definido assume texto padrao.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # size   - Tamanho dos pontos nos graficos.
  # grid   - Coloca grade nos graficos.
  # color  - Graficos coloridos (default = TRUE).
  # var    - Acrescenta as projecoes das variaveis ao grafico (default = TRUE).
  # obs    - Acrescenta as observacoes ao grafico (default = TRUE).
  # linlab - Vetor com o rotulo para as linhas.
  # class   - Vetor com os nomes das classes dos dados.
  # classcolor - Vetor com as cores das classes.
  # posleg  - 0 sem legenda,
  #           1 para legenda no canto superior esquerdo,
  #           2 para legenda no canto superior direito (default),
  #           3 para legenda no canto inferior direito,
  #           4 para legenda no canto inferior esquerdo.  
  # boxleg  - Colocar moldura na legenda (default = TRUE).
  # axes    - Plota os eixos X e Y (default = TRUE).
  # savptc  - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width   - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height  - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res     - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).
  
  # Retorna:
  # Grafico Biplot.
  # Md - Matriz autovalores.
  # Mu - Matriz U (autovetores).
  # Mv - Matriz V (autovetores).
  # coorI - Coordenadas dos individuos.
  # coorV - Coordenadas das variaveis.
  # pvar   - Proporcao dos componentes principais.
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  
  if (!is.data.frame(data) && !is.matrix(data)) 
     stop("Entrada para 'data' esta incorreta, deve ser do tipo dataframe ou matrix. Verifique!")
  
  if (!is.na(class[1])) {
    
    class <- as.matrix(class)
    
    if (nrow(data) != length(class))
      stop("Entrada 'class' ou 'data' esta incorreta, devem conter o mesmo numero de linhas. Verifique!")
  }
  
  if (!is.numeric(alpha) || alpha < 0 || alpha > 1)
     stop("Entrada para 'alpha' esta incorreta, deve ser numerica, com valor entre 0 e 1. Verifique!")
  
  if (!is.character(title) && !is.na(title[1]))
     stop("Entrada para 'title' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
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

  if (!is.logical(var)) 
     stop("Entrada para 'var' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(obs)) 
     stop("Entrada para 'obs' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!var && !obs)
     stop("Entrada para 'var' ou 'obs' esta incorreta, deve ser TRUE ou FALSE, ambas nao podem ser FALSE. Verifique!")

  if (!is.na(linlab[1]) && length(linlab)!=nrow(data))
     stop("O numero elementos do rotulo para linhas 'linlab' difere do numero de linhas da base de dados. Verifique!")
  
  if (!is.logical(boxleg)) 
     stop("Entrada para moldura da legenda 'boxleg' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(axes)) 
     stop("Entrada para 'axes' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.numeric(posleg) || posleg < 0 || posleg > 4 || (floor(posleg)-posleg) != 0)
     stop("Entrada para posicao da legenda 'posleg' esta incorreta, deve ser um numero inteiro entre [0,4]. Verifique!")
  
  if (!is.logical(savptc))
     stop("Entrada para 'savptc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.numeric(width) || width <= 0)
     stop("Entrada para 'width' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.numeric(height) || height <= 0)
     stop("Entrada para 'height' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.numeric(res) || res <= 0)
     stop("Entrada para 'res' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  # if (is.na(linlab[1])) linlab <- rownames(data)
  
  if (savptc) {
     message("\014") # limpa a tela
     message("\n\n Salvando graficos em disco. Aguarde o termino!")
  }
  
  if (is.na(title[1])) title = "Grafico Biplot" 
  
  LinNames <- linlab # nomes das observacoes
  
  Mdata = as.matrix(data) # transforma dados em matriz
  
  ### Centraliza os dados na media
  Media <- apply(Mdata, 2, mean) # medias por colunas
  Mdata <- sweep(Mdata, 2, Media, FUN = "-") # centraliza na media
    
  ### Decompondo Singularmente a Matriz de Dados
  dim  <- 2 # dimenssao 
  Mdvs <- svd(Mdata) # Matriz de Decomposicao Valor Singular
  Md = Mdvs$d # Matriz autovalores
  Mu = Mdvs$u # Matriz U (autovetores)
  Mv = Mdvs$v # Matriz V (autovetores)
  
  coorI <- Mu[,1:dim]%*%diag(Md[1:dim])^alpha     # coordenadas individuos
  coorV <- Mv[,1:dim]%*%diag(Md[1:dim])^(1-alpha) # coordenadas variaveis
  
  pvar <- (Md^2/sum(Md^2)) * 100 # Proporcao dos primeiros (dim) componentes principais
  
  if (is.na(xlabel[1]))
     xlabel = paste("Primeira coordenada (",round(pvar[1],2),"%)",sep="")

  if (is.na(ylabel[1]))
     ylabel = paste("Segunda coordenada (",round(pvar[2],2),"%)",sep="")
  
  if (posleg==1) posleg = "topleft"   # posicao das legendas nos graficos
  if (posleg==2) posleg = "topright"
  if (posleg==3) posleg = "bottomright"
  if (posleg==4) posleg = "bottomleft"
  
  boxleg = ifelse(boxleg,"o","n") # moldura nas legendas, "n" sem moldura, "o" com moldura
  
  Num.class = 0
  if (!is.na(class[1])) {
     class.Table <- table(class)        # cria tabela com as quantidade dos elementos das classes
     class.Names <- names(class.Table)  # nomes das classses
     Num.class   <- length(class.Table) # numero de classes
     NomeLinhas  <- as.matrix(class)
  } 
 
  if (Num.class != 0 && length(classcolor) != Num.class && !is.na(classcolor) ||
      Num.class == 0 && length(classcolor) != 1 && !is.na(classcolor))
     stop("Entrada para 'classcolor' esta incorreta, deve ser em quantidade igual ao numero de classes em 'class'. Verifique!")
  
  MaxX <- max(coorI[,1],coorV[,1]) + 1 # Dimenssoes maximas das linhas
  MinX <- min(coorI[,1],coorV[,1]) - 1 # Dimenssoes minimas das linhas
  MaxY <- max(coorI[,2],coorV[,2]) + 1 # Dimenssoes maximas das colunas
  MinY <- min(coorI[,2],coorV[,2]) - 1 # Dimenssoes minimas das colunas

  ##### INICIO - Grafico Biplot #####  
  
  if (savptc) png(filename = "Figure Biplot.png", width = width, height = height, res = res) # salva os graficos em arquivo
  
  plot(0,0, # Plota as variaveis
       xlab = xlabel, # Nomeia Eixo X
       ylab = ylabel, # Nomeia Eixo Y
       main = title,  # Titulo
       type = "n",    # nao plota pontos
       xlim = c(MinX,MaxX), # Dimensao para as linhas do grafico
       ylim = c(MinY,MaxY)) # Dimensao para as colunas do grafico

  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }

  if (axes) abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  if (var) {
     arrows(0,0,coorV[,1],coorV[,2], lwd = 1, code = 2, length = 0.08, angle = 25, col = ifelse(color==TRUE,"Red","Black")) # cria a seta apontando para cada variavel  

     NomeVar <- colnames(Mdata) # nomes das variaveis
  
     LocLab(coorV[,1:2], NomeVar, col = ifelse(color,"Blue","Black"))  # Coloca os nomes das variaveis
  }
  
  if (!is.na(classcolor[1])) {
     cor.classe <- classcolor
  }
  else { cor.classe <- c("red") }
  
  if (obs) {
     
     NomeVar <- LinNames # nomes das observacoes
     
     if (Num.class == 0) {
       
        points(coorI,      # coloca pontos nas posicoes dos individuos
               pch = 15,   # formato dos pontos 
               cex = size, # tamanho dos pontos         
               col = ifelse(color, cor.classe, "Black"))
       
     } else {
            
        cor <- 1 # cor inicial dos pontos e legendas
          
        Init.Form <- 14 # formato inicial dos pontos
          
        for (k in 1:Num.class) {
        
            Point.Form <- Init.Form + k # fomato dos pontos de cada classe
            
            if (!is.na(classcolor[1])) {
              cor1 <- ifelse(color, cor.classe[k], "black")
            }
            else { cor1 <- ifelse(color, cor + k, "black") }

            Point.data <- coorI[which(class == class.Names[k]),]
            
            points(Point.data,
                   pch = Point.Form, # formato dos pontos
                   cex = size, # tamanho dos pontos  
                   col = cor1) # adiciona ao grafico as coordenadas principais das colunas
        }
            
     }
     
     if (!is.na(NomeVar[1]))
        LocLab(coorI[,1:2], NomeVar, col = "Black") # Coloca os nomes dos individuos
 
     if (posleg != 0 && Num.class > 0) {
       
        Init.Form <- 15 # codigo formato ponto inicial

        cor <- ifelse(color, 2, 1)
        
        if (color) {
           if (!is.na(classcolor[1])) {
              color_b <- classcolor
           }
           else { color_b <- cor:(cor + Num.class) }
        }
        else { color_b <- cor }
        
        legend(posleg, class.Names, pch = (Init.Form):(Init.Form + Num.class), col = color_b,
               text.col = color_b, bty = boxleg, text.font = 6, y.intersp = 0.8, xpd = TRUE) # cria a legenda
     }
     
  }
 
  ##### FIM - Grafico Biplot #####
  
  if (savptc) {
     box(col = 'white')
     dev.off()
     message("\n \n Fim!")
  }
  
  Lista <- list(Md = Md, Mu = Mu, Mv = Mv, coorI = coorI,
                coorV = coorV, pvar = pvar)
  
  return (Lista) 
  
}