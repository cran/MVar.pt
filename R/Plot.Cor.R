Plot.Cor <- function(data, title = NA, grid = TRUE, leg = TRUE, boxleg = FALSE, text = FALSE, 
                     arrow = TRUE, color = TRUE, namesvar = NA, savptc = FALSE, width = 3236, 
                     height = 2000, res = 300) {
  
  # Rotina para plotar o grafico das correlacoes de variaveis
  # desenvolvida por Paulo Cesar Ossani em 08/2020 
  
  # Entrada:
  # data   - Dados com as variaveis para estabelecer as correlacoes
  # title  - Titulo para o grafico
  # grid   - Coloca grade nos grafico (default = TRUE).
  # leg    - Coloca a legenda no grafico (default = TRUE).
  # boxleg  - Colocar moldura na legenda (default = FALSE).
  # text - Coloca os valores das correlacoes nos circulos (default = FALSE).
  # arrow - Setas das correlacoes positivas (para cima) e negativas (para baixo) (default = TRUE).
  # color  - Graficos coloridos (default = TRUE).
  # namesvar - Vetor com os nomes das variaveis.
  # savptc - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width  - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res    - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).

  # Retorna:
  # Grafico das correlacoes das variaveis em data
  
  ##### INICIO - Informacoes usadas no Grafico #####
  if (!is.data.frame(data))
     stop("Entrada 'data' esta incorreta, deve ser do tipo dataframe. Verifique!")
  
  if (ncol(data) < 2)
     stop("Numero de colunas em 'data' deve ser maior que dois. Verique!")
  
  # Cria Titulo para o grafico caso nao exista
  if (!is.character(title) || is.na(title)) title = c("Grafico das correlacoes\n entre as variaveis")

  if (!is.logical(grid))
     stop("Entrada para 'grid' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(leg)) 
     stop("Entrada par 'leg' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(boxleg)) 
     stop("Entrada para 'boxleg' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(text)) 
     stop("Entrada par 'text' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(arrow)) 
     stop("Entrada par 'arrow' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.logical(color))
     stop("Entrada para 'color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.na(namesvar[1]) && length(namesvar) != ncol(data))
     stop("O numero de elementos em 'namesvar' difere do numero de colunas em 'data'. Verifique!")
  
  if (is.na(namesvar[1])) namesvar <- colnames(data)
  
  if (!is.logical(savptc))
     stop("Entrada para 'savptc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.numeric(width) || width <= 0)
     stop("Entrada para 'width' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.numeric(height) || height <= 0)
     stop("Entrada para 'height' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.numeric(res) || res <= 0)
     stop("Entrada para 'res' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  #####   FIM - Informacoes usadas nos Graficos  #####

  if (savptc) {
     message("\014") # limpa a tela
     message("\n\n Salvando graficos em disco. Aguarde o termino!")
  }
  
  if (savptc) png(filename = "Figure Correlation.png", width = width, height = height, res = res) # salva os graficos em arquivos
  
  tab.corr <- cor(data) # correlacao entre as variaveis
  
  ncol <- ncol(data)  # lado do quadrado
  
  plot(0, 0, xlim = c(0,(ncol + 0.3)), ylim = c(0,ncol), type = "n", 
       asp = 1, axes = F, xlab = " ", ylab = " ", main = title)
  
  if (grid) {
    rect(0, 0, ncol, ncol, col = ifelse(color,'gray93','white'))
    
    for(i in 0:ncol) {
      segments(i, 0, x1 = i, y1 = ncol, lwd = 2, lty = 7, col = ifelse(color,"white","black"))
      segments(0, i, x1 = ncol, y1 = i, lwd = 2, lty = 7, col = ifelse(color,"white","black"))
    }
  }
  
  seta <- 0 # determina a direcao da seta
  
  for(i in 1:ncol) {
    for(j in 1:ncol) {
      
      if (arrow) seta <- ifelse(tab.corr[i,j] < 0,-0.3, 0.3) # determina a direcao da seta
      
      vlr <- abs(tab.corr[i,j]) # valor da correlacao
      
      vlr.circ <- 0
      if (vlr >= 0 && vlr < 0.3) {
         vlr.circ <- 0.1
         col <- 2
      } else {
      if (vlr >= 0.3 && vlr < 0.5) {
         vlr.circ <- 0.3
         col <- 3
      } else {
      if (vlr >= 0.5 && vlr < 0.7) {
         vlr.circ <- 0.5
         col <- 4
      } else {
      if (vlr >= 0.7 && vlr < 0.9) {
         vlr.circ <- 0.7
         col <- 5
      } else {
      if (vlr >= 0.9 && vlr <= 1) {
         vlr.circ <- 0.92
         col <- 6
      }}}}}
      
      if (i == j) col <- "lightgray" # correlacao da mesma variavel
      
      x <- i - 0.5
      y <- ncol - j + 0.5
      
      symbols(x, y, circles = vlr.circ/2, inches = FALSE, bg = ifelse(color,col,'white'), add = TRUE)

      if (arrow && i != j) 
         arrows(x, y, x, y + seta, col = 1, lwd = 2, lty = 1, code = 2, length = 0.13, angle = 25)
      
      if(text) text(x, y, labels = round(tab.corr[i,j],3))
    }
  }
  
  if (leg) {
     leg <- c("Fraca","Baixa","Moderada","Alta","Forte") 
     y <- ncol/2 + log10(ncol) + ifelse(ncol>6,log10(ncol)-0.5,0.1) #sum(par('usr')[3:4])/2 + 2
     boxleg = ifelse(boxleg,"o","n") # moldura nas legendas, "n" sem moldura, "o" com moldura
    
     if (color) cor <- 2:6 else cor <- 'black'
     
     legend(ncol, y , legend = leg, pch = 19, col = cor, 
            text.col = cor, bty = boxleg, cex = 1, 
            y.intersp = 1.2, xpd = F) # cria a legenda
  }
  
  text(c(1:ncol)-0.5, -0.1,par("usr")[1], labels = namesvar, srt = 45, pos = 2, xpd = TRUE, cex = 0.9)
  text(-0.01,c(1:ncol)-0.5, par("usr")[1], labels = namesvar[length(namesvar):1], srt = 45, pos = 2, xpd = TRUE, cex = 0.9)
  
  if (savptc) {
     box(col = 'white')
     dev.off()
     message("\n \n Fim!")
  }
  
}

