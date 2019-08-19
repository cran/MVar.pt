Plot.FA <- function(FA, titles = NA, xlabel = NA, ylabel = NA, size = 1.1,
                    grid = TRUE, color = TRUE, linlab = NA, casc = TRUE) {
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
  # casc   - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  # Cria Titulos para os graficos caso nao existam
  if (!is.character(titles[1]) || is.na(titles[1])) titles[1] = c("Scree-plot das variancias\n das cargas fatoriais")
  if (!is.character(titles[2]) || is.na(titles[2])) titles[2] = c("Escores das observacoes dos\n dois primeiros fatores")
  if (!is.character(titles[3]) || is.na(titles[3])) titles[3] = c("Cargas fatoriais")
  if (!is.character(titles[4]) || is.na(titles[4])) titles[4] = c("Biplot")
  
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
  
  if (!is.na(linlab[1]) && length(linlab)!=nrow(FA$mtxscores))
     stop("O numero elementos do rotulo para linhas 'linlab' difere do numero de linhas da base de dados. Verifique!")
  
  if (!is.logical(casc))
     stop("Entrada para 'casc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (is.na(xlabel[1]))
     xlabel = "Primeiro fator"
  
  if (is.na(ylabel[1]))
     ylabel = "Segundo fator"
  
  #####   FIM - Informacoes usadas nos Graficos  #####
  
  if (casc) dev.new() # efeito cascata na apresentacao dos graficos

  ##### INICIO - Plotagem dos Autovalores #####
  mp <- barplot(FA$mtxvar[,1],names.arg=paste(round(FA$mtxvar[,2],2),"%",sep=""),
                main = "Variancias dos Fatores")
  ##### FIM - Plotagem dos Autovalores #####
  
  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Scree-plot dos Fatores #####
  plot(1:length(FA$mtxvar[,1]), FA$mtxvar[,1],
       type = "n", # nao plota pontos
       xlab = "Ordem dos Fatores", 
       ylab = "Variancia",
       main = titles[1])  
  
  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray93','gray93'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 2, lty = 7, equilogs = T)
    
  }
  
  points(1:length(FA$mtxvar[,1]), FA$mtxvar[,1], type = "b")
  ##### FIM - Scree-plot dos Fatores #####

  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Plotagem Escores das observacoes #####
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
  
  points(FA$mtxscores,  # cria grafico para os Escores das observacoes 
         pch = 15, # Formato dos pontos
         cex = size,  # Tamanho dos pontos
         col = ifelse(color,"red","black")) # Cor dos pontos
    
  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  if (!is.na(linlab[1])) LocLab(FA$mtxscores, cex = 1, linlab)
  ##### FIM - Plotagem Escores das observacoes #####
  
  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Cargas fatoriais #####
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
  
  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  arrows(0,0,FA$mtxcarga[,1],FA$mtxcarga[,2], lwd = 1, code = 2, length = 0.08, angle = 25, col = ifelse(color,"Red","Black")) # cria a seta apontando para cada variavel  
  
  NomeVar <- rownames(FA$mtxcarga) # nomes das variaveis
  LocLab(FA$mtxcarga[,1:2], NomeVar, col = ifelse(color,"Blue","Black"))  # Coloca os nomes das variaveis
  ##### FIM - Cargas fatoriais #####

  if (casc) dev.new() # efeito cascata na apresentacao dos graficos
  
  ##### INICIO - Biplot ##### 
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
  
  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  arrows(0,0,FA$mtxcarga[,1],FA$mtxcarga[,2], lwd = 1, code = 2, length = 0.08, angle = 25, col = ifelse(color,"Black","Black")) # cria a seta apontando para cada variavel  
  
  NomeVar <- rownames(FA$mtxcarga) # nomes das variaveis
  LocLab(FA$mtxcarga[,1:2], NomeVar, col = ifelse(color,"Blue","Black")) # Coloca os nomes das variaveis
  
  points(FA$mtxscores,    # Coloca pontos nas posicoes dos individuos
         # asp = 1,  # Aspecto do grafico
         pch = 15, # Formato dos pontos 
         cex = size,  # Tamanho dos pontos       
         col = ifelse(color,"Red","Black"))
  
  if (!is.na(linlab[1])) LocLab(FA$mtxscores, cex = 1, linlab)
  ##### FIM - Biplot #####
}