Biplot <- function(data, alpha = 0.5, title = NA, xlabel = NA, ylabel = NA,
                   size = 1.1, grid = TRUE, color = TRUE, obs = TRUE, 
                   linlab = NA) {
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
  # obs    - Acrescenta as observacoes ao grafico (default = TRUE).
  # linlab - Vetor com o rotulo para as linhas, se nao
  #          informado retorna o padrao dos dados.
  
  # Retorna:
  # Grafico Biplot.
  # Md - Matriz autovalores.
  # Mu - Matriz U (autovetores).
  # Mv - Matriz V (autovetores).
  # coorI - Coordenadas dos individuos.
  # coorV - Coordenadas das variaveis.
  # pvar   - Proporcao dos componentes principais.
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  
  if (!is.data.frame(data)) 
     stop("Entrada para 'data' esta incorreta, deve ser do tipo dataframe. Verifique!")
  
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

  if (!is.logical(obs)) 
     stop("Entrada para 'obs' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.na(linlab[1]) && length(linlab)!=nrow(data))
     stop("O numero elementos do rotulo para linhas 'linlab' difere do numero de linhas da base de dados. Verifique!")
  
  if (is.na(linlab[1])) linlab <- rownames(data)
  
  if (is.na(title[1])) title = "Grafico Biplot" 
  
  LinNames <- linlab # nomes das observacoes
  
  Mdata = as.matrix(data) # transforma dados em matriz
  
  ### Centraliza os dados na media
  Media <- apply(Mdata, 2, mean) # medias por colunas
  Mdata <- sweep(Mdata, 2, Media, FUN = "-") # Centraliza na media
    
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
  
  MaxX <- max(coorI[,1],coorV[,1]) + 1 # Dimenssoes maximas das linhas
  MinX <- min(coorI[,1],coorV[,1]) - 1 # Dimenssoes minimas das linhas
  MaxY <- max(coorI[,2],coorV[,2]) + 1 # Dimenssoes maximas das colunas
  MinY <- min(coorI[,2],coorV[,2]) - 1 # Dimenssoes minimas das colunas

  ##### INICIO - Grafico Biplot #####  
  plot(0,0, # Plota as variaveis
       xlab = xlabel,  # Nomeia Eixo X
       ylab = ylabel,  # Nomeia Eixo Y
       main = title,   # Titulo
       asp  = 1,       # Aspecto do grafico
       type = "n", # nao plota pontos
       xlim = c(MinX,MaxX), # Dimensao para as linhas do grafico
       ylim = c(MinY,MaxY)) # Dimensao para as colunas do grafico

  if (grid) {
    
     args <- append(as.list(par('usr')), c('gray95','gray95'))
    
     names(args) <- c('xleft', 'xright', 'ybottom', 'ytop', 'col', 'border')
    
     do.call(rect, args) # chama a funcao rect com os argumentos (args)
    
     grid(col = "white", lwd = 1, lty = 7, equilogs = T)
    
  }

  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  arrows(0,0,coorV[,1],coorV[,2], lwd = 1, code = 2, length = 0.08, angle = 25, col = ifelse(color==TRUE,"Red","Black")) # cria a seta apontando para cada variavel  

  NomeVar <- colnames(Mdata) # nomes das variaveis
  
  LocLab(coorV[,1:2], NomeVar, col = ifelse(color,"Blue","Black"))  # Coloca os nomes das variaveis
    
  if (obs) {
     NomeVar <- LinNames #rownames(Mdata) # nomes das observacoes
     LocLab(coorI[,1:2], NomeVar, col = "Black") # Coloca os nomes dos individuos

     points(coorI,     # Coloca pontos nas posicoes dos individuos
            asp = 1,    # Aspecto do grafico
            pch = 15,   # Formato dos pontos 
            cex = size, # Tamanho dos pontos         
            col = ifelse(color,"Red","Black"))
     
  }
 
  ##### FIM - Grafico Biplot #####
  
  Lista <- list(Md = Md, Mu = Mu, Mv = Mv, coorI = coorI,
                coorV = coorV, pvar = pvar)
  
  return (Lista) 
  
}

