Biplot <- function(Data, alfa = 0.5, Title = NA, xlabel = NA, ylabel = NA,
                   Color = TRUE, Obs = TRUE, LinLab = NA) {
  # Rotina para gerar Biplot desenvolvida 
  # por Paulo Cesar Ossani em 20/06/2015
  
  # Entrada:
  # Data  - Dados para plotagem.
  # alfa  - Representatividade dos individuos (alfa), 
  #         representatividade das variaveis (1-alfa). 
  #         Sendo 0.5 o default.
  # Title  - Titulo para o grafico. Se nao for definido assume texto padrao.
  # xlabel - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel - Nomeia o eixo Y, se nao definido retorna padrao.
  # Color   - Graficos coloridos (default = TRUE).
  # Obs     - Acrescenta as observacoes ao grafico (default = TRUE).
  # LinLab  - Vetor com o rotulo para as linhas, se nao
  #          informado retorna o padrao dos dados.
  
  # Retorna:
  # Grafico Biplot.
  # Md - Matriz autovalores.
  # Mu - Matriz U (autovetores).
  # Mv - Matriz V (autovetores).
  # Coor_I - Coordenadas dos individuos.
  # Coor_V - Coordenadas das variaveis.
  # PVar   - Proporcao dos componentes principais.
  
  ##### INICIO - Informacoes usadas nos Graficos #####
  
  if (!is.data.frame(Data)) 
     stop("Entrada para 'Data' esta incorreta, deve ser do tipo dataframe. Verifique!")
  
  if (!is.numeric(alfa) || alfa < 0 || alfa > 1)
     stop("Entrada para 'alfa' esta incorreta, deve ser numerica, com valor entre 0 e 1. Verifique!")
  
  if (!is.character(Title) && !is.na(Title))
     stop("Entrada para 'Title' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(xlabel) && !is.na(xlabel))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")

  if (!is.logical(Color))
     stop("Entrada para 'Color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(Obs)) 
     stop("Entrada para 'Obs' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.na(LinLab) && length(LinLab)!=nrow(Data))
     stop("O numero elementos do rotulo para linhas 'LinLab' difere do numero de linhas da base de dados. Verifique!")
  
  if (is.na(LinLab[1])) LinLab <- rownames(Data)
  
  if (is.na(Title)) Title = "Grafico Biplot" 
  
  LinNames <- LinLab # nomes das observacoes
  
  MData = as.matrix(Data) # transforma dados em matriz
  
  ### Centraliza os dados na media
  Media <- apply(MData, 2, mean) # medias por colunas
  MData <- sweep(MData, 2, Media, FUN = "-") # Centraliza na media
    
  ### Decompondo Singularmente a Matriz de Dados
  dim  <- 2 # dimenssao 
  Mdvs <- svd(MData) # Matriz de Decomposicao Valor Singular
  Md = Mdvs$d # Matriz autovalores
  Mu = Mdvs$u # Matriz U (autovetores)
  Mv = Mdvs$v # Matriz V (autovetores)
  
  Coor_I <- Mu[,1:dim]%*%diag(Md[1:dim])^alfa     # coordenadas individuos
  Coor_V <- Mv[,1:dim]%*%diag(Md[1:dim])^(1-alfa) # coordenadas variaveis
  
  PVar <- (Md^2/sum(Md^2)) * 100 # Proporcao dos primeiros (dim) componentes principais
  
  if (is.na(xlabel))
     xlabel = paste("Primeiro componente (",round(PVar[1],2),"%)",sep="")

  if (is.na(ylabel))
     ylabel = paste("Segundo componente (",round(PVar[2],2),"%)",sep="")
  
  MaxX <- max(Coor_I[,1],Coor_V[,1]) + 1 # Dimenssoes maximas das linhas
  MinX <- min(Coor_I[,1],Coor_V[,1]) - 1 # Dimenssoes minimas das linhas
  MaxY <- max(Coor_I[,2],Coor_V[,2]) + 1 # Dimenssoes maximas das colunas
  MinY <- min(Coor_I[,2],Coor_V[,2]) - 1 # Dimenssoes minimas das colunas
  
  # cor  <- "red" # cor inicial
  
  ##### INICIO - Grafico Biplot #####  
  plot(0,0, # Plota as variaveis
       xlab = xlabel,  # Nomeia Eixo X
       ylab = ylabel,  # Nomeia Eixo Y
       main = Title,    # Titulo
       asp  = 1,        # Aspecto do grafico
       cex  = 0,        # Tamanho dos pontos
       xlim = c(MinX,MaxX), # Dimensao para as linhas do grafico
       ylim = c(MinY,MaxY)) # Dimensao para as colunas do grafico
  
  abline(h = 0, v = 0, cex = 1.5, lty = 2) # cria o eixo central
  
  # NomeVar <- colnames(MData) # nomes das variaveis
  arrows(0,0,Coor_V[,1],Coor_V[,2], lwd = 1, code = 2, length = 0.08, angle = 25, col = ifelse(Color==TRUE,"Red","Black")) # cria a seta apontando para cada variavel  

  # NomeVar <- colnames(MData) # nomes das variaveis
  # for (i in 1:nrow(Coor_V)) {  # foi necessario criar este for para poder colocar cores diferentes para cada variavel
  #   arrows(0,0,Coor_V[i,1],Coor_V[i,2], lwd = 1, code = 2, length = 0.08, angle = 25, col = ifelse(Color==TRUE, "Red","Black")) # cria a seta apontando para cada variavel  
  #   #text(Coor_V[i,1], Coor_V[i,2], cex = 1, pos = 4, NomeVar[i], col = ifelse(Color==TRUE, cor + i, 1), xpd = TRUE)  # Coloca os nomes das variaveis
  # }

  # if (Color==TRUE) cor <- c((cor+1):(length(NomeVar)+1))
  NomeVar <- colnames(MData) # nomes das variaveis
  
  LocLab(Coor_V[,1:2], NomeVar, col = ifelse(Color,"Blue","Black"))  # Coloca os nomes das variaveis
    
  if (Obs) {
     NomeVar <- LinNames #rownames(MData) # nomes das observacoes
     LocLab(Coor_I[,1:2], NomeVar, col = "Black") # Coloca os nomes dos individuos
     #for (i in 1:nrow(Coor_I)) 
         #text(Coor_I[i,1], Coor_I[i,2], cex = 1, pos = 3, NomeVar[i], xpd = TRUE)  # Coloca os nomes dos individuos
  
     points(Coor_I,    # Coloca pontos nas posicoes dos individuos
            asp = 1,   # Aspecto do grafico
            pch = 15,  # Formato dos pontos 
            cex = 1.2, # Tamanho dos pontos         
            col = ifelse(Color,"Red","Black"))
  }
 
  ##### FIM - Grafico Biplot #####
  
  Lista <- list(Md = Md, Mu = Mu, Mv = Mv, Coor_I = Coor_I,
                Coor_V = Coor_V, PVar = PVar)
  
  return (Lista) 
  
}

