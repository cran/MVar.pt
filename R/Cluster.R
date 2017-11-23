Cluster <- function(Data, Hierarquico = TRUE, Analise = "Obs", CorAbs = FALSE, 
                    Normaliza = FALSE, Distance = "euclidean", Metodo = "complete", 
                    Horizontal = FALSE, NumGrupos = 0) {
  # Esta funcao executa a Analise de Agrupamentos Hierarquicos e
  # Nao-Hierarquicos, desenvolvida por Paulo Cesar Ossani em 07/2016
  
  # Entrada:
  # Data - Dados a serem a analizados
  # Hierarquico - Agrupamentos hierarquicos (default = TRUE), 
  #               para agrupamentos nao hierarquicos (metodo K-Means), 
  #               somente para caso Analise = "Obs".
  # Analise - "Obs" para analises nas observacoes (default),
  #           "Var" para analises nas variaveis.
  # CorAbs  - Matriz de correlacao absoluta caso Analise = "Var" (default = FALSE).
  # Normaliza - Normalizar os dados somente para caso Analise = "Obs" (default = TRUE).
  # Distance - Metrica das distancias caso agrupamentos hierarquicos:
  #            "euclidean" (default), "maximum", "manhattan",
  #            "canberra", "binary" ou "minkowski". Caso Analise = "Var" a metrica
  #            sera a matriz de correlacao, conforme CorAbs.
  # Metodo - Metodo para analises caso agrupamentos hierarquicos:
  #          "complete" (default), "ward.D", "ward.D2", "single",
  #          "average", "mcquitty", "median" ou "centroid".
  # Horizontal - Dendograma na horizontal (default = FALSE).
  # NumGrupos - Numero de grupos a formar.
  
  # Retorna:
  # Varios graficos.
  # TabRes - Tabela com as similaridades e distancias dos grupos formados.
  # Groups - Dados originais com os grupos formados.
  # ResGroups - Resultados dos grupos formados.
  # SQT - Soma do quadrado total.
  # MatrixD - Matriz das distancias.
  
  if (!is.data.frame(Data)) 
     stop("Entrada 'Data' esta incorreta, deve ser do tipo dataframe. Verifique!")

  if (!is.logical(Hierarquico)) 
     stop("Entrada para 'Hierarquico' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  Analise <- toupper(Analise)
  
  if (Analise != "OBS" && Analise != "VAR") 
     stop("Entrada para 'Analise' esta incorreta, deve ser 'Obs' para as observacoes ou 'Var' para as variaveis. Verifique!")

  if (!is.logical(CorAbs)) 
     stop("Entrada para 'CorAbs' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(Normaliza)) 
     stop("Entrada para 'Normaliza' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  DISTANCE <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  if (is.na(pmatch(Distance, DISTANCE))) 
     stop("Entrada para 'Distance' esta incorreta, deve ser: 'euclidean', 
          'maximum', 'manhattan', 'canberra', 'binary' ou 'minkowski'. Verifique!")
  
  METHODS <- c("complete", "ward.D", "ward.D2", "single", "average", "mcquitty", "median" , "centroid")
  if (is.na(pmatch(Metodo, METHODS))) 
     stop("Entrada para 'Metodo' esta incorreta, deve ser: 'complete', 'ward.D', 
          'ward.D2', 'single', 'average', 'mcquitty', 'median' ou 'centroid'. Verifique!")
  
  if (!is.logical(Horizontal)) 
     stop("Entrada para 'Horizontal' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (is.na(NumGrupos)) NumGrupos <- 0 # numero de grupos a formar
  
  if (NumGrupos >= nrow(Data) )
     stop("Entrada para 'NumGrupos' esta elevada. Verifique!")
  
  if (NumGrupos < 0)
     stop("Entrada para 'NumGrupos' esta incorreta, deve ser numeros inteiros positivos, ou zero. Verifique!")
  
  if (!Hierarquico && Analise == "VAR")
     stop("O metodo nao hierarquico e valido apenas para as observacoes. Verifique!")
  
  if (!Hierarquico && NumGrupos < 2)
     stop("Para o metodo nao hierarquico se faz necessario NumGrupo > 1. Verifique!")

  DataNew <- Data # dados a serem analizados
  
  if (Normaliza && Analise == "OBS")
     DataNew <- NormData(DataNew, 2) # normaliza por colunas os dados

  ### INICIO - Agrupamentos hierarquicos ###
  if (Hierarquico) {
     
     if (Analise == "OBS") # analise nas observacoes
         Md <- dist(DataNew, method = Distance) # matrix das distancias
     
     if (Analise == "VAR") {# analise nas variaveis
        if (CorAbs) # matrix de correlacao absoluta
            Md <- as.dist(1 - abs(cor(Data))) # matrix das distancias
        
        if (!CorAbs) # matrix de correlacao
            Md <- as.dist(1 - cor(Data)) # matrix das distancias
     }
     
     hc <- hclust(Md, method = Metodo) # procedimento hierarquico
     
     Grupos <- 0
     if (NumGrupos!=0) 
        Grupos <- cutree(hc, k = NumGrupos) # grupos formados

     if (Analise == "OBS") # novos grupos para as observacoes
        MGrupos  <- cbind(Data, Grupos) # matriz com dados originais mais os grupos formados
     
     if (Analise == "VAR") {# novos grupos para as variaveis
        MGrupos <- cbind(colnames(Data), Grupos) # matriz com dados originais mais os grupos formados
        colnames(MGrupos) <- c("Variaveis","Grupos")
        rownames(MGrupos) <- NULL
     }
     
     ## INICIO - Tabelas com as Similaridade e as Distancias ##
     if (NumGrupos == 0) Distancia <- hc$height else # Distancias dos agrupamentos
        Distancia <- hc$height[(length(hc$height) - NumGrupos):length(hc$height)]

     Sim <- (1 - Distancia/max(Md)) # calculo das similaridades
     
     Passos <- 1:length(Sim)
     SimGrupos <- length(Sim):1
     Similaridade <- Sim*100
     Tab <- cbind(Passos, SimGrupos, round(Similaridade,3), round(Distancia,2))
     colnames(Tab) <- c("Passos","Grupos","Similaridade","Distancia")
     ## FIM - Tabela com as similirades e distancias ##
     
     ## INICIO - Screen plots ##
     if (Analise == "OBS") {
        plot(length(Sim):1, 1/Sim, type="b", xlab="Numero de Agrupamentos", ylab="Similaridade dentro dos grupos")
        abline(v=NumGrupos, cex = 1.5, lty = 2) # cria o eixo no agrupamento desejado
         
        plot(length(Distancia):1, Distancia, type="b", xlab="Numero de agrupamentos", ylab="Distancias dentro dos grupos")
        abline(v=NumGrupos, cex = 1.5, lty = 2) # cria o eixo no agrupamento desejado
     }
     ## FIM - Screen plots ##
 
     ## INICIO - Plotagem do Dendograma ##
     Dendo <- as.dendrogram(hc)
     plot(Dendo, # cordenadas para plotar
          ylab   = "Distancia",  # Nomeia Eixo Y
          main   = "Dendograma", # Titulo
          center = TRUE,       # centraliza o grafico
          horiz  = Horizontal, # posicao do grafico
          cex    = 1) # Tamanho dos pontos
     
     if (NumGrupos > 1 && !Horizontal) 
        rect.hclust(hc, k = NumGrupos, border = "red") # coloca retangulos nos agrupamentos de acordo com NumGrupos
     ## FIM - Plotagem do Dendograma ##
  }
  ### FIM - Agrupamentos hierarquicos ###

  ### INICIO - Metodo K-Means ###
  if (!Hierarquico && Analise=="OBS") {
    
     set.seed(7) # semente para fixar processo heuristico
      
     hc <- kmeans(DataNew, NumGrupos, iter.max = 100) # executa o metodo K-Means
     #,iter.max = 100, nstart = 21, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")) # cria particoes pelo metodo K-means
      
     #fitted(hc, method = c("centers", "classes"))
     Grupos <- hc$cluster
     
     MGrupos  <- cbind(Data, Grupos) # matriz com dados originais mais os grupos formados
     
     Tab <- NA # tabelas com as similiridades e distancias
     Md  <- NA # matrix das distancias
  }
  ### FIM - Metodo K-Means ###
  
  ### INICIO - Analises dos grupos ###
  Sqt <- NA # soma do quadrado total 
  TabResGrupos = NA # tabela com os resultados dos grupos
  if (Analise == "OBS" && NumGrupos > 1) {
     TabResGrupos <- NULL
     MGr <- cbind(DataNew, Grupos) # matriz com dados originais mais os grupos formados
     for (i in 1:NumGrupos) { 
        NewGroups <- subset(MGr, Grupos == i) 
        GrupCalc <- NewGroups[,1:(ncol(NewGroups)-1)]
        Qtd.Elementos <- nrow(NewGroups)
        
        if (Qtd.Elementos==1) Media <- GrupCalc else
           Media <- apply(GrupCalc, 2, mean)
        
        if (Qtd.Elementos==1) SqG <- 0 else # soma dos quadrados dos grupos
           SqG <- sum(sweep(GrupCalc,2, Media)^2) # soma dos quadrados dos grupos
        
        TabResGrupos <- rbind(TabResGrupos,c(i,Qtd.Elementos,SqG,Media))
     }
     colnames(TabResGrupos) <- c("Grupos","Qtd.Elementos","Soma Quadrados",paste("Media",colnames(TabResGrupos[,4:(ncol(TabResGrupos))])))
    
     Sqt <- sum(sweep(DataNew,2,apply(DataNew, 2, mean))^2) # soma do quadrado total 
  }
  ### FIM - Analises dos grupos ###
  
  Lista <- list(TabRes = Tab, Groups = MGrupos, 
                ResGroups = TabResGrupos, SQT = Sqt,
                MatrixD = Md)
  
  return(Lista)
}