Cluster <- function(data, titles = NA, hierarquico = TRUE, analise = "Obs",  
                    corabs = FALSE, normaliza = FALSE, distance = "euclidean",  
                    method = "complete", horizontal = FALSE, numgrupos = 0,
                    lambda = 2, savptc = FALSE, width = 3236, height = 2000, 
                    res = 300, casc = TRUE) {
  
  # Esta funcao executa a analise de Agrupamentos hierarquicos e
  # Nao-hierarquicos, desenvolvida por Paulo Cesar Ossani em 07/2016
  
  # Entrada:
  # data - Dados a serem a analizados
  # titles - Titulos para os graficos.
  # hierarquico - Agrupamentos hierarquicos (default = TRUE), 
  #               para agrupamentos nao hierarquicos (method K-Means), 
  #               somente para caso analise = "Obs".
  # analise - "Obs" para analises nas observacoes (default),
  #           "Var" para analises nas variaveis.
  # corabs  - Matriz de correlacao absoluta caso analise = "Var" (default = FALSE).
  # normaliza - normalizar os dados somente para caso analise = "Obs" (default = TRUE).
  # distance - Metrica das distancias caso agrupamentos hierarquicos:
  #            "euclidean" (default), "maximum", "manhattan",
  #            "canberra", "binary" ou "minkowski". Caso analise = "Var" a metrica
  #            sera a matriz de correlacao, conforme corabs.
  # method - Metodo para analises caso agrupamentos hierarquicos:
  #          "complete" (default), "ward.D", "ward.D2", "single",
  #          "average", "mcquitty", "median" ou "centroid".
  # horizontal - Dendrograma na horizontal (default = FALSE).
  # numgrupos - Numero de grupos a formar.
  # lambda    - Valor usado na distancia de minkowski.
  # savptc   - Salva as imagens dos graficos em arquivos (default = FALSE).
  # width    - Largura do grafico quanto savptc = TRUE (defaul = 3236).
  # height   - Altura do grafico quanto savptc = TRUE (default = 2000).
  # res      - Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).
  # casc      - Efeito cascata na apresentacao dos graficos (default = TRUE).
  
  # Retorna:
  # Varios graficos.
  # tabres - Tabela com as similaridades e distancias dos grupos formados.
  # groups - Dados originais com os grupos formados.
  # resgroups - Resultados dos grupos formados.
  # sqt - Soma do quadrado total.
  # mtxD - Matriz das distancias.
  
  if (!is.data.frame(data)) 
     stop("Entrada 'data' esta incorreta, deve ser do tipo dataframe. Verifique!")

  if (!is.logical(hierarquico)) 
     stop("Entrada para 'hierarquico' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  analise <- toupper(analise)
  
  if (analise != "OBS" && analise != "VAR") 
     stop("Entrada para 'analise' esta incorreta, deve ser 'Obs' para as observacoes ou 'Var' para as variaveis. Verifique!")

  if (!is.logical(corabs)) 
     stop("Entrada para 'corabs' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(normaliza)) 
     stop("Entrada para 'normaliza' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  distance <- tolower(distance) # torna minusculo
  
  Distances <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")
  #if (is.na(pmatch(distance, distance)))
  if (!(distance %in% Distances))
     stop("Entrada para 'distance' esta incorreta, deve ser: 'euclidean', 
          'maximum', 'manhattan', 'canberra', 'binary' ou 'minkowski'. Verifique!")
  
  Methods <- c("complete", "ward.D", "ward.D2", "single", "average", "mcquitty", "median" , "centroid")
  # if (is.na(pmatch(method, methodS)))
  if (!(method %in% Methods)) 
     stop("Entrada para 'method' esta incorreta, deve ser: 'complete', 'ward.D', 
          'ward.D2', 'single', 'average', 'mcquitty', 'median' ou 'centroid'. Verifique!")
  
  if (!is.logical(horizontal)) 
     stop("Entrada para 'horizontal' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (is.na(numgrupos)) numgrupos <- 0 # numero de grupos a formar
  
  if (numgrupos >= nrow(data) )
     stop("Entrada para 'numgrupos' esta elevada. Verifique!")
  
  if (numgrupos < 0)
     stop("Entrada para 'numgrupos' esta incorreta, deve ser numeros inteiros positivos, ou zero. Verifique!")
  
  if (!hierarquico && analise == "VAR")
     stop("O method nao hierarquico e valido apenas para as observacoes. Verifique!")
  
  if (!hierarquico && numgrupos < 2)
     stop("Para o method nao hierarquico se faz necessario NumGrupo > 1. Verifique!")

  if (!is.numeric(lambda) || lambda <= 0)
     stop("Entrada para 'lambda' esta incorreta, e necessario lambda > 0. Verifique!")
  
  if (!is.logical(savptc))
     stop("Entrada para 'savptc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.numeric(width) || width <= 0)
     stop("Entrada para 'width' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.numeric(height) || height <= 0)
     stop("Entrada para 'height' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.numeric(res) || res <= 0)
     stop("Entrada para 'res' esta incorreta, deve ser numerica e maior que zero. Verifique!")
  
  if (!is.logical(casc && !savptc))
     stop("Entrada para 'casc' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  dataNew <- data # dados a serem analizados
  
  if (normaliza && analise == "OBS")
     dataNew <- NormData(dataNew, 2) # normaliza por colunas os dados

  # Cria Titulos para os graficos caso nao existam
  if (!is.character(titles[1]) || is.na(titles[1])) titles[1] = c("Grafico da similaridade\n dentro dos grupos")
  if (!is.character(titles[2]) || is.na(titles[2])) titles[2] = c("Grafico das distancias\n dentro dos grupos")
  if (!is.character(titles[3]) || is.na(titles[3])) titles[3] = c("Dendrograma")
  
  ### INICIO - Agrupamentos hierarquicos ###
  if (hierarquico) {
    
     if (savptc) {
        cat("\014") # limpa a tela
        cat("\n\n Salvando graficos em disco. Aguarde o termino!")
     }
     
     if (analise == "OBS") # analise nas observacoes
         Md <- dist(dataNew, method = distance, p = lambda) # matrix das distancias
     
     if (analise == "VAR") {# analise nas variaveis
        if (corabs) # matrix de correlacao absoluta
            Md <- as.dist(1 - abs(cor(data))) # matrix das distancias
        
        if (!corabs) # matrix de correlacao
            Md <- as.dist(1 - cor(data)) # matrix das distancias
     }
     
     hc <- hclust(Md, method = method) # procedimento hierarquico
     
     Grupos <- 0
     if (numgrupos!=0) 
        Grupos <- cutree(hc, k = numgrupos) # grupos formados

     if (analise == "OBS") # novos grupos para as observacoes
        MGrupos  <- cbind(data, Grupos) # matriz com dados originais mais os grupos formados
     
     if (analise == "VAR") {# novos grupos para as variaveis
        MGrupos <- cbind(colnames(data), Grupos) # matriz com dados originais mais os grupos formados
        colnames(MGrupos) <- c("Variaveis","Grupos")
        rownames(MGrupos) <- NULL
     }
     
     ## INICIO - Tabelas com as Similaridade e as Distancias ##
     if (numgrupos == 0) Distancia <- hc$height else # Distancias dos agrupamentos
        Distancia <- hc$height[(length(hc$height) - numgrupos):length(hc$height)]

     Sim <- (1 - Distancia/max(Md)) # calculo das similaridades
     
     Passos <- 1:length(Sim)
     SimGrupos <- length(Sim):1
     Similaridade <- Sim*100
     Tab <- cbind(Passos, SimGrupos, round(Similaridade,3), round(Distancia,2))
     colnames(Tab) <- c("Passos","Grupos","Similaridade","Distancia")
     ## FIM - Tabela com as similirades e distancias ##
     
     ## INICIO - Screen plots ##
     if (analise == "OBS") {
        
        if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
       
        if (savptc) {
           name.figure = paste("Figure Cluster Screen plots 1 - distance ",distance," - method ", method,".png",sep="")
           png(filename = name.figure, width = width, height = height, res = res) # salva os graficos em arquivos
        }
       
        plot(length(Sim):1, 1/Sim, 
             type = "b", 
             xlab = "Numero de agrupamentos", 
             ylab = "Similaridade dentro dos grupos",
             main = titles[1]) # Titulo
        
        abline(v=numgrupos, cex = 1.5, lty = 2) # cria o eixo no agrupamento desejado
        
        if (savptc) dev.off() 
        
        if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
        
        if (savptc) {
           name.figure = paste("Figure Cluster Screen plots 2 - distance ",distance," - method ", method,".png",sep="")
           png(filename = name.figure, width = width, height = height, res = res) # salva os graficos em arquivos
        }
        
        plot(length(Distancia):1, Distancia, 
             type ="b", 
             xlab ="Numero de agrupamentos", 
             ylab ="Distancias dentro dos grupos",
             main = titles[2]) # Titulo
             
        abline(v=numgrupos, cex = 1.5, lty = 2) # cria o eixo no agrupamento desejado
        
        if (savptc) dev.off() 
     }
     ## FIM - Screen plots ##
 
     if (casc && !savptc) dev.new() # efeito cascata na apresentacao dos graficos
     
     ## INICIO - Plotagem do Dendrograma ##
     if (savptc) {
        name.figure = paste("Figure Cluster Dendrogram - distance ",distance," - method ", method,".png",sep="")
        png(filename = name.figure, width = width, height = height, res = res) # salva os graficos em arquivos
     }
     
     Dendo <- as.dendrogram(hc)
     plot(Dendo, # cordenadas para plotar
          ylab   = "Distancia",  # Nomeia Eixo Y
          main   = titles[3],    # Titulo
          center = TRUE,         # centraliza o grafico
          horiz  = horizontal,   # posicao do grafico
          cex    = 1) # Tamanho dos pontos
     
     if (numgrupos > 1 && !horizontal) 
        rect.hclust(hc, k = numgrupos, border = "red") # coloca retangulos nos agrupamentos de acordo com numgrupos
     
     if (savptc) { 
        dev.off() 
        cat("\n \n Fim!")
     }
     ## FIM - Plotagem do Dendrograma ##
  }
  ### FIM - Agrupamentos hierarquicos ###

  ### INICIO - method K-Means ###
  if (!hierarquico && analise=="OBS") {
    
     set.seed(7) # semente para fixar processo heuristico
      
     hc <- kmeans(dataNew, numgrupos, iter.max = 100) # executa o method K-Means
     #,iter.max = 100, nstart = 21, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")) # cria particoes pelo method K-means
      
     #fitted(hc, method = c("centers", "classes"))
     Grupos <- hc$cluster
     
     MGrupos  <- cbind(data, Grupos) # matriz com dados originais mais os grupos formados
     
     Tab <- NA # tabelas com as similiridades e distancias
     Md  <- NA # matrix das distancias
  }
  ### FIM - method K-Means ###
  
  ### INICIO - analises dos grupos ###
  sqt <- NA # soma do quadrado total 
  tabresGrupos = NA # tabela com os resultados dos grupos
  if (analise == "OBS" && numgrupos > 1) {
     tabresGrupos <- NULL
     MGr <- cbind(dataNew, Grupos) # matriz com dados originais mais os grupos formados
     for (i in 1:numgrupos) { 
        Newgroups <- subset(MGr, Grupos == i) 
        GrupCalc <- Newgroups[,1:(ncol(Newgroups)-1)]
        Qtd.Elementos <- nrow(Newgroups)
        
        if (Qtd.Elementos==1) Media <- GrupCalc else
           Media <- apply(GrupCalc, 2, mean)
        
        if (Qtd.Elementos==1) SqG <- 0 else # soma dos quadrados dos grupos
           SqG <- sum(sweep(GrupCalc,2, Media)^2) # soma dos quadrados dos grupos
        
        tabresGrupos <- rbind(tabresGrupos,c(i,Qtd.Elementos,SqG,Media))
     }
     colnames(tabresGrupos) <- c("Grupos","Qtd.Elementos","Soma Quadrados",paste("Media",colnames(tabresGrupos[,4:(ncol(tabresGrupos))])))
    
     sqt <- sum(sweep(dataNew,2,apply(dataNew, 2, mean))^2) # soma do quadrado total 
  }
  ### FIM - analises dos grupos ###
  
  Lista <- list(tabres = Tab, groups = MGrupos, 
                resgroups = tabresGrupos, sqt = sqt,
                mtxD = Md)
  
  return(Lista)
}