Plot.Regressao <- function(Reg, TypeGraf = "Scatterplot", Title = NULL,
                           NameVarY = NULL, NameVarX = NULL, LabelX = NULL, 
                           LabelY = NULL, Color = "s", IntConf = "s", IntPrev = "s") {
  # Esta funcao gera graficos da Analise de Regressao
  # desenvolvida por Paulo Cesar Ossani em 06/2016
    
  # Entrada:
  # Reg      - Dados da funcao de regressao.
  # TypeGraf - Tipo de grafico: 
  #            "Scatterplot" - Grafico de dispersao 2 a 2,
  #            "Regression"  - Grafico da regressao linear,
  #            "QQPlot"      - Grafico de probabilidade normal dos residuos,
  #            "Histogram"   - Histograma dos residuos,
  #            "Fits"        - Grafico dos valores ajustados versus os residuos,
  #            "Order"       - Grafico da ordem das observacoes versus os residuos.
  # Title    - Titulos para os graficos, se nulo retorna padrao.
  # NameVarY - Nome da variavel Y, se nulo retorna padrao.
  # NameVarX - Nome da variavel, ou variaveis X, se nulo retorna padrao.
  # LabelX   - Nomeia o eixo X, se nulo retorna padrao.
  # LabelY   - Nomeia o eixo Y, se nulo retorna padrao.
  # Color    - "s" para graficos coloridos (default),
  #            "n" para graficos em preto e branco.
  # IntConf  - Caso TypeGraf = "Regression":
  #           "s" para graficos com intervalo de confianca (default),
  #           "n" para graficos sem intervalo de confianca. 
  # IntPrev  - Caso TypeGraf = "Regression":
  #            "s" para graficos com intervalo de previsao (default),
  #            "n" para graficos sem intervalo de previsao. 
  
  # Retorna:
  # Varios graficos.

  Graphic <- c("Scatterplot", "Regression", "QQPlot", "Histogram", "Fits", "Order")
  if (is.na(pmatch(TypeGraf, Graphic))) 
     stop("Entrada para 'TypeGraf' esta incorreta, deve ser: 'Scatterplot', 
          'Regression', 'QQPlot', 'Histogram', 'Fits' ou 'Order'. Verifique!")

  Color  = ifelse(Color=="s","S",ifelse(Color=="n","N",Color))    # transforma em maiusculo
  
  if (Color!="S" && Color!="N")
     stop("Entrada para 'Color' esta incorreta, deve ser do tipo caracter, sendo 's' ou 'n'. Verifique!")
  
  IntConf  = ifelse(IntConf=="s","S",ifelse(IntConf=="n","N",IntConf))    # transforma em maiusculo
  
  if (IntConf!="S" && IntConf!="N")
     stop("Entrada para 'IntConf' esta incorreta, deve ser do tipo caracter, sendo 's' ou 'n'. Verifique!")
  
  IntPrev  = ifelse(IntPrev=="s","S",ifelse(IntPrev=="n","N",IntPrev))    # transforma em maiusculo
  
  if (IntPrev!="S" && IntPrev!="N")
     stop("Entrada para 'IntPrev' esta incorreta, deve ser do tipo caracter, sendo 's' ou 'n'. Verifique!")
 
  if (Reg$Intercepts=="S") X <- as.matrix(Reg$X[,2:ncol(Reg$X)]) else X <- as.matrix(Reg$X)
  
  if (is.null(NameVarY))
     NameVarY <- c("Y")
  
  if (is.null(NameVarX))
     NameVarX <- c(paste("X",1:ncol(X),sep=""))
  
  ## Inicio - Scatterplot
  if (TypeGraf == "Scatterplot") {
    
     if (is.null(Title))
        Title = c("Grafico de Dispersao 2 a 2")
     
     Dat <- as.data.frame(cbind(Reg$Y,X))
     colnames(Dat) <- c(NameVarY,NameVarX)
     if (Color=="S") cor <- c(2:(ncol(X)+1)) else cor <- c(rep("black", ncol(X)))
     pairs(Dat, # Scatterplot
           main = Title, # Titulo
           pch = 21,     # Formato dos pontos 
           cex=1,        # Tamanho dos pontos
           bg = cor)
  }
  ## Fim - Scatterplot
  
  ## Inicio - Grafico da regressao
  if (TypeGraf == "Regression") {
     if (ncol(X)!=1) 
        print("Atencao! O Grafico da regressao so he possivel apenas para uma variavel regressora.")
    
     if (ncol(X)==1) { # para calculos de regressao simples
        
        if (is.null(LabelX))
           LabelX = "Eixo x"  # Nomeia Eixo X  
       
        if (is.null(LabelY))
           LabelY = "Eixo y"  # Nomeia Eixo Y
        
        if (is.null(Title))
           Title = c("Grafico da regressao linear")
        
        plot(X,Reg$Y, # cria grafico
            xlab = LabelX,  # Nomeia Eixo X
            ylab = LabelY,  # Nomeia Eixo Y
            main = Title,   # Titulo
            pch = 15,       # Formato dos pontos 
            cex=1,          # Tamanho dos pontos
            xlim=c(min(X)-0.1,max(X)+0.1), # Dimensao para as linhas do grafico
            ylim=c(min(cbind(Reg$Y,Reg$ICp[,4:5],Reg$IPp[,4:5])),max(cbind(Reg$Y,Reg$ICp[,4:5],Reg$IPp[,4:5]))+0.1), # Dimensao para as colunas do grafico
            col = ifelse(Color=="S","red","black"))             # Cor dos pontos
        
        ## Inicio - Acrescenta a reta ajustada
        if (Reg$Intercepts=="N") # Sem intersepto
           abline(0,Reg$Betas[1], lty=1) # acrescenta a reta ajustada
        
        if (Reg$Intercepts=="S") # Com intersepto 
           abline(Reg$Betas[1],Reg$Betas[2], lty=1) # acrescenta a reta ajustada
        ## Fim - Acrescenta a reta ajustada
        
        ## Inicio - Acrescenta o Intervalo de Confianca das previsoes
        if (IntConf == "S") {
           lines(sort(X),sort(Reg$ICp[,4]), lty=3) # acrescenta I.C. Lim.Infereior
           lines(sort(X),sort(Reg$ICp[,5]), lty=3) # acrescenta I.C. Lim.Superior
        }
        ## Fim - Acrescenta o Intervalo de Confianca das previsoes
        
        ## Inicio - Acrescenta o Intervalo das previsoes
        if (IntPrev == "S") {
           lines(sort(X),sort(Reg$IPp[,4]), lty=1) # acrescenta I.C. Lim.Infereior
           lines(sort(X),sort(Reg$IPp[,5]), lty=1) # acrescenta I.C. Lim.Superior
        }
        ## Fim - Acrescenta o Intervalo das previsoes
     }
  }
  ## Fim - Grafico da regressao
  
  ## Inicio - Grafico da probalidade normal
  if (TypeGraf == "QQPlot") {
    
     if (is.null(LabelX))
        LabelX = "Quantis"  # Nomeia Eixo X  
    
     if (is.null(LabelY))
        LabelY = "Amostra nos quantis"  # Nomeia Eixo Y

     if (is.null(Title))
        Title = c("Grafico da probabilidade \n normal do residuo")
     
     qqnorm(Reg$Error,
            xlab = LabelX,  # Nomeia Eixo X
            ylab = LabelY,  # Nomeia Eixo Y
            main = Title,   # Titulo
            pch = 19,       # Formato dos pontos 
            cex=1)
     qqline(Reg$Error, col = ifelse(Color=="S","red","black"))
  }
  ## Fim - Grafico da probalidade normal
  
  ## Inicio - Grafico da probalidade normal
  if (TypeGraf == "Histogram") {
    
     if (is.null(LabelX))
        LabelX = "Residuo"  # Nomeia Eixo X  
    
     if (is.null(LabelY))
        LabelY = "Frequencia"  # Nomeia Eixo Y
    
     if (is.null(Title))
        Title = c("Histograma do residuo")
     
     hist(Reg$Error,
          xlab = LabelX,  # Nomeia Eixo X
          ylab = LabelY,  # Nomeia Eixo Y
          main = Title,   # Titulo
          pch = 19,       # Formato dos pontos 
          cex=1)
  }
  ## Fim - Grafico da probalidade normal
  
  ## Inicio - Grafico dos valores ajustados com os residuos
  if (TypeGraf == "Fits") {
    
    if (is.null(LabelX))
      LabelX = "Valores ajustados"  # Nomeia Eixo X  
    
    if (is.null(LabelY))
      LabelY = "Residuos"  # Nomeia Eixo Y
    
    if (is.null(Title))
      Title = c("Valores ajustados vs. Residuos")
    
    plot(Reg$Prev,Reg$Error, # cria grafico
         xlab = LabelX,  # Nomeia Eixo X
         ylab = LabelY,  # Nomeia Eixo Y
         main = Title,   # Titulo
         pch = 19,       # Formato dos pontos 
         cex=1,          # Tamanho dos pontos
         col = ifelse(Color=="S","red","black"))             # Cor dos pontos
    
    abline(0,0, lty = 2) # acrescenta a reta do eixo X
  }
  ## Fim - Grafico dos valores ajustados com os residuos
  
  ## Inicio - Grafico com ordem das observacoes versus os residuos
  if (TypeGraf == "Order") {
    
    if (is.null(LabelX))
       LabelX = "Ordem das observacoes"  # Nomeia Eixo X  
    
    if (is.null(LabelY))
       LabelY = "Residuos"  # Nomeia Eixo Y
    
    if (is.null(Title))
       Title = c("Ordem das observacoes vs. Residuos")
    
    plot(1:length(Reg$Error),Reg$Error, # cria grafico
         xlab = LabelX,  # Nomeia Eixo X
         ylab = LabelY,  # Nomeia Eixo Y
         main = Title,   # Titulo
         type = "o",     # linhas com pontos
         pch = 19,       # Formato dos pontos 
         cex=1,          # Tamanho dos pontos
         col = ifelse(Color=="S","blue","black"))  # Cor dos pontos
    
    abline(0,0, lty = 2) # acrescenta a reta do eixo X
  }
  ## Fim - Grafico com as ordem das observacoes versus os residuos

}