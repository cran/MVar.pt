FA <- function(data, method = "PC", type = 2, nfactor = 1, 
               rotation = "None", scoresobs = "Bartlett", 
               converg = 1e-5, iteracao = 1000, testfit = TRUE) {
   # Funcao executa a Analise Fatorial.
   # Desenvolvida por Paulo Cesar Ossani em 22/06/2013 e adapitada em 25/03/2016
   
   # Entrada:
   # data      - Dados a serem analisados
   # method    - Tipo de analises:
   #             Componentes Principais - PC (Principal Components) (default)
   #             Fator Principal - PF (Principal Factor)
   #             Maxima Verossimilhanca - ML (Maximum Likelihood)
   # type      - 1 para analise utilizando a matriz de covariancia
   #             2 para analise utilizando a matriz de correlacao - default
   # rotation  - Tipo de rotacao: "None" (default) e "Varimax" 
   # nfactor   - Numero de fatores (default = 1)
   # scoresobs - Tipo de scores para as observacoes: "Bartlett" (default) ou "Regression"
   # converg   - Valor limite para convergencia para soma do quadrado dos residuos para metodo de Maxima Verossimilhanca (default = 1e-5)
   # iteracao  - Numero maximo de iteracoes para metodo de Maxima Verossimilhanca (default = 1000)
   # testfit   - Testa o ajuste do modelo para o metodo de Maxima Verossimilhanca (default = TRUE)
  
   # Saida:
   # mtxMC     - Matriz de Correlacao/Covariancia
   # mtxAutvlr - Matriz de autovalores
   # mtxAutvec - Matriz de autovetores
   # mtxvar    - Matriz de variancias e proporcoes
   # mtxcarga  - Matriz de cargas fatoriais
   # mtxvaresp - Matriz das variancias especificas
   # mtxcomuna - Matriz das comunalidades
   # mtxresidue - Matriz dos residuos
   # vlrsqrs   - Valor limite superior para a soma do quadrados dos residuos
   # vlrsqr    - Soma dos Quadrados dos residuos
   # mtxresult - Matriz com todos os resultados associados
   # mtxscores - Matriz com os escores das observarcoes

   method <- toupper(method)   # transforma em maiusculo
   
   if (!is.data.frame(data) && !is.matrix(data)) 
      stop("Entrada 'data' esta incorreta, deve ser do tipo dataframe ou matriz. Verifique!")
  
   if (!(method %in% c("PC", "PF", "ML"))) 
      stop("Entrada 'method' esta incorreta, deve ser 'PC', 'PF' ou 'ML'. Verifique!")
  
   if (type != 1 && type != 2) 
      stop("Entrada para 'type' esta incorreta, deve ser numerica, sendo 1 ou 2. Verifique!")
  
   if (!is.numeric(nfactor)) 
      stop("Entrada para 'nfactor' esta incorreta, deve ser numerica. Verifique!")

   if (nfactor > ncol(data)) 
      stop("Entrada para 'nfactor' esta incorreta, deve ser igual ou inferior ao numero de variaveis em 'data'. Verifique!")
 
   if (nfactor <= 0) 
      stop("Entrada para 'nfactor' esta incorreta, deve ser numero inteiro maior ou igual a 1. Verifique!")
 
   rotation <- toupper(rotation) # transforma em maiusculo
   
   if (!(rotation %in% c("NONE","VARIMAX", "PROMAX")))
      stop("Entrada para 'rotation' esta incorreta, deve ser 'None', 'Varimax' or 'Promax'. Verifique!")
  
   if (rotation != "NONE" && nfactor < 2)
      stop("Para a rotacao, he necessario mais do que um fator. Altere o numero de fatores (nfactor) para continuar.")

   scoresobs <- toupper(scoresobs) # transforma em maiusculo
   
   if (!(scoresobs %in% c("BARTLETT", "REGresSION")))
      stop("Entrada para 'scoresobs' esta incorreta, deve ser 'Bartlett' ou 'Regression'. Verifique!")
   
   if (!is.logical(testfit) && method == "ML")
      stop("Entrada para 'testfit' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
       
   if (type == 2) data <- scale(data) # normaliza os dados
   
   mc <- cov(data) # Matriz de Covariancia

   num.comp <- min(dim(data)) # numero de componentes
   
   Rotacao <- function(Mdata, type = NULL) {
   # Funcao que executa as rotacoes
     if (type == "VARIMAX") {
        Var <- varimax(Mdata, normalize = TRUE)
        res <- Var$loadings[,]
     }
     
     if (type == "PROMAX") {
         Var <- promax(Mdata, m = 4)
         res <- Var$loadings[,]
     }
      
     return(res)
   }
   
   if (method == "PC") { # Metodo dos Componentes Principais
      
      # Encontrando a Matriz de Decomposicao Expectral
      mav <- svd(mc) # Encontra a matriz de autovalor e autovetor
      MAutoVlr <- mav$d[1:num.comp]  # Matriz de Autovalores 
      MAutoVec <- mav$v # Matriz de Autovetores

      gama = MAutoVec%*%diag(sqrt(abs(MAutoVlr)),nrow(mc),ncol(mc)) # Matriz de Cargas Fatoriais
      if (rotation != "NONE") {
         gama <- Rotacao(gama[,1:nfactor], rotation)
      }
      rownames(gama) <- colnames(data)
      colnames(gama) <- paste("Fator",1:ncol(gama))
      
      psi = diag(mc - gama[,1:nfactor] %*% t(gama[,1:nfactor])) # Matriz de Variancias Especificas
      
      comun = diag(mc - psi) # Matriz de comunalidades
 
      # Valor Limite Superior para a Soma de Quadrados de residuos
      SQRS = MAutoVlr[(nfactor+1):nrow(as.matrix(MAutoVlr))]%*%(MAutoVlr[(nfactor+1):nrow(as.matrix(MAutoVlr))])
     
      M = mc - (gama[,1:nfactor] %*% t(gama[,1:nfactor]) + diag(psi)) # Matriz dos residuos 
      
      SQR = sum(diag(M%*%t(M))) # Soma dos Quadrados dos residuos
      
      # Matriz das Variancias
      MEigen <- as.data.frame(matrix(NA, length(MAutoVlr), 3))
      rownames(MEigen) <- paste("Fator", 1:length(MAutoVlr))
      colnames(MEigen) <- c("Autovalor", "% da variancia","% acumulada da variancia")
      MEigen[, "Autovalor"] <- MAutoVlr
      MEigen[, "% da variancia"] <- (MAutoVlr/sum(MAutoVlr)) * 100
      MEigen[, "% acumulada da variancia"] <- cumsum(MEigen[,"% da variancia"]) 
      
      # Matriz com todos os resultados associados
      result <- as.matrix(cbind(gama[,1:nfactor],comun,psi))
      result <- rbind(result,t(rbind(as.matrix(MEigen[1:nfactor,1]),sum(comun),NA)))
      result <- rbind(result,t(rbind(as.matrix(MEigen[1:nfactor,2]),MEigen[nfactor,3],NA)))
      colnames(result) <- c(paste("Carga Fator",1:nfactor),"comunalidade","Variancias especificas")
      rownames(result) <- c(colnames(data),"Variancia","% Variancia")
      
   }
    
   if (method == "PF" && det(mc) == 0) { # Metodo dos Fatores Principais
      stop("\n\nA matriz de covariancia ou correlacao nao e positiva definida. Portanto nao sendo possivel aplicar este metodo. Sugere-se utilizar o metodo 'PC' ou  'ML'\n\n")
   }
   
   if (method == "PF" && det(mc) != 0) { # Metodo dos Fatores Principais
     
      psi0 <- (solve(diag(diag(solve(mc))))) # Encontrando a Matriz psi

      Sr <- mc - psi0 # Encontrando a Matriz Sr
      
      mav <- svd(Sr) # Encontra a matriz de autovalor e autovetor
      MAutoVlr <- mav$d[1:num.comp]  # Matriz de Autovalores 
      MAutoVec <- mav$v  # Matriz de Autovetores
      
      gama = MAutoVec%*%diag(sqrt(abs(MAutoVlr)),nrow(mc),ncol(mc)) # Matriz de Cargas Fatoriais
      if (rotation != "NONE") {
         gama <- Rotacao(gama[,1:nfactor], rotation)
      }
      rownames(gama) <- colnames(data)
      colnames(gama) <- paste("Fator",1:ncol(gama))
      
      psi = diag(mc - gama[,1:nfactor] %*% t(gama[,1:nfactor])) # Matriz de Variancias Especificas
     
      comun = diag(mc - psi) # Matriz de comunalidades
      
      ## Valor Limite Superior para a Soma de Quadrados de residuos
      SQRS = MAutoVlr[(nfactor+1):nrow(as.matrix(MAutoVlr))]%*%(MAutoVlr[(nfactor+1):nrow(as.matrix(MAutoVlr))])
   
      # Soma dos Quadrados dos residuos
      M = mc - (gama[,1:nfactor] %*% t(gama[,1:nfactor]) + diag(psi))
      SQR = sum(diag(M%*%t(M)))

      # Matriz das Variancias
      MEigen <- as.data.frame(matrix(NA, length(MAutoVlr), 3))
      rownames(MEigen) <- paste("Comp", 1:length(MAutoVlr))
      colnames(MEigen) <- c("Autovalor", "% da variancia","% acumulada da variancia")
      MEigen[, "Autovalor"] <- MAutoVlr
      MEigen[, "% da variancia"] <- (MAutoVlr/sum(MAutoVlr)) * 100
      MEigen[, "% acumulada da variancia"] <- cumsum(MEigen[,"% da variancia"])
      
      # Matriz com todos os resultados associados
      result <- as.matrix(cbind(gama[,1:nfactor],comun,psi))
      result <- rbind(result,t(rbind(as.matrix(MEigen[1:nfactor,1]),sum(comun),NA)))
      result <- rbind(result,t(rbind(as.matrix(MEigen[1:nfactor,2]),MEigen[nfactor,3],NA)))
      colnames(result) <- c(paste("Carga Fator",1:nfactor),"comunalidade","Variancias especificas")
      rownames(result) <- c(colnames(data),"Variancia","% Variancia")
   }
   
   if (method == "ML") { # Metodo de maxima verossimilhanca
   
      n  <- ncol(data)*nrow(data) # numero de elementos amostrais
      mc <- (n-ncol(data))/n * mc  # Matriz de Covariancia/Correlacao Maximizada para o teste

      mav <- svd(mc) # Encontra a matriz de autovalor e autovetor
      MAutoVlr <- mav$d  # Matriz de Autovalores 
      MAutoVec <- mav$v # Matriz de Autovetores

      gama = MAutoVec%*%diag(sqrt(abs(MAutoVlr)),nrow(mc),ncol(mc)) # Matriz de Cargas Fatoriais para Inicializacao da iteracao

      psi = (diag(mc - gama[,1:nfactor] %*% t(gama[,1:nfactor]))) # Matriz das Variancias Especificas
   
      M = mc - (gama[,1:nfactor] %*% t(gama[,1:nfactor]) + diag(psi)) # Matriz dos residuos
      
      SQRi = sum(diag(M%*%t(M))) # Soma dos Quadrados dos residuos

      ### INICIO DA iteracao ###
      i = 1 # inicializa o contador de iteracoes
      while (1) {
         mc_new = diag(1/sqrt(psi))%*%(mc - diag(psi))%*% diag(1/sqrt(psi)) # nova matriz para iteracao
   
         # Encontrando a Matriz de Decomposicao Expectral
         mav <- eigen(mc_new) # Encontra a matriz de autovalor e autovetor
         MAutoVlr1 <- mav$values  # Matriz de Autovalores 
         MAutoVec1 <- mav$vectors # Matriz de Autovetores
         
         # Matriz das Cargas Fatoriais
         gama_new = diag(sqrt(psi))%*%MAutoVec1%*%diag(sqrt(abs(MAutoVlr1)),nrow(mc_new),ncol(mc_new))

         psi = (diag(mc - gama_new[,1:nfactor] %*% t(gama_new[,1:nfactor]))) # Matriz das Variancias Especificas

         # Valor Limite Superior para a Soma de Quadrados de residuos
         SQRS = MAutoVlr1[(nfactor+1):nrow(as.matrix(MAutoVlr1))]%*%(MAutoVlr[(nfactor+1):nrow(as.matrix(MAutoVlr1))])
         
         M = mc - (gama_new[,1:nfactor] %*% t(gama_new[,1:nfactor]) + diag(psi)) # Matriz dos residuos
         
         SQR = sum(diag(M%*%t(M))) # Soma dos Quadrados dos residuos
        
         if (SQR <= converg) break # sai do loop quando atingir a convergencia
       
         if (i >= iteracao) break # sai do loop apos esse limite de iteracoes
       
         i = i + 1 # incrementa o contador de iteracoes
         
      }
      ### FIM DA iteracao ###
      
      gama = gama_new # Matriz com as cargas fatoriais
  
      if (rotation != "NONE") {
         gama <- Rotacao(gama[,1:nfactor], rotation)
      }
      rownames(gama) <- colnames(data)
      colnames(gama) <- paste("Fator",1:ncol(gama))
      
      if (type == 1) {# Considera a Matriz de Covariancia para a decomposicao
         # gama <- diag(1/sqrt(diag(mc)))%*%gama[,1:nfactor] # normaliza as cargas fatoriais
         comun = rowSums(gama^2)#apply(gama,1,function(gama) gama^2)) # Matriz de comunalidades
      }
      
      if (type == 2) # Considera a Matriz de Correlacao para a decomposicao
         comun = diag(mc - psi) # Matriz de comunalidades
      
      # Matriz das Variancias
      MEigen <- as.data.frame(matrix(NA, length(MAutoVlr), 3))
      rownames(MEigen) <- paste("Comp", 1:length(MAutoVlr))
      colnames(MEigen) <- c("Autovalor", "% da variancia","% acumulada da variancia")
      MEigen[, "Autovalor"] <- MAutoVlr
      MEigen[, "% da variancia"] <- (MAutoVlr/sum(MAutoVlr)) * 100
      MEigen[, "% acumulada da variancia"] <- cumsum(MEigen[,"% da variancia"])
      
      print(paste("Numero de iteracoes:",i))
      
      # Matriz com todos os resultados associados
      result <- as.matrix(cbind(gama[,1:nfactor],comun,psi))
      result <- rbind(result,t(rbind(as.matrix(MEigen[1:nfactor,1]),sum(comun),NA)))
      result <- rbind(result,t(rbind(as.matrix(MEigen[1:nfactor,2]),MEigen[nfactor,3],NA)))
      colnames(result) <- c(paste("Carga Fator",1:nfactor),"comunalidade","Variancias especificas")
      rownames(result) <- c(colnames(data),"Variancia","% Variancia")  
      
      ### INICIO - Teste da falta de ajusto do modelo fatorial - teste Qui-quadrado ###
      if (testfit) {
         p <- nrow(gama)  # numero de parametros
      
         gl <- ((p - nfactor)^2 - nfactor - p)/2 # grau de liberdade
    
         message("### TESTE DO AJUSTE DO MODELO ###\n")
      
         message(paste("Grau de liberdade observado:", round(gl,5)),"\n")
        
         if (gl < 0) 
            message("Nao foi possivel realizar o teste de ajuste do modelo, pois grau de libertade foi negativo, aconselha-se reduzir o numero fatores, para processeguir com o teste.\n")
   
         # if (det(mc) <= 0) 
         #    message("Nao foi possivel realizar o teste de ajuste do modelo, pois o determinante da matriz de variancia/covariancia deve ser diferente de zero, para processeguir com o teste mude os parametros.\n")

         if (gl >= 0 && abs(det(mc)) > 0) {
          
            Ps_i = diag(diag(mc - gama[,1:nfactor] %*% t(gama[,1:nfactor])))
            
            Chi.Quad.Observado <- (n - 1 - (2 * p + 5) / 6 - 2 * nfactor / 3) * log(abs(det(gama[,1:nfactor] %*% t(gama[,1:nfactor]) + Ps_i)) / abs(det(mc)))
            
            qt = qchisq(0.95, gl, ncp = 0) 
         
            message(paste("Valor da estatistica do teste Qui-quadrado (Chiq1):", round(Chi.Quad.Observado,3)),"\n")
         
            message(paste("Valor Qui-quadrado observado (Chiq2) com 5% de significancia:", round(qt,3)),"\n")
        
            if (Chi.Quad.Observado<=qt) message("Como Chiq1 <= Chiq2, verifica-se que o numero de fatores FORAM suficientes.\n")
        
            if (Chi.Quad.Observado>qt) message("Como Chiq1 > Chiq2, verifica-se que o numero de fatores NAO FORAM suficientes.\n")
            
            message("Valor-p:", pchisq(Chi.Quad.Observado,gl,ncp=0, lower.tail = F))
         } 
      }
      ### FIM - Teste da falta de ajusto do modelo fatorial - teste Qui-quadrado ###
   }
   
   ### INICIO - encontrar os scores das observacoes ###
   if (type == 1)  {   # Considera a Matriz de Covariancia para os calculos
      Media  <- apply(data, 2, mean)
      dataNorm <- sweep(as.matrix(data), 2, Media, FUN = "-") # Centraliza na media por colunas
   }
   
   if (type == 2) { # Considera a Matriz de Correlacao para os calculos
      # Centraliza na media por colunas e divide pelo desvio padrao de cada coluna
      Media  <- apply(data, 2, mean) # dataNorm com as medias por colunas
      dataNorm <- sweep(data, 2, Media, FUN = "-")   # Centraliza na media
      Desvio <- sqrt(colSums(dataNorm^2)/(nrow(dataNorm)-1)) # raiz da soma do quadrado - desvio padrao amostral
      dataNorm <- sweep(dataNorm, 2, Desvio, FUN = "/")  # Divide pelo desvio padrao
   }
   
   if (scoresobs == "BARTLETT") { # Metodo Bartlett (minimos quadrados)
      # foi necessario usar a inversa generalizada pois algumas vezes a matriz he singular, assim nao tem inversa normal
      Scores <- MASS::ginv(t(gama)%*%solve(diag(psi))%*%gama)%*%(t(gama)%*%solve(diag(psi)))%*%t(dataNorm) # Matriz com os escores das observacoes
      #Scores <- solve(t(gama)%*%solve(diag(psi))%*%gama)%*%(t(gama)%*%solve(diag(psi)))%*%t(dataNorm) # Matriz com os escores das observacoes
      #Scores <- dataNorm%*%solve(mc)%*%gama # outro modo de encontrar a solucao acima
   }
   
   if (scoresobs == "REGresSION") { # Metodo de Regressao
      Media <- mean(as.matrix(data))
      dataNorm <- sweep(as.matrix(data), 2, Media, FUN = "-") # Centraliza na media geral todos os dados
      I <- diag(rep(ncol(gama)))
      Scores <- solve(I + t(gama)%*%solve(diag(psi))%*%gama)%*%(t(gama)%*%solve(diag(psi)))%*%t(dataNorm) # Matriz com os escores das observacoes
      #Scores <- t(gama)%*%solve(gama%*%t(gama)+diag(psi))%*%t(dataNorm) # outro modo de encontrar a solucao acima
   }
   Scores <- t(Scores)
   colnames(Scores) <- colnames(gama)
   rownames(Scores) <- rownames(data)
   ### FIM - encontrar os scores das observacoes ###  
 
   ### INCIO - encontra scores dos coeficientes ###
   CoefScore <- t(MASS::ginv(t(gama)%*%MASS::ginv(diag(psi))%*%gama)%*%t(gama)%*%MASS::ginv(diag(psi)))
   colnames(CoefScore) <- paste("Fator", 1:ncol(CoefScore))
   rownames(CoefScore) <- colnames(data)
   ### FIM - encontra scores dos coeficientes ###

   Lista <- list(mtxMC = mc, mtxAutvlr = MAutoVlr,
                 mtxAutvec = MAutoVec, mtxvar = MEigen,
                 mtxcarga = gama[,1:nfactor], mtxvaresp = psi,
                 mtxcomuna = comun, mtxresidue = M, vlrsqrs = SQRS,
                 vlrsqr = SQR, mtxresult = result, mtxscores = Scores[,1:nfactor],
                 coefscores = CoefScore[,1:nfactor])

   return(Lista)
}
