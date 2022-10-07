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
   # vlrsqr    - Soma dos Quadrados dos Residuos
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
   
   if (!(scoresobs %in% c("BARTLETT", "REGRESSION")))
      stop("Entrada para 'scoresobs' esta incorreta, deve ser 'Bartlett' ou 'Regression'. Verifique!")
   
   if (!is.logical(testfit) && method == "ML")
      stop("Entrada para 'testfit' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
       
   if (type == 2) data <- scale(data) # normaliza os dados
   
   MC <- cov(data) # Matriz de Covariancia

   Rotacao <- function(Mdata, type = NULL) {
   # Funcao que executa as rotacoes
     if (type == "VARIMAX") {
        Var <- varimax(Mdata, normalize = TRUE)
        Res <- Var$loadings[,]
     }
     
     if (type == "PROMAX") {
         Var <- promax(Mdata, m = 4)
         Res <- Var$loadings[,]
     }
      
     return(Res)
   }
   
   if (method == "PC") { # Metodo dos Componentes Principais
      
      # Encontrando a Matriz de Decomposicao Expectral
      MAV <- svd(MC) # Encontra a matriz de autovalor e autovetor
      MAutoVlr <- MAV$d  # Matriz de Autovalores 
      MAutoVec <- MAV$v # Matriz de Autovetores

      Gama = MAutoVec%*%diag(sqrt(abs(MAutoVlr)),nrow(MC),ncol(MC)) # Matriz de Cargas Fatoriais
      if (rotation != "NONE") {
         Gama <- Rotacao(Gama[,1:nfactor], rotation)
      }
      rownames(Gama) <- colnames(data)
      colnames(Gama) <- paste("Fator",1:ncol(Gama))
      
      Psi = diag(MC - Gama[,1:nfactor]%*%t(Gama[,1:nfactor])) # Matriz de Variancias Especificas
      
      Comun = diag(MC - Psi) # Matriz de Comunalidades
 
      # Valor Limite Superior para a Soma de Quadrados de Residuos
      SQRS = MAutoVlr[(nfactor+1):nrow(as.matrix(MAutoVlr))]%*%(MAutoVlr[(nfactor+1):nrow(as.matrix(MAutoVlr))])
     
      M = MC - (Gama[,1:nfactor]%*%t(Gama[,1:nfactor]) + diag(Psi)) # Matriz dos residuos 
      
      SQR = sum(diag(M%*%t(M))) # Soma dos Quadrados dos Residuos
      
      # Matriz das Variancias
      MEigen <- as.data.frame(matrix(NA, length(MAutoVlr), 3))
      rownames(MEigen) <- paste("Fator", 1:length(MAutoVlr))
      colnames(MEigen) <- c("Autovalor", "% da variancia","% acumulada da variancia")
      MEigen[, "Autovalor"] <- MAutoVlr
      MEigen[, "% da variancia"] <- (MAutoVlr/sum(MAutoVlr)) * 100
      MEigen[, "% acumulada da variancia"] <- cumsum(MEigen[,"% da variancia"]) 
      
      # Matriz com todos os resultados associados
      Result <- as.matrix(cbind(Gama[,1:nfactor],Comun,Psi))
      Result <- rbind(Result,t(rbind(as.matrix(MEigen[1:nfactor,1]),sum(Comun),NA)))
      Result <- rbind(Result,t(rbind(as.matrix(MEigen[1:nfactor,2]),MEigen[nfactor,3],NA)))
      colnames(Result) <- c(paste("Carga Fator",1:nfactor),"Comunalidade","Variancias especificas")
      rownames(Result) <- c(colnames(data),"Variancia","% Variancia")
      
   }
    
   if (method == "PF") { # Metodo dos Fatores Principais
     
      Psi0 <- (solve(diag(diag(solve(MC))))) # Encontrando a Matriz Psi

      Sr <- MC - Psi0 # Encontrando a Matriz Sr
      
      MAV <- svd(Sr) # Encontra a matriz de autovalor e autovetor
      MAutoVlr <- MAV$d  # Matriz de Autovalores 
      MAutoVec <- MAV$v  # Matriz de Autovetores
      
      Gama = MAutoVec%*%diag(sqrt(abs(MAutoVlr)),nrow(MC),ncol(MC)) # Matriz de Cargas Fatoriais
      if (rotation != "NONE") {
         Gama <- Rotacao(Gama[,1:nfactor], rotation)
      }
      rownames(Gama) <- colnames(data)
      colnames(Gama) <- paste("Fator",1:ncol(Gama))
      
      Psi = diag(MC - Gama[,1:nfactor]%*%t(Gama[,1:nfactor])) # Matriz de Variancias Especificas
     
      Comun = diag(MC - Psi) # Matriz de Comunalidades
      
      ## Valor Limite Superior para a Soma de Quadrados de Residuos
      SQRS = MAutoVlr[(nfactor+1):nrow(as.matrix(MAutoVlr))]%*%(MAutoVlr[(nfactor+1):nrow(as.matrix(MAutoVlr))])
   
      # Soma dos Quadrados dos Residuos
      M = MC - (Gama[,1:nfactor]%*%t(Gama[,1:nfactor]) + diag(Psi))
      SQR = sum(diag(M%*%t(M)))

      # Matriz das Variancias
      MEigen <- as.data.frame(matrix(NA, length(MAutoVlr), 3))
      rownames(MEigen) <- paste("Comp", 1:length(MAutoVlr))
      colnames(MEigen) <- c("Autovalor", "% da variancia","% acumulada da variancia")
      MEigen[, "Autovalor"] <- MAutoVlr
      MEigen[, "% da variancia"] <- (MAutoVlr/sum(MAutoVlr)) * 100
      MEigen[, "% acumulada da variancia"] <- cumsum(MEigen[,"% da variancia"])
      
      # Matriz com todos os resultados associados
      Result <- as.matrix(cbind(Gama[,1:nfactor],Comun,Psi))
      Result <- rbind(Result,t(rbind(as.matrix(MEigen[1:nfactor,1]),sum(Comun),NA)))
      Result <- rbind(Result,t(rbind(as.matrix(MEigen[1:nfactor,2]),MEigen[nfactor,3],NA)))
      colnames(Result) <- c(paste("Carga Fator",1:nfactor),"Comunalidade","Variancias especificas")
      rownames(Result) <- c(colnames(data),"Variancia","% Variancia")
   }
   
   if (method == "ML") { # Metodo de maxima verossimilhanca
   
      n  <- ncol(data)*nrow(data) # numero de elementos amostrais
      MC <- (n-ncol(data))/n*MC  # Matriz de Covariancia/Correlacao Maximizada para o teste

      MAV <- svd(MC) # Encontra a matriz de autovalor e autovetor
      MAutoVlr <- MAV$d  # Matriz de Autovalores 
      MAutoVec <- MAV$v # Matriz de Autovetores

      Gama = MAutoVec%*%diag(sqrt(abs(MAutoVlr)),nrow(MC),ncol(MC)) # Matriz de Cargas Fatoriais para Inicializacao da iteracao

      Psi = (diag(MC - Gama[,1:nfactor]%*%t(Gama[,1:nfactor]))) # Matriz das Variancias Especificas
   
      M = MC - (Gama[,1:nfactor]%*%t(Gama[,1:nfactor]) + diag(Psi)) # Matriz dos residuos
      
      SQRi = sum(diag(M%*%t(M))) # Soma dos Quadrados dos Residuos

      ### INICIO DA iteracao ###
      i = 1 # inicializa o contador de iteracoes
      while (1) {
         MC_new = diag(1/sqrt(Psi))%*%(MC - diag(Psi))%*% diag(1/sqrt(Psi)) # nova matriz para iteracao
   
         # Encontrando a Matriz de Decomposicao Expectral
         MAV <- eigen(MC_new) # Encontra a matriz de autovalor e autovetor
         MAutoVlr1 <- MAV$values  # Matriz de Autovalores 
         MAutoVec1 <- MAV$vectors # Matriz de Autovetores
         
         # Matriz das Cargas Fatoriais
         Gama_new = diag(sqrt(Psi))%*%MAutoVec1%*%diag(sqrt(abs(MAutoVlr1)),nrow(MC_new),ncol(MC_new))
   
         Psi = (diag(MC - Gama_new[,1:nfactor]%*%t(Gama_new[,1:nfactor]))) # Matriz das Variancias Especificas
   
         # Valor Limite Superior para a Soma de Quadrados de Residuos
         SQRS = MAutoVlr1[(nfactor+1):nrow(as.matrix(MAutoVlr1))]%*%(MAutoVlr[(nfactor+1):nrow(as.matrix(MAutoVlr1))])
         
         M = MC - (Gama_new[,1:nfactor]%*%t(Gama_new[,1:nfactor]) + diag(Psi)) # Matriz dos Residuos
         
         SQR = sum(diag(M%*%t(M))) # Soma dos Quadrados dos Residuos
        
         if (SQR <= converg) break # sai do loop quando atingir a convergencia
       
         if (i >= iteracao) break # sai do loop apos esse limite de iteracoes
       
         i = i + 1 # incrementa o contador de iteracoes
         
      }
      ### FIM DA iteracao ###
      
      Gama = Gama_new # Matriz com as cargas fatoriais
  
      if (rotation != "NONE") {
         Gama <- Rotacao(Gama[,1:nfactor], rotation)
      }
      rownames(Gama) <- colnames(data)
      colnames(Gama) <- paste("Fator",1:ncol(Gama))
      
      if (type == 1) {# Considera a Matriz de Covariancia para a decomposicao
         Gama <- diag(1/sqrt(diag(MC)))%*%Gama[,1:nfactor] # Matriz com as cargas fatoriais
         Comun = rowSums(Gama^2)#apply(Gama,1,function(Gama) Gama^2)) # Matriz de Comunalidades
      }
      
      if (type == 2) # Considera a Matriz de Correlacao para a decomposicao
         Comun = diag(MC - Psi) # Matriz de Comunalidades
      
      # Matriz das Variancias
      MEigen <- as.data.frame(matrix(NA, length(MAutoVlr), 3))
      rownames(MEigen) <- paste("Comp", 1:length(MAutoVlr))
      colnames(MEigen) <- c("Autovalor", "% da variancia","% acumulada da variancia")
      MEigen[, "Autovalor"] <- MAutoVlr
      MEigen[, "% da variancia"] <- (MAutoVlr/sum(MAutoVlr)) * 100
      MEigen[, "% acumulada da variancia"] <- cumsum(MEigen[,"% da variancia"])
      
      print(paste("Numero de iteracoes:",i))
      
      # Matriz com todos os resultados associados
      Result <- as.matrix(cbind(Gama[,1:nfactor],Comun,Psi))
      Result <- rbind(Result,t(rbind(as.matrix(MEigen[1:nfactor,1]),sum(Comun),NA)))
      Result <- rbind(Result,t(rbind(as.matrix(MEigen[1:nfactor,2]),MEigen[nfactor,3],NA)))
      colnames(Result) <- c(paste("Carga Fator",1:nfactor),"Comunalidade","Variancias especificas")
      rownames(Result) <- c(colnames(data),"Variancia","% Variancia")  
      
      ### INICIO - Teste da falta de ajusto do modelo fatorial - teste Qui-quadrado ###
      if (testfit) {
         p <- nrow(Gama)  # numero de parametros
      
         gl <- ((p - nfactor)^2 - nfactor - p)/2 # grau de liberdade
    
         cat("### TESTE DO AJUSTE DO MODELO ###\n")
      
         cat(paste("Grau de liberdade observado:", round(gl,5)),"\n")
        
         if (gl < 0) 
            cat("Nao foi possivel realizar o teste de ajuste do modelo, pois grau de libertade foi negativo, aconselha-se a mudar os parametros, para processeguir com o teste. Exemplo: numero de fatores ou mesmo 'type'.\n")
   
         if (det(MC) <= 0) 
            cat("Nao foi possivel realizar o teste de ajuste do modelo, pois o determinante da matriz de variancia/covariancia deve ser diferente de zero, para processeguir com o teste mude os parametros.\n")
         
         if (gl >= 0 && det(MC) > 0) {
          
            Ps_i = diag(diag(MC - Gama[,1:nfactor]%*%t(Gama[,1:nfactor])))
          
            Chi.Quad.Observado <- (n - 1 - (2*p + 5)/6 - 2*nfactor/3)*log(abs(det(Gama[,1:nfactor]%*%t(Gama[,1:nfactor])+Ps_i))/abs(det(MC)))

            qt = qchisq(0.95,gl,ncp=0)
    
            cat(paste("Valor da estatistica do teste Qui-quadrado (Chiq1):", round(Chi.Quad.Observado,3)),"\n")
         
            cat(paste("Valor Qui-quadrado observado (Chiq2) com 5% de significancia:", round(qt,3)),"\n")
        
            if (Chi.Quad.Observado<=qt) cat("Como Chiq1 <= Chiq2, verifica-se que o numero de fatores FORAM suficientes.\n")
        
            if (Chi.Quad.Observado>qt) cat("Como Chiq1 > Chiq2, verifica-se que o numero de fatores NAO FORAM suficientes.\n")
            
            cat("Valor-p:", pchisq(Chi.Quad.Observado,gl,ncp=0, lower.tail = F))
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
      Scores <- MASS::ginv(t(Gama)%*%solve(diag(Psi))%*%Gama)%*%(t(Gama)%*%solve(diag(Psi)))%*%t(dataNorm) # Matriz com os escores das observacoes
      #Scores <- solve(t(Gama)%*%solve(diag(Psi))%*%Gama)%*%(t(Gama)%*%solve(diag(Psi)))%*%t(dataNorm) # Matriz com os escores das observacoes
      #Scores <- dataNorm%*%solve(MC)%*%Gama # outro modo de encontrar a solucao acima
   }
   
   if (scoresobs == "REGRESSION") { # Metodo de Regressao
      Media <- mean(as.matrix(data))
      dataNorm <- sweep(as.matrix(data), 2, Media, FUN = "-") # Centraliza na media geral todos os dados
      I <- diag(rep(ncol(Gama)))
      Scores <- solve(I + t(Gama)%*%solve(diag(Psi))%*%Gama)%*%(t(Gama)%*%solve(diag(Psi)))%*%t(dataNorm) # Matriz com os escores das observacoes
      #Scores <- t(Gama)%*%solve(Gama%*%t(Gama)+diag(Psi))%*%t(dataNorm) # outro modo de encontrar a solucao acima
   }
   Scores <- t(Scores)
   colnames(Scores) <- colnames(Gama)
   rownames(Scores) <- rownames(data)
   ### FIM - encontrar os scores das observacoes ###  
 
   ### INCIO - encontra scores dos coeficientes ###
   CoefScore <- t(MASS::ginv(t(Gama)%*%MASS::ginv(diag(Psi))%*%Gama)%*%t(Gama)%*%MASS::ginv(diag(Psi)))
   colnames(CoefScore) <- paste("Fator", 1:ncol(CoefScore))
   rownames(CoefScore) <- colnames(data)
   ### FIM - encontra scores dos coeficientes ###

   Lista <- list(mtxMC = MC, mtxAutvlr = MAutoVlr,
                 mtxAutvec = MAutoVec, mtxvar = MEigen,
                 mtxcarga = Gama[,1:nfactor], mtxvaresp = Psi,
                 mtxcomuna = Comun, mtxresidue = M, vlrsqrs = SQRS,
                 vlrsqr = SQR, mtxresult = Result, mtxscores = Scores[,1:nfactor],
                 coefscores = CoefScore[,1:nfactor])

   return(Lista)
}
