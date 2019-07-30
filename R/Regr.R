Regr <- function(Y, X, namevarx = NA, intercepts = TRUE, sigf = 0.05) {
  # Esta funcao executa a Analise de Regressao
  # desenvolvida por Paulo Cesar Ossani em 06/2016
    
  # Entrada:
  # Y - Respotas.
  # X - Variaveis regressoras.
  # namevarx - Nome da variavel, ou variaveis X, se omitido retorna padrao.
  # intercepts  - Considerar o intercepto na regressao (default = TRUE).
  # sigf        - Nivel de significancia dos testes dos residuos (default = 5%).
  
  # Retorna:
  # Betas    - Coeficientes da regressao.
  # CovBetas - Matriz das covariancias dos coeficientes da regressao.
  # ICc      - Intervalo de confianca dos coeficientes da regressao.
  # hip.test - Teste de hipoteses dos coeficientes da regressao.
  # ANOVA    - Analise de variancia da regressao.
  # R        - Coeficiente de determinacao.
  # Rc       - Coeficiente de determinacao corrigido.
  # Ra       - Coeficiente de determinacao ajustado.
  # QME      - Variancia dos residuos.
  # ICQME    - Intervalo de confianca da variancia dos residuos.
  # prev     - previsao do ajuste da regressao.
  # IPp      - Intervalo das previsoes.  
  # ICp      - Intervalo de confianca das previsoes.
  # error    - Residuos do ajuste da regressao.
  # error.test - Retorna a 5% de significancia o teste de independencia, de 
  #              normalidade e de homogeneidade da variancia dos residuos.
  
  if (!is.vector(Y)) 
     stop("Entrada 'Y' esta incorreta, deve ser do tipo vector. Verifique!")
    
  if (!is.vector(X) && !is.data.frame(X)) 
     stop("Entrada 'X' esta incorreta, deve ser do tipo vector ou data.frame. Verifique!")
  
  if (!is.na(namevarx[1]) && ncol(as.matrix(X))!=length(namevarx))
     stop("Numero de elementos em namevarx difere do numero de colunas de X. Verifique!")
  
  if (is.na(namevarx[1]))
     namevarx <- c(paste("X",1:ncol(as.matrix(X)),sep=""))
  
  if (!is.logical(intercepts)) 
     stop("Entrada para 'intercepts' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  #### INICIO - Analises #####
  Y <- as.matrix(Y) # variavel resposta
  X <- as.matrix(X) # variaveis regressoras
  n <- nrow(X) # numero de observacoes
  p <- ncol(X) # numero de variaveis regressoras

  ## Inicio - Calculo dos coeficientes 
  if (intercepts)
     X <- cbind(rep(1,length(Y)),X)
  
  gl_i <- ifelse(intercepts, 1, 0) # grau de liberdade o intercepto
  
  B <- solve(t(X)%*%X)%*%t(X)%*%Y # Calculo dos coeficientes
  V <- ifelse(intercepts,1,0)
  rownames(B) <- paste("B",(1-V):ifelse(ncol(X)>1,(ncol(X)-V),ncol(X)),sep="") 
  colnames(B) <- c("Coeficientes")
  ## Fim - Calculo dos coeficientes
  
  ## Inicio - Tabela de Analise de Variancia (ANOVA) parcial
  if (intercepts=="N") MediaY <- 0 else MediaY <- mean(Y)
  SQR <- t(B)%*%t(X)%*%Y - n*MediaY^2 # Soma dos Quadrados da Regressao
  SQE <- t(Y)%*%Y - t(B)%*%t(X)%*%Y   # Soma dos Quadrados dos Erros (Residuos)
  SQT <- t(Y)%*%Y - n*MediaY^2        # Soma dos Quadrados Total 
  
  QMR <- SQR / p          # Quadrado Medio da Regressao
  QME <- SQE / (n-p-gl_i) # Quadrado Medio do Erro (Residuos)
  
  FCal <- QMR / QME  # F Calculado
  FTab <- qf(sigf,p,(n-p-gl_i),lower.tail = FALSE) # F Tabelado
  VlrP <- pf(FCal,p,(n-p-gl_i),lower.tail = FALSE) # Valor-p
  
  ANOVA <- as.data.frame(matrix(NA, nrow=3, ncol=6))
  rownames(ANOVA) <- c("Regressao", "Erro","Total")
  colnames(ANOVA) <- c("G.L.", "Soma Quadrados","Quadrados Medios", "f calc.","f tab.","Valor-p")
  ANOVA[,"G.L."]             <- c(p,(n-p-gl_i), n-gl_i)
  ANOVA[,"Soma Quadrados"]   <- c(round(SQR,2),round(SQE,2),round(SQT,2))
  ANOVA[,"Quadrados Medios"] <- c(round(QMR,4),round(QME,4), "-")
  ANOVA[,"f calc."]          <- c(round(FCal,3),"-","-")
  ANOVA[,"f tab."]           <- c(round(FTab,3),"-","-")
  ANOVA[,"Valor-p"]          <- c(round(VlrP,4),"-","-")
  ## Fim - Tabela de Analise de Variancia (ANOVA) parcial
  
  LinFinal <- ANOVA[2:3,] # linha do Erro + Total
  
  if (intercepts) NVaR <- c(2:ncol(X)) else NVaR <- c(1:ncol(X)); # numero das colunas com as variaveis regressoras
  
  ### Inicio - Analise sequencial nas variaveis
  ANOVA_X <- as.data.frame(matrix(NA, nrow=p, ncol=6))
  if (p > 1) { # caso a regressao seja apenas para mais uma variavel regressora
     rownames(ANOVA_X) <- namevarx  # nomes das variaveis de acordo com a coluna dos dados
     colnames(ANOVA_X) <- c("G.L.", "Soma Quadrados","Quadrados Medios", "f calc.","f tab.","Valor-p")
     ANOVA_X[,"G.L."]  <- c(rep(1,p))
     for (i in 1:p) {
       Xn <- X[,-(i + gl_i)]
       Bn    <- solve(t(Xn)%*%Xn)%*%t(Xn)%*%Y  # novos valores para os coeficientes (beta)
       SQRa  <- t(Bn)%*%t(Xn)%*%Y - n*MediaY^2 # Soma dos Quadrados da Regressao da variavel em questao
       QMRx  <- SQR - SQRa # Soma do quadrado da regressao final para a variavel em questao
       FCalX <- QMRx / QME # F Calculado
       FTabX <- qf(sigf,1,(n-p-gl_i),lower.tail = FALSE)  # F Tabelado
       VlrPX <- pf(FCalX,1,(n-p-gl_i),lower.tail = FALSE) # Valor-p
       ANOVA_X[i,"Soma Quadrados"]   <- round(QMRx,2)
       ANOVA_X[i,"Quadrados Medios"] <- round(QMRx,4)
       ANOVA_X[i,"f calc."]          <- round(FCalX,3)
       ANOVA_X[i,"f tab."]           <- round(FTabX,3)
       ANOVA_X[i,"Valor-p"]          <- round(VlrPX,4)
     }
  }
  
  if (p == 1) { # caso a regressao seja apenas para uma variavel regressora
     ANOVA_X <- ANOVA[1,]
     rownames(ANOVA_X) <- namevarx
  }
  
  ANOVA <- rbind(ANOVA[1,],ANOVA_X,LinFinal) # Tabela anova sequencial
  ## Fim - Analise sequencial nas variaveis
  
  ## Inicio - Tabela de Falta de Ajuste
  if (p == 1) # para calculos de regressao simples
     Ft <- levels(as.factor(X[,NVaR])) # niveis dos fatores que compoem os valores de X
  
  if (p != 1) { # para calculos de regressao multipla    
     Filtro <- matrix(0,ncol=1,nrow=nrow(X)) # filtro do valores replicados nas linhas de X
    
     for (i in 1:nrow(X))
       for (j in 1:ncol(X))
         Filtro[i] <- paste(Filtro[i],X[i,j],sep="") 
      
       Ft <- levels(as.factor(Filtro)) # niveis dos fatores que compoem os valores de X
  }
  
  if (length(Ft)!=nrow(Y)) {
     SQRp <- 0 # Soma do Quadrado do Erro Puro
    
    if (p == 1) { # para calculos de regressao simples
       for (i in 1:length(Ft)) {
         Rep <- Y[X[,NVaR]==Ft[i],1] # valore replicados
         Md <- mean(Rep)       # media
         Sq <- sum((Rep-Md)^2) # Soma do Quadrado
         SQRp <- SQRp + Sq     # Soma do Quadrado do Erro Puro
       }
      
       m <- length(Ft) # Numero de niveis dos fatores
    }
    
    if (p != 1) { # para calculos de regressao multipla
       for (i in 1:length(Ft)) {
         Rep <- Y[Filtro==Ft[i]] # valore replicados
         Md <- mean(Rep)       # media
         Sq <- sum((Rep-Md)^2) # Soma do Quadrado
         SQRp <- SQRp + Sq     # Soma do Quadrado do Erro Puro
       }
       m <- length(Ft) - nlevels(Ft) # Numero de niveis dos fatores
    }
    
    GLfa <- m - p - gl_i # Grau de liberdade da Falta de Ajuste
    GLep <- n - m        # Grau de liberdade do Erro Puro 
    
    SQFa <- SQE - SQRp   # Soma do Quadrado da Falta de Ajuste
    
    QMFa <- SQFa / GLfa  # Quadrado Medio da Falta de Ajuste
    QMRp <- SQRp / GLep  # Quadrado Medio do Erro Puro
    
    FCalFa <- QMFa / QMRp  # F Calculado Falta de Ajuste
    FTabFa <- qf(sigf,GLep,GLfa,lower.tail = FALSE)   # F Tabelado Falta de Ajuste
    VlrPFa <- pf(FCalFa,GLfa,GLep,lower.tail = FALSE) # Valor-p Falta de Ajuste
    
    ANOVAjuste <- as.data.frame(matrix(NA, nrow=3, ncol=6))
    rownames(ANOVAjuste) <- c("Falta de Ajuste", "Erro Puro","Total")
    colnames(ANOVAjuste) <- c("G.L.", "Soma Quadrados","Quadrados Medios", "f calc.","f tab.","Valor-p")
    ANOVAjuste[,"G.L."]             <- c(GLfa,GLep, n-gl_i)
    ANOVAjuste[,"Soma Quadrados"]   <- c(round(SQFa,2),round(SQRp,2),round(SQT,2))
    ANOVAjuste[,"Quadrados Medios"] <- c(round(QMFa,4),round(QMRp,2), "-")
    ANOVAjuste[,"f calc."]          <- c(round(FCalFa,3),"-","-")
    ANOVAjuste[,"f tab."]           <- c(round(FTabFa,3),"-","-")
    ANOVAjuste[,"Valor-p"]          <- c(round(VlrPFa,4),"-","-")
    ANOVA <- rbind(ANOVA[1:(nrow(ANOVA)-1),],ANOVAjuste) # Acrescenta na ANOVA anterior
  }
  ## Fim - Tabela de Falta de Ajuste
  
  ## Inicio -  Matriz de covariancias dos coeficientes de regressao
  CovB <- as.numeric(QME)*solve(t(X)%*%X) # Matriz de covariancias dos coeficientes de regressao
  rownames(CovB) <- paste("B",(1-V):ifelse(ncol(X)>1,(ncol(X)-V),ncol(X)),sep="") 
  colnames(CovB) <- rownames(CovB)
  ## Fim -  Matriz de covariancias dos coeficientes de regressao
  
  ## Inicio - Intervalo de Confianca dos coeficientes da regressao
  ICc <- as.data.frame(matrix(NA, nrow=nrow(B), ncol=2))
  C <- diag(solve(t(X)%*%X))
  for (i in 1:nrow(B)) {
    Vlr <- qt(sigf/2,(n - p - gl_i),lower.tail = FALSE)*sqrt(QME*C[i])
    ICc[i,1] <- B[i] - Vlr # limite inferior
    ICc[i,2] <- B[i] + Vlr # limite superior
  }
  ICc <- cbind(B,ICc)
  colnames(ICc) <- c("Coeficientes","I.C. Lim.Inferior","I.C. Lim.Superior")
  rownames(ICc) <- rownames(B)
  ## Fim - Intervalo de Confianca dos coeficientes da regressao
  
  ## Inicio - Teste de Hipoteses para os coeficientes de regressao
  hip.test <- as.data.frame(matrix(NA, nrow=nrow(B), ncol=4))
  C <- diag(solve(t(X)%*%X))
  for (i in 1:nrow(B)) {
    ErroPadrao <- sqrt(QME*C[i])
    tCalc <- B[i] / ErroPadrao # t calculado
    hip.test[i,1] <- ErroPadrao
    hip.test[i,2] <- tCalc # t calculado
    hip.test[i,3] <- qt(sigf,(n - p - gl_i),lower.tail = FALSE) # t encontrado
    hip.test[i,4] <- 2*pt(abs(tCalc),(n - p - gl_i),lower.tail = FALSE) # Valor-p
  }
  hip.test <- cbind(B,hip.test)
  colnames(hip.test) <- c("Coeficientes","Erro Padrao","t calc.","t tab.","Valor-p")
  rownames(hip.test) <- rownames(B)
  ## Fim - Teste de Hipoteses para os coeficientes de regressao
  
  ## Inicio - Coeficiente de determinacao 
  R <- SQR / SQT # Coeficiente de determinacao
  Rc <- R - (1 - R)/(n - p - gl_i) # Coeficiente de determinacao corrigido
  Rc <- ifelse(Rc <= 0, 0, Rc)
  Ra <- ((n - gl_i)*R - p)/(n - p - gl_i) # Coeficiente de determinacao ajustado
  Ra <- ifelse(Ra <= 0, 0, Ra)
  R  <- as.numeric(R)
  Rc <- as.numeric(Rc)
  Ra <- as.numeric(Ra)
  ## Fim - Coeficiente de determinacao
  
  ## Inicio - Intervalo de Confianca da variancia dos residuos 
  Li  <- qchisq(sigf/2,(n - p - gl_i),lower.tail = FALSE)
  Ls  <- qchisq(1-sigf/2,(n - p - gl_i),lower.tail = FALSE)
  ICQME <- t(c(SQE/Li,SQE/Ls))
  colnames(ICQME) <- c("Lim.Inferior","Lim.Superior")
  rownames(ICQME) <- c("Variancia")
  ## Fim - Intervalo de Confianca da variancia dos residuos 
  
  ## Inicio - Intervalo de Confianca dos coeficientes 
  ICc <- as.data.frame(matrix(NA, nrow=nrow(B), ncol=3))
  C   <- diag(solve(t(X)%*%X))
  for (i in 1:nrow(B)) {
    ErroPadrao <- qt(sigf/2,(n - p - gl_i),lower.tail = FALSE)*sqrt(QME*C[i])
    ICc[i,1] <- ErroPadrao
    ICc[i,2] <- B[i] - ErroPadrao # limite inferior
    ICc[i,3] <- B[i] + ErroPadrao # limite superior
  }
  ICc <- cbind(B,ICc)
  colnames(ICc) <- c("Coeficientes","Erro Padrao","I.C. Lim.Inferior","I.C. Lim.Superior")
  rownames(ICc) <- rownames(B)
  ## Fim - Intervalo de Confianca dos coeficientes
  
  ## Inicio - Intervalo de Confianca das previsoes
  ICp <- as.data.frame(matrix(NA, nrow=nrow(Y), ncol=3))
  C   <- solve(t(X)%*%X)
  prev <- X%*%B # previsao
  colnames(prev) <- c("previsao")
  for (i in 1:nrow(Y)) {
    ErroPadrao <- as.numeric(qt(sigf/2,(n - p - gl_i),lower.tail = FALSE)*sqrt(QME%*%t(X[i,])%*%C%*%X[i,]))
    ICp[i,1] <- ErroPadrao
    ICp[i,2] <- prev[i] - ErroPadrao # limite inferior
    ICp[i,3] <- prev[i] + ErroPadrao # limite superior
  }
  ICp <- cbind(Y,prev,ICp)
  colnames(ICp) <- c("Y","previsao","Erro Padrao","I.C. Lim.Inferior","I.C. Lim.Superior")
  ## Fim - Intervalo de Confianca das previsoes
  
  ## Inicio - Intervalo das previsoes
  IPp <- as.data.frame(matrix(NA, nrow=nrow(Y), ncol=3))
  #C <- solve(t(X)%*%X)
  #prev <- X%*%B # previsao
  for (i in 1:nrow(Y)) {
    ErroPadrao <- as.numeric(qt(sigf/2,(n - p - gl_i),lower.tail = FALSE)*sqrt(QME%*%(1+t(X[i,])%*%C%*%X[i,])))
    IPp[i,1] <- ErroPadrao
    IPp[i,2] <- prev[i] - ErroPadrao # limite inferior
    IPp[i,3] <- prev[i] + ErroPadrao # limite superior
  }
  IPp <- cbind(Y,prev,IPp)
  colnames(IPp) <- c("Y","previsao","Erro Padrao","I.P. Lim.Inferior","I.P. Lim.Superior")
  ## Fim - Intervalo das previsoes
  
  error <- Y - prev # Erro do ajuste 
  colnames(error) <- c("Erro do ajuste")
  
  ## Inicio - Resultados dos residuos
  Cor.Test     <- Box.test(error, lag = 1, type = "Box") # teste de correlacao dos residuos
  Norm.Test    <- NormTest(error) # teste de normalidade dos residuos
  First.Group  <- ceiling(length(error)/2)
  Second.Group <- length(error) - First.Group
  Homo.Test    <- bartlett.test(error,c(rep("A",First.Group),rep("B",Second.Group))) # teste de homogeneidade da variancia dos residuos
  
  MResi <- as.data.frame(matrix(NA, nrow=3, ncol=3))
  rownames(MResi) <- c("Teste de independencia","Teste de normalidade","Teste de homocedasticidade")
  colnames(MResi) <- c("Nome do teste","Estatistica do teste","Valor-p")
  MResi[,"Nome do teste"]        <- c("Box-Pierce","Coeficiente de assimetria","Bartlett")
  MResi[,"Estatistica do teste"] <- c(Cor.Test$statistic, Norm.Test$statistic, Homo.Test$statistic)
  MResi[,"Valor-p"]              <- round(c(Cor.Test$p.value, Norm.Test$p.value, Homo.Test$p.value),6)
  ## Fim - Resultados dos residuos
  
  #### FIM - Analises #####
  
  Lista <- list(Y = Y, X = X, intercepts = intercepts, Betas = B, CovBetas = CovB,
                 ICc = ICc, hip.test = hip.test, ANOVA = ANOVA, R = R, Rc = Rc,
                 Ra = Ra, QME = QME, prev = prev, error = error, ICQME= ICQME,
                 ICp = ICp, IPp = IPp, error.test = MResi)
  
  return(Lista)
}