CA <- function(data, typdata = "f", typmatrix = "I") {
  # Funcao Executa a Analise de Correspondencia - CA
  # Desenvolvida por Paulo Cesar Ossani em 06/2013
  
  # Entrada:
  # data    - Dados a serem a analizados
  # typdata - "f" for frequency data (default)
  #           "c" for qualitative data
  # typmatrix - Usado quando typdata = c;
  #             I Matriz Indicadora (default)
  #             B Matriz de Burt
  
  # Retorna:
  # depdata   - Verifica se as linhas e colunas sao dependentes ou independentes pelo teste Qui-quadrado, a nivel 5% de significancia
  # typdata   - Tipo de dados: "F" frequencia ou "C" qualitativo
  # numcood   - Numero de coordenadas principais
  # mtxP      - Matriz da frequencia relativa
  # vtrR      - Vetor com as somas das linhas
  # vtrC      - Vetor com as somas das colunas
  # mtxPR     - Matriz com perfil das linhas
  # mtxPC     - Matriz com perfil das colunas
  # mtxZ      - Matriz Z
  # mtxU      - Matriz com os autovetores U
  # mtxV      - Matriz com os autovetores V
  # mtxL      - Matriz com os Eigenvaluees 
  # mtxX      - Matriz com as coordenadas principais das linhas 
  # mtxY      - Matriz com as coordenadas principais das colunas
  # mtxAutvlr - Matriz das inercias (Variancias), com as proporcoes e proporcoes acumuladas
  
  if (!is.data.frame(data)) 
     stop("Entrada 'data' esta incorreta, deve ser do tipo dataframe. Verifique!")

  typdata   <- toupper(typdata)   # transforma em maiusculo
  
  if (typdata!="F" && typdata!="C" || !is.character(typdata))
     stop("Entrada para 'typdata' esta incorreta, deve ser do tipo caracter, sendo 'f' ou 'c'. Verifique!")
  
  typmatrix <- toupper(typmatrix) # transforma em maiusculo
  
  if (typmatrix!="I" && typmatrix!="B" || !is.character(typmatrix))
     stop("Entrada para 'typmatrix' esta incorreta, deve ser do tipo caracter, sendo 'i' ou 'b'. Verifique!")

  # verifica se os dados sao oriundos de contagem
  if (typdata=="F")  # para dados numericos
    if (sum(data)!=round(sum(abs(data)),0))
       stop("Os dados devem ser de numeros inteiros positivos oriundos de contagem. Verifique!")
  
  Nc = min(nrow(data) - 1, ncol(data) - 1) # numero de coordenadas principais
  
  if (typdata == "C") {  
    # Converte para variaveis Dummy para execucao analise
    # de Correspondencia Multipla, ou seja, em 0 e 1, caso dados nominais
    NumLinha  <- nrow(data)  # Numero de linhas na tabela
    
    for (k in 1:ncol(data)) {
      
      MConver   <- as.factor(data[,k]) # Matriz com os dados para a conversao
      Nivel     <- levels(MConver)     # Nomes dos Niveis
      Qtd_Nivel <- nlevels(MConver)    # Quantidade de Niveis
      MDummy = matrix(0,NumLinha,Qtd_Nivel) # Cria Matriz Vazia com elementos zero
      colnames(MDummy) <- paste(colnames(data)[k],Nivel,sep=":")      # Nomeia as colunas
      # colnames(MDummy) <- (Nivel)      # Nomeia as colunas
 
      for (i in 1:Qtd_Nivel)
        for ( j in 1:NumLinha)
          if (MConver[j]==Nivel[i]) MDummy[j,i] = 1
      
      if (k==1) MFinal <- MDummy
      else
        MFinal <- cbind(MFinal,MDummy)
    }
    
    data = MFinal 
    
    if (typmatrix == "B") { # Matriz de Burt
      data <- as.matrix(data)
      data <- t(data)%*%data
    }
    
  } 
  
  SDados <- sum(data) # Soma Total dos Dados
  
  MP <- as.matrix(data/SDados) # Matriz P
  
  r = apply(MP,1,sum) # Soma das Linhas
  
  c = apply(MP,2,sum) # Soma das Colunas
  
  Dr = diag(r) # Matriz diagonal de r
  
  Dc = diag(c) # Matriz diagonal de c
  
  PR = solve(Dr)%*%MP # Matriz com o perfil das linhas
  
  PC = MP%*%solve(Dc) # Matriz com perfil das colunas
  
  #### INICIO - Teste Qui-quadrado para independencia/dependencia entre as linhas e colunas #####
  Chi.Quad.Observado <- SDados*sum(diag(solve(Dr)%*%(MP-r%*%t(c))%*%solve(Dc)%*%t(MP-r%*%t(c))))
  gl = (ncol(data) - 1)*(nrow(data) - 1) # grau de liberdade
  qt = qchisq(0.95,gl,ncp=0) # teste a nivel de 5% de significancia

  Texto1 <- c("### Teste Qui-quadrado para dependencia entre linhas e colunas ###")
    
  Texto2 <- paste("Grau de liberdade observado:", round(gl,2))
    
  Texto3 <- paste("Valor da estatistica do teste Qui-quadrado (Chiq1):", round(Chi.Quad.Observado,2))
      
  Texto4 <- paste("Valor Qui-quadrado observado (Chiq2) com 5% de significancia:", round(qt,2))
  
  if (Chi.Quad.Observado>qt) Texto5 <- c("Como Chiq1 > Chiq2, verifica-se que EXISTE dependencia entre as linhas e as colunas.")
      
  if (Chi.Quad.Observado<=qt) Texto5 <- c("Como Chiq1 <= Chiq2, verifica-se que NAO EXISTE dependencia entre as linhas e as colunas.")
  
  Texto6 <- paste("Valor-p:", round(pchisq(Chi.Quad.Observado,gl,ncp=0, lower.tail = F),5))
    
  Ddata <- rbind(Texto1,Texto2,Texto3,Texto4,Texto5, Texto6)
  
  rownames(Ddata) <- NULL 
  #### FIM - Teste Qui-quadrado para independencia/dependencia entre as linhas e colunas #####
  
  ##### INICIO Calculo das Coordenadas principais das Linhas e Colunas #####
  MZ = diag(1/sqrt(diag(Dr)))%*%(MP - r%*%t(c))%*%diag(1/sqrt(diag(Dc))) # Matriz Z
  ## Encontra a Matriz de Decomposica Valor Singular de MZ ##
  Mdvs <- svd(MZ) # Matriz de Decomposicao Valor Singular
  Md = diag(Mdvs$d) # Matriz diagonal Lambda
  Mu = Mdvs$u # Matriz U
  Mv = Mdvs$v # Matriz V
  
  MA = sqrt(Dr)%*%Mu # Matriz A
  MB = sqrt(Dc)%*%Mv # Matriz B
  
  X = solve(Dr)%*%MA%*%Md # Coordenadas principais das Linhas
  rownames(X) <- rownames(data)
  colnames(X) <- paste("Coord.", 1:ncol(X))
  
  Y = solve(Dc)%*%MB%*%Md # Coordenadas principais das Colunas
  rownames(Y) <- colnames(data)
  colnames(Y) <- paste("Coord.", 1:ncol(Y))
  ##### FIM Calculo das Coordenadas principais das Linhas e Colunas #####
  
  ##### INICIO - Calculo das Inercia Total #####
  MAutoVlr = diag(Md%*%Md)
  MEigen <- as.data.frame(matrix(NA, Nc, 3))
  rownames(MEigen) <- paste("Comp", 1:Nc)
  colnames(MEigen) <- c("Autovalor", "% da variancia","% acumulada da variancia")
  MEigen[, "Autovalor"] <- MAutoVlr[1:Nc]
  MEigen[, "% da variancia"] <- (MAutoVlr[1:Nc]/sum(MAutoVlr[1:Nc])) * 100
  MEigen[, "% acumulada da variancia"] <- cumsum(MEigen[,"% da variancia"])
  ##### FIM - Calculo das Inercia Total #####
  
  Lista <- list(depdata = as.character(Ddata), typdata = typdata, numcood = Nc,
                mtxP = MP, vtrR = r, vtrC = c, mtxPR = PR, 
                mtxPC = PC, mtxZ = MZ, mtxU = Mu, mtxV= Mv, 
                mtxL = Md, mtxX = X[,1:Nc], mtxY= Y[,1:Nc], mtxAutvlr = MEigen)
  
  return(Lista)
}
