MFA <- function(data, groups, typegroups = rep("n",length(groups)), namegroups = NULL) {
  # Rotina para softwre R para uso do Metodo MFA para dados Quantitativos,
  # Categoricos e de Frequencia desenvolvida por Paulo Cesar Ossani em 
  # 03/2014

  # Entrada:
  # data - Dados a serem analisados.
  # groups - Numero de colunas para cada groups em 
  #         ordem seguindo a ordem dos Dados em 'data'.
  # typegroups - "n" para dados Numericos (default),
  #              "c" para dados Categoricos,
  #              "f" para dados de Frequencia.
  # namegroups - Nomes para cada groups.
  
  # Retorna:
  # vtrG  - Vetor com os tamanhos de cada groups
  # vtrNG - Vetor com os nomes de cada groups
  # vtrplin - Vetor com os valores usados para balancear as linhas da Matriz Z
  # vtrpcol - Vetor com os valores usados para balancear as colunas da Matriz Z
  # mtxZ  - Matriz Concatenada e Balanceada
  # mtxA  - Matriz de autovalores (variancias) com as proporcoes e proporcoes acumuladas.
  # mtxU  - Matriz U da decomposicao singular da Matriz Z
  # mtxV  - Matriz V da decomposicao singular da Matriz Z
  # mtxF  - Matriz Global dos Escores dos Fatores
  # mtxEFG - Matriz dos Escores dos Fatores por groups
  # mtxCCP - Matriz com a Correlacao dos Componentes Principais com os groups
  # mtxEV - Matriz das Inercias Parciais/Escores das Variareis
 
  if (!is.data.frame(data)) 
     stop("Entrada 'data' esta incorreta, deve ser do tipo dataframe. Verifique!")
  
  if (is.null(namegroups)) # Cria nomes para as variaveis caso nao exista
     namegroups <- paste("Variavel", 1:length(typegroups), sep = " ")
  
  if (!is.numeric(groups))
     stop("A entrada para 'groups' esta incorreta, deve ser do tipo numerico. Verifique!")
  
  if (!is.character(typegroups))
     stop("A entrada para 'typegroups' esta incorreta, deve ser do tipo caracter. Verifique!")
  
  if (!is.character(namegroups))
     stop("A entrada para 'namegroups' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (length(typegroups)!=length(groups))
     stop("O numero de componetes da entrada 'typegroups' difere da entrada 'groups'. Verifique!")
  
  if (length(namegroups)!=length(groups))
     stop("O numero de componentes da entrada 'namegroups' difere da entrada 'groups'. Verifique!")
  
  if (is.null(namegroups)) # Cria nomes para as variaveis caso nao exista
     namegroups <- paste("Variavel", 1:length(typegroups), sep = " ")
  
  typegroups <- toupper(typegroups) # transforma em maiusculo
  
  for (i in 1:length(typegroups)) 
    if (!(typegroups[i] %in% c("N", "C", "F")))
       stop("A entrada 'typegroups' esta incorreta, deve ser: n, c, ou f. Verifique!")
  
  
  CA_MFA <- function(data) {
    # Funcao que executa Analise de Correspondencia - CA 
    # nos dados e retorna o primeiro autovalor     
    # Esta funcao e usada na funcao que balanceia dados Categoricos
    
    # Entrada:
    # data - Dados a serem analisados
    
    # Retorna:
    # Inercia - Primeiro auto valor
    
    SDados <- sum(data) # Soma Total dos Dados
    
    MP <- as.matrix(data/SDados) # Matriz da frequencia relativa
    
    r = apply(MP,1,sum) # Soma das Linhas
    
    c = apply(MP,2,sum) # Soma das Colunas
    
    Dr = diag(r) # Matriz diagonal de r
    
    Dc = diag(c) # Matriz diagonal de c
    
    MZ = diag(1/sqrt(diag(Dr)))%*%(MP - r%*%t(c))%*%diag(1/sqrt(diag(Dc))) # Matriz Z
    
    Mdvs <- svd(MZ) # Matriz de Decomposicao Valor Singular
    
    Md = diag(Mdvs$d) # Matriz diagonal Lambda
    
    Inercia = diag(Md%*%Md) # Calculo das inercias - Autovalores
    
    return(Inercia[1]) 
  }
  
  ICA <- function(data,SomaLin) {
    # Funcao que retorna a Analise de Correspondencia Interna (ICA)
    # Esta funcao e usada na funcao que balanceia dados de Frequencia
    
    # Entrada:
    # data    - Tabela de Frequencia dos Dados a serem analisados
    # SomaLin - Matriz com a soma geral das linhas dos dados de frequencia
    
    # Retorna:
    # MatriFR    - Matriz com as Frequencias Relativas
    # MatrixICA  - Matriz da Analise de Correspondencia Interna (ICA)
    # MatrixSLin - Matriz com as Somas das Linhas
    # MatrixSCol - Matriz com as Somas das Colunas
    
    SomaTot <- sum(SomaLin)
    
    MFR <- as.matrix(data/SomaTot) # Matriz da Frequencia Relativa
    
    rTotal = SomaLin/SomaTot # Soma Geral das Linhas
    
    cTotal = apply(MFR,2,sum) # Soma Geral das Colunas
    
    rgroups <- apply(MFR,1,sum) # Soma das Linhas do groups i
    
    cgroups <- apply(MFR,2,sum) # Soma das Colunas do groups i
    
    ICA    <- MFR  # Matriz ICA do groups i
    
    P..t   <- sum(ICA) # Soma Total do groups i
    
    for (col in 1:ncol(ICA)) {    
      
      P.jt = cgroups[col] # Soma Geral da coluna j da Tabela t
      
      for (lin in 1:nrow(ICA)) {
        
        Pijt = ICA[lin,col] # Elemento ij da Tabela t
        
        Pi.. = rTotal[lin]  # Soma Geral da linha i da Tabela Concatenada
        
        Pi.t = rgroups[lin]  # Soma geral da linha i da Tabela t
        
        ICA[lin,col] =  1 / Pi.. * ( Pijt / P.jt - Pi.t / P..t) # Matriz ICA do groups i
      }      
    }
    
    Lista <- list(mtxFR = MFR, MatrixICA = ICA, MatrixSLin = rTotal, MatrixSCol = cTotal)
    
    return(Lista)
  }
   
  MBQ <- function(dataQ,PondGeral) {  
    # Funcao que balanceia Dados quantitativos
    
    # Entrada:
    # dataQ - Dados a serem balanceados
    # PondGeral - usado para equilibrar os conjuntos quantitativos e categoricos, quando ha tabelas de frequencias
    
    # Retorna:
    # MZ   - Matriz Balanceada
    # PLin - Pesos das Linhas
    # PCol - Pesos das Colunas
    
    MZ <- NULL    # cria uma matriz Z nula
    
    PLin <- NULL   # Matriz com os pesos das linhas nula
    
    PCol <- NULL   # Matriz com os pesos das coluna nula
    
    ### INICIO - Centraliza na Media e Padroniza os dados por coluna,  ###
    ### assim teremos media zero e soma quadrado igual ao numero de linhas ###
    MC <- as.matrix(dataQ) # Matriz dados por groups de variaveis
    
    if (sum(PondGeral)!=0) # usado para equilibrar os conjuntos quantitativos e categoricos, quando ha tabelas de frequencias 
      Media <- apply(sweep(MC,1,PondGeral,FUN="*"),2,sum) # Matriz com as medias por colunas poderada pelas linhas ponderadas da tabela de frequencia
    else
      Media <- apply(MC,2,mean) # Matriz com as medias por colunas
    
    MC <- sweep(MC, 2, Media, FUN = "-") # Centraliza na media
    
    if (sum(PondGeral)!=0) # usado para equilibrar os conjuntos quantitativos e categoricos, quando ha tabelas de frequencias
      SqSum <- sqrt(colSums(sweep(as.matrix(MC^2),1,PondGeral,FUN="*")))  # raiz quadrada da soma ao quadrado dos elementos de MC dividido pelas linhas ponderadas da tabela de frequencia
    else
      SqSum <- sqrt(colSums(MC^2)/nrow(MC))
    
    MC <- sweep(MC, 2, SqSum, FUN = "/") # Normaliza os dados ou seja as somas dos quadrados e o numero de linhas  
    ### FIM - Centraliza na Media e Padroniza os dados  ###  
    
    if (sum(PondGeral)!=0) # usado para equilibrar os conjuntos quantitativos e categoricos, quando ha tabelas de frequencias
      PLin <- PondGeral  # raiz quadrada da soma ao quadrado dos elementos de MC dividido pelas linhas ponderadas da tabela de frequencia
    else {
      MC <- as.matrix(dataQ)
      
      PLin <- rep(1,nrow(MC))
      
      NLin <- nrow(MC)
      
      SCol1 <- colSums(MC) / NLin
      
      MC <- sweep(MC, 2, SCol1, FUN = "-")
      
      SCol2 <- sqrt(colSums(MC^2)/NLin)
      
      MC <- sweep(MC, 2, SCol2, FUN = "/")
    }
    
    Pe <- GSVD(MC,PLin,rep(1,ncol(MC)))$d[1]^2   # Encontra o primeiro auto valor de MC
    
    PCol <- cbind(PCol,t(rep(1/Pe,ncol(MC)))) # Matriz com os pesos das colunas
    
    Lista <- list(MZ=MC, PLin=PLin, PCol=PCol)
    
    return(Lista)
  }
   
  MBC <- function(dataC,PondGeral) {  
    # Funcao que balanceia Dados Categoricos
    
    # Entrada:
    # dataQ - Dados a serem balanceados
    # PondGeral - usado para equilibrar os conjuntos quantitativos e categoricos, quando ha tabelas de frequencias
    
    # Retorna:
    # MZ   - Matriz Balanceada
    # PLin - Pesos das Linhas
    # PCol - Pesos das Colunas
    
    MZ   <- NULL   # cria uma matriz Z nula
    
    PLin <- NULL   # Matriz com os pesos das linhas nula
    
    PCol <- NULL   # Matriz com os pesos das coluna nula
    
    IM <- NULL     # Matriz Indicadora
    
    DB <- IM(dataC)  # Matriz dados binarios
    
    IM <- cbind(IM,DB) # Matriz Indicadora
    
    PVS <- CA_MFA(DB)  # Encontra o primeiro Valor Singular
    
    NL  <- nrow(DB)    # numero de linhas
    
    if (sum(PondGeral)!=0) # usado para equilibrar os conjuntos quantitativos e categoricos, quando ha tabelas de frequencias      
       PRL <- as.vector(PondGeral)  # pondera as linhas de acordo com os pesos das linhas da tabela de frequencia
    else  
       PRL <- as.vector(rep(1/NL,NL)) # probabilidade de ocorrencia de cada elemento da linha
    
    MB1 <- sweep(DB,1,PRL,FUN="*") # matriz pre-balanciada 1
    
    PVS <- CA_MFA(MB1) # Encontra o primeiro Valor Singular de MC
    
    SLI <- apply(MB1,2,sum)  # soma das colunas
    
    if (sum(PondGeral)!=0) # usado para equilibrar os conjuntos quantitativos e categoricos, quando ha tabelas de frequencias
       MCO <- colSums(MB1)  # media das colunas
    else 
       MCO <- apply(MB1,2,mean) # media das colunas
    
    DIF <- 1-SLI             # 1 menos soma das colunas
    
    if (sum(PondGeral)!=0) # usado para equilibrar os conjuntos quantitativos e categoricos, quando ha tabelas de frequencias
       MC <- sweep(DB,2,MCO,FUN="-") # matriz pre-balanciada 2 
    else 
       MC <- sweep(MB1,2,MCO,FUN="-") # matriz pre-balanciada 2 - subtrai MCO(media) de MB1
    
    if (sum(PondGeral)!=0) # usado para equilibrar os conjuntos quantitativos e categoricos, quando ha tabelas de frequencias
       VET <- sqrt(colSums(sweep(as.matrix(MC^2),1,PRL,FUN="*")))  # raiz quadrada da soma ao quadrado dos elementos de MC dividido pelas linhas ponderadas da tabela de frequencia
    else
       VET <- sqrt(colSums(MC^2)/NL)  # raiz quadrada da soma ao quadrado dos elementos de MC dividido pelo numero de linhas
    
    MB  <- sweep(MC,2,VET,FUN="/") # matriz balanciada - divide MB2 por VET
    
    QVC <- sum(MB1)      # quantidade de categorias de variaveis
    
    Pe  <- DIF/(PVS*QVC) # valor usado para a ponderacao do PCA
    
    PCol <- rbind(PCol,as.matrix(Pe)) # Matriz com os pesos por coluna
    
    PLin <- rep(1/nrow(MB),nrow(MB)) # Matriz com os pesos das linhas  
    
    Lista <- list(MZ=MB, PLin=PLin, PCol=PCol)
    
    return(Lista)   
  }
  
  
  MBF <- function(dataF,SumLin) {  
    # Funcao que balanceia Dados de Frequencia
    
    # Entrada:
    # dataF  - Dados a serem balanceados
    # SumLin - Matriz com a soma geral das linhas dos dados de frequencia
    
    # Retorna:
    # MZ   - Matriz Balanceada
    # PLin - Pesos das Linhas
    # PCol - Pesos das Colunas
    
    FACI <- ICA(dataF,SumLin) # Retorna dados da funcao ICA - Analise de Correspondencia Interna
    
    MACI <- FACI$MatrixICA    # Matriz da Analise de Correspondencia Interna do groups i
    
    PCol <- NULL  # Matriz com os pesos das colunas 
    
    SLin <- as.matrix(FACI$MatrixSLin) # Matriz com as Somas das linhas da Matriz de Frequencia
    
    SCol <- as.matrix(FACI$MatrixSCol) # Matriz com as Somas das colunas da Matriz de Frequencia
    
    MPVS <- NULL  # Matriz com os primeiros Valores Singulares 
    
    PVS <- GSVD(MACI, SLin, SCol)$d[1]^2 # Encontra o primeiro Auto Valor do groups i
    
    MPVS <- rbind(MPVS,as.matrix(rep(PVS,ncol(MACI)))) # Matriz com os primeiros Valores Singulares 
    
    PCol <- sweep(SCol,1,MPVS,FUN="/") # Matriz com os Pesos das Colunas - divide cada soma das linhas pelos primeiros Auto Valores de cada groups
    
    Lista <- list(MZ=MACI, PLin=SLin, PCol=PCol)
    
    return(Lista)  
  }
  
  ### Inicio - Balanceia os valores dos groups de variaveis ###
  Numgroups = length(groups) # numero de groups formados

  MZG   <- NULL  # cria uma matriz Geral Z nula
  
  PLinG <- NULL  # Matriz Geral com os pesos das linhas nula
  
  PColG <- NULL  # Matriz Geral com os pesos das coluna nula
  
  PondGeral <- 0 # usado para equilibrar os conjuntos quantitativos e categoricos, quando ha tabelas de frequencias
  
  ### Inicio - Encontra as somas totais dos dados de frequencia ###
  if("F"%in%typegroups) {
    
    SomaLinhas <- 0 # Matriz com a soma geral das linhas dos dados de frequencia
    
    j  <- 1        # coluna inicial do groups de variaveis
    
    k  <- groups[1] # coluna final do groups de variaveis
    
    for (i in 1:Numgroups) {
      
      if (typegroups[i]=="F") # Dados de Frequencia
         SomaLinhas <- SomaLinhas + apply(data[,j:k],1,sum) # Matriz com a soma geral das linhas dos dados de frequencia
      
      j <- j + groups[i] # coluna inicial do groups de variaveis
      
      k <- k + groups[i+ifelse(i!=Numgroups,1,0)]  # coluna final do groups de variaveis  
    }
    PondGeral <- SomaLinhas/sum(SomaLinhas) # usado para equilibrar os conjuntos quantitativos e categoricos, quando ha tabelas de frequencias
  } 
  ### Fim - Encontra as somas totais dos dados de frequencia ###
  
  j  <- 1 # coluna inicial do groups de variaveis

  k  <- groups[1] # coluna final do groups de variaveis
  
  for (i in 1:Numgroups) {
      
     if (typegroups[i]=="N"){  # Dados Quantitativos
        MB   <- MBQ(data[,j:k],PondGeral)
        MZ   <- MB$MZ
        PLin <- MB$PLin
        PCol <- MB$PCol
        colnames(PCol) <- colnames(data[,j:k])
     }
     
     if (typegroups[i]=="C") { # Dados Categoricos
        MB   <- MBC(data[,j:k],PondGeral)
        MZ   <- MB$MZ
        PLin <- MB$PLin
        PCol <- t(MB$PCol)
     }  
     
     if (typegroups[i]=="F") {  # Dados de Frequencia
        MB   <- MBF(data[,j:k],SomaLinhas)
        MZ   <- MB$MZ
        PLin <- t(MB$PLin)
        PCol <- t(MB$PCol)
     }  

     PLinG <- PLin  # Matriz Geral com os pesos das linhas
     
     PColG <- cbind(PColG,PCol) # Matriz Geral com os pesos das colunas
     
     MZG   <- cbind(MZG,MZ)     # Matriz Geral Balanceada
     
     j <- j + groups[i]    # coluna inicial do groups de variaveis
      
     k <- k + groups[i+ifelse(i!=Numgroups,1,0)]  # coluna final do groups de variaveis  
     
     if (typegroups[i]=="C")  # Dados Categoricos
        groups[i] <- ncol(MZ) # Como houve expansao da matriz de dados recoloca novo valor para o tamanho do groups

  }
  
  PColG <- t(PColG)  
  ### Fim - Balanceia os valores dos groups de variaveis ###
  
  ### Inicio - Encontra os Autovetores e Autovalores ###
  MDS <- GSVD(MZG, PLinG, PColG) # Encontra a matriz de autovalor e autovetor
  MAutoVlr  <- MDS$d  # Matriz de Autovalores
  MAutoVecU <- MDS$u  # Matriz de Autovetores
  MAutoVecV <- MDS$v  # Matriz de Autovetores
  
  ## Matriz das Variancias
  MEigen <- as.data.frame(matrix(NA, length(MAutoVlr), 3))
  rownames(MEigen) <- paste("Comp", 1:length(MAutoVlr))
  colnames(MEigen) <- c("Autovalor", "% da variancia","% acumulada da variancia")
  MEigen[, "Autovalor"] <- MAutoVlr^2
  MEigen[, "% da variancia"] <- (MAutoVlr^2/sum(MAutoVlr^2)) * 100
  MEigen[, "% acumulada da variancia"] <- cumsum(MEigen[,"% da variancia"])
  
  NumAutoVlr <- length(MAutoVlr) # Numero de auto valores
  
  NE <- length(MAutoVlr[MAutoVlr>1e-10]) # Numero de elementos sigificativos dos Autovalores considerados somente valores acima de 10xe^(-9), isto e importante para calculos das inversas
  ### Fim - Encontra os Autovetores e Autovalores ###
  
  ### INICIO - Matriz Glogal Escore ###
  MF <-  MAutoVecU[,1:NE]%*%diag(MAutoVlr[1:NE],NE) # Matriz F - Matriz Glogal dos Escores de Fatores
  rownames(MF) <- rownames(data) # Nomeia as linhas
  colnames(MF) <- paste("Comp.", 1:ncol(as.matrix(MF)), sep = " ") # Nomeia as colunas
  ### FIM - Matriz Glogal Escore ###
  
  ### INICIO - Matriz dos Escores dos Fatores por groups ###
  j  <- 1 # coluna inicial do groups de variaveis
  
  k  <- groups[1] # coluna final do groups de variaveis
  
  LMFgroups <- as.list(1:Numgroups) # cria lista vazia para a matriz de escores dos fatores dos groups
  
  for (i in 1:Numgroups) {       
    
    MFG <- Numgroups * MZG[,j:k]
    
    MFG <- sweep(MFG, 2, PColG[j:k], FUN="*")
 
    LMFgroups[[i]] <- MFG%*%MAutoVecV[j:k,] # cria Matriz dos Escores dos Fatores por groups
    
    colnames(LMFgroups[[i]]) <- paste("Comp.", 1:ncol(as.matrix(LMFgroups[[i]])), sep = " ") # Nomeia as colunas
 
    j <- j + groups[i]      # coluna inicial do groups de variaveis
    
    k <- k + groups[i+ifelse(i!=Numgroups,1,0)]  # coluna final do groups de variaveis  
  }
  
  names(LMFgroups) <- paste("groups", 1:Numgroups, sep = "") # nomeia os groups
  ### FIM - Matriz dos Escores dos Fatores por groups ###
  
  ### INICIO -  Correlacao dos Componentes Principais com as Variaveis Originais ###
  CCP <- sweep(as.matrix(MAutoVecV), 2, MAutoVlr, FUN = "*")  
  CCP <- t(CCP)
  rownames(CCP) <- paste("Comp.", 1:NumAutoVlr, sep = " ")
  colnames(CCP) <- colnames(MZG)
  ### FIM -  Correlacao dos Componentes Principais com as Variaveis Originais ###
  
  ### INICIO - Matriz das Inercias Parciais/Escores das Variareis ###
  CoordVar <- sweep(as.matrix(MAutoVecV), 2, sqrt(MAutoVlr), FUN = "*")  # Coordenadas das variaveis
  
  ContrVar <- sweep(as.matrix(CoordVar^2), 2, MAutoVlr, "/") # Contribuicao das variaveis
 
  ContrVar <- sweep(as.matrix(ContrVar), 1, PColG, "*")
  
  ContrGru <- matrix(data = NA, nrow = Numgroups, ncol = NumAutoVlr) # Matriz com Contribuicoes dos groups
  
  j  <- 1        # coluna inicial do groups de variaveis
  
  k  <- groups[1] # coluna final do groups de variaveis
 
  for (i in 1:Numgroups) {
    
    ContrGru[i,] <- apply(ContrVar[j:k, ], 2, sum) # Matriz com Contribuicoes dos groups
    
    j <- j + groups[i]      # coluna inicial do groups de variaveis
    
    k <- k + groups[i+ifelse(i!=Numgroups,1,0)]  # coluna final do groups de variaveis  
    
  }
  
  EscVar <- sweep(ContrGru, 2, MAutoVlr^2, "*") # cria Matriz de Escores das variaveis/Inercias parciais
  
  colnames(EscVar) <- paste("Comp.", 1:ncol(as.matrix(EscVar)), sep = " ") # Nomeia as colunas
  
  rownames(EscVar) = namegroups # Nomeias as linhas
  ### FIM - Matriz das Inercias Parciais/Escores das Variareis ###
    
  Lista <- list(vtrG = groups, vtrNG = namegroups, vtrplin = PLinG,
                vtrpcol = PColG, mtxZ = MZG, mtxA = MEigen,
                mtxU = MAutoVecU, mtxV = MAutoVecV, mtxF = MF, 
                mtxEFG = LMFgroups, mtxCCP = CCP, mtxEV = EscVar)
  
  return(Lista)
}