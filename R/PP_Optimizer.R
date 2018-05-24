PP_Optimizer = function(Data, Class = NA, Findex = "HOLES", DimProj = 2, Sphere = TRUE, 
                     OptMethod = "GTSA", Weight = TRUE, Lambda = 0.1,  r = 1, 
                     Cooling = 0.9, Eps = 1e-3, Maxiter = 3000, Half = 30) {

  # Funcao de otimizacao dos indices da projection pursuit, desenvolvida
  # por Paulo Cesar Ossani in 2017/04/06.
  
  # Entrada:
  # Data    - Conjunto de dados numericos sem informacao de classes.
  # Class   - Vetor com os nomes das classes dos dados.
  # Findex  - Funcao indice de projecao a ser usada:
  #           "lda" - Indice LDA,
  #           "pda" - Indice PDA,
  #           "lr" - Indice Lr,
  #           "holes" - Indice holes (default),
  #           "cm" - Indice massa central, 
  #           "pca" - Indice PCA,
  #           "friedmantukey" - Indice Friedman Tukey,
  #           "entropy" - Indice entropia,
  #           "legendre" - Indice Legendre,
  #           "laguerrefourier" - Indice Laguerre Fourier,
  #           "hermite" - Indice Hermite,
  #           "naturalhermite" - Indice Hermite natural,
  #           "kurtosismax" - Indice curtose maxima,
  #           "kurtosismin" - Indice curtose minima,
  #           "moment" - Indice momento, 
  #           "chi" - Indice qui-quadrado. 
  # DimProj - Dimensao para a projecao dos dados (default = 2).
  # Sphere  - Dados esfericos (default = TRUE).
  # OptMethod - Metodo de otmizacao GTSA - Grand Tour Simulated Annealing 
  #             ou SA - Simulated Annealing (default = "GTSA").
  # Weight  - Usado nos indice LDA, PDA e Lr, para ponderar os calculos
  #           pelo numero de elementos em cada classe (default = TRUE). 
  # Lambda  - Usado no indice PDA (default = 0.1).
  # r       - Usado no indice Lr(default = 1).
  # Cooling - Taxa de arrefecimento (default = 0.9).
  # Eps     - Precisao de aproximacao para Cooling (default = 1e-3).
  # Maxiter - Numero maximo de iteracoes do algoritimo (default = 3000).
  # Half    - Numero de etapas sem incremetar o indice, para em seguida
  #           diminuir o valor do Cooling (default = 30).
  
  # Retorna:
  # Num.Class   - Numero de classes.
  # Class.Names - Nomes das classes.
  # Proj.Data   - Dados projetados.
  # Vector.Opt  - Vetores de projecao encontrados.
  # Index       - Vetor com os indices de projecao encontrados no processo.
  # Findex      - Funcao indice de projecao usada.
  
  if (!is.data.frame(Data) && !is.matrix(Data))
     stop("Entrada 'Data' esta incorreta, deve ser do tipo dataframe ou matrix. Verifique!")

  if (!is.na(Class)[1]) {

    Class <- as.matrix(Class)

    if (nrow(Data) != length(Class))
       stop("Entrada 'Class' ou 'Data' esta incorreta,AAA devem conter o mesmo numero de linhas. Verifique!")
  }

  if (Findex %in% c("LDA", "PDA", "LR") && is.na(Class))
      stop("Para os indices 'LDA', 'PDA' e 'LR', necessita-se de entrada em 'Class'. Verifique!")
  
  Findex <- toupper(Findex) # transforma em maiusculo

  if (!(Findex %in% c("LDA", "PDA", "LR", "HOLES", "CM", "PCA", "FRIEDMANTUKEY", "ENTROPY",
                      "LEGENDRE",  "LAGUERREFOURIER", "HERMITE", "NATURALHERMITE",
                      "KURTOSISMAX", "KURTOSISMIN", "MOMENT", "CHI")))
     stop(paste("Funcao indice:",Findex, "nao cadastrada. Verifique!"))

  if ((Findex %in% c("PCA","KURTOSISMAX", "KURTOSISMIN"))  && DimProj != 1)
     stop("Para os indices 'PCA', 'KURTOSISMAX' e 'KURTOSISMIN', 'DimProj' deve ser 1 (um). Verifique!")

  if ((Findex %in% c("MOMENT", "CHI", "FRIEDMANTUKEY", "ENTROPY", "LEGENDRE",
                     "LAGUERREFOURIER", "HERMITE", "NATURALHERMITE")) && DimProj != 2)
     stop("Para os indices 'MOMENT', 'CHI', 'FRIEDMANTUKEY', 'ENTROPY', 'LEGENDRE', 'LAGUERREFOURIER', 'HERMITE' e 'NATURALHERMITE', 'DimProj' deve ser 2 (dois). Verifique!")

  if (DimProj >= ncol(Data))
     stop("DimProj maior, ou igual ao numero de colunas em Data. Verifique!")

  if (!is.logical(Sphere))
     stop("Entrada para 'Sphere' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(Weight))
     stop("Entrada para 'Weight' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.numeric(Lambda) || Lambda < 0 || Lambda >= 1 )
     stop("Entrada para 'Lambda' esta incorreta, deve ser um valor numerico entre [0,1). Verifique!")
  
  if (!is.numeric(r) || r <= 0 )
     stop("Entrada para 'r' esta incorreta, deve ser um valor numerico maior que zero. Verifique!")
  
  OptMethod <- toupper(OptMethod) # transforma em maiusculo

  if (!(OptMethod %in% c("GTSA", "SA")))
     stop("Entrada para 'OptMethod' esta incorreta, deve ser do tipo caracter, sendo 'GTSA' ou 'SA'. Verifique!")

  if (nrow(Data) < ncol(Data) && Sphere)
     stop("Para dados esfericos o numero de observacoes deve ser maior, ou igual ao numero de variaveis em Data. Verifique!")

  if (!is.numeric(Cooling) || Cooling <= 0 || Cooling >= 1)
     stop("Entrada para 'Cooling' esta incorreta, deve ser um valor numerico entre (0,1). Verifique!")

  if (!is.numeric(Eps) || Eps <= 0 || Eps >= 1)
     stop("Entrada para 'Eps' esta incorreta, deve ser um valor numerico entre (0,1). Verifique!")

  if (!is.numeric(Maxiter) || Maxiter < 1 || (floor(Maxiter)-Maxiter) != 0)
     stop("Entrada para 'Maxiter' esta incorreta, deve ser um numero inteiro maior que zero. Verifique!")

  if (!is.numeric(Half) || Half < 1 || (floor(Half)-Half) != 0)
     stop("Entrada para 'Half' esta incorreta, deve ser um numero inteiro maior que zero. Verifique!")

  set.seed(7) # semente para o processo de aleatoriedade
  
  #### START - Useful Functions ####
  SphereData <- function(Data) {
    # Esfera uma matriz (ou estrutura de dados), ou seja,
    # projecao das variaveis com componentes principais.
    # E um conjunto com a vista guiada, uma vez que remove padroes
    # mais simples que podem ocultar conclusoes mais interessantes.

    apply(predict(prcomp(Data)), 2, scale)

    ## Tem o mesmo resultado do anterior
    # Dat <- scale(Data, center = T, scale = F) # colunas centralizadas nas respctivas medias
    # a <- var(Dat)
    # t(diag(1/sqrt(eigen(a)$values))%*%t(eigen(a)$vectors)%*%t(Dat)) # projecao dos dados

  }

  
  Base <- function(NumLin, d) {
    # Esta funcao ajuda a encontrar uma base ortonormal usando as componentes principais

    DataBase <- matrix(rnorm(NumLin * d), ncol = d)

    return(DataBase)
  }


  if (OptMethod == "GTSA") { # somente para o metodo de otimizacao grand tour simulated annealing

    Interpolation <- function(Aa, Az) {
      # This function performs matrix Aa interpolation in Az

      # Input:
      # Aa - Initial projection
      # Az - Projection target

      # Return:
      # A - Matrix of interpolated projection of Aa in Az

      if (!is_orthonormal(Aa)) Aa <- Orthonormalise(Aa)

      if (!is_orthonormal(Az)) Az <- Orthonormalise(Az)

      sv <- svd(t(Aa) %*% Az) # decomposicao de valor singular

      # Componentes das decomposicao de varlor singular
      NumCol <- ncol(Aa)
      Lambda <- sv$d[NumCol:1] # do menor para o maior lambda para encontrar os planos mais proximos
      Va     <- sv$u[, NumCol:1]
      Vz     <- sv$v[, NumCol:1]

      # Planos para a projecao
      Ba <- Aa %*% Va
      Bz <- Az %*% Vz

      # Ortonormaliza os planos
      Ba <- Orthonormalise(Ba)
      Bz <- Orthonormalise(Bz)
      Bz <- Orthonormalise_by(Bz, Ba)

      # Calcula os angulos principais
      Tau <- suppressWarnings(acos(Lambda)) # Gera uma mensagem de aviso que corresponde ao seu argumento
      Tau.NaN <- is.nan(Tau) | Tau < Eps
      Tau[Tau.NaN] <- 0

      Bz[, Tau.NaN] <- Ba[, Tau.NaN]

      k <- 1
      # for (k in 1:length(Tau))
      while (k <= length(Tau)) {
        Bz[,k] <- Ba[,k] * cos(Tau[k]) + Bz[,k] * sin(Tau[k])
        k <- k + 1
      }

      A = Bz %*% Va

      return(A)
    }


    Normalise <- function(Base) {
      # Esta funcao normaliza uma Base
      sweep(Base, 2, sqrt(colSums(Base^2,na.rm = TRUE)), FUN = "/")
    }


    Orthonormalise_by <- function(MatX, MatY) {
      # Esta funcao Orthonormalize uma matriz por outra.Isso
      # garante que cada coluna em MatX seja ortogonal a coluna
      # correspondente em por MatY, usando o processo de Gram-Shimidt

      # verifica se as matrizes possuem o mesmo tamanho
      stopifnot(ncol(MatX) == ncol(MatY))
      stopifnot(nrow(MatX) == nrow(MatY))

      MatX <- Normalise(MatX)
      j <- 1
      while(j <= ncol(MatX)) { # processo de ortognalizacao de Gram-Schmidt
        MatX[, j] <- MatX[, j] - c(crossprod(MatX[, j], MatY[, j]) / sum(MatY[, j]^2)) * MatY[, j]
        # MatX[, j] <- MatX[, j] - crossprod(MatX[, j], MatY[, j]) * MatY[, j]
        j <- j + 1
      }

      Normalise(MatX)
    }


    Orthonormalise <- function(Base) {
      # Esta funcao encontra uma base ortogonal ou ortonormal
      # para os vetores em Base usando o processo de Gram-Shimidt

      Base <- Normalise(Base) # to be conservative

      if (ncol(Base) > 1) {
        j <- 1
        while(j <= ncol(Base)) { # processo de ortognalizacao de Gram-Schmidt
          i <- 1
          while(i <= (j - 1)) {
            Base[, j] <- Base[, j] - c(crossprod(Base[, j], Base[, i]) / sum(Base[, i]^2)) * Base[, i]
            #Base[, j] <- Base[, j] - crossprod(Base[, j], Base[, i]) * Base[, i]
            i <- i + 1
          }
          j <- j + 1
        }
      }

      Normalise(Base)
    }


    is_orthonormal <- function(Data) {
      # Esta funcao verifica se Data e ortonormal

      stopifnot(is.matrix(Data))

      Limit <- 0.001

      j <- 1
      while(j <= ncol(Data)) {
        if (sqrt(sum(Data[, j] ^ 2)) < 1 - Limit) return(FALSE)
        j <- j + 1
      }

      if (ncol(Data) > 1) {
        j <- 2
        while(j <= ncol(Data)) {
          i <- 1
          while(i <= (ncol(Data) - 1)) {
            if (abs(sum(Data[, j] * Data[, i])) > Limit) return(FALSE)
            i <- i + 1
          }
          j <- j + 1
        }
      }

      TRUE
    }
  }
  
  
  if (Findex == "CHI") { # funcao usada no indice Qui-quadrado
    # Encontrar a probabilidade de normalizacao bivariada normal sobre cada caixa radial
    fnr <- function(x) { x * exp(-0.5 * x^2) } # veja que aqui a funcao normal padrao bivariada esta em Coordenadas Polares
    ck  <- rep(1,40)
    ck[1:8]   <- integrate(fnr, 0, sqrt(2*log(6))/5)$value/8
    ck[9:16]  <- integrate(fnr, sqrt(2*log(6))/5  , 2*sqrt(2*log(6))/5)$value/8
    ck[17:24] <- integrate(fnr, 2*sqrt(2*log(6))/5, 3*sqrt(2*log(6))/5)$value/8
    ck[25:32] <- integrate(fnr, 3*sqrt(2*log(6))/5, 4*sqrt(2*log(6))/5)$value/8
    ck[33:40] <- integrate(fnr, 4*sqrt(2*log(6))/5, 5*sqrt(2*log(6))/5)$value/8
  } else ck = NA
  
  #### END - Useful Functions ####

  NumCol <- ncol(Data)

  if (Sphere) {
     Dat <- SphereData(as.matrix(Data)) # projeta dos dados usando as componentes principais
  } else Dat <- as.matrix(Data)

  Aa <- diag(1, nrow = NumCol, ncol = DimProj) # Matrix of orthogonal initialization

  if (!(Findex %in% c("LDA", "PDA", "LR", "CHI"))) {

     Proj <- Dat %*% Aa # initial projection

  } else Proj <- Dat
  
  Proj <- as.matrix(Proj)

  Proj.Data <- Proj
 
  IndexMax <- PP_Index(Data = Proj, Class = Class, Vector.Proj = Aa, Findex = Findex,
                       DimProj = DimProj, Weight = Weight, Lambda = Lambda, r = r, ck = ck)$Index # Encontra o indice de acordo com Findex

  Index <- as.matrix(IndexMax) # index of intical projection

  mi <- 1
  h  <- 0	# number of iterations without increase in index
  while (mi <= Maxiter && Cooling > Eps) {

    Ai <- Base(NumCol, DimProj) # initial base
    
    Az <- Aa + Cooling * Ai # projecao alvo

    if (OptMethod == "GTSA") # somente para o metodo de otimizacao grand tour simulated annealing
       Az <-  Interpolation(Aa, Az) # Matriz de projecao atraves da interpolacao

    if (!(Findex %in% c("LDA", "PDA", "CHI"))) {

       Proj <- Dat %*% Az # initial projection

    } else Proj <- Dat

    IndexC <- PP_Index(Data = Proj, Class = Class, Vector.Proj = Az, Findex = Findex,
                       DimProj = DimProj, Weight = Weight, Lambda = Lambda, r = r, ck = ck)  # Encontra o indice de acordo com Findex

    IndexCurent <- IndexC$Index

    print(paste ("Iteracao <-", round(mi,1),"   Indice <-", round(IndexMax,10), "   Cooling <-", round(Cooling,9)))#,"   i<- ", round(i,1)))

    if (Findex == "LDA"   && IndexCurent > IndexMax ||
        Findex == "PDA"   && IndexCurent > IndexMax ||
        Findex == "LR"    && IndexCurent > IndexMax ||
        Findex == "HOLES" && IndexCurent > IndexMax ||
        Findex == "PCA"   && IndexCurent > IndexMax ||
        Findex == "HERMITE"  && IndexCurent > IndexMax ||
        Findex == "ENTROPY"  && IndexCurent > IndexMax ||
        Findex == "LEGENDRE" && IndexCurent > IndexMax ||
        Findex == "LAGUERREFOURIER" && IndexCurent > IndexMax ||
        Findex == "FRIEDMANTUKEY"   && IndexCurent > IndexMax ||
        Findex == "NATURALHERMITE"  && IndexCurent > IndexMax ||
        Findex == "KURTOSISMAX" && IndexCurent > IndexMax || # favorece detectcao outliers
        Findex == "KURTOSISMIN" && IndexCurent < IndexMax || # favorece detectcao agrupamento
        Findex == "MOMENT" && IndexCurent < IndexMax ||
        Findex == "CHI"    && IndexCurent < IndexMax ||
        Findex == "CM"     && IndexCurent < IndexMax) {
      
      if ((Findex %in% c("LDA", "PDA", "CHI")))
         Proj <- Dat %*% Az # initial projection
      
      Aa        <- Az
      IndexMax  <- IndexCurent
      Proj.Data <- Proj
      Index     <- rbind(Index, IndexCurent)

    } else h <- h + 1

    mi <- mi + 1

    if (h == Half) {
       Cooling <- Cooling * 0.9
       h <- 0
    }

  }

  rownames(Index) <- NULL

  rownames(Proj.Data)  <- rownames(Data)

  rownames(Aa) <- colnames(Data)

  if (!is.na(Class)[1]) {

    Proj.Data <- cbind(as.data.frame(Proj.Data), Class)
    colnames(Proj.Data) <- c(paste("Projecao", 1:(ncol(Proj.Data) - 1)),"Classes")

  } else colnames(Proj.Data) <- c(paste("Projecao", 1:(ncol(Proj.Data))))

  colnames(Aa) <- c(paste("Eixo", 1:(ncol(Aa))))
 
  if (length(Index) > 1) colnames(Index) <- "Indices"

  if (!is.na(Class)[1]) {
     Class.Table <- table(Class)        # cria tabela com as quantidade dos elementos das classes
     Class.Names <- names(Class.Table)  # nomes das classses
     Num.Class   <- length(Class.Table) # numero de classes
  } else {
    Class.Names <- NA # nomes das classses
    Num.Class   <- NA # numero de classes
  }
  
  Lista <- list(Num.Class = Num.Class, Class.Names = Class.Names,
                Proj.Data = Proj.Data, Vector.Opt = Aa, 
                Index = Index, Findex = Findex)
  
  return(Lista)
  
}  





 
# if (Findex == "LEGENDRE") { # funcao usada no indice Legendre
#   
#   fn <- function(x) { # Funcao Normal
#     
#     med.x <- mean(x) # medias de x
#     
#     sd.x <- sd(x) # desvio padrao de x
#     
#     1/(sd.x * sqrt(2 * pi)) * exp(-0.5 * (x - med.x)^2 / sd.x^2 ) # funcao normal
#     
#   }
#   
#   PLeg <- function(n, x) { # Polononios de Legrendre ate a ordem 21
#     switch(n + 1
#            , 1
#            , x
#            , 1/2*x^2-1/2
#            , 5/2*x^3-3/2*x
#            , 35/8*x^3-15/4*x^2+3/8
#            , 63/8*x^5-35/4*x^3+15/8*x
#            , 231/16*x^6-315/16*x^4+105/16*x^2-5/16
#            , 429/16*x^7-693/16*x^5+315/16*x^3-35/16*x
#            #, 6435/128*x^8-3003/32*x^6+3465/64*x^4-315/32*x^2+35/128
#            #, 12155/128*x^9-6435/32*x^7+9009/64*x^5-1155/32*x^3+315/128*x
#            #, 46189/256*x^10-109395/256*x^8+45045/128*x^6-15015/128*x^4+3465/256*x^2-63/256
#            #, 88179/256*x^11-230945/256*x^9+109395/128*x^7-45045/128*x^5+15015/256*x^3-693/256*x
#            #, 676039/1024*x^12-969969/512*x^10+2078505/1024*x^8-255255/256*x^6+225225/1024*x^4-9009/512*x^2+231/1024
#            #, 1300075/1024*x^13-2028117/512*x^11+4849845/1024*x^9-692835/256*x^7+765765/1024*x^5-45045/512*x^3+3003/1024*x
#            #, 5014575/2048*x^14-16900975/2048*x^12+22309287/2048*x^10-14549535/2048*x^8+4849845/2048*x^6-765765/2048*x^4+45045/2048*x^2-429/2048
#            #, 9694845/2048*x^15-35102025/2048*x^13+50702925/2048*x^11-37182145/2048*x^9+14549535/2048*x^7-2909907/2048*x^5+255255/2048*x^3-6435/2048*x
#            #, 300540195/32768*x^16-145422675/4096*x^14+456326325/8192*x^12-185910725/4096*x^10+334639305/16384*x^8-20369349/4096*x^6+4849845/8192*x^4-109395/4096*x^2+6435/32768
#            #, 583401555/32768*x^17-300540195/4096*x^15+1017958725/8192*x^13-456326325/4096*x^11+929553625/16384*x^9-66927861/4096*x^7+20369349/8192*x^5-692835/4096*x^3+109395/32768*x
#            #, 2268783825/65536*x^18-9917826435/65536*x^16+4508102925/16384*x^14-4411154475/16384*x^12+5019589575/32768*x^10-1673196525/32768*x^8+156165009/16384*x^6-14549535/16384*x^4+2078505/65536*x^2-12155/65536
#            #, 4418157975/65536*x^19-20419054425/65536*x^17+9917826435/16384*x^15-10518906825/16384*x^13+13233463425/32768*x^11-5019589575/32768*x^9+557732175/16384*x^7-66927861/16384*x^5+14549535/65536*x^3-230945/65536*x
#            #, 34461632205/262144*x^20-83945001525/131072*x^18+347123925225/262144*x^16-49589132175/32768*x^14+136745788725/131072*x^12-29113619535/65536*x^10+15058768725/131072*x^8-557732175/32768*x^6+334639305/262144*x^4-4849845/131072*x^2+46189/262144
#     )
#   }
# }
# 
# 
# if (Findex == "LAGUERREFOURIER") { # funcao usada no indice Leguerre Fourier
#   
#   PLag <- function(n, x) { # Polononios de Laguerre
#     switch(n + 1
#            , 1
#            , - x + 1
#            , x^2-4*x+2
#            , -x^3+9*x^2-18*x+6
#            , x^4-16*x^3+72*x^2-96*x+24
#            , -x^5+25*x^4-200*x^3+600*x^2-600*x+120
#            , x^6-36*x^5+450*x^4-2400*x^3+5400*x^2-4320*x+720
#            , -x^7+49*x^6-882*x^5+7350*x^4-29400*x^3+52920*x^2-35280*x+5040
#            , x^8-64*x^7+1568*x^6-18816*x^5+117600*x^4-376320*x^3+564480*x^2-322560*x+40320
#            # , -x^9+81*x^8-2592*x^7+42336*x^6-381024*x^5+1905120*x^4-5080320*x^3+6531840*x^2-3265920*x+362880
#            # , x^10-100*x^9+4050*x^8-86400*x^7+1058400*x^6-7620480*x^5+31752000*x^4-72576000*x^3+81648000*x^2-36288000*x+3628800
#            # , -x^11+121*x^10-6050*x^9+163350*x^8-2613600*x^7+25613280*x^6-153679680*x^5+548856000*x^4-1097712000*x^3+1097712000*x^2-439084800*x+39916800
#            # , x^12-144*x^11+8712*x^10-290400*x^9+5880600*x^8-75271680*x^7+614718720*x^6-3161410560*x^5+9879408000*x^4-17563392000*x^3+15807052800*x^2-5748019200*x+479001600
#            # , -x^13+169*x^12-12168*x^11+490776*x^10-12269400*x^9+198764280*x^8-2120152320*x^7+14841066240*x^6-66784798080*x^5+185513328000*x^4-296821324800*x^3+242853811200*x^2-8051270400*x+6227020800
#            # , x^14-196*x^13+16562*x^12-794976*x^11+24048024*x^10-480960480*x^9+6492966480*x^8-59364264960*x^7+363606122880*x^6-1454424491520*x^5+3636061228800*x^4-5288816332800*x^3+3966612249600*x^2-1220496076800*x+87178291200
#            # , -x^15+225*x^14-22050*x^13+1242150*x^12-44717400*x^11+1082161080*x^10-18036018000*x^9+208702494000*x^8-1669619952000*x^7+9090153072000*x^6-32724551059200*x^5+74373979680000*x^4-99165306240000*x^3+68652904320000*x^2-19615115520000*x+1307674368000
#            # , x^16-256*x^15+28800*x^14-1881600*x^13+79497600*x^12-2289530880*x^11+46172206080*x^10-659602944000*x^9+6678479808000*x^8-47491411968000*x^7+232707918643200*x^6-761589551923200*x^5+1586644899840000*x^4-1952793722880000*x^3+1255367393280000*x^2-334764638208000*x+20922789888000
#            # , -x^17+289*x^16-36992*x^15+2774400*x^14-135945600*x^13+4594961280*x^12-110279070720*x^11+1906252508160*x^10-23828156352000*x^9+214453407168000*x^8-1372501805875200*x^7+6113871680716800*x^6-18341615042150400*x^5+35272336619520000*x^4-40311241850880000*x^3+24186745110528000*x^2-6046686277632000*x+355687428096000
#            # , x^18-324*x^17+46818*x^16-3995136*x^15+224726400*x^14-8809274880*x^13+248127909120*x^12-5104345559040*x^11+77203226580480*x^10-857813628672000*x^9+6948290392243200*x^8-40426416827596800*x^7+165074535379353600*x^6-457129482588979200*x^5+816302647480320000*x^4-870722823979008000*x^3+489781588488192000*x^2-115242726703104000*x+6402373705728000
#            # , -x^19+361*x^18-58482*x^17+5633766*x^16-360561024*x^15+16225246080*x^14-530024705280*x^13+12796310741760*x^12-230333593351680*x^11+3096707199505920*x^10-30967071995059200*x^9+228030257418163200*x^8-1216161372896870400*x^7+4583992867072819200*x^6-11787410229615820800*x^5+19645683716026368000*x^4-19645683716026368000*x^3+10400656084955136000*x^2-2311256907767808000*x+121645100408832000
#            # , x^20-400*x^19+72200*x^18-7797600*x^17+563376600*x^16-28844881920*x^15+1081683072000*x^14-30287126016000*x^13+639815537088000*x^12-10237048593408000*x^11+123868287980236800*x^10-1126075345274880000*x^9+7601008580605440000*x^8-37420349935288320000*x^7+130971224773509120000*x^6-314330939456421888000*x^5+491142092900659200000*x^4-462251381553561600000*x^3+231125690776780800000*x^2-48658040163532800000*x+2432902008176640000
#     )
#   }
# }
# 
# 
# if (Findex == "HERMITE") { # funcao usada no indice Hermite
#   
#   fn <- function(x) { # Funcao Normal
#     
#     med.x <- mean(x) # medias de x
#     
#     sd.x <- sd(x) # desvio padrao de x
#     
#     1/(sd.x * sqrt(2 * pi)) * exp(-0.5 * (x - med.x)^2 / sd.x^2 ) # funcao normal
#     
#   }
#   
#   PHer <- function(n, x) { # Polononios de Hermite
#     switch(n + 1
#            , 1
#            , 2*x
#            , 4*x^2-2
#            , 8*x^3-12*x
#            , 16*x^4-48*x^2+12
#            , 32*x^5-160*x^3+120*x
#            , 64*x^6-480*x^4+720*x^2-120
#            , 128*x^7-1344*x^5+3360*x^3-1680*x
#            # , 256*x^8-3584*x^6+13440*x^4-13440*x^2+1680
#            # , 512*x^9-9216*x^7+48384*x^5-80640*x^3+30240*x
#            # , 1024*x^10-23040*x^8+161280*x^6-403200*x^4+302400*x^2-30240
#            # , 2048*x^11-56320*x^9+506880*x^7-1774080*x^5+2217600*x^3-665280*x
#            # , 4096*x^12-135168*x^10+1520640*x^8-7096320*x^6+13305600*x^4-7983360*x^2+665280
#            # , 8192*x^13-319488*x^11+4392960*x^9-26357760*x^7+69189120*x^5-69189120*x^3+17297280*x
#            # , 16384*x^14-745472*x^12+12300288*x^10-92252160*x^8+322882560*x^6-484323840*x^4+242161920*x^2-17297280
#            # , 32768*x^15-1720320*x^13+33546240*x^11-307507200*x^9+1383782400*x^7-2905943040*x^5+2421619200*x^3-518918400*x
#            # , 65536*x^16-3932160*x^14+89456640*x^12-984023040*x^10+5535129600*x^8-15498362880*x^6+19372953600*x^4-8302694400*x^2+518918400
#            # , 131072*x^17-8912896*x^15+233963520*x^13-3041525760*x^11+20910489600*x^9-75277762560*x^7+131736084480*x^5-94097203200*x^3+17643225600*x
#            # , 262144*x^18-20054016*x^16+601620480*x^14-9124577280*x^12+75277762560*x^10-338749931520*x^8+790416506880*x^6-846874828800*x^4+317578060800*x^2-17643225600
#            # , 524288*x^19-44826624*x^17+1524105216*x^15-26671841280*x^13+260050452480*x^11-1430277488640*x^9+4290832465920*x^7-6436248698880*x^5+4022655436800*x^3-670442572800*x
#            # , 1048576*x^20-99614720*x^18+3810263040*x^16-76205260800*x^14+866834841600*x^12-5721109954560*x^10+21454162329600*x^8-42908324659200*x^6+40226554368000*x^4-13408851456000*x^2+670442572800
#     )
#   }
# }
# 
# 
# if (Findex == "NATURALHERMITE") { # funcao usada no indice Hermite Natural
#   
#   ### INICIO - Funcao Normal Bivariada ###
#   fnb <- function(vx,vy) {
#     # termos da funcao normal bivariada
#     Term1 <- 1/(2 * pi * sd.x * sd.y * sqrt(1 - rho^2))
#     Term2 <- -1/(2* (1 - rho^2))
#     Term3 <- ((vx - med.x) / sd.x)
#     Term4 <- ((vy - med.y) / sd.y)
#     
#     Term1 * exp( Term2 * ( Term3^2 + Term4^2 - 2 * rho * Term3 * Term4) ) # funcao normal bivariada
#   }
#   ### FIM - Funcao Normal Bivariada ###
#   
#   PNat <- function(n, x) { # Polononios de Hermite Natural
#     switch(n + 1
#            , 1
#            , x
#            , x^2-1
#            , x^3-3*x
#            , x^4-6*x^2+3
#            , x^5-10*x^3+15*x
#            , x^6-15*x^4+45*x^2-15
#            , x^7-21*x^5+105*x^3-105*x
#            , x^8-28*x^6+210*x^4-420*x^2+105
#            , x^9-36*x^7+378*x^5-1260*x^3+945*x
#            , x^10-45*x^8+630*x^6-3150*x^4+4725*x^2-945
#            # , x^11-55*x^9+990*x^7-6930*x^5+17325*x^3-10395*x
#            # , x^12-66*x^10+1485*x^8-13860*x^6+51975*x^4-62370*x^2+10395
#            # , x^13-78*x^11+2145*x^9-25740*x^7+135135*x^5-270270*x^3+135135*x
#            # , x^14-91*x^12+3003*x^10-45045*x^8+315315*x^6-945945*x^4+945945*x^2-135135
#            # , x^15-105*x^13+4095*x^11-75075*x^9+675675*x^7-2837835*x^5+4729725*x^3-2027025*x
#            # , x^16-120*x^14+5460*x^12-120120*x^10+1351350*x^8-7567560*x^6+18918900*x^4-16216200*x^2+2027025
#            # , x^17-136*x^15+7140*x^13-185640*x^11+2552550*x^9-18378360*x^7+64324260*x^5-91891800*x^3+34459425*x
#            # , x^18-153*x^16+9180*x^14-278460*x^12+4594590*x^10-41351310*x^8+192972780*x^6-413513100*x^4+310134825*x^2-34459425
#            # , x^19-171*x^17+11628*x^15-406980*x^13+7936110*x^11-87297210*x^9+523783260*x^7-1571349780*x^5+1964187225*x^3-654729075*x
#            # , x^20-190*x^18+14535*x^16-581400*x^14+13226850*x^12-174594420*x^10+1309458150*x^8-5237832600*x^6+9820936125*x^4-6547290750*x^2+654729075
#     )
#   }
#   
#   f.b <- function(x) { # verifica se he par ou impar, retorna vlr zero para impar e outro vlr para par
#     num <- x/2
#     if ((ceiling(num) - num) == 0) { # se x for par
#       (-1)^x * sqrt(factorial(2*x)) / (sqrt(pi) * factorial(x) * 2^(2*x+1))
#     } else 0 # se x for impar
#   }
# }
