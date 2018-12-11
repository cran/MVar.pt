GrandTour <- function(Data, Method = "Interpolation", Title = NA, xlabel = NA, ylabel = NA,
                      Color = TRUE, Label = FALSE, LinLab = NA, AxisVar = TRUE, Axis = FALSE,
                      NumRot = 200, ChoiceRot = NA, SavePicture = FALSE) {
  # Esta funcao executa a rotacao dos dados multivariados em baixa dimensao
  # basea-se nos artigos de: Asimov, D. . The Grand Tour: A Tool for Viewing
  # Multidimensional Data, SIAM Journal of Scientific and Statistical Computing
  # 6(1), 128-143, 1985., Asimov, D. and A. Buja. 1994. The grand tour via 
  # geodesic interpolation of 2-frames,. in Visual Data Exploration and Analysis,
  # Symposium on Electronic Imaging Science and Technology, IS&T/SPIE, E Wegman, E.
  # J. and J. Shen. 1993. Three-dimensional Andrews plots and the grand
  # tour, Proceedings of the 25th Symposium on the Interface, 284-288.
  # by Paulo Cesar Ossani in 2017/03/31

  # Entrada:
  # Data   - Conjunto de dados numericos.
  # Method - Metodo usado para as rotacoes:
  #          "Interpolation" - Metodo Interpolation (default),
  #          "Torus" - Metodo Torus,
  #          "Pseudo" - Metodo Pseudo Grand Tour.
  # Title  - Titulo para os graficos, se omitido retorna default.
  # xlabel   - Nomeia o eixo X, se nao definido retorna padrao.
  # ylabel   - Nomeia o eixo Y, se nao definido retorna padrao.
  # Color  - Graficos coloridos (default = TRUE).
  # Label  - Coloca os rotulos das observacoes (default = FALSE).
  # LinLab - Nomes dos rotulos das observacoes, se omitido retorna a numeracao default.
  # AxisVar - Coloca eixos de rotacao das variaveis (default = TRUE).
  # Axis    - Plota os eixos X e Y (default = FALSE).
  # NumRot  - Numero de rotacoes (default = 200).
  #           Se Method = "Interpolation", NumRot representara o angulo de rotacao.
  # ChoiceRot - Escolhe rotacao especifica e apresenta na tela,
  #             ou salva a imagem se SavePicture = TRUE.
  # SavePicture - Salva as imagens dos graficos em arquivos (default = FALSE).

  # Retorna:
  # Graficos com as rotacoes.
  # Proj.Data  - Dados projetados.
  # Vector.Opt - Vetor projecao.
  # Method     - Metodo usado no Grand Tour.

  if (!is.data.frame(Data))
     stop("Entrada 'Data' esta incorreta, deve ser do tipo dataframe. Verifique!")

  Method <- toupper(Method) # transforma em maiusculo

  if (!(Method %in% c("TORUS", "INTERPOLATION", "PSEUDO")))
     stop("Entrada para 'Method' esta incorreta, deve ser: 'Interpolation', 'Torus' or 'Pseudo'. Verifique!")
  
  if (!is.character(Title) && !is.na(Title[1]))
     stop("Entrada para 'Title' esta incorreta, deve ser do tipo caracter ou string. Verifique!")

  if (!is.logical(Color))
     stop("Entrada para 'Color' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(Label))
     stop("Entrada para 'Label' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.na(LinLab[1])) {
    if (length(LinLab) != nrow(Data)) 
      stop("Entrada para 'LinLab' esta incorreta, deve ter o mesmo numero de linhas que os dados de entrada em 'Data'. Verifique!")
    NomeLinhas = as.matrix(LinLab)
  }
  else {
    NomeLinhas = rownames(Data)
  }

  if (!is.logical(AxisVar))
     stop("Entrada para 'AxisVar' esta incorreta, deve ser TRUE ou FALSE. Verifique!")

  if (!is.logical(Axis)) 
     stop("Entrada para 'Axis' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.numeric(NumRot) || NumRot < 1)
      stop("'NumRot' deve ser um numero inteiro maior que zero. Verifique!")
      
  if (!is.na(ChoiceRot[1])) {
     if (!is.numeric(ChoiceRot) ||  ChoiceRot < 1)
         stop("'ChoiceRot' deve ser um numero inteiro maior que zero. Verifique!")
    
     if(NumRot < ChoiceRot)
       stop("NumRot < ChoiceRot, NumRot tem que ser maior ou igual ChoiceRot. Verifique!")
  }

  if (!is.logical(SavePicture))
     stop("Entrada para 'SavePicture' esta incorreta, deve ser TRUE ou FALSE. Verifique!")
  
  if (!is.character(xlabel) && !is.na(xlabel[1]))
     stop("Entrada para 'xlabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (!is.character(ylabel) && !is.na(ylabel[1]))
     stop("Entrada para 'ylabel' esta incorreta, deve ser do tipo caracter ou string. Verifique!")
  
  if (is.na(xlabel[1]) && Axis)
    xlabel = "Eixo X" 
  
  if (is.na(ylabel[1]) && Axis)
    ylabel = "Eixo Y"
  
  xlabel <- ifelse(Axis, xlabel, "")
  ylabel <- ifelse(Axis, ylabel, "")
  
  d <- 2 # dimensao de projecao
  
  if (Method == "TORUS") {
    
    # O codigo seguinte foi transladado do codigo em Matlab encontrado no Livro de
    # Wendy L. Martinez, Angel Martinez, Jeffrey Solka-Exploratory Data Analysis
    # with MATLAB, Second Edition-CRC Press (2010) paginas 128-129 Example 4.1
    
    n <- nrow(Data)
    
    p <- ncol(Data)
    
    N <- 2 * p - 3 # configurar o vetor de frequencias
    
    DivRest <- exp(1:N) %% 1 # encontra o resto da divisao
    
    NumIrr = exp(-5) # pequeno numero irracional
    
    # indice de rotacao
    J <- 2:p;
    I <- rep(1,length(J))
    I <- c(I, 2*rep(1,length(J)-1))
    J <- c(J, 3:p)
    
    BasicVector <- diag(1, p, d) # vetor basico para a rotacao
    
    NumRot <- ifelse(is.na(ChoiceRot[1]), NumRot, ChoiceRot)
    
    if (SavePicture) {
      cat("\014") # limpa a tela
      cat("\n\n Salvando graficos em disco. Aguarde o termino!")
    }
    
    i <- 1
    while (i <= NumRot) { # Inicializa o Grand Tour
      
      IndVector <- diag(1, p) # vetor identidade
      
      for (j in 1:N) { # encontra a matriz de rotacao
        
        MP <- diag(1, p)
        MP[I[j],J[j]] <-  cos(DivRest[j] * i * NumIrr)
        MP[J[j],I[j]] <-  cos(DivRest[j] * i * NumIrr)
        MP[I[j],J[j]] <- -sin(DivRest[j] * i * NumIrr)
        MP[J[j],I[j]] <-  sin(DivRest[j] * i * NumIrr)
        
        IndVector <- IndVector %*% MP
        
      }
      
      A <- IndVector %*% BasicVector # rotaciona o vetor base
      
      Proj.Data <- as.matrix(Data) %*% A # projecao em direcao a nova base
      
      if (is.na(ChoiceRot[1]) || ChoiceRot == i) {
        
        if (SavePicture) png(filename = paste("Picture ", i," - Metodo", Method,".png",step="")) # salva os graficos em arquivos
        
        maxX = max(Proj.Data[, 1], A[,1])
        minX = min(Proj.Data[, 1], A[,1])
        maxY = max(Proj.Data[, 2], A[,2])
        minY = min(Proj.Data[, 2], A[,2])
        
        Tit <- ifelse(!is.character(Title) || is.na(Title[1]), paste("Rotacao:", i), Title)
        
        plot(Proj.Data, # coordenadas do grafico
             xlab = xlabel, # Nomeia Eixo X
             ylab = ylabel, # Nomeia Eixo Y
             main = Tit, # Titulo para o grafico
             pch  = 16,  # formato dos pontos
             axes = F,   # elimina os eixos
             xlim = c(minX,maxX), # dimensao eixo X
             ylim = c(minY,maxY), # dimensao eixo Y
             col = ifelse(Color, "Blue", "Black"))
        
        if (Label)
           LocLab(Proj.Data, cex = 1, NomeLinhas)
        
        if (Axis) # coloca os eixos
           abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
        
        if (AxisVar) { # plota os eixos das variaveis
          
           Ajuste <- c(diff(range(Proj.Data[,1])) / 2 + min(Proj.Data[,1]),
                       diff(range(Proj.Data[,2])) / 2 + min(Proj.Data[,2]))
          
           PosVar <- cbind(A[,1] + Ajuste[1], A[,2] + Ajuste[2]) # Posicao para as variaveis no grafico
           
           arrows(Ajuste[1], Ajuste[2], PosVar[,1], PosVar[,2],
                  lty = 1, code = 2, length = 0.08, angle = 25,
                  col = ifelse(Color, "Red", "Black"))
           
           LocLab(PosVar, cex = 1, colnames(Data), xpd = TRUE)
          
        }
        
        if (SavePicture) dev.off()
        
        Sys.sleep(0.05) # tempo entre as plotagens dos graficos
        
      }
      
      i <- i + 1
      
    }
    
    if (SavePicture) cat("\n \n Fim!")
    
  }
  
  
  if (Method == "PSEUDO") {
    
    # O codigo seguinte foi transladado do codigo em Matlab encontrado no Livro de
    # Wendy L. Martinez, Angel Martinez, Jeffrey Solka-Exploratory Data Analysis
    # with MATLAB, Second Edition-CRC Press (2010) paginas 131-132 Example 4.2
    
    n <- nrow(Data)
    
    p <- ncol(Data)

    DivRest <- exp(1:p) %% 1 # encontra o resto da divisao
    
    NumIrr = exp(-5)   # pequeno numero irracional
    
    coefic = sqrt(2/p) # pequeno numero irracional
    
    a <- matrix(0, p, 1) # matriz de rotacao
    b <- matrix(0, p, 1) # matriz de rotacao
 
    if (SavePicture) {
      cat("\014") # limpa a tela
      cat("\n\n Salvando graficos em disco. Aguarde o termino!")
    }
    
    NumRot <- ifelse(is.na(ChoiceRot[1]), NumRot, ChoiceRot)
    
    t <- (1:NumRot) * NumIrr
    
    i <- 1
    while (i <= length(t)) { # Inicializa o Grand Tour

      for(j in 1:(p/2)) { # encontra a matriz de rotacao
        a[2*(j-1)+1] = coefic * sin(DivRest[j] * t[i]);
        a[2*j]       = coefic * cos(DivRest[j] * t[i]);
        b[2*(j-1)+1] = coefic * cos(DivRest[j] * t[i]);
        b[2*j]       = coefic * (-sin(DivRest[j] * t[i]));
      }
      
      A <- cbind(a, b) # vetor projecao
      
      Proj.Data <- as.matrix(Data) %*% A; # projecao
 
      if (is.na(ChoiceRot[1]) || ChoiceRot == i) {
        
        if (SavePicture) png(filename = paste("Picture ", i," - Metodo", Method,".png",step="")) # salva os graficos em arquivos
        
        maxX = max(Proj.Data[, 1], A[,1])
        minX = min(Proj.Data[, 1], A[,1])
        maxY = max(Proj.Data[, 2], A[,2])
        minY = min(Proj.Data[, 2], A[,2])
        
        Tit <- ifelse(!is.character(Title) || is.na(Title[1]), paste("Rotacao:", i), Title)
        
        plot(Proj.Data, # coordenadas do grafico
             xlab = xlabel, # Nomeia Eixo X
             ylab = ylabel, # Nomeia Eixo Y
             main = Tit, # Titulo para o grafico
             pch  = 16,  # formato dos pontos
             axes = F,   # elimina os eixos
             xlim = c(minX,maxX), # dimensao eixo X
             ylim = c(minY,maxY), # dimensao eixo Y
             col  = ifelse(Color, "Blue", "Black"))
        
        if (Label)
           LocLab(Proj.Data, cex = 1, NomeLinhas)
        
        if (Axis) # coloca os eixos
           abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
        
        if (AxisVar) { # plota os eixos das variaveis
          
           Ajuste <- c(diff(range(Proj.Data[,1])) / 2 + min(Proj.Data[,1]),
                      diff(range(Proj.Data[,2])) / 2 + min(Proj.Data[,2]))
          
           PosVar <- cbind(A[,1] + Ajuste[1], A[,2] + Ajuste[2]) # Posicao para as variaveis no grafico
 
           arrows(Ajuste[1], Ajuste[2], PosVar[,1], PosVar[,2],
                  lty = 1, code = 2, length = 0.08, angle = 25,
                  col = ifelse(Color, "Red", "Black"))

           LocLab(PosVar, cex = 1, colnames(Data), xpd = TRUE)
          
        }
        
        if (SavePicture) dev.off()
        
        Sys.sleep(0.05) # tempo entre as plotagens dos graficos
        
      }
      
      i <- i + 1
      
    }
    
    if (SavePicture) cat("\n \n Fim!")
    
  }  
  
  
  if (Method == "INTERPOLATION") {
    
    # O codigo seguinte foi transladado do codigo em Matlab encontrado no Livro de 
    # Wendy L. Martinez, Angel Martinez, Jeffrey Solka-Exploratory Data Analysis 
    # with MATLAB, Second Edition-CRC Press (2010) paginas 134 Example 4.3
    
    n <- nrow(Data) 
    
    p <- ncol(Data)
    
    v1 <- 1:floor(p/2)  
    
    v2 <- (max(v1) + 1):p
 
    DataNorm <- scale(Data, center = TRUE, scale = TRUE) # normaliza os dados
 
    NewData <- rbind(DataNorm, diag(p))
     
    theta = (0:NumRot) * pi/180 # rotacoes
    
    PC <- eigen(cov(DataNorm)) # Encontra os componentes principais
  
    AutVec <- PC$vectors[,p:1] # veja que os autovetores sao os dos menores autovalores para os maiores
 
    Vector1 <- NewData %*% AutVec[,v1] # projecao
    Vector2 <- NewData %*% AutVec[,v2] # projecao
    
    Proj.Data  <- cbind(Vector1, Vector2) # dados projetados
   
    # iguala o numero de colunas dos vetores caso sejam diferentes
    if (length(v1) != length(v2)) {
       if (length(v1) < length(v2)) {
          Vector1 <- cbind(Vector1,Vector2[,1])
       } else Vector2 <- cbind(Vector2,Vector1[,1])
    }
    
    cp.v <- ncol(Vector1) 
    
    NumRot <- ifelse(is.na(ChoiceRot[1]), NumRot, ChoiceRot)
      
    if (SavePicture) {
       cat("\014") # limpa a tela 
       cat("\n\n Salvando graficos em disco. Aguarde o termino!")
    }
    
    i <- 1  
    while (i <= NumRot) { # Inicializa o Grand Tour
      
      if (i > 1) # novas projecoes
         Proj.Data = Vector1 %*% diag(1,cp.v) * cos(theta[i]) + Vector2 %*% diag(1,cp.v) * sin(theta[i])
         
      if (is.na(ChoiceRot[1]) || ChoiceRot == i) {
        
        if (SavePicture) png(filename = paste("Picture ", i," - Metodo", Method,".png",step="")) # salva os graficos em arquivos
        
        maxX = max(Proj.Data[, 1])
        minX = min(Proj.Data[, 1])
        maxY = max(Proj.Data[, 2])
        minY = min(Proj.Data[, 2])
        
        Tit <- ifelse(!is.character(Title) || is.na(Title[1]), paste("Angulo de rotacao:", theta[i] / (pi/180),"graus"), Title)
        
        plot(Proj.Data[1:n,1],Proj.Data[1:n,2], # coordenadas do grafico
             xlab = xlabel,  # Nomeia Eixo X
             ylab = ylabel,  # Nomeia Eixo Y
             main = Tit, # Titulo para o grafico
             pch  = 16,  # formato dos pontos 
             axes = F,   # elimina os eixos
             xlim = c(minX,maxX), # dimensao eixo X
             ylim = c(minY,maxY), # dimensao eixo Y
             col  = ifelse(Color, "Blue", "Black"))
        
        if (Label)
           LocLab(Proj.Data[1:n,], cex = 1, NomeLinhas)
        
        if (Axis) # coloca os eixos
           abline(h = 0, v=0, cex = 1.5, lty=2) # cria o eixo central
        
        if (AxisVar) { # plota os eixos das variaveis

           PosVar <- cbind(Proj.Data[(n + 1):(n + p),1],Proj.Data[(n + 1):(n + p),2]) # vetor projecao - coordenadas para os nomes das variaveis

           arrows(0,0, PosVar[,1], PosVar[,2],
                  lty = 1, code = 2, length = 0.08, angle = 25,
                  col = ifelse(Color, "Red", "Black"))

           LocLab(PosVar, cex = 1, colnames(Data), xpd = TRUE)
           
        }
        
        if (SavePicture) dev.off()
        
        Sys.sleep(0.05) # tempo entre as plotagens dos graficos
      
      }
      
      i <- i + 1
      
    }
    
    A <- cbind(Proj.Data[(n + 1):(n + p),1], Proj.Data[(n + 1):(n + p),2]) # vetor projecao
    
    Proj.Data <- Proj.Data[1:n,1:d] # dados projetados
    
    if (SavePicture) cat("\n \n Fim!")
      
  }
  
  rownames(Proj.Data)  <- rownames(Data)
  
  colnames(Proj.Data) <- c(paste("Projecao", 1:(ncol(Proj.Data))))
  
  rownames(A) <- colnames(Data)
  
  colnames(A) <- c(paste("Eixo", 1:(ncol(A))))
  
  Lista <- list(Proj.Data = Proj.Data, Vector.Opt = A, Method = Method)
   
  return(Lista)
   
}
