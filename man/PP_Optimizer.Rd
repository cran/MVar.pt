\name{PP_Optimizer}

\alias{PP_Optimizer}

\title{Funcao de otimizacao dos indices da projection pursuit (busca de projecao).}

\description{Funcao de otimizacao dos indices da projection pursuit (busca de projecao).}

\usage{PP_Optimizer(Data, Class = NA, Findex = "HOLES",   
             DimProj = 2, Sphere = TRUE, OptMethod = "GTSA",   
             Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9,  
             Eps = 1e-3, Maxiter = 3000, Half = 30)}

\arguments{
  \item{Data}{Conjunto de dados numericos sem a informacao das classes.}
  \item{Class}{Vetor com os nomes das classes dos dados.}
  \item{Findex}{Funcao indice de projecao a ser usada:\cr
                "lda" - Indice LDA,\cr
                "pda" - Indice PDA,\cr
                "lr" - Indice Lr,\cr
                "holes" - Indice holes (default),\cr
                "cm" - Indice massa central,\cr
                "pca" - Indice PCA,\cr
                "friedmantukey" - Indice Friedman Tukey,\cr
                "entropy" - Indice entropia,\cr
                "legendre" - Indice Legendre,\cr
                "laguerrefourier" - Indice Laguerre Fourier,\cr
                "hermite" - Indice Hermite,\cr
                "naturalhermite" - Indice Hermite natural,\cr
                "kurtosismax" - Indice curtose maxima,\cr
                "kurtosismin" - Indice curtose minima,\cr
                "moment" - Indice momento,\cr
                "chi" - Indice qui-quadrado.}
  \item{DimProj}{Dimensao para a projecao dos dados (default = 2).}
  \item{Sphere}{Dados esfericos (default = TRUE).}
  \item{OptMethod}{Metodo de otimizacao GTSA - Grand Tour Simulated Annealing ou SA - Simulated Annealing (default = "GTSA").}
  \item{Weight}{Usado nos indice LDA, PDA e Lr, para ponderar os calculos pelo numero de elementos em cada classe (default = TRUE).}
  \item{Lambda}{Usado no indice PDA (default = 0.1).}
  \item{r}{Usado no indice Lr (default = 1).}
  \item{Cooling}{Taxa de arrefecimento (default = 0.9).}
  \item{Eps}{Precisao de aproximacao para Cooling (default = 1e-3).}
  \item{Maxiter}{Numero maximo de iteracoes do algoritmo (default = 3000).}
  \item{Half}{Numero de etapas sem incrementar o indice, para em seguida diminuir o valor do Cooling (default = 30).}
}

\value{
  \item{Num.Class}{Numero de classes.}
  \item{Class.Names}{Nomes das classes.}
  \item{Proj.Data}{Dados projetados.}
  \item{Vector.Opt}{Vetores de projecao encontrados.}
  \item{Index}{Vetor com os indices de projecao encontrados no processo, convergindo para o maximo, ou o minimo.}
  \item{Findex}{Funcao indice de projecao usada.}
}

\references{
COOK, D., LEE, E. K., BUJA, A., WICKHAM, H.. Grand tours, projection pursuit guided tours and manual controls. In Chen, Chunhouh, Hardle, Wolfgang, Unwin, e Antony (Eds.), \emph{Handbook of Data Visualization}, Springer Handbooks of Computational Statistics, chapter III.2, p. 295-314. Springer, 2008.

LEE, E., COOK, D., KLINKE, S., LUMLEY, T.. Projection pursuit for exploratory supervised classification. \emph{Journal of Computational and Graphical Statistics}, 14(4):831-846, 2005.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.PP}} and \code{\link{PP_Index}}}

\examples{
data(iris) # conjunto de dados

# Exemplo 1 - Sem as classes nos dados
Dat <- iris[,1:4]

Class <- NA # classe dos dados

FcIndex <- "kurtosismax" # funcao indice

Dim <- 1 # dimensao da projecao dos dados

Sphere <- TRUE # Dados esfericos

Res <- PP_Optimizer(Data = Dat, Class = Class, Findex = FcIndex,
                    OptMethod = "GTSA", DimProj = Dim, Sphere = Sphere, 
                    Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
                    Eps = 1e-3, Maxiter = 1000, Half = 30)
 
print("Numero de classes:"); Res$Num.Class
print("Nomes das classes:"); Res$Class.Names
print("Funcao indice de projecao:"); Res$Findex
print("Dados projetados:"); Res$Proj.Data
print("Vetores de projecao:"); Res$Vector.Opt
print("Indices de projecao:"); Res$Index


# Exemplo 2 - Com as classes nos dados
Class <- iris[,5] # classe dos dados

Res <- PP_Optimizer(Data = Dat, Class = Class, Findex = FcIndex,
                    OptMethod = "GTSA", DimProj = Dim, Sphere = Sphere, 
                    Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
                    Eps = 1e-3, Maxiter = 1000, Half = 30)

print("Numero de classes:"); Res$Num.Class
print("Nomes das classes:"); Res$Class.Names
print("Funcao indice de projecao:"); Res$Findex
print("Dados projetados:"); Res$Proj.Data
print("Vetores de projecao:"); Res$Vector.Opt
print("Indices de projecao:"); Res$Index
}

\keyword{Projection pursuit}
\keyword{PP}
