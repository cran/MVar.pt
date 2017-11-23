\name{PP_Index}

\alias{PP_Index}

\title{Funcao para encontrar os indices da projection pursuit.}

\description{Funcao usada para encontrar os indices da projection pursuit.}

\usage{PP_Index(Data, Class = NULL, Vector.Proj = NULL, 
         Findex = "HOLES", DimProj = 2, Weight = TRUE, 
         Lambda = 0.1, r = 1, ck = NULL)}

\arguments{
\item{Data}{Conjunto de dados numericos sem informacao de classes.}
\item{Class}{Vetor com os nomes das classes dos dados.}
\item{Vector.Proj}{Vetor projecao.}
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
\item{DimProj}{Dimensao da projecao dos dados (default = 2).}
\item{Weight}{Usado nos indice LDA, PDA e Lr, para ponderar os calculos
            pelo numero de elementos em cada classe (default = TRUE).}
\item{Lambda}{Usado no indice PDA (default = 0.1).}
\item{r}{Usado no indice Lr (default = 1).}
\item{ck}{Uso interno da funcao indice CHI.}
}

\value{
\item{Num.Class}{Numero de classes.}
\item{Class.Names}{Nomes das classes.}
\item{Findex}{Funcao indice de projecao usada.}
\item{Vector.Proj}{Vetores de projecao encontrados.}
\item{Index}{Indice de projecao encontrado no processo.}
}

\references{
COOK, D., BUJA, A., CABRERA, J.. Projection pursuit indexes based on orthonormal function expansions. \emph{Journal of Computational and Graphical Statistics}, 2(3):225-250, 1993.

COOK, D., BUJA, A., CABRERA, J., HURLEY, C.. Grand tour and projection pursuit, \emph{Journal of Computational and Graphical Statistics}, 4(3), 155-172, 1995.

COOK, D., SWAYNE, D. F.. Interactive and Dynamic Graphics for Data Analysis: With R and GGobi. Springer. 2007.

ESPEZUA, S., VILLANUEVA, E., MACIEL, C.D., CARVALHO, A.. A projection pursuit framework for supervised dimension reduction of high dimensional small sample datasets. \emph{Neurocomputing}, 149, 767-776, 2015.

FRIEDMAN, J. H., TUKEY, J. W. A projection pursuit algorithm for exploratory data analysis. \emph{IEEE Transaction on Computers}, 23(9):881-890, 1974.

HASTIE, T., BUJA, A., TIBSHIRANI, R.: Penalized discriminant analysis. \emph{The Annals of Statistics}. 23(1), 73-102 . 1995.

HUBER, P. J.. Projection pursuit. \emph{Annals of Statistics}, 13(2):435-475, 1985.

JONES, M. C., SIBSON, R.. What is projection pursuit, (with discussion), \emph{Journal of the Royal Statistical Society}, Series A 150, 1-36, 1987.

LEE, E. K., COOK, D.. A projection pursuit index for large p small n data. \emph{Statistics and Computing}, 20(3):381-392, 2010.

LEE, E., COOK, D., KLINKE, S., LUMLEY, T.. Projection pursuit for exploratory supervised classification. \emph{Journal of Computational and Graphical Statistics}, 14(4):831-846, 2005.

MARTINEZ, W. L., MARTINEZ, A. R.; \emph{Computational Statistics Handbook with MATLAB}, 2th. ed. New York: Chapman & Hall/CRC, 2007. 794 p.

MARTINEZ, W.  L., MARTINEZ, A. R., SOLKA, J.; \emph{Exploratory Data Analysis with MATLAB}, 2th. ed. New York: Chapman & Hall/CRC, 2010. 499 p.

PENA, D., PRIETO, F.. Cluster identification using projections. \emph{Journal of the American Statistical Association}, 96(456):1433-1445, 2001.

POSSE, C.. Projection pursuit exploratory data analysis, \emph{Computational Statistics and Data Analysis}, 29:669-687, 1995a.

POSSE, C.. Tools for two-dimensional exploratory projection pursuit, \emph{Journal of Computational and Graphical Statistics}, 4:83-100, 1995b.

}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{PP_Optimizer}} and \code{\link{Plot.PP}}}

\examples{
data(iris) # conjunto de dados

Dat <- iris[,1:4]

# Exemplo 1 - Sem as classes nos dados
Ind <- PP_Index(Data = Dat, Class = NULL, Vector.Proj = NULL, 
          Findex = "moment", DimProj = 2, Weight = TRUE, 
          Lambda = 0.1, r = 1)

print("Numero de classes:"); Ind$Num.Class
print("Nomes das classes:"); Ind$Class.Names
print("Funcao indice de projecao:"); Ind$Findex
print("Vetores de projecao:"); Ind$Vector.Proj  
print("Indice de projecao:"); Ind$Index


# Exemplo 2 - Com as classes nos dados
Class <- iris[,5] # classe dos dados

FcIndex <- "pda" # funcao indice

Sphere <- TRUE # Dados esfericos

Res <- PP_Optimizer(Data = Dat, Class = Class, Findex = FcIndex,
                    OptMethod = "SA", DimProj = 2, Sphere = Sphere, 
                    Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
                    Eps = 1e-3, Maxiter = 3000, Half = 30)

# Comparando o resultado obtido
if (match(toupper(FcIndex),c("LDA", "PDA", "LR"), nomatch = 0) > 0) {
  if (Sphere) {
     Data <- apply(predict(prcomp(Dat)), 2, scale) # dados esfericos
  } else  Data <- Dat # dados nao esfericos
} else Data <- as.matrix(Res$Proj.Data[,1:Dim])

Ind <- PP_Index(Data = Data, Class = Class, Vector.Proj = Res$Vector.Opt, 
         Findex = FcIndex, DimProj = 2, Weight = TRUE, Lambda = 0.1,
         r = 1)

print("Numero de classes:"); Ind$Num.Class
print("Nomes das classes:"); Ind$Class.Names
print("Funcao indice de projecao:"); Ind$Findex
print("Vetores de projecao:"); Ind$Vector.Proj  
print("Indice de projecao:"); Ind$Index
print("Indice de projecao otimizado:"); Res$Index[length(Res$Index)]
}

\keyword{Projection pursuit}
\keyword{PP}
