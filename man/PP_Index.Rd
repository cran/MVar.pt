\name{PP_Index}

\alias{PP_Index}

\title{Funcao para encontrar os indices da projection pursuit (PP).}

\description{Funcao usada para encontrar os indices da projection pursuit (PP).}

\usage{PP_Index(data, class = NA, vector.proj = NA, 
         findex = "HOLES", dimproj = 2, weight = TRUE, 
         lambda = 0.1, r = 1, ck = NA)}

\arguments{
\item{data}{Conjunto de dados numericos sem a informacao das classes.}
\item{class}{Vetor com os nomes das classes dos dados.}
\item{vector.proj}{Vetor projecao.}
\item{findex}{Funcao indice de projecao a ser usada:\cr
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
              "mf" - Indice MF,\cr
              "chi" - Indice qui-quadrado.}
\item{dimproj}{Dimensao da projecao dos dados (default = 2).}
\item{weight}{Usado nos indice LDA, PDA e Lr, para ponderar os calculos
            pelo numero de elementos em cada classe (default = TRUE).}
\item{lambda}{Usado no indice PDA (default = 0.1).}
\item{r}{Usado no indice Lr (default = 1).}
\item{ck}{Uso interno da funcao indice CHI.}
}

\value{
\item{num.class}{Numero de classes.}
\item{class.names}{Nomes das classes.}
\item{findex}{Funcao indice de projecao usada.}
\item{vector.proj}{Vetores de projecao encontrados.}
\item{index}{Indice de projecao encontrado no processo.}
}

\references{
COOK, D., BUJA, A., CABRERA, J.. Projection pursuit indexes based on orthonormal function expansions. \emph{Journal of Computational and Graphical Statistics}, 2(3):225-250, 1993.

COOK, D., BUJA, A., CABRERA, J., HURLEY, C.. Grand tour and projection pursuit, \emph{Journal of Computational and Graphical Statistics}, 4(3), 155-172, 1995.

COOK, D., SWAYNE, D. F.. Interactive and Dynamic Graphics for data Analysis: With R and GGobi. Springer. 2007.

ESPEZUA, S., VILLANUEVA, E., MACIEL, C.D., CARVALHO, A.. A projection pursuit framework for supervised dimension reduction of high dimensional small sample datasets. \emph{Neurocomputing}, 149, 767-776, 2015.

FRIEDMAN, J. H., TUKEY, J. W. A projection pursuit algorithm for exploratory data analysis. \emph{IEEE Transaction on Computers}, 23(9):881-890, 1974.

HASTIE, T., BUJA, A., TIBSHIRANI, R.: Penalized discriminant analysis. \emph{The Annals of Statistics}. 23(1), 73-102 . 1995.

HUBER, P. J.. Projection pursuit. \emph{Annals of Statistics}, 13(2):435-475, 1985.

JONES, M. C., SIBSON, R.. What is projection pursuit, (with discussion), \emph{Journal of the Royal Statistical Society}, Series A 150, 1-36, 1987.

LEE, E. K., COOK, D.. A projection pursuit index for large p small n data. \emph{Statistics and Computing}, 20(3):381-392, 2010.

LEE, E., COOK, D., KLINKE, S., LUMLEY, T.. Projection pursuit for exploratory supervised classification. \emph{Journal of Computational and Graphical Statistics}, 14(4):831-846, 2005.

MARTINEZ, W. L., MARTINEZ, A. R.; \emph{Computational Statistics Handbook with MATLAB}, 2th. ed. New York: Chapman & Hall/CRC, 2007. 794 p.

MARTINEZ, W.  L., MARTINEZ, A. R., SOLKA, J.; \emph{Exploratory data Analysis with MATLAB}, 2th. ed. New York: Chapman & Hall/CRC, 2010. 499 p.

PENA, D., PRIETO, F.. Cluster identification using projections. \emph{Journal of the American Statistical Association}, 96(456):1433-1445, 2001.

POSSE, C.. Projection pursuit exploratory data analysis, \emph{Computational Statistics and data Analysis}, 29:669-687, 1995a.

POSSE, C.. Tools for two-dimensional exploratory projection pursuit, \emph{Journal of Computational and Graphical Statistics}, 4:83-100, 1995b.

}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{PP_Optimizer}} and \code{\link{Plot.PP}}}

\examples{
data(iris) # conjunto de dados

data <- iris[,1:4]

# Exemplo 1 - Sem as classes nos dados
Ind <- PP_Index(data = data, class = NA, vector.proj = NA, 
                findex = "moment", dimproj = 2, weight = TRUE, 
                lambda = 0.1, r = 1)

print("Numero de classes:"); Ind$num.class
print("Nomes das classes:"); Ind$class.names
print("Funcao indice de projecao:"); Ind$findex
print("Vetores de projecao:"); Ind$vector.proj  
print("Indice de projecao:"); Ind$index


# Exemplo 2 - Com as classes nos dados
class <- iris[,5] # classe dos dados

Fcindex <- "pda" # funcao indice

sphere <- TRUE # Dados esfericos

Res <- PP_Optimizer(data = data, class = class, findex = Fcindex,
                    optmethod = "SA", dimproj = 2, sphere = sphere, 
                    weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9, 
                    eps = 1e-3, maxiter = 1000, half = 30)

# Comparando o resultado obtido
if (match(toupper(Fcindex),c("LDA", "PDA", "LR"), nomatch = 0) > 0) {
  if (sphere) {
     data <- apply(predict(prcomp(data)), 2, scale) # dados esfericos
  }
} else data <- as.matrix(Res$proj.data[,1:Dim])

Ind <- PP_Index(data = data, class = class, vector.proj = Res$vector.opt, 
                findex = Fcindex, dimproj = 2, weight = TRUE, lambda = 0.1,
                r = 1)

print("Numero de classes:"); Ind$num.class
print("Nomes das classes:"); Ind$class.names
print("Funcao indice de projecao:"); Ind$findex
print("Vetores de projecao:"); Ind$vector.proj  
print("Indice de projecao:"); Ind$index
print("Indice de projecao otimizado:"); Res$index[length(Res$index)]
}

\keyword{Projection pursuit}
\keyword{PP}
