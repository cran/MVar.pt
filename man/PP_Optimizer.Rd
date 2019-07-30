\name{PP_Optimizer}

\alias{PP_Optimizer}

\title{Funcao de otimizacao dos indices da projection pursuit (busca de projecao).}

\description{Funcao de otimizacao dos indices da projection pursuit (busca de projecao).}

\usage{PP_Optimizer(data, class = NA, findex = "HOLES",   
             dimproj = 2, sphere = TRUE, optmethod = "GTSA",   
             weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9,  
             eps = 1e-3, maxiter = 3000, half = 30)}

\arguments{
  \item{data}{Conjunto de dados numericos sem a informacao das classes.}
  \item{class}{Vetor com os nomes das classes dos dados.}
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
  \item{dimproj}{Dimensao para a projecao dos dados (default = 2).}
  \item{sphere}{Dados esfericos (default = TRUE).}
  \item{optmethod}{Metodo de otimizacao GTSA - Grand Tour Simulated Annealing ou SA - Simulated Annealing (default = "GTSA").}
  \item{weight}{Usado nos indice LDA, PDA e Lr, para ponderar os calculos pelo numero de elementos em cada classe (default = TRUE).}
  \item{lambda}{Usado no indice PDA (default = 0.1).}
  \item{r}{Usado no indice Lr (default = 1).}
  \item{cooling}{Taxa de arrefecimento (default = 0.9).}
  \item{eps}{Precisao de aproximacao para cooling (default = 1e-3).}
  \item{maxiter}{Numero maximo de iteracoes do algoritmo (default = 3000).}
  \item{half}{Numero de etapas sem incrementar o indice, para em seguida diminuir o valor do cooling (default = 30).}
}

\value{
  \item{num.class}{Numero de classes.}
  \item{class.names}{Nomes das classes.}
  \item{proj.data}{Dados projetados.}
  \item{vector.opt}{Vetores de projecao encontrados.}
  \item{index}{Vetor com os indices de projecao encontrados no processo, convergindo para o maximo, ou o minimo.}
  \item{findex}{Funcao indice de projecao usada.}
}

\references{
COOK, D., LEE, E. K., BUJA, A., WICKHAM, H.. Grand tours, projection pursuit guided tours and manual controls. In Chen, Chunhouh, Hardle, Wolfgang, Unwin, e Antony (Eds.), \emph{Handbook of data Visualization}, Springer Handbooks of Computational Statistics, chapter III.2, p. 295-314. Springer, 2008.

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
data <- iris[,1:4]

class <- NA # classe dos dados

Fcindex <- "kurtosismax" # funcao indice

Dim <- 1 # dimensao da projecao dos dados

sphere <- TRUE # Dados esfericos

Res <- PP_Optimizer(data = data, class = class, findex = Fcindex,
                    optmethod = "GTSA", dimproj = Dim, sphere = sphere, 
                    weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9, 
                    eps = 1e-3, maxiter = 1000, half = 30)
 
print("Numero de classes:"); Res$num.class
print("Nomes das classes:"); Res$class.names
print("Funcao indice de projecao:"); Res$findex
print("Dados projetados:"); Res$proj.data
print("Vetores de projecao:"); Res$vector.opt
print("Indices de projecao:"); Res$index


# Exemplo 2 - Com as classes nos dados
class <- iris[,5] # classe dos dados

Res <- PP_Optimizer(data = data, class = class, findex = Fcindex,
                    optmethod = "GTSA", dimproj = Dim, sphere = sphere, 
                    weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9, 
                    eps = 1e-3, maxiter = 1000, half = 30)

print("Numero de classes:"); Res$num.class
print("Nomes das classes:"); Res$class.names
print("Funcao indice de projecao:"); Res$findex
print("Dados projetados:"); Res$proj.data
print("Vetores de projecao:"); Res$vector.opt
print("Indices de projecao:"); Res$index
}

\keyword{Projection pursuit}
\keyword{PP}
