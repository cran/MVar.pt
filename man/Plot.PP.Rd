\name{Plot.PP}

\alias{Plot.PP}

\title{Graficos da projection pursuit.}

\description{Graficos da projection pursuit.}

\usage{Plot.PP(PP, Titles = NA, xlabel = NA, ylabel = NA,
        PosLeg = 2, BoxLeg = TRUE, Color = TRUE, Label = FALSE,
        LinLab = NA, AxisVar = TRUE, Axis = TRUE)}
 
\arguments{
  \item{PP}{Dados da funcao PP_Optimizer.}
  \item{Titles}{Titulos para os graficos. Se nao for definido assume texto padrao.}
  \item{xlabel}{Nomeia o eixo X, se nao definido retorna padrao.}
  \item{ylabel}{Nomeia o eixo Y, se nao definido retorna padrao.}
  \item{PosLeg}{0 sem legenda,\cr
                1 para legenda no canto superior esquerdo,\cr
                2 para legenda no canto superior direito (default),\cr
                3 para legenda no canto inferior direito,\cr
                4 para legenda no canto inferior esquerdo.}
  \item{BoxLeg}{Colocar moldura na legenda (default = TRUE).}
  \item{Color}{Graficos coloridos (default = TRUE).}
  
  \item{Label}{Coloca os rotulos das observacoes (default = FALSE).}
  \item{LinLab}{Nomes dos rotulos das observacoes, se omitido retorna a numeracao default.}
  \item{AxisVar}{Coloca eixos de rotacao das variaveis, somente quando DimProj > 1 (default = TRUE).}
  \item{Axis}{Plot os eixos X e Y (default = TRUE). }

}

\value{Grafico da evolucao dos indices, e graficos cujos dados foram reduzidos em duas dimensoes.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{PP_Optimizer}} and \code{\link{PP_Index}}}

\examples{
data(iris) # conjunto de dados

# Exemplo 1 - Sem as classes nos dados
Dat <- iris[,1:4]

Class = NA # Nomes das classes

FcIndex <- "kurtosismax" # funcao indice

Dim <- 1 # dimensao da projecao dos dados

Sphere <- TRUE # dados esfericos

Res <- PP_Optimizer(Data = Dat, Class = Class, Findex = FcIndex,
               OptMethod = "GTSA", DimProj = Dim, Sphere = Sphere, 
               Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
               Eps = 1e-3, Maxiter = 500, Half = 30)
 
Tit <- NA # titulos para os graficos

Plot.PP(Res, Titles = Tit, PosLeg = 1, BoxLeg = FALSE, Color = TRUE,
        Label = FALSE, LinLab = Class, AxisVar = TRUE, Axis = TRUE)


# Exemplo 2 - Com as classes nos dados
Class <- iris[,5] # classe dos dados

Res <- PP_Optimizer(Data = Dat, Class = Class, Findex = FcIndex,
               OptMethod = "GTSA", DimProj = Dim, Sphere = Sphere, 
               Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
               Eps = 1e-3, Maxiter = 500, Half = 30)

Tit <- c(NA,"Exemplo de grafico") # titulos para os graficos

Plot.PP(Res, Titles = Tit, PosLeg = 1, BoxLeg = FALSE, Color = TRUE,
        Label = FALSE, LinLab = Class, AxisVar = TRUE, Axis = TRUE)


# Exemplo 3 - Sem as classes nos dados, mas informando 
#             as classes na funcao plot
Res <- PP_Optimizer(Data = Dat, Class = NA, Findex = "Moment",
               OptMethod = "GTSA", DimProj = 2, Sphere = Sphere, 
               Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
               Eps = 1e-3, Maxiter = 500, Half = 30)

Tit <- c(NA,NA) # titulos para os graficos

Class <- c(rep("a",50),rep("b",50),rep("c",50)) # classe dos dados

Plot.PP(Res, Titles = Tit, PosLeg = 1, BoxLeg = FALSE, Color = TRUE,
        Label = FALSE, LinLab = Class, AxisVar = TRUE, Axis = TRUE)


# Exemplo 4 - Com as classes nos dados, mas nao informada na funcao plot
Class <- iris[,5] # classe dos dados

Dim <- 2 # dimensao da projecao dos dados

FcIndex <- "lda" # funcao indice

Res <- PP_Optimizer(Data = Dat, Class = Class, Findex = FcIndex,
               OptMethod = "GTSA", DimProj = Dim, Sphere = Sphere, 
               Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
               Eps = 1e-3, Maxiter = 500, Half = 30)

Tit <- c("",NA) # titulos para os graficos

Plot.PP(Res, Titles = Tit, PosLeg = 1, BoxLeg = FALSE, Color = TRUE,
        Label = FALSE, LinLab = NA, AxisVar = TRUE, Axis = TRUE)
}

\keyword{Projection pursuit}
\keyword{PP}
