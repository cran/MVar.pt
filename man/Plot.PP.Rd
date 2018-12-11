\name{Plot.PP}

\alias{Plot.PP}

\title{Graficos da projection pursuit (PP).}

\description{Graficos da projection pursuit (PP).}

\usage{Plot.PP(PP, Titles = NA, xlabel = NA, ylabel = NA,
        PosLeg = 2, BoxLeg = TRUE, Color = TRUE, Label = FALSE,
        LinLab = NA, AxisVar = TRUE, Axis = TRUE, Casc = TRUE)}
 
\arguments{
  \item{PP}{Dados da funcao PP_Optimizer.}
  \item{Titles}{Titulos para os graficos, se nao for definido assume texto padrao.}
  \item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
  \item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
  \item{PosLeg}{0 sem legenda,\cr
                1 para legenda no canto superior esquerdo,\cr
                2 para legenda no canto superior direito (default),\cr
                3 para legenda no canto inferior direito,\cr
                4 para legenda no canto inferior esquerdo.}
  \item{BoxLeg}{Coloca moldura na legenda (default = TRUE).}
  \item{Color}{Graficos coloridos (default = TRUE).}
  \item{Label}{Coloca os rotulos das observacoes (default = FALSE).}
  \item{LinLab}{Vetor com os rotulos para as observacoes, se nao for definido assume texto padrao.}
  \item{AxisVar}{Coloca eixos de rotacao das variaveis, somente quando DimProj > 1 (default = TRUE).}
  \item{Axis}{Plota os eixos X e Y (default = TRUE). }
  \item{Casc}{Efeito cascata na apresentacao dos graficos (default = TRUE).}

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
Data <- iris[,1:4]

FcIndex <- "kurtosismax" # funcao indice

Dim <- 1 # dimensao da projecao dos dados

Sphere <- TRUE # dados esfericos

Res <- PP_Optimizer(Data = Data, Class = NA, Findex = FcIndex,
                    OptMethod = "GTSA", DimProj = Dim, Sphere = Sphere, 
                    Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
                    Eps = 1e-3, Maxiter = 500, Half = 30)
 
Plot.PP(Res, Titles = NA, PosLeg = 1, BoxLeg = FALSE, Color = TRUE,
        Label = FALSE, LinLab = NA, AxisVar = TRUE, Axis = TRUE, 
        Casc = FALSE)


# Exemplo 2 - Com as classes nos dados
Class <- iris[,5] # classe dos dados

Res <- PP_Optimizer(Data = Data, Class = Class, Findex = FcIndex,
                    OptMethod = "GTSA", DimProj = Dim, Sphere = Sphere, 
                    Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
                    Eps = 1e-3, Maxiter = 500, Half = 30)

Tit <- c(NA,"Exemplo de grafico") # titulos para os graficos

Plot.PP(Res, Titles = Tit, PosLeg = 1, BoxLeg = FALSE, Color = TRUE,
        Label = FALSE, LinLab = Class, AxisVar = TRUE, Axis = TRUE,
        Casc = FALSE)


# Exemplo 3 - Sem as classes nos dados, mas informando 
#             as classes na funcao plot
Res <- PP_Optimizer(Data = Data, Class = NA, Findex = "Moment",
                    OptMethod = "GTSA", DimProj = 2, Sphere = Sphere, 
                    Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
                    Eps = 1e-3, Maxiter = 500, Half = 30)

Class <- c(rep("a",50),rep("b",50),rep("c",50)) # classe dos dados

Plot.PP(Res, Titles = NA, PosLeg = 1, BoxLeg = FALSE, Color = TRUE,
        Label = FALSE, LinLab = Class, AxisVar = TRUE, Axis = TRUE,
        Casc = FALSE)


# Exemplo 4 - Com as classes nos dados, mas nao informada na funcao plot
Class <- iris[,5] # classe dos dados

Dim <- 2 # dimensao da projecao dos dados

FcIndex <- "lda" # funcao indice

Res <- PP_Optimizer(Data = Data, Class = Class, Findex = FcIndex,
                    OptMethod = "GTSA", DimProj = Dim, Sphere = Sphere, 
                    Weight = TRUE, Lambda = 0.1, r = 1, Cooling = 0.9, 
                    Eps = 1e-3, Maxiter = 500, Half = 30)

Tit <- c("",NA) # titulos para os graficos

Plot.PP(Res, Titles = Tit, PosLeg = 1, BoxLeg = FALSE, Color = TRUE,
        Label = FALSE, LinLab = NA, AxisVar = TRUE, Axis = TRUE,
        Casc = FALSE)
}

\keyword{Projection pursuit}
\keyword{PP}
