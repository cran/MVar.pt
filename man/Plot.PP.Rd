\name{Plot.PP}

\alias{Plot.PP}

\title{Graficos da projection pursuit (PP).}

\description{Graficos da projection pursuit (PP).}

\usage{Plot.PP(PP, titles = NA, xlabel = NA, ylabel = NA, posleg = 2, boxleg = TRUE,
        size = 1.1, grid = TRUE, color = TRUE, classcolor = NA, linlab = NA, 
        axesvar = TRUE, axes = TRUE, savptc = FALSE, width = 3236, height = 2000, 
        res = 300, casc = TRUE)}
 
\arguments{
  \item{PP}{Dados da funcao PP_Optimizer.}
  \item{titles}{Titulos para os graficos, se nao for definido assume texto padrao.}
  \item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
  \item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
  \item{posleg}{0 sem legenda,\cr
                1 para legenda no canto superior esquerdo,\cr
                2 para legenda no canto superior direito (default),\cr
                3 para legenda no canto inferior direito,\cr
                4 para legenda no canto inferior esquerdo.}
  \item{boxleg}{Coloca moldura na legenda (default = TRUE).}
  \item{size}{Tamanho dos pontos no grafico.}
  \item{grid}{Coloca grade nos graficos (default = TRUE).}
  \item{color}{Graficos coloridos (default = TRUE).}
  \item{classcolor}{Vetor com as cores das classes.}
  \item{linlab}{Vetor com os rotulos para as observacoes.}
  \item{axesvar}{Coloca eixos de rotacao das variaveis, somente quando dimproj > 1 (default = TRUE).}
  \item{axes}{Plota os eixos X e Y (default = TRUE).}
  \item{savptc}{Salva as imagens dos graficos em arquivos (default = FALSE).}   
  \item{width}{Largura do grafico quanto savptc = TRUE (defaul = 3236).}
  \item{height}{Altura do grafico quanto savptc = TRUE (default = 2000).}
  \item{res}{Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).}
  \item{casc}{Efeito cascata na apresentacao dos graficos (default = TRUE).}

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
data <- iris[,1:4]

findex <- "kurtosismax" # funcao indice

dim <- 1 # dimensao da projecao dos dados

sphere <- TRUE # dados esfericos

res <- PP_Optimizer(data = data, class = NA, findex = findex,
                    optmethod = "GTSA", dimproj = dim, sphere = sphere, 
                    weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9, 
                    eps = 1e-3, maxiter = 500, half = 30)

Plot.PP(res, titles = NA, posleg = 1, boxleg = FALSE, color = TRUE,
        linlab = NA, axesvar = TRUE, axes = TRUE, savptc = FALSE, 
        width = 3236, height = 2000, res = 300, casc = FALSE)


# Exemplo 2 - Com as classes nos dados
class <- iris[,5] # classe dos dados

res <- PP_Optimizer(data = data, class = class, findex = findex,
                    optmethod = "GTSA", dimproj = dim, sphere = sphere, 
                    weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9, 
                    eps = 1e-3, maxiter = 500, half = 30)

tit <- c(NA,"Exemplo de grafico") # titulos para os graficos

Plot.PP(res, titles = tit, posleg = 1, boxleg = FALSE, color = TRUE, 
        classcolor = c("blue3","red","goldenrod3"), linlab = NA, 
        axesvar = TRUE, axes = TRUE, savptc = FALSE, width = 3236, 
        height = 2000, res = 300, casc = FALSE)


# Exemplo 3 - Sem as classes nos dados, mas informando 
#             as classes na funcao plot
res <- PP_Optimizer(data = data, class = NA, findex = "Moment",
                    optmethod = "GTSA", dimproj = 2, sphere = sphere, 
                    weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9, 
                    eps = 1e-3, maxiter = 500, half = 30)

lin <- c(rep("a",50),rep("b",50),rep("c",50)) # classe dos dados

Plot.PP(res, titles = NA, posleg = 1, boxleg = FALSE, color = TRUE,
        linlab = lin, axesvar = TRUE, axes = TRUE, savptc = FALSE, 
        width = 3236, height = 2000, res = 300, casc = FALSE)


# Exemplo 4 - Com as classes nos dados, mas nao informada na funcao plot
class <- iris[,5] # classe dos dados

dim <- 2 # dimensao da projecao dos dados

findex <- "lda" # funcao indice

res <- PP_Optimizer(data = data, class = class, findex = findex,
                    optmethod = "GTSA", dimproj = dim, sphere = sphere, 
                    weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9, 
                    eps = 1e-3, maxiter = 500, half = 30)

tit <- c("",NA) # titulos para os graficos

Plot.PP(res, titles = tit, posleg = 1, boxleg = FALSE, color = TRUE,
        linlab = NA, axesvar = TRUE, axes = TRUE, savptc = FALSE, 
        width = 3236, height = 2000, res = 300, casc = FALSE)
}

\keyword{Projection pursuit}
\keyword{PP}
