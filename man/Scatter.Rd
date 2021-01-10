\name{Scatter}

\alias{Scatter}

\title{Grafico de dispersao.}

\description{Realiza o grafico de dispersao.}

\usage{Scatter(data, ellipse = TRUE, ellipse.level = 0.95, rectangle = FALSE,  
        title = NA, xlabel = NA, ylabel = NA,  posleg = 2, boxleg = TRUE, 
        axes = TRUE, size = 1.1, grid = TRUE, color = TRUE, linlab = NA,   
        class = NA, classcolor = NA, savptc = FALSE, width = 3236, 
        height = 2000, res = 300)}
          


\arguments{
\item{data   }{Dados com as coordenadas x e y.}
\item{ellipse}{Coloca uma elipse envolta das classes (default = TRUE).}
\item{ellipse.level}{Nivel de significancia da elipse (defaul = 0.95).}
\item{rectangle}{Coloca retangulo para diferenciar as classes (default = FALSE).}
\item{title  }{Titulo para os graficos, se nao for definido assume texto padrao.}
\item{xlabel }{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel }{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{posleg }{0 sem legenda,\cr
               1 para legenda no canto superior esquerdo,\cr
               2 para legenda no canto superior direito (default),\cr
               3 para legenda no canto inferior direito,\cr
               4 para legenda no canto inferior esquerdo.}
\item{boxleg}{Coloca moldura na legenda (default = TRUE).} 
\item{axes  }{Plota os eixos X e Y (default = TRUE).}
\item{size}{Tamanho dos pontos no grafico.}
\item{grid}{Coloca grade nos graficos (default = TRUE).}
\item{color}{Graficos coloridos (default = TRUE).}
\item{linlab}{Vetor com os rotulos para as observacoes.}
\item{class}{Vetor com os nomes das classes dos dados.}
\item{classcolor}{Vetor com as cores das classes.}
\item{savptc}{Salva as imagens dos graficos em arquivos (default = FALSE).}   
\item{width}{Largura do grafico quanto savptc = TRUE (defaul = 3236).}
\item{height}{Altura do grafico quanto savptc = TRUE (default = 2000).}
\item{res}{Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).}
}

\value{{Grafico de dispersao.}}

\references{
RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.

ANTON, H.; RORRES, C. \emph{Elementary linear algebra: applications version.} 10th ed. New Jersey: John Wiley & Sons, 2010. 768 p.
}

\author{
Paulo Cesar Ossani
}

\examples{
data(iris) # conjunto de dados

data <- iris[,3:4]

cls <- iris[,5] # classe dos dados

Scatter(data, ellipse = TRUE, ellipse.level = 0.95, rectangle = FALSE,  
        title = NA, xlabel = NA, ylabel = NA,  posleg = 1, boxleg = FALSE, 
        axes = FALSE, size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
        class = cls, classcolor = c("goldenrod3","blue","red"),
        savptc = FALSE, width = 3236, height = 2000, res = 300)

Scatter(data, ellipse = FALSE, ellipse.level = 0.95, rectangle = TRUE,  
        title = NA, xlabel = NA, ylabel = NA,  posleg = 1, boxleg = TRUE, 
        axes = FALSE, size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
        class = cls, classcolor = c("goldenrod3","blue","red"),
        savptc = FALSE, width = 3236, height = 2000, res = 300)
}

\keyword{Scatter Plot}
