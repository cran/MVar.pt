\name{GrandTour}

\alias{GrandTour}

\title{Tecnica de animacao Grand Tour.}

\description{Realiza a exploracao dos dados atraves da tecnica de animacao Grand Tour.}

\usage{GrandTour(data, method = "Interpolation", title = NA, xlabel = NA, 
          ylabel = NA, size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
          class = NA, posleg = 2, boxleg = TRUE, axesvar = TRUE, axes = FALSE, 
          numrot = 200, choicerot = NA, savepicture = FALSE)}

\arguments{
\item{data    }{Conjunto de dados numericos.}
\item{method  }{Metodo usado para as rotacoes:\cr
                "Interpolation" - Metodo Interpolation (default),\cr
                "Torus" - Metodo Torus,\cr
                "Pseudo" - Metodo Pseudo Grand Tour.}
\item{title }{Titulo para os graficos, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{size}{Tamanho dos pontos no grafico.}
\item{grid}{Coloca grade nos graficos (default = TRUE).}
\item{color}{Graficos coloridos (default = TRUE).}
\item{linlab}{Vetor com os rotulos para as observacoes.}
\item{class}{Vetor com os nomes das classes dos dados.}
\item{posleg}{0 sem legenda,\cr
              1 para legenda no canto superior esquerdo,\cr
              2 para legenda no canto superior direito (default),\cr
              3 para legenda no canto inferior direito,\cr
              4 para legenda no canto inferior esquerdo.}
\item{boxleg}{Coloca moldura na legenda (default = TRUE).}
\item{axesvar }{Coloca eixos de rotacao das variaveis (default = TRUE).}
\item{axes    }{Plota os eixos X e Y (default = FALSE).}
\item{numrot  }{Numero de rotacoes (default = 200). Se method = "Interpolation", numrot representara o angulo de rotacao.}
\item{choicerot }{Escolhe rotacao especifica e apresenta na tela, ou salva a imagem se savepicture = TRUE.}
\item{savepicture}{Salva as imagens dos graficos em arquivos (default = FALSE).}          
}

\value{{Graficos com as rotacoes.}
\item{proj.data  }{Dados projetados.}
\item{vector.opt }{Vetor projecao.}
\item{method     }{Metodo usado no Grand Tour.}
}

\references{
ASIMOV, D. The Grand Tour: A Tool for Viewing Multidimensional data. \emph{SIAM Journal of Scientific and Statistical Computing},  6(1), 128-143, 1985.

ASIMOV, D.; BUJA, A. The grand tour via geodesic interpolation of 2-frames. in Visual data Exploration and Analysis. \emph{Symposium on Electronic Imaging Science and Technology}, IS&T/SPIE. 1994.

BUJA, A. ; ASIMOV, D. Grand tour methods: An outline. \emph{Computer Science and Statistics}, 17:63-67. 1986.

BUJA, A.; COOK, D.; ASIMOV, D.; HURLEY, C. Computational methods for High-Dimensional Rotations in data Visualization, in C. R. Rao, E. J. Wegman & J. L. Solka, eds, \emph{"Handbook of Statistics: data Mining and Visualization"}, Elsevier/North Holland, http://www.elsevier.com, pp. 391-413. 2005.

HURLEY, C.; BUJA, A. Analyzing high-dimensional data with motion graphics, \emph{SIAM Journal of Scientific and Statistical Computing}, 11 (6), 1193-1211. 1990.

MARTINEZ, W.  L., MARTINEZ, A. R., SOLKA, J.; \emph{Exploratory data Analysis with MATLAB}, 2th. ed. New York: Chapman & Hall/CRC, 2010. 499 p.

YOUNG, F. W.; RHEINGANS P.  Visualizing structure in high-dimensional multivariate data, \emph{IBM Journal of Research and Development}, 35:97-107, 1991.

YOUNG, F. W.; FALDOWSKI R. A.; McFARLANE M. M. \emph{Multivariate statistical visualization, in Handbook of Statistics}, Vol 9, C. R. Rao (ed.), The Netherlands: Elsevier Science Publishers, 959-998, 1993.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(iris) # conjunto de dados

Res <- GrandTour(iris[,1:4], method = "Torus", title = NA, xlabel = NA, ylabel = NA,
                 color = TRUE, linlab = NA, class = NA, posleg = 2, boxleg = TRUE, 
                 axesvar = TRUE, axes = FALSE, numrot = 10, choicerot = NA,
                 savepicture = FALSE)

print("Dados projetados:"); Res$proj.data
print("Vetores de projecao:"); Res$vector.opt
print("Metodo da projecao Grand Tour:"); Res$method

          
Res <- GrandTour(iris[,1:4], method = "Interpolation", title = NA, xlabel = NA, ylabel = NA,
                 color = TRUE, linlab = NA, class = iris[,5], posleg = 2, boxleg = FALSE, 
                 axesvar = FALSE, axes = FALSE, numrot = 10, choicerot = NA,
                 savepicture = FALSE)
          
print("Dados projetados:"); Res$proj.data
print("Vetores de projecao:"); Res$vector.opt
print("Metodo da projecao Grand Tour:"); Res$method

}

\keyword{Grand Tour}
