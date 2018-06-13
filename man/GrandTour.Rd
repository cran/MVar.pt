\name{GrandTour}

\alias{GrandTour}

\title{Tecnica de animacao Grand Tour.}

\description{Realiza a exploracao dos dados atraves da tecnica de animacao Grand Tour.}

\usage{GrandTour(Data, Method = "Interpolation", Title = NA, 
          xlabel = NA, ylabel = NA, Color = TRUE, Label = FALSE, 
          LinLab = NA, AxisVar = TRUE, Axis = FALSE,
          NumRot = 200, ChoiceRot = NA, SavePicture = FALSE)}

\arguments{
\item{Data    }{Conjunto de dados numericos.}
\item{Method  }{Metodo usado para as rotacoes:\cr
                "Interpolation" - Metodo Interpolation (default),\cr
                "Torus" - Metodo Torus,\cr
                "Pseudo" - Metodo Pseudo Grand Tour.}
\item{Title }{Titulo para os graficos, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{Color }{Graficos coloridos (default = TRUE).}
\item{Label }{Coloca os rotulos das observacoes (default = FALSE).}
\item{LinLab}{Vetor com os rotulos para as observacoes, se nao for definido assume texto padrao.}
\item{AxisVar }{Coloca eixos de rotacao das variaveis (default = TRUE).}
\item{Axis    }{Plota os eixos X e Y (default = FALSE).}
\item{NumRot  }{Numero de rotacoes (default = 200). Se Method = "Interpolation", NumRot representara o angulo de rotacao.}
\item{ChoiceRot }{Escolhe rotacao especifica e apresenta na tela, ou salva a imagem se SavePicture = TRUE.}
\item{SavePicture}{Salva as imagens dos graficos em arquivos (default = FALSE).}          
}

\value{{Graficos com as rotacoes.}
\item{Proj.Data  }{Dados projetados.}
\item{Vector.Opt }{Vetor projecao.}
\item{Method     }{Metodo usado no Grand Tour.}
}

\references{
ASIMOV, D. The Grand Tour: A Tool for Viewing Multidimensional Data. \emph{SIAM Journal of Scientific and Statistical Computing},  6(1), 128-143, 1985.

ASIMOV, D.; BUJA, A. The grand tour via geodesic interpolation of 2-frames. in Visual Data Exploration and Analysis. \emph{Symposium on Electronic Imaging Science and Technology}, IS&T/SPIE. 1994.

BUJA, A. ; ASIMOV, D. Grand tour methods: An outline. \emph{Computer Science and Statistics}, 17:63-67. 1986.

BUJA, A.; COOK, D.; ASIMOV, D.; HURLEY, C. Computational Methods for High-Dimensional Rotations in Data Visualization, in C. R. Rao, E. J. Wegman & J. L. Solka, eds, \emph{"Handbook of Statistics: Data Mining and Visualization"}, Elsevier/North Holland, http://www.elsevier.com, pp. 391-413. 2005.

HURLEY, C.; BUJA, A. Analyzing high-dimensional data with motion graphics, \emph{SIAM Journal of Scientific and Statistical Computing}, 11 (6), 1193-1211. 1990.

MARTINEZ, W.  L., MARTINEZ, A. R., SOLKA, J.; \emph{Exploratory Data Analysis with MATLAB}, 2th. ed. New York: Chapman & Hall/CRC, 2010. 499 p.

YOUNG, F. W.; RHEINGANS P.  Visualizing structure in high-dimensional multivariate data, \emph{IBM Journal of Research and Development}, 35:97-107, 1991.

YOUNG, F. W.; FALDOWSKI R. A.; McFARLANE M. M. \emph{Multivariate statistical visualization, in Handbook of Statistics}, Vol 9, C. R. Rao (ed.), The Netherlands: Elsevier Science Publishers, 959-998, 1993.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(iris) # conjunto de dados

Dat <- iris[,1:4]

Dat <- as.data.frame(NormData(Dat, 2))

Res <- GrandTour(Dat, Method = "Interpolation", Title = NA, Color = TRUE,
                 Label = FALSE, LinLab = NA, AxisVar = TRUE, Axis = TRUE,
                 NumRot = 10, ChoiceRot = NA, SavePicture = FALSE)

print("Dados projetados:"); Res$Proj.Data
print("Vetores de projecao:"); Res$Vector.Opt
print("Metodo da projecao Grand Tour:"); Res$Method

}

\keyword{Grand Tour}
