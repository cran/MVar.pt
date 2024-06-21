\name{GrandTour}

\alias{GrandTour}

\title{Tecnica de animacao Grand Tour.}

\description{Realiza a exploracao dos dados atraves da tecnica de animacao Grand Tour.}

\usage{GrandTour(data, method = "Interpolation", title = NA, xlabel = NA, 
          ylabel = NA, size = 1.1, grid = TRUE, color = TRUE, linlab = NA, 
          class = NA, classcolor = NA, posleg = 2, boxleg = TRUE,  
          axesvar = TRUE, axes = TRUE, numrot = 200, choicerot = NA, 
          savptc = FALSE, width = 3236, height = 2000, res = 300)}


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
\item{classcolor}{Vetor com as cores das classes.}
\item{posleg}{0 sem legenda,\cr
              1 para legenda no canto superior esquerdo,\cr
              2 para legenda no canto superior direito (default),\cr
              3 para legenda no canto inferior direito,\cr
              4 para legenda no canto inferior esquerdo.}
\item{boxleg}{Coloca moldura na legenda (default = TRUE).}
\item{axesvar }{Coloca eixos de rotacao das variaveis (default = TRUE).}
\item{axes    }{Plota os eixos X e Y (default = TRUE).}
\item{numrot  }{Numero de rotacoes (default = 200). Se method = "Interpolation", numrot representara o angulo de rotacao.}
\item{choicerot }{Escolhe rotacao especifica e apresenta na tela, ou salva a imagem se savptc = TRUE.}
\item{savptc}{Salva as imagens dos graficos em arquivos (default = FALSE).}   
\item{width}{Largura do grafico quanto savptc = TRUE (defaul = 3236).}
\item{height}{Altura do grafico quanto savptc = TRUE (default = 2000).}
\item{res}{Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).}
}

\value{{Graficos com as rotacoes.}
\item{proj.data  }{Dados projetados.}
\item{vector.opt }{Vetor projecao.}
\item{method     }{Metodo usado no Grand Tour.}
}

\references{
Asimov, D. The Grand Tour: A Tool for Viewing Multidimensional data. \emph{SIAM Journal of Scientific and Statistical Computing},  6(1), 128-143, 1985.

Asimov, D.; Buja, A. The grand tour via geodesic interpolation of 2-frames. in Visual data Exploration and Analysis. \emph{Symposium on Electronic Imaging Science and Technology}, IS&T/SPIE. 1994.

Buja, A.; Asimov, D. Grand tour methods: An outline. \emph{Computer Science and Statistics}, 17:63-67. 1986.

Buja, A.; Cook, D.; Asimov, D.; Hurley, C. Computational methods for High-Dimensional Rotations in data Visualization, in C. R. Rao, E. J. Wegman & J. L. Solka, eds, \emph{"Handbook of Statistics: data Mining and Visualization"}, Elsevier/North Holland, http://www.elsevier.com, pp. 391-413. 2005.

Hurley, C.; Buja, A. Analyzing high-dimensional data with motion graphics, \emph{SIAM Journal of Scientific and Statistical Computing}, 11 (6), 1193-1211. 1990.

Martinez, W.  L.; Martinez, A. R.; Solka, J.; \emph{Exploratory data Analysis with MATLAB}, 2th. ed. New York: Chapman & Hall/CRC, 2010. 499 p.

Young, F. W.; Rheingans P. Visualizing structure in high-dimensional multivariate data, \emph{IBM Journal of Research and Development}, 35:97-107, 1991.

Young, F. W.; Faldowski R. A.; McFarlane M. M. \emph{Multivariate statistical visualization, in Handbook of Statistics}, Vol 9, C. R. Rao (ed.), The Netherlands: Elsevier Science Publishers, 959-998, 1993.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(iris) # conjunto de dados

res <- GrandTour(iris[,1:4], method = "Torus", title = NA, xlabel = NA, ylabel = NA,
                 color = TRUE, linlab = NA, class = NA, posleg = 2, boxleg = TRUE, 
                 axesvar = TRUE, axes = FALSE, numrot = 10, choicerot = NA,
                 savptc = FALSE, width = 3236, height = 2000, res = 300)

print("Dados projetados:"); res$proj.data
print("Vetores de projecao:"); res$vector.opt
print("Metodo da projecao Grand Tour:"); res$method

          
res <- GrandTour(iris[,1:4], method = "Interpolation", title = NA, xlabel = NA, ylabel = NA,
                 color = TRUE, linlab = NA, posleg = 2, boxleg = FALSE, axesvar = FALSE, 
                 axes = FALSE, numrot = 10, choicerot = NA, class = iris[,5],
                 classcolor = c("goldenrod3","gray53","red"), savptc = FALSE, 
                 width = 3236, height = 2000, res = 300)
         
print("Dados projetados:"); res$proj.data
print("Vetores de projecao:"); res$vector.opt
print("Metodo da projecao Grand Tour:"); res$method

}

\keyword{Grand Tour}
