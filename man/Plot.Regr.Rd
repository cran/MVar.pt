\name{Plot.Regr}

\alias{Plot.Regr}

\title{Graficos dos resultados da regressao linear.}

\description{Graficos dos resultados da regressao linear.}

\usage{Plot.Regr(Reg, typegraf = "Scatterplot", title = NA, xlabel = NA, 
          ylabel = NA, namevary = NA, namevarx = NA, size = 1.1, 
          grid = TRUE, color = TRUE, intconf = TRUE, intprev = TRUE, 
          casc = TRUE)}

\arguments{
\item{Reg}{Dados da funcao de regressao.}
\item{typegraf}{Tipo de grafico:\cr
                "Scatterplot" - Grafico de dispersao 2 a 2,\cr
                "Regression"  - Grafico da regressao linear,\cr
                "QQPlot"      - Grafico de probabilidade normal dos residuos,\cr
                "Histogram"   - Histograma dos residuos,\cr
                "Fits"        - Grafico dos valores ajustados versus os residuos,\cr
                "Order"       - Grafico da ordem das observacoes versus os residuos.}
\item{title}{Titulos para os graficos, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{namevary}{Nome da variavel Y, se nao for definido assume texto padrao.}
\item{namevarx}{Nome da variavel X, ou variaveis X, se nao for definido assume texto padrao.}
\item{size}{Tamanho dos pontos no grafico.}
\item{grid}{Coloca grade nos graficos (default = TRUE).}
\item{color}{Graficos coloridos (default = TRUE).}
\item{intconf}{Caso typegraf = "Regression". Graficos com intervalo de confianca (default = TRUE). }
\item{intprev}{Caso typegraf = "Regression". Graficos com intervalo de previsao (default = TRUE)}
\item{casc}{Efeito cascata na apresentacao dos graficos (default = TRUE).}
}

\value{Retorna varios graficos.}

\author{
Paulo Cesar Ossani
}

\seealso{\code{\link{Regr}}}

\examples{
data(DataMix)

Y <- DataMix[,2]

X <- DataMix[,7]

NomeY <- "Media das notas"

NomeX <- "Cafes comerciais"

Res <- Regr(Y, X, namevarx = NomeX ,intercepts = TRUE, sigf = 0.05)

Tit <- c("Scatterplot")
Plot.Regr(Res, typegraf = "Scatterplot", title = Tit,
          namevary = NomeY, namevarx = NomeX, color = TRUE)

Tit <- c("Grafico de Dispersao com a \n reta ajustada")
Plot.Regr(Res, typegraf = "Regression", title = Tit, 
          xlabel = NomeX, ylabel = NomeY, color = TRUE,
          intconf = TRUE, intprev = TRUE)

dev.new() # necessario para nao sobrepor os graficos seguintes ao grafico anterior

par(mfrow = c(2,2)) 

Plot.Regr(Res, typegraf = "QQPlot", casc = FALSE)
Plot.Regr(Res, typegraf = "Histogram", casc = FALSE)
Plot.Regr(Res, typegraf = "Fits", casc = FALSE)
Plot.Regr(Res, typegraf = "Order", casc = FALSE)
}

\keyword{Regressao}
