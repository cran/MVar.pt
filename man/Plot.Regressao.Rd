\name{Plot.Regressao}

\alias{Plot.Regressao}

\title{Graficos dos resultados da regressao linear.}

\description{Graficos dos resultados da regressao linear.}

\usage{Plot.Regressao(Reg, TypeGraf = "Scatterplot", Title = NA,
               xlabel = NA, ylabel = NA, NameVarY = NA, 
               NameVarX = NA,  Color = TRUE, IntConf = TRUE, 
               IntPrev = TRUE)}

\arguments{
\item{Reg}{Dados da funcao de regressao.}
\item{TypeGraf}{Tipo de grafico:\cr
                "Scatterplot" - Grafico de dispersao 2 a 2,\cr
                "Regression"  - Grafico da regressao linear,\cr
                "QQPlot"      - Grafico de probabilidade normal dos residuos,\cr
                "Histogram"   - Histograma dos residuos,\cr
                "Fits"        - Grafico dos valores ajustados versus os residuos,\cr
                "Order"       - Grafico da ordem das observacoes versus os residuos.}
\item{Title}{Titulos para os graficos, se nulo retorna padrao.}
\item{xlabel}{Nomeia o eixo X, se nulo retorna padrao.}
\item{ylabel}{Nomeia o eixo Y, se nulo retorna padrao.}
\item{NameVarY}{Nome da variavel Y, se nulo retorna padrao.}
\item{NameVarX}{Nome da variavel, ou variaveis X, se nulo retorna padrao.}
\item{Color}{Graficos coloridos (default = TRUE).}
\item{IntConf}{Caso TypeGraf = "Regression". Graficos com intervalo de confianca (default = TRUE). }
\item{IntPrev}{Caso TypeGraf = "Regression". Graficos com intervalo de previsao (default = TRUE)}
}

\value{Retorna varios graficos.}

\author{
Paulo Cesar Ossani
}

\seealso{\code{\link{Regressao}}}

\examples{
data(DataMix)

Y <- DataMix[,2]

X <- DataMix[,7]

NomeY <- "Medias notas"

NomeX <- "Cafes Comerciais"

Res <- Regressao(Y, X, NameVarX = NomeX ,Intercepts = TRUE, SigF = 0.05)

Titulos <- c("Scatterplot")
Plot.Regressao(Res, TypeGraf = "Scatterplot", Title = Titulos,
               NameVarY = NomeY, NameVarX = NomeX, Color = TRUE)

Titulos <- c("Grafico de Dispersao com a \n reta ajustada")
Plot.Regressao(Res, TypeGraf = "Regression", Title = Titulos, xlabel = NomeX, 
               ylabel = NomeY, Color = TRUE, IntConf = TRUE, IntPrev = TRUE)

par (mfrow = c(2,2)) 

Plot.Regressao(Res, TypeGraf = "QQPlot")
Plot.Regressao(Res, TypeGraf = "Histogram")
Plot.Regressao(Res, TypeGraf = "Fits")
Plot.Regressao(Res, TypeGraf = "Order")
}

\keyword{Regressao}
