\name{Regr}

\alias{Regr}

\title{Regressao linear.}

\description{Realiza a regressao linear em um conjunto de dados.}

\usage{Regr(Y, X, namevarx = NA, intercepts = TRUE, sigf = 0.05)}

\arguments{
\item{Y}{Variaveis respotas.}
\item{X}{Variaveis regressoras.}
\item{namevarx}{Nome da variavel, ou variaveis X, se nao for definido assume texto padrao.}
\item{intercepts}{Considerar o intercepto na regressao (default = TRUE).}
\item{sigf}{Nivel de significancia dos testes dos residuos (default = 5\%).}
}

\value{
\item{Betas}{Coeficientes da regressao.}
\item{CovBetas}{Matriz de covariancias dos coeficientes da regressao.}
\item{ICc}{Intervalo de confianca dos coeficientes da regressao.}
\item{hip.test}{Teste de hipoteses dos coeficientes da regressao.}
\item{ANOVA}{Analise de variancia da regressao.}
\item{R}{Coeficiente de determinacao.}
\item{Rc}{Coeficiente de determinacao corrigido.}
\item{Ra}{Coeficiente de determinacao ajustado.}
\item{QME}{Variancia dos residuos.}
\item{ICQME}{Intervalo de confianca da variancia dos residuos.}
\item{prev}{Previsao do ajuste da regressao.}
\item{IPp}{Intervalo das previsoes.}
\item{ICp}{Intervalo de confianca das previsoes.}
\item{error}{Residuos do ajuste da regressao.}
\item{error.test}{Retorna a 5\% de significancia o teste de independencia, de   normalidade e de homogeneidade da variancia dos residuos.}
}

\references{
CHARNET, R.; at al.. \emph{Analise de modelos de regressao lienar,} 2a ed. Campinas: Editora da Unicamp, 2008. 357 p.

RENCHER, A. C.; SCHAALJE, G. B. \emph{Linear models in statisctic.} 2th. ed. New Jersey: John & Sons, 2008. 672 p.

RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani
}

\seealso{\code{\link{Plot.Regr}}}

\examples{
data(DataMix)

Y <- DataMix[,2]

X <- DataMix[,6:7]

name.x <- c("Cafes Expeciais","Cafes Comerciais")

res <- Regr(Y, X, namevarx = name.x , intercepts = TRUE, sigf = 0.05)

print("Coeficientes da Regressao:"); round(res$Betas,4)
print("Analise de Variancia:"); res$ANOVA
print("Teste de Hipoteses dos Coeficientes da Regressao:"); round(res$hip.test,4)
print("Coeficiente de Determinacao:"); round(res$R,4)
print("Coeficiente de Determinacao Corrigido:"); round(res$Rc,4) 
print("Coeficiente de Determinacao Ajustado:"); round(res$Ra,4)
print("Testes dos Residuos:"); res$error.test
}

\keyword{Regressao}
