\name{Biplot}

\alias{Biplot}

\title{Grafico Biplot.}

\description{Realiza o grafico Biplot.}

\usage{Biplot(data, alpha = 0.5, title = NA, xlabel = NA, ylabel = NA,
       size = 1.1, grid = TRUE, color = TRUE, obs = TRUE, linlab = NA)}

\arguments{
\item{data}{Dados para plotagem.}
\item{alpha}{Representatividade dos individuos (alpha),
            representatividade das variaveis (1 - alpha).
            Sendo 0.5 o default.}
\item{title}{Titulo para o grafico, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{size}{Tamanho dos pontos no grafico.}
\item{grid}{Coloca grade nos graficos (default = TRUE).}
\item{color}{Graficos coloridos (default = TRUE).}
\item{obs}{Acrescenta as observacoes ao grafico (default = TRUE).}
\item{linlab}{Vetor com os rotulos para as observacoes, se nao for definido assume texto padrao.}
}

\value{
\item{Biplot}{Grafico Biplot.}
\item{Md}{Matriz autovalores.}
\item{Mu}{Matriz U (autovetores).}
\item{Mv}{Matriz V (autovetores).}
\item{coorI}{Coordenadas dos individuos.}
\item{coorV}{Coordenadas das variaveis.}
\item{pvar}{Proporcao dos componentes principais.}
}

\references{
RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # conjunto de dados quantitativos

data <- DataQuan[,2:ncol(DataQuan)]
rownames(data) <- DataQuan[,1]

Biplot(data)

LinNames <- paste("C",1:nrow(data), sep="")
Biplot(data, alpha = 0.6, title = "Biplot dos Dados\n valorizando os individuos",
       xlabel = "Eixo X", ylabel = "Eixo Y", color = TRUE, obs = TRUE, 
       linlab = LinNames)
       
Biplot(data, alpha = 0.4, title = "Grafico valorizando as variaveis",
       xlabel = "", ylabel = "", color = FALSE, obs = FALSE)
}

\keyword{Biplot}
