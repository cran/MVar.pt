\name{Biplot}

\alias{Biplot}

\title{Grafico Biplot.}

\description{Realiza o grafico Biplot.}

\usage{Biplot(Data, alpha = 0.5, Title = NA, xlabel = NA, ylabel = NA,
       Color = TRUE, Obs = TRUE, LinLab = NA)}

\arguments{
\item{Data}{Dados para plotagem.}
\item{alpha}{Representatividade dos individuos (alpha),
            representatividade das variaveis (1 - alpha).
            Sendo 0.5 o default.}
\item{Title}{Titulo para o grafico, se nao for definido assume texto padrao.}
\item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
\item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
\item{Color}{Graficos coloridos (default = TRUE).}
\item{Obs}{Acrescenta as observacoes ao grafico (default = TRUE).}
\item{LinLab}{Vetor com os rotulos para as observacoes, se nao for definido assume texto padrao.}
}

\value{
\item{Biplot}{Grafico Biplot.}
\item{Md}{Matriz autovalores.}
\item{Mu}{Matriz U (autovetores).}
\item{Mv}{Matriz V (autovetores).}
\item{Coor_I}{Coordenadas dos individuos.}
\item{Coor_V}{Coordenadas das variaveis.}
\item{PVar}{Proporcao dos componentes principais.}
}

\references{
RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # conjunto de dados quantitativos

Dat <- DataQuan[,2:ncol(DataQuan)]
rownames(Dat) <- DataQuan[,1]

Biplot(Dat)

LinNames <- paste("C",1:nrow(Dat))
Biplot(Dat, alpha = 0.6, Title = "Biplot dos Dados\n valorizando os individuos",
       xlabel = "Eixo X", ylabel = "Eixo Y", Color = TRUE, Obs = TRUE, 
       LinLab = LinNames)
       
Biplot(Dat, alpha = 0.4, Title = "Grafico valorizando as variaveis",
       xlabel = "", ylabel = "", Color = FALSE, Obs = FALSE)
}

\keyword{Biplot}
