\name{Biplot}

\alias{Biplot}

\title{Grafico Biplot}

\description{Realiza o grafico Biplot.}

\usage{Biplot(Data, alfa = 0.5, Title = NA, Label_x = NA, Label_y = NA, Color = "s")}

\arguments{
\item{Data}{Dados para plotagem.}
\item{alfa}{Representatividade dos individuos (alfa),
            representatividade das variaveis (1 - alfa).\cr
            Sendo 0.5 o default.}
\item{Title}{Titulo para o grafico. Se nao for definido assume texto padrao.}
\item{Label_x}{Rotulo do eixo X. Se nao for definido assume texto padrao.}
\item{Label_y}{Rotulo do eixo Y. Se nao for definido assume texto padrao.}
\item{Color}{"s" para graficos coloridos - default,\cr
             "n" para graficos em preto e branco.}
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
RENCHER, A.C.; \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
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

Biplot(Dat, alfa = 0.6, Title = "Biplot dos Dados\n valorizando os individuos",
       Label_x = "Eixo X", Label_y = "Eixo Y", Color = "s")
       
Biplot(Dat, alfa = 0.4, Title = "Grafico valorizando as variaveis",
       Label_x = "", Label_y = "", Color = "n")
}

\keyword{Biplot}
