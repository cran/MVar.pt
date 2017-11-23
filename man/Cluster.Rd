\name{Cluster}

\alias{Cluster}

\title{Analise de cluster.}

\description{Realiza analise de cluster hierarquico e nao hierarquico em um conjunto de dados.}

\usage{Cluster(Data, Hierarquico = TRUE, Analise = "Obs", CorAbs = FALSE, 
        Normaliza = FALSE, Distance = "euclidean", Metodo = "complete", 
        Horizontal = FALSE, NumGrupos = 0)}

\arguments{
\item{Data}{Dados a serem a analizados.}
\item{Hierarquico}{Agrupamentos hierarquicos (default = TRUE), para agrupamentos nao hierarquicos (metodo K-Means), somente para caso Analise = "Obs".}
\item{Analise}{"Obs" para analises nas observacoes (default), "Var" para analises nas variaveis.}
\item{CorAbs}{Matriz de correlacao absoluta caso Analise = "Var" (default = FALSE).}
\item{Normaliza}{Normalizar os dados somente para caso Analise = "Obs" (default = TRUE).}
\item{Distance}{Metrica das distancias caso agrupamentos hierarquicos: "euclidean" (default), "maximum", "manhattan", "canberra", "binary" ou "minkowski". Caso Analise = "Var" a metrica sera a matriz de correlacao, conforme CorAbs.}
\item{Metodo}{Metodo para analises caso agrupamentos hierarquicos: "complete" (default), "ward.D", "ward.D2", "single", "average", "mcquitty", "median" ou "centroid".}
\item{Horizontal}{Dendograma na horizontal (default = FALSE).}
\item{NumGrupos}{Numero de grupos a formar.}
}

\value{{Varios graficos.}
\item{TabRes}{Tabela com as similaridades e distancias dos grupos formados.}
\item{Groups}{Dados originais com os grupos formados.}
\item{ResGroups}{Resultados dos grupos formados.}
\item{SQT}{Soma do quadrado total.}
\item{MatrixD}{Matriz das distancias.}
}

\references{
MINGOTI, S. A. \emph{Analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

RENCHER, A. C. \emph{Methods of Multivariate Analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # conjunto de dados quantitativos

Dat <- DataQuan[,2:8]

rownames(Dat) <- DataQuan[1:nrow(DataQuan),1]

Res <- Cluster(Dat, Hierarquico = TRUE, Analise = "Obs", CorAbs = FALSE, 
       Normaliza = FALSE, Distance = "euclidean", Metodo = "ward.D", 
       Horizontal = FALSE, NumGrupos = 2)

print("Tabela com as similaridades e distancias:"); Res$TabRes
print("Grupos formados:"); Res$Groups
print("Tabela com os resultados dos grupos:"); Res$ResGroups
print("Soma do quadrado total:"); Res$SQT
print("Matriz de distancias:"); Res$MatrixD 

write.table(file="TabelaSimilaridade.csv", Res$TabRes, sep=";",dec=",",row.names = FALSE) 
write.table(file="DadosAgrupados.csv", Res$Groups, sep=";",dec=",",row.names = TRUE) 
write.table(file="ResultadosGrupos.csv", Res$ResGroups, sep=";",dec=",",row.names = TRUE) 
}

\keyword{Analise de Cluster}
\keyword{Cluster}
