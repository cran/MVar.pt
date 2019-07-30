\name{Cluster}

\alias{Cluster}

\title{Analise de cluster.}

\description{Realiza analise de cluster hierarquico e nao hierarquico em um conjunto de dados.}

\usage{Cluster(data, titles = NA, hierarquico = TRUE, analise = "Obs",  
        corabs = FALSE, normaliza = FALSE, distance = "euclidean",  
        method = "complete", horizontal = FALSE, numgrupos = 0,
        lambda = 2, casc = TRUE)}
        
\arguments{
\item{data}{Dados a serem analizados.}
\item{titles}{Titulos para os graficos, se nao for definido assume texto padrao.}
\item{hierarquico}{Agrupamentos hierarquicos (default = TRUE), para agrupamentos nao hierarquicos (method K-Means), somente para caso analise = "Obs".}
\item{analise}{"Obs" para analises nas observacoes (default), "Var" para analises nas variaveis.}
\item{corabs}{Matriz de correlacao absoluta caso analise = "Var" (default = FALSE).}
\item{normaliza}{Normaliza os dados somente para caso analise = "Obs" (default = TRUE).}
\item{distance}{Metrica das distancias caso agrupamentos hierarquicos: "euclidean" (default), "maximum", "manhattan", "canberra", "binary" ou "minkowski". Caso analise = "Var" a metrica sera a matriz de correlacao, conforme corabs.}
\item{method}{Metodo para analises caso agrupamentos hierarquicos: "complete" (default), "ward.D", "ward.D2", "single", "average", "mcquitty", "median" ou "centroid".}
\item{horizontal}{Dendrograma na horizontal (default = FALSE).}
\item{numgrupos}{Numero de grupos a formar.}
\item{lambda}{Valor usado na distancia de minkowski.}
\item{casc}{Efeito cascata na apresentacao dos graficos (default = TRUE).}
}

\value{{Varios graficos.}
\item{tabres}{Tabela com as similaridades e distancias dos grupos formados.}
\item{groups}{Dados originais com os grupos formados.}
\item{resgroups}{Resultados dos grupos formados.}
\item{sqt}{Soma do quadrado total.}
\item{mtxD}{Matriz das distancias.}
}

\references{
MINGOTI, S. A. \emph{analise de dados atraves de metodos de estatistica multivariada:} uma abordagem aplicada. Belo Horizonte: UFMG, 2005. 297 p.

FERREIRA, D. F. \emph{Estatistica Multivariada.} 2a ed. revisada e ampliada. Lavras: Editora UFLA, 2011. 676 p.

RENCHER, A. C. \emph{Methods of multivariate analysis.} 2th. ed. New York: J.Wiley, 2002. 708 p.
}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\examples{
data(DataQuan) # conjunto de dados quantitativos

data <- DataQuan[,2:8]

rownames(data) <- DataQuan[1:nrow(DataQuan),1]

Res <- Cluster(data, hierarquico = TRUE, analise = "Obs", corabs = FALSE, 
               normaliza = FALSE, distance = "euclidean", method = "ward.D", 
               horizontal = FALSE, numgrupos = 2)

print("Tabela com as similaridades e distancias:"); Res$tabres
print("Grupos formados:"); Res$groups
print("Tabela com os resultados dos grupos:"); Res$resgroups
print("Soma do quadrado total:"); Res$sqt
print("Matriz de distancias:"); Res$mtxD 

write.table(file=file.path(tempdir(),"SimilarityTable.csv"), Res$tabres, sep=";",
            dec=",",row.names = FALSE) 
write.table(file=file.path(tempdir(),"Groupeddata.csv"), Res$groups, sep=";",
            dec=",",row.names = TRUE) 
write.table(file=file.path(tempdir(),"GroupResults.csv"), Res$resgroups, sep=";",
            dec=",",row.names = TRUE) 
}

\keyword{analise de Cluster}
\keyword{Cluster}
