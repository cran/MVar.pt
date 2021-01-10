\name{MFA}

\alias{MFA}

\title{Analise de multiplos fatores (MFA).}

\description{Realiza analise de multiplos fatores (MFA) em grupos de variaveis. Os grupos de variaveis podem ser dados quantitativos, qualitativos, de frequencia (MFACT), ou dados mistos.}

\usage{MFA(data, groups, typegroups = rep("n", length(groups)), namegroups = NULL)}

\arguments{
  \item{data}{Dados a serem analisados.}
  \item{groups}{Numero de colunas para cada grupo em ordem seguindo a ordem dos dados em 'data'.}
  \item{typegroups}{Tipo de grupo:\cr
                    "n" para dados numericos (default),\cr
                    "c" para dados categoricos,\cr
                    "f" para dados de frequencia.}
  \item{namegroups}{Nomes para cada grupo.}
}

\value{
\item{vtrG}{Vetor com os tamanhos de cada grupo.}
\item{vtrNG}{Vetor com os nomes de cada grupo.}
\item{vtrplin}{Vetor com os valores usados para balancear as linhas da matriz Z.}
\item{vtrpcol}{Vetor com os valores usados para balancear as colunas da matriz Z.}
\item{mtxZ}{Matriz concatenada e balanceada.}
\item{mtxA}{Matriz de autovalores (variancias) com as proporcoes e proporcoes acumuladas.}
\item{mtxU}{Matriz U da decomposicao singular da matriz Z.}
\item{mtxV}{Matriz V da decomposicao singular da matriz Z.}
\item{mtxF}{Matriz global dos escores dos fatores onde as linhas sao as observacoes e as colunas os componentes.}
\item{mtxEFG}{Matriz dos escores dos fatores por grupo.}
\item{mtxCCP}{Matriz de correlacao dos componentes principais com as variaveis originais.}
\item{mtxEV}{Matriz das inercias parciais/escores das variaveis.}
}

\references{
ABDESSEMED, L.; ESCOFIER, B. Analyse factorielle multiple de tableaux de frequencies: comparaison avec l'analyse canonique des correspondences. \emph{Journal de la Societe de Statistique de Paris}, Paris, v. 137, n. 2, p. 3-18, 1996..

ABDI, H. Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In: SALKIND, N. J. (Ed.). \emph{Encyclopedia of measurement and statistics.} Thousand Oaks: Sage, 2007. p. 907-912.

ABDI, H.; VALENTIN, D. Multiple factor analysis (MFA). In: SALKIND, N. J. (Ed.). \emph{Encyclopedia of measurement and statistics.} Thousand Oaks: Sage, 2007. p. 657-663.

ABDI, H.; WILLIAMS, L. Principal component analysis. \emph{WIREs Computational Statatistics}, New York, v. 2, n. 4, p. 433-459, July/Aug. 2010.

ABDI, H.; WILLIAMS, L.; VALENTIN, D. Multiple factor analysis: principal component analysis for multitable and multiblock data sets. \emph{WIREs Computational Statatistics}, New York, v. 5, n. 2, p. 149-179, Feb. 2013.

BECUE-BERTAUT, M.; PAGES, J. A principal axes method for comparing contingency tables: MFACT. \emph{Computational Statistics & data Analysis}, New York, v. 45, n. 3, p. 481-503, Feb. 2004

BECUE-BERTAUT, M.; PAGES, J. Multiple factor analysis and clustering of a mixture of quantitative, categorical and frequency data. \emph{Computational Statistics & data Analysis}, New York, v. 52, n. 6, p. 3255-3268, Feb. 2008.

BENZECRI, J. Analyse de l'inertie intraclasse par l'analyse d'un tableau de contingence: intra-classinertia analysis through the analysis of a contingency table. \emph{Les Cahiers de l'Analyse des Donnees}, Paris, v. 8, n. 3, p. 351-358, 1983.

ESCOFIER, B. Analyse factorielle en reference a un modele: application a l'analyse d'un tableau d'echanges. \emph{Revue de Statistique Appliquee}, Paris, v. 32, n. 4, p. 25-36, 1984.

ESCOFIER, B.; DROUET, D. Analyse des differences entre plusieurs tableaux de frequence. \emph{Les Cahiers de l'Analyse des Donnees}, Paris, v. 8, n. 4, p. 491-499, 1983.

ESCOFIER, B.; PAGES, J. \emph{Analyse factorielles simples et multiples.} Paris: Dunod, 1990. 267 p.

ESCOFIER, B.; PAGES, J. \emph{Analyses factorielles simples et multiples:} objectifs, methodes et interpretation. 4th ed. Paris: Dunod, 2008. 318 p.

ESCOFIER, B.; PAGES, J. \emph{Comparaison de groupes de variables definies sur le meme ensemble d'individus:} un exemple d'applications. Le Chesnay: Institut National de Recherche en Informatique et en Automatique, 1982. 121 p.

ESCOFIER, B.; PAGES, J. Multiple factor analysis (AFUMULT package). \emph{Computational Statistics & data Analysis}, New York, v. 18, n. 1, p. 121-140, Aug. 1994

GREENACRE, M.; BLASIUS, J. \emph{Multiple correspondence analysis and related methods.} New York: Taylor and Francis, 2006. 607 p.

OSSANI, P. C.; CIRILLO, M. A.; BOREM, F. M.; RIBEIRO, D. E.; CORTEZ, R. M.. Quality of specialty coffees: a sensory evaluation by consumers using the MFACT technique. \emph{Revista Ciencia Agronomica (UFC. Online)}, v. 48, p. 92-100, 2017.

PAGES, J. Analyse factorielle multiple appliquee aux variables qualitatives et aux donnees mixtes. \emph{Revue de Statistique Appliquee}, Paris, v. 50, n. 4, p. 5-37, 2002.

PAGES, J.. Multiple factor analysis: main features and application to sensory data. \emph{Revista Colombiana de Estadistica}, Bogota, v. 27, n. 1, p. 1-26, 2004.

}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{Plot.MFA}}}

\examples{
data(DataMix) # conjunto de dados mistos

data <- DataMix[,2:ncol(DataMix)] 

rownames(data) <- DataMix[1:nrow(DataMix),1]

group.names = c("Notas Cafes/Trabalho", "Formacao/Dedicacao", "Cafes")

mf <- MFA(data = data, c(2,2,2), typegroups = c("n","c","f"), group.names) # realiza MFA

print("Variancias dos Componentes Principais:"); round(mf$mtxA,2)

print("Matriz das Inercias Parciais/Escores das Variaveis:"); round(mf$mtxEV,2)
}

\keyword{Analise de multiplos fatores}
\keyword{MFA}
\keyword{MFACT}