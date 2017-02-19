\name{Plot.MFA}

\alias{Plot.MFA}

\title{Graficos da analise de multiplos fatores (MFA).}

\description{Graficos da nalise de multiplos fatores (MFA).}

\usage{Plot.MFA(MFA, Titles = matrix(NA,1,4), PosLeg = 2, 
         BoxLeg = "s", Color = "s", NamArr = "n")}
 
\arguments{
  \item{MFA}{Dados da funcao MFA.}
  \item{Titles}{Titulos para os graficos. Se nao for definido assume texto padrao.}
  \item{PosLeg}{1 para legenda no canto superior esquerdo,\cr
                2 para legenda no canto superior direito (default),\cr
                3 para legenda no canto inferior direito,\cr
                4 para legenda no canto inferior esquerdo.}
  \item{BoxLeg}{"s" para colocar moldura na legenda (default).\cr
                "n" nao coloca moldura na legenda.}
  \item{Color}{"s" para graficos coloridos (default),\cr
               "n" para graficos em preto e branco.}
  \item{NamArr}{"s" para colocar nomes nos pontos na nuvem ao redor do
                    centroide no grafico correspondente a 
                    analise global dos individuos e variaveis,\cr
                "n" caso contrario (default).}
}

\value{Retorna varios graficos.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{MFA}}}

\examples{
data(DataMix) # conjunto de dados mistos

Dat <- DataMix[,2:ncol(DataMix)] 

rownames(Dat) <- DataMix[1:nrow(DataMix),1]

GroupNames = c("Notas Cafes/Trabalho", "Formacao/Dedicacao", "Cafes")
           
MF <- MFA(Dat, c(2,2,2), TipoGrupo = c("n","c","f"), GroupNames) # realiza MFA

Titulos = c("Scree-plot","Observacoes","Observacoes/Variaveis","Inercias Grupos Variaveis")

Plot.MFA(MF, Titulos, 2, "n", "s", "n") # Imprime varios graficos da tela
}

\keyword{Analise de multiplos fatores}
\keyword{MFA}
\keyword{MFACT}
