\name{Plot.MFA}

\alias{Plot.MFA}

\title{Graficos da analise de multiplos fatores (MFA).}

\description{Graficos da analise de multiplos fatores (MFA).}

\usage{Plot.MFA(MFA, Titles = NA, xlabel = NA, ylabel = NA,
         PosLeg = 2, BoxLeg = TRUE, Color = TRUE, 
         NamArr = FALSE, LinLab = NA, Casc = TRUE)}
 
\arguments{
  \item{MFA}{Dados da funcao MFA.}
  \item{Titles}{Titulos para os graficos, se nao for definido assume texto padrao.}
  \item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
  \item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
  \item{PosLeg}{1 para legenda no canto superior esquerdo,\cr
                2 para legenda no canto superior direito (default),\cr
                3 para legenda no canto inferior direito,\cr
                4 para legenda no canto inferior esquerdo.}
  \item{BoxLeg}{Coloca moldura na legenda (default = TRUE).}
  \item{Color}{Graficos coloridos (default = TRUE).}
  \item{NamArr}{Coloca nomes nos pontos na nuvem ao redor do centroide no Grafico Correspondente a Analise Global dos Individuos e Variaveis (default = FALSE).}
  \item{LinLab}{Vetor com os rotulos para as observacoes, se nao for definido assume texto padrao.}
  \item{Casc}{Efeito cascata na apresentacao dos graficos (default = TRUE).}
}

\value{Retorna varios graficos.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{MFA}}}

\examples{
data(DataMix) # conjunto de dados mistos

Data <- DataMix[,2:ncol(DataMix)] 

rownames(Data) <- DataMix[1:nrow(DataMix),1]

GroupNames = c("Notas Cafes/Trabalho", "Formacao/Dedicacao", "Cafes")
           
MF <- MFA(Data, c(2,2,2), TypeGroups = c("n","c","f"), GroupNames) # realiza MFA

Tit = c("Scree-plot","Observacoes","Observacoes/Variaveis","Inercias dos Grupos Variaveis")

Plot.MFA(MF, Titles = Tit, xlabel = NA, ylabel = NA,
         PosLeg = 2, BoxLeg = FALSE, Color = TRUE, 
         NamArr = FALSE, LinLab = NA, 
         Casc = FALSE) # Plota varios graficos da tela

Plot.MFA(MF, Titles = Tit, xlabel = NA, ylabel = NA,
         PosLeg = 2, BoxLeg = FALSE, Color = TRUE, 
         NamArr = FALSE, LinLab = rep("A?",10), 
         Casc = FALSE) # Plota varios graficos da tela
}

\keyword{Analise de multiplos fatores}
\keyword{MFA}
\keyword{MFACT}
