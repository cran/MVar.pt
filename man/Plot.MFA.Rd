\name{Plot.MFA}

\alias{Plot.MFA}

\title{Graficos da analise de multiplos fatores (MFA).}

\description{Graficos da analise de multiplos fatores (MFA).}

\usage{Plot.MFA(MFA, titles = NA, xlabel = NA, ylabel = NA,
         posleg = 2, boxleg = TRUE, size = 1.1, grid = TRUE, 
         color = TRUE, groupscolor = NA, namarr = FALSE, 
         linlab = NA, savptc = FALSE, width = 3236, 
         height = 2000, res = 300, casc = TRUE)}
 
\arguments{
  \item{MFA}{Dados da funcao MFA.}
  \item{titles}{Titulos para os graficos, se nao for definido assume texto padrao.}
  \item{xlabel}{Nomeia o eixo X, se nao for definido assume texto padrao.}
  \item{ylabel}{Nomeia o eixo Y, se nao for definido assume texto padrao.}
  \item{posleg}{1 para legenda no canto superior esquerdo,\cr
                2 para legenda no canto superior direito (default),\cr
                3 para legenda no canto inferior direito,\cr
                4 para legenda no canto inferior esquerdo.}
  \item{boxleg}{Coloca moldura na legenda (default = TRUE).}
  \item{size}{Tamanho dos pontos no grafico.}
  \item{grid}{Coloca grade nos graficos (default = TRUE).}
  \item{color}{Graficos coloridos (default = TRUE).}
  \item{groupscolor}{Vetor com as cores dos grupos.}
  \item{namarr}{Coloca nomes nos pontos na nuvem ao redor do centroide no grafico correspondente a analise global dos individuos e variaveis (default = FALSE).}
  \item{linlab}{Vetor com os rotulos para as observacoes, se nao for definido assume texto padrao.}
\item{savptc}{Salva as imagens dos graficos em arquivos (default = FALSE).}   
\item{width}{Largura do grafico quanto savptc = TRUE (defaul = 3236).}
\item{height}{Altura do grafico quanto savptc = TRUE (default = 2000).}
\item{res}{Resolucao nominal em ppi do grafico quanto savptc = TRUE (default = 300).}
\item{casc}{Efeito cascata na apresentacao dos graficos (default = TRUE).}
}

\value{Retorna varios graficos.}

\author{
Paulo Cesar Ossani

Marcelo Angelo Cirillo
}

\seealso{\code{\link{MFA}}}

\examples{
data(DataMix) # conjunto de dados mistos

data <- DataMix[,2:ncol(DataMix)] 

rownames(data) <- DataMix[1:nrow(DataMix),1]

group.names = c("Notas Cafes/Trabalho", "Formacao/Dedicacao", "Cafes")
           
mf <- MFA(data, c(2,2,2), typegroups = c("n","c","f"), group.names) # realiza MFA

tit <- c("Scree-plot","Observacoes","Observacoes/Variaveis",
        "Circulo de Correlacoes","Inercias dos Grupos Variaveis")

Plot.MFA(MFA = mf, titles = tit, xlabel = NA, ylabel = NA,
         posleg = 2, boxleg = FALSE, color = TRUE, 
         groupscolor = c("blue3","red","goldenrod3"), 
         namarr = FALSE, linlab = NA, savptc = FALSE,
         width = 3236, height = 2000, res = 300, 
         casc = FALSE) # Plota varios graficos da tela

Plot.MFA(MFA = mf, titles = tit, xlabel = NA, ylabel = NA,
         posleg = 2, boxleg = FALSE, color = TRUE, 
         namarr = FALSE, linlab = rep("A?",10), 
         savptc = FALSE, width = 3236, height = 2000, 
         res = 300, casc = FALSE) # Plota varios graficos da tela
}

\keyword{Analise de multiplos fatores}
\keyword{MFA}
\keyword{MFACT}
