DA <- function(data, class = NA, type = "lda", validation = "Learning", 
               method = "moment", prior = NA, testing = NA) {

  # Esta funcao executa a Analide discriminante linear e quadratica
  # desenvolvida por Paulo Cesar Ossani em 05/2019
  
  # Entrada:
  # data  - Dados a serem a classificados.
  # class - Vetor com os nomes das classes dos dados.
  # type  - "lda": analise discriminante linear (default), ou "qda": analise discriminante quadratica.
  # validation - Tipo de validacao: "Learning" - Treinamento dos dados (default), ou "testing" - classifica os dados do vetor "testing".
  # method  - Metodo de classificacao: "mle" para MLEs, "mve" para usar cov.mv, "moment" (default) para estimadores padrao da media e variancia ou "t" para estimativas robustas baseadas em uma distribuicao t.
  # prior   - Probabilidades de ocorrencia das classes. Se nao especificado, tomara as proporcoes das classes. Se especificado, as probabilidades devem seguir a ordem dos niveis dos fatores.
  # testing - Vetor com os indices que serao utilizados em data como teste. Para validation = "Learning", tem-se testing = NA.
  
  # Retorna:
  # confusion - Tabela de confusao.
  # error.rate - Proporcao global de erro.
  # prior - Probabilidade das classes.
  # type  - Tipo de analise discriminante.
  # validation - Tipo de validacao.
  # num.class	- Numero de classes.
  # class.names	- Nomes das classes.
  # method  - Metodo de classificacao.
  # num.correct - Numero de observacoes corretas.
  # results - Matriz com resultados comparativos das classificacoes.

  if (!is.data.frame(data)) 
     stop("Entrada 'data' esta incorreta, deve ser do tipo dataframe. Verifique!")
  
  if (!is.na(class[1])) {

     class <- as.matrix(class)

     if (nrow(data) != length(class))
        stop("Entrada 'class' ou 'data' esta incorreta, devem conter o mesmo numero de linhas. Verifique!")
  }
  
  type = tolower(type) # torna minusculo
  if (!(type %in% c("lda", "qda")))
     stop("Entrada para 'type' esta incorreta, deve ser: 'lda' ou 'qda'. Verifique!")
  
  if (!is.na(prior[1]) && sum(prior) != 1)
     stop("A soma dos elementos em 'prior' deve ser igual a um. Verifique!")
  
  validation = tolower(validation) # torna minusculo
  if (!(validation %in% c("learning","testing")))
     stop("Entrada para 'validation' esta incorreta, deve ser: 'Learning' ou 'testing'. Verifique!")
  
  if (validation == "testing" && is.na(testing[1]))
     stop("Entrada para validation = 'testing', deve-se acrescentar o vetor 'testing'. Verifique!")
  
  method = tolower(method) # torna minusculo
  if (!(method %in% c("mle","mve","moment","t")))
     stop("Entrada para 'method' esta incorreta, deve ser: 'mle', 'mve', 'moment' ou 't'. Verifique!")
  
  if (validation == "learning" && !is.na(testing[1]))
     stop("Para validation = 'learning', testing deve-se ser ingual a 'NA'. Verifique!")
  
  if (!is.na(class[1])) {
     class.Table <- table(class)        # cria tabela com as quantidade dos elementos das classes
     class.names <- names(class.Table)  # nomes das classses
     num.class   <- length(class.Table) # numero de classes
  } else {
     num.class <- 1
  }

  if (!is.na(prior[1]) && length(prior) != num.class)
     stop("O numero de elementos em 'prior' deve ser igual ao numero de classes. Verifique!")
  
  if (is.na(prior[1])) # caso probabilidade a priori nao seja informada
     prior <- as.double(rep(1,num.class)/num.class)
  
  if (validation == "learning")
     Learning = as.integer(rownames(data))
  
  if (validation == "testing" && !is.na(testing[1]))
     Learning = as.integer(rownames(data[-testing,]))
     
  ## Analise Discriminante Linear
  if (type == "lda") {
     disc <- MASS::lda(class~., data, prior = prior, method = method, subset = Learning)
  }
  
  ## Analise Discriminante quadratica
  if (type == "qda") {
     disc <- MASS::qda(class~., data, prior = prior, method = method, subset = Learning)
  }
  
  if (validation == "learning" || is.na(testing[1]))
     Learning = -Learning
  
  Predict <- predict(disc, data[-Learning,])$class
  
  Mclass <- cbind(as.vector(class[-Learning]), as.vector(Predict), " ")
  Mclass[Mclass[,1]!=Mclass[,2],3] = "*" # acrescenta * quando divergir
  Mclass <- as.data.frame(Mclass)
  colnames(Mclass) <- c("classes inicial", "classes predita", "Divergencia")

  confusion <- table(class[-Learning], Predict) # tabela de confunsao
  
  num.correct <- sum(diag(confusion)) # numero de observacoes corretas
  
  error_rate <- 1 - num.correct / sum(confusion) # taxa de erro
  
  total <- colSums(confusion)
  prop  <- round(diag(confusion)/total,4)
  confusion <- rbind(confusion, total) # total real
  confusion <- rbind(confusion, diag(confusion)) # total de acertos
  confusion <- as.table(rbind(confusion, as.character(prop)))
  rownames(confusion) <- c(colnames(confusion), "Total", "Numero de acertos", "Proporcao de acertos")
  
  Lista <- list(confusion = confusion, error.rate = error_rate, prior = disc$prior, type = type,
                num.class = num.class, class.names = class.names, method = method, 
                validation = validation, num.correct = num.correct, results = Mclass)

  return(Lista)
}
