\name{HEdropout-package}
\alias{HEdropout-package}
\docType{package}
\title{
\packageTitle{HEdropout}
}
\description{
\packageDescription{HEdropout}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{HEdropout}
\packageIndices{HEdropout}
}
\author{
\packageAuthor{HEdropout}

Maintainer: \packageMaintainer{HEdropout}
}
\references{

}

\keyword{ package }
\seealso{
}
\examples{
library(HEdropout)

data("db_example")

lista = classify_dropout(db_example)

modelo_treinado = lista[[1]]

predicoes = lista[[2]]

matriz_confusao = lista[[3]]


#if one chooses to predict the classes for a new database:

nova_base = sample_n(db_example, size=50)

predict(modelo_treinado,nova_base)

}
