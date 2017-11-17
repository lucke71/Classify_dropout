\name{preprocess}
\alias{preprocess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Prepares dataframe for modeling with classify_dropout.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function transforms data created by "create_dropout_database". 
}
\usage{
preprocess(database)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{database}{
%%     ~~Describe \code{database} here~~
Dataframe that contains student data named according to "create_dropout_database".
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Changes attributes to factors and to numeric depending on their names. Removes undue spaces on character attributes. Furthermore, it creates a few attributes based on values of current attributes and removes attributes that possess little variance (according to caret::nearZeroVar).
}
\value{
Dataframe.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Lucas Assis
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link[HEdropout]{create_dropout_database}}
\code{\link[HEdropout]{classify_dropout}}
}
\examples{
library(HEdropout)

data("db_example")

db_example = preprocess(db_example)

lista = classify_dropout(db_example,preprocess="no")

modelo_treinado = lista[[1]]

predicoes = lista[[2]]

matriz_confusao = lista[[3]]


#if one chooses to predict the classes for a new database:

nova_base = sample_n(db_example, size=50)

predict(modelo_treinado,nova_base)


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
