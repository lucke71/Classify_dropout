\name{classify_dropout}
\alias{classify_dropout}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Transforms dataframe and classifies data using multiple classifiers.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function transforms data created by "create_dropout_database" and classifies dropout students according to the users choosing. 
}
\usage{
classify_dropout(database, cv_folds = 4, pct_training = 0.75, classifier = "CART", num_cores = 2, balance = "down", preprocess="yes")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{database}{
%%     ~~Describe \code{database} here~~
Dataframe that contains student data named according to "create_dropout_database".
}
  \item{cv_folds}{
%%     ~~Describe \code{cv_folds} here~~
Number of folds to be used in cross-validation. Only numbers greater than 0 (zero).
}
  \item{pct_training}{
%%     ~~Describe \code{pct_training} here~~
Percentage of the original database to be used for training the classifiers. Only numbers between 0 and 1 are accepted.
}
  \item{classifier}{
%%     ~~Describe \code{classifier} here~~
String contaning the name of classifier to be used. Options are "CART", "C50", "Nnet", "reglog", "NB" and "all". See details.
}
  \item{num_cores}{
%%     ~~Describe \code{num_cores} here~~
Number of cpu machine cores to be used. Only works when runnning R in a Unix system.
}
  \item{balance}{
%%     ~~Describe \code{balance} here~~
String containing the name of the technique to balance data. Options are "up", "down" and "no". See details.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The CART classifier is called from the caret's package using the "rpart" method with tunelength equal to 10.
The C5.0 classifier (C50) is called from the caret's package using the "C50" method with a grid that produces 10 different configurations.
The neural net classifier (Nnet) is called from the caret's package using the "nnet" method with maximum iterations equal to 1000.
The logistic regression classifier (reglog) is called from the caret's package using the "glm" method and the "logit" link.
The naive bayes classifier (NB) is called from the caret's package using the "nb" method.
All classifiers are using the specificity metric to find the best fitting model.

For balancing, simple random sample is used to downsample the data to the same amount of dropout students. For up-sampling, simple random sampling with replacement of dropout students is used to add to the original data.
}
\value{
List.
\item{comp1 }{A list of class train. Characteristics varies according to the selected classifier. See Caret package for more details on each class.}
\item{comp2 }{A factor with the predictions for the test data. It uses the best model, based on specificity, determined during training. Check comp1 for details on the model.}
\item{comp3 }{Confusion matrix for the predictions on the test data.}
%If chosen classifier is C50:
%\item{comp1 }{A list of class train. It only appears if the C4.5 classifier was selected. See Caret package for more details on the class.}
%\item{comp2 }{A factor with the predictions for the test data using the best C4.5 model based on specificity.}
%\item{comp3 }{Confusion matrix for the C4.5 predictions on the test data.}
%If chosen classifier is Nnet:
%\item{comp1 }{A list of class train. It only appears if the neural net classifier was selected. See Caret package for more details on the class.}
%\item{comp2 }{A factor with the predictions for the test data using the best neural net model based on specificity.}
%\item{comp3 }{Confusion matrix for the neural net predictions on the test data.}
%If chosen classifier is reglog:
%\item{comp1 }{A list of class train. It only appears if the logistic regression classifier was selected. See Caret package for more details on the class.}
%\item{comp2 }{A factor with the predictions for the test data using the best logistic regression model based on specificity.}
%\item{comp3 }{Confusion matrix for the logistic regression predictions on the test data.}
%If chosen classifier is NB:
%\item{comp1 }{A list of class train. It only appears if the naive bayes classifier was selected. See Caret package for more details on the class.}
%\item{comp2 }{A factor with the predictions for the test data using the best naive bayes model based on specificity.}
%\item{comp3 }{Confusion matrix for the naive bayes predictions on the test data.}
}
\references{
%% ~put references to the literature/web site here ~
http://topepo.github.io/caret/training.html
Kuhn (2008), "Building Predictive Models in R Using the caret" (http://www.jstatsoft.org/article/view/v028i05/v28i05.pdf)
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
\code{\link[caret]{train}}
\code{\link[HEdropout]{create_dropout_database}}
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


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
