\name{create_dropout_database}
\alias{create_dropout_database}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Creates databases to classify dropout students.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Creates the databases that should be used to classify the dropout students of Brazil`s higher education students.
}
\usage{
create_dropout_database(co_ies, tipo_evasao)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{co_ies}{
%%     ~~Describe \code{co_ies} here~~
numeric vector containing the institutions codes.
}
  \item{tipo_evasao}{
%%     ~~Describe \code{tipo_evasao} here~~
A string specifying which level of dropout is to be generated. Possible values are: "curso", "ies" and "grupo".
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
dataframe
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
\code{\link[HEdropout]{classify_dropout}}
\code{\link[HEdropout]{db_example}}

}
\examples{
# Example of dropout at major level for institution 10.
create_dropout_database(co_ies=10,tipo_evasao="curso")

# Example of dropout at group level for institutions 415, 15001 and 55.
create_dropout_database(co_ies=c(415,15001,55),tipo_evasao="ies")
