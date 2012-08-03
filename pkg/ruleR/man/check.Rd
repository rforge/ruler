\name{check}
\alias{check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{function \code{check}
}
\description{A function that generates numeric sequence (with random starting values) and checks whether it is not constant, 
elements are not too big (>1000) or too small (-1000<) and if the sequence is unique for the test or contains
any \code{NA} values. 
Rules to generate sequence are either specyfied by user or combined automatically.
}
\usage{check(seqlen, items, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seqlen}{
- the length of numeric sequence to be generated
}
  \item{items}{
- a matrix in which previously generated sequences are stored (for checking uniqueness)
}
  \item{type}{
- specifying where to generate rules from. \code{type=1} means that the rules are combined automatically,
\code{type=2} means that only rules specyfied by user are to be used.
}
}
\details{
This function is used by \code{automaticTest} 
}
\value{
Result od \code{check} is a list:


\item{$result}{numeric sequence that is later added \code{items} matrix in \code{automaticTest}}
\item{$fun}{rule used to create the \code{$result} functioned that is later saved to \code{rules} list in \code{automaticTest}}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{automaticTest}},
\code{\link{conCheck}},
\code{\link{AddRule}}

}
\examples{
m<-matrix(1:6,1,6)
b<-check(seqlen=6,type=1,items=a)
unlist(b$result)
b$fun

# in this example generating sequence '1 2 3 4 5 6 ' is not allowed
# as 'm' matrix is specyfied as argument 'items'

}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
