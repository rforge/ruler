\name{extract_single_comment}
\alias{extract_single_comment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
extract_single_comment
}
\description{
A function to print single argument rules in common language.
}
\usage{
extract_single_comment(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{SingleRule}
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
\code{\linkS4class{SingleRule}},
\code{\linkS4class{DoubleRule}},
\code{\link{extract_double_comment}},
\code{\link{createTest}}

}
\examples{

#m<-createTest(1,chain=0,exact=1)
#extract_single_comment(m[[2]][[1]])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
