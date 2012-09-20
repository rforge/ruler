\name{extract_double_comment}
\alias{extract_double_comment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
extract_double_commen
}
\description{
A function to print double argument rules in common language.
}
\usage{
extract_double_comment(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
is an object of class \code{DoubleRule}
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
\code{\link{extract_single_comment}},
\code{\link{createTest}}

}
\examples{

b<-createTest(1,chain=0,exact=4,del=0)
extract_double_comment(b[[2]][[1]])

  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
