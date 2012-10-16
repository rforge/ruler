\name{calculate-methods}
\docType{methods}
\alias{calculate-methods}
%\alias{calculate,DoubleRule,numeric,numeric-method}
%\alias{calculate,SingleRule,numeric,ANY-method}
\title{ ~~ Methods for Function \code{calculate}  ~~}
\description{
Methods to execute rules on specified numeric arguments.
}
\section{Methods}{
\describe{

\item{\code{signature(x = "DoubleRule", y = "numeric", z = "numeric")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "SingleRule", y = "numeric", z = "ANY")}}{
'z' argument is ignored for objects of class "SingleRule"
}
}}

\details{
\code{calculate} expects \code{y} to be \code{numeric} of length 1. Only some rules will support vectorization.
}

\seealso{
\code{\link{calculateSpecific-methods}}
}


\keyword{methods}
%\keyword{ ~~ other possible keyword(s) ~~ }
