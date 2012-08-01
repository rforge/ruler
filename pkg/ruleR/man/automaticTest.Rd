\name{automaticTest}
\alias{automaticTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
Function to generate desired amount of test items (numeric sequence of specyfied length).
Items can be created using automatically combined rules, or using only the
rules specyfied by the user (stored on \code{MyRules} list).
Function /code{automaticTest}is also checking the uniqueness of generated sequences and prevents constat sequences
to be generated.
For every numeric sequence \code{automaticTest} returns the list of rules used to create it
as well as 'noise' answers (in case muliple choice is needed in the test)
}
\usage{
automaticTest(testlen, type = 1, seqlen = 6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{testlen}{
- how many numeric sequences (test items) are to be generated
}
  \item{type}{
- which rules to be applied. \code{type=1} means that the rules are generated automatically,
\code{type=2} means that ony rules defined by user (stored on \code{MyRules} list) should be used
 to create a sequence. By default \code{type=1}.
}
  \item{seqlen}{
 - desired length of numeric sequence (by default \code{seqlen=6})
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{automaticTest} function returns a list containing the following:

\item{$items}{generated numeric sequences (in case multiple answer format is needed in a test)}
\item{$noise }{noise 'answers' }
\item{$rules}{a list of rules used to generate numeric sequences}

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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

#[1] everything generated automatically
m<-automaticTest(10) # type=1, seqlen=6
m$items # generated numeric sequences
m$noise # sample noise answers
m$rules # list of rules used to create items 


#[2] items generated only with the rules supplied by user
m<-new("AddConstSingleRule", constantVal=6) #single rule adding 6 to an element of sequence
r<-new("AddConstSingleRule", constantVal=10)#single rule adding 10 to an element of sequence
AddRule(m) #adding rule 'm' to 'MyRules' list
AddRule(r) # adding rule 'r' to 'MyRules' list
m<-automaticTest(10,type=2 # generating a test consisting of 10 items, with rules 'm' and 'r' only
m$items # generated numeric sequences
m$rules # list of rules used to create items
m$noise # sample noise answers

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ automaticTest }

