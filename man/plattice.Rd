\name{plattice}
\alias{plattice}
\title{
Lattice plot of panel data along with the model fitted values
}

\description{
Lattice plot of panel data along with the model fitted values.
}

\usage{
plattice(x, rows, pcol = "blue", psize = 2, lcol = "red", lsize = 1,
legcol = "orange", xlabel = NULL, ylabel = NULL, y_labels = NULL)
}

\arguments{
\item{x}{
A data frame with 4 columns of this specific order.
Column 1: the grouping variable.
Column 2: the time periods.
Column 3: the observed response values.
Column 4: the fitted values.
}
\item{rows}{
The number of rows the lattice plot will contain.
}
\item{pcol}{
The colour of the points (observed response values).
}
\item{psize}{
The size of the points (observed response values).
}
\item{lcol}{
The colour of the line of the fitted values.
}
\item{lsize}{
The size of the line of the fitted values.
}
\item{legcol}{
The colour of the background of the title of each sub-graph.
}
\item{xlabel}{
The label of the x-axis.
}
\item{ylabel}{
The label of the y-axis.
}
\item{y_labels}{
A vector with the numbers to appear in the y-axis. If left NULL,
10 equidistant values will appear.
}
}

%\details{
%}

\value{
A lattice plot with as many diagrams as the number of groups.
}

%\references{
%Faust, K. and S. Wasserman (1992). Blockmodels: Interpretation and evaluation.
%\emph{Social Networks}, 14, 5-61.
%}

%\seealso{
%\code{\link{adja_gnp}}
%}

\author{
Christos Adam and Michail Tsagris.
}

\examples{
y <- rnorm(120, 3, 1)
yhat <- 3 * y + rnorm(120, 0, 0.4)
id <- rep( c("DENMARK", "FRANCE", "GERMANY", "GREECE", "IRELAND", "ITALY"), 20)
a <- data.frame( country = id, year = rep(2000:2019, each = 6), y = y, yhat =  yhat)
plattice(a, rows = 2)
}


