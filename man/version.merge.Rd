% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/datamerge.R
\name{version.merge}
\alias{version.merge}
\title{Merge data frames}
\usage{
version.merge(..., add.cols = TRUE, add.rows = TRUE, add.values = FALSE,
  verbose = TRUE)
}
\arguments{
\item{...}{A list of data frames to be merged with falling priority.}

\item{add.cols}{Logical indicating if columns missing in frames with higher
priority should be added from frames with lower priority.}

\item{add.rows}{Logical indicating if rows missing in frames with higher
priority should be added from frames with lower priority.}

\item{add.values}{Logical indicating if missing values in frames with higher
priority should be imputed from frames with lower priority.}

\item{verbose}{Logical indicating if a summary should be displayed.}
}
\value{
A single merged data frame.
}
\description{
This function combines data from data frames in falling priority. That is
whenever the first data frame is missing a row, column and/or value it will
be taken from the first following data frame which does have it.
}
\details{
The task the function was written to solve is to merge a collection of
inconsistent versioned xls files. To do this import all the files using
\code{\link[utils]{read.csv}} or similar, make sure row and column names are
consistent (otherwise they wont be recognized as matching), run
\code{version.merge} and manually take care of the problems reported.
}
\examples{
# Make 3 similar data frames
frames <- lapply(1:3, function(i){
    d <- data.frame(matrix(runif(60), 10, 6))
    d[[sample(6, 1)]][sample(10, 5)] <- NA
    rownames(d) <- paste("obj_", sample(20, 10), sep="")
    names(d) <- c(paste("feat_", sort(sample(8, 6)), sep=""))
    return(d)
})
merged.frame <- version.merge(frames[[1]], frames[[2]], frames[[3]],
    add.values=TRUE, verbose=TRUE)
}
\author{
Christofer \enc{Bäcklin}{Backlin}
}

