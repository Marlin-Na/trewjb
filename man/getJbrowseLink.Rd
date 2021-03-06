% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trewjb.R
\name{getJbrowseLink}
\alias{getJbrowseLink}
\title{Generate a url to TREW genome browser}
\usage{
getJbrowseLink(genome, chromosome, range_start, range_end, hight_start,
  hight_end, tracks = c("DNA", "gene_model"),
  baseurl = "http://180.208.58.19/jbrowse", show_navagation = TRUE,
  show_tracklist = FALSE, show_overview = FALSE)
}
\arguments{
\item{genome}{Which genome to view, current supported values are "hg19", "mm10"
and "dm6".}

\item{chromosome}{Which chromosome, e.g. "chr1"}

\item{range_start, range_end}{Length-one numeric vectors to indicate the start and end
of the desired range.}

\item{hight_start, hight_end}{Length-one numeric vectors to indicate the start and end
of the hightlight range, if missing, highlight will be disabled.}

\item{tracks}{Character vectors to indicate which tracks to show. In TREW database, they
are also valid experiment id, other primary tracks include "DNA", "gene_model",
"all_m6A", "all_m5C", "all_m1A" and "all_Psi". List of available tracks
can be found at http://180.208.58.19/jbrowse/ .}

\item{baseurl}{Default "http://180.208.58.19/jbrowse".}

\item{show_navagation}{Logical, whether show the navagation parts of the Jbrowse UI?}

\item{show_tracklist}{Logical, whether show the tracklist parts of the Jbrowse UI?}

\item{show_overview}{Logical, whether show the overview parts of the Jbrowse UI?}
}
\value{
A character as url.
}
\description{
Generate a link to view desired range on the TREW browser according to the
suppied arguments.
}
\examples{
if (interactive()) {
    url <- getJbrowseLink("hg19", "chr8", 92114847, 92231464,
        hight_start = 92150113, hight_end = 92152064,
        tracks = c("DNA", "gene_model", "all_m6A"),
        show_tracklist = TRUE, show_overview = TRUE)
    browseURL(url)
}

}

