

#' getJbrowseLink
#'
#' Generate a link to view desired range on the TREW browser according to the
#' suppied arguments.
#'
#' @param genome
#'     Which genome to view, current supported values are "hg19", "mm10"
#'     and "dm6".
#' @param chromosome
#'     Which chromosome, e.g. "chr1"
#' @param range_start,range_end
#'     Length-one numeric vectors to indicate the start and end
#'     of the desired range.
#' @param hight_start,hight_end
#'     Length-one numeric vectors to indicate the start and end
#'     of the hightlight range, if missing, highlight will be disabled.
#' @param tracks
#'     Character vectors to indicate which tracks to show. In TREW database, they
#'     are also valid experiment id, other primary tracks include "DNA", "gene_model",
#'     "all_m6A", "all_m5C", "all_m1A" and "all_Psi". List of available tracks
#'     can be found at http://180.208.58.19/jbrowse/ .
#' @param baseurl
#'     Default "http://180.208.58.19/jbrowse".
#' @param show_navagation
#'     Logical, whether show the navagation parts of the Jbrowse UI?
#' @param show_tracklist
#'     Logical, whether show the tracklist parts of the Jbrowse UI?
#' @param show_overview
#'     Logical, whether show the overview parts of the Jbrowse UI?
#' @return
#'     A character as url.
#' @export
#'
#' @examples
#' if (interactive()) {
#'     url <- getJbrowseLink("hg19", "chr8", 92114847, 92231464,
#'         hight_start = 92150113, hight_end = 92152064,
#'         tracks = c("DNA", "gene_model", "all_m6A"),
#'         show_tracklist = TRUE, show_overview = TRUE)
#'     browseURL(url)
#' }
#'
getJbrowseLink <- function(genome,
                           chromosome,
                           range_start,
                           range_end,
                           hight_start,
                           hight_end,
                           tracks = c("DNA", "gene_model"), # "all_m6A"
                           baseurl = "http://180.208.58.19/jbrowse",
                           show_navagation = TRUE,
                           show_tracklist = FALSE,
                           show_overview = FALSE) {
    baseurl_ <- baseurl
    genome_ <- genome
    chromosome_ <- chromosome

    range_ <-
        if (missing(range_start) || missing(range_end)) ''
        else paste0(':', parseRange(start = range_start, end = range_end, resizeFactor = 1.5))
    highlight_ <-
        if (missing(hight_start) || missing(hight_end)) ''
        else paste0('&highlight=',chromosome,':',
                    parseRange(start = hight_start, end = hight_end, resizeFactor = 1))

    tracks_ <- paste(unique(tracks), collapse = ",")

    navagation_ <- if (show_navagation) '' else '&nav=0'
    tracklist_  <- if (show_tracklist) '' else '&tracklist=0'
    overview_ <- if (show_overview) '' else '&overview=0'

    sprintf("%s/?data=data/%s&loc=%s%s&tracks=%s%s%s%s%s",
            baseurl_,genome_,chromosome_,range_,tracks_,highlight_,
            navagation_,tracklist_,overview_)
}

# TODO: not export it
#' parseRange
#'
#' Used internally to resize start and end of a given range and paste them together.
#'
#' @export
parseRange <- function(start, end, resizeFactor = 1) {
    stopifnot(is.numeric(start),
              is.numeric(end),
              is.numeric(resizeFactor),
              length(start) == 1,
              length(end) == 1,
              end >= start)

    width <- end - start + 1

    rgstart <- start - round(((resizeFactor-1)/2)*width)
    rgend <- end + round(((resizeFactor-1)/2)*width)

    range <- paste0(rgstart,'..',rgend)
    range
}








#' Create iframe view for Jbrowse from a url
#'
#' A sample url is
#' "http://180.208.58.19/jbrowse/?data=data/hg19&loc=chr6:30309362..30310357&tracks=DNA,all_m6A,gene_model&highlight=chr6:30309513..30310230&nav=0&tracklist=0&overview=0" .
#'
#' @examples
#' iframeJbrowse("http://180.208.58.19/jbrowse/?data=data/hg19&loc=chr6:30309362..30310357&tracks=DNA,all_m6A,gene_model&highlight=chr6:30309513..30310230&nav=0&tracklist=0&overview=0")
#'
#' @import htmlwidgets
#'
#' @export
iframeJbrowse <- function(link, width = NULL, height = NULL, elementId = NULL) {

    # TODO: provide options to customize iframe style and div style

    style = 'border: 1px solid black'
    div_style = 'width: 100%; height: 500px;'

    iframe_html <- htmltools::tags$iframe(
        src = link, style = style,
        width = '100%', height = '100%',
        "Sorry, your browser does not support iframe."
    )
    div_html <- htmltools::tags$div(iframe_html, style = div_style)
    html <- as.character(div_html)

    # forward options using x
    x = list(
        html = html
    )

    # create widget
    htmlwidgets::createWidget(
        name = 'trewjb',
        x,
        width = width,
        height = height,
        package = 'trewjb',
        elementId = elementId
    )
}


#' Shiny bindings for TREW Jbrowse iframe
#'
#' Output and render functions for using trewjb within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a trewjb
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name Jbrowse-shiny
#'
#' @details \code{renderJbrowse} should return with \link[trewjb]{iframeJbrowse}.
#'
#' @examples
#' if (interactive() && require(shiny))
#' shinyApp(
#'     ui = fluidPage(
#'         fluidRow(
#'             JbrowseOutput(outputId = 'test')
#'         )
#'     ),
#'     server = function(input, output) {
#'         output$test <- renderJbrowse(
#'             iframeJbrowse('http://www.example.org')
#'         )
#'     }
#' )
#'
#' @export
JbrowseOutput <- function(outputId, width = '100%', height = '400px'){
    htmlwidgets::shinyWidgetOutput(outputId, 'trewjb', width, height, package = 'trewjb')
}

#' @rdname Jbrowse-shiny
#' @export
renderJbrowse <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) { expr <- substitute(expr) } # force quoted
    htmlwidgets::shinyRenderWidget(expr, JbrowseOutput, env, quoted = TRUE)
}
