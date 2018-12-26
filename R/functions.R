#' Title
#'
#' @import dygraphs
#' @import htmlwidgets
#' @export
#'
dygraph_to_png <- function(objet, filename = "dygraph.png", ...) {
  message("... dygraphs to png")
  html_out <- tempfile(pattern = "dygraph",fileext =  ".html")
  message(glue::glue("html_out : {html_out}"))
  message(glue::glue("filename : {filename}"))
  on.exit(unlink(html_out), add = TRUE)
  message('fin unlink')
  html <- htmlwidgets::saveWidget(objet, html_out) #htmlwidgets::
  # webshot::webshot(html_out, filename, debug=TRUE) #webshot::
  webshot2(html_out, filename, debug=TRUE) # same as webshot but with message
}
#' Title
#'
#'
#' @export
#'
webshot2 <- function (url = NULL, file = "webshot.png", vwidth = 992, vheight = 744,
                      cliprect = NULL, selector = NULL, expand = NULL, delay = 0.2,
                      zoom = 1, eval = NULL, debug = FALSE, useragent = NULL)
{
  if (is.null(url)) {
    stop("Need url.")
  }
  if (!is.null(cliprect) && !is.list(cliprect))
    cliprect <- list(cliprect)
  if (!is.null(selector) && !is.list(selector))
    selector <- list(selector)
  if (!is.null(expand) && !is.list(expand))
    expand <- list(expand)
  
  message("a1")
  arg_list <- list(url = url, file = file, vwidth = vwidth,
                   vheight = vheight, cliprect = cliprect, selector = selector,
                   expand = expand, delay = delay, zoom = zoom, eval = eval,
                   debug = debug, options = options)
  arg_length <- vapply(arg_list, length, numeric(1))
  
  message("a2")
  max_arg_length <- max(arg_length)
  if (any(!arg_length %in% c(0, 1, max_arg_length))) {
    stop("All arguments should have same length or be single elements or NULL")
  }
  if (length(url) < max_arg_length)
    url <- rep(url, max_arg_length)
  if (length(url) > 1 && length(file) == 1) {
    file <- vapply(1:length(url), FUN.VALUE = character(1),
                   function(i) {
                     replacement <- sprintf("%03d.\\1", i)
                     gsub("\\.(.{3,4})$", replacement, file)
                   })
  }
  
  message("a3")
  
  if (webshot:::is_windows()) {
    url <- webshot:::fix_windows_url(url)
  }
  
  message("a4")
  message(url)
  if (!is.null(cliprect) && !is.null(selector)) {
    stop("Can't specify both cliprect and selector.")
  }
  else if (is.null(selector) && !is.null(cliprect)) {
    cliprect <- lapply(cliprect, function(x) {
      if (is.character(x)) {
        if (x == "viewport") {
          x <- c(0, 0, vwidth, vheight)
        }
        else {
          stop("Invalid value for cliprect: ", x)
        }
      }
      else {
        if (!is.numeric(x) || length(x) != 4) {
          stop("'cliprect' must be a 4-element numeric vector or a list of such vectors")
        }
      }
      x
    })
  }
  if (!is.null(expand)) {
    lengths <- vapply(expand, length, numeric(1))
    if (any(!lengths %in% c(1, 4))) {
      stop("'expand' must be a vector with one or four numbers, or a list of such vectors.")
    }
  }
  
  
  message("a5")
  
  optsList <- data.frame(url = url, file = file, vwidth = vwidth,
                         vheight = vheight)
  
  
  message("a6")
  argToVec <- function(arg) {
    vapply(arg, FUN.VALUE = character(1), function(x) {
      if (is.null(x) || is.na(x))
        NA_character_
      else paste(x, collapse = ",")
    })
  }
  
  
  message("a7")
  if (!is.null(cliprect))
    optsList$cliprect <- argToVec(cliprect)
  if (!is.null(selector))
    optsList$selector <- argToVec(selector)
  if (!is.null(expand))
    optsList$expand <- argToVec(expand)
  if (!is.null(delay))
    optsList$delay <- delay
  if (!is.null(zoom))
    optsList$zoom <- zoom
  if (!is.null(eval))
    optsList$eval <- eval
  if (!is.null(useragent))
    
    
    optsList$options <- jsonlite::toJSON(list(pageSettings = list(userAgent = useragent)),
                                         auto_unbox = TRUE)
  
  
  
  message("a8")
  optsList$debug <- debug
  args <- list("--ignore-ssl-errors=true", system.file("webshot.js",
                                                       package = "webshot"), jsonlite::toJSON(optsList))
  
  
  message("a9")
  res <- phantom_run2(args)
  
  message("a10")
  if (is.null(res))
    return(NULL)
  if (res != 0) {
    stop("webshot.js returned failure value: ", res)
  }
  
  message("a11")
  structure(file, class = "webshot")
}



phantom_run2 <-  function (args, wait = TRUE)
{
  
  phantom_bin <- webshot:::find_phantom()
  message("phantom_bin")
  message(phantom_bin)
  if (is.null(phantom_bin))
    return(NULL)
  args <- as.character(args)
  message("args")
  message(args)
  p <- callr::process$new(phantom_bin, args = args, stdout = "|",
                          stderr = "|", supervise = TRUE)

  message(capture.output(p))
  if (isTRUE(wait)) {
    on.exit({
      p$kill()
    })
    cat_n <- function(txt) {
      if (length(txt) > 0) {
        cat(txt, sep = "\n")
      }
    }
    
    message(capture.output(p))
    while (p$is_alive()) {
      message(capture.output(p))
      p$wait(200)
      cat_n(p$read_error_lines())
      cat_n(p$read_output_lines())
    }
  }
  p$get_exit_status()
}



#' Title
#'
#'
#' @return
#' @export
#'
#' @examples
modUI <- function(id){
  
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      downloadButton(ns("download_as_png"),"download as png")
    ),
    mainPanel(
      dygraphOutput(ns("p1"))
    )
  )
}

#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
mod <- function(input,output,session){
  
  dessin <- reactiveVal({
    lungDeaths <- cbind(mdeaths, fdeaths)
    dygraph(lungDeaths)
  })
  output$p1 <- renderDygraph({
    dessin()
  })
  
  output$download_as_png <- downloadHandler(
    filename = function() {
      "dessin.png"
    },
    
    content = function(file) {
      dygraph_to_png(objet = dessin(),filename = file)
    }
  )
  
}

