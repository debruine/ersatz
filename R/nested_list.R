#' Output a nested list in RMarkdown list format
#'
#' @param x The list
#' @param type Text to specify type of lists (unorderd, ordered, headers)
#' @param pre Text to prefix to each line (e.g., if you want all lines indented 4 spaces to start, use "    ")
#' @param quote Text to quote values with (e.g., use "`" to make sure values are not parsed as markdown
#'
#' @return A character string
#' @export
#'
#' @examples
#' x <- list(
#'   a = list(a1 = "Named", a2 = "List"),
#'   b = list("Unnamed", "List"),
#'   c = c(c1 = "Named", c2 = "Vector"),
#'   d = c("Unnamed", "Vector"),
#'   e = list(e1 = list("A", "B", "C"),
#'            e2 = list(a = "A", b = "B"),
#'            e3 = c("A", "B", "C"),
#'            e4 = 100),
#'   f = "single item vector",
#'   g = list()
#' )
#' nested_list(x)
#' nested_list(x, pre = "    ")
#' nested_list(x, type = "headers", pre= "#", quote = "'")
nested_list <- function(x,  pre = "", quote = "", type = "unordered") {
  txt <- c()
  if (type == "unordered") parameters= c("* ","    ",": ")
  if (type == "ordered") parameters= c("1. ","    ",": ")
  if (type == "headers") parameters= c("# ","#","\n")
  
  # use default if parameters not given correctly
  if (!exists("parameters")) parameters= c("* ","    ",": ")
  
  if (is.function(x)) {
    fnc <- x %>%
      jsonlite::toJSON() %>%
      jsonlite::fromJSON()
    
    txt <- c("```r", fnc, "```") %>%
      paste0(pre, .)
  } else if (!is.null(x) & !is.atomic(x) & !is.vector(x) & !is.list(x)) {
    # not a displayable type
    txt <- class(x)[1] %>% paste0("{", ., "}")
  } else if (is.null(x) | length(x) == 0) {
    txt <- "{empty}"
  } else if (length(x) == 1 &
             is.null(names(x)) &
             !is.list(x)) { # single-item unnamed vector
    txt <- paste0(quote, x, quote)
  } else { # x is a list, named vector, or vector length > 1
    # handle named, unnamed, or partially named
    list_names <- names(x)
    if (is.null(list_names)) {
      bullet <- paste0(1:length(x), ". ")
      if (type == "headers") bullet <- paste0(parameters[1], 1:length(x), "_no_title", parameters[3])
    } else {
      blanks <- grep("^$", list_names)
      list_names[blanks] <- paste0("{", blanks, "}")
      bullet <- paste0(parameters[1], list_names, parameters[3])
    }
    
    pre2 <- paste0(pre, parameters[2])
    txt <- lapply(seq_along(x), function(i) {
      item <- x[[i]]
      sub <- nested_list(item, pre2, quote,type)
      # add line break unless item is unnamed and length = 1
      lbreak <- ifelse(length(item) > 1 | (length(names(item)) > 0), "\n", "")
      if (grepl("\n", sub)) lbreak <- "\n"
      paste0(pre, bullet[i], lbreak, sub)
    })
  }
  collapsing = ifelse (type == "headers","\n\n","\n")
  
  list_txt <- paste(txt, collapse = collapsing)
  class(list_txt) <- c("nested_list", "character")
  
  list_txt
}

#' Print Nested List
#'
#' @param x The nested_list string
#' @param ... Additional parameters for print
#'
#' @export
#'
print.nested_list <- function(x, ...) {
  cat(x)
}
