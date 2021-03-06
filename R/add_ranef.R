#' Add random effects to a data frame
#'
#' @param data the data frame
#' @param .by the grouping column (groups by row if NULL)
#' @param ... the name and standard deviation of each random effect
#' @param .cors the correlations among multiple random effects, to be passed to \code{\link{rnorm_multi}} as r
#' @param empirical logical. To be passed to \code{\link{rnorm_multi}} as empirical
#'
#' @return data frame with new random effects columns
#' @export
#'
#' @examples
#' add_random(rater = 2, stimulus = 2, time = 2) %>%
#'   add_ranef("rater", u0r = 1.5) %>%
#'   add_ranef("stimulus", u0s = 2.2, u1s = 0.75, .cors = 0.5) %>%
#'   add_ranef(c("rater", "stimulus"), u0sr = 1.2)
add_ranef <- function(data, .by = NULL, ..., .cors = 0, empirical = FALSE) {
  if (is.null(.by)) {
    .by <- names(data)
    grps <- data
  } else {
    grps <- unique(data[.by])
  }
  sd <- c(...)
  
  ranefs <- faux::rnorm_multi(
    n = nrow(grps),
    sd = sd,
    vars = length(sd),
    r = .cors,
    empirical = empirical
  ) %>%
    dplyr::bind_cols(grps)
  
  dplyr::left_join(data, ranefs, by = .by)
}

#' Recode a categorical column
#'
#' @param data the data frame
#' @param col the column to recode
#' @param newcol the name of the recoded column (defaults to col.c)
#' @param ... coding for categorical column
#'
#' @return data frame with new fixed effects columns
#' @export
#'
#' @examples
#' add_random(subj = 4, item = 4) %>%
#'   add_between("subj", cond = c("cntl", "test")) %>%
#'   add_recode("cond", "cond.t", cntl = 0, test = 1)
add_recode <- function(data, col, newcol = paste0(col, ".c"), ...) {
  data[newcol] <- list(.x = data[[col]]) %>%
      c(list(...)) %>%
      do.call(dplyr::recode, .)

  data
}

#' Add random factors to a data structure
#'
#' @param data the data frame
#' @param ... the new random factor column name and the number of values of the random factor (if crossed) or the n per group (if nested); can be a vector of n per group if nested
#' @param nested_in the column(s) to nest in (if NULL, the factor is crossed with all columns)
#'
#' @return a data frame
#' @export
#'
#' @examples
#' # start a data frame
#' data1 <- add_random(school = 3)
#' # nest classes in schools (2 classes per school)
#' data2 <- add_random(data1, class = 2, nested_in = "school")
#' # nest pupils in each class (different n per class)
#' data3 <- add_random(data2, pupil = c(20, 24, 23, 21, 25, 24), nested_in = "class")
#' # cross each pupil with 10 questions
#' data4 <- add_random(data3, question = 10)
#' 
#' # compare nesting in 2 different factors
#' data <- add_random(A = 2, B = 2)
#' add_random(data, C = 2, nested_in = "A")
#' add_random(data, C = 2, nested_in = "B")
add_random <- function(.data = NULL, ..., nested_in = NULL) {
  grps <- list(...)
  prefix <- substr(names(grps), 1, 1)

  if (is.null(nested_in)) {
    ids <- mapply(make_id, grps, prefix, SIMPLIFY = FALSE)
    ranfacs <- do.call(tidyr::crossing, ids)
    .mydata <- .data # stops rlang_data_pronoun warning
    tidyr::crossing(.mydata, ranfacs)
  } else {
    if (length(grps) > 1) {
      stop("You can only add 1 nested random factor at a time")
    }
    name <- names(grps)[[1]]
    n <- grps[[1]]
    ingrps <- unique(.data[nested_in])
    if (length(n) == 1) n <- rep(n, nrow(ingrps))
    if (length(n) != nrow(ingrps)) {
      stop("n must be a single integer or a vector ", 
           "with the same length as the number of unique values in ", 
           nested_in)
    }
    ids <- data.frame(
      .row = rep(1:nrow(ingrps), times = n),
      y = make_id(sum(n), prefix = prefix[[1]])
    )
    names(ids)[2] <- name
    ingrps[".row"] <- 1:nrow(ingrps)
    newdat <- dplyr::left_join(ingrps, ids, by = ".row")
    newdat[".row"] <- NULL
    
    dplyr::right_join(.data, newdat, by = nested_in)
  }
}
  
#' Add between factors
#'
#' @param .data the data frame
#' @param .by the grouping column (groups by row if NULL)
#' @param ... the names and levels of the new factors
#' @param shuffle whether to assign cells randomly or in "order"
#' @param prob probability of each level, equal if NULL
#'
#' @return data frame
#' @export
#'
#' @examples
#' add_random(subj = 4, item = 2) %>%
#'   add_between("subj", condition = c("cntl", "test")) %>%
#'   add_between("item", version = c("A", "B"))
add_between <- function(.data, .by = NULL, ..., shuffle = FALSE, prob = NULL) {
  if (is.null(.by)) {
    .by <- names(.data)
    grps <- .data
  } else {
    grps <- unique(.data[.by])
  }
  
  if(isTRUE(shuffle)) grps <- grps[sample(1:nrow(grps)), ]
  
  if (is.null(prob)) {
    # equal probability for each level
    # return as equal combos as possible 
    vars <- tidyr::crossing(...)
    for (v in names(vars)) {
      grps[v] <- rep_len(vars[[v]], nrow(grps))
    }
  } else {
    # set prob for each level
    vars <- list(...)
    for (v in names(vars)) {
      p <- if (is.na(prob[v]) || is.null(prob[[v]])) unlist(prob) else prob[[v]]
      p <- rep_len(p, length(vars[[v]]))
      if (sum(p) == nrow(grps)) {
        # exact N
        grps[v] <- rep(vars[[v]], times = p)
      } else {
        # sampled N
        grps[v] <- sample(vars[[v]], nrow(grps), T, prob = p)
      }
    }
  }
  
  dplyr::left_join(.data, grps, by = .by)
}

#' Add within factors
#'
#' @param .data the data frame
#' @param .by the grouping column (groups by row if NULL)
#' @param ... the names and levels of the new factors
#'
#' @return data frame
#' @export
#'
#' @examples
#' add_random(subj = 2, item =  2) %>%
#'   add_within("subj", time = c("pre", "post"))
add_within <- function(.data, .by = NULL, ...) {
  if (is.null(.by)) {
    .by <- names(.data)
    grps <- .data
  } else {
    grps <- unique(.data[.by])
  }
  vars <- list(...)
  newdat <- c(list(grps), vars) %>%
    do.call(tidyr::crossing, .)
  
  dplyr::left_join(.data, newdat, by = .by)
}
