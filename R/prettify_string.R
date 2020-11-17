
# Prettify strings

# prettify_string / names / title -----------------------------------------

#' @title Prettify strings
#' @description
#'
#' Remove:
#'
#' - punctuation,
#'
#' - special characters,
#'
#' - spaces and tabs,
#'
#' - accents
#'
#' Force everything to lower
#'
#' @param x   A string or a vector of strings.
#' @param ... Custom replacement. See examples.
#'
#' @return Returns a clean x
#'
#' @importFrom stringr str_replace_all
#'
#' @examples
#'
#' # remove all symbols and accents
#' x <- "a1~!#$€%^&*|__+:____()[]{}<>?,./;:'-=òìàèéù dASFGHRV"
#' y <- c("a1~!#$€%^&*|__+:____()[]{}<>?,./;:'-=òìàèéù dASFGHRV",
#'        "CIAAAOOOO")
#'
#' prettify_string(x)
#' prettify_string(y)
#'
#' # custom replacement
#' prettify_string(x = "5%_in_€")
#' prettify_string(x = "5%_in_€", "%" = "perc")
#' prettify_string(x = "5%_in_€", "%" = "perc", "€" = "eur")
#'
#'
#' @export
#'
prettify_string <- function(x, ...){

  # custom replace
  if(!missing(...)){
    x <- str_replace_all(string = x, c(...))
  }

  # keep only alphanumeric
  x <- str_replace_all(x, "[^[:alnum:]]+", "_")

  # remove accents and weird letters
  x <- iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT')

  tolower(x)

}

#' @title Wrap of prettify_string for names
#' @description
#'
#' Remove:
#'
#' - punctuation,
#'
#' - special characters,
#'
#' - spaces and tabs,
#'
#' - accents
#'
#' Force everything to lower.
#'
#' @param x A dataframe, a tibble, a named vector, a named list.
#' @param ... Custom replacement. See prettify_strings.
#'
#' @return Returns x with clean names.
#'
#' @examples
#'
#' head(iris)
#' head(prettify_names(iris))
#'
#' @export
#'
prettify_names <- function(x, ...){

  names(x) <- prettify_string(names(x), ...)
  x

}

#' @title Wrap of prettify_string for titles
#' @description
#'
#' Remove:
#'
#' - punctuation,
#'
#' - special characters,
#'
#' - spaces and tabs,
#'
#' - accents
#'
#' Force everything to title with spaces between words.
#'
#' @param x A string or a vector of strings.
#' @param ... Custom replacement. See prettify_strings.
#' @param replace String to use instad of the underscore. Default blank.
#'
#' @return Returns a clean x as printable title.
#'
#' @importFrom stringr str_replace_all str_to_title
#'
#' @examples
#'
#' x <- "a1~!#$%^&*|__+:____()[]{}<>?,./;:'-=òìàèéù dASFGHRV"
#' y <- c("a1~!#$%^&*|__+:____()[]{}<>?,./;:'-=òìàèéù dASFGHRV",
#'        "CIAAAOOOO")
#'
#' prettify_title(x)
#' prettify_title(y)
#'
#' @export
#'
prettify_title <- function(x, ..., replace = " "){

  x <- prettify_string(x, ...)
  x <- str_to_title(str_replace_all(x, "_", replace))
  x

}

#' @title Wrap of prettify_string for titles
#' @description
#'
#' Remove:
#'
#' - punctuation,
#'
#' - special characters,
#'
#' - spaces and tabs,
#'
#' - accents
#'
#' Force everything to title with spaces between words.
#'
#' @param x A string or a vector of strings.
#' @param ... Custom replacement. See prettify_strings.
#' @param replace String to use instad of the underscore. Default blank.
#'
#' @return Returns a clean x as printable title.
#'
#' @importFrom stringr str_replace_all str_to_title
#'
#' @examples
#'
#' prettify_names_title(iris)
#'
#' @export
#'
prettify_names_title <- function(x, ..., replace = " "){

  names(x) <- prettify_title(names(x), ..., replace = replace)
  x

}

