
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
    args <- list(...)
    for(a in names(args)) x <- str_replace_all(string = x, pattern = a, replacement = args[[a]])
  }

  # remove punctuation
  x <- str_replace_all(x, "[[:punct:]]", "_")

  # remove spaces and tabs
  x <- str_replace_all(x, "[[:space:]]", "_")

  # remove remove special characters
  special_char <- c("$", "€", "+", "^", "~", "<", ">", "=", "|")
  s_special_char <- paste0("\\", special_char, collapse = "|")
  x <- str_replace_all(x, s_special_char, "_")

  # remove extra _
  x <- str_replace_all(x, "_+", "_")

  # remove accents and weird letters
  x <- iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT')

  # everything to lower
  x <- tolower(x)

  x
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

  n <- names(x)

  n <- prettify_string(n, ...)

  names(x) <- n

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

  n <- names(x)

  n <- prettify_title(n, ..., replace = replace)

  names(x) <- n

  x

}

