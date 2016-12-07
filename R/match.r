#' operator for array-like logical ops
#'
#' @param a  First vector
#' @param b  Second vector
#' @return   TRUE/FALSE for each element
`%or%` = function(a, b) {
    cmp = function(a,b) if (identical(a, FALSE) ||
                            is.null(a) ||
                            is.na(a) ||
                            is.nan(a) ||
                            length(a) == 0 ||
                            nchar(a) == 0) b else a

    if (is.list(a))
        lapply(1:length(a), function(i) cmp(a[[i]], b[[i]]))
    else if (length(a) > 1) #TODO: does that do what we want?
        mapply(cmp, a, b)
    else
        cmp(a, b)
}


#' match() function with extended functionality
#'
#' @param x            Vector of identifiers that should be mapped
#' @param from         Vector of identifiers that can be mapped
#' @param to           Matched mapping for all identifiers
#' @param filter_from  Restrict matching to a subset from `from`
#' @param filter_to    Restrict matching to a subset from `to`
#' @param data         List containing the data `from` and `to` reference
#' @param fuzzy_level  0 for exact, 1 punctuation, and 2 closest character
#' @param table        Return a matching table instead of just the matches
#' @param na_rm        Flag to remove items that can not be mapped
#' @param warn         Display warning for all fuzzy matches
#' @return             Mapped values
match = function(x, from, to, filter_from=NULL, filter_to=NULL, data=parent.frame(),
                 fuzzy_level=0, table=FALSE, na_rm=FALSE, warn=!table && fuzzy_level>0) {

    if (is.character(from) && length(from) == 1)
        from = data[[from]]
    if (is.character(to) && length(to) == 1)
        to = data[[to]]

    if (length(from) != length(to))
        stop("arguments `from` and `to` need to be of the same length")

    # filter matching table
    if (!is.null(filter_from))
        to[!from %in% filter_from] = NA
    if (!is.null(filter_to))
        to[!to %in% filter_to] = NA

    # remove identical mappings, then map ambivalent to NA
    df = data.frame(from=from, to=to)
	df = df[!duplicated(df),]
	df[duplicated(df$from) | rev(duplicated(rev(df$from))),] = NA
	df = df[!duplicated(df),]
    from = df$from
    to = df$to

    # 1st iteration: exact matches
    index = list(level0 = base::match(x, from))

    # 2nd iteration: non-punctuation exact matches
    if (fuzzy_level > 0) {
        FROM = stringr::str_replace_all(toupper(from), "[[:punct:]\\ ]", "")
        x_match = stringr::str_replace_all(toupper(x), "[[:punct:]\\ ]", "")
        index$level1 = base::match(x_match, FROM)
    }

    #TODO: insert iteration here that does closest string matches, but does not
    #  map two different strings to the same fuzzy match

    # 3rd iteration: closest string matches w/o punctuation
    if (fuzzy_level > 1) {
        distances = utils::adist(FROM, x_match)
        mind = apply(distances, 2, min)
        nmin = sapply(1:length(mind), function(i) sum(mind[i]==distances[,i]))
        mind[nmin>1] = NA # no non-unique matches
        index$level2 = sapply(1:length(mind), function(i)
            `%or%`(which(distances[,i]==mind[i]), NA))
    }

    # return best match
    re = Reduce(`%or%`, index)
    from = from[re]
    to = to[re]

    if (warn) {
        warning("Non-exact matches detected")
        print(stats::na.omit(data.frame(x=x, from=from)[x!=from,]))
    }

#    if (table && fuzzy_level == 0)
#        .omit$na(data_frame(x=x, to=to), omit=na_rm)
#    else if (table && fuzzy_level > 0)
#        .omit$na(data_frame(x=x, from=from, to=to), cols=c('x','to'), omit=na_rm)
#    else
#        .omit$na(stats::setNames(to, x), omit=na_rm)

    if (table && fuzzy_level == 0)
        data.frame(x=x, to=to)
    else if (table && fuzzy_level > 0)
        data.frame(x=x, from=from, to=to)
    else
        stats::setNames(to, x)
}
