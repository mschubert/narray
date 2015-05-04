#' Binds arrays together disregarding names
#'
#' @param arrayList  A list of n-dimensional arrays
#' @param along      Along which axis to bind them together
#' @return           A joined array
bind = function(arrayList, along=length(dim(arrayList[[1]]))+1) {
#TODO: check names?, call bind when no stacking needed automatically?
#TODO: data.table::rbindlist?
    do.call(function(f) abind::abind(f, along=along), arrayList)
}
