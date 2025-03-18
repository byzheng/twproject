
#' Get standard names
#'
#' @param names A charactor vector for names
#' @param group Group of standard names
#'
#' @returns A vector for standard name
#' @export
standard_name <- function(names, group) {

    stopifnot(is.character(group))
    stopifnot(length(group) == 1)
    stopifnot(is.vector(names))
    stopifnot(length(names) > 0)
    filter <- sprintf("[tag[%s]]", group)
    tiddlers <- rtiddlywiki::get_tiddlers(filter)

    # Split aka field and loop through group tiddlers
    standard_name <- rep(NA, length(names))

    k <- 1
    for (k in seq(along = names)) {

        m <- 1
        for (m in seq(along = tiddlers)) {
            if (is.null(tiddlers[[m]]$name_aka99999999999999999)) {
                tiddlers[[m]]$name_aka99999999999999999 <- c(tiddlers[[m]]$title,
                                                             rtiddlywiki::split_field(tiddlers[[m]]$aka)) |>
                    unique() |>
                    tolower()
            }

            # go to next if not find
            if (!(tolower(names[k]) %in% tiddlers[[m]]$name_aka99999999999999999)) {
                next
            }

            # Assign values if find
            standard_name[k] <- tiddlers[[m]]$title
            break
        }

    }

    standard_name
}
