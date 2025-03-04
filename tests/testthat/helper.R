
#' Test whether server can be connected
#'
#' @return TRUE if test server is available.
is_test_tw <- function() {
    rtiddlywiki::tw_options(host = "http://127.0.0.1:9090")
    x <- try({
        status <- rtiddlywiki::get_status()
        if (!is.null(status)) {
            return(TRUE)
        }
    })
    if (inherits(x, "try-error")) {
        return(FALSE)
    }
    return(FALSE)
}


test_folder <- function() "sfdkdhfsdahfsahfskafhsodisafsacsdfsajf"
