#' Connect ot gaia
#'
#' test if gaia is already mounted and do it if not
#' @export
gaia_mount <- function() {
  if (!file.exists("/Users/aurelien.ginolhac/gaia/midica16/")) {
    message("gaia not mounted, mounting...")
    system("~/bin/gaia_mount")
  }
}