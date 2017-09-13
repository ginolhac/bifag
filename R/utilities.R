#' Connect ot gaia
#'
#' test if gaia is already mounted and do it if not
#' @export
gaia_mount <- function() {
  if (!file.exists("/Users/aurelien.ginolhac/gaia/.bashrc/")) {
    message("gaia not mounted, mounting...")
    system("~/bin/gaia_mount")
  }
}


#' test if gaia is already mounted and do it if not
#' @export
cluster_mount <- function(cluster) {
  test_bashrc <- file.path(Sys.getenv("HOME"), cluster, ".bashrc")
  if (!file.exists("test_bashrc")) {
    message(paste(cluster, "not mounted, mounting..."))
    mnt <- paste0(cluster, "_mount")
    stopifnot(file.exists(file.path(Sys.getenv("HOME"), "bin", mnt)))
    system(file.path(Sys.getenv("HOME"), "bin", mnt))
  }
}
