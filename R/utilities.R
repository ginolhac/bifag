NULL



#' Connect to a cluster using sshfs
#' test if the cluster is already mounted
#' @param cluster cluster name
#' @export
cluster_mount <- function(cluster) {
  test_bashrc <- file.path(Sys.getenv("HOME"), cluster, ".bashrc")
  if (!file.exists(test_bashrc)) {
    message(paste(cluster, "not mounted, mounting..."))
    mnt <- paste0(cluster, "_mount")
    stopifnot(file.exists(file.path(Sys.getenv("HOME"), "bin", mnt)))
    system(file.path(Sys.getenv("HOME"), "bin", mnt))
  }
}

