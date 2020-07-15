#' @importFrom purrr map
#' @importFrom purrr flatten_chr
#' @importFrom purrr map_dfr
#' @importFrom purrr possibly
#' @importFrom stringr str_subset
#' @importFrom stringr str_match_all
#' @importFrom stringr str_extract
#' @importFrom knitr current_input
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom utils packageDescription
#' @importFrom withr with_collate
#' @importFrom fs file_info
NULL

#' fill dates for Rmd header, with creation and last modification date
#' @param input vector of an input file, if null use current Rmd from knitting
#' @export
dates_report <- function(input = NULL) {
  if (is.null(input)) input <- current_input()
  if (!file.exists(input)) stop("file does not exist", call. = FALSE)
  # first try to get the first date in git history
  creation_date <- tryCatch({
    system2("git", args = c("log", "--pretty=format:'%ad'",
                            "--date=short", "--reverse",
                            input), stdout = TRUE)[1]
  },
  # if not available use the file modification date
  warning = function(cond) {
    birth <- fs::file_info(input)$birth_time
    birth <- stringr::str_extract(birth, "^\\d{4}-\\d{2}-\\d{2}")
    return(birth)
  },
  # same for an error
  error = function(cond) {
    birth <- fs::file_info(input)$birth_time
    birth <- stringr::str_extract(birth, "^\\d{4}-\\d{2}-\\d{2}")
    return(birth)
  })
  last_changed <- fs::file_info(input)$change_time
  last_changed <- stringr::str_extract(last_changed, "^\\d{4}-\\d{2}-\\d{2}")
  paste0(creation_date, " (last change: ", last_changed, ", compiled: ", Sys.Date(),")")
}

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

#' look from library calls and display their version and origin in a tibble
#' From Eric Koncina
#' @param file NULL for current doc or a path to a file
#' @param type either 'library' or 'colons' for library calls or colons for '::' calls
#' @return A tibble of used packages
#' @export
session_info_nodep <- function(file = NULL, type = c("all", "library")) {

  type <- match.arg(type)
  if (type == "all") {
    # regex to fetch pkg found here: https://www.kaggle.com/drobinson/analysis-of-r-packages-on-stack-overflow-over-time
    # by David Robinson
    reg <- "(?:library|require)\\([\"\']?([\\.a-zA-Z\\d]+).*?[\"\']?\\)|([\\.a-zA-Z\\d]+)::[\\._a-zA-Z\\d]+[\\(|:]"
  } else if (type == "library") {
    reg <- "library\\(\"?(\\w+)\"?\\)"
  }
  f_input <- ifelse(is.null(file), possibly(rstudioapi::getActiveDocumentContext,
                                            otherwise = list(path = knitr::current_input()))()[["path"]], file)
  f_input %>%
    readLines() %>%
    stringr::str_replace_all("#.*\n", "\n") %>%
    str_match_all(reg) %>%
    flatten_chr() %>%
    str_subset_inv("\\(") %>%
    unique() %>%
    stats::na.omit() %>%
    map_dfr(pkg_info)
}

#' inverse version of str_subset
#' @param vec a vector
#' @param pattern pattern as character
#' @return a vector without elements containing the pattern
#' @export
str_subset_inv <- function(vec, pattern) {
  vec[!stringr::str_detect(vec, pattern)]
}


#' copy from devtools
#' @param desc a package name
pkg_date <- function(desc) {
  if (!is.null(desc$`Date/Publication`)) {
    date <- desc$`Date/Publication`
  }
  else if (!is.null(desc$Built)) {
    built <- strsplit(desc$Built, "; ")[[1]]
    date <- built[3]
  }
  else {
    date <- NA_character_
  }
  as.character(as.Date(strptime(date, "%Y-%m-%d")))
}

#' copy from devtools
#' @param desc a package name
pkg_source <- function(desc)
{
  if (!is.null(desc$GithubSHA1)) {
    str <- paste0("Github (", desc$GithubUsername, "/", desc$GithubRepo,
                  "@", substr(desc$GithubSHA1, 1, 7), ")")
  }
  else if (!is.null(desc$RemoteType)) {
    remote_type <- desc$RemoteType
    if (!is.null(desc$RemoteUsername) && (!is.null(desc$RemoteRepo))) {
      user_repo <- paste0(desc$RemoteUsername, "/", desc$RemoteRepo)
    }
    else {
      user_repo <- NULL
    }
    if (!is.null(desc$RemoteSha)) {
      sha <- paste0("@", substr(desc$RemoteSha, 1, 7))
    }
    else {
      sha <- NULL
    }
    if (!is.null(user_repo) || !is.null(sha)) {
      user_repo_and_sha <- paste0(" (", user_repo, sha,
                                  ")")
    }
    else {
      user_repo_and_sha <- NULL
    }
    str <- paste0(remote_type, user_repo_and_sha)
  }
  else if (!is.null(desc$Repository)) {
    repo <- desc$Repository
    if (!is.null(desc$Built)) {
      built <- strsplit(desc$Built, "; ")[[1]]
      ver <- sub("$R ", "", built[1])
      repo <- paste0(repo, " (", ver, ")")
    }
    repo
  }
  else if (!is.null(desc$biocViews)) {
    "Bioconductor"
  }
  else {
    "local"
  }
}

#' copy from devtools
#' @param pkgs package names
pkg_info <- function(pkgs = loadedNamespaces()) {
  desc <- suppressWarnings(lapply(pkgs, packageDescription))
  not_installed <- vapply(desc, identical, logical(1), NA)
  if (any(not_installed)) {
    stop("`pkgs` ", paste0("'", pkgs[not_installed], "'",
                           collapse = ", "), " are not installed", call. = FALSE)
  }
  pkgs <- with_collate("C", pkgs[order(tolower(pkgs), pkgs)])
  attached <- pkgs %in% sub("^package:", "", search())
  desc <- lapply(pkgs, packageDescription)
  version <- vapply(desc, function(x) x$Version, character(1))
  date <- vapply(desc, pkg_date, character(1))
  source <- vapply(desc, pkg_source, character(1))
  pkgs_df <- data.frame(package = pkgs, version = version, date = date, source = source,
                        stringsAsFactors = FALSE, check.names = FALSE)
  rownames(pkgs_df) <- NULL
  class(pkgs_df) <- c("packages_info", "data.frame")
  pkgs_df
}