

#' @importFrom httr content
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
PyPi = R6::R6Class(
  "PyPi",
  inherit = PackageHandler,
  public = list(

    initialize = function(pkgname, htmlpage, strategy, url, fedora) {

      self$language <- "Python"

      private$pkgname <- pkgname
      cli::cli_h1(stringr::str_interp("package [${self$get_pkg_name()}]"))
      private$repo_name <- "pypi"
      private$htmlpage <- htmlpage
      private$url <- url
      private$strategy <- strategy
      private$page_tables <- rvest::html_nodes(
        httr::content(htmlpage, encoding="UTF-8"), "table")

      private$parse_page(fedora)

    }
  ),

  private = list(

    parse_page = function(fedora) {
      mytable <- private$page_tables[[1]]
      attrs <- rvest::html_attrs(mytable)
      if ("table table--downloads" %in% attrs) {
        print("tagged")
      } else {
        mytable <- private$page_tables[[2]]
        attrs <- rvest::html_attrs(mytable)
        if (!"table table--downloads" %in% attrs) {
          silent_stop("unable to pick table ...")
        }
      }


      links = mytable %>% rvest::html_nodes("a") %>% rvest::html_attr("href")

      print(links)

      if (any(stringr::str_detect(links, "^https.*tar.gz"))) {
        httplinks <- stringr::str_which(links, "^https.*tar.gz")
        links <- links[httplinks]
        link <- links[length(links)]
      } else if (any(stringr::str_detect(links, "^https.*zip"))) {
        httplinks <- stringr::str_which(links, "^https.*zip")
        links <- links[httplinks]
        link <- links[length(links)]
      } else {
        cli::cli_alert_danger(stringr::str_interp("PyPi without expected suffix ... [${private$pkgname}]"))
      }
      print(link)

      tarball <- file.path(fedora$get_rpmsource_dir(), basename(link))

      # has the file already been downloading
      cli::cli_alert(stringr::str_interp("looking for cached PyPi tarball [${tarball}]"))
      if (!file.exists(tarball)) {
        cli::cli_alert_info("downloading file to SOURCE directory")
        curl::curl_download(link, tarball)
      } else {
        cli::cli_alert_info("tarball already available in SOURCE directory")
      }

      ## creatively parse dependencies from this setup.py

      # extract license
      private$license <- httr::content(private$htmlpage, "text") %>%
        stringr::str_c(collapse="") %>%
        stringr::str_extract("(?<=<p><strong>License:</strong>)[^<]+") %>%
        stringr::str_trim()
      cli::cli_alert_info(stringr::str_interp("license     : ${self$get_license()}"))

      # extract version_str - pulling from the filename = more reliable than setup.py
      private$version_str <- basename(link) %>%
        stringr::str_replace(paste0(private$pkgname, "-"), "")  %>%
        stringr::str_replace(".tar.gz", "")  %>%
        stringr::str_replace(".zip", "")
      cli::cli_alert_info(stringr::str_interp("version_str: ${self$get_version_str()}"))
      cli::cli_alert_info(stringr::str_interp("version    : ${self$get_version()}"))
      cli::cli_alert_info(stringr::str_interp("release    : ${self$get_release()}"))

      # extract source code link
      private$source_code <- link
      cli::cli_alert_info(stringr::str_interp("source     : ${self$get_source()}"))

      # extract package dependencies
      command = stringr::str_interp("~/.local/bin/johnnydep --output-format pinned ${private$pkgname}")
      johnnydata <- system(command, intern=TRUE) %>%
        stringr::str_extract("^.*(?=\\=\\=)")
      private$depends <- johnnydata[!stringr::str_detect(johnnydata, private$pkgname)]
      # extract package dependencies
      cli::cli_h2(stringr::str_interp("[${self$get_pkg_name()}] dependencies"))
      print(private$depends)
    },


    get_value_from_setuppy = function(regex , setuppy) {
      response <- setuppy %>%
        stringr::str_extract(regex)

      response <- response[!is.na(response)] %>%
        stringr::str_replace("^\\s*=\\s*\"", "") %>%
        stringr::str_replace("\"$", "")

      return(response)

    }

  )
)
