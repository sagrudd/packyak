

#' @importFrom httr content
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
Bioconductor = R6::R6Class(
  "Bioconductor",
  inherit = PackageHandler,
  public = list(

    initialize = function(pkgname, htmlpage, strategy, url) {

      private$pkgname <- pkgname
      cli::cli_h1(stringr::str_interp("package [${self$get_pkg_name()}]"))


      private$repo_name <- "bioc"
      private$htmlpage <- htmlpage
      private$url <- url
      private$strategy <- strategy
      private$page_tables <- rvest::html_nodes(
        httr::content(htmlpage, encoding="UTF-8"), "table")

      private$parse_page()
    }
  ),

  private = list(

    parse_page = function() {
      details_table <- private$page_tables[[3]] %>% rvest::html_table(fill = TRUE)
      #print(details_table)

      # extract license
      private$license <- private$get_value_from_table("^License", details_table)
      cli::cli_alert_info(stringr::str_interp("license     : ${self$get_license()}"))

      # extract version_str
      private$version_str <- private$get_value_from_table("^Version", details_table)
      cli::cli_alert_info(stringr::str_interp("version_str: ${self$get_version_str()}"))
      cli::cli_alert_info(stringr::str_interp("version    : ${self$get_version()}"))
      cli::cli_alert_info(stringr::str_interp("release    : ${self$get_release()}"))

      # extract source code link
      private$source_code <- private$get_download_location("Source Package", private$page_tables[[4]])
      cli::cli_alert_info(stringr::str_interp("source     : ${self$get_source()}"))

      # extract package dependencies
      cli::cli_h2(stringr::str_interp("[${self$get_pkg_name()}] dependencies"))
      private$depends <- unique(c(
        private$referenced_package_filter("Depends", details_table),
        private$referenced_package_filter("Imports", details_table),
        private$referenced_package_filter("LinkingTo", details_table)))
      print(private$depends)

      # and the suggested packages
      cli::cli_h2(stringr::str_interp("[${self$get_pkg_name()}] suggestions"))
      private$suggestions <- private$referenced_package_filter("Suggests", details_table)
      private$novels <- private$strategy$add_imports(private$suggestions)
      print(private$suggestions)

      # and check the system requirements
      cli::cli_h2(stringr::str_interp("[${self$get_pkg_name()}] system requirements"))
      private$sys_reqs <- private$referenced_package_filter("SystemRequirements", details_table)
      private$strategy$add_sys_reqs(private$sys_reqs)
      print(private$sys_reqs)
    }

  )
)
