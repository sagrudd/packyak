# lookup = httr::GET("https://cran.r-project.org/web/packages/RCurl/index.html")


#' @import R6
#' @importFrom magrittr %>%
#' @importFrom rvest html_nodes
#' @importFrom httr content
#' @importFrom rvest html_table
#' @importFrom stringr str_interp
Cran = R6::R6Class(
  "Cran",
  inherit = PackageHandler,
  public = list(

    initialize = function(pkgname, htmlpage, strategy, url) {
      private$pkgname <- pkgname
      cli::cli_h1(stringr::str_interp("package [${self$get_pkg_name()}]"))
      private$repo_name <- "cran"
      private$htmlpage <- htmlpage
      private$url <- url
      private$strategy <- strategy
      private$page_tables <- rvest::html_nodes(httr::content(htmlpage), "table")

      private$parse_page()

      #if (private$is_installable()) {
      #  private$install_package()
      #}
    }


  ),

  private = list(

    parse_page = function() {
      details_table <- private$page_tables[[1]] %>% rvest::html_table(fill = TRUE)

      # extract license
      private$license <- private$get_value_from_table("^License", details_table)
      cli::cli_alert_info(stringr::str_interp("license     : ${self$get_license()}"))

      # extract version_str
      private$version_str <- private$get_value_from_table("^Version", details_table)
      cli::cli_alert_info(stringr::str_interp("version_str: ${self$get_version_str()}"))
      cli::cli_alert_info(stringr::str_interp("version    : ${self$get_version()}"))
      cli::cli_alert_info(stringr::str_interp("release    : ${self$get_release()}"))

      # extract source code link
      private$source_code <- private$get_download_location("Package", private$page_tables[[2]])
      cli::cli_alert_info(stringr::str_interp("source     : ${self$get_source()}"))


      # extract package dependencies
      cli::cli_h2(stringr::str_interp("[${self$get_pkg_name()}] dependencies"))
      private$depends <- unique(c(
        private$referenced_package_filter("Depends:", details_table),
        private$referenced_package_filter("Imports:", details_table),
        private$referenced_package_filter("LinkingTo:", details_table)))
      print(private$depends)

      # and the suggested packages
      cli::cli_h2(stringr::str_interp("[${self$get_pkg_name()}] suggestions"))
      private$suggestions <- private$referenced_package_filter("Suggests:", details_table)
      private$novels <- private$strategy$add_imports(private$suggestions)
      print(private$suggestions)

      # and check the system requirements
      cli::cli_h2(stringr::str_interp("[${self$get_pkg_name()}] system requirements"))
      private$sys_reqs <- private$referenced_package_filter("SystemRequirements:", details_table)
      private$strategy$add_sys_reqs(private$sys_reqs)
      print(private$sys_reqs)
    }

  )
)

