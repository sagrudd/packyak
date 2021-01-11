
#' @export
PackYak = R6::R6Class(
  "PackYak",
  inherit = RPackage,
  public = list(

    initialize = function(pkgname) {
      cli::cli_alert(
        stringr::str_interp(
          "creating RPM file from package [{pkgname}]"))
      private$package_name <- pkgname
      if (self$is_cran_resource()) {
        cli::cli_alert_success("successfully loaded a CRAN page")
      } else if (self$is_bioconductor_resource()) {
        cli::cli_alert_success("successfully loaded a Bioconductor page")
      } else {

      }
    },


    is_cran_resource = function() {
      cran <- stringr::str_interp("https://cran.r-project.org/web/packages/${private$package_name}/index.html")
      cli::cli_alert(stringr::str_interp("checking cran [{cran}]"))
      lookup <- httr::GET(cran)
      if (lookup$status_code == 200) {
        private$package_page <- Cran$new(private$package_name, lookup)
        return(TRUE)
      }
      return(FALSE)
    },

    is_bioconductor_resource = function() {
      bioc <- stringr::str_interp("https://www.bioconductor.org/packages/release/bioc/html/${private$package_name}.html")
      cli::cli_alert(stringr::str_interp("checking bioc [{bioc}]"))
      lookup <- httr::GET(bioc)
      if (lookup$status_code == 200) {
        private$package_page <- Bioconductor$new(private$package_name, lookup)
        return(TRUE)
      }
      return(FALSE)
    }
  ),

  private = list(
    package_name = NA,
    package_page = NA,

    find_package = function() {

    }

  )
)





