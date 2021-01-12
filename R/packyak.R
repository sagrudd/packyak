
#' @export
PackYak = R6::R6Class(
  "PackYak",
  public = list(

    initialize = function(pkgname, strategy=NULL) {
      cli::cli_alert(
        stringr::str_interp(
          "creating RPM file from package [{pkgname}]"))
      private$package_name <- pkgname
      if (is.null(strategy)) {
        private$strategy <- PackageStrategy$new()
      } else {
        private$strategy <- strategy
        private$strategy$print()
      }
      if (self$is_cran_resource()) {
        cli::cli_alert_success("successfully loaded a CRAN page")
      } else if (self$is_bioconductor_resource()) {
        cli::cli_alert_success("successfully loaded a Bioconductor page")
      } else {
        cli::cli_alert_warning("This package cannot be found at BioC or CRAN")
        stop()
      }

    },


    is_cran_resource = function() {
      cran <- stringr::str_interp("https://cran.r-project.org/web/packages/${private$package_name}/index.html")
      cli::cli_alert(stringr::str_interp("checking cran [{cran}]"))
      lookup <- httr::GET(cran)
      if (lookup$status_code == 200) {
        private$package_page <- Cran$new(private$package_name, lookup, private$strategy, cran)
        return(TRUE)
      }
      return(FALSE)
    },

    is_bioconductor_resource = function() {
      bioc <- stringr::str_interp("https://www.bioconductor.org/packages/release/bioc/html/${private$package_name}.html")
      cli::cli_alert(stringr::str_interp("checking bioc [{bioc}]"))
      lookup <- httr::GET(bioc)
      if (lookup$status_code == 200) {
        private$package_page <- Bioconductor$new(private$package_name, lookup, bioc)
        return(TRUE)
      }
      return(FALSE)
    }


  ),

  private = list(
    strategy = NA,
    package_name = NA,
    package_page = NA
  )
)


rversion <- function() {
  paste(sessionInfo()$R.version$major, sessionInfo()$R.version$minor,sep=".")
}


