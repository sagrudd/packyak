Cran = R6::R6Class(
  "Cran",
  inherit = RPackage,
  public = list(

    initialize = function(pkgname, htmlpage) {
      cli::cli_alert(
        stringr::str_interp(
          "investigating CRAN package [{pkgname}]"))
    }
  ),

  private = list(
    package_name = NA

  )
)

