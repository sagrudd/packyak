Bioconductor = R6::R6Class(
  "Bioconductor",
  inherit = RPackage,
  public = list(

    initialize = function(pkgname, htmlpage) {
      cli::cli_alert(
        stringr::str_interp(
          "investigating Bioconductor package [{pkgname}]"))
    }
  ),

  private = list(
    package_name = NA

  )
)
