RPackage = R6::R6Class(
  "RPackage",
  public = list(

    get_version = function() {
      return(private$version)
    },

    follow_links = function() {
      cli::cli_alert("following package links")
    }

  ),

  private = list(
    version = NA
  )
)
