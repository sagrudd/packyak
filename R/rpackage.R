RPackage = R6::R6Class(
  "RPackage",
  public = list(

    get_version = function() {
      return(private$version)
    }

  ),

  private = list(
    version = NA
  )
)
