

SPECtacular = R6::R6Class(
  "SPECtacular",

  inherit = FedoraPackager,

  public = list(

    initialize = function() {

    },

    get_license = function() {
      return(private$license)
    }

  ),

  private = list(
    license = NULL

  )

)
