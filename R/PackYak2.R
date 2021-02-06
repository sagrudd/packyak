
#' @export
PackYak2 = R6::R6Class(
  "PackYak2",
  public = list(

    initialize = function() {

      pypackage = PySPEC$new("medaka")
      if (!pypackage$is_valid_package()) {
        cli::cli_alert_danger("PackPakFailure ...")
      }

    }

  )

)
