
#' @export
PackYak2 = R6::R6Class(
  "PackYak2",
  public = list(

    initialize = function(package="urllib3") {

      pypackage = PySPEC$new(package)
      if (!pypackage$is_valid_package()) {
        cli::cli_alert_danger("PackPakFailure ...")
      } else {
        if (pypackage$create_specfile()) {
          if (pypackage$spec_file_exists()) {
            cli::cli_alert_success("updating specfile for package")
            silent_stop("yikes ... check this")
            pypackage$update_spec()
          } else {
            cli::cli_alert_success("creating specfile for package")
            pypackage$create_spec()
          }
        } else {
          cli::cli_alert_success("SPECfile is in order for package")
        }
      }

    }

  )

)
