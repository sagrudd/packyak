
SpecOps = R6::R6Class(
  "SpecOps",
  public = list(

    initialize = function(pkgname, rpackage=NA, specfile=NA) {
      cli::cli_alert(
        stringr::str_interp(
          "checking SPEC file for [{pkgname}]"))
    },

    exists = function() {

    },

    same_version = function() {

    }

  ),

  private = list(

    load_spec = function() {

    },

    append_logs = function() {

    },

    write_spec = function() {

    }


  )
)
