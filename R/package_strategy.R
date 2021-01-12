PackageStrategy = R6::R6Class(
  "PackageStrategy",
  public = list(

    initialize = function() {
      cli::cli_alert("creating strategy handler")
      private$suggested_list <- list()
      private$imports_list <- list()
      private$installed_list <- list()
    },



    add_imports = function(items) {
      novels <- NULL
      for (item in items) {
        print(item)
        if (item %in% private$imports_list) {
          cli::cli_alert(
            stringr::str_interp("[${item}] already defined as imports package"))
        } else {
          cli::cli_alert(
            stringr::str_interp("adding [${item}] as an imports package"))
          private$suggested_list <- append(private$suggested_list, item)
          novels <- append(novels, item)
        }
        return(novels)
      }
    },

    add_sys_reqs = function(items) {
      for (item in items) {
        if (item == "GNU make")
          self$register(item, "make")
        else if (item == "libcurl")
          self$register(item, "libcurl-devel")
        else if (grepl("libxml2", item))
          self$register(item, "libcurl-devel")
        else {
          cli::cli_alert_warning(stringr::str_interp("new sys_req [${item}]"))
          stop()
        }
      }

    },

    is_installed = function(pkg) {
      if (pkg %in% names(private$installed_list)) {
        return(TRUE)
      }
      return(FALSE)
    },

    print = function(...) {
      cli::cli_alert_info(stringr::str_interp("Suggests: ${length(private$suggested_list)}; Imports: ${length(private$imports_list)}"))
    },

    register = function(rname, rpm) {
      cli::cli_alert_success(stringr::str_interp("registering [${rname}] with [${rpm}]"))
      private$installed_list[rname] <- rpm
    },

    get_feature = function(rname) {
      return(as.character(private$installed_list[rname]))
    }

  ),

  private = list(
    suggested_list = NA,
    imports_list = NA,
    installed_list = NA


  )
)
