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

    add_sys_reqs = function(name, items) {

      skipped_methods = c("lubridate", "git2r")
      if (name %in% skipped_methods) {
        return()
      }

      items <- gsub("\\s.*","", items)
      items <- gsub(":.*", "", items)
      items <- gsub("\\).*", "", items)
      strip_vals = c("GNU", "Subversion", "Package", "optional", "C++11", "Optional")
      items <- items[!items %in% strip_vals]

      atomics <- c("xclip", "libxml2", "libgit2", "git", "libxml2-devel", "pandoc", "libsecret-devel")
      for (item in items[items %in% atomics]) {
        self$register(item, item)
      }
      items <- items[!items %in% atomics]

      corpus <- list(
        "libcurl" = "libcurl-devel",
        "GNU make" = "make",
        "OpenSSL" = "openssl",
        ICU4C = "icu",
        zlib = "zlib-devel",
        libpng = "libpng-devel",
        FreeType = "freetype",
        libbz2 = "bzip2-devel",
        libjpeg = "libjpeg-turbo",
        libsodium = "libsodium-devel",
        libpq = "libpq-devel"
      )

      for (item in items) {
        if (item %in% names(corpus)) {
          self$register(item, as.character(corpus[item]))
        } else {
          silent_stop(stringr::str_interp("new [${name}] sys_req [\"${item}\"]"))
        }
      }

      # clean_up_filters = c("C++11", "libbz2 & liblzma & libcurl")
      # if (any(clean_up_filters %in% items)) {
      #   items <- items[-which(items %in% clean_up_filters)]
      # }

      # for (item in items) {
      #   if (item == "GNU make")
      #     self$register(item, "make")
      #   else if (item == "libcurl")
      #     self$register(item, "libcurl-devel")
      #   else if (grepl("libxml2", item))
      #     self$register(item, "libcurl-devel")
      #   else if (item == "libpng")
      #     self$register(item, "libpng-devel")
      #   else if (item == "libjpeg")
      #     self$register(item, "libjpeg-devel")
      #   else if (item == "ICU4C")
      #      self$register(item, "icu")
      #   else if (item == "xclip")
      #     self$register(item, "xclip")
      #   else {
      #     silent_stop(stringr::str_interp("new sys_req [${item}]"))
      #     # stop()
      #   }
      #}

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
