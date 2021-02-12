

SPECtacular = R6::R6Class(
  "SPECtacular",

  inherit = FedoraPackager,

  public = list(

    initialize = function() {

    },

    get_license = function() {
      return(private$license)
    },

    spec_file_exists = function() {
      cli::cli_alert(
        stringr::str_interp("looking for specfile [${self$spec_file}]"))
      cli::cli_alert(self$spec_path())
      if (file.exists(self$spec_path())) {
        return(TRUE)
      }
      return(FALSE)
    },

    spec_path = function() {
      file.path(self$spec_dir(), paste0(self$spec_file, ".spec"))
    },

    get_specced_version = function() {
      return(self$get_something_from_spec("version", "Version:"))
    },

    is_specfile_locked = function() {
      spec_lines <- stringr::str_c(readLines(self$spec_path()), collapse="")
      if (grepl("%global specfile_lock 1",spec_lines)) {
        return(TRUE)
      }
      return(FALSE)
    },

    create_specfile = function() {
      return(private$update_specfile)
    },

    is_valid_package = function() {
      return(private$valid_package)
    },

    canoninal_rpm_name = function() {
      silent_stop("canonical_rpm called at root")
    },


    create_spec = function() {
      cli::cli_h2(stringr::str_interp("ab initio build of SPECfile [${private$package_name}]"))
      if (!is.null(private$python_version)) {
        cli::cli_h3("wrapping python context into SPECfile")
        private$seed_template("py-template.spec")

        # set `%global packname`
        self$set_something_in_spec("%global packname", private$package_name)

        # set `%global pyversion`
        self$set_spec_py_version(private$python_version)

        # set `%global packrel`
        self$set_something_in_spec("%global packrel", "1") # it is a first creation??

        # set the SPEC summary field
        self$set_something_in_spec(
          stringr::str_pad("Summary:", 17, side="right"),
          stringr::str_interp("PackYak automated build of package = ${private$package_name} (${private$package_version})"))

        # set `Name:             `
        self$set_something_in_spec(stringr::str_pad("Name:", 17, side="right"), self$spec_file) # it is a first creation??

        # set `Version:          `
        self$set_something_in_spec(stringr::str_pad("Version:", 17, side="right"), private$package_version)

        # set `License:          `
        self$set_something_in_spec(stringr::str_pad("License:", 17, side="right"), private$license)

        # set `URL:              `
        self$set_something_in_spec(stringr::str_pad("URL:", 17, side="right"), private$url)

        # set `Source0:          `
        self$set_something_in_spec(stringr::str_pad("Source0:", 17, side="right"), self$get_download_link())

        # set `BuildRequires:    `
        build_requires_tag <- paste(
          stringr::str_pad("BuildRequires:", 17, side="right"),
          stringr::str_interp("python${private$python_version}"))
        self$set_something_in_spec(stringr::str_pad("BuildRequires:", 17, side="right"), stringr::str_interp("python${private$python_version}"))

        # set `Requires:         `
        requires_tag <- paste(
          stringr::str_pad("Requires:", 17, side="right"),
          stringr::str_interp("python${private$python_version}"))
        self$set_something_in_spec(stringr::str_pad("Requires:", 17, side="right"), stringr::str_interp("python${private$python_version}"))


        self$set_something_in_spec("%files -n ",
                                   paste(self$canoninal_rpm_name(),"-f INSTALLED_FILES"))


        for (dependency in self$trawl_dependencies()) {
          if (dependency != private$package_name) {
            cli::cli_alert(stringr::str_interp("linking dependency [${dependency}]"))
            self$append_something_in_spec(
              build_requires_tag,
              paste(stringr::str_pad("BuildRequires:", 17, side="right"), self$canoninal_rpm_name(dependency)))
            self$append_something_in_spec(
              requires_tag,
              paste(stringr::str_pad("Requires:", 17, side="right"), self$canoninal_rpm_name(dependency)))
          }
        }

        for (dependency in self$trawl_dependencies()) {
          if (dependency != private$package_name) {
            PackYak2$new(dependency)
          }
        }


        # self$append_something_in_spec(
        #   build_requires_tag,
        #   "XYZ"
        # )
        #
        # self$append_something_in_spec(
        #   requires_tag,
        #   "ABC"
        # )

        # amend %changelog
        private$comment_spec(stringr::str_interp("first build of [${private$package_name}] version [${private$package_version}]"))
      }
    },


    update_spec = function() {
      cli::cli_h2(stringr::str_interp("updating SPECfile [${private$package_name}]"))
      if (!is.null(private$python_version)) {
        cli::cli_h3("wrapping python context into SPECfile")
        pyversion <- self$get_spec_py_version()
        if (utils::compareVersion(pyversion, private$python_version) == -1) {
          update <- stringr::str_interp("updating core symbioinfo python from [${pyversion}] to [${private$python_version}]")
          cli::cli_alert(update)
          private$comment_spec(update)
          self$set_spec_py_version(private$python_version)
        }

        specversion <- self$get_specced_version()
        if (utils::compareVersion(specversion, private$package_version) == -1) {
          update <- stringr::str_interp("updating [${private$package_name}] version from [${specversion}] to [${private$package_version}]")
          cli::cli_alert(update)

          # update the download link
          download_link <- self$get_download_link()

          # update the SPEC summary field
          self$set_something_in_spec(
            stringr::str_pad("Summary:", 17, side="right"),
            stringr::str_interp("PackYak automated rebuild of package = ${private$package_name} (${private$package_version})"))

          # update the buildrequires and fields (allow manual fields to be retained)


          #
        }
      }
    },

    trawl_dependencies = function() {
      cli::cli_alert_danger("parental type of trawl_dependencies called ...")
    },

    get_download_link = function() {
      cli::cli_alert_danger("top_level_placeholder called")
    },

    get_spec_py_version = function() {
      return(self$get_something_from_spec("pyversion", "%global pyversion"))
    },

    set_spec_py_version = function(version) {
      return(self$set_something_in_spec("%global pyversion", version))
    },

    get_something_from_spec = function(name, something) {
      spec_lines <- readLines(self$spec_path())
      cli::cli_div(theme = list(span.emph = list(color = "red")))
      xfeature <-
        stringr::str_trim(gsub(
          something,
          "",
          spec_lines[which(grepl(something, spec_lines))]))
      cli::cli_alert(
        stringr::str_interp(
          "[${name}] from SPECfile [{.emph ${xfeature}}]"))
      cli::cli_end()
      return(xfeature)
    },

    set_something_in_spec = function(something, value) {
      spec_lines <- readLines(self$spec_path())

      spec_lines[which(grepl(something, spec_lines))] <-
        stringr::str_interp("${something} ${value}")

      con <- file(self$spec_path(), "w")
      writeLines(spec_lines, con = con)
      close(con)
    },


    append_something_in_spec = function(tag, something) {
      spec_lines <- readLines(self$spec_path())

      index <- which(grepl(tag, spec_lines))
      spec_lines <- append(spec_lines, something, after=index)

      con <- file(self$spec_path(), "w")
      writeLines(spec_lines, con = con)
      close(con)
    }


  ),

  active = list(

    spec_file = function(value=NULL) {
      if (!is.null(value)) {
        private$spec_file_target <- value
      }
      return(private$spec_file_target)
    }

  ),


  private = list(
    url = NULL,
    valid_package = NULL,
    license = NULL,
    package_name = NULL,
    package_version = NULL,
    spec_file_target = NULL,
    update_specfile = NULL,
    python_version = NULL,


    comment_spec = function(comment) {

      spec_lines <- readLines(self$spec_path())
      comment <- paste("-", comment, stringr::str_interp("by PackYak v${packageVersion(\"packyak\")}"))

      today <- paste(
        "*", as.character(lubridate::wday(lubridate::today(), label = TRUE)),
        as.character(lubridate::month(lubridate::today(), label = TRUE)),
        lubridate::day(lubridate::today()),
        lubridate::year(lubridate::today()),
        git2r::config()$global$user.name,
        paste0("<",git2r::config()$global$user.email,">"))
      if (today %in% spec_lines) {
        index <- which(spec_lines==today)
        spec_lines <- append(spec_lines, c(comment), after=index)
      } else {
        index <- which(spec_lines=="%changelog")
        spec_lines <- append(spec_lines, c(today, comment), after=index)
      }

      con <- file(self$spec_path(), "w")
      writeLines(spec_lines, con = con)
      close(con)
    },


    seed_template = function(template_id) {
      cli::cli_alert(stringr::str_interp("Seeding template [${template_id}]"))
      template <- file.path(system.file(package="packyak"), "extdata", template_id)
      print(template)
      spec_lines <- readLines(template)
      con <- file(self$spec_path(), "w")
      writeLines(spec_lines, con = con)
      close(con)
    }



  )

)
