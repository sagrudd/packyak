
#' @importFrom git2r config
#' @importFrom lubridate today
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom lubridate year
SpecOps = R6::R6Class(
  "SpecOps",
  public = list(

    initialize = function(pkgobj, fedora, overwrite=FALSE) {
      cli::cli_h1(
        stringr::str_interp(
          "checking SPEC file for [${pkgobj$get_pkg_name()}]"))

      private$pkgobj <- pkgobj
      private$fedora <- fedora
      private$set_name()

      if (self$file_exists() & overwrite) {
        cli::cli_alert_warning("over-writing existing SPEC file")
        comments <- private$retreive_comments()
        private$create_spec_from_template(comments)
        private$update <- TRUE

      } else if (self$file_exists() & !overwrite) {
        cli::cli_alert("file already exists ...")
        private$load_local_spec()
        if ((private$get_version() == private$pkgobj$get_version()) &
            (private$get_release() == private$pkgobj$get_release())) {
          cli::cli_alert_success(
            stringr::str_interp(
              "package [${pkgobj$get_pkg_name()}] with version [${pkgobj$get_version_str()}] already exists"))
        } else {
          cli::cli_alert(
            stringr::str_interp(
              "updating [${pkgobj$get_pkg_name()}] package to version [${pkgobj$get_version_str()}]"))
          private$update_package()
          private$update <- TRUE
        }
      } else {
        cli::cli_alert("creating a new file ...")
        private$create_spec_from_template()
        private$update <- TRUE
      }
    },

    file_exists = function(fedora) {
      private$spec_file = file.path(
        private$fedora$get_rpmbuild_dir(),
        "SPECS", paste0(private$pkgname, ".spec"))
      return(file.exists(private$spec_file))
    },

    get_rpm = function() {
      return(private$pkgname)
    },

    get_spec_file = function() {
      return(private$spec_file)
    },

    is_updated = function() {
      return(private$update)
    }

  ),

  private = list(
    pkgname = NA,
    pkgobj = NA,
    fedora = NA,
    spec_file = NA,
    spec_lines = NA,
    update = FALSE,

    set_name = function() {
      private$pkgname <- stringr::str_interp(
        # "r_symbioinfo_${private$pkgobj$get_repo()}_${tolower(private$pkgobj$get_pkg_name())}")
        "r_${tolower(private$pkgobj$get_pkg_name())}")
      cli::cli_alert(stringr::str_interp("RPM name [${private$pkgname}]"))
    },

    retreive_comments = function() {
      cli::cli_alert(stringr::str_interp("parsing comments from [${private$spec_file}]"))
      lines <- readLines(private$spec_file)
      return(lines[which("%changelog" == lines):length(lines)])
    },

    load_local_spec = function() {
      cli::cli_alert(stringr::str_interp("loading content from [${private$spec_file}]"))
      private$spec_lines <- readLines(private$spec_file)
    },

    create_spec_from_template = function(comments=NULL) {
      template <- file.path(system.file(package="packyak"), "extdata", "r-template.spec")
      private$spec_lines <- readLines(template)

      private$set_packname_variable()
      private$check_r_version()
      private$set_package_versions()
      private$update_source()
      private$update_license()
      private$update_dependencies()

      if (!is.null(comments)) {
        private$spec_lines <- c(
          private$spec_lines[1:which("%changelog" == private$spec_lines)-1],
          comments)
        private$comment_spec(stringr::str_interp("- forced rebuild of SPEC file by PackYak v${packageVersion(\"packyak\")}"))
      }

      con <- file(private$spec_file, "w")
      writeLines(private$spec_lines, con = con)
      close(con)
    },


    update_package = function() {
      private$check_r_version()
      private$set_package_versions()
      private$update_source()

      con <- file(private$spec_file, "w")
      writeLines(private$spec_lines, con = con)
      close(con)
    },



    set_packname_variable = function() {
      private$spec_lines[which(grepl("%global packname", private$spec_lines))] <-
        stringr::str_interp("%global packname ${private$pkgobj$get_pkg_name()}")
      private$spec_lines[which(grepl("^Name:", private$spec_lines))] <-
        stringr::str_interp("Name:             ${private$pkgname}")
    },


    check_r_version = function() {
      thisr <- rversion()
      taggedr <- gsub(".+\\s", "", private$spec_lines[which(grepl("%global rversion", private$spec_lines))])
      cli::cli_alert(stringr::str_interp("checking R versions [${taggedr} :: ${thisr}]"))
      if (taggedr != thisr) {
        cli::cli_alert(stringr::str_interp("updating R version to [${thisr}]"))
        private$spec_lines[which(grepl("%global rversion", private$spec_lines))] <-
          stringr::str_interp("%global rversion  ${thisr}")
        private$comment_spec(stringr::str_interp("- updated to R version [${thisr}]"))
      }
    },

    set_package_versions = function() {
      version <- private$pkgobj$get_version()
      private$spec_lines[which(grepl("^Version:", private$spec_lines))] <-
        stringr::str_interp("Version:          ${version}")
      release <- private$pkgobj$get_release()
      private$spec_lines[which(grepl("^%global packrel", private$spec_lines))] <-
        stringr::str_interp("%global packrel ${release}")
      private$comment_spec(stringr::str_interp("- updated [${private$pkgobj$get_pkg_name()}] package version to [${version}-${release}] by PackYak v${packageVersion(\"packyak\")}"))
    },


    update_source = function() {
      source <- private$pkgobj$get_source()
      private$spec_lines[which(grepl("^Source0:", private$spec_lines))] <-
        stringr::str_interp("Source0:          ${source}")

      private$spec_lines[which(grepl("^URL:", private$spec_lines))] <-
        stringr::str_interp("URL:              ${private$pkgobj$get_url()}")

      summary <- stringr::str_interp("PackYak v${packageVersion(\"packyak\")} build of R-package [${private$pkgobj$get_pkg_name()}] version [${private$pkgobj$get_version_str()}]")
      private$spec_lines[which(grepl("^Summary:", private$spec_lines))] <-
        stringr::str_interp("Summary:          ${summary}")

    },


    update_license = function() {
      license <- private$pkgobj$get_license()
      private$spec_lines[which(grepl("^License", private$spec_lines))] <-
        stringr::str_interp("License:          ${license}")
    },


    update_dependencies = function() {
      build_requires = "tex(latex) R-core = %{rversion}"
      requires = "tex(latex) R-core = %{rversion}"
      dependencies <- private$pkgobj$get_depends()
      if(!is.null(dependencies)) {
        cli::cli_alert(stringr::str_interp("linking dependencies [${paste(dependencies)}]"))
        for (dep in dependencies) {
          build_requires <- paste(build_requires, private$pkgobj$get_package_id(dep))
          requires <- paste(requires, private$pkgobj$get_package_id(dep))
        }
      }


      sys_reqs <- private$pkgobj$get_sys_reqs()
      if(!is.null(sys_reqs)) {
        cli::cli_alert(stringr::str_interp("linking sys_reqs [${paste(sys_reqs)}]"))
        for (dep in sys_reqs) {
          feature <- private$pkgobj$get_package_id(dep)
          if (!is.null(feature) && !feature=="NULL") {
            build_requires <- paste(build_requires, feature)
            requires <- paste(requires, feature)
          }
        }
      }

      private$spec_lines[which(grepl("^BuildRequires:", private$spec_lines))] <-
        stringr::str_interp("BuildRequires:    ${build_requires}")

      private$spec_lines[which(grepl("^Requires:", private$spec_lines))] <-
        stringr::str_interp("Requires:         ${requires}")

    },


    comment_spec = function(comment) {
      today <- paste(
        "*", as.character(lubridate::wday(lubridate::today(), label = TRUE)),
        as.character(lubridate::month(lubridate::today(), label = TRUE)),
        lubridate::day(lubridate::today()),
        lubridate::year(lubridate::today()),
        git2r::config()$global$user.name,
        paste0("<",git2r::config()$global$user.email,">"))
      if (today %in% private$spec_lines) {
        index <- which(private$spec_lines==today)
        private$spec_lines <- append(private$spec_lines, c(comment), after=index)
      } else {
        index <- which(private$spec_lines=="%changelog")
        private$spec_lines <- append(private$spec_lines, c(today, comment), after=index)
      }
    },

    get_version = function() {
      return(gsub("Version:\\s+","",private$spec_lines[which(grepl("^Version:",private$spec_lines))]))
    },

    get_release = function() {
      return(gsub("%global packrel\\s+","",private$spec_lines[which(grepl("^%global packrel ",private$spec_lines))]))
    }




  )
)
