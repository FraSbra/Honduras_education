# Reusable LaTeX table helpers for ordinary R scripts.
#
# Install the two dependencies once with:
# install.packages(c("knitr", "kableExtra"))

.check_latex_table_packages <- function() {
  required <- c("knitr", "kableExtra")
  missing <- required[
	!vapply(required, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) > 0L) {
	stop(
	  "Install the missing package(s) first: ",
	  paste(sprintf('install.packages("%s")', missing), collapse = "; "),
	  call. = FALSE
	)
  }
}

# Build a LaTeX table object without writing a file. This is useful when you
# want to add further kableExtra layers before exporting.
make_latex_table <- function(
	data,
	caption = NULL,
	label = NULL,
	digits = 2,
	align = NULL,
	column_names = NULL,
	row_names = FALSE,
	format_args = list(),
	na = "",
	escape = TRUE,
	booktabs = TRUE,
	longtable = FALSE,
	striped = FALSE,
	scale_down = FALSE,
	hold_position = TRUE,
	repeat_header = longtable,
	font_size = NULL,
	notes = NULL) {
  .check_latex_table_packages()

  if (!is.data.frame(data) && !is.matrix(data)) {
	stop("`data` must be a data frame or matrix.", call. = FALSE)
  }

  if (!is.null(label)) {
	if (is.null(caption)) {
	  stop("Supply a caption when you supply a label.", call. = FALSE)
	}

	# knitr automatically adds the "tab:" prefix.
	label <- sub("^tab:", "", label)
	if (!nzchar(label) || grepl("[^[:alnum:]_.:-]", label)) {
	  stop(
		"`label` may contain letters, numbers, underscore, hyphen, colon, or period, but no spaces.",
		call. = FALSE
	  )
	}
  }

  if (longtable && scale_down) {
	stop(
	  "`scale_down` cannot be combined with `longtable`; use smaller text, fewer columns, or landscape pages.",
	  call. = FALSE
	)
  }

  old_options <- options(knitr.kable.NA = na)
  on.exit(options(old_options), add = TRUE)

  table <- kableExtra::kbl(
	x = data,
	format = "latex",
	digits = digits,
	row.names = row_names,
	col.names = if (is.null(column_names)) NA else column_names,
	align = align,
	caption = caption,
	label = label,
	format.args = format_args,
	escape = escape,
	booktabs = booktabs,
	longtable = longtable,
	linesep = ""
  )

  latex_options <- "basic"
  if (striped) {
	latex_options <- c(latex_options, "striped")
  }
  if (hold_position && !longtable) {
	latex_options <- c(latex_options, "hold_position")
  }
  if (scale_down) {
	latex_options <- c(latex_options, "scale_down")
  }
  if (repeat_header && longtable) {
	latex_options <- c(latex_options, "repeat_header")
  }

  table <- kableExtra::kable_styling(
	table,
	latex_options = latex_options,
	full_width = FALSE,
	position = "center",
	font_size = font_size
  )

  if (!is.null(notes) && length(notes) > 0L) {
	table <- kableExtra::footnote(
	  table,
	  general = as.character(notes),
	  general_title = "Note: ",
	  footnote_as_chunk = TRUE,
	  threeparttable = !longtable,
	  escape = escape
	)
  }

  table
}

# Build and export a complete LaTeX table fragment. Re-running a reproducible
# script replaces only the named output file unless overwrite = FALSE.
export_latex_table <- function(
	data,
	file,
	...,
	overwrite = TRUE) {
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
	stop("`file` must be one path ending in .tex.", call. = FALSE)
  }
  if (tolower(tools::file_ext(file)) != "tex") {
	stop("`file` must end in .tex.", call. = FALSE)
  }
  if (file.exists(file) && !overwrite) {
	stop("The output already exists. Use `overwrite = TRUE` to replace it.", call. = FALSE)
  }

  table <- make_latex_table(data = data, ...)
  output_directory <- dirname(file)
  if (!dir.exists(output_directory)) {
	dir.create(output_directory, recursive = TRUE)
  }

  writeLines(enc2utf8(as.character(table)), con = file, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

