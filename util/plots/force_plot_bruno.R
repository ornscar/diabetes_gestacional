force_plot_bruno <- 
  function (object, baseline = NULL, feature_values = NULL, display = c("viewer", 
                                                                        "html"), ...) 
  {
    if (!requireNamespace("reticulate", quietly = TRUE)) {
      stop("Package \"reticulate\" needed for this function to work. Please ", 
           "install it.", call. = FALSE)
    }
    if (!reticulate::py_module_available("shap")) {
      stop("The Python package \"shap\" is needed for this function to work. ", 
           "Please install it; visit https://github.com/slundberg/shap for ", 
           "details.", call. = FALSE)
    }
    if (is.null(baseline)) {
      baseline <- attr(object, which = "baseline")
      if (length(unique(baseline)) == 1) {
        baseline <- unique(baseline)
      }
    }
    if (!is.null(feature_values)) {
      feature_values <- data.matrix(feature_values)
    }
    shap <- reticulate::import("shap")
    fp <- shap$force_plot(base_value = if (is.null(baseline)) 
      0
      else baseline, shap_values = data.matrix(object), features = feature_values, 
      feature_names = names(object), matplotlib = FALSE, ...)
    tfile <- tempfile(fileext = ".html")
    shap$save_html(tfile, plot = fp)
    # shap$save_html(tfile)
    display <- match.arg(display)
    if (display == "viewer") {
      if (requireNamespace("rstudioapi", quietly = TRUE)) {
        if (rstudioapi::isAvailable()) {
          rstudioapi::viewer(tfile)
        }
      }
      else if (requireNamespace("utils", quietly = TRUE)) {
        utils::browseURL(tfile)
      }
      else {
        stop("Packages \"rstudioapi\" or \"utils\" needed for this function to ", 
             "work. Please install one of them.", call. = FALSE)
      }
    }
    else {
      if (!requireNamespace("htmltools", quietly = TRUE)) {
        stop("Package \"htmltools\" needed for this function to work. Please ", 
             "install it.", call. = FALSE)
      }
      htmltools::includeHTML(tfile)
    }
  }
