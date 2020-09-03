#' Trace or untrace ggplot functions
#'
#' @param pkgs packages where to find functions to trace/untrace
#' @param prefixes prefixes of those functions
#' @param exceptions exceptions
#'
#' @export
trace_funs <- function(
  pkgs = "ggplot2",
  prefixes = c("annotate_", "coord_", "facet_", "geom_", "scale_", "stat_", "theme"),
  exceptions = c("scale_type", "theme_get", "coord_munch", "geom_blank")) {

  tracer <- quote({
    if(getOption("ggfail")) {

      scs <- sys.calls()
      n <- length(scs) - 4 # -4 to account for the trace overhead

      # Detect calls to `+` by the presence of a srcref
      call <- paste(as.character(getSrcref(scs[[n]]), useSource = TRUE), collapse = "")

      plus_call_lgl <- ifelse(nzchar(call),
                              identical(quote(`+`), str2lang(call)[[1]]),
                              FALSE)

      # check if the current comes from the pkg namespace by recursing through
      # the frame stack
      is_called_internally_lgl <- FALSE
      pkg_env <- asNamespace(pkg)
      while (n > 2 && !is_called_internally_lgl) {
        env <- sys.frames()[[n-1]]
        while(TRUE) {
          if (identical(env, emptyenv())) {
            n <- n - 1
            break
          }
          parent = parent.env(env)

          # avoid infinite loops
          if (identical(parent, env)) {
            n <- n - 1
            break
          }

          if (identical(parent, pkg_env)) {
            is_called_internally_lgl <- TRUE
            break
          }

          env <- parent
        }
      }

      if(n != 1 && !plus_call_lgl && !is_called_internally_lgl) {
        stop("Did you forget a `+` in a ggplot call ?\n",
             "Use \n`print(", paste(deparse(scs[[n]], width.cutoff = 500),
                                    collapse = "\n"),
             ")`\n to view the ",
             "object, or set `options(ggfail = FALSE)` to disable this error.",
             call. = FALSE)
      }
    }
  })

  for(pkg in pkgs) {
    all_funs <- getNamespaceExports(pkg)
    traced_funs <- all_funs[Reduce(`|`, lapply(prefixes, startsWith, x = all_funs))]
    # don't consider some prefixed functions
    traced_funs <- setdiff(traced_funs, exceptions)
    traced_funs <- lapply(traced_funs, as.name)
    # embed pkg name into tracer
    tracer <- as.call(list(tracer[[1]],
                           eval(substitute(quote(pkg <- x), list(x = pkg))),
                           tracer[[2]]))

    suppressMessages(trace(what = do.call(expression, traced_funs),
                           tracer = tracer, print = FALSE, where = asNamespace(pkg)))
  }
  invisible(NULL)
}


#' @rdname trace_funs
#' @export
untrace_funs <- function(
  pkgs = "ggplot2",
  prefixes = c("annotate_", "coord_", "facet_", "geom_", "scale_", "stat_", "theme"),
  exceptions = c("scale_type", "theme_get", "coord_munch")) {

  for(pkg in pkgs) {
    all_funs <- getNamespaceExports(pkg)
    traced_funs <- all_funs[Reduce(`|`, lapply(prefixes, startsWith, x = all_funs))]
    # don't consider some prefixed functions
    traced_funs <- setdiff(traced_funs, exceptions)
    # don't consider functions which aren't currently traced.
    # necessary to prevent methods:::.TraceWithMethods from erroring
    traced_funs <- traced_funs[
      sapply(traced_funs,
             function(x) is(getFunction(x, where = asNamespace(pkg)), "traceable"))
    ]
    traced_funs <- lapply(traced_funs, as.name)
    suppressMessages(
      untrace(do.call(expression, traced_funs), where = asNamespace(pkg)))
  }
  invisible(NULL)
}

