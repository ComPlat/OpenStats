env_utils_V1_2 <- new.env(parent = getNamespace("OpenStats"))

split_formula <- function(formula) {
  f <- as.character(formula)
  list(
    response = str2lang(f[2]),
    right_site = str2lang(f[3])
  )
}
env_utils_V1_2$split_formula <- split_formula

vars_rhs <- function(rhs) {
 all.vars(rhs)
}
env_utils_V1_2$vars_rhs <- vars_rhs

num_to_factor <- function(df, cols) {
  for (i in seq_along(cols)) {
    if (is.numeric(df[, cols[i]])) {
      df[, cols[i]] <- as.factor(df[, cols[i]])
    }
  }
  return(df)
}
env_utils_V1_2$num_to_factor <- num_to_factor

char_to_orig_type <- function(vec) {
  if (is.list(vec)) vec <- unlist(vec)
  if (any(is.na(as.numeric(vec)))) {
    return(vec)
  }
  vec <- as.numeric(vec)
  vec
}
env_utils_V1_2$char_to_orig_type <- char_to_orig_type

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
env_utils_V1_2$firstup <- firstup

df_2_string <- function(df) {
  stopifnot(
    "Input to env_utils_V1_2$df_2_string is not of type DataFrame" = is.data.frame(df)
  )
  resNames <- names(df)
  resNames <- paste(resNames, collapse = "\t")
  res <- apply(df, 1, function(x) {
    x <- as.character(x)
    x <- paste(x, collapse = "\t")
    return(x)
  })
  res <- c(resNames, res)
  res <- paste0(res, "\n")
  res <- Reduce(paste0, res)
  return(res)
}
env_utils_V1_2$df_2_string <- df_2_string

create_excel_file <- function(l) {
  if (length(l) == 0) {
    print_warn("Nothing to upload")
    return(NULL)
  }

  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, "Results")

  curr_row <- 1
  plot_files <- c()
  line_style <- openxlsx::createStyle(border = "bottom", borderStyle = "thick")
  # save data to excel file
  for (i in seq_along(l)) {
    openxlsx::writeData(wb, "Results", names(l)[i], startRow = curr_row)
    curr_row <- curr_row + 2
    if (inherits(l[[i]], "plot")) {
      p <- l[[i]]@p
      width <- l[[i]]@width
      height <- l[[i]]@height
      resolution <- l[[i]]@resolution
      fn <- tempfile(fileext = ".png")
      ggsave(
        plot = p,
        filename = fn, width = width, height = height, dpi = resolution
      )
      plot_files <- c(plot_files, fn)
      openxlsx::insertImage(wb, "Results", fn, startRow = curr_row)
      curr_row <- curr_row + 20
      openxlsx::addStyle(
        wb, sheet = "Results", style = line_style, rows = curr_row,
        cols = 1:width,
        gridExpand = TRUE
      )
      curr_row <- curr_row + 2
    } else if (inherits(l[[i]], "diagnosticPlot")) {
      p <- l[[i]]@p
      width <- l[[i]]@width
      height <- l[[i]]@height
      resolution <- l[[i]]@resolution
      fn <- tempfile(fileext = ".png")
      ggsave(
        plot = p,
        filename = fn, width = width, height = height, dpi = resolution
      )
      openxlsx::insertImage(wb, "Results", fn, startRow = curr_row)
      curr_row <- curr_row + 20
      plot_files <- c(plot_files, l[[i]]@p) # TODO: why????
      plot_files <- c(plot_files, fn)
      openxlsx::addStyle(
        wb, sheet = "Results", style = line_style, rows = curr_row,
        cols = 1:width,
        gridExpand = TRUE
      )
      curr_row <- curr_row + 5
    } else if (inherits(l[[i]], "doseResponse")) {
      openxlsx::writeData(wb, "Results", l[[i]]@df, startRow = curr_row)
      curr_row <- curr_row + nrow(l[[i]]@df) + 5
      p <- l[[i]]@p
      for (idx in seq_len(length(p))) {
        fn <- tempfile(fileext = ".png")
        ggsave(plot = p[[idx]], filename = fn)
        openxlsx::insertImage(wb, "Results", fn, startRow = curr_row)
        curr_row <- curr_row + 20
        plot_files <- c(plot_files, fn)
      }
      openxlsx::addStyle(
        wb, sheet = "Results", style = line_style, rows = curr_row,
        cols = 1:dim(l[[i]]@df)[2],
        gridExpand = TRUE
      )
      curr_row <- curr_row + 5
    } else if (inherits(l[[i]], "summaryModel")) {
      p <- l[[i]]@p@p
      s <- l[[i]]@summary
      ic <- l[[i]]@information_criterions
      fn <- tempfile(fileext = ".png")
      ggsave(plot = p, filename = fn)
      openxlsx::insertImage(wb, "Results", fn, startRow = curr_row)
      curr_row <- curr_row + 20
      plot_files <- c(plot_files, fn)
      openxlsx::writeData(wb, "Results", s, startRow = curr_row)
      curr_row <- curr_row + dim(s)[1] + 2
      openxlsx::writeData(wb, "Results", ic, startRow = curr_row)
      curr_row <- curr_row + dim(ic)[1] + 2
      openxlsx::addStyle(
        wb, sheet = "Results", style = line_style, rows = curr_row,
        cols = 1:dim(l[[i]]@summary)[2],
        gridExpand = TRUE
      )
    } else if (inherits(l[[i]], "data.frame")) {
      openxlsx::writeData(wb, "Results", l[[i]], startRow = curr_row)
      curr_row <- curr_row + dim(l[[i]])[1] + 1
      openxlsx::addStyle(
        wb, sheet = "Results", style = line_style, rows = curr_row,
        cols = 1:dim(l[[i]])[2],
        gridExpand = TRUE
      )
      curr_row <- curr_row + 5
    } else if (is.character(l[[i]])) {
      openxlsx::writeData(wb, "Results", l[[i]], startRow = curr_row)
      curr_row <- curr_row + length(l[[i]])[1] + 1
      openxlsx::addStyle(
        wb, sheet = "Results", style = line_style, rows = curr_row,
        cols = 1,
        gridExpand = TRUE
      )
      curr_row <- curr_row + length(l[[i]])[1] + 5
    }
  }

  # create temporary file
  file <- function() {
    # TODO: is it necessary to store this in this folder. Or could i use tempfile without dir argument?
    # Is it needed in the docker container?
    # tempfile <- tempfile(tmpdir = "/home/shiny/results", fileext = ".xlsx")
    tempfile <- tempfile(fileext = ".xlsx")
    return(tempfile)
  }
  fn <- file()


  # save workbook
  res <- tryCatch(
    expr = {
      openxlsx::saveWorkbook(wb, fn)
    },
    error = function(e) {
      print_err("Error saving file")
    }
  )

  # Clean up
  unlink(plot_files[file.exists(plot_files)], force = TRUE)

  return(fn)
}
env_utils_V1_2$create_excel_file <- create_excel_file

create_js_string <- function(l) {
  names_l <- names(l)
  jsString <- c()
  js_names <- c()
  for (i in seq_along(l)) {
    if (inherits(l[[i]], "plot")) {
      p <- l[[i]]@p
      width <- l[[i]]@width
      height <- l[[i]]@height
      resolution <- l[[i]]@resolution
      fn <- tempfile(fileext = ".png")
      ggsave(
        plot = p,
        filename = fn, width = width, height = height, dpi = resolution
      )
      jsString <- c(jsString, paste0("data:image/png;base64,", base64enc::base64encode(fn)))
      unlink(fn)
      js_names <- c(js_names, names_l[i])
    } else if (inherits(l[[i]], "diagnosticPlot")) {
      jsString <- c(jsString, paste0("data:image/png;base64,", base64enc::base64encode(l[[i]]@p)))
      unlink(l[[i]]@p)
      js_names <- c(js_names, names_l[i])
    } else if (inherits(l[[i]], "doseResponse")) {
      p <- l[[i]]@p
      fn <- tempfile(fileext = ".png") # TODO: check is this used?
      for (idx in seq_len(length(p))) {
        fn <- tempfile(fileext = ".png")
        ggsave(plot = p[[idx]], filename = fn)
        jsString <- c(jsString, paste0("data:image/png;base64,", base64enc::base64encode(fn)))
        unlink(fn)
        js_names <- c(js_names, paste0(names_l[i], "_PlotNr", idx))
      }
      unlink(fn)
      jsString <- c(jsString, env_utils_V1_2$df_2_string(l[[i]]@df))
      js_names <- c(js_names, names_l[i])
    } else if (inherits(l[[i]], "summaryModel")) {
      p <- l[[i]]@p@p
      fn <- tempfile(fileext = ".png")
      ggsave(plot = p, filename = fn)
      jsString <- c(jsString, paste0("data:image/png;base64,", base64enc::base64encode(fn)))
      unlink(fn)
      js_names <- c(js_names, paste0(names_l[i], " plot"))

      jsString <- c(jsString, env_utils_V1_2$df_2_string(l[[i]]@summary))
      js_names <- c(js_names, names_l[i])

      jsString <- c(jsString, env_utils_V1_2$df_2_string(l[[i]]@information_criterions))
      js_names <- c(js_names, paste0(names_l[i], " Information criterions"))
    } else if (inherits(l[[i]], "data.frame")) {
      jsString <- c(jsString, env_utils_V1_2$df_2_string(l[[i]]))
      js_names <- c(js_names, names_l[i])
    } else if (is.character(l[[i]])) {
      jsString <- c(jsString, l[[i]])
      js_names <- c(js_names, names_l[i])
    }
  }
  return(list(jsString, js_names))
}
env_utils_V1_2$create_js_string <- create_js_string

stack_df <- function(df, keepCol) {
  as.data.frame(pivot_longer(df,
    cols = -keepCol,
    names_to = "name", values_to = "value"
  ))
}
env_utils_V1_2$stack_df <- stack_df

unstack_df <- function(df, name, value) {
  df <- pivot_wider(df, names_from = name, values_from = value)
  df <- map(df, simplify) %>% # TODO: is this from purrr? Maybe use lapply
    as.data.frame()
  as.data.frame(df)
}
env_utils_V1_2$unstack_df <- unstack_df

correct_name <- function(name, df) {
  name %in% names(df)
}
env_utils_V1_2$correct_name <- correct_name

change_char_input <- function(chars) {
  nams <- unlist(strsplit(chars, split = ","))
  for (i in 1:length(nams)) {
    nams[i] <- gsub(" ", "", nams[i])
  }
  nams
}
env_utils_V1_2$change_char_input <- change_char_input

combine <- function(new, vec, df, first) {
  if (length(vec) == 0) {
    return(new)
  }
  if (env_utils_V1_2$correct_name(vec[length(vec)], df)) {
    if (isTRUE(first)) {
      new <- df[, vec[length(vec)]]
      first <- FALSE
    } else {
      new <- interaction(new, df[, vec[length(vec)]])
    }
  }
  vec <- vec[-length(vec)]
  env_utils_V1_2$combine(new, vec, df, first)
}
env_utils_V1_2$combine <- combine

split_data <- function(df, formula) {
  df <- model.frame(formula, data = df)
  stopifnot(ncol(df) >= 2)
  res <- data.frame(
    value = df[, 1], interaction = interaction(df[, 2:ncol(df)])
  )
  res
}
env_utils_V1_2$split_data <- split_data

create_df_name <- function(current_df_name, column_names) {
  if (!(current_df_name %in% column_names)) {
    return(current_df_name)
  }
  counter <- 1
  while (TRUE) {
    current_df_name <- paste0(current_df_name, counter)
    counter <- counter + 1
    if (!(current_df_name %in% column_names)) {
      return(current_df_name)
    }
  }
}
env_utils_V1_2$create_df_name <- create_df_name

create_r_names <- function(df) {
  names <- sapply(names(df), make.names)
  names(df) <- names
  return(df)
}
env_utils_V1_2$create_r_names <- create_r_names

# Split groups
split_groups <- function(df, cols, levels) {
  df_res <- df
  for (i in seq_along(cols)) {
    levels_temp <- levels[levels %in% unique(df_res[, cols[i]])]
    df_res <- df_res[df_res[, cols[i]] %in% levels_temp, ]
  }
  if (nrow(df_res) == 0) stop("Subset contains 0 rows")
  return(df_res)
}
env_utils_V1_2$split_groups <- split_groups

# Check axis limits
check_axis_limits <- function(col, min, max) { # TODO: is this still required?
  if (is.numeric(col)) {
    if (!is.numeric(min) || !is.numeric(max)) {
      stop("Found invalid axis limits")
    }
    if (max <= min) {
      stop("Found invalid axis limits: max <= min")
    }
    return()
  } else {
    choices <- unique(col)
    if (length(choices) == 1) {
      if (!(min == choices && max == choices)) {
        stop("If only one level is available the max and min value have to be set to this value!")
      }
    } else {
      if (!(min %in% choices) || !(max %in% choices)) {
        stop("Found invalid axis limits")
      }
      if (which(max == choices) <= which(min == choices)) {
        stop("Found invalid axis limits. The max value is found before the min value")
      }
    }
    return()
  }
}
env_utils_V1_2$check_axis_limits <- check_axis_limits

# Check filename
extract_extension <- function(filename) {
  ex <- strsplit(basename(filename), split = "\\.")[[1]]
  ex <- ex[[length(ex)]]
  return(ex)
}
env_utils_V1_2$extract_extension <- extract_extension

is_valid_filename <- function(filename) {
  try({
    if (!is.character(filename)) {
      return(FALSE)
    }
    if (grepl(" ", filename)) {
      return(FALSE)
    }
    invalid_chars <- "[<>:\"/\\|?*]"
    if (grepl(invalid_chars, filename)) {
      return(FALSE)
    }
    if (nchar(filename) == 0) {
      return(FALSE)
    }
    if (nchar(filename) >= 100) {
      return(FALSE)
    }
    ex <- strsplit(basename(filename), split = "\\.")[[1]]
    if (length(ex) == 1) { # no extension found
      return(FALSE)
    }
    return(TRUE)
  })
}
env_utils_V1_2$is_valid_filename <- is_valid_filename

why_filename_invalid <- function(filename) {
  try({
    if (!is.character(filename)) {
      return("Filename has to consist of characters")
    }
    if (grepl(" ", filename)) {
      return("Found spaces in filename")
    }
    invalid_chars <- "[<>:\"/\\|?*]"
    if (grepl(invalid_chars, filename)) {
      return("Found invalid chars in filename: [<>:\"\\|?*")
    }
    if (nchar(filename) == 0) {
      return("Filename is empty")
    }
    if (nchar(filename) >= 100) {
      return("Filename is too long (> 100 characters)")
    }
    ex <- strsplit(basename(filename), split = "\\.")[[1]]
    if (length(ex) == 1) { # no extension found
      return("Filename extension is missing")
    }
    return("")
  })
}
env_utils_V1_2$why_filename_invalid <- why_filename_invalid

check_filename_for_server <- function(filename) {
  ex <- strsplit(basename(filename), split = "\\.")[[1]]
  ex <- ex[[length(ex)]]
  ex == "xlsx"
}
env_utils_V1_2$check_filename_for_server <- check_filename_for_server

check_filename_for_serverless <- function(filename) {
  ex <- env_utils_V1_2$extract_extension(filename)
  ex <- ex[[length(ex)]]
  ex == "zip"
}
env_utils_V1_2$check_filename_for_serverless <- check_filename_for_serverless

# Split list of plots into panels of 9 plots
create_plot_pages <- function(plotList) { # TODO: probably not needed anymore. Remove!
  if (length(plotList) == 0) {
    plotList <- list(ggplot2::ggplot() +
      ggplot2::geom_point())
  }
  n_full_pages <- floor(length(plotList) / 9)
  if (n_full_pages == 0) {
    return(list(cowplot::plot_grid(plotlist = plotList)))
  }
  n_plots_last_page <- length(plotList) %% 9
  res <- list()
  i <- 1
  for (i in seq_len(n_full_pages)) {
    if (i == 1) {
      res[[i]] <- plotList[1:(i * 9)]
    } else {
      res[[i]] <- plotList[((i - 1) * 9 + 1):(i * 9)]
    }
  }
  res[[i + 1]] <- plotList[(n_full_pages * 9 + 1):
  (n_full_pages * 9 + n_plots_last_page)]
  lapply(res, function(x) {
    cowplot::plot_grid(plotlist = x)
  })
}
env_utils_V1_2$create_plot_pages <- create_plot_pages

# check result list size (rls)
# Here also the length is checked
check_rls <- function(ResultsState, newObj) {
  if (length(ResultsState) > 1000) {
    stop("You can only store 1000 results. Consider removing some results")
  }
  current_size <- object.size(ResultsState)
  max_size <- 500 * 1024^2 # 500 MB per user
  if (current_size + object.size(newObj) > max_size) {
    stop("Memory limit exceeded for user results. Consider removing some results.")
  }
}
env_utils_V1_2$check_rls <- check_rls

# internal dataframe function
elongate_col <- function(col, l) {
  times <- l / length(col)
  if (floor(times) == times) {
    return(rep(col, times))
  } else {
    res <- rep(col, floor(times))
    remaining_elems <- l %% length(col)
    res <- c(res, col[1:remaining_elems])
    return(res)
  }
}
env_utils_V1_2$elongate_col <- elongate_col
