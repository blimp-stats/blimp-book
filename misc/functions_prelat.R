# GLOBAL STYLING AND HELPERS ----

plot_colors <- c(
  red    = "#F8766D",
  green  = "#7CAE00",
  teal   = "#00BFC4",
  violet = "#C77CFF",
  orange = "#E58700",
  blue   = "#619CFF",
  yellow = "#FFD700",
  pink   = "#FF61C3"
)

# alpha used for filled layers (bars, ribbons, etc.) across every plot
plot_shading <- 0.25

# MAD-based winsorization used in multiple plotting functions
winsor_mad <- function(x, k = 3) {
  med <- stats::median(x, na.rm = TRUE)
  mad <- stats::mad(x, constant = 1.4826, na.rm = TRUE)
  if (!is.finite(mad) || mad == 0) return(x)
  lo <- med - k * mad
  hi <- med + k * mad
  pmin(pmax(x, lo), hi)
}

# SHAred THEME ----

blimp_theme <- function(font_size = 14) {
  base_theme <- ggplot2::theme_minimal(base_size = font_size)
  replace_theme <- ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.ticks.y     = ggplot2::element_blank(),
    axis.text.y      = ggplot2::element_blank(),
    axis.text.x      = ggplot2::element_text(
      size   = font_size,
      margin = ggplot2::margin(t = 2)
    ),
    axis.title.x     = ggplot2::element_text(
      size   = font_size,
      margin = ggplot2::margin(t = 12)
    ),
    plot.title       = ggplot2::element_text(
      hjust = 0.5,
      face  = "bold",
      size  = font_size + 2
    ),
    legend.title     = ggplot2::element_text(size = font_size),
    legend.text      = ggplot2::element_text(size = font_size),
    legend.key.height = ggplot2::unit(1.1, "lines"),
    legend.key.width  = ggplot2::unit(1.6, "lines"),
    legend.position   = c(0.92, 0.85),
    legend.justification = c("right", "top"),
    legend.background = ggplot2::element_blank(),
    legend.key        = ggplot2::element_blank()
  )
  
  ggplot2::`%+replace%`(base_theme, replace_theme)
}

# EXTRACT VARIABLES FROM MODEL SECTION ----

# helper: coerce @syntax to a single text blob
.syntax_as_text <- function(sx) {
  if (inherits(sx, "blimp_syntax") || is.list(sx)) {
    # modern BLIMP objects
    paste(unlist(sx, use.names = FALSE), collapse = "\n")
  } else if (is.character(sx)) {
    # plain text syntax
    paste(sx, collapse = "\n")
  } else {
    # S4 or any other object: fall back to as.character()
    paste(as.character(sx), collapse = "\n")
  }
}

# helper: pull the MODEL block lines (handles focal:/predictors: etc.)
# Returns only the MODEL block, handling both modern list syntax (sx$model)
# and legacy flat text with "MODEL:" and other section headers.
.get_model_block <- function(model) {
  if (is.null(model@syntax)) return(character(0))
  sx <- model@syntax
  
  # Case 1: modern blimp_syntax/list with a $model component
  if ((inherits(sx, "blimp_syntax") || is.list(sx)) && "model" %in% names(sx)) {
    mb <- as.character(sx$model)
    mb <- mb[!is.na(mb)]
    mb <- trimws(mb)
    mb <- mb[nzchar(mb)]
    return(mb)
  }
  
  # Case 2: plain text / legacy syntax with "MODEL:" header
  txt <- .syntax_as_text(sx)
  if (!nzchar(txt)) return(character(0))
  
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
  if (!length(lines)) return(character(0))
  
  idx_model <- grep("^\\s*MODEL\\s*:", lines, ignore.case = TRUE)
  if (!length(idx_model)) return(character(0))
  
  lines2 <- lines[idx_model[1]:length(lines)]
  # remove the literal "MODEL:" label from the first line
  lines2[1] <- sub("^\\s*MODEL\\s*:\\s*", "", lines2[1], ignore.case = TRUE)
  
  # stop at next ALL-CAPS header (SEED:, OPTIONS:, SAVE:, etc.)
  if (length(lines2) > 1L) {
    hdr_idx <- which(grepl("^\\s*[A-Z]+\\s*:", lines2[-1L]))
    if (length(hdr_idx)) {
      stop_at <- hdr_idx[1L] + 1L
      lines2  <- lines2[seq_len(stop_at)]
    }
  }
  
  lines2
}

# ORDINAL / NOMINAL HELPER ----
# Returns variable names declared in ORDINAL/NOMINAL sections, expanding ranges
# like dep1:dep7 -> dep1, ..., dep7. Works for both modern list syntax
# ($ordinal/$nominal) and legacy flat text with ORDINAL:/NOMINAL: headers.

.get_categorical_vars <- function(model) {
  sx  <- model@syntax
  out <- character(0)
  
  # ---- 1) Modern blimp_syntax/list case: use $ordinal / $nominal directly ----
  if ((inherits(sx, "blimp_syntax") || is.list(sx)) &&
      any(c("ordinal", "nominal") %in% names(sx))) {
    
    pieces <- c(
      if ("ordinal" %in% names(sx)) sx$ordinal else NULL,
      if ("nominal" %in% names(sx)) sx$nominal else NULL
    )
    pieces <- pieces[!is.na(pieces)]
    
    if (length(pieces)) {
      tokens <- unlist(
        strsplit(paste(pieces, collapse = " "), "\\s+"),
        use.names = FALSE
      )
      tokens <- tokens[nzchar(tokens)]
      
      # expand tokens like dep1:dep7 into dep1, dep2, ..., dep7
      expand_range <- function(tok) {
        if (!grepl(":", tok, fixed = TRUE)) return(tok)
        
        # match dep1:dep7  (same base on both sides)
        # group 1 = base ("dep"), group 2 = start index ("1"), group 3 = end index ("7")
        m <- regexec("^([A-Za-z._]+)([0-9]+):\\1([0-9]+)$", tok, perl = TRUE)
        res <- regmatches(tok, m)[[1]]
        if (length(res) != 4L) return(tok)  # didn't match dep1:dep7 form
        
        base  <- res[2]
        start <- as.integer(res[3])
        end   <- as.integer(res[4])
        if (!is.finite(start) || !is.finite(end)) return(tok)
        
        if (start <= end) {
          paste0(base, seq.int(start, end))
        } else {
          paste0(base, seq.int(start, end))
        }
      }
      
      out <- unique(unlist(lapply(tokens, expand_range), use.names = FALSE))
    }
  }
  
  # ---- 2) Fallback: older "flat text" case via ORDINAL:/NOMINAL: labels ----
  if (!length(out)) {
    txt <- .syntax_as_text(sx)
    if (!nzchar(txt)) return(character(0))
    
    txt1 <- gsub("\\s+", " ", txt)
    
    grab <- function(key) {
      rx <- paste0("(?i)\\b", key, "\\s*:\\s*([^;]+)\\s*;")
      m  <- gregexpr(rx, txt1, perl = TRUE)
      segs <- regmatches(txt1, m)[[1]]
      if (!length(segs)) return(character(0))
      
      toks <- unlist(lapply(segs, function(seg) {
        s <- sub(rx, "\\1", seg, perl = TRUE)  # captured list of names
        s <- gsub(",", " ", s)                 # allow commas or spaces
        tt <- strsplit(s, "\\s+", perl = TRUE)[[1]]
        tt[nzchar(tt)]
      }), use.names = FALSE)
      
      unique(toks)
    }
    
    tokens <- unique(c(grab("ORDINAL"), grab("NOMINAL")))
    if (!length(tokens)) return(character(0))
    
    expand_range <- function(tok) {
      if (!grepl(":", tok, fixed = TRUE)) return(tok)
      
      m <- regexec("^([A-Za-z._]+)([0-9]+):\\1([0-9]+)$", tok, perl = TRUE)
      res <- regmatches(tok, m)[[1]]
      if (length(res) != 4L) return(tok)
      
      base  <- res[2]
      start <- as.integer(res[3])
      end   <- as.integer(res[4])
      if (!is.finite(start) || !is.finite(end)) return(tok)
      
      if (start <= end) {
        paste0(base, seq.int(start, end))
      } else {
        paste0(base, seq.int(start, end))
      }
    }
    
    out <- unique(unlist(lapply(tokens, expand_range), use.names = FALSE))
  }
  
  out
}

# MAP MODEL PREDICTOR NAME TO COLUMN NAME ----

map_predictor_to_col <- function(pred_name, cols, cluster_id = NULL) {
  # 1) direct match
  if (pred_name %in% cols) {
    return(pred_name)
  }
  
  # 2) handle ".mean" predictors when there is a cluster_id
  #    Examples:
  #      MODEL: frlunch.mean  -> frlunch.1.mean[school]
  #      MODEL: probsolvpre.mean -> probsolvpre.mean[school]
  #      MODEL: stanmath.mean -> stanmath.mean[school]
  if (!is.null(cluster_id) && grepl("\\.mean$", pred_name)) {
    base <- sub("\\.mean$", "", pred_name)
    
    # (a) nominal-style: base.<digit>.mean[cluster_id]
    #     e.g. frlunch.1.mean[school]
    pattern1 <- paste0("^", base, "\\.[0-9]+\\.mean\\[", cluster_id, "\\]$")
    cands1   <- grep(pattern1, cols, value = TRUE)
    if (length(cands1) == 1L) {
      return(cands1)
    } else if (length(cands1) > 1L) {
      message("Multiple matches for PREDICTOR '", pred_name,
              "' with cluster_id '", cluster_id,
              "'. Using: ", cands1[1])
      return(cands1[1])
    }
    
    # (b) continuous-style: base.mean[cluster_id]
    #     e.g. probsolvpre.mean[school], stanmath.mean[school]
    pattern2 <- paste0("^", base, "\\.mean\\[", cluster_id, "\\]$")
    cands2   <- grep(pattern2, cols, value = TRUE)
    if (length(cands2) == 1L) {
      return(cands2)
    } else if (length(cands2) > 1L) {
      message("Multiple matches for PREDICTOR '", pred_name,
              "' with cluster_id '", cluster_id,
              "'. Using: ", cands2[1])
      return(cands2[1])
    }
  }
  
  # 3) no mapping found; return original name so caller can handle failure
  pred_name
}

# CLUSTERID HELPER ----
.get_cluster_id <- function(model) {
  sx <- model@syntax
  
  # strip anything after the first ';' and trim
  strip_after_semicolon <- function(x) {
    x <- as.character(x)[1L]
    if (!nzchar(x)) return(NA_character_)
    x <- sub(";.*$", "", x)
    x <- trimws(x)
    ifelse(nzchar(x), x, NA_character_)
  }
  
  # given a raw CLUSTERID value, return a single chosen ID
  pick_cluster_id <- function(raw_val) {
    val <- strip_after_semicolon(raw_val)
    if (is.na(val) || !nzchar(val)) return(NA_character_)
    
    # split on whitespace (e.g., "student school")
    ids <- unlist(strsplit(val, "[[:space:]]+", perl = TRUE), use.names = FALSE)
    ids <- ids[nzchar(ids)]
    if (!length(ids)) return(NA_character_)
    
    # if only one, just return it
    if (length(ids) == 1L) return(ids[1L])
    
    # if two (or more), choose the one with more unique values in the data
    # use @average_imp if available, otherwise first imputation
    df <- NULL
    if (!is.null(model@average_imp)) {
      df <- model@average_imp
    } else if (is.list(model@imputations) && length(model@imputations) > 0L) {
      df <- model@imputations[[1L]]
    }
    
    # if we can't get data, or none of the IDs are in the data, fall back to first
    if (is.null(df) || !is.data.frame(df)) return(ids[1L])
    
    present <- ids[ids %in% names(df)]
    if (!length(present)) return(ids[1L])
    if (length(present) == 1L) return(present[1L])
    
    # compute number of unique values for each candidate
    nuniq <- vapply(present, function(v) {
      x <- df[[v]]
      x <- x[!is.na(x)]
      length(unique(x))
    }, integer(1L))
    
    # choose the ID with the largest number of unique values
    present[which.max(nuniq)][1L]
  }
  
  ## Case 1: modern blimp_syntax / list: use the $clusterid element
  if (inherits(sx, "blimp_syntax") || is.list(sx)) {
    nms <- tolower(names(sx))
    i   <- match("clusterid", nms)
    if (!is.na(i)) {
      return(pick_cluster_id(sx[[i]]))
    }
  }
  
  ## Case 2: legacy: parse CLUSTERID from the syntax text
  txt <- .syntax_as_text(sx)
  if (!nzchar(txt)) return(NA_character_)
  
  m <- regexpr("(?i)CLUSTERID\\s*:\\s*([^\\n]+)", txt, perl = TRUE)
  if (m < 0) return(NA_character_)
  
  seg <- regmatches(txt, m)
  seg <- sub("(?i)CLUSTERID\\s*:\\s*", "", seg, perl = TRUE)
  
  pick_cluster_id(seg)
}

# COUNT CLUSTERID VARIABLES ----
# Returns:
#   0 → no CLUSTERID declared (single-level model)
#   1 → one CLUSTERID (two-level model)
#   2 → two CLUSTERIDs (three-level model)
.count_cluster_ids <- function(model) {
  sx      <- model@syntax
  ids_vec <- character(0)
  
  # Case 1: modern syntax list/blimp_syntax with $clusterid element
  if ((inherits(sx, "blimp_syntax") || is.list(sx)) &&
      "clusterid" %in% names(sx)) {
    raw <- as.character(sx$clusterid)[1]
    if (nzchar(raw)) {
      # strip trailing stuff after ';' if present
      raw  <- sub(";.*$", "", raw)
      toks <- strsplit(raw, "[[:space:]]+", perl = TRUE)[[1]]
      ids_vec <- toks[nzchar(toks)]
    }
  } else {
    # Case 2: legacy flat text with CLUSTERID: header
    txt <- .syntax_as_text(sx)
    if (nzchar(txt)) {
      m <- regexpr("(?i)CLUSTERID\\s*:\\s*([^\\n]+)", txt, perl = TRUE)
      if (m > 0) {
        seg  <- regmatches(txt, m)
        seg  <- sub("(?i)CLUSTERID\\s*:\\s*", "", seg, perl = TRUE)
        seg  <- sub(";.*$", "", seg)
        toks <- strsplit(seg, "[[:space:]]+", perl = TRUE)[[1]]
        ids_vec <- toks[nzchar(toks)]
      }
    }
  }
  
  length(ids_vec)
}

# LEVEL-2 PREDICTOR DETECTION ----

.get_level2_predictors <- function(model, dv_pred_pairs, cols, cluster_id) {
  # If we don't have a cluster id or it isn't in the data, nothing to do
  if (is.null(cluster_id) || !cluster_id %in% cols) return(character(0))
  if (!length(dv_pred_pairs)) return(character(0))
  
  # Use the first imputation to determine cluster-constant variables
  d <- model@imputations[[1]]
  if (!cluster_id %in% names(d)) return(character(0))
  
  g <- d[[cluster_id]]
  if (all(is.na(g))) return(character(0))
  
  # All predictors appearing on RHS of any DV in dv_pred_pairs (MODEL-scale names)
  all_preds <- unique(unlist(dv_pred_pairs, use.names = FALSE))
  all_preds <- all_preds[nzchar(all_preds)]
  
  l2_preds <- character(0)
  
  for (pn in all_preds) {
    # Map MODEL PREDICTOR name -> actual column in imputations
    colname <- map_predictor_to_col(pn, cols, cluster_id)
    if (!colname %in% names(d)) next
    
    x <- d[[colname]]
    # split by cluster, check if each cluster has <= 1 unique value
    by_cl <- split(x, g)
    if (!length(by_cl)) next
    
    const_within <- vapply(by_cl, function(z) {
      z <- z[is.finite(z)]
      if (!length(z)) TRUE else length(unique(z)) <= 1L
    }, logical(1L))
    
    if (all(const_within)) {
      l2_preds <- c(l2_preds, pn)
    }
  }
  
  unique(l2_preds)
}

# EXTRACT VARIABLES FROM MODEL SECTION ----
# Extracts all variable names appearing anywhere in the MODEL section,
# after stripping path labels like x@b1. Also appends categorical vars
# from ORDINAL/NOMINAL. Used to decide which variables to plot by default.

.extract_model_vars <- function(model) {
  # ensure @syntax exists
  if (is.null(model@syntax))
    stop("@syntax not found on model object")
  
  sx <- model@syntax
  
  # Case 1: modern objects — list/`blimp_syntax` with a `model` element
  if ((inherits(sx, "blimp_syntax") || is.list(sx)) && "model" %in% names(sx)) {
    model_text <- as.character(sx$model)
    model_text <- paste(model_text, collapse = " ")
  } else {
    # Case 2: legacy text — try to find lines beginning with 'MODEL:'
    sx_chr <- as.character(sx)
    lines  <- unlist(strsplit(paste(sx_chr, collapse = "\n"), "\n", fixed = TRUE))
    if (!length(lines))
      stop("Could not coerce @syntax to character.")
    model_lines <- grep("^\\s*MODEL\\s*:", lines, value = TRUE, ignore.case = TRUE)
    if (!length(model_lines))
      stop("No MODEL section found in @syntax")
    # strip the leading 'MODEL:' label and join
    model_text <- gsub("^\\s*MODEL\\s*:\\s*", "", paste(model_lines, collapse = " "),
                       ignore.case = TRUE)
  }
  
  # strip path labels like depress_sum@b1, intercept@b0, male@b2
  model_text <- gsub("@[A-Za-z0-9_.]+", "", model_text)
  
  # Tokenize variable names appearing anywhere in the MODEL section
  clean <- gsub("[^A-Za-z0-9_.]+", " ", model_text)
  toks  <- unlist(strsplit(clean, "\\s+"))
  toks  <- toks[nzchar(toks)]
  
  # Drop reserved tokens (operators, intercept keyword, etc.)
  reserved <- c("~", "|", "intercept")
  toks  <- setdiff(toks, reserved)
  model_vars <- unique(toks)
  
  # Also include categorical vars from ORDINAL/NOMINAL (handles ranges like dep1:dep7)
  cat_vars <- .get_categorical_vars(model)
  
  unique(c(model_vars, cat_vars))
}

# STANDARDIZE RESIDUALS (MI) ----
# Pools residual mean/variance across imputations and returns stacked z-scores.

standardize_residuals <- function(model, vars = NULL, na.rm = TRUE) {
  if (!is.list(model@imputations) || length(model@imputations) == 0)
    stop("@imputations must be a non-empty list of data frames")
  cols1 <- names(model@imputations[[1]])
  
  resid_cols <- grep("\\.residual$", cols1, value = TRUE)
  
  if (is.null(vars)) {
    bases <- sub("\\.residual$", "", resid_cols)
  } else {
    stopifnot(is.character(vars), length(vars) >= 1)
    bases <- unique(sub("\\.residual$", "", vars))
    have <- paste0(bases, ".residual")
    bases <- bases[have %in% resid_cols]
    if (!length(bases))
      stop("None of the requested variables have a corresponding '*.residual' column in imputations.")
  }
  
  get_resid <- function(imp_df, base) {
    nm <- paste0(base, ".residual")
    if (!nm %in% names(imp_df)) return(NULL)
    imp_df[[nm]]
  }
  
  out_rows <- list()
  stats    <- data.frame(
    base        = bases,
    pooled_mean = NA_real_,
    pooled_sd   = NA_real_,
    m           = NA_integer_,
    n_total     = NA_integer_,
    stringsAsFactors = FALSE
  )
  
  row_counter <- 1L
  for (b in bases) {
    r_list <- lapply(model@imputations, get_resid, base = b)
    keep   <- vapply(r_list, function(x) !is.null(x), logical(1L))
    r_list <- r_list[keep]
    m      <- length(r_list)
    if (m == 0L) next
    
    mu_j <- vapply(r_list, function(x) mean(x, na.rm = na.rm),  numeric(1L))
    v_j  <- vapply(r_list, function(x) stats::var(x,  na.rm = na.rm), numeric(1L))
    
    pooled_mean <- mean(mu_j, na.rm = TRUE)
    pooled_var  <- mean(v_j,  na.rm = TRUE)
    pooled_sd   <- sqrt(pooled_var)
    
    for (j in seq_len(m)) {
      r <- r_list[[j]]
      n <- length(r)
      z <- if (is.finite(pooled_sd) && pooled_sd > 0) (r - pooled_mean) / pooled_sd else rep(NA_real_, n)
      
      out_rows[[row_counter]] <- data.frame(
        base   = b,
        imp    = j,
        row    = seq_len(n),
        resid  = r,
        z      = z,
        stringsAsFactors = FALSE
      )
      row_counter <- row_counter + 1L
    }
    
    stats[stats$base == b, c("pooled_mean","pooled_sd","m","n_total")] <-
      list(pooled_mean, pooled_sd, m, sum(vapply(r_list, length, integer(1L))))
    if (!is.finite(pooled_sd) || pooled_sd <= 0)
      warning("Pooled SD is non-positive for '", b, "'. z-scores set to NA.")
  }
  
  data_stacked <- if (length(out_rows)) do.call(rbind, out_rows) else
    data.frame(base=character(), imp=integer(), row=integer(),
               resid=numeric(), z=numeric(), stringsAsFactors = FALSE)
  
  structure(list(data = data_stacked, stats = stats), class = "blimp_stdres")
}

# MI STACKING HELPER ----
# Stack a y/x pair across imputations (adds row index for joining).
# Used by residuals_plot() and bivariate_plot().
.stack_cols <- function(model, ycol, xcol) {
  dfs <- lapply(seq_along(model@imputations), function(i) {
    d <- model@imputations[[i]]
    if (!all(c(ycol, xcol) %in% names(d))) return(NULL)
    data.frame(
      x   = d[[xcol]],
      y   = d[[ycol]],
      imp = i,
      row = seq_len(nrow(d)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, dfs)
}

# DISCRETE / NUMERIC DETECTION ----
# Unified rule:
#   - If declared categorical → discrete
#   - Else if non-numeric → discrete
#   - Else if <= 8 unique numeric values → discrete
#   - Otherwise numeric
.is_discrete <- function(model, xvec, xname) {
  # 1. declared categorical in MODEL
  cats <- tolower(.get_categorical_vars(model))
  if (tolower(xname) %in% cats) return(TRUE)
  
  # 2. non-numeric always discrete
  if (!is.numeric(xvec)) return(TRUE)
  
  # 3. numeric with <= 8 unique values
  ux <- unique(xvec)
  ux <- ux[!is.na(ux)]
  length(ux) <= 8
}


# IMPUTATION HISTOGRAM(S) ----
# If 'bins' is NULL, choose bin count adaptively based on unique values.
# If non-NULL, the user-specified number of bins is used verbatim.

distribution_plot <- function(model, var = NULL, bins = NULL, main = NULL,
                              fill_color = "blue", font_size = 14) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!is.list(model@imputations) || !length(model@imputations)) stop("@imputations must be non-empty")
  if (!fill_color %in% names(plot_colors)) stop("fill_color must be one of: ", paste(names(plot_colors), collapse = ", "))
  
  vars_all <- names(model@imputations[[1]])
  
  # helper: get DV names from MODEL block (handles focal:, etc.)
  # Used to find cluster-level and random-slope terms tied to those DVs.
  get_dv_names <- function(model) {
    mb <- .get_model_block(model)
    if (!length(mb)) return(character(0))
    chunks <- unlist(strsplit(paste(mb, collapse = " "), ";", fixed = TRUE), use.names = FALSE)
    chunks <- gsub("\\s+", " ", trimws(chunks))
    chunks <- chunks[nzchar(chunks)]
    
    # message("---- residuals_plot(): MODEL chunks ----")
    # for (ch in chunks) message("  chunk: ", ch)
    
    dvs <- character(0)
    for (ch in chunks) {
      if (!grepl("~", ch, fixed = TRUE)) next
      parts <- strsplit(ch, "~", fixed = TRUE)[[1]]
      dv_raw <- trimws(parts[1])
      dv_tokens <- strsplit(dv_raw, "\\s+", perl = TRUE)[[1]]
      dv_token_last <- dv_tokens[length(dv_tokens)]
      dv <- sub(":$", "", dv_token_last)
      dvs <- c(dvs, dv)
    }
    unique(dvs)
  }
  
  if (is.null(var)) {
    model_vars <- unique(.extract_model_vars(model))
    
    # base name = everything before first dot, as before
    base_all  <- sub("\\..*$", "", vars_all)
    var <- vars_all[base_all %in% model_vars]
    
    # exclude *.predicted columns
    var <- var[!grepl("\\.predicted$", var)]
    
    # add cluster-level and random-slope terms tied to the DVs
    dv_names <- get_dv_names(model)
    if (length(dv_names)) {
      # things like y[cluster]  (random intercepts / cluster-level residuals)
      cluster_cols <- unlist(lapply(dv_names, function(y) {
        vars_all[grepl(paste0("^", y, "\\["), vars_all)]
      }), use.names = FALSE)
      
      # things like y$x[cluster] (random slopes: posaff$pain[person])
      slope_cols <- unlist(lapply(dv_names, function(y) {
        vars_all[grepl(paste0("^", y, "\\$"), vars_all)]
      }), use.names = FALSE)
      
      extra <- c(cluster_cols, slope_cols)
      if (length(extra)) {
        var <- unique(c(var, extra))
      }
    }
    
    if (!length(var)) stop("No matching variables found based on MODEL section.")
    message("Plotting variables based on MODEL section: ", paste(var, collapse = ", "))
    
  } else if (!all(var %in% vars_all)) {
    bad <- setdiff(var, vars_all)
    stop("Variable(s) not found in imputations: ", paste(bad, collapse = ", "))
  }
  
  fill_col <- unname(plot_colors[fill_color])
  n_sets   <- length(model@imputations)
  
  # inner plot builder (bins_arg comes from outer 'bins')
  build_one <- function(v, bins_arg) {
    imp_vals <- unlist(lapply(model@imputations, `[[`, v), use.names = FALSE)
    if (!is.numeric(imp_vals)) {
      message("Skipping non-numeric variable: ", v)
      return(NULL)
    }
    
    x <- imp_vals[is.finite(imp_vals)]
    n <- length(x)
    if (n == 0L) {
      message("No finite values for variable: ", v)
      return(NULL)
    }
    
    n_unique <- length(unique(x))
    
    main_text <- if (is.null(main))
      paste0("Distribution Over ", n_sets, " Imputed Data Sets: ", v)
    else main
    
    ## Case 1: low-support numeric -> treat as discrete (bar chart of counts)
    if (n_unique <= 10L) {
      df <- data.frame(x = factor(x, levels = sort(unique(x))))
      
      ggplot2::ggplot(df, ggplot2::aes(x = x)) +
        ggplot2::geom_bar(
          fill  = fill_col,
          color = fill_col,
          alpha = plot_shading
        ) +
        ggplot2::labs(
          title = main_text,
          x     = v,
          y     = "Count"
        ) +
        blimp_theme(font_size)
      
    } else {
      ## Case 2: more continuous -> choose a conservative bin count
      
      if (!is.null(bins_arg)) {
        # user explicitly supplied bins -> respect it
        bins_use <- as.integer(bins_arg)
      } else {
        # adaptive: grows sub-linearly with unique values, clamped
        bins_use <- as.integer(round(sqrt(n_unique) * 1.5))
        bins_use <- max(15L, min(40L, bins_use))
      }
      
      ggplot2::ggplot(data.frame(x = x),
                      ggplot2::aes(x = x, y = ggplot2::after_stat(density))) +
        ggplot2::geom_histogram(
          bins  = bins_use,
          fill  = fill_col,
          color = fill_col,
          alpha = plot_shading
        ) +
        ggplot2::labs(
          title = main_text,
          x     = v,
          y     = NULL
        ) +
        blimp_theme(font_size)
    }
  }
  
  plots <- setNames(lapply(var, build_one, bins_arg = bins), var)
  for (p in plots) if (!is.null(p)) print(p)
  invisible(plots)
}

# OBSERVED VS. IMPUTED ----
# If 'bins' is NULL, choose bin count adaptively based on unique values.
# If non-NULL, the user-specified number of bins is used verbatim.

imputed_vs_observed_plot <- function(model, var = NULL, bins = NULL, main = NULL,
                                     observed_fill = "blue", imputed_line = "red",
                                     font_size = 14) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!is.list(model@imputations) || !length(model@imputations))
    stop("@imputations must be non-empty")
  if (is.null(model@variance_imp))
    stop("@variance_imp not found on model object")
  if (!observed_fill %in% names(plot_colors) || !imputed_line %in% names(plot_colors))
    stop("observed_fill and imputed_line must be names in plot_colors: ",
         paste(names(plot_colors), collapse = ", "))
  
  # ---- figure out which variables are eligible ----
  vars_imp1   <- names(model@imputations[[1]])
  vars_varimp <- names(model@variance_imp)
  vars_all    <- intersect(vars_varimp, vars_imp1)
  
  # drop derived columns; we only want the "base" variables
  derived_pattern <- "\\.(latent|residual|predicted|probability)$"
  vars_base <- vars_all[!grepl(derived_pattern, vars_all)]
  
  # special rule: do NOT make observed-vs-imputed plots for yjt(...) variables
  is_yjt <- grepl("^yjt\\(", vars_base)
  if (any(is_yjt)) {
    message("Skipping yjt(...) variables for observed vs. imputed plots: ",
            paste(vars_base[is_yjt], collapse = ", "))
  }
  vars_base_noyjt <- vars_base[!is_yjt]
  
  if (is.null(var)) {
    # use robust MODEL parser
    model_vars <- .extract_model_vars(model)
    # exact name matching to avoid age/cigage problems
    var <- intersect(vars_base_noyjt, model_vars)
    if (!length(var))
      stop("No matching variables found that are both in the MODEL section and in @variance_imp/@imputations.")
    message("Plotting variables with observed values from MODEL section: ",
            paste(var, collapse = ", "))
  } else {
    # user-specified vars: check they exist in base set
    missing_vars <- setdiff(var, vars_base)
    if (length(missing_vars)) {
      stop("Variable(s) not found as base variables in @variance_imp/@imputations: ",
           paste(missing_vars, collapse = ", "))
    }
  }
  
  obs_col <- unname(plot_colors[observed_fill])
  imp_col <- unname(plot_colors[imputed_line])
  n_imps  <- length(model@imputations)
  
  # inner builder: bins_arg is the outer 'bins'
  build_one <- function(v, bins_arg) {
    # extra guard: never plot yjt(...) even if explicitly requested
    if (grepl("^yjt\\(", v)) {
      message("Skipping yjt-derived variable in observed vs. imputed plot: ", v)
      return(NULL)
    }
    
    vimp <- model@variance_imp[[v]]
    if (is.null(vimp)) {
      message("Skipping variable with no variance_imp information: ", v)
      return(NULL)
    }
    
    # BLIMP convention: nonzero finite entries indicate missing/imputed values
    miss_idx <- is.finite(vimp) & (vimp != 0)
    
    obs_vals <- model@imputations[[1]][[v]][!miss_idx]
    imp_vals <- unlist(lapply(model@imputations, function(d) d[[v]][miss_idx]), use.names = FALSE)
    
    if (!length(obs_vals) || all(is.na(obs_vals))) {
      message("Skipping variable with no observed data: ", v)
      return(NULL)
    }
    if (!length(imp_vals) || all(is.na(imp_vals))) {
      message("Skipping variable with no imputed data: ", v)
      return(NULL)
    }
    if (!is.numeric(obs_vals) || !is.numeric(imp_vals)) {
      message("Skipping non-numeric variable: ", v)
      return(NULL)
    }
    
    obs_vals <- obs_vals[is.finite(obs_vals)]
    imp_vals <- imp_vals[is.finite(imp_vals)]
    if (!length(obs_vals) || !length(imp_vals)) {
      message("Skipping variable with no finite observed/imputed data: ", v)
      return(NULL)
    }
    
    # Decide discrete vs. continuous based on support of the observed data.
    # We don't want high-resolution bins when the original scale had few values.
    n_unique_obs <- length(unique(obs_vals))
    
    title_text <- if (is.null(main))
      paste0("Observed vs. Imputed Scores Over ", n_imps, " Imputed Data Sets: ", v)
    else main
    
    ## Case 1: low-support numeric -> treat as discrete (bar chart of counts)
    if (n_unique_obs <= 10L) {
      levs <- sort(unique(c(obs_vals, imp_vals)))
      df_disc <- rbind(
        data.frame(x = factor(obs_vals, levels = levs), grp = "Observed"),
        data.frame(x = factor(imp_vals, levels = levs), grp = "Imputed")
      )
      
      ggplot2::ggplot(df_disc, ggplot2::aes(x = x, fill = grp)) +
        ggplot2::geom_bar(position = "dodge", alpha = plot_shading) +
        ggplot2::scale_fill_manual(
          values = c(Observed = obs_col, Imputed = imp_col),
          drop   = TRUE
        ) +
        ggplot2::labs(
          title = title_text,
          x     = v,
          y     = "Count",
          fill  = NULL
        ) +
        blimp_theme(font_size)
      
    } else {
      ## Case 2: more continuous -> overlay density histograms with adaptive bins
      x_all <- c(obs_vals, imp_vals)
      rng   <- range(x_all, na.rm = TRUE)
      span  <- diff(rng)
      if (!is.finite(span) || span <= 0) span <- 1
      
      # If user supplied bins, respect that; otherwise adapt based on support
      if (!is.null(bins_arg)) {
        bins_use <- as.integer(bins_arg)
      } else {
        n_unique_all <- length(unique(x_all))
        bins_use <- as.integer(round(sqrt(n_unique_all) * 1.5))
        bins_use <- max(15L, min(40L, bins_use))
      }
      
      binwidth <- span / bins_use
      boundary <- rng[1]
      
      df_obs <- data.frame(x = obs_vals, grp = "Observed")
      df_imp <- data.frame(x = imp_vals, grp = "Imputed")
      
      ggplot2::ggplot() +
        ggplot2::geom_histogram(
          data = df_obs,
          ggplot2::aes(x = x, y = ggplot2::after_stat(density), fill = grp),
          binwidth = binwidth, boundary = boundary,
          color    = obs_col, alpha = plot_shading
        ) +
        ggplot2::geom_histogram(
          data = df_imp,
          ggplot2::aes(x = x, y = ggplot2::after_stat(density), color = grp),
          binwidth = binwidth, boundary = boundary,
          fill     = NA, linewidth = 1.2
        ) +
        ggplot2::coord_cartesian(xlim = rng) +
        ggplot2::labs(
          title = title_text,
          x     = v, y = NULL,
          fill  = NULL, color = NULL
        ) +
        ggplot2::scale_fill_manual(
          values = c(Observed = obs_col),
          drop   = TRUE
        ) +
        ggplot2::scale_color_manual(
          values = c(Imputed = imp_col),
          drop   = TRUE
        ) +
        ggplot2::guides(
          fill  = ggplot2::guide_legend(
            order = 1,
            override.aes = list(color = obs_col, alpha = plot_shading)
          ),
          color = ggplot2::guide_legend(
            order = 2,
            override.aes = list(fill = NA, linewidth = 1.2)
          )
        ) +
        blimp_theme(font_size)
    }
  }
  
  plots <- setNames(lapply(var, build_one, bins_arg = bins), var)
  for (p in plots) if (!is.null(p)) print(p)
  invisible(plots)
}


# RESIDUALS VS. PredICTED & RESIDUALS VS. predictors ----
# Sections:
#   A) Residuals vs Predicted (continuous, pooled over imputations)
#   B) Residuals vs predictors from MODEL (handles ordinal/nominal, level-2, etc.)
#   C) Standardized residual index plots + outlier table
#   D) Histograms of standardized residuals for every variable in the index plots
#   E) Standardized residual spread by cluster (level-1 DVs only)

residuals_plot <- function(
    model,
    var           = NULL,   # vector of DV bases (e.g., c("dpdd","inflam_sum"))
    # smoother & pooling
    smoother      = "loess",
    degree        = 4,
    span          = 0.9,
    robust        = TRUE,
    ci            = TRUE,
    level         = 0.95,
    # where to show the curve/ribbon
    support       = c("density", "quantile"),
    trim_prop     = 0.025,
    window_prop   = 0.15,
    min_n_prop    = 0.005,
    # robustification for FITTING ONLY
    center_by_imp = FALSE,
    winsor_fit    = TRUE,
    winsor_k      = 3,
    # styling
    point_alpha   = 0.15,
    point_size    = 1.2,
    curve_color   = "red",
    band_fill     = "red",
    band_alpha    = plot_shading,   # follow global shading
    font_size     = 14,
    # index options
    show_index    = TRUE,
    signed_index  = TRUE,
    index_cutoff  = if (signed_index) c(-3, -2, 0, 2, 3) else c(2, 3),
    index_aggregate = c("mean", "max"),
    index_order   = c("rank", "row"),
    index_point_color = "blue",
    index_line_color  = "red",
    print_threshold   = 3,
    print_head        = 20
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!is.list(model@imputations) || !length(model@imputations))
    stop("@imputations must be a non-empty list of data frames")
  if (!curve_color %in% names(plot_colors) || !band_fill %in% names(plot_colors))
    stop("curve_color and band_fill must be names in plot_colors: ",
         paste(names(plot_colors), collapse = ", "))
  
  support         <- match.arg(support)
  index_aggregate <- match.arg(index_aggregate)
  index_order     <- match.arg(index_order)
  
  # ==== helpers ===============================================================
  
  # Rubin-pooled category means + CI (for discrete X)
  pool_means_by_category <- function(df_xy, level = 0.95) {
    levs <- levels(df_xy$x)
    out  <- lapply(levs, function(L) {
      dL <- df_xy[df_xy$x == L, , drop = FALSE]
      if (!nrow(dL)) return(NULL)
      imps <- split(dL, dL$imp)
      means <- Uj <- numeric(length(imps))
      for (j in seq_along(imps)) {
        yj <- imps[[j]]$y
        yj <- yj[is.finite(yj)]
        if (!length(yj)) { means[j] <- NA_real_; Uj[j] <- NA_real_; next }
        means[j] <- mean(yj)
        nj <- length(yj); sj2 <- stats::var(yj)
        Uj[j] <- if (is.finite(sj2) && nj > 0) sj2 / nj else NA_real_
      }
      means <- means[is.finite(means)]; Uj <- Uj[is.finite(Uj)]
      m <- length(means); if (!m) return(NULL)
      Qbar <- mean(means); Ubar <- if (length(Uj)) mean(Uj) else 0
      B <- if (m > 1) stats::var(means) else 0
      Tvar <- Ubar + (1 + 1/m) * B
      r <- if (Ubar > 0) ((1 + 1/m) * B) / Ubar else Inf
      nu <- (m - 1) * (1 + 1/r)^2; if (!is.finite(nu) || nu <= 0) nu <- m - 1
      tcrit <- stats::qt(1 - (1 - level)/2, df = max(1, nu))
      seTot <- sqrt(pmax(0, Tvar))
      data.frame(level = L, mean = Qbar,
                 lwr = Qbar - tcrit * seTot, upr = Qbar + tcrit * seTot,
                 stringsAsFactors = FALSE)
    })
    do.call(rbind, out)
  }
  
  # Rubin-pooled LOESS smoother over imputations (for continuous X).
  loess_pooled <- function(df_xy, span, level, center_by_imp, winsor_fit, winsor_k, robust) {
    fitdat <- df_xy
    if (center_by_imp) {
      fitdat$y <- fitdat$y - ave(fitdat$y, fitdat$imp, FUN = function(z) mean(z, na.rm = TRUE))
    }
    if (winsor_fit) {
      fitdat$y <- ave(fitdat$y, fitdat$imp, FUN = function(z) winsor_mad(z, k = winsor_k))
    }
    rng_x <- range(fitdat$x, na.rm = TRUE)
    xgrid <- if (!is.finite(diff(rng_x)) || diff(rng_x) == 0) rng_x else seq(rng_x[1], rng_x[2], length.out = 200)
    imps  <- split(fitdat, fitdat$imp)
    fam   <- if (robust) "symmetric" else "gaussian"
    
    min_span_n   <- 50L
    min_span     <- 0.12
    min_unique_x <- 25L
    jitter_frac  <- 1e-7
    
    collapse_dupes <- function(d) {
      d <- stats::na.omit(d[, c("y","x")]); if (!nrow(d)) return(d)
      agg <- aggregate(y ~ x, data = d, FUN = mean)
      agg[order(agg$x), , drop = FALSE]
    }
    
    preds <- lapply(imps, function(d) {
      d0 <- collapse_dupes(d); n <- nrow(d0)
      if (n < 10) return(list(fit = rep(NA_real_, length(xgrid)), se2 = rep(NA_real_, length(xgrid))))
      u <- length(unique(d0$x))
      span_eff <- max(span, min_span, min_span_n / max(10L, n))
      if (u < min_unique_x) span_eff <- max(span_eff, min(1, min_unique_x / max(5L, u)))
      if (anyDuplicated(d0$x)) {
        xr <- diff(range(d0$x)); j <- if (is.finite(xr) && xr > 0) xr * jitter_frac else jitter_frac
        set.seed(1L); d0$x <- d0$x + stats::rnorm(n, 0, j)
      }
      ctl <- stats::loess.control(surface = "interpolate", trace.hat = "approximate")
      fit <- try(
        suppressWarnings(
          stats::loess(y ~ x, data = d0,
                       span = min(1, span_eff),
                       degree = 1,
                       family = fam, control = ctl)
        ),
        silent = TRUE
      )
      if (inherits(fit, "try-error"))
        return(list(fit = rep(NA_real_, length(xgrid)), se2 = rep(NA_real_, length(xgrid))))
      
      pr <- try(
        suppressWarnings(
          stats::predict(fit, newdata = data.frame(x = xgrid), se = TRUE)
        ),
        silent = TRUE
      )
      if (inherits(pr, "try-error") || is.null(pr$se.fit)) {
        yhat <- try(
          suppressWarnings(stats::predict(fit, newdata = data.frame(x = xgrid))),
          silent = TRUE
        )
        yhat <- if (inherits(yhat, "try-error")) rep(NA_real_, length(xgrid)) else as.numeric(yhat)
        list(fit = yhat, se2 = rep(NA_real_, length(xgrid)))
      } else {
        list(fit = as.numeric(pr$fit), se2 = as.numeric(pr$se.fit)^2)
      }
    })
    
    Fmat <- do.call(cbind, lapply(preds, `[[`, "fit"))
    Wmat <- do.call(cbind, lapply(preds, `[[`, "se2"))
    keep <- which(colSums(is.finite(Fmat)) > 0)
    if (!length(keep)) return(data.frame(x = xgrid, mean = 0, lwr = 0, upr = 0))
    Fmat <- Fmat[, keep, drop = FALSE]
    Wmat <- Wmat[, keep, drop = FALSE]
    m <- ncol(Fmat)
    
    mean_fit <- rowMeans(Fmat, na.rm = TRUE)
    W <- rowMeans(Wmat, na.rm = TRUE)
    B <- apply(Fmat, 1, stats::var, na.rm = TRUE); B[!is.finite(B)] <- 0
    Tvar <- W + (1 + 1/m) * B
    r <- ifelse(W > 0, ((1 + 1/m) * B) / W, Inf)
    nu <- (m - 1) * (1 + 1/r)^2
    nu[!is.finite(nu) | nu <= 0] <- m - 1
    tcrit <- stats::qt(1 - (1 - level)/2, df = pmax(1, nu))
    seTot <- sqrt(pmax(0, Tvar))
    
    data.frame(
      x    = xgrid,
      mean = mean_fit,
      lwr  = mean_fit - tcrit * seTot,
      upr  = mean_fit + tcrit * seTot
    )
  }
  
  # ==== detect available bases =================================================
  cols1 <- names(model@imputations[[1]])
  bases_from_cols <- unique(sub("\\.(residual|predicted)$", "",
                                grep("\\.(residual|predicted)$", cols1, value = TRUE)))
  has_resid_col   <- function(b) paste0(b, ".residual")   %in% cols1
  has_pair_cols   <- function(b) all(c(paste0(b, ".residual"), paste0(b, ".predicted")) %in% cols1)
  
  # Cluster ID (e.g., "school"), used for mapping *.mean predictors and Section E.
  cluster_id <- .get_cluster_id(model)
  
  if (!is.null(var)) {
    stopifnot(is.character(var), length(var) >= 1)
    var_req <- unique(var)
  } else {
    var_req <- bases_from_cols
  }
  
  bases_resid <- var_req[vapply(var_req, has_resid_col, logical(1L))]
  bases_pair  <- var_req[vapply(var_req, has_pair_cols, logical(1L))]
  
  if (!is.null(var)) {
    if (length(setdiff(var_req, bases_resid)))
      message("Requested variable(s) missing '.residual' column; will skip where needed: ",
              paste(setdiff(var_req, bases_resid), collapse = ", "))
    if (length(setdiff(var_req, bases_pair)))
      message("Requested variable(s) missing '.predicted' (or residual) pair; skipping Residuals vs. Predicted: ",
              paste(setdiff(var_req, bases_pair), collapse = ", "))
  }
  
  # ==== standardized residuals for plotting (A/B/C/D/E) =======================
  std_data <- data.frame()
  if (length(bases_resid)) {
    std_obj  <- standardize_residuals(model, vars = bases_resid)
    std_data <- std_obj$data  # columns: base, imp, row, resid, z
  }
  
  # Also standardize any [clusterid] residual columns (random intercepts/slopes
  # such as posaff[person], posaff$pain[person]) and append to std_data.
  cluster_cols <- cols1[grepl("\\[", cols1)]  # NOTE: regex, NOT fixed=TRUE
  if (length(cluster_cols)) {
    for (cc in cluster_cols) {
      # if this base is already in std_data from standardize_residuals(), skip
      if (nrow(std_data) && cc %in% unique(std_data$base)) next
      
      dfs <- lapply(seq_along(model@imputations), function(i) {
        d <- model@imputations[[i]]
        if (!cc %in% names(d)) return(NULL)
        data.frame(
          base  = cc,
          imp   = i,
          row   = seq_len(nrow(d)),
          resid = d[[cc]],
          stringsAsFactors = FALSE
        )
      })
      dfc <- do.call(rbind, dfs)
      if (is.null(dfc) || !nrow(dfc)) next
      
      y_all <- dfc$resid
      y_all <- y_all[is.finite(y_all)]
      if (length(y_all) < 2L) next
      
      mu  <- mean(y_all)
      sig <- stats::sd(y_all)
      if (!is.finite(sig) || sig <= 0) next
      
      dfc$z <- (dfc$resid - mu) / sig
      std_data <- rbind(std_data, dfc[, c("base","imp","row","resid","z")])
    }
  }
  
  # ==== categorical PREDICTOR set from @syntax (plus <4-unique fallback) =======
  cat_vars <- tolower(.get_categorical_vars(model))
  is_categorical_name <- function(vname) tolower(vname) %in% cat_vars
  
  # ==== colors, counts, collectors ============================================
  curve_col <- unname(plot_colors[curve_color])
  band_col  <- unname(plot_colors[band_fill])
  n_imps    <- length(model@imputations)
  
  plots     <- list()
  summaries <- NULL
  
  # A) Residuals vs. Predicted ----
  
  if (length(bases_pair)) {
    build_rvp <- function(base) {
      ycol <- paste0(base, ".residual")
      xcol <- paste0(base, ".predicted")
      if (!all(c(ycol, xcol) %in% cols1)) {
        message("Skipping (columns missing): ", base)
        return(NULL)
      }
      df <- .stack_cols(model, ycol, xcol)
      if (is.null(df) || !nrow(df)) {
        message("Skipping (no data across imputations): ", base)
        return(NULL)
      }
      if (!is.numeric(df$x) || !is.numeric(df$y)) {
        message("Skipping non-numeric: ", base)
        return(NULL)
      }
      
      # Replace raw residuals with standardized residuals (z) when available.
      if (nrow(std_data)) {
        sub_z <- std_data[std_data$base == base, c("imp", "row", "z")]
        if (nrow(sub_z)) {
          df <- merge(df, sub_z, by = c("imp", "row"), all.x = TRUE, sort = FALSE)
          df$y <- df$z
        }
      }
      
      if (tolower(smoother) == "loess") {
        curve_df <- loess_pooled(df, span, level, center_by_imp, winsor_fit, winsor_k, robust)
        if (support == "quantile") {
          pr <- stats::quantile(df$x, c(trim_prop, 1 - trim_prop), na.rm = TRUE)
          curve_df <- subset(curve_df, x >= pr[1] & x <= pr[2])
        } else {
          xr <- range(df$x, na.rm = TRUE)
          win <- window_prop * diff(xr)
          min_n <- max(10L, ceiling(min_n_prop * nrow(df)))
          counts <- vapply(curve_df$x, function(x0) sum(abs(df$x - x0) <= win/2), integer(1))
          curve_df <- curve_df[counts >= min_n, , drop = FALSE]
        }
      } else {
        rng_x <- range(df$x, na.rm = TRUE)
        xgrid <- seq(rng_x[1], rng_x[2], length.out = 200)
        imps  <- split(df, df$imp)
        coef_mat <- lapply(imps, function(d) {
          dd <- stats::na.omit(d[, c("y","x")])
          if (nrow(dd) < degree + 1) return(rep(NA_real_, degree + 1))
          fit <- try(
            stats::lm(y ~ stats::poly(x, degree = degree, raw = TRUE), data = dd),
            silent = TRUE
          )
          if (inherits(fit, "try-error")) return(rep(NA_real_, degree + 1))
          stats::coef(fit)
        })
        coef_mat <- do.call(rbind, coef_mat)
        coef_mat <- coef_mat[stats::complete.cases(coef_mat), , drop = FALSE]
        if (!nrow(coef_mat)) {
          curve_df <- data.frame(x = xgrid, mean = 0, lwr = NA, upr = NA)
        } else {
          beta <- colMeans(coef_mat)
          yhat <- beta[1] + sapply(xgrid, function(x) sum(beta[-1] * x^(seq_along(beta[-1]))))
          curve_df <- data.frame(x = xgrid, mean = as.numeric(yhat), lwr = NA, upr = NA)
        }
      }
      
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(
          alpha = point_alpha, size = point_size,
          color = unname(plot_colors["blue"])
        ) +
        ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 1.2) +
        {
          if (ci && "lwr" %in% names(curve_df) && nrow(curve_df))
            ggplot2::geom_ribbon(
              data = curve_df,
              ggplot2::aes(x = x, ymin = lwr, ymax = upr),
              inherit.aes = FALSE,
              fill = band_col,
              alpha = band_alpha
            ) else NULL
        } +
        ggplot2::geom_line(
          data = curve_df,
          ggplot2::aes(x = x, y = mean),
          inherit.aes = FALSE,
          color = curve_col,
          linewidth = 1.2
        ) +
        ggplot2::coord_cartesian(ylim = c(-4, 4)) +
        ggplot2::labs(
          title = paste0("Residuals vs. Predicted Values Over ", n_imps,
                         " Imputed Data Sets: ", base),
          x = paste0(base, ".predicted"),
          y = "Standardized Residual"
        ) +
        blimp_theme(font_size) +
        ggplot2::theme(
          axis.text.y  = ggplot2::element_text(size = font_size),
          axis.ticks.y = ggplot2::element_line(),
          axis.text.x  = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 2)),
          axis.title.x = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 12))
        )
    }
    
    rvp_plots <- setNames(lapply(bases_pair, build_rvp), paste0(bases_pair, "_vs_pred"))
    plots <- c(plots, rvp_plots)
    lapply(rvp_plots, function(p) if (!is.null(p)) print(p))
  }
  
  
  
  
  
  # B) Residuals vs. Predictors (from @MODEL) ----
  
  model_lines <- .get_model_block(model)
  
  if (length(model_lines)) {
    chunks <- unlist(
      strsplit(paste(model_lines, collapse = " "), ";", fixed = TRUE),
      use.names = FALSE
    )
    chunks <- gsub("\\s+", " ", trimws(chunks))
    chunks <- chunks[nzchar(chunks)]
    
    # --- how many CLUSTERID variables were declared? -------------------------
    # 0 → single-level, 1 → two-level, 2 → three-level.
    # NOTE: For three-level models (2 CLUSTERIDs), random-effect
    # residual-vs-predictor plots are intentionally disabled below.
    n_cluster_ids <- .count_cluster_ids(model)
    
    # --- level-2 predictor helper --------------------------------------------
    # For now we rely only on the "constant within cluster" fallback;
    # l2_from_helper is left empty so is_level2_predictor() uses the data-based
    # definition.
    l2_from_helper <- character(0L)
    
    is_level2_predictor <- function(pred_name) {
      # If helper provided a list, use that.
      if (length(l2_from_helper)) {
        return(pred_name %in% l2_from_helper)
      }
      
      # Fallback: constant within cluster in the data.
      if (is.null(cluster_id) || !cluster_id %in% cols1) return(FALSE)
      d <- model@imputations[[1]]
      if (!cluster_id %in% names(d)) return(FALSE)
      
      colname <- map_predictor_to_col(pred_name, cols1, cluster_id)
      if (!colname %in% names(d)) return(FALSE)
      
      x <- d[[colname]]
      if (!is.numeric(x)) return(FALSE)
      g <- d[[cluster_id]]
      if (all(is.na(g))) return(FALSE)
      
      by_cl <- split(x, g)
      if (!length(by_cl)) return(FALSE)
      
      const_within <- vapply(by_cl, function(z) {
        z <- z[is.finite(z)]
        if (!length(z)) TRUE else length(unique(z)) <= 1L
      }, logical(1L))
      
      all(const_within)
    }
    
    # --- helpers for parsing the MODEL line ----------------------------------
    
    # Parse RHS "main part" (before "|") into tokens, stripping labels and "*".
    parse_rhs_vars <- function(rhs_main) {
      rhs2 <- gsub("@[A-Za-z0-9_.]+", "", rhs_main)   # strip x@b1 labels
      rhs2 <- gsub("\\*", " ", rhs2)                  # break x*y into x y
      toks <- strsplit(trimws(rhs2), "\\s+", perl = TRUE)[[1]]
      toks <- toks[nzchar(toks)]
      toks <- setdiff(toks, c("intercept", "+", "-", "/", "*"))
      unique(toks)
    }
    
    # Extract interaction pairs from the main RHS (before "|").
    # Returns list of c(var1, var2) for each "var1*var2" token (spaces allowed).
    find_interactions <- function(rhs_main) {
      rhs2 <- gsub("@[A-Za-z0-9_.]+", "", rhs_main)       # strip labels
      rhs2 <- gsub("\\s*\\*\\s*", "*", rhs2)              # normalize spaces around "*"
      toks <- strsplit(rhs2, "\\s+", perl = TRUE)[[1]]
      toks <- toks[nzchar(toks)]
      inter_tokens <- toks[grepl("\\*", toks, fixed = TRUE)]
      pairs <- lapply(inter_tokens, function(tok) {
        parts <- strsplit(tok, "\\*", fixed = TRUE)[[1]]
        if (length(parts) != 2L) return(NULL)  # ignore 3-way etc
        parts
      })
      Filter(Negate(is.null), pairs)
    }
    
    # Parse the random-slope part after "|" into slope variables.
    parse_random_slopes <- function(rhs_rand) {
      if (!nzchar(rhs_rand)) return(character(0))
      rhs2 <- gsub("@[A-Za-z0-9_.]+", "", rhs_rand)
      toks <- strsplit(trimws(rhs2), "\\s+", perl = TRUE)[[1]]
      toks[nzchar(toks)]
    }
    
    # Given a DV and a slope variable, return the level-2 predictors
    # that appear in an interaction with that slope on the MODEL line(s).
    interaction_l2_for <- function(dv, slope_var) {
      out <- character(0)
      
      # Level-2 predictors among the main predictors for this DV
      preds_dv <- dv_main_preds[[dv]]
      if (is.null(preds_dv) || !length(preds_dv)) return(out)
      l2_preds <- preds_dv[vapply(preds_dv, is_level2_predictor, logical(1L))]
      if (!length(l2_preds)) return(out)
      
      for (ch in chunks) {
        if (!grepl("~", ch, fixed = TRUE)) next
        
        parts <- strsplit(ch, "~", fixed = TRUE)[[1]]
        dv_raw <- trimws(parts[1])
        rhs    <- trimws(parts[2])
        
        # Normalize DV name (strip prefixes like "level2:" etc.)
        dv_tokens     <- strsplit(dv_raw, "\\s+", perl = TRUE)[[1]]
        dv_token_last <- dv_tokens[length(dv_tokens)]
        dv_this       <- sub(":$", "", dv_token_last)
        if (!identical(dv_this, dv)) next
        
        # Main RHS (before "|")
        rhs_split <- strsplit(rhs, "\\|", perl = TRUE)[[1]]
        rhs_main  <- trimws(rhs_split[1])
        
        # Strip any labels like x@b1
        rhs_clean <- gsub("@[A-Za-z0-9_.]+", "", rhs_main)
        
        # For each L2 predictor, check for "slope*L2" or "L2*slope"
        for (other in l2_preds) {
          pat1 <- paste0("\\b", slope_var, "\\s*\\*\\s*", other, "\\b")
          pat2 <- paste0("\\b", other, "\\s*\\*\\s*", slope_var, "\\b")
          if (grepl(pat1, rhs_clean) || grepl(pat2, rhs_clean)) {
            out <- c(out, other)
          }
        }
      }
      
      unique(out)
    }
    
    # dv_main_predictors: MODEL-scale DV -> all RHS main-part predictors
    dv_main_preds <- list()
    
    # slope_to_l2_for_dv: key = "dv::$slopevar" -> level-2 predictors that
    # appear in an interaction with that slope (e.g., pain*stress with
    # stress being level-2).
    slope_to_l2_for_dv <- list()
    
    # ---- Pass 1: parse MODEL syntax into main preds and random-slope interactions ----
    
    for (ch in chunks) {
      if (!grepl("~", ch, fixed = TRUE)) next
      
      parts <- strsplit(ch, "~", fixed = TRUE)[[1]]
      dv_raw <- trimws(parts[1])
      rhs    <- trimws(parts[2])
      
      # Handle prefixes like "level2: ranicept" or "focal: turnover"
      dv_tokens     <- strsplit(dv_raw, "\\s+", perl = TRUE)[[1]]
      dv_token_last <- dv_tokens[length(dv_tokens)]
      dv            <- sub(":$", "", dv_token_last)
      
      # Split RHS into main vs random-slope part (before and after "|")
      rhs_split <- strsplit(rhs, "\\|", perl = TRUE)[[1]]
      rhs_main  <- trimws(rhs_split[1])
      rhs_rand  <- if (length(rhs_split) > 1L) trimws(rhs_split[2]) else ""
      
      preds_main   <- parse_rhs_vars(rhs_main)
      inter_pairs  <- find_interactions(rhs_main)
      rand_slopes  <- parse_random_slopes(rhs_rand)
      
      # Accumulate main-part predictors for this DV (used for level-1 DV
      # and random-intercept plots).
      if (length(preds_main)) {
        dv_main_preds[[dv]] <- unique(c(dv_main_preds[[dv]], preds_main))
      }
      
      # For each random slope variable, record which level-2 predictors appear
      # in an interaction with that slope variable (e.g., pain*stress).
      if (length(rand_slopes) && length(inter_pairs)) {
        for (slope_var in rand_slopes) {
          # candidates: "the other variable" in any pair that includes slope_var
          cand <- character(0)
          for (pair in inter_pairs) {
            a <- pair[1]; b <- pair[2]
            if (identical(a, slope_var) && !identical(b, slope_var)) {
              cand <- c(cand, b)
            } else if (identical(b, slope_var) && !identical(a, slope_var)) {
              cand <- c(cand, a)
            }
          }
          cand <- unique(cand)
          if (!length(cand)) next
          
          # keep only level-2 predictors (per helper/fallback)
          cand_l2 <- cand[vapply(cand, is_level2_predictor, logical(1L))]
          if (!length(cand_l2)) next
          
          key <- paste0(dv, "::$", slope_var)
          slope_to_l2_for_dv[[key]] <- unique(c(slope_to_l2_for_dv[[key]], cand_l2))
        }
      }
    }
    
    # ---- Pass 2: build dv_pred_pairs in "base name" space --------------------
    # base names include:
    #   - level-1 DV residuals:            "posaff"
    #   - ordinal/nominal residual pieces: "posaff.1", "posaff.2", ...
    #   - random intercepts:               "posaff[person]"
    #   - random slopes:                   "posaff$pain[person]"
    
    dv_pred_pairs <- list()
    
    for (dv in names(dv_main_preds)) {
      preds <- dv_main_preds[[dv]]
      if (!length(preds)) next
      
      # 1) Level-1 DV residuals: base = dv (when dv.residual exists).
      if (paste0(dv, ".residual") %in% cols1) {
        dv_pred_pairs[[dv]] <- unique(c(dv_pred_pairs[[dv]], preds))
      }
      
      # 2) Ordinal/nominal: bases like dv.1, dv.2, ... that have .residual.
      cand_unit <- bases_resid[startsWith(bases_resid, paste0(dv, "."))]
      if (length(cand_unit)) {
        for (b in cand_unit) {
          dv_pred_pairs[[b]] <- unique(c(dv_pred_pairs[[b]], preds))
        }
      }
      
      # 3) Cluster-level DVs for this dv: random intercept + random slopes.
      #    Columns look like "dv[cluster]" and "dv$slope[cluster]".
      #    If there are TWO CLUSTERID variables declared, we skip these entirely
      #    (this machinery is designed only for a single cluster ID).
      if (n_cluster_ids <= 1L) {
        cluster_cols_for_dv <- cols1[grepl(paste0("^", dv, ".*\\["), cols1)]
        
        if (length(cluster_cols_for_dv)) {
          # random intercept: no "$" in the name
          intercept_cols <- cluster_cols_for_dv[!grepl("\\$", cluster_cols_for_dv, fixed = TRUE)]
          # random slopes: have "$" in the name
          slope_cols     <- setdiff(cluster_cols_for_dv, intercept_cols)
          
          # 3a) Random intercept columns: regress on *all* level-2 predictors
          #     among preds (e.g., pain.mean, stress, female).
          if (length(intercept_cols)) {
            preds_l2 <- preds[vapply(preds, is_level2_predictor, logical(1L))]
            if (length(preds_l2)) {
              for (b in intercept_cols) {
                dv_pred_pairs[[b]] <- unique(c(dv_pred_pairs[[b]], preds_l2))
              }
            }
          }
          
          # 3b) Random slope columns: regress on level-2 predictors that appear
          #     in an interaction with that slope variable (e.g., pain*stress).
          if (length(slope_cols)) {
            for (b in slope_cols) {
              # b looks like "dv$slopevar[cluster]"
              slope_part <- sub(paste0("^", dv, "\\$"), "", b)
              slope_var  <- sub("\\[.*$", "", slope_part)
              key        <- paste0(dv, "::$", slope_var)
              
              preds_slope <- slope_to_l2_for_dv[[key]]
              if (!length(preds_slope)) next    # no qualifying interaction; skip
              
              ## IMPORTANT CHANGE: overwrite any existing mapping for this base
              dv_pred_pairs[[b]] <- unique(preds_slope)
            }
          }
        }
      }
    }
    
    # ---- Post-processing: enforce interaction-only predictors for random slopes ----
    # Any base of the form "dv$slopevar[cluster]" gets *only* L2 predictors
    # that interact with that slopevar in the MODEL syntax.
    if (length(dv_pred_pairs)) {
      for (base in names(dv_pred_pairs)) {
        # Match "dv$slopevar[anything]"
        m  <- regexec("^([^$]+)\\$([^\\[]+)\\[", base)
        mm <- regmatches(base, m)[[1]]
        if (length(mm) == 3L) {
          dv        <- mm[2]
          slope_var <- mm[3]
          allowed   <- interaction_l2_for(dv, slope_var)
          
          if (length(allowed)) {
            dv_pred_pairs[[base]] <- allowed
          } else {
            # No L2 interaction partners: no residual-by-predictor plots
            dv_pred_pairs[[base]] <- character(0)
          }
        }
      }
    }
    
    # ---- Pass 3: actually build the residual-vs-predictor plots -------------
    
    if (length(dv_pred_pairs)) {
      
      build_rvx <- function(base, xname) {
        # Prefer *.residual if it exists; otherwise fall back to the raw column.
        resid_col <- paste0(base, ".residual")
        if (resid_col %in% cols1) {
          ycol <- resid_col
        } else if (base %in% cols1) {
          ycol <- base
        } else {
          message("Skipping (residual missing): neither '", resid_col,
                  "' nor '", base, "' found for DV base ", base)
          return(NULL)
        }
        
        # Map MODEL predictor name -> actual column name.
        xcol <- map_predictor_to_col(xname, cols1, cluster_id)
        if (!xcol %in% cols1) {
          message("Skipping (predictor missing): ", xname,
                  " (no column found) for DV base ", base)
          return(NULL)
        }
        
        df <- .stack_cols(model, ycol, xcol)
        if (is.null(df) || !nrow(df)) {
          message("Skipping (no data across imputations): ", base, " ~ ", xname)
          return(NULL)
        }
        if (!is.numeric(df$y)) {
          message("Skipping non-numeric residuals for: ", base)
          return(NULL)
        }
        
        # STANDARDIZE RESIDUALS FOR PLOTTING
        if (nrow(std_data)) {
          sub_z <- std_data[std_data$base == base, c("imp", "row", "z")]
          if (nrow(sub_z)) {
            df <- merge(df, sub_z, by = c("imp", "row"),
                        all.x = TRUE, sort = FALSE)
            if (any(is.finite(df$z))) {
              df$y <- df$z
            }
          } else {
            all_y <- df$y[is.finite(df$y)]
            if (length(all_y) > 1L) {
              m_y <- mean(all_y)
              s_y <- stats::sd(all_y)
              if (is.finite(s_y) && s_y > 0) {
                df$y <- (df$y - m_y) / s_y
              }
            }
          }
        } else {
          all_y <- df$y[is.finite(df$y)]
          if (length(all_y) > 1L) {
            m_y <- mean(all_y)
            s_y <- stats::sd(all_y)
            if (is.finite(s_y) && s_y > 0) {
              df$y <- (df$y - m_y) / s_y
            }
          }
        }
        
        # ----- DISCRETE vs NUMERIC X: mimic bivariate_plot logic --------------
        x_is_numeric <- is.numeric(df$x)
        if (x_is_numeric) {
          ux <- sort(unique(df$x[is.finite(df$x)]))
          n_unique_x <- length(ux)
        } else {
          ux <- unique(df$x)
          n_unique_x <- length(ux)
        }
        
        # Treat X as discrete if:
        #  - listed as categorical in the model OR
        #  - non-numeric OR
        #  - numeric with < 8 unique values (same as bivariate_plot auto rule)
     
        # Unified discrete/numeric rule (same as bivariate_plot via .is_discrete)
        is_discrete <- .is_discrete(model, df$x, xname)
        
        if (is_discrete) {
          # ---- DISCRETE STYLE ----
          if (x_is_numeric) {
            # numeric discrete X: match bivariate_plot jitter behavior
            if (n_unique_x > 1L) {
              min_step     <- min(diff(ux))
              jitter_width <- 0.01 * min_step
            } else {
              jitter_width <- 0.02
            }
            
            df$x_num <- df$x
            df$x_f   <- factor(df$x_num, levels = ux)
            
            summ <- pool_means_by_category(
              df_xy = data.frame(y = df$y, x = df$x_f, imp = df$imp),
              level = level
            )
            if (!is.null(summ) && nrow(summ)) {
              # map levels back to numeric x positions
              summ$x_num <- as.numeric(as.character(summ$level))
            }
            
            p <- ggplot2::ggplot(df, ggplot2::aes(x = x_num, y = y)) +
              ggplot2::geom_jitter(
                width  = jitter_width,
                height = 0,
                alpha  = point_alpha,
                size   = point_size,
                color  = unname(plot_colors["blue"])
              ) +
              ggplot2::geom_hline(
                yintercept = 0,
                color      = "black",
                linewidth  = 1.2
              )
            
            if (!is.null(summ) && nrow(summ)) {
              if (isTRUE(ci)) {
                p <- p +
                  ggplot2::geom_errorbar(
                    data        = summ,
                    ggplot2::aes(x = x_num, ymin = lwr, ymax = upr),
                    inherit.aes = FALSE,
                    width       = 0.12,
                    color       = curve_col,
                    linewidth   = 0.9
                  )
              }
              
              p <- p +
                ggplot2::geom_line(
                  data        = summ,
                  ggplot2::aes(x = x_num, y = mean),
                  inherit.aes = FALSE,
                  color       = curve_col,
                  linewidth   = 1.2
                ) +
                ggplot2::geom_point(
                  data        = summ,
                  ggplot2::aes(x = x_num, y = mean),
                  inherit.aes = FALSE,
                  size        = 3,
                  color       = "black"
                )
            }
            
            p +
              ggplot2::coord_cartesian(ylim = c(-4, 4)) +
              ggplot2::labs(
                title = paste0("Residuals vs. Predictor Over ", n_imps,
                               " Imputed Data Sets: ", base, " vs. ", xname),
                x = xname,
                y = "Standardized Residual"
              ) +
              blimp_theme(font_size) +
              ggplot2::theme(
                axis.text.y  = ggplot2::element_text(size = font_size),
                axis.ticks.y = ggplot2::element_line(),
                axis.text.x  = ggplot2::element_text(
                  size   = font_size,
                  margin = ggplot2::margin(t = 2)
                ),
                axis.title.x = ggplot2::element_text(
                  size   = font_size,
                  margin = ggplot2::margin(t = 12)
                )
              )
            
          } else {
            # non-numeric discrete X: keep factor behavior
            df$x_f <- factor(df$x)
            summ <- pool_means_by_category(
              df_xy = data.frame(y = df$y, x = df$x_f, imp = df$imp),
              level = level
            )
            if (!is.null(summ) && nrow(summ))
              summ$x_f <- factor(summ$level, levels = levels(df$x_f))
            
            jitter_width <- 0.20
            
            p <- ggplot2::ggplot(df, ggplot2::aes(x = x_f, y = y)) +
              ggplot2::geom_jitter(
                width  = jitter_width,
                height = 0,
                alpha  = point_alpha,
                size   = point_size,
                color  = unname(plot_colors["blue"])
              ) +
              ggplot2::geom_hline(
                yintercept = 0,
                color      = "black",
                linewidth  = 1.2
              )
            
            if (!is.null(summ) && nrow(summ)) {
              if (isTRUE(ci)) {
                p <- p +
                  ggplot2::geom_errorbar(
                    data        = summ,
                    ggplot2::aes(x = x_f, ymin = lwr, ymax = upr),
                    inherit.aes = FALSE,
                    width       = 0.18,
                    color       = curve_col,
                    linewidth   = 0.9
                  )
              }
              
              p <- p +
                ggplot2::geom_line(
                  data        = summ,
                  ggplot2::aes(x = x_f, y = mean, group = 1),
                  inherit.aes = FALSE,
                  color       = curve_col,
                  linewidth   = 1.2
                ) +
                ggplot2::geom_point(
                  data        = summ,
                  ggplot2::aes(x = x_f, y = mean),
                  inherit.aes = FALSE,
                  size        = 3,
                  color       = "black"
                )
            }
            
            p +
              ggplot2::coord_cartesian(ylim = c(-4, 4)) +
              ggplot2::labs(
                title = paste0("Residuals vs. Predictor Over ", n_imps,
                               " Imputed Data Sets: ", base, " vs. ", xname),
                x = xname,
                y = "Standardized Residual"
              ) +
              blimp_theme(font_size) +
              ggplot2::theme(
                axis.text.y  = ggplot2::element_text(size = font_size),
                axis.ticks.y = ggplot2::element_line(),
                axis.text.x  = ggplot2::element_text(
                  size   = font_size,
                  margin = ggplot2::margin(t = 2)
                ),
                axis.title.x = ggplot2::element_text(
                  size   = font_size,
                  margin = ggplot2::margin(t = 12)
                )
              )
          }
          
        } else {
          # ---- NUMERIC STYLE: LOESS / polynomial + ribbon (same as before) ----
          if (!is.numeric(df$x)) {
            message("Skipping non-numeric predictor: ", xname)
            return(NULL)
          }
          
          if (tolower(smoother) == "loess") {
            curve_df <- loess_pooled(df, span, level,
                                     center_by_imp, winsor_fit, winsor_k, robust)
            if (support == "quantile") {
              pr <- stats::quantile(df$x, c(trim_prop, 1 - trim_prop), na.rm = TRUE)
              curve_df <- subset(curve_df, x >= pr[1] & x <= pr[2])
            } else {
              xr <- range(df$x, na.rm = TRUE)
              win <- window_prop * diff(xr)
              min_n <- max(10L, ceiling(min_n_prop * nrow(df)))
              counts <- vapply(
                curve_df$x,
                function(x0) sum(abs(df$x - x0) <= win / 2),
                integer(1)
              )
              curve_df <- curve_df[counts >= min_n, , drop = FALSE]
            }
          } else {
            rng_x <- range(df$x, na.rm = TRUE)
            xgrid <- seq(rng_x[1], rng_x[2], length.out = 200)
            imps  <- split(df, df$imp)
            coef_mat <- lapply(imps, function(d) {
              dd <- stats::na.omit(d[, c("y", "x")])
              if (nrow(dd) < degree + 1) return(rep(NA_real_, degree + 1))
              fit <- try(
                stats::lm(y ~ stats::poly(x, degree = degree, raw = TRUE), data = dd),
                silent = TRUE
              )
              if (inherits(fit, "try-error")) return(rep(NA_real_, degree + 1))
              stats::coef(fit)
            })
            coef_mat <- do.call(rbind, coef_mat)
            coef_mat <- coef_mat[stats::complete.cases(coef_mat), , drop = FALSE]
            if (!nrow(coef_mat)) {
              curve_df <- data.frame(x = xgrid, mean = 0, lwr = NA, upr = NA)
            } else {
              beta <- colMeans(coef_mat)
              yhat <- beta[1] + sapply(
                xgrid,
                function(x) sum(beta[-1] * x^(seq_along(beta[-1])))
              )
              curve_df <- data.frame(
                x    = xgrid,
                mean = as.numeric(yhat),
                lwr  = NA,
                upr  = NA
              )
            }
          }
          
          ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_point(
              alpha = point_alpha, size = point_size,
              color = unname(plot_colors["blue"])
            ) +
            ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 1.2) +
            {
              if (ci && exists("curve_df") &&
                  "lwr" %in% names(curve_df) && nrow(curve_df))
                ggplot2::geom_ribbon(
                  data = curve_df,
                  ggplot2::aes(x = x, ymin = lwr, ymax = upr),
                  inherit.aes = FALSE,
                  fill = band_col,
                  alpha = band_alpha
                ) else NULL
            } +
            {
              if (exists("curve_df"))
                ggplot2::geom_line(
                  data = curve_df,
                  ggplot2::aes(x = x, y = mean),
                  inherit.aes = FALSE,
                  color = curve_col,
                  linewidth  = 1.2
                ) else NULL
            } +
            ggplot2::coord_cartesian(ylim = c(-4, 4)) +
            ggplot2::labs(
              title = paste0("Residuals vs. Predictor Over ", n_imps,
                             " Imputed Data Sets: ", base, " vs. ", xname),
              x = xname,
              y = "Standardized Residual"
            ) +
            blimp_theme(font_size) +
            ggplot2::theme(
              axis.text.y  = ggplot2::element_text(size = font_size),
              axis.ticks.y = ggplot2::element_line(),
              axis.text.x  = ggplot2::element_text(
                size   = font_size,
                margin = ggplot2::margin(t = 2)
              ),
              axis.title.x = ggplot2::element_text(
                size   = font_size,
                margin = ggplot2::margin(t = 12)
              )
            )
        }
      }
      
      rvx_list <- list()
      for (base in names(dv_pred_pairs)) {
        for (xname in dv_pred_pairs[[base]]) {
          p <- build_rvx(base, xname)
          nm <- paste0(base, "_vs_", xname)
          rvx_list[[nm]] <- p
          if (!is.null(p)) print(p)
        }
      }
      plots <- c(plots, rvx_list)
    }
  }
  
  
  # C) Standardized residual index plots + outlier tables ----
  # D) Histograms of standardized residuals ----
  
  if (isTRUE(show_index) && nrow(std_data)) {
    
    # Identify random intercepts / slopes from std_data:
    #  - any base with "[" in the name
    #  - whose underlying DV (before "$" and "[") is in bases_resid
    re_bases_all <- unique(std_data$base[grepl("\\[", std_data$base)])
    
    if (length(re_bases_all)) {
      dv_part <- sub("\\[.*$", "", re_bases_all)
      dv_root <- sub("\\$.*$", "", dv_part)
      keep    <- dv_root %in% bases_resid
      bases_random <- re_bases_all[keep]
    } else {
      bases_random <- character(0L)
    }
    
    # Allowed bases for index/hist plots:
    #   - ordinary residual bases (bases_resid, e.g., "posaff")
    #   - random intercepts/slopes tied to those DVs (bases_random)
    allowed_bases <- unique(c(bases_resid, bases_random))
    
    dat <- std_data[std_data$base %in% allowed_bases, , drop = FALSE]
    
    if (nrow(dat)) {
      n_sets <- length(model@imputations)
      
      # 1. Aggregate per (base, row).
      if (signed_index) {
        agg_df <- stats::aggregate(
          z ~ base + row,
          data = dat,
          FUN = mean, na.rm = TRUE
        )
        names(agg_df)[names(agg_df) == "z"] <- "y"
        ylab <- "Standardized Residual"
      } else {
        dat$absz <- abs(dat$z)
        fun <- if (index_aggregate == "mean") mean else max
        agg_df <- stats::aggregate(
          absz ~ base + row,
          data = dat,
          FUN = fun, na.rm = TRUE
        )
        names(agg_df)[names(agg_df) == "absz"] <- "y"
        ylab <- "Absolute Value of Standardized Residual"
      }
      
      # Drop rows with non-finite y.
      df_idx    <- agg_df[is.finite(agg_df$y), c("base", "row", "y")]
      bases_idx <- unique(df_idx$base)
      
      # 2. Outlier summaries (per base).
      summaries <- lapply(bases_idx, function(b) {
        d <- dat[dat$base == b, , drop = FALSE]
        if (!nrow(d)) return(NULL)
        rows <- sort(unique(d$row))
        tab <- data.frame(
          variable    = b,
          row         = rows,
          num_outlier = NA_integer_,
          num_imps    = NA_integer_,
          mean_abs_z  = NA_real_,
          min_abs_z   = NA_real_,
          max_abs_z   = NA_real_,
          stringsAsFactors = FALSE
        )
        for (i in seq_along(rows)) {
          rr <- rows[i]
          zi <- d$z[d$row == rr]
          a  <- abs(zi)
          tab$num_outlier[i] <- sum(a > print_threshold, na.rm = TRUE)
          tab$num_imps[i]    <- sum(is.finite(a))
          tab$mean_abs_z[i]  <- mean(a, na.rm = TRUE)
          tab$min_abs_z[i]   <- suppressWarnings(min(a, na.rm = TRUE))
          tab$max_abs_z[i]   <- suppressWarnings(max(a, na.rm = TRUE))
        }
        tab <- tab[tab$num_outlier > 0, , drop = FALSE]
        if (!nrow(tab)) {
          message("No rows with |z| > ", print_threshold, " for ", b, ".")
          return(NULL)
        }
        tab[, c("mean_abs_z", "min_abs_z", "max_abs_z")] <-
          round(tab[, c("mean_abs_z", "min_abs_z", "max_abs_z")], 2)
        tab <- tab[order(-tab$num_outlier, -tab$mean_abs_z, tab$row), , drop = FALSE]
        message("Outlier summary for ", b, " (|z| > ", print_threshold,
                "): n rows = ", nrow(tab))
        print(utils::head(tab, print_head), row.names = FALSE)
        tab
      })
      names(summaries) <- bases_idx
      
      # 3. Build index plots.
      build_index <- function(b) {
        d <- df_idx[df_idx$base == b, , drop = FALSE]
        if (!nrow(d)) return(NULL)
        
        if (index_order == "rank") {
          d <- d[order(d$y), ]
          d$index <- seq_len(nrow(d))
          xtitle <- "Cases Ranked by Standardized Residual"
        } else {
          d$index <- d$row
          xtitle <- "Case (row order)"
        }
        
        cutoff_layer <- NULL
        if (length(index_cutoff)) {
          if (signed_index) {
            cutoff_layer <- list(
              ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.8),
              ggplot2::geom_hline(
                yintercept = index_cutoff[index_cutoff != 0],
                color      = unname(plot_colors[index_line_color]),
                linewidth  = 0.7,
                linetype   = "dashed"
              )
            )
          } else {
            cutoff_layer <- list(
              ggplot2::geom_hline(
                yintercept = index_cutoff,
                color      = unname(plot_colors[index_line_color]),
                linewidth  = 0.7,
                linetype   = "dashed"
              )
            )
          }
        }
        
        ggplot2::ggplot(d, ggplot2::aes(x = index, y = y)) +
          ggplot2::geom_point(
            size  = 1.4,
            alpha = 0.85,
            color = unname(plot_colors[index_point_color])
          ) +
          cutoff_layer +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
          ggplot2::labs(
            title = paste0("Mean Standardized Residual Index Plot Over ", n_sets,
                           " Imputed Data Sets: ", b),
            x = xtitle,
            y = ylab
          ) +
          blimp_theme(font_size) +
          ggplot2::theme(
            axis.text.y  = ggplot2::element_text(
              size   = font_size,
              margin = ggplot2::margin(r = 6)
            ),
            axis.ticks.y = ggplot2::element_line(),
            axis.title.y = ggplot2::element_text(size = font_size),
            axis.text.x  = ggplot2::element_text(
              size   = font_size,
              margin = ggplot2::margin(t = 2)
            ),
            axis.title.x = ggplot2::element_text(
              size   = font_size,
              margin = ggplot2::margin(t = 12)
            ),
            panel.border     = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
          )
      }
      
      idx_plots <- setNames(lapply(bases_idx, build_index), paste0(bases_idx, "_index"))
      plots <- c(plots, idx_plots)
      lapply(idx_plots, function(p) if (!is.null(p)) print(p))
      
      # 4. D) Residual histograms (standardized residual z, across imputations).
      build_hist <- function(b) {
        z_vals <- dat$z[dat$base == b]
        z_vals <- z_vals[is.finite(z_vals)]
        if (!length(z_vals)) return(NULL)
        
        ggplot2::ggplot(
          data.frame(z = z_vals),
          ggplot2::aes(x = z, y = ggplot2::after_stat(density))
        ) +
          ggplot2::geom_histogram(
            bins  = 100,  # fixed for residual_plot()
            fill  = unname(plot_colors["blue"]),
            color = unname(plot_colors["blue"]),
            alpha = plot_shading
          ) +
          ggplot2::labs(
            title = paste0(
              "Distribution of Standardized Residuals Over ",
              n_sets, " Imputed Data Sets: ", b
            ),
            x = "Standardized Residual",
            y = NULL
          ) +
          blimp_theme(font_size)
      }
      
      hist_plots <- setNames(lapply(bases_idx, build_hist),
                             paste0(bases_idx, "_hist"))
      plots <- c(plots, hist_plots)
      lapply(hist_plots, function(p) if (!is.null(p)) print(p))
      
    } else {
      message("No standardized residuals available for index plots (after filtering to .residual and random effects tied to those DVs).")
    }
  }
  
  # print(table(std_data$base))
  
  
  
  # E) Standardized residual spread by cluster (level-1 DVs only) ----
  
  if (nrow(std_data) &&
      !is.null(cluster_id) && !is.na(cluster_id) && nzchar(cluster_id) &&
      cluster_id %in% cols1) {
    
    # Use first imputation to classify level-1 vs level-2, but only among
    # bases_resid (and, if var was supplied, only those in var_req).
    d0 <- model@imputations[[1]]
    g  <- d0[[cluster_id]]
    
    resid_cols <- paste0(bases_resid, ".residual")
    resid_cols <- resid_cols[resid_cols %in% cols1]
    
    if (length(resid_cols)) {
      is_level1 <- vapply(resid_cols, function(rc) {
        x <- d0[[rc]]
        if (!is.numeric(x)) return(FALSE)
        by_cl <- split(x, g)
        const_within <- vapply(by_cl, function(z) {
          z <- z[is.finite(z)]
          if (!length(z)) TRUE else length(unique(z)) <= 1L
        }, logical(1L))
        any(!const_within)
      }, logical(1L))
      
      bases_lvl1 <- unique(sub("\\.residual$", "", resid_cols[is_level1]))
      
      if (length(bases_lvl1)) {
        if (length(bases_lvl1) == 1L) {
          message("Level-1 DV for residual-by-cluster spread plot: ", bases_lvl1)
        } else {
          message(
            "Multiple level-1 DVs for residual-by-cluster spread plots: ",
            paste(bases_lvl1, collapse = ", ")
          )
        }
        
        # Helper: build one cluster-spread plot for a given DV.
        build_spread <- function(this_dv) {

          dat_dv <- std_data[
            std_data$base %in% bases_resid & 
              std_data$base == this_dv,
            ,
            drop = FALSE
          ]
          
          if (!nrow(dat_dv)) {
            message("No standardized residuals found for DV '", this_dv, "'.")
            return(NULL)
          }
          
          # Attach cluster labels from first imputation (row index).
          row_cluster <- data.frame(
            row     = seq_len(nrow(d0)),
            cluster = d0[[cluster_id]],
            stringsAsFactors = FALSE
          )
          
          dat2 <- merge(dat_dv, row_cluster,
                        by = "row", all.x = TRUE, sort = FALSE)
          dat2 <- dat2[is.finite(dat2$z) & !is.na(dat2$cluster), , drop = FALSE]
          if (!nrow(dat2)) {
            message("No finite standardized residuals with cluster labels for '", this_dv, "'.")
            return(NULL)
          }
          dat2$cluster <- as.character(dat2$cluster)
          
          # Within-cluster SDs (for ordering) and minimum n per cluster.
          cluster_min_n <- 3
          
          stats_cl <- aggregate(
            z ~ cluster,
            data = dat2,
            FUN = function(z) {
              z <- z[is.finite(z)]
              if (length(z) < cluster_min_n) NA_real_ else stats::sd(z)
            }
          )
          names(stats_cl)[names(stats_cl) == "z"] <- "sd"
          
          n_cl <- aggregate(
            z ~ cluster,
            data = dat2,
            FUN = function(z) sum(is.finite(z))
          )
          names(n_cl)[names(n_cl) == "z"] <- "n"
          
          stats_cl <- merge(stats_cl, n_cl, by = "cluster", all.x = TRUE)
          stats_cl <- stats_cl[is.finite(stats_cl$sd) & stats_cl$n >= cluster_min_n,
                               , drop = FALSE]
          if (!nrow(stats_cl)) {
            message("No clusters had at least ", cluster_min_n,
                    " standardized residuals for DV '", this_dv, "'.")
            return(NULL)
          }
          
          keep_clusters <- stats_cl$cluster
          dat2 <- dat2[dat2$cluster %in% keep_clusters, , drop = FALSE]
          
          # Order clusters by spread (SD) or by label.
          if (index_order == "rank") {
            stats_cl <- stats_cl[order(stats_cl$sd), ]
            xlab <- "Clusters (Ordered by SD)"
          } else {
            stats_cl <- stats_cl[order(stats_cl$cluster), ]
            xlab <- "Cluster"
          }
          cluster_levels <- stats_cl$cluster
          dat2$cluster_ord <- factor(dat2$cluster, levels = cluster_levels)
          
          n_sets <- length(model@imputations)
          
          y_limits <- c(-6.5, 6.5)
          
          p <- ggplot2::ggplot(
            dat2,
            ggplot2::aes(x = cluster_ord, y = z)
          ) +
            ggplot2::geom_point(
              alpha    = 0.20,
              size     = 1.0,
              color    = unname(plot_colors["blue"]),
              position = ggplot2::position_jitter(width = 0.20, height = 0)
            ) +
            ggplot2::geom_hline(
              yintercept = c(-4, -2, 2, 4),
              color      = unname(plot_colors["red"]),
              linewidth  = 0.8,
              linetype   = "dashed"
            ) +
            ggplot2::geom_hline(
              yintercept = 0,
              color      = "black",
              linewidth  = 0.8
            ) +
            ggplot2::scale_y_continuous(
              limits = y_limits,
              breaks = seq(-6, 6, 1)
            ) +
            ggplot2::labs(
              title = paste0(
                "Standardized Residuals by Cluster Over ",
                n_sets, " Imputed Data Sets: ", this_dv
              ),
              x = xlab,
              y = "Standardized Residual"
            ) +
            blimp_theme(font_size) +
            ggplot2::theme(
              axis.text.y  = ggplot2::element_text(
                size   = font_size,
                margin = ggplot2::margin(r = 6)
              ),
              axis.ticks.y = ggplot2::element_line(),
              axis.title.y = ggplot2::element_text(size = font_size),
              axis.text.x  = ggplot2::element_blank(),  # hashes only
              axis.ticks.x = ggplot2::element_line()
            )
          
          print(p)
          p
        }
        
        cluster_plots <- setNames(
          lapply(bases_lvl1, build_spread),
          paste0(bases_lvl1, "_cluster_spread")
        )
        plots <- c(plots, cluster_plots)
      }
    }
  }
  
  # Final return: keep old behavior (return summaries when index plots exist).
  if (!is.null(summaries)) {
    invisible(list(plots = plots, summaries = summaries))
  } else {
    invisible(plots)
  }
}


# BIVARIATE SCATTER WITH LOESS ----
# Adds proper y-axis labeling, tick marks, and bounds for probability outcomes.
# Handles MI pooling of LOESS smooths or discrete-level means.
# When lines = TRUE, parses CLUSTERID from model@syntax via .get_cluster_id()
# and draws spaghetti lines using @average_imp (preferred) or first imputation.
#
# x_type:
#   "auto"     = if < 8 unique x -> discrete style, else numeric/LOESS
#   "discrete" = always use discrete style (means + line, optional error bars)
#   "numeric"  = always use numeric style (LOESS)

bivariate_plot <- function(
    model, formula,
    span = 0.9, degree = 1, robust = TRUE, level = 0.95,
    x_type  = c("auto", "discrete", "numeric"),
    point_alpha = 0.15, point_size = 1.2,
    curve_color = "red", band_fill = "red",
    font_size = 14,
    lines = FALSE,
    errorbars = FALSE   # show error bars for discrete plots?
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  stopifnot(inherits(formula, "formula"))
  x_type  <- match.arg(x_type)
  
  ## --- parse y ~ x ----------------------------------------------------------
  tt       <- terms(formula)
  vars_raw <- attr(tt, "variables")  # ~, lhs, rhs
  if (length(vars_raw) != 3L) stop("Formula must be like y ~ x.")
  
  lhs        <- as.character(vars_raw[[2]])
  rhs_labels <- attr(tt, "term.labels")
  if (length(rhs_labels) != 1L) stop("Formula must be like y ~ x.")
  
  y_name <- lhs
  x_name <- rhs_labels[1]
  
  ## --- checks & stack main MI data ------------------------------------------
  if (!is.list(model@imputations) || !length(model@imputations))
    stop("@imputations must be a non-empty list of data frames")
  cols1 <- names(model@imputations[[1]])
  
  if (!all(c(y_name, x_name) %in% cols1))
    stop("Both '", y_name, "' and '", x_name,
         "' must exist in @imputations data. Available columns are:\n",
         paste(cols1, collapse = ", "))
  
  df <- .stack_cols(model, y_name, x_name)
  df <- stats::na.omit(df[, c("x","y","imp")])
  
  if (!nrow(df)) stop("No complete cases for the selected variables.")
  if (!is.numeric(df$x) || !is.numeric(df$y))
    stop("Both variables must be numeric.")
  n_imps <- length(model@imputations)
  
  ## --- spaghetti data from @average_imp (preferred) -------------------------
  df_lines <- NULL
  if (lines) {
    # use canonical cluster-id helper
    cluster_id <- .get_cluster_id(model)
    
    if (is.null(cluster_id) || is.na(cluster_id) || !nzchar(cluster_id)) {
      warning(
        "lines = TRUE requested, but no CLUSTERID declaration found in model@syntax; ",
        "no spaghetti lines will be drawn."
      )
    } else {
      if (!is.null(model@average_imp)) {
        base_df <- model@average_imp
      } else {
        base_df <- model@imputations[[1]]
      }
      
      cols_base <- names(base_df)
      needed    <- c(x_name, y_name, cluster_id)
      if (!all(needed %in% cols_base)) {
        warning(
          "lines = TRUE requested, but variables ",
          paste(needed, collapse = ", "),
          " were not all found in ",
          if (!is.null(model@average_imp)) "@average_imp" else "the first imputation",
          "; no spaghetti lines will be drawn."
        )
      } else {
        df_lines <- data.frame(
          x       = base_df[[x_name]],
          y       = base_df[[y_name]],
          cluster = base_df[[cluster_id]]
        )
        df_lines <- stats::na.omit(df_lines)
        if (nrow(df_lines)) {
          df_lines <- df_lines[order(df_lines$cluster, df_lines$x), ]
        } else {
          df_lines <- NULL
        }
      }
    }
  }
  
  ## --- probability-like outcome? -------------------------------------------
  qy        <- stats::quantile(df$y, c(0.01, 0.99), na.rm = TRUE)
  use_logit <- is.finite(qy[1]) && is.finite(qy[2]) && qy[1] >= 0 && qy[2] <= 1
  inv_logit <- function(z) 1/(1+exp(-z))
  clamp01   <- function(v, eps = 1e-6) pmin(pmax(v, eps), 1 - eps)
  
  ## --- determine plot type: discrete vs numeric -----------------------------
  if (x_type == "discrete") {
    plot_type <- "discrete"
  } else if (x_type == "numeric") {
    plot_type <- "numeric"
  } else {  # auto
    plot_type <- if (.is_discrete(model, df$x, x_name)) "discrete" else "numeric"
  }
  
  ## --- helpers --------------------------------------------------------------
  
  # Rubin-pooling of LOESS curve (numeric x)
  loess_pooled <- function(dat_xy) {
    fitdat <- dat_xy
    fam    <- if (robust) "symmetric" else "gaussian"
    if (use_logit) {
      fitdat$y <- clamp01(fitdat$y)
      fitdat$y <- qlogis(fitdat$y)
    } else {
      fitdat$y <- ave(fitdat$y, fitdat$imp, FUN = function(z) winsor_mad(z, k = 3))
    }
    rng_x <- range(fitdat$x, na.rm = TRUE)
    xgrid <- seq(rng_x[1], rng_x[2], length.out = 200)
    imps  <- split(fitdat, fitdat$imp)
    
    preds <- lapply(imps, function(d) {
      d0 <- stats::na.omit(d[, c("y","x")])
      if (nrow(d0) < 10)
        return(list(fit = rep(NA_real_, length(xgrid)),
                    se2 = rep(NA_real_, length(xgrid))))
      ctl <- stats::loess.control(surface = "interpolate",
                                  trace.hat = "approximate")
      fit <- try(
        suppressWarnings(
          stats::loess(y ~ x, data = d0,
                       span = span, degree = degree,
                       family = fam, control = ctl)
        ),
        silent = TRUE
      )
      if (inherits(fit, "try-error"))
        return(list(fit = rep(NA_real_, length(xgrid)),
                    se2 = rep(NA_real_, length(xgrid))))
      pr <- try(
        suppressWarnings(
          stats::predict(fit, newdata = data.frame(x = xgrid), se = TRUE)
        ),
        silent = TRUE
      )
      if (inherits(pr, "try-error") || is.null(pr$se.fit))
        return(list(
          fit = as.numeric(stats::predict(
            fit, newdata = data.frame(x = xgrid))),
          se2 = rep(NA_real_, length(xgrid))
        ))
      list(fit = as.numeric(pr$fit),
           se2 = as.numeric(pr$se.fit)^2)
    })
    
    Fmat <- do.call(cbind, lapply(preds, `[[`, "fit"))
    Wmat <- do.call(cbind, lapply(preds, `[[`, "se2"))
    keep <- which(colSums(is.finite(Fmat)) > 0)
    if (!length(keep))
      return(data.frame(x = xgrid, mean = NA, lwr = NA, upr = NA))
    Fmat <- Fmat[, keep, drop = FALSE]
    Wmat <- Wmat[, keep, drop = FALSE]
    m    <- ncol(Fmat)
    
    mean_fit <- rowMeans(Fmat, na.rm = TRUE)
    W        <- rowMeans(Wmat, na.rm = TRUE)
    B        <- apply(Fmat, 1, stats::var, na.rm = TRUE); B[!is.finite(B)] <- 0
    Tvar     <- W + (1 + 1/m) * B
    tcrit    <- stats::qt(1 - (1 - level)/2, df = pmax(1, m - 1))
    seTot    <- sqrt(pmax(0, Tvar))
    
    if (use_logit) {
      mu  <- inv_logit(mean_fit)
      lwr <- inv_logit(mean_fit - tcrit * seTot)
      upr <- inv_logit(mean_fit + tcrit * seTot)
    } else {
      mu  <- mean_fit
      lwr <- mean_fit - tcrit * seTot
      upr <- mean_fit + tcrit * seTot
    }
    data.frame(x = xgrid, mean = mu, lwr = lwr, upr = upr)
  }
  
  # Rubin-pooled mean per x-level (discrete x)
  pooled_mean_by_level <- function(dat_xy) {
    if (use_logit) {
      dat_xy$y <- clamp01(dat_xy$y)
      dat_xy$y <- qlogis(dat_xy$y)
    }
    levs <- sort(unique(dat_xy$x))
    m    <- length(unique(dat_xy$imp))
    
    out  <- lapply(levs, function(L) {
      dL <- dat_xy[dat_xy$x == L, , drop = FALSE]
      if (!nrow(dL)) {
        return(c(mean = NA_real_, lwr = NA_real_, upr = NA_real_))
      }
      by_imp <- split(dL$y, dL$imp)
      mu_j <- vapply(by_imp, function(z) mean(z, na.rm = TRUE), numeric(1L))
      n_j  <- vapply(by_imp, function(z) sum(is.finite(z)), integer(1L))
      s2_j <- vapply(by_imp, function(z) stats::var(z, na.rm = TRUE), numeric(1L))
      Ubar <- mean(s2_j / pmax(1, n_j))
      B    <- stats::var(mu_j)
      Tvar <- Ubar + (1 + 1/m) * B
      se   <- sqrt(pmax(0, Tvar))
      tcrit <- stats::qt(1 - (1 - level)/2, df = pmax(1, m - 1))
      
      mu_bar <- mean(mu_j)
      
      if (use_logit) {
        c(
          mean = inv_logit(mu_bar),
          lwr  = inv_logit(mu_bar - tcrit * se),
          upr  = inv_logit(mu_bar + tcrit * se)
        )
      } else {
        c(
          mean = mu_bar,
          lwr  = mu_bar - tcrit * se,
          upr  = mu_bar + tcrit * se
        )
      }
    })
    
    out <- do.call(rbind, out)
    data.frame(x = levs, mean = out[, "mean"], lwr = out[, "lwr"], upr = out[, "upr"])
  }
  
  ## --- y-axis limits --------------------------------------------------------
  if (use_logit) {
    ymax <- max(df$y, na.rm = TRUE)
    if (!is.finite(ymax)) ymax <- 1
    upper <- min(1, ymax + 0.02)
    if (upper <= 0) upper <- 1
    y_limits <- c(0, upper)
  } else {
    y_limits <- range(df$y, na.rm = TRUE)
  }
  y_breaks <- scales::pretty_breaks()(y_limits)
  
  ## --- plotting -------------------------------------------------------------
  if (plot_type == "discrete") {
    # Discrete plot: jitter blue points, pooled mean dots (black),
    # purple line connecting means, and optional error bars.
    
    mean_df <- pooled_mean_by_level(df)
    
    # slight jitter relative to spacing
    ux <- sort(unique(df$x[is.finite(df$x)]))
    n_unique_x <- length(ux)
    if (n_unique_x > 1) {
      min_step     <- min(diff(ux))
      jitter_width <- 0.01 * min_step
    } else {
      jitter_width <- 0.02
    }
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))
    
    # spaghetti FIRST (background) even for discrete x
    if (lines && !is.null(df_lines)) {
      p <- p +
        ggplot2::geom_line(
          data        = df_lines,
          ggplot2::aes(x = x, y = y, group = cluster),
          inherit.aes = FALSE,
          linewidth   = 0.3,
          alpha       = 0.15,
          color       = "grey40"
        )
    }
    
    p <- p +
      ggplot2::geom_jitter(
        width  = jitter_width,
        height = 0,
        alpha  = point_alpha,
        size   = point_size,
        color  = unname(plot_colors["blue"])
      ) +
      {
        if (errorbars) {
          ggplot2::geom_errorbar(
            data        = mean_df,
            ggplot2::aes(x = x, ymin = lwr, ymax = upr),
            inherit.aes = FALSE,
            width       = 0.12,
            color       = unname(plot_colors[curve_color]),
            linewidth   = 0.9
          )
        } else {
          NULL
        }
      } +
      ggplot2::geom_line(
        data        = mean_df,
        ggplot2::aes(x = x, y = mean),
        inherit.aes = FALSE,
        color       = unname(plot_colors[curve_color]),
        linewidth   = 1.2
      ) +
      ggplot2::geom_point(
        data        = mean_df,
        ggplot2::aes(x = x, y = mean),
        inherit.aes = FALSE,
        size        = 3,
        color       = "black"
      ) +
      ggplot2::scale_y_continuous(breaks = y_breaks, limits = y_limits) +
      ggplot2::labs(
        title = paste0("Bivariate Plot Over ", n_imps, " Imputed Data Sets: ",
                       y_name, " vs. ", x_name),
        x = x_name, y = y_name
      ) +
      blimp_theme(font_size) +
      ggplot2::theme(
        axis.text.y  = ggplot2::element_text(size = font_size,
                                             margin = ggplot2::margin(r = 6)),
        axis.ticks.y = ggplot2::element_line(),
        axis.title.y = ggplot2::element_text(size = font_size),
        axis.text.x  = ggplot2::element_text(size = font_size,
                                             margin = ggplot2::margin(t = 2)),
        axis.title.x = ggplot2::element_text(size = font_size,
                                             margin = ggplot2::margin(t = 12))
      )
    
  } else {
    # Numeric plot: points (+ optional spaghetti) + LOESS curve + ribbon.
    
    curve_df <- loess_pooled(df)
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))
    
    # spaghetti FIRST (background)
    if (lines && !is.null(df_lines)) {
      p <- p +
        ggplot2::geom_line(
          data        = df_lines,
          ggplot2::aes(x = x, y = y, group = cluster),
          inherit.aes = FALSE,
          linewidth   = 0.3,
          alpha       = 0.15,
          color       = "grey40"
        )
    }
    
    # points + LOESS
    p <- p +
      ggplot2::geom_point(
        alpha = point_alpha,
        size  = point_size,
        color = unname(plot_colors["blue"])
      ) +
      ggplot2::geom_ribbon(
        data        = curve_df,
        ggplot2::aes(x = x, ymin = lwr, ymax = upr),
        inherit.aes = FALSE,
        fill        = unname(plot_colors[band_fill]),
        alpha       = plot_shading
      ) +
      ggplot2::geom_line(
        data        = curve_df,
        ggplot2::aes(x = x, y = mean),
        inherit.aes = FALSE,
        color       = unname(plot_colors[curve_color]),
        linewidth   = 1.2
      ) +
      ggplot2::scale_y_continuous(breaks = y_breaks, limits = y_limits) +
      ggplot2::labs(
        title = paste0("Bivariate Plot with LOESS Over ", n_imps,
                       " Imputed Data Sets: ", y_name, " vs. ", x_name),
        x = x_name, y = y_name
      ) +
      blimp_theme(font_size) +
      ggplot2::theme(
        axis.text.y  = ggplot2::element_text(size = font_size,
                                             margin = ggplot2::margin(r = 6)),
        axis.ticks.y = ggplot2::element_line(),
        axis.title.y = ggplot2::element_text(size = font_size),
        axis.text.x  = ggplot2::element_text(size = font_size,
                                             margin = ggplot2::margin(t = 2)),
        axis.title.x = ggplot2::element_text(size = font_size,
                                             margin = ggplot2::margin(t = 12))
      )
  }
  
  p
}