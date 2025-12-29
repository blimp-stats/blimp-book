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

# Primary colors for plotting (can be changed here)
plot_point_color <- "blue"    # color for scatter points
plot_curve_color <- "red"     # color for fitted curves/lines  
plot_band_color  <- "red"     # color for confidence bands/ribbons
plot_fill_color  <- "blue"    # color for histograms/bars (univariate_plot)

# Confidence band X-axis coverage (inner percentage of data to show)
# 0.95 = show bands for inner 95% of X data (2.5th to 97.5th percentile)
# 0.90 = show bands for inner 90% of X data (5th to 95th percentile)
# 0.98 = show bands for inner 98% of X data (1st to 99th percentile)
plot_band_coverage <- 0.95

# Jitter width for discrete X plots (as fraction of minimum spacing between levels)
# For discrete plots, points are jittered horizontally to prevent overplotting
plot_jitter_fraction <- 0.01  # 1% of spacing between X levels

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
    axis.line = ggplot2::element_line(colour = "grey60", linewidth = 0.4),
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

# HELPER: Check if variable is level-2 (constant within clusters) ----
.is_level2_variable <- function(imp_df, var_name, cluster_id) {
  # Validate inputs
  if (!var_name %in% names(imp_df)) return(FALSE)
  if (!cluster_id %in% names(imp_df)) return(FALSE)
  
  x <- imp_df[[var_name]]
  g <- imp_df[[cluster_id]]
  
  # Check if cluster ID is all NA
  if (all(is.na(g))) return(FALSE)
  
  # Split by cluster, check if constant within each cluster
  by_cl <- split(x, g)
  if (!length(by_cl)) return(FALSE)
  
  const_within <- vapply(by_cl, function(z) {
    z <- z[is.finite(z)]
    if (!length(z)) TRUE else length(unique(z)) <= 1L
  }, logical(1L))
  
  # If constant in ALL clusters, it's level-2
  all(const_within)
}

# HELPER: Get unique cluster values for level-2 variables ----
# For level-2 variables (constant within cluster), extract one value per cluster
.get_cluster_unique_values <- function(imp_df, var_name, cluster_id) {
  # Validate inputs
  if (!var_name %in% names(imp_df)) return(numeric(0))
  if (!cluster_id %in% names(imp_df)) return(numeric(0))
  
  # Use aggregate to get one value per cluster
  # Since level-2 vars are constant within cluster, any value works (take first)
  unique_vals <- stats::aggregate(
    imp_df[[var_name]],
    by = list(cluster = imp_df[[cluster_id]]),
    FUN = function(v) v[1]  # All same within cluster, take first
  )$x
  
  return(unique_vals)
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

# STANDARDIZE VARIABLE (MI) ----
# Pools mean/variance across imputations and returns pooled statistics.
# Used for standardizing any variable in plotting functions.
#
# Returns list with:
#   pooled_mean - mean pooled across imputations
#   pooled_sd   - sqrt(pooled variance) across imputations
#   m           - number of imputations

.standardize_variable_pooled <- function(model, var_name, na.rm = TRUE) {
  if (!is.list(model@imputations) || length(model@imputations) == 0)
    stop("@imputations must be a non-empty list of data frames")
  
  # Get cluster ID (most granular if multiple levels)
  cluster_id <- .get_cluster_id(model)
  
  # Check if this variable is level-2 (constant within clusters)
  is_level2 <- FALSE
  if (!is.na(cluster_id)) {
    d <- model@imputations[[1]]
    if (cluster_id %in% names(d) && var_name %in% names(d)) {
      is_level2 <- .is_level2_variable(d, var_name, cluster_id)
    }
  }
  
  # Extract variable from each imputation
  var_list <- lapply(model@imputations, function(imp_df) {
    if (!var_name %in% names(imp_df)) return(NULL)
    
    if (is_level2) {
      # Level-2: Get unique value per cluster (correct n!)
      .get_cluster_unique_values(imp_df, var_name, cluster_id)
    } else {
      # Level-1: Use all observations
      imp_df[[var_name]]
    }
  })
  
  # Remove NULLs
  keep <- vapply(var_list, function(x) !is.null(x), logical(1L))
  var_list <- var_list[keep]
  m <- length(var_list)
  
  if (m == 0L) {
    stop("Variable '", var_name, "' not found in any imputation")
  }
  
  # Calculate mean and variance in each imputation
  # Now using correct n for level-2 variables!
  mu_j <- vapply(var_list, function(x) mean(x, na.rm = na.rm), numeric(1L))
  v_j  <- vapply(var_list, function(x) stats::var(x, na.rm = na.rm), numeric(1L))
  
  # Pool statistics (Rubin's rules: pool parameter estimates)
  pooled_mean <- mean(mu_j, na.rm = TRUE)
  pooled_var  <- mean(v_j, na.rm = TRUE)
  pooled_sd   <- sqrt(pooled_var)
  
  if (!is.finite(pooled_sd) || pooled_sd <= 0) {
    warning("Pooled SD is non-positive for '", var_name, "'. Cannot standardize.")
    return(list(pooled_mean = pooled_mean, pooled_sd = NA_real_, m = m))
  }
  
  list(pooled_mean = pooled_mean, pooled_sd = pooled_sd, m = m)
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
  
  # Get cluster ID for level-2 detection
  cluster_id <- .get_cluster_id(model)
  
  get_resid <- function(imp_df, base, is_level2) {
    nm <- paste0(base, ".residual")
    if (!nm %in% names(imp_df)) return(NULL)
    
    if (is_level2 && !is.na(cluster_id)) {
      # Level-2 residuals: Get unique per cluster
      .get_cluster_unique_values(imp_df, nm, cluster_id)
    } else {
      # Level-1 residuals: Use all observations
      imp_df[[nm]]
    }
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
    # Check if this base variable is level-2
    is_level2 <- FALSE
    if (!is.na(cluster_id)) {
      d <- model@imputations[[1]]
      if (b %in% names(d) && cluster_id %in% names(d)) {
        is_level2 <- .is_level2_variable(d, b, cluster_id)
      }
    }
    
    r_list <- lapply(model@imputations, get_resid, base = b, is_level2 = is_level2)
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

# CLUSTER-AWARE STACK COLS ----
# Handles level-2 variables correctly (extracts unique per cluster)
# Errors if trying to mix level-1 and level-2 variables
.stack_cols_cluster_aware <- function(model, ycol, xcol) {
  # Get cluster ID
  cluster_id <- .get_cluster_id(model)
  
  # If no cluster ID, use standard stacking
  if (is.na(cluster_id)) {
    return(.stack_cols(model, ycol, xcol))
  }
  
  # Check variable levels
  d <- model@imputations[[1]]
  if (!cluster_id %in% names(d)) {
    return(.stack_cols(model, ycol, xcol))
  }
  
  y_is_level2 <- FALSE
  x_is_level2 <- FALSE
  
  if (ycol %in% names(d)) {
    y_is_level2 <- .is_level2_variable(d, ycol, cluster_id)
  }
  if (xcol %in% names(d)) {
    x_is_level2 <- .is_level2_variable(d, xcol, cluster_id)
  }
  
  # Case 1: Both level-1 (use all observations)
  if (!y_is_level2 && !x_is_level2) {
    return(.stack_cols(model, ycol, xcol))
  }
  
  # Case 2: Both level-2 (extract unique per cluster)
  if (y_is_level2 && x_is_level2) {
    dfs <- lapply(seq_along(model@imputations), function(i) {
      imp_df <- model@imputations[[i]]
      if (!all(c(ycol, xcol, cluster_id) %in% names(imp_df))) return(NULL)
      
      # Extract unique cluster-level values
      y_unique <- .get_cluster_unique_values(imp_df, ycol, cluster_id)
      x_unique <- .get_cluster_unique_values(imp_df, xcol, cluster_id)
      
      data.frame(
        x = x_unique,
        y = y_unique,
        imp = i,
        row = seq_along(x_unique),  # Now row = cluster index
        stringsAsFactors = FALSE
      )
    })
    return(do.call(rbind, dfs))
  }
  
  # Case 3: Mixed levels (ERROR)
  level_y <- ifelse(y_is_level2, "level-2 (constant within clusters)", "level-1 (varies within clusters)")
  level_x <- ifelse(x_is_level2, "level-2 (constant within clusters)", "level-1 (varies within clusters)")
  
  stop("Cannot plot variables at different levels.\n",
       "  Y variable '", ycol, "' is ", level_y, "\n",
       "  X variable '", xcol, "' is ", level_x, "\n\n",
       "Both variables must be at the same level.\n",
       "Suggestions:\n",
       "  - Plot level-1 variables against other level-1 variables\n",
       "  - Plot level-2 variables against other level-2 variables\n",
       "  - Aggregate level-1 variables to cluster level before plotting\n",
       "  - Use univariate_plot() to examine variables separately")
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


# Helper: Compute pooled statistics using Rubin's rules ----
# Returns mean, sd, skewness, and excess kurtosis pooled across imputations
.compute_pooled_stats <- function(model, var_name) {
  # Extract variable from each imputation
  values_by_imp <- lapply(model@imputations, function(imp) {
    x <- imp[[var_name]]
    x[is.finite(x)]
  })
  
  m <- length(values_by_imp)  # Number of imputations
  
  # Compute statistics for each imputation
  stats_list <- lapply(values_by_imp, function(x) {
    if (length(x) < 2) return(NULL)
    
    n <- length(x)
    mean_val <- mean(x)
    sd_val <- sd(x)
    
    # Skewness: E[(X - μ)³] / σ³
    centered <- x - mean_val
    skew_val <- mean(centered^3) / (sd_val^3)
    
    # Excess kurtosis: E[(X - μ)⁴] / σ⁴ - 3
    kurt_val <- mean(centered^4) / (sd_val^4) - 3
    
    list(
      n = n,
      mean = mean_val,
      sd = sd_val,
      skew = skew_val,
      kurt = kurt_val
    )
  })
  
  # Remove NULL results
  stats_list <- stats_list[!sapply(stats_list, is.null)]
  
  if (length(stats_list) == 0) return(NULL)
  
  # Pool each statistic using Rubin's rules
  pool_stat <- function(stat_name) {
    qs <- sapply(stats_list, `[[`, stat_name)
    
    # Rubin's pooling
    q_bar <- mean(qs)  # Pooled estimate
    
    if (m > 1) {
      # Within-imputation variance (for mean and sd, we don't have SEs, so skip CI)
      # For skew/kurt, direct pooling is appropriate
      b <- var(qs)  # Between-imputation variance
      
      # Total variance (simplified - no within-imputation variance)
      # This is appropriate for descriptive statistics
      total_var <- b + b/m
      
      list(estimate = q_bar, se = sqrt(total_var))
    } else {
      list(estimate = q_bar, se = NA)
    }
  }
  
  list(
    n = stats_list[[1]]$n,  # Sample size (same across imputations)
    mean = pool_stat("mean"),
    sd = pool_stat("sd"),
    skew = pool_stat("skew"),
    kurt = pool_stat("kurt")
  )
}


# UNIVARIATE DISTRIBUTION PLOTS ----
# Shows distribution of variables across imputed datasets.
# Auto-detects categorical variables from ORDINAL/NOMINAL declarations.

univariate_plot <- function(vars = NULL, model = NULL, discrete_vars = NULL, 
                            bins = 30, font_size = 14, stats = FALSE, 
                            fill_color = plot_fill_color, print = FALSE) {
  # Handle common usage: univariate_plot(model) where model is first positional arg
  if (is.null(model) && !is.null(vars)) {
    # Check if 'vars' is actually a model object (S4 with @imputations)
    if (isS4(vars) && .hasSlot(vars, "imputations")) {
      model <- vars
      vars <- NULL
    }
  }
  
  if (is.null(model)) {
    stop("'model' argument is required")
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!is.list(model@imputations) || !length(model@imputations)) stop("@imputations must be non-empty")
  
  # Add normalized bracket copies for easier plotting
  model <- .add_bracket_copies(model)
  
  # Auto-populate discrete_vars from ORDINAL/NOMINAL if not provided
  if (is.null(discrete_vars)) {
    discrete_vars <- .get_categorical_vars(model)
  }
  
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
  
  if (is.null(vars)) {
    model_vars <- unique(.extract_model_vars(model))
    
    # Base name = everything before first dot
    base_all  <- sub("\\..*$", "", vars_all)
    vars <- vars_all[base_all %in% model_vars]
    
    # Exclude *.predicted columns
    vars <- vars[!grepl("\\.predicted$", vars)]
    
    # Add cluster-level and random-slope terms tied to the DVs
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
        vars <- unique(c(vars, extra))
      }
    }
    
    if (!length(vars)) stop("No matching variables found based on MODEL section.")
    message("Plotting variables: ", paste(vars, collapse = ", "))
    
  } else {
    # User-specified: validate they exist
    missing_vars <- setdiff(vars, vars_all)
    if (length(missing_vars)) {
      stop("Variable(s) not found: ", paste(missing_vars, collapse = ", "))
    }
  }
  
  # Get fill color (can be color name or hex code)
  if (fill_color %in% names(plot_colors)) {
    fill_col <- unname(plot_colors[fill_color])
  } else {
    fill_col <- fill_color  # Use as-is (hex code)
  }
  
  n_sets   <- length(model@imputations)
  
  # Get cluster ID for level-2 detection
  cluster_id <- .get_cluster_id(model)
  
  # Inner plot builder
  build_one <- function(v) {
    # Check if this variable is level-2 (constant within clusters)
    is_level2 <- FALSE
    if (!is.na(cluster_id)) {
      d <- model@imputations[[1]]
      if (v %in% names(d) && cluster_id %in% names(d)) {
        is_level2 <- .is_level2_variable(d, v, cluster_id)
      }
    }
    
    # Extract values (unique per cluster for level-2)
    if (is_level2) {
      imp_vals <- unlist(lapply(model@imputations, function(imp_df) {
        .get_cluster_unique_values(imp_df, v, cluster_id)
      }), use.names = FALSE)
    } else {
      imp_vals <- unlist(lapply(model@imputations, `[[`, v), use.names = FALSE)
    }
    
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
    
    # For bin calculation, use ALL values (not deduplicated) to get smoother histograms
    # This gives better visual representation even for level-2 variables
    if (is_level2) {
      x_for_bins <- unlist(lapply(model@imputations, `[[`, v), use.names = FALSE)
      x_for_bins <- x_for_bins[is.finite(x_for_bins)]
    } else {
      x_for_bins <- x
    }
    
    title_text <- paste0("Distribution Over ", n_sets, " Imputed Data Sets: ", v)
    
    # Determine if discrete (from ORDINAL/NOMINAL declarations)
    is_discrete <- v %in% discrete_vars
    
    if (is_discrete) {
      # Discrete: bar chart with categorical X
      levs <- sort(unique(x))
      df <- data.frame(x = factor(x, levels = levs))
      
      ggplot2::ggplot(df, ggplot2::aes(x = x)) +
        ggplot2::geom_bar(
          fill  = fill_col,
          color = fill_col,
          alpha = plot_shading
        ) +
        ggplot2::labs(
          title = title_text,
          x     = v,
          y     = "Count"
        ) +
        blimp_theme(font_size)
      
    } else {
      # Continuous: histogram with density
      # Use optimal bin width if bins not explicitly set
      bins_to_use <- bins  # Local copy to avoid affecting other variables
      if (bins == 30) {  # Default value check
        # Smart binning: adapts to data characteristics
        n_obs <- length(x_for_bins)
        n_unique <- length(unique(x_for_bins))
        
        # For data with limited unique values (common in psychology/social science),
        # use the unique values directly (one bin per value, no gaps)
        if (n_unique <= 50) {
          bins_to_use <- n_unique
          message(sprintf("Variable '%s': n=%d, unique=%d, using unique values as bins=%d", 
                          v, n_obs, n_unique, bins_to_use))
        } else {
          # For continuous-like data (many unique values), use ggplot2's default (30)
          # This is a reasonable balance that works well across most data types
          bins_to_use <- 30
          message(sprintf("Variable '%s': n=%d, unique=%d, using default bins=%d", 
                          v, n_obs, n_unique, bins_to_use))
        }
        
        # Ensure minimum of 10 bins for stability
        bins_to_use <- max(10, bins_to_use)
      } else {
        message(sprintf("Variable '%s': using specified bins=%d", v, bins_to_use))
      }
      
      p <- ggplot2::ggplot(data.frame(x = x),
                           ggplot2::aes(x = x, y = ggplot2::after_stat(density))) +
        ggplot2::geom_histogram(
          bins  = bins_to_use,
          fill  = fill_col,
          color = fill_col,
          alpha = plot_shading
        ) +
        ggplot2::labs(
          title = title_text,
          x     = v,
          y     = "Density"
        ) +
        blimp_theme(font_size)
      
      # Add statistics if requested
      if (stats) {
        pooled_stats <- .compute_pooled_stats(model, v)
        
        if (!is.null(pooled_stats)) {
          # Format statistics text with right alignment
          # Use sprintf with padding to ensure alignment
          stat_text <- sprintf(
            "             Mean = %.2f\n               SD = %.2f\n         Skewness = %.2f\nExcess Kurtosis = %.2f",
            pooled_stats$mean$estimate,
            pooled_stats$sd$estimate,
            pooled_stats$skew$estimate,
            pooled_stats$kurt$estimate
          )
          
          # Add text annotation to plot
          # Position in top-right corner
          p <- p + ggplot2::annotate(
            "text",
            x = Inf,
            y = Inf,
            label = stat_text,
            hjust = 1.05,
            vjust = 1.2,
            size = font_size / 2.8,  # Larger font (was 3.5)
            lineheight = 0.9
          )
        }
      }
      
      p
    }
  }
  
  plots <- setNames(lapply(vars, build_one), vars)
  plots <- plots[!sapply(plots, is.null)]
  
  # Add cluster index plots for level-1 variables
  if (!is.na(cluster_id) && cluster_id %in% names(model@imputations[[1]])) {
    d0 <- model@imputations[[1]]
    
    for (v in vars) {
      # Check if this variable is level-1 (varies within clusters)
      is_level2 <- FALSE
      if (v %in% names(d0)) {
        is_level2 <- .is_level2_variable(d0, v, cluster_id)
      }
      
      # Only create cluster plot for level-1 variables
      if (!is_level2 && v %in% names(d0)) {
        cluster_plot <- tryCatch({
          .build_univariate_cluster_plot(model, v, cluster_id, font_size, fill_col)
        }, error = function(e) {
          message("Could not create cluster plot for '", v, "': ", e$message)
          NULL
        })
        
        if (!is.null(cluster_plot)) {
          cluster_plot_name <- paste0(v, "_cluster")
          plots[[cluster_plot_name]] <- cluster_plot
        }
      }
    }
  }
  
  if (print) {
    for (p in plots) print(p)
    invisible(plots)
  } else {
    plots
  }
}

# HELPER: Build cluster index plot for level-1 variables ----
.build_univariate_cluster_plot <- function(model, var_name, cluster_id, font_size = 14, fill_col = NULL) {
  # Default color if not provided
  if (is.null(fill_col)) {
    fill_col <- unname(plot_colors["blue"])
  }
  
  # Stack data across imputations
  data_list <- lapply(seq_along(model@imputations), function(i) {
    imp_df <- model@imputations[[i]]
    if (!var_name %in% names(imp_df)) return(NULL)
    if (!cluster_id %in% names(imp_df)) return(NULL)
    
    data.frame(
      value = imp_df[[var_name]],
      cluster = as.character(imp_df[[cluster_id]]),
      imp = i,
      stringsAsFactors = FALSE
    )
  })
  
  data_stacked <- do.call(rbind, data_list)
  data_stacked <- data_stacked[is.finite(data_stacked$value) & !is.na(data_stacked$cluster), ]
  
  if (!nrow(data_stacked)) {
    return(NULL)
  }
  
  # Compute within-cluster statistics (per imputation, then pool)
  cluster_min_n <- 3
  
  # Compute within-cluster statistics (on pooled data, matching residuals_plot)
  cluster_min_n <- 3
  
  stats_cl <- aggregate(
    value ~ cluster,
    data = data_stacked,
    FUN = function(x) {
      x <- x[is.finite(x)]
      if (length(x) < cluster_min_n) NA_real_ else stats::sd(x)
    }
  )
  names(stats_cl)[names(stats_cl) == "value"] <- "sd"
  
  n_cl <- aggregate(
    value ~ cluster,
    data = data_stacked,
    FUN = function(x) sum(is.finite(x))
  )
  names(n_cl)[names(n_cl) == "value"] <- "n"
  
  stats_cl <- merge(stats_cl, n_cl, by = "cluster", all.x = TRUE)
  stats_cl <- stats_cl[is.finite(stats_cl$sd) & stats_cl$n >= cluster_min_n, , drop = FALSE]
  
  if (!nrow(stats_cl)) {
    return(NULL)
  }
  
  # Keep only clusters with sufficient data
  keep_clusters <- stats_cl$cluster
  data_stacked <- data_stacked[data_stacked$cluster %in% keep_clusters, , drop = FALSE]
  
  # Order clusters by SD
  stats_cl <- stats_cl[order(stats_cl$sd), ]
  cluster_levels <- stats_cl$cluster
  
  # Convert cluster to character for factor, but preserve the SD-based order
  # Important: Don't let factor() reorder alphabetically!
  data_stacked$cluster_ord <- factor(
    as.character(data_stacked$cluster), 
    levels = as.character(cluster_levels)  # Explicit order
  )
  
  # DEBUG: Show SD range
  message("Cluster SD range for '", var_name, "':")
  message("  Min SD: ", round(min(stats_cl$sd, na.rm = TRUE), 3))
  message("  Max SD: ", round(max(stats_cl$sd, na.rm = TRUE), 3))
  message("  Median SD: ", round(median(stats_cl$sd, na.rm = TRUE), 3))
  message("  Number of clusters: ", nrow(stats_cl))
  message("  First 5 clusters (by SD): ", paste(head(cluster_levels, 5), collapse = ", "))
  message("  Last 5 clusters (by SD): ", paste(tail(cluster_levels, 5), collapse = ", "))
  
  n_sets <- length(model@imputations)
  
  # Create plot
  p <- ggplot2::ggplot(
    data_stacked,
    ggplot2::aes(x = cluster_ord, y = value)
  ) +
    ggplot2::geom_point(
      alpha    = 0.20,
      size     = 1.0,
      color    = fill_col,
      position = ggplot2::position_jitter(width = 0.10, height = 0)  # Further reduced jitter
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      color      = "grey70",
      linewidth  = 0.5,
      linetype   = "dashed"
    ) +
    ggplot2::labs(
      title = paste0(
        "Distribution by Cluster Over ",
        n_sets, " Imputed Data Sets: ", var_name
      ),
      x = "Clusters (Ordered by SD)",
      y = var_name
    ) +
    blimp_theme(font_size) +
    ggplot2::theme(
      axis.text.y  = ggplot2::element_text(
        size   = font_size,
        margin = ggplot2::margin(r = 6)
      ),
      axis.ticks.y = ggplot2::element_line(),
      axis.title.y = ggplot2::element_text(size = font_size),
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line()
    )
  
  p
}

# HELPER: Normalize bracket variable names ----
# Converts bracket notation to dot notation:
#   posaff[person] -> posaff.person
#   posaff$pain[person] -> posaff_on_pain.person
#   pain.mean[person] -> pain.mean.person (preserve ALL dots!)
.normalize_bracket_names <- function(names_vec) {
  sapply(names_vec, function(nm) {
    # Check if it has brackets
    if (!grepl("\\[", nm)) {
      return(nm)  # No brackets, return as-is
    }
    
    # Extract cluster name (inside brackets)
    cluster <- sub("^.+\\[(.+)\\]$", "\\1", nm)
    
    if (grepl("\\$", nm)) {
      # Pattern: var$slope[cluster]
      # Example: posaff$pain[person] -> posaff_on_pain.person
      
      # Extract parts before $
      base <- sub("^(.+)\\$.+\\[.+\\]$", "\\1", nm)
      # Extract part between $ and [
      slope <- sub("^.+\\$(.+)\\[.+\\]$", "\\1", nm)
      
      # Replace $ with _on_ (preserve ALL dots in base and slope!)
      return(paste0(base, "_on_", slope, ".", cluster))
      
    } else {
      # Pattern: var[cluster]
      # Examples: posaff[person] -> posaff.person
      #           pain.mean[person] -> pain.mean.person
      #           probsolvpost.residual[school] -> probsolvpost.residual.school
      
      # Extract base (everything before [)
      base <- sub("\\[.+\\]$", "", nm)
      
      # Simply replace [ with . and remove ]
      # Preserve ALL dots in the original variable name!
      return(paste0(base, ".", cluster))
    }
  }, USE.NAMES = FALSE)
}

# HELPER: Add normalized bracket copies to model imputations ----
.add_bracket_copies <- function(model) {
  # Check if any bracket variables exist
  var_names <- names(model@imputations[[1]])
  bracket_vars <- var_names[grepl("\\[", var_names)]
  
  if (length(bracket_vars) == 0) {
    return(model)  # No bracket variables, return as-is
  }
  
  # Create normalized names
  normalized_names <- .normalize_bracket_names(bracket_vars)
  
  # DEBUG: Show what's being created
  message("Creating bracket normalized copies:")
  for (i in seq_along(bracket_vars)) {
    message("  ", bracket_vars[i], " -> ", normalized_names[i])
  }
  
  # Add copies with normalized names to each imputation
  for (i in seq_along(model@imputations)) {
    for (j in seq_along(bracket_vars)) {
      original_name <- bracket_vars[j]
      new_name <- normalized_names[j]
      
      # Only add if not already present
      if (!new_name %in% names(model@imputations[[i]])) {
        model@imputations[[i]][[new_name]] <- model@imputations[[i]][[original_name]]
      }
    }
  }
  
  # Also add to variance_imp if present
  if (!is.null(model@variance_imp)) {
    for (j in seq_along(bracket_vars)) {
      original_name <- bracket_vars[j]
      new_name <- normalized_names[j]
      
      if (original_name %in% names(model@variance_imp) && !new_name %in% names(model@variance_imp)) {
        model@variance_imp[[new_name]] <- model@variance_imp[[original_name]]
      }
    }
  }
  
  return(model)
}

# IMPUTATION QUALITY VISUALIZATION ----
# Shows distribution of observed vs. imputed values to assess imputation quality.
# Uses unified visual style: observed (blue filled) vs imputed (red outlined).
# Auto-detects categorical variables from ORDINAL/NOMINAL declarations.

imputation_plot <- function(
    vars = NULL,
    model = NULL,
    discrete_vars = NULL,
    bins = 30,
    font_size = 14,
    observed_color = plot_fill_color,
    imputed_color = plot_curve_color,
    print = FALSE
) {
  # Handle common usage: imputation_plot(model) where model is first positional arg
  if (is.null(model) && !is.null(vars)) {
    # Check if 'vars' is actually a model object (S4 with @imputations)
    if (isS4(vars) && .hasSlot(vars, "imputations")) {
      model <- vars
      vars <- NULL
    }
  }
  
  if (is.null(model)) {
    stop("'model' argument is required")
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!is.list(model@imputations) || !length(model@imputations))
    stop("@imputations must be non-empty")
  if (is.null(model@variance_imp))
    stop("@variance_imp not found on model object")
  
  # Add normalized bracket copies for easier plotting
  model <- .add_bracket_copies(model)
  
  # Auto-populate discrete_vars from ORDINAL/NOMINAL if not provided
  if (is.null(discrete_vars)) {
    discrete_vars <- .get_categorical_vars(model)
  }
  
  # ---- Determine which variables to plot ----
  vars_imp1   <- names(model@imputations[[1]])
  vars_varimp <- names(model@variance_imp)
  vars_all    <- intersect(vars_varimp, vars_imp1)
  
  # Exclude derived columns (.latent, .residual, .predicted, .probability)
  derived_pattern <- "\\.(latent|residual|predicted|probability)$"
  vars_base <- vars_all[!grepl(derived_pattern, vars_all)]
  
  if (is.null(vars)) {
    # Auto: use variables from MODEL section
    model_vars <- .extract_model_vars(model)
    vars <- intersect(vars_base, model_vars)
    if (!length(vars))
      stop("No matching variables found in MODEL section with imputation data.")
    message("Plotting variables: ", paste(vars, collapse = ", "))
  } else {
    # User-specified: validate they exist
    missing_vars <- setdiff(vars, vars_base)
    if (length(missing_vars)) {
      stop("Variable(s) not found: ", paste(missing_vars, collapse = ", "))
    }
  }
  
  # Get colors (can be color names or hex codes)
  if (observed_color %in% names(plot_colors)) {
    obs_col <- unname(plot_colors[observed_color])
  } else {
    obs_col <- observed_color  # Use as-is (hex code)
  }
  
  if (imputed_color %in% names(plot_colors)) {
    imp_col <- unname(plot_colors[imputed_color])
  } else {
    imp_col <- imputed_color  # Use as-is (hex code)
  }
  n_imps  <- length(model@imputations)
  
  # Build plot for one variable
  build_one <- function(v) {
    vimp <- model@variance_imp[[v]]
    if (is.null(vimp)) {
      message("Skipping variable with no variance_imp: ", v)
      return(NULL)
    }
    
    # BLIMP convention: nonzero finite entries indicate missing/imputed
    miss_idx <- is.finite(vimp) & (vimp != 0)
    
    obs_vals <- model@imputations[[1]][[v]][!miss_idx]
    imp_vals <- unlist(lapply(model@imputations, function(d) d[[v]][miss_idx]), 
                       use.names = FALSE)
    
    # Validate data
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
      message("Skipping variable with no finite data: ", v)
      return(NULL)
    }
    
    # Determine if discrete
    is_discrete <- v %in% discrete_vars
    
    # Create unified data frame
    df <- data.frame(
      value = c(obs_vals, imp_vals),
      type = factor(c(rep("Observed", length(obs_vals)),
                      rep("Imputed", length(imp_vals))),
                    levels = c("Observed", "Imputed"))
    )
    
    title_text <- paste0("Imputation Quality Over ", n_imps, " Imputations: ", v)
    
    if (is_discrete) {
      # Discrete: bar chart with categorical X
      levs <- sort(unique(df$value))
      df$value <- factor(df$value, levels = levs)
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = value)) +
        # Observed: blue filled bars (proportions)
        ggplot2::geom_bar(
          data = subset(df, type == "Observed"),
          ggplot2::aes(y = ggplot2::after_stat(count / sum(count))),
          fill = obs_col,
          alpha = plot_shading
        ) +
        # Imputed: red outlined bars (proportions)
        ggplot2::stat_count(
          data = subset(df, type == "Imputed"),
          ggplot2::aes(y = ggplot2::after_stat(count / sum(count))),
          geom = "bar",
          fill = NA,
          color = imp_col,
          linewidth = 1.2
        ) +
        ggplot2::labs(
          title = title_text,
          x = v,
          y = "Proportion"
        ) +
        blimp_theme(font_size)
      
    } else {
      # Continuous: histogram with numeric X
      rng <- range(df$value, na.rm = TRUE)
      
      # Check if observed values are integers (using original obs_vals)
      is_integer <- all(abs(obs_vals - round(obs_vals)) < 1e-6, na.rm = TRUE)
      
      if (is_integer) {
        # Integer data: one bin per integer value
        binwidth <- 1
        boundary <- floor(min(c(obs_vals, imp_vals), na.rm = TRUE)) - 0.5
      } else {
        # Continuous data: smart binning based on data characteristics
        bins_to_use <- bins
        if (bins == 30) {  # Default value check
          all_vals <- c(obs_vals, imp_vals)
          n_obs <- length(all_vals)
          n_unique <- length(unique(all_vals))
          
          # For discrete/limited-range data, use unique values
          if (n_unique <= 50) {
            bins_to_use <- n_unique
          } else {
            # For continuous data, use ggplot2 default (30)
            bins_to_use <- 30
          }
          
          bins_to_use <- max(10, bins_to_use)
        }
        binwidth <- diff(rng) / bins_to_use
        boundary <- rng[1]
      }
      
      # Compute max density for proper Y-axis scaling
      # Ensure breaks span the full range of both observed and imputed
      data_min <- min(c(obs_vals, imp_vals), na.rm = TRUE)
      data_max <- max(c(obs_vals, imp_vals), na.rm = TRUE)
      
      # Make sure boundary is at or below data minimum
      if (boundary > data_min) {
        boundary <- floor(data_min / binwidth) * binwidth
      }
      
      # Create breaks that definitely span the full range
      breaks_seq <- seq(boundary, data_max + binwidth, by = binwidth)
      
      obs_hist <- hist(obs_vals, breaks = breaks_seq, plot = FALSE)
      imp_hist <- hist(imp_vals, breaks = breaks_seq, plot = FALSE)
      
      obs_density <- obs_hist$counts / (length(obs_vals) * binwidth)
      imp_density <- imp_hist$counts / (length(imp_vals) * binwidth)
      
      max_density <- max(c(obs_density, imp_density), na.rm = TRUE)
      y_upper <- max_density * 1.1  # Add 10% padding
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = value)) +
        # Observed: blue filled histogram (density)
        ggplot2::geom_histogram(
          data = subset(df, type == "Observed"),
          ggplot2::aes(y = ggplot2::after_stat(density)),
          binwidth = binwidth,
          boundary = boundary,
          fill = obs_col,
          alpha = plot_shading
        ) +
        # Imputed: red outlined histogram (density)
        ggplot2::geom_histogram(
          data = subset(df, type == "Imputed"),
          ggplot2::aes(y = ggplot2::after_stat(density)),
          binwidth = binwidth,
          boundary = boundary,
          fill = NA,
          color = imp_col,
          linewidth = 1.2
        ) +
        ggplot2::coord_cartesian(xlim = rng, ylim = c(0, y_upper)) +
        ggplot2::labs(
          title = title_text,
          x = v,
          y = "Density"
        ) +
        ggplot2::theme_minimal(base_size = font_size) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = font_size),
          axis.title.x = ggplot2::element_text(size = font_size, margin = ggplot2::margin(t = 12)),
          axis.text.y = ggplot2::element_text(size = font_size),
          axis.title.y = ggplot2::element_text(size = font_size),
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = font_size + 2)
        )
    }
    
    # Add simple legend
    # Manual legend using dummy aesthetics
    legend_df <- data.frame(
      type = c("Observed", "Imputed"),
      x = 1,
      y = 1
    )
    
    p <- p +
      ggplot2::geom_point(
        data = legend_df,
        ggplot2::aes(x = x, y = y, fill = type, color = type),
        size = 0,
        alpha = 0
      ) +
      ggplot2::scale_fill_manual(
        name = NULL,
        values = c(Observed = obs_col, Imputed = NA),
        labels = c("Observed", "Imputed"),
        guide = ggplot2::guide_legend(
          override.aes = list(
            fill = c(obs_col, NA),
            color = c(obs_col, imp_col),
            alpha = c(plot_shading, 1),
            linewidth = c(0, 1.2),
            size = 3,
            shape = 15
          )
        )
      ) +
      ggplot2::scale_color_manual(
        values = c(Observed = obs_col, Imputed = imp_col),
        guide = "none"
      ) +
      ggplot2::theme(
        legend.position = "top",
        legend.justification = "left"
      )
    
    p
  }
  
  plots <- setNames(lapply(vars, build_one), vars)
  plots <- plots[!sapply(plots, is.null)]
  
  if (print) {
    for (p in plots) print(p)
    invisible(plots)
  } else {
    plots
  }
}


# RESIDUALS VS. PredICTED & RESIDUALS VS. predictors ----
# Sections:
#   A) Residuals vs Predicted (continuous, pooled over imputations)
#   B) Residuals vs predictors from MODEL (handles ordinal/nominal, level-2, etc.)
#   C) Standardized residual index plots + outlier table
#   D) Histograms of standardized residuals for every variable in the index plots
#   E) Standardized residual spread by cluster (level-1 DVs only)

residuals_plot <- function(
    var           = NULL,   # vector of DV bases (e.g., c("dpdd","inflam_sum"))
    model         = NULL,
    # polynomial fitting & pooling
    poly_degree   = 3,
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
    curve_color   = plot_curve_color,
    band_fill     = plot_band_color,
    band_alpha    = plot_shading,   # follow global shading
    font_size     = 14,
    # index options
    show_index    = TRUE,
    signed_index  = TRUE,
    index_cutoff  = if (signed_index) c(-3, -2, 0, 2, 3) else c(2, 3),
    index_aggregate = c("mean", "max"),
    index_order   = c("rank", "row"),
    index_point_color = plot_point_color,
    index_line_color  = plot_curve_color,
    print_threshold   = 3,
    print_head        = 20
) {
  # Handle common usage: residuals_plot(model) where model is first positional arg
  if (is.null(model) && !is.null(var)) {
    # Check if 'var' is actually a model object (S4 with @imputations)
    if (isS4(var) && .hasSlot(var, "imputations")) {
      model <- var
      var <- NULL
    }
  }
  
  if (is.null(model)) {
    stop("'model' argument is required")
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!is.list(model@imputations) || !length(model@imputations))
    stop("@imputations must be a non-empty list of data frames")
  if (!curve_color %in% names(plot_colors) || !band_fill %in% names(plot_colors))
    stop("curve_color and band_fill must be names in plot_colors: ",
         paste(names(plot_colors), collapse = ", "))
  
  # Validate poly_degree
  if (!is.numeric(poly_degree) || poly_degree < 1) {
    stop("poly_degree must be a positive integer")
  }
  poly_degree <- as.integer(poly_degree)
  
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
  
  # Rubin-pooled polynomial regression over imputations (for continuous X).
  polynomial_pooled <- function(df_xy, degree, level, center_by_imp, winsor_fit, winsor_k) {
    fitdat <- df_xy
    if (center_by_imp) {
      fitdat$y <- fitdat$y - ave(fitdat$y, fitdat$imp, FUN = function(z) mean(z, na.rm = TRUE))
    }
    if (winsor_fit) {
      fitdat$y <- ave(fitdat$y, fitdat$imp, FUN = function(z) winsor_mad(z, k = winsor_k))
    }
    
    # Constrain prediction grid to avoid extreme edge explosion
    # Use 2nd and 98th percentiles with larger buffer
    rng_x <- range(fitdat$x, na.rm = TRUE)
    if (!is.finite(diff(rng_x)) || diff(rng_x) == 0) {
      xgrid <- rng_x
    } else {
      # Calculate percentiles based on global coverage setting
      tail_prob <- (1 - plot_band_coverage) / 2
      x_quantiles <- stats::quantile(fitdat$x, c(tail_prob, 1 - tail_prob), na.rm = TRUE)
      x_min <- max(rng_x[1], x_quantiles[1] - 0.2 * diff(x_quantiles))
      x_max <- min(rng_x[2], x_quantiles[2] + 0.2 * diff(x_quantiles))
      xgrid <- seq(x_min, x_max, length.out = 200)
    }
    
    imps  <- split(fitdat, fitdat$imp)
    m     <- length(imps)
    
    # Fit polynomial in each imputation and get predictions + SEs
    preds <- lapply(imps, function(d) {
      d0 <- stats::na.omit(d[, c("y", "x")])
      n <- nrow(d0)
      
      # Need at least degree+1 points
      if (n <= degree) {
        return(list(
          fit = rep(NA_real_, length(xgrid)),
          se = rep(NA_real_, length(xgrid)),
          df_resid = NA_real_
        ))
      }
      
      # Fit polynomial (orthogonal for numerical stability)
      fit <- try(
        stats::lm(y ~ stats::poly(x, degree = degree, raw = FALSE), data = d0),
        silent = TRUE
      )
      
      if (inherits(fit, "try-error")) {
        return(list(
          fit = rep(NA_real_, length(xgrid)),
          se = rep(NA_real_, length(xgrid)),
          df_resid = NA_real_
        ))
      }
      
      # Get predictions with standard errors
      pred <- try(
        stats::predict(fit, newdata = data.frame(x = xgrid), 
                       se.fit = TRUE, interval = "none"),
        silent = TRUE
      )
      
      if (inherits(pred, "try-error") || is.null(pred$se.fit)) {
        yhat <- try(
          stats::predict(fit, newdata = data.frame(x = xgrid)),
          silent = TRUE
        )
        yhat <- if (inherits(yhat, "try-error")) rep(NA_real_, length(xgrid)) else as.numeric(yhat)
        return(list(
          fit = yhat,
          se = rep(NA_real_, length(xgrid)),
          df_resid = NA_real_
        ))
      }
      
      list(
        fit = as.numeric(pred$fit),
        se = as.numeric(pred$se.fit),
        df_resid = pred$df
      )
    })
    
    # Extract predictions and SEs
    Qmat <- do.call(cbind, lapply(preds, `[[`, "fit"))
    Umat <- do.call(cbind, lapply(preds, `[[`, "se"))^2  # Variance
    df_resids <- sapply(preds, `[[`, "df_resid")
    
    # Remove imputations that failed
    keep <- which(colSums(is.finite(Qmat)) > 0)
    if (!length(keep)) return(data.frame(x = xgrid, mean = 0, lwr = 0, upr = 0))
    
    Qmat <- Qmat[, keep, drop = FALSE]
    Umat <- Umat[, keep, drop = FALSE]
    df_resids <- df_resids[keep]
    m <- ncol(Qmat)
    
    # Rubin's pooling
    Q_bar <- rowMeans(Qmat, na.rm = TRUE)                    # Pooled estimate
    U_bar <- rowMeans(Umat, na.rm = TRUE)                    # Within variance
    B <- apply(Qmat, 1, stats::var, na.rm = TRUE)           # Between variance
    B[!is.finite(B)] <- 0
    
    # Total variance
    T_total <- U_bar + (1 + 1/m) * B
    
    # Degrees of freedom (Barnard-Rubin adjustment)
    # lambda = fraction of missing information
    lambda <- (1 + 1/m) * B / pmax(T_total, 1e-10)
    lambda <- pmin(lambda, 0.99)  # Cap at 0.99 for stability
    
    # Observed degrees of freedom (from residual df)
    df_obs <- mean(df_resids, na.rm = TRUE)
    if (!is.finite(df_obs) || df_obs <= 0) df_obs <- Inf
    
    # Old degrees of freedom
    df_old <- (m - 1) / pmax(lambda^2, 1e-10)
    
    # Adjusted degrees of freedom
    df_adj <- (df_obs * df_old) / (df_obs + df_old)
    df_adj <- pmax(df_adj, 1)  # At least 1 df
    
    # Critical values (can vary by point if df_adj varies)
    tcrit <- stats::qt(1 - (1 - level)/2, df = df_adj)
    
    # Standard error
    se_total <- sqrt(pmax(T_total, 0))
    
    # Confidence intervals
    data.frame(
      x    = xgrid,
      mean = Q_bar,
      lwr  = Q_bar - tcrit * se_total,
      upr  = Q_bar + tcrit * se_total
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
      df <- .stack_cols(model, ycol, xcol)  # Both level-1, keep standard
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
      
      # Fit polynomial with proper Rubin's pooling
      curve_df <- polynomial_pooled(df, poly_degree, level, center_by_imp, winsor_fit, winsor_k)
      
      # Apply support trimming
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
      
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(
          alpha = point_alpha, size = point_size,
          color = unname(plot_colors[plot_point_color])
        ) +
        ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 1.2) +
        {
          if (ci && "lwr" %in% names(curve_df) && nrow(curve_df))
            suppressWarnings(
              ggplot2::geom_ribbon(
                data = curve_df,
                ggplot2::aes(x = x, ymin = lwr, ymax = upr),
                inherit.aes = FALSE,
                fill = band_col,
                alpha = band_alpha
              )
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
    
    # Expand arrow syntax (predictor -> dv1 dv2 ...) into multiple tilde equations
    # This must happen BEFORE we start parsing for "~" in the chunks
    expand_arrow_syntax_fn <- function(chunks) {
      expanded <- lapply(chunks, function(ch) {
        if (!grepl("->", ch, fixed = TRUE)) return(ch)
        
        parts <- strsplit(ch, "->", fixed = TRUE)[[1]]
        if (length(parts) != 2L) {
          warning("Malformed arrow syntax (expected single '->'): ", ch)
          return(ch)
        }
        
        lhs_arrow <- trimws(parts[1])
        rhs_arrow <- trimws(parts[2])
        
        prefix <- ""
        rhs_working <- rhs_arrow
        prefix_match <- regexec("^([A-Za-z0-9_]+):\\s*(.*)$", rhs_arrow, perl = TRUE)
        prefix_res <- regmatches(rhs_arrow, prefix_match)[[1]]
        if (length(prefix_res) == 3L) {
          prefix <- paste0(prefix_res[2], ": ")
          rhs_working <- prefix_res[3]
        }
        
        temp_rhs <- rhs_working
        transform_map <- list()
        transform_counter <- 0L
        
        while (grepl("[A-Za-z_][A-Za-z0-9_]*\\([^)]+\\)", temp_rhs)) {
          match <- regexec("([A-Za-z_][A-Za-z0-9_]*)\\(([^)]+)\\)", temp_rhs)
          match_res <- regmatches(temp_rhs, match)[[1]]
          
          if (length(match_res) == 3L) {
            full_match <- match_res[1]
            transform_counter <- transform_counter + 1L
            placeholder <- paste0("__XFORM", transform_counter, "__")
            transform_map[[placeholder]] <- full_match
            temp_rhs <- sub(full_match, placeholder, temp_rhs, fixed = TRUE)
          } else {
            break
          }
        }
        
        tokens <- strsplit(temp_rhs, "\\s+", perl = TRUE)[[1]]
        tokens <- tokens[nzchar(tokens)]
        
        if (!length(tokens)) {
          warning("No DVs found in arrow syntax RHS: ", ch)
          return(ch)
        }
        
        tokens <- sapply(tokens, function(tok) {
          for (ph in names(transform_map)) {
            tok <- gsub(ph, transform_map[[ph]], tok, fixed = TRUE)
          }
          tok
        }, USE.NAMES = FALSE)
        
        sapply(tokens, function(dv_tok) {
          paste0(prefix, dv_tok, " ~ ", lhs_arrow)
        }, USE.NAMES = FALSE)
      })
      
      unlist(expanded, use.names = FALSE)
    }
    
    chunks <- expand_arrow_syntax_fn(chunks)
    
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
        
        df <- .stack_cols_cluster_aware(model, ycol, xcol)
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
                color  = unname(plot_colors[plot_point_color])
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
                color  = unname(plot_colors[plot_point_color])
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
          # ---- NUMERIC STYLE: polynomial + ribbon ----
          if (!is.numeric(df$x)) {
            message("Skipping non-numeric predictor: ", xname)
            return(NULL)
          }
          
          # Fit polynomial with proper Rubin's pooling
          curve_df <- polynomial_pooled(df, poly_degree, level, center_by_imp, winsor_fit, winsor_k)
          
          # Apply support trimming
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
          
          ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_point(
              alpha = point_alpha, size = point_size,
              color = unname(plot_colors[plot_point_color])
            ) +
            ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 1.2) +
            {
              if (ci && exists("curve_df") &&
                  "lwr" %in% names(curve_df) && nrow(curve_df))
                suppressWarnings(
                  ggplot2::geom_ribbon(
                    data = curve_df,
                    ggplot2::aes(x = x, ymin = lwr, ymax = upr),
                    inherit.aes = FALSE,
                    fill = band_col,
                    alpha = band_alpha
                  )
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
        
        # Smart binning: adapts to data characteristics
        n_obs <- length(z_vals)
        n_unique <- length(unique(z_vals))
        
        # For discrete/limited-range data, use unique values
        if (n_unique <= 50) {
          bins_to_use <- max(10, n_unique)
        } else {
          # For continuous residuals, use ggplot2 default (30)
          bins_to_use <- 30
        }
        
        ggplot2::ggplot(
          data.frame(z = z_vals),
          ggplot2::aes(x = z, y = ggplot2::after_stat(density))
        ) +
          ggplot2::geom_histogram(
            bins  = bins_to_use,
            fill  = unname(plot_colors[plot_point_color]),
            color = unname(plot_colors[plot_point_color]),
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
              color    = unname(plot_colors[plot_point_color]),
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


# BIVARIATE SCATTER WITH POLYNOMIAL REGRESSION ----
# Handles MI pooling of polynomial regression or discrete-level means.
# When lines = TRUE, parses CLUSTERID from model@syntax via .get_cluster_id()
# and draws spaghetti lines using @average_imp (preferred) or first imputation.
#
# TWO MODES:
#   1. Single plot: bivariate_plot(y ~ x, model)
#   2. Multiple plots: bivariate_plot(vars = c("a", "b", "c"), model = model)
#                      bivariate_plot(y_vars = c(...), x_vars = c(...), model = model)
#
# discrete_x:
#   Character vector of variable names that should use discrete plot style
#   (jittered points, means with error bars, connecting line).
#   If NULL (default), automatically populated from ORDINAL/NOMINAL declarations
#   in model@syntax. Set to character(0) to disable auto-detection and force
#   all variables to use numeric/polynomial style. Only affects X variables.
#
# Visual styles:
#   Discrete: Blue jittered points + red line connecting means + red error bars + red dots
#   Numeric:  Blue scatter points + red polynomial curve + red ribbon
#   Both use same color scheme for visual consistency.

bivariate_plot <- function(
    formula = NULL,
    vars = NULL,
    y_vars = NULL,
    x_vars = NULL,
    model,
    discrete_x = NULL,
    standardize = c("none", "y", "x", "both"),
    poly_degree = 3,
    polynomial = TRUE,      # NEW: Turn polynomial fitting on/off
    ci = TRUE,              # NEW: Show/hide confidence intervals (error bars/ribbons)
    rsquare = FALSE,        # NEW: Show R² on plot
    level = 0.95,
    points = TRUE,          # NEW: Show/hide points
    point_alpha = 0.15, point_size = 1.2,
    point_color = plot_point_color,
    curve_color = plot_curve_color, band_fill = plot_band_color,
    font_size = 14,
    lines = FALSE,
    print = FALSE        # Set to TRUE to force printing
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  standardize <- match.arg(standardize)
  
  # Add normalized bracket copies for easier plotting
  model <- .add_bracket_copies(model)
  
  # Auto-populate discrete_x from ORDINAL/NOMINAL if not provided
  if (is.null(discrete_x)) {
    discrete_x <- .get_categorical_vars(model)
  }
  
  # Validate poly_degree
  if (!is.numeric(poly_degree) || poly_degree < 1) {
    stop("poly_degree must be a positive integer")
  }
  poly_degree <- as.integer(poly_degree)
  
  # Convert color parameters (can be color names or hex codes)
  if (point_color %in% names(plot_colors)) {
    point_col <- unname(plot_colors[point_color])
  } else {
    point_col <- point_color  # Use as-is (hex code)
  }
  
  if (curve_color %in% names(plot_colors)) {
    curve_col <- unname(plot_colors[curve_color])
  } else {
    curve_col <- curve_color  # Use as-is (hex code)
  }
  
  if (band_fill %in% names(plot_colors)) {
    band_col <- unname(plot_colors[band_fill])
  } else {
    band_col <- band_fill  # Use as-is (hex code)
  }
  
  ## --- DISPATCH: Single plot mode vs. multiple plots mode ---
  
  # Check for common mistake: passing model as first positional arg when using vars
  if (!is.null(formula) && !inherits(formula, "formula")) {
    stop("First argument should be a formula (y ~ x) or NULL.\n",
         "If using vars/y_vars/x_vars mode, you must use named argument 'model = ':\n",
         "  bivariate_plot(vars = c(...), model = mymodel)")
  }
  
  # Count how many input modes are specified
  has_formula <- !is.null(formula)
  has_vars <- !is.null(vars)
  has_y_x <- !is.null(y_vars) || !is.null(x_vars)
  
  n_modes <- sum(has_formula, has_vars, has_y_x)
  
  if (n_modes == 0) {
    stop("Must specify either:\n",
         "  - formula (e.g., y ~ x) for single plot, OR\n",
         "  - vars (e.g., vars = c('a', 'b', 'c')) for multiple plots, OR\n",
         "  - y_vars and x_vars for multiple plots")
  }
  
  if (n_modes > 1) {
    stop("Cannot mix formula with vars/y_vars/x_vars. Choose one input mode:\n",
         "  - formula for single plot, OR\n",
         "  - vars/y_vars/x_vars for multiple plots")
  }
  
  # MULTIPLE PLOTS MODE: Use bivariate_pairs logic
  if (has_vars || has_y_x) {
    return(.bivariate_pairs_internal(
      vars = vars,
      y_vars = y_vars,
      x_vars = x_vars,
      model = model,
      discrete_x = discrete_x,
      standardize = standardize,
      poly_degree = poly_degree,
      polynomial = polynomial,
      ci = ci,
      rsquare = rsquare,
      level = level,
      points = points,
      point_alpha = point_alpha,
      point_size = point_size,
      point_color = point_color,
      curve_color = curve_color,
      band_fill = band_fill,
      font_size = font_size,
      lines = lines,
      print = print
    ))
  }
  
  # SINGLE PLOT MODE: Continue with original logic
  stopifnot(inherits(formula, "formula"))
  
  ## --- parse y ~ x or y ~ x | group -----------------------------------------
  
  # Check for grouping variable (| operator)
  formula_str <- deparse(formula, width.cutoff = 500L)
  has_grouping <- grepl("\\|", formula_str)
  
  group_var <- NULL
  
  if (has_grouping) {
    # Parse: y ~ x | group
    # Split on ~
    sides <- strsplit(formula_str, "~", fixed = TRUE)[[1]]
    if (length(sides) != 2) {
      stop("Formula must be like y ~ x or y ~ x | group")
    }
    
    y_name <- trimws(sides[1])
    
    # Split RHS on |
    rhs_parts <- strsplit(sides[2], "|", fixed = TRUE)[[1]]
    if (length(rhs_parts) != 2) {
      stop("Group syntax must be: y ~ x | groupvar")
    }
    
    x_name <- trimws(rhs_parts[1])
    group_var <- trimws(rhs_parts[2])
    
  } else {
    # No grouping - use standard parsing
    tt       <- terms(formula)
    vars_raw <- attr(tt, "variables")  # ~, lhs, rhs
    if (length(vars_raw) != 3L) stop("Formula must be like y ~ x.")
    
    # Use deparse to preserve exact syntax for both sides
    y_name <- deparse(vars_raw[[2]])
    x_name <- deparse(vars_raw[[3]])
  }
  
  ## --- checks & stack main MI data ------------------------------------------
  if (!is.list(model@imputations) || !length(model@imputations))
    stop("@imputations must be a non-empty list of data frames")
  cols1 <- names(model@imputations[[1]])
  
  # Validate group variable if present
  if (!is.null(group_var)) {
    # Check if group variable is categorical
    categorical_vars <- tolower(.get_categorical_vars(model))
    
    if (!tolower(group_var) %in% categorical_vars) {
      stop("Group variable '", group_var, "' must be declared as ORDINAL or NOMINAL in model syntax.\n",
           "Available categorical variables: ", 
           paste(.get_categorical_vars(model), collapse = ", "))
    }
    
    # Check if group variable exists in data
    if (!group_var %in% cols1) {
      stop("Group variable '", group_var, "' not found in data.\n",
           "Available columns: ", paste(cols1, collapse = ", "))
    }
  }
  
  # Try to match variable names, handling spacing differences
  # deparse() adds spaces around operators, but column names may not have them
  y_match <- y_name
  x_match <- x_name
  
  if (!y_name %in% cols1) {
    # Try removing spaces around operators
    y_no_spaces <- gsub("\\s+", "", y_name)
    if (y_no_spaces %in% cols1) {
      y_match <- y_no_spaces
    }
  }
  
  if (!x_name %in% cols1) {
    # Try removing spaces around operators
    x_no_spaces <- gsub("\\s+", "", x_name)
    if (x_no_spaces %in% cols1) {
      x_match <- x_no_spaces
    }
  }
  
  if (!all(c(y_match, x_match) %in% cols1))
    stop("Both '", y_name, "' and '", x_name,
         "' must exist in @imputations data. Available columns are:\n",
         paste(cols1, collapse = ", "))
  
  # Use the matched names for data extraction
  y_name <- y_match
  x_name <- x_match
  
  
  df <- .stack_cols_cluster_aware(model, y_name, x_name)
  
  # Add group variable if specified
  if (!is.null(group_var)) {
    group_data <- lapply(model@imputations, function(imp_df) {
      imp_df[[group_var]]
    })
    df$group <- unlist(group_data)
    
    # Convert to factor
    df$group <- as.factor(df$group)
    
    # Check for too many groups
    n_groups <- length(levels(df$group))
    if (n_groups > 8) {
      warning("Group variable '", group_var, "' has ", n_groups, 
              " levels. More than 8 groups may be difficult to distinguish visually.")
    }
  }
  
  df <- stats::na.omit(df[, c("x","y","imp", if(!is.null(group_var)) "group")])
  
  if (!nrow(df)) stop("No complete cases for the selected variables.")
  if (!is.numeric(df$x) || !is.numeric(df$y))
    stop("Both variables must be numeric.")
  n_imps <- length(model@imputations)
  
  ## --- Standardization (if requested) ----------------------------------------
  y_label <- y_name
  x_label <- x_name
  
  if (standardize %in% c("y", "both")) {
    # Standardize Y variable using pooled statistics
    y_stats <- .standardize_variable_pooled(model, y_name, na.rm = TRUE)
    
    if (is.finite(y_stats$pooled_sd) && y_stats$pooled_sd > 0) {
      df$y <- (df$y - y_stats$pooled_mean) / y_stats$pooled_sd
      y_label <- paste0(y_name, " (standardized)")
    } else {
      warning("Cannot standardize Y variable '", y_name, "': pooled SD is non-positive")
    }
  }
  
  if (standardize %in% c("x", "both")) {
    # Standardize X variable using pooled statistics
    x_stats <- .standardize_variable_pooled(model, x_name, na.rm = TRUE)
    
    if (is.finite(x_stats$pooled_sd) && x_stats$pooled_sd > 0) {
      df$x <- (df$x - x_stats$pooled_mean) / x_stats$pooled_sd
      x_label <- paste0(x_name, " (standardized)")
    } else {
      warning("Cannot standardize X variable '", x_name, "': pooled SD is non-positive")
    }
  }
  
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
  
  ## --- determine plot type: discrete vs numeric -----------------------------
  # Check if X variable is in the discrete_x list
  is_discrete_plot <- !is.null(discrete_x) && x_name %in% discrete_x
  
  ## --- helpers --------------------------------------------------------------
  
  # Rubin-pooling of polynomial regression (numeric x)
  # If group_var is not NULL, fits separate polynomials for each group
  polynomial_pooled <- function(dat_xy, by_group = FALSE) {
    if (!by_group || !"group" %in% names(dat_xy)) {
      # Single polynomial fit (original behavior)
      fitdat <- dat_xy
      # Winsorize Y to handle outliers
      fitdat$y <- ave(fitdat$y, fitdat$imp, FUN = function(z) winsor_mad(z, k = 3))
      
      # Constrain prediction grid to avoid extreme edge explosion
      # Use global plot_band_coverage (default 95% = 2.5th to 97.5th percentile)
      rng_x <- range(fitdat$x, na.rm = TRUE)
      tail_prob <- (1 - plot_band_coverage) / 2
      x_quantiles <- stats::quantile(fitdat$x, c(tail_prob, 1 - tail_prob), na.rm = TRUE)
      x_min <- max(rng_x[1], x_quantiles[1] - 0.2 * diff(x_quantiles))
      x_max <- min(rng_x[2], x_quantiles[2] + 0.2 * diff(x_quantiles))
      xgrid <- seq(x_min, x_max, length.out = 200)
      
      imps  <- split(fitdat, fitdat$imp)
      m     <- length(imps)
      
      # Fit polynomial in each imputation
      preds <- lapply(imps, function(d) {
        d0 <- stats::na.omit(d[, c("y", "x")])
        n <- nrow(d0)
        
        # Need at least poly_degree+1 points
        if (n <= poly_degree) {
          return(list(
            fit = rep(NA_real_, length(xgrid)),
            se = rep(NA_real_, length(xgrid)),
            df_resid = NA_real_
          ))
        }
        
        # Fit polynomial
        fit <- try(
          stats::lm(y ~ stats::poly(x, degree = poly_degree, raw = FALSE), data = d0),
          silent = TRUE
        )
        
        if (inherits(fit, "try-error")) {
          return(list(
            fit = rep(NA_real_, length(xgrid)),
            se = rep(NA_real_, length(xgrid)),
            df_resid = NA_real_
          ))
        }
        
        # Get predictions with SEs
        pred <- try(
          stats::predict(fit, newdata = data.frame(x = xgrid), 
                         se.fit = TRUE, interval = "none"),
          silent = TRUE
        )
        
        if (inherits(pred, "try-error") || is.null(pred$se.fit)) {
          yhat <- try(
            stats::predict(fit, newdata = data.frame(x = xgrid)),
            silent = TRUE
          )
          yhat <- if (inherits(yhat, "try-error")) rep(NA_real_, length(xgrid)) else as.numeric(yhat)
          return(list(
            fit = yhat,
            se = rep(NA_real_, length(xgrid)),
            df_resid = NA_real_
          ))
        }
        
        list(
          fit = as.numeric(pred$fit),
          se = as.numeric(pred$se.fit),
          df_resid = pred$df
        )
      })
      
      # Extract predictions and SEs
      Qmat <- do.call(cbind, lapply(preds, `[[`, "fit"))
      Umat <- do.call(cbind, lapply(preds, `[[`, "se"))^2  # Variance
      df_resids <- sapply(preds, `[[`, "df_resid")
      
      # Remove failed imputations
      keep <- which(colSums(is.finite(Qmat)) > 0)
      if (!length(keep)) {
        return(data.frame(x = xgrid, mean = NA, lwr = NA, upr = NA))
      }
      
      Qmat <- Qmat[, keep, drop = FALSE]
      Umat <- Umat[, keep, drop = FALSE]
      df_resids <- df_resids[keep]
      m <- ncol(Qmat)
      
      # Rubin's pooling
      Q_bar <- rowMeans(Qmat, na.rm = TRUE)
      U_bar <- rowMeans(Umat, na.rm = TRUE)
      B <- apply(Qmat, 1, stats::var, na.rm = TRUE)
      B[!is.finite(B)] <- 0
      
      # Total variance
      T_total <- U_bar + (1 + 1/m) * B
      
      # Degrees of freedom (Barnard-Rubin)
      lambda <- (1 + 1/m) * B / pmax(T_total, 1e-10)
      lambda <- pmin(lambda, 0.99)
      
      df_obs <- mean(df_resids, na.rm = TRUE)
      if (!is.finite(df_obs) || df_obs <= 0) df_obs <- Inf
      
      df_old <- (m - 1) / pmax(lambda^2, 1e-10)
      df_adj <- (df_obs * df_old) / (df_obs + df_old)
      df_adj <- pmax(df_adj, 1)
      
      tcrit <- stats::qt(1 - (1 - level)/2, df = df_adj)
      se_total <- sqrt(pmax(T_total, 0))
      
      mean_fit <- Q_bar
      mu  <- mean_fit
      lwr <- mean_fit - tcrit * se_total
      upr <- mean_fit + tcrit * se_total
      
      data.frame(x = xgrid, mean = mu, lwr = lwr, upr = upr)
    } else {
      # Grouped polynomial fits - fit separately for each group
      groups <- levels(dat_xy$group)
      result_list <- lapply(groups, function(grp) {
        dat_grp <- dat_xy[dat_xy$group == grp, ]
        
        # Call polynomial_pooled recursively for this group (without grouping)
        curve_grp <- polynomial_pooled(dat_grp, by_group = FALSE)
        curve_grp$group <- grp
        curve_grp
      })
      
      do.call(rbind, result_list)
    }
  }
  
  # Rubin-pooled mean per x-level (discrete x)
  # If group variable present, computes separately for each group
  pooled_mean_by_level <- function(dat_xy, by_group = FALSE) {
    if (!by_group || !"group" %in% names(dat_xy)) {
      # Single group (original behavior)
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
        
        # Within-imputation variance
        Ubar <- mean(s2_j / pmax(1, n_j))
        
        # Between-imputation variance
        B <- stats::var(mu_j)
        
        # Total variance (Rubin's rules)
        Tvar <- Ubar + (1 + 1/m) * B
        se <- sqrt(pmax(0, Tvar))
        
        # Barnard-Rubin degrees of freedom adjustment
        lambda <- (1 + 1/m) * B / pmax(Tvar, 1e-10)
        lambda <- pmin(lambda, 0.99)
        
        # Observed degrees of freedom (average n - 1 across imputations)
        df_obs <- mean(pmax(1, n_j - 1))
        if (!is.finite(df_obs) || df_obs <= 0) df_obs <- Inf
        
        # Old degrees of freedom
        df_old <- (m - 1) / pmax(lambda^2, 1e-10)
        
        # Adjusted degrees of freedom
        df_adj <- (df_obs * df_old) / (df_obs + df_old)
        df_adj <- pmax(df_adj, 1)
        
        tcrit <- stats::qt(1 - (1 - level)/2, df = df_adj)
        
        mu_bar <- mean(mu_j)
        
        c(
          mean = mu_bar,
          lwr  = mu_bar - tcrit * se,
          upr  = mu_bar + tcrit * se
        )
      })
      
      out <- do.call(rbind, out)
      data.frame(x = levs, mean = out[, "mean"], lwr = out[, "lwr"], upr = out[, "upr"])
    } else {
      # Grouped - compute separately for each group
      groups <- levels(dat_xy$group)
      result_list <- lapply(groups, function(grp) {
        dat_grp <- dat_xy[dat_xy$group == grp, ]
        
        # Call recursively without grouping
        mean_grp <- pooled_mean_by_level(dat_grp, by_group = FALSE)
        mean_grp$group <- grp
        mean_grp
      })
      
      do.call(rbind, result_list)
    }
  }
  
  ## --- y-axis limits --------------------------------------------------------
  y_limits <- range(df$y, na.rm = TRUE)
  y_breaks <- scales::pretty_breaks()(y_limits)
  
  ## --- plotting -------------------------------------------------------------
  if (is_discrete_plot) {
    # Discrete plot: jittered points, line connecting means, error bars
    
    has_groups <- !is.null(group_var) && "group" %in% names(df)
    mean_df <- pooled_mean_by_level(df, by_group = has_groups)
    
    # Calculate jitter width using global parameter
    ux <- sort(unique(df$x[is.finite(df$x)]))
    n_unique_x <- length(ux)
    if (n_unique_x > 1) {
      min_step <- min(diff(ux))
      jitter_width <- plot_jitter_fraction * min_step
    } else {
      jitter_width <- plot_jitter_fraction * 2
    }
    
    if (has_groups) {
      # Grouped discrete plot
      p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, color = group))
      
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
        # Add points (if requested)
        {
          if (points) {
            ggplot2::geom_point(
              alpha  = point_alpha,
              size   = point_size,
              position = ggplot2::position_jitter(
                width = jitter_width,
                height = 0
              )
            )
          } else {
            NULL
          }
        } +
        # Add reference line at y=0 when Y is standardized
        {
          if (standardize %in% c("y", "both")) {
            ggplot2::geom_hline(yintercept = 0, color = "grey70", 
                                linetype = "dashed", linewidth = 0.5)
          } else {
            NULL
          }
        } +
        # Line connecting means (shows trend across levels)
        ggplot2::geom_line(
          data        = mean_df,
          ggplot2::aes(x = x, y = mean, color = group, group = group),
          inherit.aes = FALSE,
          linewidth   = 1.2
        ) +
        # Error bars showing confidence intervals at each discrete level (if requested)
        {
          if (ci) {
            ggplot2::geom_errorbar(
              data        = mean_df,
              ggplot2::aes(x = x, ymin = lwr, ymax = upr, color = group),
              inherit.aes = FALSE,
              width       = 0,
              linewidth   = 0.9
            )
          } else {
            NULL
          }
        } +
        # Points at means
        ggplot2::geom_point(
          data        = mean_df,
          ggplot2::aes(x = x, y = mean, color = group),
          inherit.aes = FALSE,
          size        = 3
        ) +
        ggplot2::scale_x_continuous(breaks = sort(unique(mean_df$x))) +
        ggplot2::scale_y_continuous(breaks = y_breaks, limits = y_limits) +
        ggplot2::labs(
          title = paste0(y_label, " vs. ", x_label, " by ", group_var),
          x     = x_label,
          y     = y_label,
          color = group_var
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
      # Ungrouped discrete plot (original)
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
        # Add points (if requested)
        {
          if (points) {
            ggplot2::geom_jitter(
              width  = jitter_width,
              height = 0,
              alpha  = point_alpha,
              size   = point_size,
              color  = point_col
            )
          } else {
            NULL
          }
        } +
        # Add reference line at y=0 when Y is standardized
        {
          if (standardize %in% c("y", "both")) {
            ggplot2::geom_hline(yintercept = 0, color = "grey70", 
                                linetype = "dashed", linewidth = 0.5)
          } else {
            NULL
          }
        } +
        # Line connecting means (shows trend across levels)
        ggplot2::geom_line(
          data        = mean_df,
          ggplot2::aes(x = x, y = mean),
          inherit.aes = FALSE,
          color       = curve_col,
          linewidth   = 1.2
        ) +
        # Error bars showing confidence intervals at each discrete level (if requested)
        {
          if (ci) {
            ggplot2::geom_errorbar(
              data        = mean_df,
              ggplot2::aes(x = x, ymin = lwr, ymax = upr),
              inherit.aes = FALSE,
              width       = 0,  # No caps, just vertical lines
              color       = curve_col,
              linewidth   = 0.9
            )
          } else {
            NULL
          }
        } +
        # Points at means
        ggplot2::geom_point(
          data        = mean_df,
          ggplot2::aes(x = x, y = mean),
          inherit.aes = FALSE,
          size        = 3,
          color       = curve_col
        ) +
        ggplot2::scale_x_continuous(breaks = sort(unique(mean_df$x))) +
        ggplot2::scale_y_continuous(breaks = y_breaks, limits = y_limits) +
        ggplot2::labs(
          title = paste0("Bivariate Plot Over ", n_imps, " Imputed Data Sets: ",
                         y_label, " vs. ", x_label),
          x = x_label, y = y_label
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
    }  # End ungrouped discrete plot
    
  } else {
    # Numeric plot: points (+ optional spaghetti) + polynomial curve + ribbon.
    
    has_groups <- !is.null(group_var) && "group" %in% names(df)
    curve_df <- polynomial_pooled(df, by_group = has_groups)
    
    # Calculate pooled R² for the polynomial fits
    pooled_r2 <- NULL
    if (!has_groups) {
      # Only calculate overall R² if not grouped
      tryCatch({
        # Fit polynomial in each imputation and get R²
        imps <- split(df, df$imp)
        r2_values <- sapply(imps, function(d) {
          d0 <- stats::na.omit(d[, c("y", "x")])
          if (nrow(d0) <= poly_degree) return(NA_real_)
          
          fit <- try(
            stats::lm(y ~ stats::poly(x, degree = poly_degree, raw = FALSE), data = d0),
            silent = TRUE
          )
          
          if (inherits(fit, "try-error")) return(NA_real_)
          
          # Get R²
          summary(fit)$r.squared
        })
        
        # Pool R² using Fisher's z-transformation
        r2_finite <- r2_values[is.finite(r2_values)]
        if (length(r2_finite) > 0) {
          # Convert R² to R, then to Fisher's z
          r_values <- sqrt(pmax(0, pmin(1, r2_finite)))
          z_values <- 0.5 * log((1 + r_values) / (1 - r_values))
          
          # Pool z values
          z_pooled <- mean(z_values, na.rm = TRUE)
          
          # Convert back to R, then to R²
          r_pooled <- tanh(z_pooled)
          pooled_r2 <- r_pooled^2
        }
      }, error = function(e) NULL)
    }
    
    if (has_groups) {
      # Grouped continuous plot
      p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, color = group))
      
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
      
      # points + polynomial regression
      p <- p +
        # Add points (if requested)
        {
          if (points) {
            ggplot2::geom_point(
              alpha = point_alpha,
              size  = point_size
            )
          } else {
            NULL
          }
        } +
        # Add reference line at y=0 when Y is standardized
        {
          if (standardize %in% c("y", "both")) {
            ggplot2::geom_hline(yintercept = 0, color = "grey70", 
                                linetype = "dashed", linewidth = 0.5)
          } else {
            NULL
          }
        }
      
      # Add polynomial curve and ribbon only if polynomial = TRUE
      if (polynomial) {
        # Ensure curve_df$group is a factor matching df$group
        curve_df$group <- factor(curve_df$group, levels = levels(df$group))
        
        p <- p +
          # Add confidence ribbon (if requested)
          {
            if (ci) {
              suppressWarnings(
                ggplot2::geom_ribbon(
                  data        = curve_df,
                  ggplot2::aes(x = x, ymin = lwr, ymax = upr, fill = group),
                  inherit.aes = FALSE,
                  alpha       = 0.15
                )
              )
            } else {
              NULL
            }
          } +
          ggplot2::geom_line(
            data        = curve_df,
            ggplot2::aes(x = x, y = mean, color = group),
            inherit.aes = FALSE,
            linewidth   = 1.2
          )
      }
      
      p <- p +
        ggplot2::scale_y_continuous(breaks = y_breaks, limits = y_limits) +
        ggplot2::labs(
          title = paste0(y_label, " vs. ", x_label, " by ", group_var),
          x = x_label, 
          y = y_label,
          color = group_var,
          fill = group_var
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
      # Ungrouped continuous plot (original)
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
      
      # points + polynomial regression
      p <- p +
        # Add points (if requested)
        {
          if (points) {
            ggplot2::geom_point(
              alpha = point_alpha,
              size  = point_size,
              color = point_col
            )
          } else {
            NULL
          }
        } +
        # Add reference line at y=0 when Y is standardized
        {
          if (standardize %in% c("y", "both")) {
            ggplot2::geom_hline(yintercept = 0, color = "grey70", 
                                linetype = "dashed", linewidth = 0.5)
          } else {
            NULL
          }
        }
      
      # Add polynomial curve and ribbon only if polynomial = TRUE
      if (polynomial) {
        p <- p +
          # Add confidence ribbon (if requested)
          {
            if (ci) {
              suppressWarnings(
                ggplot2::geom_ribbon(
                  data        = curve_df,
                  ggplot2::aes(x = x, ymin = lwr, ymax = upr),
                  inherit.aes = FALSE,
                  fill        = band_col,
                  alpha       = plot_shading
                )
              )
            } else {
              NULL
            }
          } +
          ggplot2::geom_line(
            data        = curve_df,
            ggplot2::aes(x = x, y = mean),
            inherit.aes = FALSE,
            color       = curve_col,
            linewidth   = 1.2
          )
      }
      
      p <- p +
        ggplot2::scale_y_continuous(breaks = y_breaks, limits = y_limits) +
        ggplot2::labs(
          title = paste0("Bivariate Plot Over ", n_imps,
                         " Imputed Data Sets: ", y_label, " vs. ", x_label),
          x = x_label, y = y_label
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
      
      # Add R² annotation if requested and available
      if (rsquare && !is.null(pooled_r2) && is.finite(pooled_r2)) {
        p <- p + ggplot2::annotate(
          "text",
          x = Inf, y = Inf,
          label = sprintf("Pooled R² = %.3f", pooled_r2),
          hjust = 1.1, vjust = 1.5,
          size = font_size / .pt,
          color = "black"
        )
      }
    }  # End ungrouped continuous plot
  }  # End continuous vs discrete branching
  
  # Return behavior:
  # - If print = TRUE: print now and return invisible
  # - If print = FALSE: return visible (R will auto-print if not assigned)
  if (print) {
    print(p)
    invisible(p)
  } else {
    p
  }
}


# INTERNAL: Multiple bivariate plots ----
# Called by bivariate_plot() when vars/y_vars/x_vars are specified
# Not exported - users should call bivariate_plot() instead

.bivariate_pairs_internal <- function(
    vars = NULL,
    y_vars = NULL,
    x_vars = NULL,
    model,
    discrete_x = NULL,
    standardize = c("none", "y", "x", "both"),
    poly_degree = 3,
    polynomial = TRUE,
    ci = TRUE,
    rsquare = FALSE,
    level = 0.95,
    points = TRUE,
    point_alpha = 0.15,
    point_size = 1.2,
    point_color = plot_point_color,
    curve_color = plot_curve_color,
    band_fill = plot_band_color,
    font_size = 14,
    lines = FALSE,
    print = FALSE
) {
  standardize <- match.arg(standardize)
  
  # Convert color parameters (can be color names or hex codes)
  if (point_color %in% names(plot_colors)) {
    point_col <- unname(plot_colors[point_color])
  } else {
    point_col <- point_color  # Use as-is (hex code)
  }
  
  if (curve_color %in% names(plot_colors)) {
    curve_col <- unname(plot_colors[curve_color])
  } else {
    curve_col <- curve_color  # Use as-is (hex code)
  }
  
  if (band_fill %in% names(plot_colors)) {
    band_col <- unname(plot_colors[band_fill])
  } else {
    band_col <- band_fill  # Use as-is (hex code)
  }
  
  # Auto-populate discrete_x from ORDINAL/NOMINAL if not provided
  if (is.null(discrete_x)) {
    discrete_x <- .get_categorical_vars(model)
  }
  
  # Determine which pairs to create
  if (!is.null(vars)) {
    # All unique pairs from vars
    if (!is.null(y_vars) || !is.null(x_vars)) {
      warning("'vars' specified, ignoring 'y_vars' and 'x_vars'")
    }
    if (length(vars) < 2) {
      stop("Need at least 2 variables in 'vars' to create pairs")
    }
    
    # Create all unique pairs (no duplicates, no self-pairs)
    pairs_list <- combn(vars, 2, simplify = FALSE)
    pair_names <- sapply(pairs_list, function(p) paste(p[1], "~", p[2]))
    
  } else if (!is.null(y_vars) && !is.null(x_vars)) {
    # Cartesian product: each Y with each X
    pairs_list <- lapply(y_vars, function(y) {
      lapply(x_vars, function(x) c(y, x))
    })
    pairs_list <- unlist(pairs_list, recursive = FALSE)
    pair_names <- sapply(pairs_list, function(p) paste(p[1], "~", p[2]))
    
  } else {
    stop("Must specify either 'vars' OR both 'y_vars' and 'x_vars'")
  }
  
  # Create plots for each pair
  plots <- vector("list", length(pairs_list))
  names(plots) <- pair_names
  
  for (i in seq_along(pairs_list)) {
    pair <- pairs_list[[i]]
    y_var <- pair[1]
    x_var <- pair[2]
    
    # Build formula
    formula_str <- paste0(y_var, " ~ ", x_var)
    
    # Try to create the plot
    tryCatch({
      # Create formula
      fmla <- stats::as.formula(formula_str)
      
      # Call bivariate_plot in single-plot mode
      p <- bivariate_plot(
        formula = fmla,
        vars = NULL,      # Explicitly NULL to avoid confusion
        y_vars = NULL,    # Explicitly NULL
        x_vars = NULL,    # Explicitly NULL
        model = model,
        discrete_x = discrete_x,
        standardize = standardize,
        poly_degree = poly_degree,
        polynomial = polynomial,
        ci = ci,
        rsquare = rsquare,
        level = level,
        points = points,
        point_alpha = point_alpha,
        point_size = point_size,
        curve_color = curve_color,
        band_fill = band_fill,
        font_size = font_size,
        lines = lines
      )
      
      plots[[i]] <- p
      
      if (print) {
        print(p)
      }
      
    }, error = function(e) {
      warning("Failed to create plot for ", pair_names[i], ": ", e$message)
      plots[[i]] <- NULL
    })
  }
  
  # Remove NULL plots (failed)
  plots <- plots[!sapply(plots, is.null)]
  
  if (length(plots) == 0) {
    warning("No plots were successfully created")
    return(invisible(NULL))
  }
  
  message("Created ", length(plots), " bivariate plot(s)")
  
  if (print) {
    for (p in plots) print(p)
    invisible(plots)
  } else {
    plots
  }
}

# CHI-BAR TEST OF RANDOM SLOPES ----

#' Chi-Bar-Square Test for Random Slopes in rblimp Models
#' 
#' Computes chi-bar-square tests for random slopes using MCMC-based Wald 
#' chi-square statistics from rblimp model output. Tests the null hypothesis
#' that both the slope variance and intercept-slope covariance are zero.
#' 
#' IMPORTANT: This test is designed for models with a SINGLE random slope.
#' If your model has multiple random slopes, this test may be invalid because
#' it does not account for correlations with other random effects. See Details.
#'
#' @param model An rblimp model object with random slopes
#' @param DV Name of the dependent variable (e.g., "Empower")
#' @param IV Name of the predictor variable with random slope (e.g., "LMX")
#'
#' @return A list containing:
#'   - summary_table: Data frame with results from different methods
#'   - wald_chisq: The computed Wald chi-square statistic
#'   - rho: Correlation between slope variance and covariance parameters
#'   - theta: Level probability (for Case 6)
#'   - posterior_means: Vector of posterior means (used for Wald test)
#'   - posterior_medians: Vector of posterior medians (used for reporting)
#'   - posterior_cov: Covariance matrix of the two parameters
#'   - n_random_slopes: Number of random slopes detected in the model
#'   - warning_multiple_slopes: Logical indicating if multiple slopes detected
#'
#' @details
#' ## Point Estimates: Median vs Mean
#' The function reports posterior **medians** for the variance and covariance
#' parameters, following rblimp's convention. However, the Wald chi-square test
#' uses posterior **means** as required by the test statistic formula. This is
#' appropriate because variance parameters often have skewed posterior distributions,
#' making the median a better point estimate for reporting, while the mean is 
#' required for the quadratic form in the Wald test.
#' 
#' ## Model Requirements
#' This test assumes your model has:
#' - Random intercepts
#' - ONE random slope (for the IV being tested)
#' - Optional: fixed effects, level-2 predictors
#' 
#' ## Multiple Random Slopes
#' If your model has multiple random slopes (e.g., random slopes for both
#' LMX and another variable), the test becomes more complex:
#' 
#' **Option 1: Test Each Slope Separately (INVALID)**
#' Running this test separately for each slope is technically invalid because
#' it ignores the correlation structure among all random effects. The weights
#' for the chi-bar-square distribution depend on the full covariance structure.
#' 
#' **Option 2: Joint Test (REQUIRES EXTENSION)**
#' To properly test multiple random slopes simultaneously, you would need:
#' - Test all slope variances and covariances jointly
#' - Account for full covariance matrix of all random effects
#' - Use higher-dimensional chi-bar-square distribution
#' - Weights become much more complex (see Stoel et al. 2006)
#' 
#' For now, this function only handles the single random slope case. If you
#' have multiple random slopes, consider:
#' 1. Testing slopes in separate models (each with only one random slope)
#' 2. Using simulation-based methods (e.g., parametric bootstrap)
#' 3. Consulting Stoel et al. (2006) for the general multivariate case
#' 
#' @references
#' Stoel, R.D., Garre, F.G., Dolan, C., & van den Wittenboer, G. (2006). 
#' On the likelihood ratio test in structural equation modeling when 
#' parameters are subject to boundary constraints. *Psychological Methods*, 
#' 11(4), 439-455.
#'
#' @export
chibar_test <- function(model, DV, IV) {
  
  # Extract iterations object
  iterations <- model@iterations
  
  # Get column names
  col_names <- names(iterations)
  
  # Use the provided DV name
  dv_name <- DV
  predictor_name <- IV
  
  # Construct the parameter column names
  slope_var_name <- paste0(dv_name, ".level-2 slope variance of.", predictor_name)
  covar_name <- paste0(dv_name, ".level-2 intercept covariance with.", predictor_name)
  
  # Check for multiple random slopes in the model
  slope_pattern <- paste0(dv_name, ".level-2 slope variance of.")
  slope_columns <- grep(slope_pattern, col_names, value = TRUE, fixed = TRUE)
  n_random_slopes <- length(slope_columns)
  
  warning_multiple_slopes <- FALSE
  if (n_random_slopes > 1) {
    warning_multiple_slopes <- TRUE
    warning(
      "\n",
      "========================================================================\n",
      "WARNING: Multiple random slopes detected in model!\n",
      "========================================================================\n",
      "Your model appears to have ", n_random_slopes, " random slopes:\n",
      paste("  -", gsub(paste0(dv_name, ".level-2 slope variance of."), "", slope_columns), 
            collapse = "\n"), "\n\n",
      "This test is designed for models with a SINGLE random slope.\n",
      "Testing each slope separately (as done here) is technically INVALID\n",
      "because it ignores correlations among all random effects.\n\n",
      "The proper test would need to jointly test all random slopes and\n",
      "account for the full covariance structure (see Stoel et al. 2006).\n\n",
      "Recommendations:\n",
      "  1. Re-fit separate models, each with only one random slope\n",
      "  2. Use simulation-based methods (parametric bootstrap)\n",
      "  3. Interpret these results with extreme caution\n",
      "========================================================================\n"
    )
  }
  
  # Check if columns exist
  if (!slope_var_name %in% col_names) {
    stop("Cannot find column: ", slope_var_name, "\n",
         "Available columns: ", paste(col_names, collapse = ", "))
  }
  
  if (!covar_name %in% col_names) {
    stop("Cannot find column: ", covar_name, "\n",
         "Available columns: ", paste(col_names, collapse = ", "))
  }
  
  # Extract MCMC samples
  slope_var_samples <- iterations[[slope_var_name]]
  covar_samples <- iterations[[covar_name]]
  
  # Remove any NA or infinite values
  valid_idx <- complete.cases(slope_var_samples, covar_samples) & 
    is.finite(slope_var_samples) & 
    is.finite(covar_samples)
  
  if (sum(valid_idx) < length(slope_var_samples)) {
    warning(sum(!valid_idx), " iterations removed due to NA or infinite values")
  }
  
  slope_var_samples <- slope_var_samples[valid_idx]
  covar_samples <- covar_samples[valid_idx]
  
  n_samples <- length(slope_var_samples)
  
  if (n_samples < 100) {
    warning("Fewer than 100 valid MCMC samples may give unstable estimates")
  }
  
  # Compute posterior means (theta) - USED FOR WALD TEST
  theta <- c(mean(slope_var_samples), mean(covar_samples))
  names(theta) <- c("slope_variance", "intercept_slope_covariance")
  
  # Compute posterior medians - USED FOR REPORTING
  # (Parameters may have skewed distributions)
  theta_median <- c(median(slope_var_samples), median(covar_samples))
  names(theta_median) <- c("slope_variance", "intercept_slope_covariance")
  
  # Null hypothesis (theta_0)
  theta_0 <- c(0, 0)
  
  # Compute covariance matrix of the parameters
  param_matrix <- cbind(slope_var_samples, covar_samples)
  cov_matrix <- cov(param_matrix)
  
  # Compute Wald chi-square statistic
  # χ²_MCMC = (θ - θ₀)' cov(θ)⁻¹ (θ - θ₀)
  theta_diff <- theta - theta_0
  
  # Check if covariance matrix is invertible
  if (det(cov_matrix) < 1e-10) {
    warning("Covariance matrix is nearly singular. Results may be unstable.")
  }
  
  cov_inv <- solve(cov_matrix)
  wald_chisq <- as.numeric(t(theta_diff) %*% cov_inv %*% theta_diff)
  
  # Compute correlation between parameters (for Case 6)
  rho <- cor(slope_var_samples, covar_samples)
  
  # Compute level probability (Self & Liang Case 6)
  theta_level <- acos(-rho) / pi
  
  # =========================================================================
  # Compute p-values using different methods
  # =========================================================================
  
  # Method 1: Standard chi-square(2)
  p_standard <- pchisq(wald_chisq, df = 2, lower.tail = FALSE)
  
  # Method 2: Stram & Lee (1994) - "Most common"
  # Weights: 0.50, 0.25, 0.25 for chi-sq(0, 1, 2)
  w_stram_lee <- c(0.50, 0.25, 0.25)
  # P(chi-sq(0) > stat) = 0 if stat > 0, so first term drops out
  p_stram_lee <- w_stram_lee[2] * pchisq(wald_chisq, df = 1, lower.tail = FALSE) +
    w_stram_lee[3] * pchisq(wald_chisq, df = 2, lower.tail = FALSE)
  
  # Method 3: Self & Liang (1987) Case 6 - Correlation-adjusted
  # Weights depend on rho
  w0_case6 <- 0.5
  w1_case6 <- 0.5 - theta_level
  w2_case6 <- theta_level
  w_case6 <- c(w0_case6, w1_case6, w2_case6)
  
  # P(chi-sq(0) > stat) = 0 if stat > 0, so first term drops out
  p_case6 <- w_case6[2] * pchisq(wald_chisq, df = 1, lower.tail = FALSE) +
    w_case6[3] * pchisq(wald_chisq, df = 2, lower.tail = FALSE)
  
  # =========================================================================
  # Create summary table
  # =========================================================================
  
  summary_table <- data.frame(
    Method = c(
      "Standard chi-square",
      "Stram & Lee (1994)",
      "Self & Liang (1987) Case 6"
    ),
    ChiSq = round(rep(wald_chisq, 3), 3),
    dfWeights = c(
      "df = 2",
      sprintf("(%.2f, %.2f, %.2f)", w_stram_lee[1], w_stram_lee[2], w_stram_lee[3]),
      sprintf("(%.2f, %.2f, %.2f)", w_case6[1], w_case6[2], w_case6[3])
    ),
    PValue = round(c(p_standard, p_stram_lee, p_case6), 3),
    Notes = c(
      "Ignores boundary issue",
      "Assumes independence (rho=0)",
      sprintf("Uses rho=%.3f, theta=%.3f", rho, theta_level)
    ),
    stringsAsFactors = FALSE
  )
  
  # Add assumption note as an attribute
  assumption_note <- paste0(
    "ASSUMPTION: Model has only ONE random slope (", predictor_name, ").\n",
    if (n_random_slopes > 1) {
      paste0("WARNING: ", n_random_slopes, " random slopes detected. Test may be invalid!")
    } else {
      "Assumption satisfied."
    }
  )
  
  # =========================================================================
  # Create result object
  # =========================================================================
  
  result <- list(
    summary_table = summary_table,
    assumption_note = assumption_note,
    wald_chisq = wald_chisq,
    rho = rho,
    theta = theta_level,
    posterior_means = theta,
    posterior_medians = theta_median,
    posterior_cov = cov_matrix,
    n_samples = n_samples,
    predictor_name = predictor_name,
    dv_name = dv_name,
    n_random_slopes = n_random_slopes,
    all_random_slopes = gsub(paste0(dv_name, ".level-2 slope variance of."), "", slope_columns),
    warning_multiple_slopes = warning_multiple_slopes,
    slope_var_samples = slope_var_samples,
    covar_samples = covar_samples
  )
  
  class(result) <- "chibar_rblimp"
  
  return(result)
}


#' Print method for chibar_rblimp objects
#' @export
print.chibar_rblimp <- function(x, digits = 3, ...) {
  cat("=================================================================\n")
  cat("Chi-Bar-Square Test for Random Slope\n")
  cat("=================================================================\n")
  cat("\n")
  cat("Model:", x$dv_name, "~ ...\n")
  cat("Random slope predictor:", x$predictor_name, "\n")
  cat("MCMC samples:", x$n_samples, "\n")
  cat("\n")
  cat("Testing H0: var(slope) = 0 AND cov(intercept, slope) = 0\n")
  cat("\n")
  
  # Print assumption note at the top
  if (x$warning_multiple_slopes) {
    cat("*** WARNING: MULTIPLE RANDOM SLOPES DETECTED ***\n")
    cat("Random slopes in model:", paste(x$all_random_slopes, collapse = ", "), "\n")
    cat("This test assumes ONLY ONE random slope.\n")
    cat("Results may be INVALID. See documentation for details.\n")
  } else {
    cat("Note: Test assumes that the fitted model has only one random slope.\n")
  }
  cat("\n")
  
  cat("-----------------------------------------------------------------\n")
  cat("Posterior Estimates:\n")
  cat("-----------------------------------------------------------------\n")
  cat("\n")
  cat(sprintf("  Slope variance:              %8.*f\n", digits, x$posterior_medians[1]))
  cat(sprintf("  Intercept-slope covariance:  %8.*f\n", digits, x$posterior_medians[2]))
  cat(sprintf("  Parameter correlation (rho): %8.*f\n", digits, x$rho))
  cat("\n")
  
  cat("-----------------------------------------------------------------\n")
  cat("Wald Chi-Square Statistic:\n")
  cat("-----------------------------------------------------------------\n")
  cat("\n")
  cat(sprintf("  χ² = %.*f\n", digits, x$wald_chisq))
  cat("\n")
  
  cat("-----------------------------------------------------------------\n")
  cat("Test Results:\n")
  cat("-----------------------------------------------------------------\n")
  cat("\n")
  
  # Column starting positions from desired_spacing.dat (0-indexed):
  # Method: 0, ChiSq: 30, dfWeights: 39, PValue: 61, Notes: 71
  
  # Build each row by positioning columns at exact starting positions
  for (i in 0:nrow(x$summary_table)) {
    if (i == 0) {
      # Header row
      method_text <- "Method"
      chisq_text <- "ChiSq"
      weights_text <- "dfWeights"
      pvalue_text <- "PValue"
      notes_text <- "Notes"
    } else {
      # Data rows
      method_text <- x$summary_table$Method[i]
      chisq_text <- sprintf("%.3f", x$summary_table$ChiSq[i])
      weights_text <- x$summary_table$dfWeights[i]
      pvalue_text <- sprintf("%.3f", x$summary_table$PValue[i])
      notes_text <- x$summary_table$Notes[i]
    }
    
    # Build the line with exact column positions
    line <- sprintf("%-30s%-9s%-22s%-10s%s",
                    method_text,
                    chisq_text, 
                    weights_text,
                    pvalue_text,
                    notes_text)
    cat(line, "\n", sep="")
  }
  
  cat("\n")
  cat("-----------------------------------------------------------------\n")
  
  invisible(x)
}


#' Plot method for chibar_rblimp objects
#' @export
plot.chibar_rblimp <- function(x, ...) {
  par(mfrow = c(2, 2))
  
  # Plot 1: Joint distribution of parameters
  plot(x$slope_var_samples, x$covar_samples,
       xlab = "Slope Variance",
       ylab = "Intercept-Slope Covariance",
       main = sprintf("Joint Posterior Distribution\nrho = %.3f", x$rho),
       pch = 16, col = rgb(0, 0, 0, 0.2),
       xlim = range(c(0, x$slope_var_samples)),
       ylim = range(c(min(x$covar_samples), max(x$covar_samples))))
  abline(h = 0, v = 0, col = "red", lty = 2, lwd = 2)
  points(x$posterior_means[1], x$posterior_means[2], 
         pch = 19, col = "blue", cex = 2)
  legend("topright", legend = c("Posterior samples", "Posterior mean", "Null (0,0)"),
         pch = c(16, 19, NA), lty = c(NA, NA, 2), 
         col = c(rgb(0,0,0,0.5), "blue", "red"), cex = 0.8)
  
  # Plot 2: Trace plot - Slope variance
  plot(x$slope_var_samples, type = "l",
       xlab = "Iteration",
       ylab = "Slope Variance",
       main = "MCMC Trace: Slope Variance",
       col = rgb(0, 0, 0, 0.7))
  abline(h = x$posterior_means[1], col = "blue", lwd = 2)
  
  # Plot 3: Trace plot - Covariance
  plot(x$covar_samples, type = "l",
       xlab = "Iteration",
       ylab = "Intercept-Slope Covariance",
       main = "MCMC Trace: Covariance",
       col = rgb(0, 0, 0, 0.7))
  abline(h = x$posterior_means[2], col = "blue", lwd = 2)
  abline(h = 0, col = "red", lty = 2, lwd = 2)
  
  # Plot 4: P-value comparison
  methods <- x$summary_table$Method
  p_values <- x$summary_table$P_Value
  
  barplot(p_values, 
          names.arg = c("Standard\nchi-sq(2)", "Stram-Lee\n(rho=0)", "Case 6\n(rho-adj)"),
          ylab = "P-value",
          main = "P-value Comparison",
          col = c("gray70", "skyblue", "steelblue"),
          ylim = c(0, max(p_values) * 1.2))
  abline(h = 0.05, col = "red", lty = 2, lwd = 2)
  abline(h = 0.10, col = "orange", lty = 2, lwd = 1)
  text(1:3, p_values, labels = sprintf("%.4f", p_values), 
       pos = 3, cex = 0.9)
  legend("topright", legend = c("alpha = 0.05", "alpha = 0.10"),
         lty = 2, col = c("red", "orange"), lwd = c(2, 1), cex = 0.8)
  
  par(mfrow = c(1, 1))
}


#' Summary method for chibar_rblimp objects
#' @export
summary.chibar_rblimp <- function(object, ...) {
  cat("\n")
  cat("=================================================================\n")
  cat("DETAILED SUMMARY: Chi-Bar-Square Test for Random Slope\n")
  cat("=================================================================\n")
  cat("\n")
  
  # Model info
  cat("Model Information:\n")
  cat("------------------\n")
  cat("Dependent variable:", object$dv_name, "\n")
  cat("Random slope predictor:", object$predictor_name, "\n")
  cat("MCMC samples used:", object$n_samples, "\n")
  cat("Number of random slopes in model:", object$n_random_slopes, "\n")
  if (object$n_random_slopes > 1) {
    cat("All random slopes:", paste(object$all_random_slopes, collapse = ", "), "\n")
    cat("\n*** WARNING: Multiple random slopes detected! ***\n")
  }
  cat("\n")
  
  # Hypothesis
  cat("Null Hypothesis:\n")
  cat("----------------\n")
  cat("H0: var(", object$predictor_name, " slope) = 0 AND\n", sep = "")
  cat("    cov(intercept, ", object$predictor_name, " slope) = 0\n", sep = "")
  cat("\n")
  
  # Posterior summaries
  cat("Posterior Summaries:\n")
  cat("--------------------\n")
  cat("(Medians reported; means used for Wald test)\n")
  cat(sprintf("Slope variance:              Median = %8.4f, Mean = %8.4f, SD = %8.4f\n", 
              object$posterior_medians[1],
              object$posterior_means[1], 
              sqrt(object$posterior_cov[1,1])))
  cat(sprintf("Intercept-slope covariance:  Median = %8.4f, Mean = %8.4f, SD = %8.4f\n", 
              object$posterior_medians[2],
              object$posterior_means[2], 
              sqrt(object$posterior_cov[2,2])))
  cat("\n")
  
  # Covariance structure
  cat("Parameter Covariance Matrix:\n")
  cat("----------------------------\n")
  print(object$posterior_cov, digits = 4)
  cat("\n")
  cat(sprintf("Parameter correlation (rho): %.4f\n", object$rho))
  cat("\n")
  
  # Test statistic
  cat("Wald Chi-Square Statistic:\n")
  cat("--------------------------\n")
  cat("Formula: χ² = (θ - θ₀)' cov(θ)⁻¹ (θ - θ₀)\n")
  cat(sprintf("Value:   χ² = %.4f\n", object$wald_chisq))
  cat("\n")
  
  # Chi-bar-square details
  cat("Chi-Bar-Square Distribution Details:\n")
  cat("-------------------------------------\n")
  cat("\nStram & Lee (1994) weights:\n")
  cat("  Assumes independence (rho = 0)\n")
  cat("  w0 = 0.50 (chi-sq 0), w1 = 0.25 (chi-sq 1), w2 = 0.25 (chi-sq 2)\n")
  cat("\n")
  cat("Self & Liang Case 6 weights:\n")
  cat(sprintf("  Accounts for correlation (rho = %.4f)\n", object$rho))
  cat(sprintf("  Level probability theta = %.4f\n", object$theta))
  cat(sprintf("  w0 = %.4f (chi-sq 0), w1 = %.4f (chi-sq 1), w2 = %.4f (chi-sq 2)\n",
              0.5, 0.5 - object$theta, object$theta))
  cat("\n")
  
  # Results table
  cat("Test Results:\n")
  cat("-------------\n")
  print(object$summary_table, row.names = FALSE, digits = 4)
  cat("\n")
  
  # Model assumptions
  cat("Model Assumptions:\n")
  cat("------------------\n")
  cat(object$assumption_note, "\n")
  cat("\n")
  
  # Recommendations
  cat("Recommendations:\n")
  cat("----------------\n")
  
  if (object$warning_multiple_slopes) {
    cat("*** CRITICAL: Model has multiple random slopes ***\n")
    cat("\nThis test is INVALID for models with multiple random slopes.\n")
    cat("The test ignores correlations among all random effects.\n\n")
    cat("To obtain valid results:\n")
    cat("  1. Refit model with ONLY the random slope you want to test\n")
    cat("  2. Use simulation-based methods (parametric bootstrap)\n")
    cat("  3. Implement joint test for all random slopes (see Stoel et al. 2006)\n")
  } else {
    if (abs(object$rho) > 0.3) {
      cat("* Parameter correlation is substantial (|rho| > 0.3)\n")
      cat("  → RECOMMEND using Case 6 (correlation-adjusted) p-value\n")
    } else if (abs(object$rho) > 0.15) {
      cat("* Parameter correlation is moderate (0.15 < |rho| < 0.3)\n")
      cat("  → Case 6 provides more accurate p-value\n")
    } else {
      cat("* Parameter correlation is small (|rho| < 0.15)\n")
      cat("  → Case 6 and Stram-Lee give similar results\n")
    }
    
    p_diff <- abs(object$summary_table$P_Value[2] - object$summary_table$P_Value[3])
    if (p_diff > 0.01) {
      cat(sprintf("* P-values differ by %.4f between methods\n", p_diff))
      cat("  → Correlation adjustment matters for this test\n")
    }
  }
  
  cat("\n")
  cat("=================================================================\n")
  
  invisible(object)
}


# ============================================================================
# EXAMPLE USAGE
# ============================================================================

if (FALSE) {
  # Assuming you have an rblimp model object called 'model2'
  # with a random slope for predictor 'LMX' and DV 'Empower'
  
  # Run the test
  result <- chibar_test_rblimp(
    model = model2, 
    DV = "Empower",
    IV = "LMX"
  )
  
  # Print results
  print(result)
  
  # Detailed summary
  summary(result)
  
  # Diagnostic plots
  plot(result)
  
  # Access specific components
  result$summary_table
  result$wald_chisq
  result$rho
  result$posterior_means
  result$n_random_slopes  # Check how many random slopes detected
  
  # Just get the table
  result$summary_table
}
