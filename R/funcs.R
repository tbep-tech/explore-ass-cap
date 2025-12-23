# get datasets from repo
# try simple load, download if fail
rdataload <- function(dataurl = NULL) {
  x <- gsub('\\.RData', '', basename(dataurl))

  # try simple load
  ld <- try(load(url(dataurl)), silent = T)

  # return x if load worked
  if (!inherits(ld, 'try-error')) {
    out <- get(x)
  }

  # download x if load failed
  if (inherits(ld, 'try-error')) {
    fl <- paste(tempdir(), basename(dataurl), sep = '/')
    download.file(flurl, destfile = fl, quiet = T)
    load(file = fl)
    out <- get(x)
    suppressMessages(file.remove(fl))
  }

  return(out)
}

# lag plot function
lagplo_fun <- function(x, subtitle, col, title = T) {
  ttl <- 'Chlorophyll-a vs Total Nitrogen Load by Lag Time'
  if (!title) {
    ttl <- NULL
  }

  p <- ggplot(x, aes(x = tn_load / 100, y = chla)) +
    geom_point(alpha = 0.5, size = 0.5) +
    geom_smooth(
      method = 'lm',
      formula = y ~ x,
      color = col,
      se = T
    ) +
    # scale_x_log10() +
    # scale_y_log10() +
    facet_wrap(~lag, ncol = 7, scales = 'free_x') +
    labs(
      x = 'Total Nitrogen Load (0.01 x tons/month)',
      y = 'Chlorophyll-a (µg/L)',
      title = ttl,
      subtitle = subtitle
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 8)
    )

  return(p)
}

# rsq plot function
rsqplo_fun <- function(mod) {
  ggplot(mod, aes(x = as.numeric(lag), y = rsq)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = 0:6) +
    labs(
      x = 'Lag in Months',
      y = expression(R^2),
      title = 'Model fits (R²) by Lag Time'
    ) +
    theme_minimal() +
    theme(legend.position = 'bottom')
}
