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

# rsq model function
rsqmod_fun <- function(tomod, frmtxt, useglm = F) {
  tomod <- tomod |>
    group_nest(lag)

  if (!useglm) {
    out <- tomod |>
      mutate(
        model = map(data, function(x) {
          mod <- lm(
            as.formula(frmtxt),
            data = x
          )

          # get rsq for all
          rsq <- summary(mod)$r.squared

          tibble(
            rsq = rsq,
          )
        })
      )
  }

  if (useglm) {
    out <- tomod |>
      mutate(
        model = map(data, function(x) {
          mod <- glm(
            as.formula(frmtxt),
            family = gaussian(link = 'log'),
            data = x
          )

          # get rsq for all
          rsq <- 1 - (mod$deviance / mod$null.deviance)

          tibble(
            rsq = rsq
          )
        })
      )
  }

  out <- out |>
    select(-data) |>
    unnest(model) |>
    pivot_longer(
      cols = starts_with('rsq'),
      names_to = 'period',
      values_to = 'rsq'
    ) |>
    mutate(
      lag = str_remove(lag, 'Lag ')
    )

  return(out)
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

# model function
mod_fun <- function(tomodotb, frmtxt, useglm = F) {
  tomodotb <- tomodotb |>
    pivot_wider(
      names_from = lag,
      values_from = tn_load
    ) |>
    nest()

  if (!useglm) {
    out <- tomodotb |>
      mutate(
        model = map(data, function(x) {
          lm(
            as.formula(frmtxt),
            data = x,
            na.action = 'na.exclude'
          )
        }),
        pred = map2(model, data, function(mod, dat) {
          predict(mod, newdata = dat)
        })
      )
  }

  if (useglm) {
    out <- tomodotb |>
      mutate(
        model = map(data, function(x) {
          glm(
            as.formula(frmtxt),
            data = x,
            family = gaussian(link = 'log'),
            na.action = 'na.exclude'
          )
        }),
        pred = map2(model, data, function(mod, dat) {
          predict(mod, newdata = dat, type = 'response')
        })
      )
  }

  return(out)
}

# model prediction function plots
modprd_fun <- function(toprdotb, stp = c('mo', 'yr')) {
  stp <- match.arg(stp)

  toplo <- toprdotb |>
    select(-model) |>
    unnest(c(data, pred))

  if (stp == 'mo') {
    p <- ggplot(toplo, aes(x = date, y = chla)) +
      geom_point() +
      geom_line(aes(y = pred), color = 'cornflowerblue') +
      theme_minimal() +
      theme(
        legend.position = 'bottom'
      ) +
      labs(
        title = 'Monthly Observed and Predicted Chlorophyll-a in Old Tampa Bay',
        y = 'µg/L',
        x = NULL
      )
  }

  if (stp == 'yr') {
    annavg <- tbeptools::anlz_avedat(epcdata)$ann |>
      filter(bay_segment %in% 'OTB', var == 'mean_chla') |>
      filter(yr >= min(toplo$yr))

    modestavg <- toplo |>
      summarize(
        predchla = mean(pred, na.rm = TRUE),
        .by = c(yr)
      )

    p <- ggplot(annavg, aes(x = yr, y = val)) +
      geom_line(aes(color = 'Observed')) +
      geom_point(aes(color = 'Observed'), size = 3) +
      geom_line(
        data = modestavg,
        aes(x = yr, y = predchla, color = 'Predicted')
      ) +
      geom_point(
        data = modestavg,
        aes(x = yr, y = predchla, color = 'Predicted'),
        size = 3
      ) +
      scale_color_manual(
        values = c('Observed' = 'black', 'Predicted' = 'cornflowerblue')
      ) +
      theme_minimal() +
      theme(
        legend.position = 'bottom'
      ) +
      guides(color = guide_legend(override.aes = list(shape = NA))) +
      labs(
        title = 'Annual Average Observed and Predicted Chlorophyll-a in Old Tampa Bay',
        y = 'µg/L',
        x = 'Year',
        color = NULL
      )
  }

  return(p)
}

# model reduction scenarios
redscn_fun <- function(toprdotb, type = NULL) {
  crossing(
    redscn = seq(0.05, 1, by = 0.05),
    lddat |>
      filter(bay_segment %in% 'OTB') |>
      filter(yr >= 2000)
  ) |>
    group_nest(redscn) |>
    mutate(
      data = pmap(list(redscn, data), function(red, dat) {
        dat |>
          mutate(
            tn_load_lag0 = tn_load * red,
            tn_load_lag1 = lag(tn_load_lag0, 1),
            tn_load_lag2 = lag(tn_load_lag0, 2),
            tn_load_lag3 = lag(tn_load_lag0, 3)
          ) |>
          fill(tn_load_lag1:tn_load_lag3, .direction = 'up') |>
          mutate(
            tn_load_lag1 = rowSums(
              pick(tn_load_lag0:tn_load_lag1),
              na.rm = FALSE
            ),
            tn_load_lag2 = rowSums(
              pick(tn_load_lag0:tn_load_lag2),
              na.rm = FALSE
            ),
            tn_load_lag3 = rowSums(
              pick(tn_load_lag0:tn_load_lag3),
              na.rm = FALSE
            )
          ) |>
          rename(
            `Lag 0` = tn_load_lag0,
            `Lag 1` = tn_load_lag1,
            `Lag 2` = tn_load_lag2,
            `Lag 3` = tn_load_lag3
          ) |>
          inner_join(
            toprdotb$data[[1]] |> select(date, yr, mo, sal, temp),
            by = c('date', 'yr', 'mo')
          )
      }),
      data = map(data, function(dat) {
        mod <- toprdotb |> pull(model)
        prd <- predict(
          mod[[1]],
          newdata = dat,
          type = type
        )
        bind_cols(dat, tibble(predchla = prd))
      }),
      annavg = map(data, function(dat) {
        dat |>
          summarize(
            tn_load = sum(`Lag 0`, na.rm = TRUE),
            predchla = mean(predchla, na.rm = TRUE),
            .by = yr
          )
      })
    )
}

# reduction scenario plots
redscn_plo <- function(redscn, stp = c('mo', 'yr'), var = c('chla', 'tn')) {
  stp <- match.arg(stp)
  var <- match.arg(var)

  if (stp == 'mo') {
    toplo <- redscn |>
      select(redscn, data) |>
      unnest(data)

    if (var == 'chla') {
      p <- ggplot(toplo, aes(x = date, y = predchla)) +
        geom_line(aes(group = redscn, color = redscn)) +
        scale_color_viridis_c(name = 'Load Reduction\nFraction', option = 'C') +
        theme_minimal() +
        theme(
          legend.position = 'bottom'
        ) +
        labs(
          title = 'Monthly Predicted Chlorophyll-a Under Load Reduction Scenarios',
          y = 'µg/L',
          x = NULL
        )
    }

    if (var == 'tn') {
      p <- ggplot(toplo, aes(x = date, y = `Lag 0`)) +
        geom_line(aes(group = redscn, color = redscn)) +
        scale_color_viridis_c(name = 'Load Reduction\nFraction', option = 'D') +
        theme_minimal() +
        theme(
          legend.position = 'bottom'
        ) +
        labs(
          title = 'Monthly Load Reduction Scenarios',
          y = 'tons/month',
          x = NULL
        )
    }
  }

  if (stp == 'yr') {
    toplo <- redscn |>
      select(redscn, annavg) |>
      unnest(annavg)

    if (var == 'tn') {
      p <- ggplot(toplo, aes(x = yr, y = tn_load, group = redscn)) +
        geom_line(aes(color = redscn)) +
        scale_color_viridis_c(name = 'Load Reduction\nFraction', option = 'D') +
        theme_minimal() +
        theme(
          legend.position = 'bottom'
        ) +
        labs(
          title = 'Annual Total Nitrogen Loads from Monthly Reduction Scenarios',
          y = 'tons/year',
          x = NULL
        )
    }

    if (var == 'chla') {
      p <- ggplot(toplo, aes(x = yr, y = predchla, group = redscn)) +
        geom_line(aes(color = redscn)) +
        geom_hline(yintercept = 8.5, linetype = 'dashed', color = 'red') +
        scale_color_viridis_c(name = 'Load Reduction\nFraction', option = 'C') +
        theme_minimal() +
        theme(
          legend.position = 'bottom'
        ) +
        labs(
          title = 'Annual Average Predicted Chlorophyll-a Under Load Reduction Scenarios',
          y = 'µg/L',
          x = NULL
        )
    }
  }

  return(p)
}

# get assimilative capacity data
asscapdata_fun <- function(redscn) {
  redscn |>
    select(redscn, annavg) |>
    unnest(annavg) |>
    summarise(
      avechl = mean(predchla, na.rm = TRUE),
      hichl = tryCatch(t.test(predchla)$conf.int[2], error = function(e) NA),
      lochl = tryCatch(t.test(predchla)$conf.int[1], error = function(e) NA),
      tn_load = mean(tn_load, na.rm = TRUE),
      .by = c(redscn)
    )
}

# get assimilative capacity
asscap_fun <- function(asscapdata) {
  asscap <- lm(
    avechl ~ tn_load,
    data = asscapdata
  ) |>
    coefficients()
  asscap <- (8.5 - asscap[1]) / asscap[2]

  return(asscap)
}

# annual avg values used for assimilative capacity
redscnaggplo_fun <- function(asscapdata, redscn, var = c('chla', 'tn')) {
  var <- match.arg(var)

  yrs <- unique(redscn$annavg[[1]]$yr)
  toplo <- asscapdata |>
    crossing(
      yr = yrs
    )

  if (var == 'tn') {
    p <- ggplot(toplo, aes(x = yr, y = tn_load, group = redscn)) +
      geom_line(aes(color = redscn)) +
      scale_color_viridis_c(name = 'Load Reduction\nFraction', option = 'D') +
      theme_minimal() +
      theme(
        legend.position = 'bottom'
      ) +
      labs(
        title = 'Mean Total Nitrogen Loads Across Years for Load Reduction Scenarios',
        y = 'tons/year',
        x = NULL
      )
  }

  if (var == 'chla') {
    p <- ggplot(toplo, aes(x = yr, y = avechl, group = redscn)) +
      geom_line(aes(color = redscn)) +
      geom_hline(yintercept = 8.5, linetype = 'dashed', color = 'red') +
      scale_color_viridis_c(name = 'Load Reduction\nFraction', option = 'C') +
      theme_minimal() +
      theme(
        legend.position = 'bottom'
      ) +
      labs(
        title = 'Mean Predicted Chlorophyll-a Across Years for Load Reduction Scenarios',
        y = 'µg/L',
        x = NULL
      )
  }

  return(p)
}

# assimilative capacity plot
asscap_plo <- function(asscapdata, asscap) {
  addlns <- tibble(
    avechl = 8.5,
    tn_load = asscap
  )

  ggplot(asscapdata, aes(x = tn_load, y = avechl)) +
    geom_ribbon(aes(ymin = lochl, ymax = hichl), alpha = 0.3) +
    geom_line() +
    geom_point() +
    geom_segment(
      data = addlns,
      aes(
        x = tn_load,
        y = avechl,
        xend = tn_load,
        yend = 0
      ),
      arrow = arrow(length = unit(0.6, "cm")),
      linetype = 'solid',
    ) +
    geom_text(
      data = addlns,
      aes(
        x = tn_load,
        y = 0,
        label = paste0(
          'Estimated load at 8.5 µg/L: ',
          round(tn_load, 0),
          ' tons/yr'
        )
      ),
      hjust = 1.05,
      vjust = 0,
      size = 3,
      fontface = 'bold'
    ) +
    geom_hline(yintercept = 8.5, linetype = 'dashed', color = 'red') +
    theme_minimal() +
    theme(
      legend.position = 'none'
    ) +
    labs(
      title = 'Average Predicted Chlorophyll-a vs. Average Nitrogen\nLoad Under Load Reduction Scenarios',
      y = 'µg/L',
      x = 'tons/yr',
    )
}
