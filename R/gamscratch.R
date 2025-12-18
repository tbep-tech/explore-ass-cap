library(mgcv)

# create up to six month lags for tn_load
newtomod <- newdat |>
  mutate(
    tn_load_lag1 = lag(tn_load, 1),
    tn_load_lag2 = lag(tn_load, 2),
    tn_load_lag3 = lag(tn_load, 3),
    tn_load_lag4 = lag(tn_load, 4),
    tn_load_lag5 = lag(tn_load, 5),
    tn_load_lag6 = lag(tn_load, 6),
    yrcat = ifelse(yr < 2010, 'Before 2010', '2010 and After'),
    .by = bay_segment
  ) |>
  rename(tn_load_lag0 = tn_load) |>
  mutate(
    tn_load_lag1 = rowSums(pick(tn_load_lag0:tn_load_lag1), na.rm = FALSE),
    tn_load_lag2 = rowSums(pick(tn_load_lag0:tn_load_lag2), na.rm = FALSE),
    tn_load_lag3 = rowSums(pick(tn_load_lag0:tn_load_lag3), na.rm = FALSE),
    tn_load_lag4 = rowSums(pick(tn_load_lag0:tn_load_lag4), na.rm = FALSE),
    tn_load_lag5 = rowSums(pick(tn_load_lag0:tn_load_lag5), na.rm = FALSE),
    tn_load_lag6 = rowSums(pick(tn_load_lag0:tn_load_lag6), na.rm = FALSE)
  ) |>
  pivot_longer(
    cols = starts_with('tn_load_lag'),
    names_to = 'lag',
    values_to = 'tn_load'
  ) |>
  mutate(
    lag = str_remove(lag, 'tn_load_'),
    lag = str_replace(lag, 'lag', 'Lag ')
  )

mod <- newtomod |>
  group_nest(lag) |>
  mutate(
    model = map(data, function(x) {
      kv <- 15
      modall <- gam(
        chla ~ s(tn_load, bs = 'tp', k = kv) +
          s(mo, bs = 'cc', k = 12) +
          ti(tn_load, mo, bs = c('tp', 'cc'), k = c(kv, 12)),
        knots = list(mo = c(1, 12)),
        family = Gamma(link = 'log'),
        data = x
      )
      modpre <- gam(
        chla ~ s(tn_load, bs = 'tp', k = kv) +
          s(mo, bs = 'cc', k = 12) +
          ti(tn_load, mo, bs = c('tp', 'cc'), k = c(kv, 12)),
        knots = list(mo = c(1, 12)),
        family = Gamma(link = 'log'),
        data = x[x$yrcat == 'Before 2010', ]
      )
      modpos <- gam(
        chla ~ s(tn_load, bs = 'tp', k = kv) +
          s(mo, bs = 'cc', k = 12) +
          ti(tn_load, mo, bs = c('tp', 'cc'), k = c(kv, 12)),
        knots = list(mo = c(1, 12)),
        family = Gamma(link = 'log'),
        data = x[x$yrcat == '2010 and After', ]
      )

      # get rsq for all
      rsqall <- summary(modall)$r.sq
      rsqpre <- summary(modpre)$r.sq
      rsqpos <- summary(modpos)$r.sq

      tibble(
        rsqall = rsqall,
        rsqpre = rsqpre,
        rsqpos = rsqpos
      )
    })
  ) |>
  select(-data) |>
  unnest(model) |>
  pivot_longer(
    cols = starts_with('rsq'),
    names_to = 'period',
    values_to = 'rsq'
  ) |>
  mutate(
    lag = str_remove(lag, 'Lag '),
    period = case_when(
      period == 'rsqall' ~ '1995 to 2024',
      period == 'rsqpre' ~ 'Before 2010',
      period == 'rsqpos' ~ '2010 and After'
    ),
    period = factor(
      period,
      levels = c('1995 to 2024', 'Before 2010', '2010 and After')
    )
  )

rsqplo_fun(mod, c(0.3, 0.9))

newtoprd <- newtomod |>
  mutate(
    lag = gsub(' ', '', lag)
  ) |>
  pivot_wider(
    names_from = lag,
    values_from = tn_load
  ) |>
  group_nest(yrcat) |>
  mutate(
    model = map(data, function(x) {
      kv <- 15
      mod <- gam(
        chla ~ s(Lag3, bs = 'tp', k = kv) +
          s(mo, bs = 'cc', k = 12) +
          ti(Lag3, mo, bs = c('tp', 'cc'), k = c(kv, 12)),
        knots = list(mo = c(1, 12)),
        data = x,
        family = Gamma(link = 'log'),
        na.action = 'na.exclude'
      )
      return(mod)
    }),
    pred = map2(model, data, function(mod, dat) {
      predict(mod, newdata = dat, type = 'response')
    })
  )

toplo <- newtoprd |>
  select(-model) |>
  unnest(c(data, pred))

ggplot(toplo, aes(x = date, y = chla)) +
  geom_point() +
  geom_line(aes(y = pred, group = yrcat, color = yrcat)) +
  # geom_hline(yintercept = 9.3, linetype = 'dashed', color = 'red') +
  theme_minimal() +
  theme(
    legend.position = 'bottom'
  ) +
  labs(
    title = 'Monthly Observed and Predicted Chlorophyll-a in Old Tampa Bay',
    y = 'µg/L',
    x = NULL,
    color = 'Model'
  )

annavg <- tbeptools::anlz_avedat(epcdata)$ann |>
  filter(bay_segment %in% 'OTB', var == 'mean_chla') |>
  filter(yr >= 1995) |>
  mutate(
    achieve = ifelse(val <= 9.3, 'Yes', 'No'),
  )

modestavg <- toplo |>
  summarize(
    predchla = mean(pred, na.rm = TRUE),
    .by = c(yr, yrcat)
  ) |>
  mutate(
    achieve = ifelse(predchla <= 9.3, 'Yes', 'No')
  )

ggplot(annavg, aes(x = yr, y = val)) +
  geom_line(aes(color = 'Observed')) +
  geom_point(aes(shape = achieve, color = 'Observed'), size = 3) +
  geom_line(data = modestavg, aes(x = yr, y = predchla, color = 'Predicted')) +
  geom_point(
    data = modestavg,
    aes(x = yr, y = predchla, shape = achieve, color = 'Predicted'),
    size = 3
  ) +
  geom_hline(yintercept = 9.3, linetype = 'dashed', color = 'red') +
  scale_shape_manual(values = c(16, 17)) +
  scale_color_manual(values = c('Observed' = 'black', 'Predicted' = 'blue')) +
  theme_minimal() +
  theme(
    legend.position = 'bottom'
  ) +
  guides(color = guide_legend(override.aes = list(shape = NA))) +
  labs(
    title = 'Annual Average Observed and Predicted Chlorophyll-a in Old Tampa Bay',
    y = 'µg/L',
    x = 'Year',
    shape = 'Meets 9.3 µg/L Target',
    color = NULL
  )

redscn <- crossing(
  redscn = seq(0, 1, by = 0.05),
  newdat |>
    mutate(yrcat = ifelse(yr < 2010, 'Before 2010', '2010 and After'))
) |>
  group_nest(redscn, yrcat) |>
  mutate(
    yrcat = factor(
      yrcat,
      levels = c('Before 2010', '2010 and After')
    ),
    data = pmap(list(redscn, data), function(red, dat) {
      dat |>
        mutate(
          tn_load_lag0 = tn_load * red,
          tn_load_lag1 = lag(tn_load_lag0, 1),
          tn_load_lag2 = lag(tn_load_lag0, 2),
          tn_load_lag3 = lag(tn_load_lag0, 3)
        ) |>
        mutate(
          tn_load_lag1 = rowSums(
            pick(tn_load_lag0:tn_load_lag1),
            na.rm = FALSE
          ),
          tn_load_lag2 = rowSums(
            pick(tn_load_lag0:tn_load_lag2),
            na.rm = FALSE
          ),
          tn_load_lag3 = rowSums(pick(tn_load_lag0:tn_load_lag3), na.rm = FALSE)
        ) |>
        rename(`Lag3` = tn_load_lag3)
    }),
    data = map2(data, yrcat, function(dat, yrc) {
      mod <- newtoprd |> filter(yrcat == yrc) |> pull(model)
      prd <- predict(
        mod[[1]],
        newdata = dat,
        type = 'response'
      )
      bind_cols(dat, tibble(predchla = prd))
    }),
    annavg = map(data, function(dat) {
      dat |>
        summarize(
          tn_load = sum(tn_load_lag0, na.rm = TRUE),
          predchla = mean(predchla, na.rm = TRUE),
          .by = yr
        )
    })
  )

toplo <- redscn |>
  select(redscn, yrcat, data) |>
  unnest(data)

ggplot(toplo, aes(x = date, y = tn_load_lag0)) +
  geom_line(aes(group = redscn, color = redscn)) +
  scale_color_viridis_c(name = 'Load Reduction\nFraction', option = 'D') +
  facet_wrap(~yrcat, ncol = 1, scales = 'free_x') +
  theme_minimal() +
  theme(
    legend.position = 'bottom'
  ) +
  labs(
    title = 'Monthly Load Reduction Scenarios',
    y = 'tons/month',
    x = NULL
  )

ggplot(toplo, aes(x = date, y = predchla)) +
  geom_line(aes(group = redscn, color = redscn)) +
  geom_hline(yintercept = 9.3, linetype = 'dashed', color = 'red') +
  scale_color_viridis_c(name = 'Load Reduction\nFraction', option = 'C') +
  facet_wrap(~yrcat, ncol = 1, scales = 'free_x') +
  theme_minimal() +
  theme(
    legend.position = 'bottom'
  ) +
  labs(
    title = 'Monthly Predicted Chlorophyll-a Under Load Reduction Scenarios',
    y = 'µg/L',
    x = NULL
  )

toplo2 <- redscn |>
  select(redscn, yrcat, annavg) |>
  unnest(annavg)

ggplot(toplo2, aes(x = yr, y = tn_load, group = redscn)) +
  geom_line(aes(color = redscn)) +
  scale_color_viridis_c(name = 'Load Reduction\nFraction', option = 'D') +
  facet_wrap(~yrcat, ncol = 1, scales = 'free_x') +
  theme_minimal() +
  theme(
    legend.position = 'bottom'
  ) +
  labs(
    title = 'Annual Total Nitrogen Loads from Monthly Reduction Scenarios',
    y = 'tons/year',
    x = NULL
  )

ggplot(toplo2, aes(x = yr, y = predchla, group = redscn)) +
  geom_line(aes(color = redscn)) +
  geom_hline(yintercept = 9.3, linetype = 'dashed', color = 'red') +
  scale_color_viridis_c(name = 'Load Reduction\nFraction', option = 'C') +
  facet_wrap(~yrcat, ncol = 1, scales = 'free_x') +
  theme_minimal() +
  theme(
    legend.position = 'bottom'
  ) +
  labs(
    title = 'Annual Average Predicted Chlorophyll-a Under Load Reduction Scenarios',
    y = 'µg/L',
    x = NULL
  )

toplo3 <- toplo2 |>
  summarise(
    avechl = mean(predchla, na.rm = TRUE),
    hichl = t.test(predchla)$conf.int[2],
    locl = t.test(predchla)$conf.int[1],
    tn_load = mean(tn_load, na.rm = TRUE),
    .by = c(redscn, yrcat)
  )

# get the x-axis load values for 9.3 ug/L chl-a
befass <- lm(
  avechl ~ tn_load,
  data = toplo3[toplo3$yrcat %in% 'Before 2010', ]
) |>
  coefficients()
aftass <- lm(
  avechl ~ tn_load,
  data = toplo3[toplo3$yrcat %in% '2010 and After', ]
) |>
  coefficients()
befass <- (9.3 - befass[1]) / befass[2]
aftass <- (9.3 - aftass[1]) / aftass[2]

addlns <- tibble(
  avechl = 9.3,
  yrcat = factor(
    c('Before 2010', '2010 and After'),
    levels = c('Before 2010', '2010 and After')
  ),
  tn_load = c(befass, aftass)
)

ggplot(toplo3, aes(x = tn_load, y = avechl)) +
  geom_line(aes(color = yrcat)) +
  geom_point(aes(color = yrcat)) +
  geom_segment(
    data = addlns,
    aes(
      x = tn_load,
      y = avechl,
      xend = tn_load,
      yend = 0,
      color = yrcat
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
        'Estimated load at 9.3 µg/L: ',
        round(tn_load, 0),
        ' tons/yr'
      ),
      color = yrcat
    ),
    hjust = 1.05,
    vjust = 0,
    size = 3,
    fontface = 'bold'
  ) +
  geom_ribbon(
    aes(
      ymin = locl,
      ymax = hichl,
      fill = yrcat
    ),
    alpha = 0.2,
    color = NA
  ) +
  geom_hline(yintercept = 9.3, linetype = 'dashed', color = 'red') +
  facet_wrap(~yrcat) +
  theme_minimal() +
  theme(
    legend.position = 'none'
  ) +
  labs(
    title = 'Average Predicted Chlorophyll-a vs. Average Nitrogen\nLoad Under Load Reduction Scenarios',
    y = 'µg/L',
    x = 'tons/yr',
  )
