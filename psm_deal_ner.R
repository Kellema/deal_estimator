# ---- Load packages in R ----
library(MatchIt)
library(dplyr)
library(ggplot2)
library(haven)
library(gridExtra)

# ---- Load dataset in R ----
pres0 <- read_dta("pres0.dta")

# ---- Descriptive statistics per group ----
pres0 %>%
  group_by(seed) %>%
  summarise(n_hh = n(),
            mean_outcome = mean(lcsicrisisplus),
            std_error_outcome = sd(lcsicrisisplus) / sqrt(n_hh))

# ---- Welch Two Sample t-test ----
with(pres0, t.test(lcsicrisisplus ~ seed))

# ---- Selection of working dataframe with dplyr ----
  ## Variables selection
  pres0_cov <- pres0 %>%
    select(
      lcsicrisisplus,
      seed,
      hhsize_1824,
      hh_gender,
      hhsize_over8,
      o_pregnant,
      hhsize_0617,
      region,
      hhsize_65,
      crp_proddif,
      hhh_age2,
      drinkable_water,
      shock_noshock,
      hh_education,
      income_main,
      hh_wealth_light_clean
    )
  ## Variables names list
  pres0cov <- c(
      "lcsicrisisplus",
      "seed",
      "hhsize_1824",
      "hh_gender",
      "hhsize_over8",
      "o_pregnant",
      "hhsize_0617",
      "region",
      "hhsize_65",
      "crp_proddif",
      "hhh_age2",
      "drinkable_water",
      "shock_noshock",
      "hh_education",
      "income_main",
      "hh_wealth_light_clean"
    )

# ---- Mean for each covariate by the treatment status ----
pres0_cov %>%
group_by(seed) %>%
  select(one_of(pres0cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

# ---- T-test on covariates except treatment one ----
lapply(pres0cov[-2], function(v) {
  t.test(pres0_cov[[v]] ~ pres0_cov$seed)
})

# ---- Logit selection model ----
m_ps <- glm(seed ~ hhsize_1824 +
            hh_gender +
            hhsize_over8 +
            o_pregnant +
            hhsize_0617 +
            region +
            hhsize_65 +
            crp_proddif +
            hhh_age2 +
            drinkable_water +
            shock_noshock +
            hh_education +
            income_main +
            hh_wealth_light_clean,
            family = binomial(), data = pres0_cov)
summary(m_ps)

# ---- Data augmented with propensity score ----
prs_df <- data.frame(pr_score = predict(m_ps, type = "response"), seed = m_ps$model$seed)
head(prs_df)

# ---- Ploting the propensity score distribution ----
prs_df %>%
  mutate(
    seed = factor(seed, levels = c(0, 1), labels = c("No_Seed", "Seed"))
  ) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(bins = 30, color = "white") +
  facet_wrap(~ seed) +
  xlab("Probability of receiving seeds") +
  theme_bw()

# ---- Suppression of missing values: Not accounted by MatchIt ----
pres0_cov_nomiss <- pres0_cov %>%  # MatchIt does not allow missing values
select(lcsicrisisplus, seed, one_of(pres0cov)) %>%
  na.omit()

# ---- Treatment effect estimation ----
mod_match <- matchit(seed ~ hhsize_1824 +
                       hh_gender +
                       hhsize_over8 +
                       o_pregnant +
                       hhsize_0617 +
                       region +
                       hhsize_65 +
                       crp_proddif +
                       hhh_age2 +
                       drinkable_water +
                       shock_noshock +
                       hh_education +
                       income_main +
                       hh_wealth_light_clean,
                     method = "nearest", data = pres0_cov_nomiss)
dta_m <- match.data(mod_match)
dim(dta_m)

# ---- function: Visualizing the balance test ----
fn_bal <- function(dta, variable) {
  
  # pull the column as a *vector* (works for tibble + data.frame)
  dta$variable <- dta[[variable]]
  
  dta$seed <- as.factor(dta$seed)
  
  p <- ggplot(dta, aes(x = distance, y = variable, color = seed)) +
    geom_point(alpha = 0.2, size = 1.3) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw()
  
  # If y is numeric/integer, add loess + y-limits
  if (is.numeric(dta$variable) || is.integer(dta$variable)) {
    support <- range(dta$variable, na.rm = TRUE)
    p <- p +
      geom_smooth(method = "loess", se = FALSE) +
      coord_cartesian(ylim = support)   # safer than ylim() (doesn't drop points)
  } else {
    # For categorical variables: jitter points, no loess, no ylim
    p <- p +
      geom_jitter(height = 0.15, alpha = 0.2, size = 1.3)
  }
  
  p
}


# ---- Plotting the balance test with fn_bal ----
grid.arrange(
  fn_bal(dta_m, "hhsize_1824"),
  fn_bal(dta_m, "hh_gender") + theme(legend.position = "none"),
  fn_bal(dta_m, "hhsize_over8"),
  fn_bal(dta_m, "o_pregnant") + theme(legend.position = "none"),
  fn_bal(dta_m, "hhsize_0617"),
  fn_bal(dta_m, "region") + theme(legend.position = "none"),
  nrow = 3, widths = c(1, 0.8)
)

# ---- Comparing means after matching by treatment group: `seed` ----
dta_m %>%
  group_by(seed) %>%
  select(one_of(pres0cov)) %>%
  summarise_all(funs(mean))

# ---- Balance check by t-tests ----
lapply(pres0cov[-2], function(v) {
  t.test(dta_m[[v]] ~ dta_m$seed)
})

# ---- Treatment effect estimation: t-test ----
with(dta_m, t.test(lcsicrisisplus ~ seed))

# ---- Treatment effect estimation: OLS without covariates ----
lm_lcsi <- lm(lcsicrisisplus ~ seed, data = dta_m)
summary(lm_lcsi)

# ---- Treatment effect estimation: OLS with covariates ----
lm_lcsi_cov <- lm(lcsicrisisplus ~ seed +
                hhsize_1824 +
                hh_gender +
                hhsize_over8 +
                o_pregnant +
                hhsize_0617 +
                region +
                hhsize_65 +
                crp_proddif +
                hhh_age2 +
                drinkable_water +
                shock_noshock +
                hh_education +
                income_main +
                hh_wealth_light_clean,
              data = dta_m)
summary(lm_lcsi_cov)

# ---- Deploy and open the shiny app ----
rsconnect::deployApp(
  appDir        = "C:/Users/Boria/OneDrive - Food and Agriculture Organization/FAO 2026/DEAL/RPSM",
  appPrimaryDoc = "psm_shiny_app.R",
  appName       = "psm_shiny_app"
)
shiny::runApp("psm_shiny_app.R")
