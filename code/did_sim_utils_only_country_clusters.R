suppressWarnings(suppressPackageStartupMessages({
	library(tidyverse)
	library(fixest)
}))

NCOUNTRIES <- 30
NFIRMS <- 500
NYEARS <- 10
NCOUNTRIES_TREATED <- floor(NCOUNTRIES/2)
YEAR_TREATMENT <- floor(NYEARS/2) + 1
EFFECT_SIZE <- 0.2
# Factor for firm autoregression of residuals 0: no autoregression 1: full AR
RHO_C <- 0.5
RHO_F <- 0.5

sim_sample <- function(
	effect_size = EFFECT_SIZE, rho_c = RHO_C, rho_f = RHO_F,	
	ncountries = NCOUNTRIES, nfirms = NFIRMS, nyears = NYEARS,
	ncountries_treated = NCOUNTRIES_TREATED, year_treatment = YEAR_TREATMENT
) {
	dta <- array(
		data = NA,
		dim = c(ncountries, nfirms, nyears), 
		dimnames = list(
			sprintf("country_%02d", 1:ncountries),
			sprintf("firm_%02d", 1:nfirms),
			1:nyears
		)
	)
	
	for (c in 1:ncountries) {
		cerror <- vector("numeric", nyears)
		cerror[1] <- rnorm(1) # IID Error in year 1
		for (f in 1:nfirms) {
			dta[c,f,1] <-  sqrt(1/5)*cerror[1] +  sqrt(1/5)*rnorm(1) 
			for (y in 2:nyears) {
				cerror[y] <- rho_c * cerror[y - 1] + sqrt(1 - rho_c^2)*rnorm(1)
				# autoregressive residuals at the country-level
				dta[c,f,y] <- sqrt(1/5)*cerror[y] + sqrt(1/5)*rnorm(1)
			}
			firm_fe <- rnorm(1)
			dta[c,f,] <- dta[c,f,] + sqrt(1/5)*firm_fe 
		}
		country_fe <- rnorm(1)
		dta[c,,] <- dta[c,,] + sqrt(1/5)*country_fe 
	}
	
	for(y in 1:nyears) {
		year_fe <- rnorm(1)
		dta[,,y] <- dta[,,y] + sqrt(1/5)*year_fe 
	}
	
	df <- as.data.frame.table(dta) %>%
		rename(
			country = Var1, firm = Var2, year = Var3, y = Freq
		) %>%
		mutate(
			firm = paste0(country, "_", firm),
			year = as.numeric(year)
		) %>%
		arrange(country, firm, year)
	
	treated_countries <- sprintf(
		"country_%02d", sample(1:ncountries, ncountries_treated)
	)
	
	df %>% mutate(
		treatment = country %in% treated_countries,
		post = year >= year_treatment,
		treated = treatment & post, 
		y = ifelse(treated, y + effect_size, y)
	)
}


est_models <- function(df) {
	list(
		simple_did = feols(y ~ post*treatment, data = df),
		twfe_did_iid = feols(
			y ~ treated | firm + year, se = "iid", data = df
		),
		twfe_did_firm_cl = feols(
			y ~ treated | firm + year, cluster = "firm", data = df
		),
		twfe_did_country_cl = feols(
			y ~ treated | firm + year, cluster = "country", data = df
		)
	)
}


create_stats <- function(mods) {
	mod_stats <- function(mod) {
		cis <- confint(mod)
		tibble(
			est = unname(coef(mod)[dim(cis)[1]]),
			lb = unname(cis[dim(cis)[1],1]),
			ub = unname(cis[dim(cis)[1],2])
		)
	}
	bind_cols(
		tibble(model = names(mods)),
		bind_rows(lapply(mods, mod_stats))
	)
}


run_sims <- function(sim_runs) {
	bind_cols(
		tibble(run = rep(1:sim_runs, 1, each = 4)),
		bind_rows(
			replicate(sim_runs, create_stats(est_models(sim_sample())), simplify = F)
		)
	)
}
