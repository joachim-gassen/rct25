library(tidyverse)
library(fixest)

SIM_RUNS <- 1000

sim_run <- function(nfirms = 9000, nyears = 8) {
	df <- expand_grid(
		firm = 1:nfirms,
		year = 1:nyears
	) %>%
		mutate(
			tment = firm > nfirms/2,
			post = year > nyears/2,
			treated = tment & post,
			y = rnorm(nfirms*nyears)
		) %>%
		group_by(firm) %>%
		mutate(
			ypre_below0 = sum(y* ! post) < 0
		) %>%
		ungroup()
	mod_base <- feols(y ~ treated| firm + year, data = df)
	suppressMessages({
		mod_cs <- feols(y ~ treated*ypre_below0 | firm + year, data = df)
	})
	ci_base <- confint(mod_base)[]
	ci_cs <- confint(mod_cs)[]
	tibble(
		base_lb = ci_base[1, 1],
		base_est = coef(mod_base)[[1]],
		base_ub = ci_base[1, 2],
		cs_lb = ci_cs[2, 1],
		cs_est = coef(mod_cs)[[2]],
		cs_ub = ci_cs[2, 2]
	)
}

sim_results <- bind_rows(replicate(SIM_RUNS, sim_run(), simplify = FALSE))

sim_results %>%
	summarise(
		pct_base_sig = mean(sign(base_lb) == sign(base_ub)),
		pct_cs_sig = mean(sign(cs_lb) == sign(cs_ub)),
	)
