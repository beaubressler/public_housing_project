### learning did tools

library(lattice)

set.seed(123456L)

# 60 time periods, 30 individuals, and 5 waves of treatment
tmax = 60; imax = 30; nlvls = 5

dat = 
  expand.grid(time = 1:tmax, id = 1:imax) |>
  within({
    
    cohort      = NA
    effect      = NA
    first_treat = NA
    
    for (chrt in 1:imax) {
      cohort = ifelse(id==chrt, sample.int(nlvls, 1), cohort)
    }
    
    for (lvls in 1:nlvls) {
      effect      = ifelse(cohort==lvls, sample(2:10, 1), effect)
      first_treat = ifelse(cohort==lvls, sample(1:(tmax+20), 1), first_treat)
    }
    
    first_treat = ifelse(first_treat>tmax, Inf, first_treat)
    treat       = time>=first_treat
    rel_time    = time - first_treat
    y           = id + time + ifelse(treat, effect*rel_time, 0) + rnorm(imax*tmax)
    
    rm(chrt, lvls, cohort, effect)
  })

head(dat)
#>   time id        y rel_time treat first_treat
#> 1    1  1 2.158289      -11 FALSE          12
#> 2    2  1 2.498052      -10 FALSE          12
#> 3    3  1 3.034077       -9 FALSE          12
#> 4    4  1 4.886266       -8 FALSE          12
#> 5    5  1 7.085950       -7 FALSE          12
#> 6    6  1 5.788352       -6 FALSE          12


trellis.par.set(list(
  axis.line      = list(col = NA),
  reference.line = list(col = "gray85", lty = 3),
  superpose.line = list(col = hcl.colors(imax, "SunsetDark")),
  par.xlab.text  = list(fontfamily = "ArialNarrow"),
  par.ylab.text  = list(fontfamily = "ArialNarrow"),
  axis.text      = list(fontfamily = "ArialNarrow")
))

xyplot(
  y ~ time,  
  groups = id,
  type = c("l", "g"),
  ylab = "Y", xlab = "Time variable",
  data = dat
)

### Callaway Sant'Anna