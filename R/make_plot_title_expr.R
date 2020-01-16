make_plot_title_expr <- function(para) {
  crown_age <- para_to_pars(para)[1]
  lambda <-  para_to_pars(para)[2]
  mu <-  para_to_pars(para)[3]
  title <- bquote(
    "Age"       ~ "="  ~ .(crown_age) ~
      lambda[0] ~ "="  ~ .(lambda)    ~
      mu[0]     ~ "="  ~ .(mu)
  )
  title
}
