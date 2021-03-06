% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_generator.R
\name{data_generator}
\alias{data_generator}
\title{Data Generator}
\usage{
data_generator(b_0, b_treat, icc, v_overall, n_litters, pups_litter)
}
\arguments{
\item{b_0}{intercept (e.g., mean of group one)}

\item{b_treat}{treatment effect (e.g., difference from b_0)}

\item{icc}{total variance explained by litter}

\item{v_overall}{total variance}

\item{n_litters}{number of litters}

\item{pups_litter}{observations per litter (must be even)}
}
\description{
This function allows the user generate data with given characteristics.
}
\examples{

# specify values
b_0 <- 5
b_treat <- 2
icc <- 0.5
v_overall <- 10
n_litters <- 12
pups_litter <- 4
my_data <- data_generator(b_0 = b_0, b_treat = b_treat, 
                   icc = icc, v_overall = v_overall, 
                   n_litters = n_litters, pups_litter = pups_litter
}
