
#'
#' # Define names of parameters #
#'


# names of parameters for generating fields
names_fields <- c('alpha','sigma_2','tau_2','lambda','size','res')
devtools::use_data(names_fields,overwrite = T)

# names of parameters for sampling measurements from fields
names_meas <- c('I','J','r')
devtools::use_data(names_meas,overwrite = T)

