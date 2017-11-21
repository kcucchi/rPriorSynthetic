

#'generate fields from parameters
#'
#'@param vect_param a vector containing parameters for fields generation
#'@param nb_fields an integer specifying the number of fields to generate
#'@param verbose boolean for whether to display comments
#'@return a dataframe where each column corresponds to generated field
#'@export
generate_fields <- function(vect_param,nb_fields = 100,verbose=F){

  # this will contains all simulations
  # generate grid
  Y_xy <-
    expand.grid(x=seq(from=0,
                      to=vect_param[['size']],
                      by=vect_param[['res']]),
                y=seq(from=0,
                      to=vect_param[['size']],
                      by=vect_param[['res']]))

  # generate vector of site-specific means
  vect_mu <- rnorm(n = nb_fields,
                   mean = vect_param[['alpha']],
                   sd = sqrt(vect_param[['tau_2']]))
  # hist(vect_mu)
  # generate vector of site-specific variances
  vect_sigma_2 <-
    rep(x = vect_param[['sigma_2']],
        nb_fields)

  for(i in 1:nb_fields){

    if(verbose){cat(paste0(i,'/',nb_fields))}

    g.dummy_i <-
      gstat::gstat(formula=z~1,
            locations=~x+y, dummy=T,
            beta=vect_mu[i],
            model=vgm(psill = 0.5 * vect_sigma_2[i],
                      range=vect_param[['lambda']],
                      model='Exp',
                      nugget = 0.04),
            nmax=20)

    # generate I fields
    # unconditional Gaussian simulations
    yy <- gstat::predict(g.dummy_i, newdata=Y_xy, nsim=1)
    # str(yy)

    # append to main variable
    Y_xy <- dplyr::left_join(x = Y_xy,y = yy, by = c("x","y"))

    # rename column sim1 to Si
    names(Y_xy)[which(names(Y_xy) == "sim1")] <- paste0("S",i)

  }

  return(Y_xy)


}



#'sample measurement from generated fields
#'
#'@param vect_param a vector containing parameters for sampling measurements
#'@param Y_xy original fields
#'@param verbose boolean for whether to display comments
#'@return a dataframe containing measurements
#'@export
generate_meas <- function(vect_meas,Y_xy,verbose=F){

  # this will contain measurement values at all sites
  Y_meas <-
    data.frame(id_meas=numeric(0),
               site_id=numeric(0),
               val=numeric(0),
               x=numeric(0),
               y=numeric(0))


  # randomly sample sites

  site_names <-
    sample(names(Y_xy)[grep(pattern = '^S',
                            x = names(Y_xy))],
           vect_meas[['I']])

  for(i_site in 1:vect_meas[['I']]){

    ### generate measurement locations

    res <- diff(sort(unique(Y_xy$x))[1:2])
    size <- max(unique(Y_xy$x))

    ## uniform sampling within circle
    # sample distance from center
    rho <- sqrt(runif(vect_meas[['J']]))
    # sample angles
    theta <- runif(vect_meas[['J']], 0, 2*pi)
    # transform to x,y at res solution
    Y_meas_i_site <- data.frame(
      x = res *
        round( (size / 2 +
                  vect_meas[['r']] * rho * cos(theta)) / res ),
      y = res *
        round( (size / 2 +
                  vect_meas[['r']] * rho * sin(theta)) / res ),
      id_meas = 1:vect_meas[['J']],
      site_id = site_names[i_site]
    )

    # merge measurement values at x and y
    Y_meas_i_site <-
      dplyr::left_join(
        x = Y_meas_i_site,
        y = Y_xy[,c("x","y",site_names[i_site])],
        by = c("x","y"))

    # change name of value field
    names(Y_meas_i_site)[which(names(Y_meas_i_site)==site_names[i_site])] <- 'val'

    # shuffle columns
    Y_meas_i_site <- Y_meas_i_site[,names(Y_meas)]

    # append to main dataframe
    Y_meas <- rbind(Y_meas,Y_meas_i_site)

    rm(Y_meas_i_site)

  } # end loop for sites

  return(Y_meas)

}



