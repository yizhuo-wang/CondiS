#' CondiS Function
#'
#' This function allows you to impute survival time.
#' @param time The follow up time for right-censored data.
#' @param status The censoring indicator, normally 0=right censored, 1=event at time.
#' @param tmax A self-defined time-of-interest point; if left undefined, then it is defaulted as the maximum follow up time.
#' @export

CondiS <- function(time, status, tmax) {
  id <- c(1:length(time))
  sdata <- data.frame(id, time,
                      status)

  fit = survival::Surv(time, status) ~ 1
  curve = survival::survfit(fit, data = sdata)
  rmst = rep(NA, nrow(sdata))
  sdata = sdata[order(sdata$time), ]

  true_ext = summary(curve, times = sdata$time)

  if (missing(tmax)) {
    for (i in 1:nrow(sdata))
    {
      func = approxfun(true_ext$time, true_ext$surv, ties = min)
      l = sdata$time[i]
      u = range(curve$time)[2]

      if (sdata$status[i] == 0) {
        if (l > u) {
          rmst[i] = l
        } else{
          intvalue = integrate(func,
                               l,
                               u,
                               subdivisions = 2000,
                               stop.on.error = FALSE)$value
          rmst[i] = sdata$time[i] + intvalue / true_ext$surv[i]
        }
      } else {
        rmst[i] = sdata$time[i]
      }
    }

  } else {
    for (i in 1:nrow(sdata))
    {
      func = approxfun(true_ext$time, true_ext$surv, ties = min)
      l = sdata$time[i]
      u = tmax

      if (sdata$status[i] == 0) {
        if (l > u) {
          rmst[i] = l
        } else{
          intvalue = integrate(func,
                               l,
                               u,
                               subdivisions = 2000,
                               stop.on.error = FALSE)$value
          rmst[i] = sdata$time[i] + intvalue / true_ext$surv[i]
        }
      } else {
        rmst[i] = sdata$time[i]
      }
    }


  }

  sdata$rmst = rmst
  sdata = sdata[order(sdata$id), ]
  return(c(sdata$rmst))
}

