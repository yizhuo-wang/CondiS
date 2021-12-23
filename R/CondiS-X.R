#' CondiS-X Function
#'
#' This function allows you to improve the imputed survival time by incorporating covariate information.
#' @param pred_time The imputed follow up time for right-censored data.
#' @param status The censoring indicator, normally 0=right censored, 1=event at time.
#' @param covariates The additional patient data that is presumably associated with the survival time.
#' @param method Choose from 8 machine learning algorithms; the default is "glm".
#' @export
#' @examples
#' CondiS(survival_dat)



CondiS_X <- function(pred_time, status, covariates, method) {

  preproc <- caret::preProcess(covariates, method = c('center', 'scale'))
  trainPreProc <- predict(preproc, covariates)

  sdata <- data.frame(pred_time,
                      status, covariates)

  train_control <- caret::trainControl(method = "repeatedcv")

  if (missing(method)) {
    fit_glm = caret::train(
      pred_time ~ .,
      data = sdata,
      method = "glm",
      trControl = train_control
    )

    pred_time_2 = predict(fit_glm, sdata)

    for (i in 1:length(sdata$pred_time))
    {
      if (sdata$status[i] == 1) {
        pred_time_2[i] = sdata$pred_time[i]
      }
    }

  } else if (method == "glm") {
    fit_glm = caret::train(
      pred_time ~ .,
      data = sdata,
      method = "glm",
      trControl = train_control,
      na.action = na.omit
    )

    pred_time_2 = predict(fit_glm, sdata)

    for (i in 1:length(sdata$pred_time))
    {
      if (sdata$status[i] == 1) {
        pred_time_2[i] = sdata$pred_time[i]
      }
    }
  } else if (method == "ridge") {
    fit_ridge = caret::train(
      pred_time ~ .,
      data = sdata,
      method = "glmnet",
      tuneGrid = expand.grid(alpha = 0),
      trControl = train_control,
      na.action = na.omit
    )

    pred_time_2 = predict(fit_ridge, sdata)

    for (i in 1:length(sdata$pred_time))
    {
      if (sdata$status[i] == 1) {
        pred_time_2[i] = sdata$pred_time[i]
      }
    }
  } else if (method == "lasso") {
    fit_lasso = caret::train(
      pred_time ~ .,
      data = sdata,
      method = "glmnet",
      tuneGrid = expand.grid(alpha = 1),
      trControl = train_control,
      na.action = na.omit
    )

    pred_time_2 = predict(fit_lasso, sdata)

    for (i in 1:length(sdata$pred_time))
    {
      if (sdata$status[i] == 1) {
        pred_time_2[i] = sdata$pred_time[i]
      }
    }
  } else if (method == "gbm") {
    fit_gbm = caret::train(
      pred_time ~ .,
      data = sdata,
      method = "gbm",
      trControl = train_control,
      na.action = na.omit
    )

    pred_time_2 = predict(fit_gbm, sdata)

    for (i in 1:length(sdata$pred_time))
    {
      if (sdata$status[i] == 1) {
        pred_time_2[i] = sdata$pred_time[i]
      }
    }
  } else if (method == "rf") {
    mtry <- sqrt(ncol(covariates))

    fit_rf = caret::train(
      pred_time ~ .,
      data = sdata,
      method = "rf",
      tuneGrid = expand.grid(.mtry = mtry),
      trControl = train_control,
      na.action = na.omit
    )

    pred_time_2 = predict(fit_rf, sdata)

    for (i in 1:length(sdata$pred_time))
    {
      if (sdata$status[i] == 1) {
        pred_time_2[i] = sdata$pred_time[i]
      }
    }
  } else if (method == "svm") {
    fit_svm = caret::train(
      pred_time ~ .,
      data = sdata,
      method = "svmRadial",
      trControl = train_control,
      na.action = na.omit
    )

    pred_time_2 = predict(fit_svm, sdata)

    for (i in 1:length(sdata$pred_time))
    {
      if (sdata$status[i] == 1) {
        pred_time_2[i] = sdata$pred_time[i]
      }
    }
  } else if (method == "knn") {
    fit_knn = caret::train(
      pred_time ~ .,
      data = sdata,
      method = "knn",
      trControl = train_control,
      na.action = na.omit
    )

    pred_time_2 = predict(fit_knn, sdata)

    for (i in 1:length(sdata$pred_time))
    {
      if (sdata$status[i] == 1) {
        pred_time_2[i] = sdata$pred_time[i]
      }
    }
  } else if (method == "ann") {


    preproc <- caret::preProcess(sdata, method = 'range')
    trainPreProc <- predict(preproc, sdata)


    sdata$pred_time = pred_time
    sdata$status = status

    fit_ann = caret::train(
      pred_time ~ .,
      data = sdata,
      method = "nnet",
      linout = 1,
      trControl = train_control,
      na.action = na.omit
    )

    pred_time_2 = predict(fit_ann, sdata)

    for (i in 1:length(sdata$pred_time))
    {
      if (sdata$status[i] == 1) {
        pred_time_2[i] = sdata$pred_time[i]
      }
    }
  }
  return(pred_time_2)
}


