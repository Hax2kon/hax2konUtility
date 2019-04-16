

#' Make data.frame from regression models for easy export.
#'
#' This is a function to create data frame out of regression results.
#' @keywords tables export
#' @export
#' @examples
#' results_frame()


######    \\    --    results_frame()   --    //   #####
#This function produces a data.frame of regression results ready to be printed.


results_frame <- function(..., standard_errors=NULL, t.values=FALSE, digits=2, omit.coef=NULL, omit.coef.label=NULL,
                          omit.stat=c("R-squared", "DF"), stars=TRUE,
                          intercept.placement="bottom", covariate.labels=NULL, intercept.label="Constant"){
  library(dplyr);library(tidyr)
  #source("force_bind.R")

  #####     \\    Fix main part of frame      //      #####

  ##### Extract coefficients  #####
  betalist <- lapply(list(...), function(x) coef(x))

  ##### Extract covariate names  #####
  namelist <- lapply(list(...), function(x) names(coef(x)))


  ##### Extract standard errors #####
  ##### TODO: Add possibility to have robust/clustered standard errors
  if(is.null(standard_errors)){
    uncertainty <- lapply(list(...), function(x) sqrt(diag(vcov(x))))

  } else if(length(list(...))!=length(standard_errors)){
    warning("List of standard errors is not of equal length as the number of models.")

  } else{
    uncertainty <- standard_errors
  }

  #####   Change to T-values if desired   #####
  tval <- mapply(FUN = "/", betalist, uncertainty, SIMPLIFY = FALSE)
  if(t.values){
    uncertainty <- tval
  }

  #####   Significance level      #####
  if(stars){
    sig <- lapply(tval, function(x) ifelse(abs(x) >=3.3, "***",
                                           ifelse(abs(x) >=2.58, "**",
                                                  ifelse(abs(x) >=1.96, "*", ""
                                                  ))))
  } else if(!stars){
    sig <- lapply(tval, function(x) ifelse(abs(x) >=0, "", ""))
  }


  ##### Connect Coefficient and uncertainty     #####
  res <- mapply(function(name, x, y, z) cbind(name, paste0(round(x, digits), y, ";(", round(z, digits), ")")), name=namelist, x=betalist, y=sig, z=uncertainty, SIMPLIFY = FALSE)


  #####   Separate into rows    #####
  res <- lapply(res, function(x) separate_rows(data.frame(x), 2, sep=";"))
  newnames <- lapply(res, function(x) ifelse(duplicated(x[,"name"]), paste0(x[, "name"], "_se"), as.character(x[,"name"])))
  res <- mapply(function(name, x) data.frame("names"=name, "res"=x[, 2], stringsAsFactors = FALSE), name=newnames, x=res, SIMPLIFY = FALSE)


  #####   Make frame of main results  ####
  res <- Reduce(function(...) full_join(..., by = c("names"), all=TRUE), res)
  colnames(res) <- c("Covariate", paste0("Model ", 1:(ncol(res)-1)))



  #####   Omit covariates   #####

  if(!is.null(omit.coef) & is.null(omit.coef.label)){
    omitted <- lapply(omit.coef, function(x) c(x, if_else( apply( res[grep(x, res$Covariate),], 2, function(y) all(is.na(y)))[2:ncol(res)], "No", "Yes") ) )
    res <- res[grep(paste0(omit.coef, collapse = "|"), res$Covariate, invert = TRUE),]

  } else if(!is.null(omit.coef) & !is.null(omit.coef.label) & length(omit.coef)!=length(omit.coef.label)){
    stop("When used, omit.coef and omit.coef.label must be vectors of same length. Alternatively, omit.coef.label can be omitted. In this case, the labels from omit.coef will be used.")

  } else if(!is.null(omit.coef) & !is.null(omit.coef.label)){
    omitted <- mapply(function(x, y) c(y, if_else( apply( res[grep(x, res$Covariate),], 2, function(y) all(is.na(y)))[2:ncol(res)], "No", "Yes") ), x=omit.coef, y=omit.coef.label, SIMPLIFY=FALSE )
    res <- res[grep(paste0(omit.coef, collapse = "|"), res$Covariate, invert = TRUE),]
  }


  #####   Change position of intercept and add covariate labels     #####
  if(!intercept.placement %in%  c("bottom", "top")){
    warning("intercept.placement should be either 'bottom' or 'top'")
  } else if(intercept.placement=="bottom"){
    res <- rbind(res[grep("Intercept|Constant", x=res$Covariate, invert=TRUE, ignore.case = TRUE),], res[grep("Intercept|Constant", x=res$Covariate, ignore.case = TRUE),])
  }


  #####   Add covariate labels     #####
  if(!is.null(covariate.labels) & (length(res$Covariate[grep("_se$", res$Covariate, invert=TRUE)])-1)!=length(covariate.labels)){
    stop("covariate.labels is not of same length as number of covariates.")
    }

  if(!is.null(covariate.labels) & intercept.placement=="bottom"){
    res$Covariate[grep("_se$", res$Covariate, invert=TRUE)] <- c(covariate.labels, intercept.label)
  } else if(!is.null(covariate.labels) & intercept.placement=="top"){
    res$Covariate[grep("_se$", res$Covariate, invert=TRUE)] <- c(intercept.label, covariate.labels)
  }

  #####   \\    Model statistics    //    #####
  N        <- c("N", sapply(list(...), function(x) length(resid(x))))
  adj_r    <- c("Adj. R-squared", sapply(list(...), function(x) ifelse(is.null(summary(x)$adj.r.squared), "", as.character(round(summary(x)$adj.r.squared, digits)))))
  r_sq     <- c("R-squared", sapply(list(...), function(x) ifelse(is.null(summary(x)$r.squared), "", as.character(round(summary(x)$r.squared, digits)))))
  dfree    <- c("DF", sapply(list(...), function(x) ifelse(is.null(x$df.residual), "", as.character(round(x$df.residual, digits)))))
  modtype  <- c("Model type", sapply(list(...), function(x) class(x)[1]))
  ll       <- c("LL", sapply(list(...), function(x) ifelse("glm" %in% class(x), as.character(as.numeric(round(logLik(x), digits))) , "")))

  #Combine model statistics
  modstats <- data.frame(rbind(N, modtype, adj_r, r_sq, dfree, ll), stringsAsFactors = FALSE)

  #Remove model statistics that should be omitted
  modstats <- modstats[!modstats[,1] %in% omit.stat,]

  #Remove model statistics that are irrelevant
  modstats <- modstats[apply(modstats[,2:ncol(modstats)], 1, function(x) paste0(x, collapse=""))!="",]


  #####   \\    Aesthetics    //    #####

  #####     Make NA empty    #####
  res[is.na(res)] <- ""

  #####   Remove _se-names    #####
  res$Covariate[grep("_se", res$Covariate)] <- ""


  #####   Add omitted to bottom   #####
  if(exists("omitted")){
    res <- force_bind(res, data.frame(do.call("rbind", omitted), stringsAsFactors = FALSE))
  }

  #####   Add model statistics   #####
  res <- force_bind(res, modstats)

  #Return
  return(res)

}

