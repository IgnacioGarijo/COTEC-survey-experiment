function (optimizer, fn, par, lower = -Inf, upper = Inf, control = list(), 
    adj = FALSE, calc.derivs = TRUE, use.last.params = FALSE, 
    verbose = 0L) 
{
    optfun <- getOptfun(optimizer)
    optName <- if (is.character(optimizer)) 
        optimizer
    else deparse(substitute(optimizer))[[1L]]
    lower <- rep_len(lower, length(par))
    upper <- rep_len(upper, length(par))
    if (adj) 
        switch(optName, bobyqa = {
            if (!is.numeric(control$rhobeg)) control$rhobeg <- 2e-04
            if (!is.numeric(control$rhoend)) control$rhoend <- 2e-07
        }, Nelder_Mead = {
            if (is.null(control$xst)) {
                thetaStep <- 0.1
                nTheta <- length(environment(fn)$pp$theta)
                betaSD <- sqrt(diag(environment(fn)$pp$unsc()))
                control$xst <- 0.2 * c(rep.int(thetaStep, nTheta), 
                  pmin(betaSD, 10))
            }
            if (is.null(control$xt)) control$xt <- control$xst * 
                5e-04
        })
    switch(optName, bobyqa = {
        if (all(par == 0)) par[] <- 0.001
        if (!is.numeric(control$iprint)) control$iprint <- min(verbose, 
            3L)
    }, Nelder_Mead = control$verbose <- verbose, nloptwrap = control$print_level <- min(as.numeric(verbose), 
        3L), if (verbose) warning(gettextf("'verbose' not yet passed to optimizer '%s'; consider fixing optwrap()", 
        optName), domain = NA))
    arglist <- list(fn = fn, par = par, lower = lower, upper = upper, 
        control = control)
    if (optName == "optimx") {
        if (is.null(method <- control$method)) 
            stop("must specify 'method' explicitly for optimx")
        arglist$control$method <- NULL
        arglist <- c(arglist, list(method = method))
    }
    curWarnings <- list()
    opt <- withCallingHandlers(do.call(optfun, arglist), warning = function(w) {
        curWarnings <<- append(curWarnings, list(w$message))
    })
    if (optName == "bobyqa") {
        opt$convergence <- opt$ierr
    }
    else if (optName == "Nelder_Mead") {
        if (opt$NM.result == 4) 
            opt$convergence <- 4
    }
    else if (optName == "optimx") {
        opt <- list(par = coef(opt)[1, ], fvalues = opt$value[1], 
            method = method, conv = opt$convcode[1], feval = opt$fevals + 
                opt$gevals, message = attr(opt, "details")[, 
                "message"][[1]])
    }
    if ((optconv <- getConv(opt)) != 0) {
        wmsg <- paste("convergence code", optconv, "from", optName)
        if (!is.null(getMsg(opt))) 
            wmsg <- paste0(wmsg, ": ", getMsg(opt))
        warning(wmsg)
        curWarnings <<- append(curWarnings, list(wmsg))
    }
    if (calc.derivs) {
        if (use.last.params) {
            orig_pars <- opt$par
            orig_theta <- environment(fn)$pp$theta + 0
            orig_pars[seq_along(orig_theta)] <- orig_theta
        }
        if (verbose > 10) 
            cat("computing derivatives\n")
        derivs <- deriv12(fn, opt$par, fx = opt$value)
        if (use.last.params) {
            fn(orig_pars)
        }
    }
    else derivs <- NULL
    if (!use.last.params) {
        fn(opt$par)
    }
    structure(opt, optimizer = optimizer, control = control, 
        warnings = curWarnings, derivs = derivs)
}