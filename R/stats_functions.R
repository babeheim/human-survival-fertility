

precis <- function (model, depth = 1, pars, ci = TRUE, prob = 0.89, corr = FALSE, 
    digits = 2, warn = TRUE, spark = FALSE, messages = TRUE) 
{
    the.class <- class(model)[1]
    found.class <- FALSE
    if (the.class == "numeric") {
        model <- as.data.frame(model)
        the.class <- class(model)[1]
    }
    if (any(precis.whitelist$class == the.class)) 
        found.class <- TRUE
    if (the.class == "list") 
        if (class(model[[1]]) != "mcarray") 
            found.class <- FALSE
    if (found.class == TRUE) {
        est <- rethinking:::xcoef(model)
        se <- rethinking:::xse(model)
        if (corr == TRUE) 
            Rho <- xrho(model)
    }
    if (found.class == FALSE & messages == TRUE) {
        message(paste("No handler found for model of class", 
            the.class))
        return(invisible())
    }
    fname <- "Mean"
    fname <- concat(toupper(substring(fname, 1, 1)), substring(fname, 
        2))
    result <- data.frame(est = est, se = se)
    colnames(result) <- c(fname, "StdDev")
    if (ci == TRUE) {
        ci <- confint_quad(est = est, se = se, prob = prob)
        if (the.class == "data.frame") {
            ci <- t(apply(model, 2, HPDI, prob = prob))
        }
        result <- cbind(result, ci)
        if (the.class == "map2stan") {
            post <- extract.samples(model)
            result <- postlistprecis(post, prob = prob, spark = spark)
        }
        if (the.class == "stanfit") {
            post <- extract.samples(model)
            post[["lp__"]] <- NULL
            result <- postlistprecis(post, prob = prob, spark = spark)
        }
    }
    if (the.class == "map2stan" | the.class == "stanfit") {
        if (the.class == "map2stan") 
            the_summary <- summary(model@stanfit)$summary
        else the_summary <- summary(model)$summary
        n_eff <- the_summary[, "n_eff"]
        n_eff <- n_eff[-which(names(n_eff) == "lp__")]
        Rhat <- the_summary[, "Rhat"]
        Rhat <- Rhat[-which(names(Rhat) == "lp__")]
        if (the.class == "map2stan") {
            n_eff <- n_eff[-which(names(n_eff) == "dev")]
            Rhat <- Rhat[-which(names(Rhat) == "dev")]
        }
        result <- cbind(result, n_eff, Rhat)
        nd <- divergent(model)
        if (nd > 0 & warn == TRUE & messages == TRUE) {
            warning(concat("There were ", nd, " divergent iterations during sampling.\nCheck the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid."))
        }
    }
    if (corr == TRUE) {
        result <- cbind(result, Rho)
    }
    if (precis.whitelist$vcov.method[precis.whitelist$class == 
        the.class] == "vcov.VarCorr" & messages == TRUE) {
        message("Quadratic approximation (standard errors) unreliable for variance components. Use MCMC to estimate precision of variance components.")
    }
    if (depth == 1) {
        hits <- regexpr("]", rownames(result), fixed = TRUE)
        hits_idx <- which(hits > -1)
        if (length(hits_idx) > 0 & messages == TRUE) {
            result <- result[-hits_idx, ]
            message(paste(length(hits_idx), "vector or matrix parameters omitted in display. Use depth=2 to show them."))
        }
    }
    if (!missing(pars)) {
        clean_names <- as.character(sapply(rownames(result), 
            function(s) strsplit(s, "[", fixed = TRUE)[[1]][1]))
        result <- result[clean_names %in% pars, ]
    }
    new("precis", output = result, digits = digits)
}

prob_abs_0 <- function(z) min(mean(z<0), mean(z>0))


su <- function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)

contour.ci <- function(chain1, chain2, my.col, levels=0.95){

    require(MASS)
    require(cluster)

    chain.matrix <- cbind(chain1, chain2)

    if(length(levels)==1){
        fit <- cov.mve(chain.matrix, quantile.used=nrow(chain.matrix)*levels, nsamp=10000)

        points_in_ellipse <- as.matrix(chain.matrix[fit$best, ])
        ellipse_boundary <- predict(ellipsoidhull(points_in_ellipse))
        polygon(ellipse_boundary, col=col.alpha(my.col, 0.1), border=NA)
    }
    if(length(levels)>1){
    
        for(i in 1:length(levels)) contour.ci(chain1, chain2, my.col, levels=levels[i])
    
    }
    
}

modal <- function(data){
    mode <- NA
    if(length(data) > 0 & !all(is.na(data))){
        mode <- names(sort(table(data),decreasing=T))[1]
        options(warn=-1)
        if(!is.na(as.numeric(mode))){
        mode <- as.numeric(mode)
        }
        options(warn=0)
    }
    return(mode)
}