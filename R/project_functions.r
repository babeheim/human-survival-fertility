
library(rethinking)
library(lme4)
library(mgcv)
library(RColorBrewer)
library(knitr)
library(rmarkdown)
library(gdata)
library(sp)
library(RCurl)
library(readxl)
library(dplyr)

set.seed(1854)

save_temp <- TRUE


draw_polygon <- function(factor=10, border="black", col="dodgerblue"){

    output <- list(x=numeric(), y=numeric())
    for(i in 1:1000){

        new_coord <- locator(1)

        if(i > 3){
            average_distance <- mean(sqrt(diff(output$x)^2 + diff(output$y)^2))
            jump <- sqrt( (output$x[i-1] - new_coord$x)^2 + (output$y[i-1] - new_coord$y)^2 )
            if( jump > factor * average_distance ) break()
        }

        output$x <- c(output$x, new_coord$x)
        output$y <- c(output$y, new_coord$y)

        polygon(output, border=border, col=col)
    }
    output
}

download_file <- function(url, dest, overwrite=FALSE, mode="w"){
    if(!file.exists(dest) | overwrite){
        if(url.exists(url)){
            download.file(url, dest, mode=mode)
        } else {
            print("url is not responding")
        }
    } else {
        print("file already exists")
    }
}

capwords <- function(s, strict = FALSE) {
 cap <- function(s) paste(toupper(substring(s,1,1)),
               {s <- substring(s,2); if(strict) tolower(s) else s},
                          sep = "", collapse = " " )
 sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

dir_init <- function(path, verbose=FALSE){
    if(substr(path, 1, 2)!='./') stop('path argument must be formatted
        with "./" at beginning')
    contents <- dir(path, recursive=TRUE)
    if(verbose){
        if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
        if(length(contents)>0) print(paste('folder ', path, ' wiped of ', length(contents), ' files/folders.', sep=""))
    }
    if(dir.exists(path)) unlink(path, recursive=TRUE)
    dir.create(path)
}

gradient_maker <- function(start=NA, stop=NA, cols=c("darkorange", "white", "darkcyan"), vis=FALSE, n=1000){
    if(is.na(start) | is.na(stop)) stop("need to specify start and stop points on a numerical scale")
    colfunc <- colorRampPalette(cols)
    color.list <- colfunc(n)
    color.locations <- seq(start, stop, length=n)
    names(color.locations) <- color.list
    if(vis==TRUE) plot(color.locations, rep(1, n), col=color.list, pch="|", ylim=c(0.9, 1.1), cex=5)
    return(color.locations)
}

data_gradient <- function(data, colors=c("darkorange", "white", "darkcyan"), my.start=NA, my.stop=NA){
    if(is.na(my.start)) my.start <- min(data, na.rm=TRUE)
    if(is.na(my.stop)) my.stop <- max(data, na.rm=TRUE)
    my.gradient <- gradient_maker(start=my.start, stop=my.stop, cols=colors)
    if(any(data > max(my.gradient), na.rm=T) | any(data < min(my.gradient), na.rm=T)) warning("data is not within gradient range")
    data.colors <- rep(NA, length(data))
    for(i in 1:length(data)){
        if(!is.na(data[i])) data.colors[i] <- names(my.gradient)[which.min(abs(data[i]-my.gradient))]
    }
    data.colors
}


precis <-
function (model, depth = 1, pars, ci = TRUE, prob = 0.89, corr = FALSE, 
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

col_alpha <- function (acol, alpha = 0.2){
    acol <- col2rgb(acol)
    acol.red <- acol["red",]/255
    acol.green <- acol["green",]/255
    acol.blue <- acol["blue",]/255
    acol <- mapply(function(red, green, blue, alphas) rgb(red, green, blue, alphas), acol.red, acol.green, acol.blue, alpha)
    return(as.character(acol))
}

su <- function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)

texttab <- function(input.matrix, alignment=NA, hlines=NA, caption="", scale=NA){
    output <- character(nrow(input.matrix))
    for(i in 1:nrow(input.matrix)){
        add.amps <- paste(input.matrix[i,], collapse=" & ")
        output[i] <- paste(add.amps, "\\\\", sep=" ")
    }
    if(all(!is.na(hlines))){
        for(i in 1:length(hlines)) output <- append(output, "\\hline", hlines[i]+(i-1))
    }
    return(output)
}

dir_init <- function(path, verbose=FALSE){
    if(substr(path, 1, 2)!='./') stop('path argument must be formatted
        with "./" at beginning')
    contents <- dir(path, recursive=TRUE)
    if(verbose){
        if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
        if(length(contents)>0) print(paste('folder ', path, ' wiped of ', length(contents), ' files/folders.', sep=""))
    }
    if(dir.exists(path)) unlink(path, recursive=TRUE)
    dir.create(path)
}

module_init <- function(path, verbose=FALSE){
    code_path <- paste(path, '/code', sep='')
    inputs_path <- paste(path, '/inputs', sep='')
    dir_init(path, verbose=verbose)
    dir_init(code_path, verbose=verbose)
    dir_init(inputs_path, verbose=verbose)
}

subdir_clone <- function(from, to){
    if(!file.exists(to)) dir.create(to)
    if(from==to) stop('destination must be different from origin')
    files <- list.files(from, full.names=TRUE, recursive=TRUE)
    new_files <- gsub('^\\.', to, files)
    file_begins <- unlist(lapply(gregexpr('/', new_files), max))
    paths <- unique(substr(new_files, 1, file_begins))
    unlink(paths, recursive=TRUE)
    for(i in 1:length(paths)){
        cuts <- gregexpr('/', paths[i])[[1]]
        for(j in 2:length(cuts)){
            proposal <- substr(paths[i], 1, cuts[j])
            if(!file.exists(proposal)) dir.create(proposal)
        }
    }
    file.copy(files, new_files)
}

col.alpha <- function (acol, alpha = 0.2){
    acol <- col2rgb(acol)
	acol.red <- acol["red",]/255
	acol.green <- acol["green",]/255
	acol.blue <- acol["blue",]/255
	acol <- mapply(function(red, green, blue, alphas) rgb(red, green, blue, alphas), acol.red, acol.green, acol.blue, alpha)
    as.character(acol)
}


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