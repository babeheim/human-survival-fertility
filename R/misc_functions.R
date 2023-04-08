
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

capwords <- function(s, strict = FALSE) {
 cap <- function(s) paste(toupper(substring(s,1,1)),
               {s <- substring(s,2); if(strict) tolower(s) else s},
                          sep = "", collapse = " " )
 sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

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
