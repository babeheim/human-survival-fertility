
library(yaml)

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

files <- list.files(".", pattern="*.yaml$", full.names=TRUE)

file_counter <- 0

for(j in 1:length(files)){
    x <- readLines( files[j] )
    x <- gsub("\"", "'", x)
    for(i in 1:length(x)){
        if( length(grep(": ", x[i])) > 0 & length(grep(": $", x[i])) == 0 ){
            x[i] <- gsub(": ", ": \"", x[i])
            x[i] <- gsub("$", "\"", x[i])
        }
    }
    new <- yaml.load(paste(x, collapse="\n"))
    output <- jsonlite::toJSON(new, pretty=TRUE)
    newname <- gsub("yaml", "json", files[j])
    writeLines(output, newname)
    file_counter <- file_counter+1
}

print( paste0( file_counter, " files have been converted") )

