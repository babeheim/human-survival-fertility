
rm(list=ls())

library(jsonlite)

nullToNA <- function(x) {
    x[sapply(x, is.null)] <- NA
    return(x)
}

id_maker <- function(n, reserved='', seed=NA, nchars=NA){
    my_let <- letters 
    my_num <- 0:9 
    # if(is.na(seed) | !is.numeric(seed)) set.seed(rnorm(1))
    if(!is.na(seed) & is.numeric(seed)) set.seed(seed)
    output <- replicate(n, paste(sample(c(my_let, my_num), nchars, replace=TRUE), 
        collapse=''))
    rejected <- duplicated(output) | output %in% reserved | substr(output, 1, 1) %in% my_num
    while(any(rejected)){
        output <- output[-which(rejected)]
        remaining <- n-length(output)
        output <- c(output, replicate(remaining, paste(sample(c(my_let, my_num), nchars, 
            replace=TRUE), collapse='')))
        rejected <- duplicated(output) | output %in% reserved | substr(output, 1, 1) %in% my_num
    }
    output
}



df_list_to_dataframe <- function( input_list ){
    for(i in 1:ncol(input_list)){
        if(class(input_list[,i])=="list"){
            empty <- which(lapply(input_list[,i], class)=="list")
            if(length(empty)>0) input_list[empty,i] <- NA
            input_list[,i] <- unlist(nullToNA(input_list[,i]))
        }
    }
    input_list
}

list_to_dataframe <- function( input_list ){

    column_names <- unique(unlist(lapply(input_list, names)))

    output <- matrix( "", nrow = length(input_list), ncol=length(column_names) )
    colnames(output) <- column_names
    output <- as.data.frame( output )
    for(j in 1:ncol(output)) output[,j] <- as.character(output[,j]) 

    for(i in 1:length(input_list)){

        new_record <- unlist(input_list[[i]])
        output[i, names(new_record)] <- new_record 

    }

    output

}

rbind_plus <- function( df1, df2 ){

    df1 <- do.call(data.frame, df1)
    df2 <- do.call(data.frame, df2)

    n1 <- colnames(df1)
    n2 <- colnames(df2)

    if( any(!n1 %in% n2) ){

        new_cols <- n1[!n1 %in% n2]
        df2 <- cbind(df2, matrix("", nrow=nrow(df2), ncol=length(new_cols)))
        colnames(df2) <- c(n2, new_cols)

    }

    if( any(!n2 %in% n1) ){

        new_cols <- n2[!n2 %in% n1]
        df1 <- cbind(df1, matrix("", nrow=nrow(df1), ncol=length(new_cols)))
        colnames(df1) <- c(n1, new_cols)

    }

    output <- rbind(df1, df2)
    output

}

new <- read_json("gurven-kaplan-2007-data.json", simplifyVector=TRUE) # works but only as seperate files...

new <- df_list_to_dataframe(new)


new$l15 <- as.numeric(new$l15)
new$e0 <- as.numeric(new$e0)
m1 <- lm(e0 ~ l15, data=new)
abline(m1) # approximate at least

new$e0_est <- coef(m1)[1] + coef(m1)[2]*new$l15

new$e0[is.na(new$e0)] <- new$e0_est[is.na(new$e0)] 

write.csv(new, "./gurven-kaplan-2007-data.csv", row.names=FALSE)
