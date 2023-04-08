
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
