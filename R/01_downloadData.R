
library(googledrive)
drive_find(n_max = 30)

getcsv_Files  <- function(driveURL, savepath, mypattern = "csv$", maxfiles = NULL) {

  targetFiles <- drive_ls(driveURL, recursive = F)
  targetFiles_csv <- targetFiles[grep(pattern=mypattern, targetFiles$name, perl = T), ]

  if(is.null(maxfiles)) { maxfiles <- nrow(targetFiles_csv) }

  lapply(1:maxfiles, function(i){
    file_id <- targetFiles_csv$id[i]
    myName <- paste0(targetFiles_csv[i,"name"])
    myName <- gsub("-", "_", myName)
    googledrive::drive_download(as_id(file_id), overwrite = T, path = file.path(savepath, myName ))
  })

}

getcsv_Files(
  "https://drive.google.com/drive/folders/1wgJbMAeibkBeAqzYm0ga7ba5hD3jsW3-",
  savepath = wd$ids,
  mypattern = "__id_"
)

getcsv_Files(
  "https://drive.google.com/drive/folders/1wgJbMAeibkBeAqzYm0ga7ba5hD3jsW3-",
  wd$obs,
  mypattern = "^(?!.*_id_)."
)

getcsv_Files(
  driveURL = "https://drive.google.com/drive/u/2/folders/1fs6tjkG8_NRM9fU5bMzuRTv9mqgTm-0w",
  savepath = wd$tax
)
