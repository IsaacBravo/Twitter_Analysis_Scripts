download_images_esp <- function(data) {
  
  list_images <- data %>% 
    select(id_original, date, url_image) %>% 
    mutate(id = paste0(id_original, "_", date))
  
  downloadPath = paste0(getwd(),"./data_IMG_2021_spa/08_August/")
  
  # Define title of each image file
  title_image = c()
  for (i in 1:nrow(list_images)) {
    title_image[i] = paste0("id_",list_images$id[i])
  }
  
  for (i in 1:nrow(list_images)){
    try(
      download.file(list_images$url_image[i], 
                    destfile = paste0(downloadPath, title_image[i],".jpg"),
                    mode = 'wb'),
      silent = TRUE)
    Sys.sleep(1)
  }
}

download_images_ger <- function(data) {
  
  list_images <- data %>% 
    select(id_original, date, url_image) %>% 
    mutate(id = paste0(id_original, "_", date))
  
  downloadPath = paste0(getwd(),"./data_IMG_2020_ger/12_December/")
  
  # Define title of each image file
  title_image = c()
  for (i in 1:nrow(list_images)) {
    title_image[i] = paste0("id_",list_images$id[i])
  }
  
  for (i in 1:nrow(list_images)){
    try(
      download.file(list_images$url_image[i], 
                    destfile = paste0(downloadPath, title_image[i],".jpg"),
                    mode = 'wb'),
      silent = TRUE)
    Sys.sleep(1)
  }
}

