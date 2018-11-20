

remove_extension <- function(x){ #copied from tools::file_path_sans_ext
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

create_name <- function(r,output_format="tif",odix="_resampled") {
  if(class(r)[1]== "RasterLayer"|| class(r)[1]== "RasterBrick") input_basename <- remove_extension(basename(r@file@name))
  if(class(r)[1]== "RasterStack") input_basename <- remove_extension(basename(r@layers[[1]]@file@name))
  paste0(input_basename, odix, ".",output_format)

}



