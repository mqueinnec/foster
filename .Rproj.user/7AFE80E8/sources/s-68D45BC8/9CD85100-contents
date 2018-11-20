#landsat metrics for each year
# input can be a path or a raster object
# user can specify the indices or provide custom ones?
# output is a file written to hdd

#ndvi, nbr, evi
# path<- "F:\\TEMP\\foster\\Proxies\\SRef_UTM21S_1984_proxy_v2_crop.tif"
# output_dir <- "F:\\TEMP\\foster\\indices"

calcIndices <- function(in_path,output_dir, overwrite=T) {

  assertive::is_existing_file(in_path)


  r <- brick(in_path)

  ind <- RStoolbox::spectralIndices(r,blue=1, red=3, nir=4,swir3 = 6, scaleFactor = 10000,
                                    indices=c("ndvi","nbri","evi"))

  tascap <- RStoolbox::tasseledCap(r, "Landsat5TM")

  s <- stack(ind,tascap)

  #write to hdd
  if(is.na(output_dir)) {
    if(class(r)[1]== "RasterLayer" || class(r)[1]== "RasterBrick" ) output_dir <- dirname(x@file@name)
    if(class(r)[1]== "RasterStack") output_dir <- dirname(x@layers[[1]]@file@name)
  }
  for (n in names(s)) {
    # print(n)
    output_name <- create_name(r,"tif",paste0("_",n))
    output_image <- paste0(output_dir,"/", output_name)

    writeRaster(eval(parse(text=paste0("s$",n))),output_image, overwrite=overwrite)
  }
}

# calcIndices(in_path = path, output_dir = output_dir)




# calcIndices <- function(x, blue=) {
#
#
#
# }


