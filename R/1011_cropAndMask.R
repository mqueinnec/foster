#clip ntems

# r_ntems <- raster("Z:\\LandsatExtrapolationProject\\NTEMS\\HarrysRiver\\VLCE")
# in_raster <- raster("Z:\\LandsatExtrapolationProject\\NTEMS\\HarrysRiver\\DEM\\UTM21S_DEM.dat")

# ref_raster <- raster("O:\\TEMP\\foster\\ABA_rasters\\HarrysRiver_H_ABA_resampled.tif")


match_to_aba <- function(in_raster,
                         reference_raster,
                         output_dir=NA,
                         output_name=NA,
                         output_format="tif",
                         overwrite=TRUE) {

  #check if input reference image is raster or rasterstack
  if(!class(reference_raster)[1]== "RasterLayer") reference_raster <- raster(reference_image)

  #if in_raster is not a rasterlayer or stack the read it from disk as stack
  if(!class(in_raster)[1]== "RasterLayer" || !class(in_raster)[1]== "RasterStack") in_raster <- stack(in_raster)


  out <- crop(in_raster, reference_raster) #crop
  out <- mask(out, reference_raster)       #mask

  #write output
  #if output name is not provided, then write the resampled image with modified filename
  if(is.na(output_name)) {
    output_name <- create_name(r=in_raster, output_format = "tif",odix="_crop")
  }

  #if output_dir is NA then write image in the same dir
  if(is.na(output_dir)) {
    if(class(r)[1]== "RasterLayer") output_dir <- dirname(in_raster@file@name)
    if(class(r)[1]== "RasterStack") output_dir <- dirname(in_raster@layers[[1]]@file@name)
  }

  #construct output_image based on output_dir and output_name
  output_image <- paste0(output_dir,"/", output_name)

  writeRaster(out,output_image, overwrite=overwrite)
}


#
# match_to_aba(in_raster=raster("O:\\TEMP\\foster\\VLCE\\LC_Class_HMM_21S_1984.dat"), ref_raster)
# match_to_aba(in_raster="O:\\TEMP\\foster\\Proxies\\SRef_UTM21S_1985_proxy_v2.dat", ref_raster)
# match_to_aba(in_raster=stack("O:\\TEMP\\foster\\Proxies\\SRef_UTM21S_1985_proxy_v2.dat"), ref_raster)
#

