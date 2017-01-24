######
# vertical datum conversion to LMSL (local mean sea level)
# all need to be converted except IRL
# tampa bay is nadv88
# all others are mllw from USGS bathymetry
# 
# Original files are processed in sg_depth project, copied to this project, than manually processed with the vdatum program (http://vdatum.noaa.gov/welcome.html).  Output from vdatum is then copied back into the project. 
# 
# sgpts_2010_902.RData (Old Tampa Bay)
# 
#   Original File: M:\GIS\seagrass\sgpts_2010_902.shp
# 	
# 	Geographic Coordinate System:	GCS_North_American_1983
# 	Datum: 	D_North_American_1983
# 	
# 	Vertical datum: NAVD88
# 
# 	columns (lon, lat, height): 2, 3, 0
# 
# 
# sgpts_2007_303.RData (Choctowhatchee)
# 
# 	Original File: M:\GIS\seagrass\sgpts_2007_303.shp
# 
# 	Geographic Coordinate System:	GCS_North_American_1983
# 	Datum: 	D_North_American_1983
# 
# 	Vertical datum: MLLW
# 
# 	columns (lon, lat, height): 2, 3, 0
# 
# 
# sgpts_2006_820.RData (Big Bend)
# 
# 	Original file: M:\GIS\seagrass\sgpts_2006_820.shp
# 
# 	Geographic Coordinate System:	GCS_North_American_1983
# 	Datum: 	D_North_American_1983
# 
# 	Vertical datum: MLLW
# 
# 	columns (lon, lat, height): 2, 3, 0
# 
# sgpts_2010_tb.RData
# 
# 	Original file: M:\GIS\seagrass\sgpts_2010_tb.shp
# 
# 	Geographic Coordinate System:	GCS_North_American_1983
# 	Datum: 	D_North_American_1983
# 
# 	Vertical datum: NADV88
# 
# 	columns (lon, lat, height): 2, 3, 0	

##
# individual segments

data(shps)
segs_to_conv <- c('sgpts_2006_820.shp', 'sgpts_2007_303.shp', 'sgpts_2010_902.shp')
tmp <- shps[segs_to_conv]

for(i in names(tmp)){
    
  out <- data.frame(tmp[[i]])
  nm <- gsub('.shp', '.txt', i)
  write.table(out, paste0('C:/Users/mbeck/Desktop/', nm), 
    sep=',', quote = F, row.names = F)
        
  }

# all Tampa Bay

data(sgpts_2010_tb)
write.table(data.frame(sgpts_2010_tb), 'C:/Users/mbeck/Desktop/sgpts_2010_tb.txt', sep =',', quote = F, row.names = F)

# all Choctawhatchee Bay

data(sgpts_2007_choc)
write.table(data.frame(sgpts_2007_choc), 'C:/Users/mbeck/Desktop/sgpts_2007_choc.txt', sep =',', quote = F, row.names = F)


##
# then process...
#
# open vdatum.jar from M:/GIS/vdatum
#
# specify horizontal and vertical datum of source and target file
# specify columns of height, long, and lat data (0 is first column)
# specify output file, then convert

##
# import results and save to original object

rm(list = ls())

##
# only use this if processing segs 820, 303, 902
# path with results
res_path <- 'C:/Users/mbeck/Desktop/result/'

# import new results with lmsl datum correction
segs_to_conv <- c('sgpts_2006_820.shp', 'sgpts_2007_303.shp', 'sgpts_2010_902.shp')
segs_to_save <- gsub('\\.shp', '\\.txt', segs_to_conv)
segs_to_save <- c(segs_to_save, 'sgpts_2010_tb.txt')
out_ls <- vector('list', length(segs_to_save))
names(out_ls) <- segs_to_save
for(fl in segs_to_save){
  
  tmp <- read.table(paste0(res_path, fl), header = T, sep = ',', na.strings = '-999999')
  cat(sum(is.na(tmp$Depth)), '\t')
  cat(nrow(tmp), '\n')
  tmp <- tmp[!is.na(tmp$Depth), ]
  tmp[tmp$Depth < 0, 'Depth'] <- 0
  coordinates(tmp) <- c('coords.x1', 'coords.x2')
  out_ls[[fl]] <- tmp
  
}

# replace rdata files with corrected 
data(shps)
old_shps <- shps

shps[['sgpts_2007_303.shp']] <- out_ls[['sgpts_2007_303.txt']]
shps[['sgpts_2006_820.shp']] <- out_ls[['sgpts_2006_820.txt']]
shps[['sgpts_2010_902.shp']] <- out_ls[['sgpts_2010_902.txt']]

save(shps, file = 'data/shps.RData')

file.remove('data/sgpts_2010_tb.RData')
sgpts_2010_tb <- out_ls[['sgpts_2010_tb.txt']]
save(sgpts_2010_tb, file = 'data/sgpts_2010_tb.RData')

##
# use this if processing individual files different from above
res_path <- 'C:/Users/mbeck/Desktop/result/'
fl <- 'sgpts_2007_choc.txt'
tmp <- read.table(paste0(res_path, fl), header = T, sep = ',', na.strings = '-999999')
names(tmp) <- c('Depth', 'Seagrass', 'coords.x1', 'coords.x2')
cat(sum(is.na(tmp$Depth)), '\t')
cat(nrow(tmp), '\n')
tmp <- tmp[!is.na(tmp$Depth), ]
tmp[tmp$Depth < 0, 'Depth'] <- 0
coordinates(tmp) <- c('coords.x1', 'coords.x2')

sgpts_2007_choc <- tmp
save(sgpts_2007_choc, file = 'data/sgpts_2007_choc.RData')

  
  
  