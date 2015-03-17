# Seagrass depth of colonization

Materials for manuscript describing seagrass depth of colonization estimates using a spatially-referenced approach.

# Data

`buff_ex_dat.RData` Shapefile data for buff_ex figure as list, includes five shapefiles

`cb_sats.RData` Two element list including raster image of all averaged clarity data and data frame of each year of clarity data from satellite images for Choctawhatchee Bay 2003-2007, processed in sg_depth.RProj

`choc_seg.RData` WBIDs for Choctawhatchee Bay that included seagrass depht points as polygon shapefile.

`dep_shps.RData` Bathymetry data shapefiles for each segment

`ests_out.RData` Seagrass depth of colonization estimates for grids in each segment

`ests_out_nonsigs.RData` Same as `ests_out.RData` but includes zcmax values with confidence less than zero.  Only used in code chunk for results. 

`ests_seg.RData` Seagrass depth of colonization estimates for entire segments (large radius)

`irl_light.RData` Seagrass light requirements for Indian River Lagoon, slightly modified output from `secchi_doc` function, created in `light_irl` chunk

`irl_seg.RData` Shapefile polygon of Indian River Lagoon, all segments

`secc_all.RData` Secchi data for all of Florida from IWR40 database, created in sg_depth.RProj

`secc_sum.RData` Summary of secchi observations by bay and segment from IWR40 database, also includes light requirement summaries from `irl_light.RData` and `tb_light.RData`

`seg_summ.RData` Summarized data for table of segment characteristics

`sg_shps.RData` Shapefiles of seagrass coverage polygons for each segment

`sgpts_2009_irl.RData` All seagrass depth points for Choctawhatchee Bay (all segments)

`sgpts_2009_irl.RData` All seagrass depth points for Indian River Lagoon (all segments)

`sgpts_2010_tb.RData` All seagrass depth points for Tampa Bay (all segments)

`shps.RData` Multiple shapefiles for each segment, including all segment polygons, individual segment polygons, and seagrass depth points for each segment

`tb_light.RData` Seagrass light requirements for Tampa Bay, slightly modified output from `secchi_doc` function, created in `light_tb` chunk

`tb_sats.RData` Two element list including raster image of all averaged clarity data and data frame of each year of clarity data from satellite images for Tampa Bay 2003-2010, processed in sg_depth.RProj

`tb_seg.RData` Shapefile polygon of Tampa Bay, all segments
