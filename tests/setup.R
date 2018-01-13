if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
lodown( "ahrf" , output_dir = file.path( getwd() ) )
ahrf_df <- readRDS( file.path( getwd() , "county/AHRF_2016-2017.rds" ) )

ahrf_df <- 
	transform( 
		ahrf_df , 
		
		cbsa_indicator_code = 
			factor( 
				1 + as.numeric( f1406715 ) , 
				labels = c( "not metro" , "metro" , "micro" ) 
			) ,
			
		mhi_2014 = f1322614 ,
		
		whole_county_hpsa_2016 = as.numeric( f0978716 == 1 ) ,
		
		census_region = 
			factor( 
				as.numeric( f04439 ) , 
				labels = c( "northeast" , "midwest" , "south" , "west" ) 
			)

	)
	
nrow( ahrf_df )

table( ahrf_df[ , "cbsa_indicator_code" ] , useNA = "always" )
mean( ahrf_df[ , "mhi_2014" ] , na.rm = TRUE )

tapply(
	ahrf_df[ , "mhi_2014" ] ,
	ahrf_df[ , "cbsa_indicator_code" ] ,
	mean ,
	na.rm = TRUE 
)
prop.table( table( ahrf_df[ , "census_region" ] ) )

prop.table(
	table( ahrf_df[ , c( "census_region" , "cbsa_indicator_code" ) ] ) ,
	margin = 2
)
sum( ahrf_df[ , "mhi_2014" ] , na.rm = TRUE )

tapply(
	ahrf_df[ , "mhi_2014" ] ,
	ahrf_df[ , "cbsa_indicator_code" ] ,
	sum ,
	na.rm = TRUE 
)
quantile( ahrf_df[ , "mhi_2014" ] , 0.5 , na.rm = TRUE )

tapply(
	ahrf_df[ , "mhi_2014" ] ,
	ahrf_df[ , "cbsa_indicator_code" ] ,
	quantile ,
	0.5 ,
	na.rm = TRUE 
)
sub_ahrf_df <- subset( ahrf_df , f12424 == "CA" )
mean( sub_ahrf_df[ , "mhi_2014" ] , na.rm = TRUE )
var( ahrf_df[ , "mhi_2014" ] , na.rm = TRUE )

tapply(
	ahrf_df[ , "mhi_2014" ] ,
	ahrf_df[ , "cbsa_indicator_code" ] ,
	var ,
	na.rm = TRUE 
)
t.test( mhi_2014 ~ whole_county_hpsa_2016 , ahrf_df )
this_table <- table( ahrf_df[ , c( "whole_county_hpsa_2016" , "census_region" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		mhi_2014 ~ whole_county_hpsa_2016 + census_region , 
		data = ahrf_df
	)

summary( glm_result )
library(dplyr)
ahrf_tbl <- tbl_df( ahrf_df )
ahrf_tbl %>%
	summarize( mean = mean( mhi_2014 , na.rm = TRUE ) )

ahrf_tbl %>%
	group_by( cbsa_indicator_code ) %>%
	summarize( mean = mean( mhi_2014 , na.rm = TRUE ) )
