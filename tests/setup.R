#
#
#
library(haven)

tf <- tempfile()

ahrf_url <- "https://data.hrsa.gov//DataDownload/AHRF/AHRF_2021-2022_SAS.zip"

download.file( ahrf_url , tf , mode = 'wb' )

unzipped_files <- unzip( tf , exdir = tempdir() )

sas_fn <- grep( "\\.sas7bdat$" , unzipped_files , value = TRUE )

ahrf_tbl <- read_sas( sas_fn )

ahrf_df <- data.frame( ahrf_tbl )

names( ahrf_df ) <- tolower( names( ahrf_df ) )

# ahrf_fn <- file.path( path.expand( "~" ) , "AHRF" , "this_file.rds" )
# saveRDS( ahrf_df , file = ahrf_fn , compress = FALSE )
# ahrf_df <- readRDS( ahrf_fn )
ahrf_df <- 
	transform( 
		ahrf_df , 
		
		cbsa_indicator_code = 
			factor( 
				as.numeric( f1406720 ) , 
				levels = 0:2 ,
				labels = c( "not metro" , "metro" , "micro" ) 
			) ,
			
		mhi_2020 = f1322620 ,
		
		whole_county_hpsa_2022 = as.numeric( f0978722 ) == 1 ,
		
		census_region = 
			factor( 
				as.numeric( f04439 ) , 
				labels = c( "northeast" , "midwest" , "south" , "west" ) 
			)

	)
	
nrow( ahrf_df )

table( ahrf_df[ , "cbsa_indicator_code" ] , useNA = "always" )
mean( ahrf_df[ , "mhi_2020" ] , na.rm = TRUE )

tapply(
	ahrf_df[ , "mhi_2020" ] ,
	ahrf_df[ , "cbsa_indicator_code" ] ,
	mean ,
	na.rm = TRUE 
)
prop.table( table( ahrf_df[ , "census_region" ] ) )

prop.table(
	table( ahrf_df[ , c( "census_region" , "cbsa_indicator_code" ) ] ) ,
	margin = 2
)
sum( ahrf_df[ , "mhi_2020" ] , na.rm = TRUE )

tapply(
	ahrf_df[ , "mhi_2020" ] ,
	ahrf_df[ , "cbsa_indicator_code" ] ,
	sum ,
	na.rm = TRUE 
)
quantile( ahrf_df[ , "mhi_2020" ] , 0.5 , na.rm = TRUE )

tapply(
	ahrf_df[ , "mhi_2020" ] ,
	ahrf_df[ , "cbsa_indicator_code" ] ,
	quantile ,
	0.5 ,
	na.rm = TRUE 
)
sub_ahrf_df <- subset( ahrf_df , f12424 == "CA" )
mean( sub_ahrf_df[ , "mhi_2020" ] , na.rm = TRUE )
var( ahrf_df[ , "mhi_2020" ] , na.rm = TRUE )

tapply(
	ahrf_df[ , "mhi_2020" ] ,
	ahrf_df[ , "cbsa_indicator_code" ] ,
	var ,
	na.rm = TRUE 
)
t.test( mhi_2020 ~ whole_county_hpsa_2022 , ahrf_df )
this_table <- table( ahrf_df[ , c( "whole_county_hpsa_2022" , "census_region" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		mhi_2020 ~ whole_county_hpsa_2022 + census_region , 
		data = ahrf_df
	)

summary( glm_result )
stopifnot( nrow( ahrf_df ) == 3232 )
library(dplyr)
ahrf_tbl <- as_tibble( ahrf_df )
ahrf_tbl %>%
	summarize( mean = mean( mhi_2020 , na.rm = TRUE ) )

ahrf_tbl %>%
	group_by( cbsa_indicator_code ) %>%
	summarize( mean = mean( mhi_2020 , na.rm = TRUE ) )
library(data.table)
ahrf_dt <- data.table( ahrf_df )
ahrf_dt[ , mean( mhi_2020 , na.rm = TRUE ) ]

ahrf_dt[ , mean( mhi_2020 , na.rm = TRUE ) , by = cbsa_indicator_code ]
library(duckdb)
con <- dbConnect( duckdb::duckdb() , dbdir = 'my-db.duckdb' )
dbWriteTable( con , 'ahrf' , ahrf_df )
dbGetQuery( con , 'SELECT AVG( mhi_2020 ) FROM ahrf' )

dbGetQuery(
	con ,
	'SELECT
		cbsa_indicator_code ,
		AVG( mhi_2020 )
	FROM
		ahrf
	GROUP BY
		cbsa_indicator_code'
)
