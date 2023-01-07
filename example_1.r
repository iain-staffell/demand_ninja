#####
## ##  DEMAND.NINJA SIMPLEST EXAMPLE
#####
##
#
#    this example code uses the demand.ninja to model the
#    electricity demand from a house in london with air
#    conditioning and electric heating
#

	# point me to where you saved this
	setwd('M:/WORK/Projects/NEUPA/Model/GitHub/')

	# include the demand.ninja code
	source('demand_ninja.r')



	################################
	## TEMPERATURE INDEX PARAMETERS
	##

	bait = list()

	# the influence that temperatures on the previous two days have on the current temperature index
	# this represents the building's thermal inertia, higher values imply better insulation
	# units: days^-1
	# default: 0.50  - should be in the range of 0 to 1
	# context: 0.20 to 0.80 is the typical range to use (2 stdev across different datasets)
	bait$smoothing = 0.50

	# the influence that greater sunshine has on increasing perceived temperature
	# this represents solar gains in the building, higher values imply larger windows allowing sunlight in
	# units: degrees C per W/m^2
	# default: 0.012 - should be positive, unless sunshine makes you colder :-|
	# context: 0.004 to 0.020 is the typical range to use (2 stdev across different datasets)
	bait$solar = 0.012
	
	# the influence that greater wind speed has on reducing perceived temperature
	# this represents wind chill in the building, larger (less negative) values imply better air tightness (less leaky)
	# units: degrees C per m/s^2
	# default: -0.20 - should be negative
	# context: -0.05 to -0.35 is the typical range to use (2 stdev across different datasets)
	bait$wind = -0.20

	# the influence that greater humidity has on moving perceived temperature further from the desired level
	# this represents the added discomfort from humidity, larger values imply greater discomfort during humid weather
	# units: degrees C per g/kg
	# default: 0.05 - should be positive
	# context: 0.00 to 0.10 is the typical range to use (2 stdev across different datasets)
	bait$humidity = 0.05



	##############################
	## BUILDING ENERGY PARAMETERS
	##

	nrg = list()

	# the heating threshold - temperature below which heating is required
	# units: degrees C
	# default: 14
	# context: 11 to 17 is the typical range to use (2 stdev across different datasets)
	nrg$t_heat = 14

	# the cooling threshold - the temperature above which cooling is required
	# units: degrees C
	# default: 20
	# context: 17 to 23 is the typical range to use (2 stdev across different datasets)
	nrg$t_cool = 19.5

	# baseline power - the base level of demand when it is neither hot nor cold
	# units: kW
	# default: 0 - should be a positive value
	# context: if trying to simulate electricity or gas consumption, this value represents 'other'
	#          uses that are not correlated to outdoor temperature (e.g. cooking, water heating, lighting, appliances)
	nrg$p_base = 0.325

	# heating power - the additional demand for heat as temperature falls below heating_threshold
	# units: kW/degree C
	# default: 0.3 - should be a positive value
	# context: this will scale with building size, whether you measure heat delivered or fuel consumed,
	#          and if the latter, the efficiency of the heating system
	#          25 W per degree C is a typical value per person for electricity consumption and
	#          100 W per degree C is a typical value per person for gas consumption across
	#          our household datasets, and these averages are about 3x higher per building
	nrg$p_heat = 0.125

	# cooling_power - the additional demand for cool as temperature rises above cooling_threshold
	# units: kW/degree C
	# default: 0.15 - should be a positive value
	# context: this will scale with building size, whether you measure heat delivered or electricity consumed,
	#          and if the latter, the efficiency of the air conditioning system
	#          50 W per degreeC is a typical value per person for electricity consumption
	#          across our household datasets, these averages are about 3x higher per building
	nrg$p_cool = 0.250



	#################
	## OTHER OPTIONS
	##

	# should we use our global average empirical profile for the diurnal demand
	# when upsampling from daily to hourly (i.e. when in the day people heat & cool)
	use_diurnal_profile = TRUE

	# should we return intermediate variables with the results (weather data, BAIT, HDD, CDD)
	add_raw_data = TRUE



	# wow, that was a lot of setup wasn't it ... we will make that quicker in the next example, promise :-)



	########################
	## RUN THE DEMAND.NINJA
	##

	# first read in some weather data you downloaded from the dev.renewables.ninja website
	# e.g. this file came from https://tinyurl.com/demand-ninja-test-query
	weather = read_ninja_weather('./ninja_weather_51.4772_0.0000_uncorrected.csv')

	# now convert that into demand
	demand = demand_ninja(weather, bait, nrg, use_diurnal_profile, add_raw_data)

	# take a look at it
	plot(demand$time, demand$total_demand, type='l', col='red3', lwd=2)

	# neat
