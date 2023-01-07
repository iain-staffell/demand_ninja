##################################################################
#                                                                #
#  BSD 3-Clause License                                          #
#  Copyright (C) 2020-2023  Iain Staffell  <staffell@gmail.com>  #
#  All rights reserved.                                          #
#                                                                #
##################################################################
####  ###
####  ###
##   ##      DEMAND.NINJA MODEL
#####
##
#
#
#    this model performs the 'core' demand.ninja calculations
#    it delivers an hourly time-series of energy demand for a 
#    given location, for a given set of temperaure thresholds
#    and BAIT (building-adjusted internal temperature) values
#    which describe the characteristics of a building and its
#    occupants. the main function `demand_ninja` uses weather
#    data from the dev.renewables.ninja website for a desired
#    location, processes this into the BAIT temperature index
#    and then finally calculates energy demand.
#
#
#
#  MAIN USER FUNCTIONS:
#
#    read weather data from the renewables.ninja site  
#       read_ninja_weather = function(filename)
#
#    turn this weather data and your parameters into demand
#       demand_ninja = function(weather, bait, nrg)
#
#
#
#
#  OTHER BACKGROUND FUNCTIONS:
#
#    a list holding data about your api requests
#    used to keep track of your usage limits
#       apilog$...
#
#
#    for converting weather into BAIT:
#        ninja_temperature_index = function(weather, bait)
#        smooth_temperatures = function(temperature, weights)
#
#    calculating degree days:
#        HDD = function(temperature, threshold)
#        CDD = function(temperature, threshold)
#
#    converting BAIT into demand:
#        ninja_energy_demand = function(time, temperature, nrg)
#
#    other bits and bobs:
#
#    background functions to build URLs to access the API
#       ninja_build_wind_url = function(lat, lon, ...)
#       ninja_build_solar_url = function(lat, lon, ...)
#       ninja_build_weather_url = function(lat, lon, ...)
#
#    background functions to download and process ninja simulations 
#       ninja_get_url = function(url)
#       ninja_aggregate_urls = function(urls)
#
#    background functions that do other stuff
#       cat_flush(...)
#       format_date(...)
#
#
#    note there is minimal error checking in these functions!
#
#
#
#  RENEWABLES.NINJA API SPECIFICATION:
# 
#    https://dev.renewables.ninja/api/models
#




#####
## ##  PREREQS
#####

	require(lubridate)




#####
## ##  MAIN USER FUNCTIONS
#####


	# read a csv file of weather data downloaded from the dev.renewables.ninja website
	# this can either be downloaded from the API (e.g. https://tinyurl.com/demand-ninja-test-query)
	# or from the web interface: select `weather` from the dropdown menu, choose your location and year, 
	# then select the four variables: `air temperature`, `ground-level horizontal solar irradiance`, 
	# `specific humidity` and `wind speed`; then run the simulation and click `save hourly output as csv`
	#
	# inputs: the full path and filename for a csv file downloaded from renewables.ninja
	# outputs: a data.frame of daily weather observations, with columns for time, 
	#          T (temperature in degrees C), W (wind speed in m/s), S (solar irradiance in W/m2), H (humidity in g/kg)
	#
	read_ninja_weather = function(filename)
	{
		# read the data
		csv = read.csv(filename, skip=3, stringsAsFactors=FALSE)

		# format the time column
		csv$time = as.POSIXct(csv$time, format="%Y-%m-%d")
		csv$local_time = NULL

		# rename becase i am too damn lazy to read long words
		colnames(csv) = c('time', 'T', 'S', 'W', 'H')

		# return
		csv
	}



	# calculate hourly energy demand from a building based on the input weather data and parameters
	# this does the whole demand.ninja process for you \o/
	#
	# inputs: a data.frame of weather, produced by read_ninja_weather()
	#         a list containing bait parameters (see `example 1.r` for explanation)
	#         a list containing energy demand parameters (see `example 1.r` for explanation)
	#         two optional parameters to control how the model runs and what is returned
	# outputs: a data.frame of hourly energy demand for heating, cooling and total
	#          this optionally also includes weather, bait, heating and cooling degree days
	#
	demand_ninja = function(weather, bait, nrg, use_diurnal_profile=TRUE, add_raw_data=TRUE)
	{
		# kick out losers
		if (!all( c('time', 'T', 'S', 'W', 'H') %in% colnames(weather) ))
			stop('demand_ninja dead: you need to have $time, $T, $S, $W and $H as column names inside the `weather` data.frame...\n')
		if (!all( c('smoothing', 'solar', 'wind', 'humidity') %in% names(bait) ))
			stop('demand_ninja dead: you need to have $smoothing, $solar, $wind and $humidity inside the `bait` variable...\n')
		if (!all( c('t_heat', 't_cool', 'p_base', 'p_heat', 'p_cool') %in% names(nrg) ))
			stop('demand_ninja dead: you need to have $t_heat, $t_cool, $p_base, $p_heat and $p_cool inside the `nrg` variable...\n')
		

		# aggregate our weather daily averages
		daily = aggregate_daily(weather, 'time')

		# calculate the BAIT
		daily$BAIT = ninja_temperature_index(daily, bait)

		# upsample BAIT to hourly
		weather$BAIT = interpolate_hourly(daily$date, daily$BAIT, weather$time)

		# transform to degree days and energy demand
		ninja = ninja_energy_demand(weather$time, weather$BAIT, nrg, use_diurnal_profile)

		# bind these together, ignoring the ninja$time column
		hourly = cbind(weather, ninja[ , -1])

		# kill off the raw data if not wanted
		if (!add_raw_data)
			hourly$T = hourly$S = hourly$W = hourly$H = hourly$BAIT = hourly$HDD = hourly$CDD = NULL

		# return
		hourly
	}





#####
## ##  INTERNAL FUNCTIONS
#####

	# create the modified and smoothed ninja temperature index
	#
    # inputs: a data.frame of daily weather data observations (from read_weather)
    #         our four 'bait' parameters: how much should sun, wind and humidity
    #         affect the index, and how much temporal smoothing should there be
    # output: our ninja temperature index (an array as the same length as the inputs)
    #
    ninja_temperature_index = function(weather, bait)
    {
        # 1. we compute 'setpoint' values for wind, sun and humidity
        #    these are the 'average' values for the given temperature
        #    and are used to decide if it is windier than average, 
        #    sunnier than aveage, etc.  this makes N correlate roughly
        #    1:1 with T, rather than is biased above or below it.
 		setpoint_S = 100 + 7 * weather$T           # W/m2
		setpoint_W = 4.5 - 0.025 * weather$T       # m/s
		setpoint_H = exp(1.1 + 0.06 * weather$T)   # g water per kg air
		setpoint_T = 16                            # degrees - around which 'discomfort' is measured



        # 2. calculate the unsmoothed ninja temperature index
		#    this is a 'feels like' index - how warm does it 'feel' to your building

		# initialise it to temperature
		N = weather$T

        # if it's sunny, it feels warmer
		N = N + (weather$S - setpoint_S) * bait$solar

        # if it's windy, it feels colder
		N = N + (weather$W - setpoint_W) * bait$wind
		
		# if it's humid, both hot and cold feel more extreme
		discomfort = (N - setpoint_T)
		N = setpoint_T + discomfort + (discomfort * (weather$H - setpoint_H) * bait$humidity)



		# 3. apply temporal smoothing to our temperatures over the last two days
		#    we assume 2nd day smoothing is the square of the first day (i.e. compounded decay)
		N = smooth_temperatures(N, weights=bait$smoothing^(1:2))
        


        # 4. blend the smoothed BAIT with raw temperatures to account for occupant
		#    behaviour changing with the weather (i.e. people open windows when it's hot)

		# these are fixed parameters we don't expose the user to
		lower_blend = 15   # *C at which we start blending T into N
		upper_blend = 23   # *C at which we have fully blended T into N
		max_raw_var = 0.5  # maximum amount of T that gets blended into N

		# transform this window to a sigmoid function, mapping lower & upper onto -5 and +5
		# pahaha why the fuck do i attempt such complex bollocks
        avg_blend = (lower_blend + upper_blend) / 2
        dif_blend = upper_blend - lower_blend
		blend = (weather$T - avg_blend) * 10 / dif_blend
		blend = max_raw_var / (1 + exp(-blend))

		# apply the blend
		N = (weather$T * blend) + (N * (1-blend))

        

        # 5. done
		return(N)
    }


	# smooth a temperature time series over time, with the given weighting for previous days
	#
	# inputs: an array of temperatures
	#         an array of weights for smoothing.  the first element is how much
	#         yesterday's temperature will be, the 2nd element is 2 days ago, etc.
	# output: an array of smoothed temperatures
	#         
	# e.g. to model a temperature half-life of 24 hours, set weights = c(0.5, 0.25)
	#      if you wanted to change this to work with hourly temperature data, you'd 
	#      switch that to weights = (0.5^(1/24)) ^ (1:48), but that will give you
	#      some grief around day-night cycles
	#
	smooth_temperatures = function(temperature, weights)
	{
		lag = smooth = temperature
		N = length(temperature)

		# run through each weight in turn going one time-step backwards each time
		for (w in weights)
		{
			# create a time series of temperatures the day before
			lag = c(lag[1], lag[-N])

			# add on these lagged temperatures multiplied by this smoothing factor
			if (w != 0) smooth = smooth + (lag * w)
		}

		# renormalise and return
		return( smooth / (1 + sum(weights)) )
	}



	# convert temperatures into heating degree days
	#
	# inputs: an array of temperatures
	#         your threshold temperature (below which heating is needed)
	# output: an array of heating degree days
	#
	HDD = function(temperature, threshold)
	{
		# pmax computes the maximum for each element of your inputs in turn (i.e. cap the minimum to 0)
		HDD = pmax(0, threshold-temperature)
		return(HDD)
	}



	# convert temperatures into cooling degree days
	#
	# inputs: an array of temperatures
	#         your threshold temperature (above which cooling is needed)
	# output: an array of cooling degree days
	#
	CDD = function(temperature, threshold)
	{
		# pmax computes the maximum for each element of your inputs in turn (i.e. cap the minimum to 0
		CDD = pmax(0, temperature-threshold)
		return(CDD)
	}



	# convert temperatures into energy demand
	#
    # inputs: an array of timestamps
	#         an array of temperatures (from ninja_temperature_index)
    #         a list of five 'energy' parameters describing the \_/ relationship between temperature and demand
	#         (optional) whether we should convolute energy demand with the ninja's diurnal profiles
    # output: a data.frame containing our estimated energy demand profile + interim calculations
    #
	ninja_energy_demand = function(time, temperature, nrg, use_diurnal_profile=TRUE)
	{
		# initialise our results
		output = data.frame(time=time, HDD=0, CDD=0, heating_demand=0, cooling_demand=0, total_demand=0)

		# add demand for heating
		if (nrg$p_heat > 0)
		{
			output$HDD = HDD(temperature, nrg$t_heat)
			output$heating_demand = output$HDD * nrg$p_heat
		}

		# add demand for cooling
		if (nrg$p_cool > 0)
		{
			output$CDD = CDD(temperature, nrg$t_cool)
			output$cooling_demand = output$CDD * nrg$p_cool
		}

		# apply the diurnal profiles if wanted
		if (use_diurnal_profile)
		{
			# read in the profiles: a data-frame of 24 hourly values containing:
			# hour [0-23], heating and cooling [both normalised multiplicative scale factors, with mean of 1]
			profiles = read.csv('global_diurnal_profiles.csv')
			if (!all(c('hour', 'heating', 'cooling') %in% colnames(profiles)))
				stop('ninja_energy_demand dead: your global_diurnal_profiles.csv file is corrupt and immoral\n')

			# get the hour of day for each timestep - which we use as an array index
			h = hour(output$time) + 1

			# convolute
			output$heating_demand = output$heating_demand * profiles$heating[h]
			output$cooling_demand = output$cooling_demand * profiles$cooling[h]
		}

		# sum total demand
		output$total_demand = nrg$p_base + output$heating_demand + output$cooling_demand

		# romeo dunn
		return(output)
	}



	# convert energy demand (or other parameters) from daily to hourly resolution
	#
    # inputs: an array of dates for your input values (daily resolution)
	#         an array of values (demand, bait, temperature, etc) (daily resolution)
	#         an array of times for your output values (hourly resolution)
    # output: the interpolated values at hourly resolution
    #
	interpolate_hourly = function(input_dates, input_values, output_times)
	{
		# treat our input data as coming from the middle of each day
		input_dates = input_dates + hours(12)

		# do a simple cubic spline interpolation
		output = spline(x=input_dates, y=input_values, xout=output_times)

		# return
		output$y
	}



	# aggregate a data.frame from hourly to daily resolution
	#
    # inputs: an array of dates for your input values (daily resolution)
	#         an array of values (demand, bait, temperature, etc) (daily resolution)
	#         an array of times for your output values (hourly resolution)
    # output: the interpolated values at hourly resolution
    #
	aggregate_daily = function(hourly, col='time')
	{
		dates = floor_date(hourly[ , col], 'day')
		daily = aggregate(hourly, by=list(dates), mean)
		daily[ , col] = NULL
		colnames(daily)[1] = 'date'
		daily$date = as.POSIXct(daily$date)
		daily
	}


