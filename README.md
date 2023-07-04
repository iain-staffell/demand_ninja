# The Demand.ninja model

This code performs the 'core' calculations of the [Demand.ninja](https://demand.ninja) model.

It delivers an hourly time-series of energy demand for a given location, for your chosen temperaure thresholds and BAIT (building-adjusted internal temperature) parameters which describe the characteristics of a building and its occupants. The main function `demand_ninja` uses weather data from the www.renewables.ninja website for a desired location, processes this into the BAIT temperature index and then finally calculates energy demand.


## Setup
Demand.ninja requires R version 3+, with the `lubridate` library.

Download the files from this repository and you are ready to go. The calculation code lives inside `demand_ninja.r`. An example of how to use this code is inside `example_1.r`.  In that file, you just need to change the path to say where you have downloaded all the files.


## Credits and contact
Contact [Iain Staffell](i.staffell@imperial.ac.uk) and [Nathan Johnson](nathan.johnson17@imperial.ac.uk) if you have questions about Demand.ninja.  Demand.ninja is a component of the [Renewables.ninja](https://renewables.ninja) project, developed by Stefan Pfenninger and Iain Staffell.


## Citation
If you use Demand.ninja or code derived from it in academic work, please cite:

Iain Staffell, Stefan Pfenninger and Nathan Johnson (2023). _A global model of hourly space heating and cooling demand at multiple spatial scales._ Nature Energy.


## License

The `Demand.ninja` code is available under the BSD-3-Clause license. Examples which use the [Renewables.ninja](https://renewables.ninja) must also respect its [CC BY-NC 4.0](https://www.renewables.ninja/about) license.
