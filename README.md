# The Demand.ninja model

This code performs the 'core' calculations of the [Demand.ninja](https://dev.renewables.ninja) model.

It delivers an hourly time-series of energy demand for a given location, for a given set of temperaure thresholds and BAIT (building-adjusted internal temperature) values which describe the characteristics of a building and its occupants. the main function `demand_ninja` uses weather data from the dev.renewables.ninja website for a desired location, processes this into the BAIT temperature index and then finally calculates energy demand.


## Setup
`Demand.ninja` requires R version 3+, with the `lubridate` library.

Download the files from the and you are ready to go. The calculation code lives inside `demand_ninja.r`. An example of how to use this code is inside `example_1.r`.  In that file, you just need to change the path to say where you have downloaded all the files, then you're good to go.


## Credits and contact
Contact Iain Staffell if you have questions about `Demand.ninja`.  `Demand.ninja` is a component of the [Renewables.ninja](https://renewables.ninja) project, developed by Stefan Pfenninger and Iain Staffell. Use the contact page there if you want more information.

## Citation
If you use `Demand.ninja` or code derived from it in academic work, please cite:

Iain Staffell, Stefan Pfenninger and Nathan Johnson (2022). _[Satellite weather data accurately models global space heating and cooling demand.](https://doi.org/10.21203/rs.3.rs-1834485/v1)_
