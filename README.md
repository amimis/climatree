# climatree
CO2 sequestration model in tree crops.

The model is dynamic, runs on a monthly timestep and starts at the present time until 50 years ahead. Spatially, an instance of the model can run for all regions (or for specific regions) in NUTS 1, 2 and 3 level. The model consists of the biomass (trunk and roots), the debris pool and the soil. The biomass grows with time and accordingly the roots. The pruning (if left on the field) and the crop losses are feeding the debris and then the soil and the roots exudates are feeding the soil as well. The soil processes are modeled by the RothC model which are affected by climatic data and soil characteristics. 
