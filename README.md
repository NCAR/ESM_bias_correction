Fortran code to read 3D atmospheric fields from an Earth System Model and bias correct them to match a reanalysis dataset.

This code is not yet well documented, and there is no way to configure it other than modifying the src/initialize.f90 file.

Input datasets need to specify the latitude, longitude, and vertical coordinates.  Coordinates in the references (reanalysis) dataset must be in the same units as the ESM dataset.

Date range to use for calibration and the time frame to correct can be specified as simple strings, e.g., "2000-01-01 12:00:00".

This code will apply a basic quantile mapping correction gridcell by gridcell to all variables listed.

There are basic tests included.  Run make_test.py, then run esm_bias_correction to confirm that it does not print errors or crash. 
