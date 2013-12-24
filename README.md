animalTrack
===========

R package for animal track reconstruction for high frequency 2-dimensional (2D) or 3-dimensional (3D) movement data. 2D and 3D animal tracking data can be used to reconstruct tracks through time/space with correction based on known positions. 3D visualization of animal position and attitude.

The current version of the package on the CRAN can be found at http://cran.us.r-project.org/web/packages/animalTrack

This package is intended for processing and visualization of high frequency animal tracking data, either 2D or 3D. As miniaturization technology has improved over the past 30 years, high resolution animal-borne data loggers have become increasingly used by scientists to study animal behavior. This package is intended to bridge the gap between raw multi-sensor data and meaningful calibrated variables (e.g. position, speed, attitude, heading) that can be used to understand natural behavior. All calculations that are performed to estimate the pitch, roll, and yaw angles of the animal are in the north-east-down (NED) frame of reference. It is important that the user either record measurements in the NED frame, or shift the raw measurements of each axis to NED before any calculations using this package.
