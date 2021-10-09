#!/usr/bin/env python

import xarray as xr

nx = 12
ny = 10
nz = 8

lon_data = np.arange(nx)[np.newaxis,:].repeat(ny,axis=0)
lat_data = np.arange(ny)[:,np.newaxis].repeat(nx,axis=1)
z_data = np.arange(nz)[:,np.newaxis, np.newaxis].repeat(ny,axis=1).repeat(nx,axis=2)

lat = xr.DataArray(lat_data, name="lat", dims=("y","x",))
lon = xr.DataArray(lon_data, name="lon", dims=("y","x",))
z   = xr.DataArray(  z_data, name="z",   dims=("lev","y","x",))

ds = xr.Dataset({"lat":lat, "lon":lon, "z":z})
ds.to_netcdf("erai.nc")
