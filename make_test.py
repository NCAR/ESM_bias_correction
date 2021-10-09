#!/usr/bin/env python

import xarray as xr
import numpy as np

nt = 24
nx = 12
ny = 10
nz = 8

lon_data = np.arange(nx)[np.newaxis,:].repeat(ny,axis=0)
lat_data = np.arange(ny)[:,np.newaxis].repeat(nx,axis=1)
z_data = np.arange(nz)[:,np.newaxis, np.newaxis].repeat(ny,axis=1).repeat(nx,axis=2)

qv_data = 1 / ((z_data + 1) * 10)
qv_data = qv_data[np.newaxis,:,:,:].repeat(nt, axis=0)
th_data = np.zeros((nt, nz,ny,nx))


lat = xr.DataArray(lat_data, name="lat", dims=("y","x",))
lon = xr.DataArray(lon_data, name="lon", dims=("y","x",))
z   = xr.DataArray(  z_data, name="z",   dims=("lev","y","x",))

th  = xr.DataArray( th_data, name="theta",dims=("t","lev","y","x",))
qv  = xr.DataArray( qv_data, name="qv",   dims=("t","lev","y","x",))

ds = xr.Dataset({"lat":lat, "lon":lon, "z":z, "theta":th, "qv":qv})
ds.to_netcdf("erai.nc")
ds.to_netcdf("esm.nc")
