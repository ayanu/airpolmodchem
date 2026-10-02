# Calculate dry depostion volocity

Calculates the dry depostion velocity of a gas following the
parameterisation of Wesely. On input meteorological condtions, land
cover type and season need to be provided.

## Usage

``` r
dry.deposition.velocity(
  u.star,
  zz,
  z.0,
  GR,
  TT.s,
  lu = 2,
  season = 1,
  spec = c("O3", "SO2", "NO2", "NO", "HNO3", "H2O2", "HONO"),
  H.star,
  f.0,
  Dv.by.Dq
)
```

## Arguments

- u.star:

  Friction velocity in the surface layer (m/s)

- zz:

  Reference height for which to calculate dry deposition velocity (m)

- z.0:

  Roughness length (m)

- GR:

  Global radiation (W/m2)

- TT.s:

  Surface temperature in (degree C)

- lu:

  Landuse/landcover class used by Wesely. 1) Urban land; 2) agricultural
  land; 3) range land; 4) deciduous forest; 5) coniferous forest; 6)
  mixed forest; 7) water, both salt and fresh; 8) barren land, mostly
  desert; 9) nonforested wetland; 10) mixed agricultural and range
  land; 11) rocky open areas with low growing shrubs

- season:

  Integer 1 to 5 describing different seasons: 1) midsummer with lush
  vegetation; 2) autum with unharvested cropland; 3) late autum afer
  frost, no snow; 4) winter, snow on ground and subfreezing; 5)
  transitional spring with partially green short annuals

- spec:

  Species for which deposition velocity should be calculated. One of O3,
  SO2, NO2, NO, HNO3 H2O2, HONO.

- H.star:

  Effective Henry's law constant (mol/l bar-1). If not given standard
  value by species is given.

- f.0:

  Reactivity factor (unitless; range 0-1). If not given standard value
  by species is given.

- Dv.by.Dq:

  Ratio of the diffusion coefficient of water vapor to that of gas q. If
  not given standard value by species is given.

## Value

dry deposition velocity in m/s

## References

Wesely, M. L., 1989: Parameterization of surface resistances to gaseous
dry deposition in regional-scale numerical models. Atmospheric
Environment (1967), 23, 1293-1304.
