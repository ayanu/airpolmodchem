# Dry deposition parameters

Dry deposition parameters as used by the Wesely 1989 parameterisation.

## Usage

``` r
data(dry.depo.para)
```

## Format

A list of 5 data.frames with variables given for 13 land cover types.
Each data.frame represents a different part of the growing season and
contains columns:

- lu:

  land use index

- r.min:

  minimal stomatal resistance

- r.cut.0:

  base cuticle resistance

- r.canp:

  lower canopy resistance

- r.soil.SO2:

  soil resistance for SO2

- r.soil.O3:

  soil resistance for O3

- r.surf.SO2:

  exposed surface resistance for SO2

- r.surf.O3:

  exposed surface resistance for O3

- season:

  season index; same as list index

Seasons are:

- 1:

  midsummer with lush vegetation

- 2:

  autumn with unharvested cropland

- 3:

  late autumn after frost, no snow

- 4:

  winter, snow on ground and subfreezing

- 5:

  transitional spring with partially green short annuals

Land cover types are:

- 1:

  Urban land

- 2:

  agricultural land

- 3:

  range land

- 4:

  deciduous forest

- 5:

  coniferous forest

- 6:

  mixed forest including wetland

- 7:

  water, both salt and fresh

- 8:

  barren land, mostly desert

- 9:

  non-forested wetland

- 10:

  mixed agricultural and range land

- 11:

  rocky open areas with low-growing shrubs

- 12:

  snow; added in FLEXPART Stohl et al. 2005

- 13:

  rainforest; added in FLEXPART Stohl et al. 2005

## References

https://doi.org/10.1016/0004-6981(89)90153-4
