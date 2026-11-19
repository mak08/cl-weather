# cl-weather
Access weather forecasts (NOAA &amp; ECMWF)

Regular latitude/longitude interpolation uses the first GRIB node, increments,
dimensions and scan directions. Regional grids may cross Greenwich or the
antimeridian; only global grids wrap the final cell back to the first column.
Boundary nodes remain sampleable without accessing a neighbour outside the array.
Queries outside coverage return four double-float zeros for vector fields, or
one double-float zero for scalar and direction fields. They do not constrain
the router's search area. AROME retains its bitmap-aware interpolation.

Interpolation regression tests are in `tests/interpolation-tests.cl`, loaded by
`asdf:test-system "cl-weather"`. They include offset regional grids such as IBI,
scan directions, longitude wrapping, edge nodes and out-of-region samples.

ECMWF cycle selection follows the configured dissemination schedule. IFS and
AIFS start at +5h45. Nominal completion (including a 30-minute safety margin)
is +8h04 for IFS 00/12Z and all AIFS runs, and +6h57 for IFS 06/18Z.
IFS currents share the IFS wind schedule. The latest-complete selector uses
these run-specific thresholds rather than a separate fixed delay; tests cover
one second before, at and after each boundary, including day rollover.
