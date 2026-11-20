# cl-weather
Access weather forecasts (NOAA &amp; ECMWF)

Wrap a unit of work in `(cl-weather:with-forecast-access-context () ...)` to keep
missing GRIB files unavailable for that operation. The context is a fresh,
dynamically bound table discarded on exit, including error exits. A confirmed
missing file is remembered by its absolute physical file path, so combined-file
steps and direction fields share the decision. Repeated samples signal
`missing-forecast` without reopening the file or repeating its warning; existing
previous-cycle fallback remains in effect. A newly created file, even if another
operation puts its fields in the shared cache, stays missing in the original
context. A fresh context can use it immediately. Successful forecast caching
remains global; decoding errors and failures on existing files are not remembered.
Calls outside a context retain their existing behavior. Worker threads must
establish their own context or explicitly propagate the dynamic binding.

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

Download availability is shared across all sources. `cycle-for-run` selects UTC
run timestamps from `schedule-runs`, including daily and irregular schedules.
`latest-scheduled-cycle` and `publishing-cycles` read the run-specific
`dissem-start` and `dissem-end` metadata. Default completion/in-progress selectors,
startup recovery and recurring timers use this common logic. Startup recovers the
latest complete cycle and all publishing cycles, with duplicates removed. It does
not use routing-specific cycle-selection heuristics.

ECMWF routing cycle selection also uses these common defaults. IFS and
AIFS start at +5h45. Nominal completion (including a 30-minute safety margin)
is +8h04 for IFS 00/12Z and all AIFS runs, and +6h57 for IFS 06/18Z.
IFS currents share the IFS wind schedule. There are no separate IFS/AIFS
completion or in-progress timing implementations. Existing GFS, AROME and
Fairwinds routing selection policies remain specialized; they do not determine
download timing. URL construction, forecast step selection, transfer/retry
strategies and GRIB decoding retain their source-specific methods.

Scheduling tests cover every configured source, exact publication boundaries,
day rollover, overlapping windows, irregular run hours, metadata changes and
recurring timer callbacks. Tests use disposable stubs and never start downloads.
