# Arome Weather Model Support

## Overview
Added support for the French Météo-France Arome weather model to cl-weather. Arome is a high-resolution convective-scale weather model covering regional areas.

## Configuration

### Datasource: `arome-wind`

The Arome datasource is automatically included in the active datasources list (`*datasources*` in [cycle-update.cl](cycle-update.cl)) and will be periodically downloaded.

### Default Parameters
- **Runs**: 00, 06, 12, 18 UTC (6-hourly cycles)
- **Forecast Steps**: 0, 3, 6, ..., 48 hours (3-hour intervals)
- **Dissemination Timing**: 150-350 minutes after model run (2.5-5.8 hours)
- **Max Forecast Period**: 48 hours
- **Default Region**: `western_med` (western Mediterranean)
- **Data Source**: CloudFront CDN

## Data URL Format

```
https://dk7714bfk71nn.cloudfront.net/arome/{region}/{YYYYMMDD}/{HH}/arome.t{HH}z.{region}.f006-f048.grib2
```

Example:
```
https://dk7714bfk71nn.cloudfront.net/arome/western_med/20260613/06/arome.t06z.western_med.f006-f048.grib2
```

## Local Storage

Files are stored in:
```
./weather/arome/{region}/{YYYYMMDD}/{HH}/arome.t{HH}z.{region}.f006-f048.grib2
```

Example:
```
./weather/arome/western_med/20260613/06/arome.t06z.western_med.f006-f048.grib2
```

## Implementation Details

### Class Hierarchy
```
arome-wind
  ├── datasource (base class)
  ├── file-download (uses full-file download strategy, no range queries)
  └── datakind-wind (U/V wind variables)
```

### Methods Implemented
- `datasource-schedule`: Returns schedule with 6-hourly runs and 3-hourly steps
- `current-cycle`: Computes current cycle (6-hourly cycles, 150min dissemination delay)
- `latest-complete-cycle`: Determines latest theoretically complete cycle
- `timestamp-cycle`: Selects appropriate cycle for a given timestamp
- `cycle-forecast`: Maps timestamp to forecast step
- `next-forecast`: Returns next available forecast step
- `file-step`: Returns the file step (same as requested step for Arome)
- `get-grib-file-ranges`: Not supported (returns error - Arome uses full-file download)
- `local-pathname`: Constructs local file path
- `index-uri`: Returns nil (no separate index file)
- `probe-uris`: Checks if the remote file exists
- `data-uri`: Constructs the CloudFront URL

## Region Support

Currently configured for `western_med` region by default. To support additional Arome regions:

1. Create a new datasource instance with different region:
   ```lisp
   (get-datasource 'arome-wind 
                   (make-cycle :timestamp (now))
                   :region "region-name")
   ```

2. Or modify the `:region :initform "western_med"` in the class definition for different defaults.

### Known Arome Regions (examples)
- `western_med` - Western Mediterranean
- `arctic` - Arctic region
- Other regional domains available from Météo-France

## Usage

### Automatic
Once configured, Arome data will be automatically downloaded and cached by `start-cycle-updates`.

### Manual Download
```lisp
(start-cycle-updates)  ; Includes arome-wind in the datasources list
```

### Accessing Forecasts
```lisp
;; Get interpolated wind at a location and time
(multiple-value-bind (direction speed)
    (cl-weather:get-wind 'arome-wind 
                         (local-time:now)
                         latitude
                         longitude)
  (format t "Wind: ~a° at ~a knots~%" direction (cl-weather:m/s-to-knots speed)))

;; Get parameters for manual interpolation
(let ((params (get-params 'arome-wind timestamp)))
  (interpolate-uv latitude longitude params))
```

## Notes

- Arome files are downloaded as complete files (no byte-range requests)
- Forecast steps are 3-hourly from 0 to 48 hours
- Default dissemination timing assumes ~150 minutes for Arome data availability
- Region parameter provides extensibility for additional regional Arome domains
