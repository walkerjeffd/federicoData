DROP TABLE IF EXISTS usgs_dv;
DROP TABLE IF EXISTS usgs_stations;

CREATE TABLE usgs_stations (
  station_id TEXT PRIMARY KEY,
  station_name TEXT,
  latitude REAL,
  longitude REAL
);

CREATE TABLE usgs_dv (
  id BIGSERIAL PRIMARY KEY,
  station_id TEXT REFERENCES usgs_stations(station_id),
  date DATE,
  value REAL,
  flag TEXT,
  param TEXT,
  param_code TEXT,
  units TEXT,
  stat_code TEXT
);
CREATE UNIQUE INDEX CONCURRENTLY
  usgs_dv_station_id_param_date_idx
  ON usgs_dv(station_id, param, date);
