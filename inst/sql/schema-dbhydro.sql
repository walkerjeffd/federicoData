DROP TABLE IF EXISTS dbhydro_wq;
DROP TABLE IF EXISTS dbhydro_hydro;
DROP TABLE IF EXISTS dbhydro_dbkeys;
DROP TABLE IF EXISTS dbhydro_stations;

CREATE TABLE dbhydro_stations (
  station_id TEXT PRIMARY KEY,
  site TEXT,
  type TEXT,
  latitude REAL,
  longitude REAL,
  x_coord REAL,
  y_coord REAL,
  county TEXT,
  basin TEXT,
  sec INTEGER,
  twp INTEGER,
  rng INTEGER,
  description TEXT
);

CREATE TABLE dbhydro_dbkeys (
  dbkey TEXT PRIMARY KEY,
  station_id TEXT REFERENCES dbhydro_stations(station_id) ON DELETE CASCADE,
  site_group TEXT,
  data_type TEXT,
  freq TEXT,
  stat TEXT,
  recorder TEXT,
  agency TEXT,
  start_date DATE,
  end_date DATE,
  strata INTEGER,
  op_num INTEGER,
  struct TEXT
);

CREATE TABLE dbhydro_wq (
  id SERIAL PRIMARY KEY,
  station_id TEXT REFERENCES dbhydro_stations(station_id),
  wq_param TEXT,
  date DATE,
  value REAL,
  units TEXT,
  flag TEXT,
  sample_type_new TEXT,
  collection_method TEXT,
  project_code TEXT,
  sample_id TEXT,
  depth REAL,
  depth_unit TEXT,
  matrix TEXT,
  test_number INTEGER,
  test_name TEXT,
  storet_code INTEGER,
  method TEXT,
  first_trigger_date TIMESTAMPTZ,
  collection_date TIMESTAMPTZ,
  measure_date TIMESTAMPTZ,
  receive_date TIMESTAMPTZ,
  sigfig_value REAL,
  uncertainty TEXT,
  mdl REAL,
  pql REAL,
  rdl REAL,
  remark_code TEXT,
  lims_number TEXT,
  collection_agency TEXT,
  source TEXT,
  owner TEXT,
  validation_level TEXT,
  validator TEXT,
  sampling_purpose TEXT,
  data_investigation TEXT,
  t_depth REAL,
  upper_depth REAL,
  lower_depth REAL,
  dcs_meters REAL,
  filtration_date TIMESTAMPTZ,
  sample_type INTEGER,
  qc_type TEXT,
  discharge INTEGER,
  up_down_stream INTEGER,
  weather_code INTEGER,
  program_type TEXT,
  ndec INTEGER,
  sample_comments TEXT,
  result_comments TEXT
);
CREATE UNIQUE INDEX CONCURRENTLY
  dbhydro_wq_station_param_sample_project_source_idx
  ON dbhydro_wq(station_id, wq_param, sample_id, project_code, source);

CREATE TABLE dbhydro_hydro (
  id BIGSERIAL PRIMARY KEY,
  dbkey TEXT REFERENCES dbhydro_dbkeys(dbkey),
  type TEXT,
  units TEXT,
  date Date,
  value REAL,
  qualifier TEXT,
  revision_date DATE
);
CREATE UNIQUE INDEX CONCURRENTLY
  dbhydro_hydro_dbkey_date_idx
  ON dbhydro_hydro(dbkey, date);
