DROP TABLE IF EXISTS trackers CASCADE;
DROP TABLE IF EXISTS trackers_dbhydro_hydro;
DROP TABLE IF EXISTS trackers_dbhydro_wq;
DROP TABLE IF EXISTS trackers_usgs_dv;

CREATE TABLE trackers (
  id TEXT PRIMARY KEY,
  description TEXT
);

CREATE TABLE trackers_dbhydro_hydro (
  tracker_id TEXT REFERENCES trackers(id) ON DELETE CASCADE,
  dbkey TEXT NOT NULL,
  date_min DATE,
  date_max DATE
);
CREATE UNIQUE INDEX CONCURRENTLY trackers_dbhydro_hydro_tracker_id_dbkey_idx ON trackers_dbhydro_hydro(tracker_id, dbkey);

CREATE TABLE trackers_dbhydro_wq (
  tracker_id TEXT REFERENCES trackers(id) ON DELETE CASCADE,
  station_id TEXT NOT NULL,
  wq_param TEXT NOT NULL,
  date_min DATE,
  date_max DATE
);
CREATE UNIQUE INDEX CONCURRENTLY trackers_dbhydro_wq_tracker_id_station_id_wq_param_idx ON trackers_dbhydro_wq(tracker_id, station_id, wq_param);

CREATE TABLE trackers_usgs_dv (
  tracker_id TEXT REFERENCES trackers(id) ON DELETE CASCADE,
  station_id TEXT NOT NULL,
  param TEXT NOT NULL,
  date_min DATE,
  date_max DATE
);
CREATE UNIQUE INDEX CONCURRENTLY trackers_usgs_dv_tracker_id_station_id_param_idx ON trackers_usgs_dv(tracker_id, station_id, param);
