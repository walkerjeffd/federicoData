DROP TABLE IF EXISTS trackers CASCADE;
DROP TABLE IF EXISTS trackers_hydro;
DROP TABLE IF EXISTS trackers_wq;

CREATE TABLE trackers (
  id TEXT PRIMARY KEY,
  description TEXT
);

CREATE TABLE trackers_wq (
  tracker_id TEXT REFERENCES trackers(id) ON DELETE CASCADE,
  station_id TEXT NOT NULL,
  wq_param TEXT NOT NULL,
  date_min DATE,
  date_max DATE
);
CREATE UNIQUE INDEX CONCURRENTLY trackers_wq_tracker_id_station_id_idx ON trackers_wq(tracker_id, station_id);

CREATE TABLE trackers_hydro (
  tracker_id TEXT REFERENCES trackers(id) ON DELETE CASCADE,
  dbkey TEXT NOT NULL,
  date_min DATE,
  date_max DATE
);
CREATE UNIQUE INDEX CONCURRENTLY trackers_hydro_tracker_id_dbkey_idx ON trackers_hydro(tracker_id, dbkey);
