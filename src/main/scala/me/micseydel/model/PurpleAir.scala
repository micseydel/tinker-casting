package me.micseydel.model

case class RawSensorData(
  SensorId: String,
  DateTime: String,
  Geo: String,
  Mem: Int,
  memfrag: Int,
  memfb: Int,
  memcs: Int,
  Id: Int,
  lat: Double,
  lon: Double,
  Adc: Double,
  loggingrate: Int,
  place: String,
  version: String,
  uptime: Long,
  rssi: Int,
  period: Int,
  httpsuccess: Int,
  httpsends: Int,
  hardwareversion: String,
  hardwarediscovered: String,
  current_temp_f: Int,
  current_humidity: Int,
  current_dewpoint_f: Int,
  pressure: Double,
  p25aqic: String,
  pm2_5_aqi: Int,
  pm1_0_cf_1: Double,
  p_0_3_um: Double,
  pm2_5_cf_1: Double,
  p_0_5_um: Double,
  pm10_0_cf_1: Double,
  p_1_0_um: Double,
  pm1_0_atm: Double,
  p_2_5_um: Double,
  pm2_5_atm: Double,
  p_5_0_um: Double,
  pm10_0_atm: Double,
  p_10_0_um: Double,
  pa_latency: Int,
  wlstate: String,
  status_0: Int,
  status_1: Int,
  status_2: Int,
  status_3: Int,
  status_4: Int,
  status_5: Int,
  status_7: Int,
  status_8: Int,
  status_9: Int,
  ssid: String
)