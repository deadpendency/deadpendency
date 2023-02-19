resource "google_compute_network" "deadpendency_cache_network" {
  name         = "deadpendency-cache-network"
  routing_mode = "REGIONAL"
  project      = google_project.deadpendency_project.project_id
}

resource "google_compute_global_address" "deadpendency_cache_service_range" {
  name          = "deadpendency-cache-network-range"
  project       = google_project.deadpendency_project.project_id
  purpose       = "VPC_PEERING"
  address_type  = "INTERNAL"
  prefix_length = 16
  network       = google_compute_network.deadpendency_cache_network.id
}

resource "google_service_networking_connection" "deadpendency_cache_private_service_connection" {
  network                 = google_compute_network.deadpendency_cache_network.id
  service                 = "servicenetworking.googleapis.com"
  reserved_peering_ranges = [google_compute_global_address.deadpendency_cache_service_range.name]
}

resource "google_vpc_access_connector" "deadpendency_cache_vpc_connector2" {
  # must be less than 25 chars
  name          = "dp-cach-vpc-con2"
  project       = google_project.deadpendency_project.project_id
  region        = "us-central1"
  ip_cidr_range = "10.9.0.0/28"
  network       = google_compute_network.deadpendency_cache_network.name
}

resource "google_redis_instance" "deadpendency_cache2" {
  name           = "deadpendency-cache2"
  project        = google_project.deadpendency_project.project_id
  tier           = "BASIC"
  memory_size_gb = 1

  authorized_network = google_compute_network.deadpendency_cache_network.id
  connect_mode       = "PRIVATE_SERVICE_ACCESS"

  redis_version = "REDIS_6_X"
  display_name  = "Deadpendency Action Cache 2"

  depends_on = [google_service_networking_connection.deadpendency_cache_private_service_connection]
}
