resource "google_project_service" "deadpendency_project_containerregistry" {
  project                    = google_project.deadpendency_project.project_id
  service                    = "containerregistry.googleapis.com"
  disable_dependent_services = true
}

resource "google_project_service" "deadpendency_project_cloudresourcemanager" {
  project                    = google_project.deadpendency_project.project_id
  service                    = "cloudresourcemanager.googleapis.com"
  disable_dependent_services = true
}

resource "google_project_service" "deadpendency_project_cloudrun" {
  project                    = google_project.deadpendency_project.project_id
  service                    = "run.googleapis.com"
  disable_dependent_services = true
}

resource "google_project_service" "deadpendency_project_iam" {
  project                    = google_project.deadpendency_project.project_id
  service                    = "iam.googleapis.com"
  disable_dependent_services = true
}

resource "google_project_service" "deadpendency_project_compute" {
  project                    = google_project.deadpendency_project.project_id
  service                    = "compute.googleapis.com"
  disable_dependent_services = true
}

resource "google_project_service" "deadpendency_project_cloudtrace" {
  project                    = google_project.deadpendency_project.project_id
  service                    = "cloudtrace.googleapis.com"
  disable_dependent_services = true
}

resource "google_project_service" "deadpendency_project_servicenetworking" {
  project                    = google_project.deadpendency_project.project_id
  service                    = "servicenetworking.googleapis.com"
  disable_dependent_services = true
}

resource "google_project_service" "deadpendency_project_vpcaccess" {
  project                    = google_project.deadpendency_project.project_id
  service                    = "vpcaccess.googleapis.com"
  disable_dependent_services = true
}

resource "google_project_service" "deadpendency_project_redis" {
  project                    = google_project.deadpendency_project.project_id
  service                    = "redis.googleapis.com"
  disable_dependent_services = true
}

resource "google_project_service" "deadpendency_project_monitoring" {
  project                    = google_project.deadpendency_project.project_id
  service                    = "monitoring.googleapis.com"
  disable_dependent_services = true
}

resource "google_project_service" "deadpendency_project_secretmanager" {
  project                    = google_project.deadpendency_project.project_id
  service                    = "secretmanager.googleapis.com"
  disable_dependent_services = true
}

resource "google_project_service" "deadpendency_project_cloudscheduler" {
  project                    = google_project.deadpendency_project.project_id
  service                    = "cloudscheduler.googleapis.com"
  disable_dependent_services = true
}

resource "google_project_service" "deadpendency_project_appengine" {
  project                    = google_project.deadpendency_project.project_id
  service                    = "appengine.googleapis.com"
  disable_dependent_services = true
}
