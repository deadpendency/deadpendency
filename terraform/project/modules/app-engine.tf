# required to use cloud scheduler https://cloud.google.com/scheduler/docs/#supported_regions

resource "google_app_engine_application" "dummy_app" {
  project     = google_project.deadpendency_project.project_id
  location_id = "us-central"
}
