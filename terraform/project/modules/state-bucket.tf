resource "google_storage_bucket" "tf_dp_store" {
  name     = "dgtw-products-deadpendency-action-tf-state-central1"
  location = "US"
  project  = google_project.deadpendency_project.project_id
  versioning {
    enabled = true
  }
  force_destroy = true
}
