resource "google_service_account" "front_proxy_cloud_run_account" {
  account_id   = "${var.app_env}-fp-cloud-run-account"
  display_name = "Front Proxy Cloud Run Account ${var.app_env}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_project_iam_member" "front_proxy_cloud_run_invoker" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/run.invoker"
  member  = "serviceAccount:${google_service_account.front_proxy_cloud_run_account.email}"
}

resource "google_project_iam_member" "front_proxy_cloud_run_account_logging_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.front_proxy_cloud_run_account.email}"
}
