resource "google_service_account" "run_finalizer_cloud_run_account" {
  account_id   = "${var.app_env}-${var.app_live}-rf-cloud-run-account"
  display_name = "Run Finalizer Cloud Run Account ${var.app_env}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_project_iam_member" "run_finalizer_cloud_run_account_logging_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.run_finalizer_cloud_run_account.email}"
}

resource "google_project_iam_member" "run_finalizer_run_account_trace_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/cloudtrace.agent"
  member  = "serviceAccount:${google_service_account.run_finalizer_cloud_run_account.email}"
}

resource "google_secret_manager_secret_iam_member" "run_finalizer_cloud_run_account_github_app_private_key_secret_reader" {
  project = data.google_project.deadpendency_action_project.project_id
  secret_id = "${var.app_env}-github-app-private-key-secret"
  role = "roles/secretmanager.secretAccessor"
  member = "serviceAccount:${google_service_account.run_finalizer_cloud_run_account.email}"
}
