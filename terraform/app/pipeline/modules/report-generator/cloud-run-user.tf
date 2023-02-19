resource "google_service_account" "report_generator_cloud_run_account" {
  account_id   = "${var.app_env}-${var.app_live}-rg-cloud-run-account"
  display_name = "Report Generator Cloud Run Account ${var.app_env}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_topic_iam_member" "report_generator_cloud_run_account_report_generated_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = google_pubsub_topic.deadpendency_action_pubsub_report_generated.name
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.report_generator_cloud_run_account.email}"
}

resource "google_pubsub_topic_iam_member" "report_generator_cloud_run_account_processing_failure_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.processing_failure_queue_topic_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.report_generator_cloud_run_account.email}"
}

resource "google_project_iam_member" "report_generator_cloud_run_account_logging_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.report_generator_cloud_run_account.email}"
}

resource "google_project_iam_member" "report_generator_run_account_trace_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/cloudtrace.agent"
  member  = "serviceAccount:${google_service_account.report_generator_cloud_run_account.email}"
}
