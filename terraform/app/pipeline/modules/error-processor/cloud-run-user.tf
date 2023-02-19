resource "google_service_account" "error_processor_cloud_run_account" {
  account_id   = "${var.app_env}-${var.app_live}-ep-run-account"
  display_name = "Message Replayer Cloud Run Account ${var.app_env}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_project_iam_member" "error_processor_cloud_run_account_logging_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.error_processor_cloud_run_account.email}"
}

resource "google_project_iam_member" "error_processor_run_account_trace_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/cloudtrace.agent"
  member  = "serviceAccount:${google_service_account.error_processor_cloud_run_account.email}"
}

resource "google_pubsub_topic_iam_member" "error_processor_cloud_run_account_processing_failure_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.processing_failure_queue_topic_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.error_processor_cloud_run_account.email}"
}