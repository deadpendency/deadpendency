resource "google_pubsub_topic" "deadpendency_action_pubsub_run_prepared" {
  project = data.google_project.deadpendency_action_project.project_id
  name    = "${var.app_env}-${var.app_live}-deadpendency-action-run-prepared"
}

resource "google_pubsub_topic" "deadpendency_action_pubsub_run_prepared_dlq" {
  project = data.google_project.deadpendency_action_project.project_id
  name    = "${var.app_env}-${var.app_live}-deadpendency-action-run-prepared-dlq"
}

resource "google_pubsub_topic_iam_member" "service_account_run_prepared_dlq_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = google_pubsub_topic.deadpendency_action_pubsub_run_prepared_dlq.id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:service-${data.google_project.deadpendency_action_project.number}@gcp-sa-pubsub.iam.gserviceaccount.com"
}
