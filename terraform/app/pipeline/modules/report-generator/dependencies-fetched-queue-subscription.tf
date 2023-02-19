resource "google_pubsub_subscription" "dependencies_fetched_queue_report_generator_subscription" {
  name    = "${var.app_env}-${var.app_live}-dependencies-fetched-queue-report-generator-subscription"
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.dependencies_fetched_queue_topic_id

  ack_deadline_seconds       = 60

  push_config {
    push_endpoint = google_cloud_run_service.deadpendency_action_report_generator.status[0].url
    oidc_token {
      service_account_email = google_service_account.dependencies_fetched_queue_report_generator_invoker_account.email
    }

    attributes = {
      x-goog-version = "v1"
    }
  }

  dead_letter_policy {
    dead_letter_topic     = var.dependencies_fetched_queue_topic_dlq_id
    max_delivery_attempts = 5
  }
}

resource "google_service_account" "dependencies_fetched_queue_report_generator_invoker_account" {
  account_id   = "${var.app_env}-${var.app_live}-rg-invoker-account"
  display_name = "Report Generator Invoker Cloud Run Account ${var.app_env}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_subscription_iam_member" "dependencies_fetched_queue_report_generator_subscription_member_account" {
  subscription = google_pubsub_subscription.dependencies_fetched_queue_report_generator_subscription.name
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.dependencies_fetched_queue_report_generator_invoker_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_topic_iam_member" "report_generator_cloud_run_account_dependencies_fetched_queue_pubsub_subscriber" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.dependencies_fetched_queue_topic_id
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.dependencies_fetched_queue_report_generator_invoker_account.email}"
}

resource "google_pubsub_topic_iam_member" "report_generator_cloud_run_account_dependencies_fetched_dlq_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.dependencies_fetched_queue_topic_dlq_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.dependencies_fetched_queue_report_generator_invoker_account.email}"
}

resource "google_pubsub_subscription_iam_member" "dependencies_fetched_queue_pubsub_sa_subscription_member_account" {
  subscription = google_pubsub_subscription.dependencies_fetched_queue_report_generator_subscription.name
  role         = "roles/pubsub.subscriber"
  member       = "serviceAccount:service-${data.google_project.deadpendency_action_project.number}@gcp-sa-pubsub.iam.gserviceaccount.com"
  project      = data.google_project.deadpendency_action_project.project_id
}
