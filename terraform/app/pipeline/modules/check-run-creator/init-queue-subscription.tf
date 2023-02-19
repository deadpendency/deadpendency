resource "google_pubsub_subscription" "init_queue_check_run_creator_subscription" {
  name    = "${var.app_env}-${var.app_live}-init-queue-check-run-creator-subscription"
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.init_queue_topic_id

  ack_deadline_seconds       = 60

  push_config {
    push_endpoint = google_cloud_run_service.deadpendency_action_check_run_creator.status[0].url
    oidc_token {
      service_account_email = google_service_account.init_queue_check_run_creator_invoker_account.email
    }

    attributes = {
      x-goog-version = "v1"
    }
  }

  retry_policy {
    minimum_backoff = "1s"
    maximum_backoff = "5s"
  }

  dead_letter_policy {
    dead_letter_topic     = var.init_queue_topic_dlq_id
    max_delivery_attempts = 5
  }
}

resource "google_service_account" "init_queue_check_run_creator_invoker_account" {
  account_id   = "${var.app_env}-${var.app_live}-crc-invoker-account"
  display_name = "Check Run Creator Invoker Cloud Run Account ${var.app_env}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_subscription_iam_member" "init_queue_check_run_creator_subscription_member_account" {
  subscription = google_pubsub_subscription.init_queue_check_run_creator_subscription.name
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.init_queue_check_run_creator_invoker_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_topic_iam_member" "check_run_creator_cloud_run_account_init_queue_pubsub_subscriber" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.init_queue_topic_id
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.init_queue_check_run_creator_invoker_account.email}"
}

resource "google_pubsub_topic_iam_member" "check_run_creator_cloud_run_account_init_dlq_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.init_queue_topic_dlq_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.init_queue_check_run_creator_invoker_account.email}"
}

resource "google_pubsub_subscription_iam_member" "init_queue_pubsub_sa_subscription_member_account" {
  subscription = google_pubsub_subscription.init_queue_check_run_creator_subscription.name
  role         = "roles/pubsub.subscriber"
  member       = "serviceAccount:service-${data.google_project.deadpendency_action_project.number}@gcp-sa-pubsub.iam.gserviceaccount.com"
  project      = data.google_project.deadpendency_action_project.project_id
}
