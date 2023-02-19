# invoker account

resource "google_service_account" "error_processor_invoker_account" {
  account_id   = "${var.app_env}-${var.app_live}-ep-invoker-account"
  display_name = "Dependency Determiner Invoker Cloud Run Account ${var.app_env}"
  project      = data.google_project.deadpendency_action_project.project_id
}

# check run created dlq

resource "google_pubsub_subscription" "check_run_created_dlq_error_processor_subscription" {
  name    = "${var.app_env}-${var.app_live}-check-run-created-dlq-error-processor-subscription"
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.check_run_created_queue_topic_dlq_id

  ack_deadline_seconds       = 60

  push_config {
    push_endpoint = google_cloud_run_service.deadpendency_action_error_processor.status[0].url
    oidc_token {
      service_account_email = google_service_account.error_processor_invoker_account.email
    }

    attributes = {
      x-goog-version = "v1"
    }
  }

  retry_policy {
    minimum_backoff = "1s"
    maximum_backoff = "5s"
  }
}

resource "google_pubsub_subscription_iam_member" "check_run_created_dlq_error_processor_subscription_member_account" {
  subscription = google_pubsub_subscription.check_run_created_dlq_error_processor_subscription.name
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.error_processor_invoker_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_topic_iam_member" "error_processor_cloud_run_account_check_run_created_dlq_pubsub_subscriber" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.check_run_created_queue_topic_dlq_id
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.error_processor_invoker_account.email}"
}

resource "google_pubsub_topic_iam_member" "error_processor_cloud_run_account_check_run_created_dlq_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.check_run_created_queue_topic_dlq_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.error_processor_invoker_account.email}"
}

resource "google_pubsub_subscription_iam_member" "check_run_created_dlq_pubsub_sa_subscription_member_account" {
  subscription = google_pubsub_subscription.check_run_created_dlq_error_processor_subscription.name
  role         = "roles/pubsub.subscriber"
  member       = "serviceAccount:service-${data.google_project.deadpendency_action_project.number}@gcp-sa-pubsub.iam.gserviceaccount.com"
  project      = data.google_project.deadpendency_action_project.project_id
}

# run prepared dlq

resource "google_pubsub_subscription" "run_prepared_dlq_error_processor_subscription" {
  name    = "${var.app_env}-${var.app_live}-run-prepared-dlq-error-processor-subscription"
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.run_prepared_queue_topic_dlq_id

  ack_deadline_seconds       = 60

  push_config {
    push_endpoint = google_cloud_run_service.deadpendency_action_error_processor.status[0].url
    oidc_token {
      service_account_email = google_service_account.error_processor_invoker_account.email
    }

    attributes = {
      x-goog-version = "v1"
    }
  }

  retry_policy {
    minimum_backoff = "1s"
    maximum_backoff = "5s"
  }
}

resource "google_pubsub_subscription_iam_member" "run_prepared_dlq_error_processor_subscription_member_account" {
  subscription = google_pubsub_subscription.run_prepared_dlq_error_processor_subscription.name
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.error_processor_invoker_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_topic_iam_member" "error_processor_cloud_run_account_run_prepared_dlq_pubsub_subscriber" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.run_prepared_queue_topic_dlq_id
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.error_processor_invoker_account.email}"
}

resource "google_pubsub_topic_iam_member" "error_processor_cloud_run_account_run_prepared_dlq_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.run_prepared_queue_topic_dlq_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.error_processor_invoker_account.email}"
}

resource "google_pubsub_subscription_iam_member" "run_prepared_dlq_pubsub_sa_subscription_member_account" {
  subscription = google_pubsub_subscription.run_prepared_dlq_error_processor_subscription.name
  role         = "roles/pubsub.subscriber"
  member       = "serviceAccount:service-${data.google_project.deadpendency_action_project.number}@gcp-sa-pubsub.iam.gserviceaccount.com"
  project      = data.google_project.deadpendency_action_project.project_id
}

# dep determine dlq

resource "google_pubsub_subscription" "dependencies_determined_dlq_error_processor_subscription" {
  name    = "${var.app_env}-${var.app_live}-dependencies-determined-dlq-error-processor-subscription"
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.dependencies_determined_queue_topic_dlq_id

  ack_deadline_seconds       = 60

  push_config {
    push_endpoint = google_cloud_run_service.deadpendency_action_error_processor.status[0].url
    oidc_token {
      service_account_email = google_service_account.error_processor_invoker_account.email
    }

    attributes = {
      x-goog-version = "v1"
    }
  }

  retry_policy {
    minimum_backoff = "1s"
    maximum_backoff = "5s"
  }
}

resource "google_pubsub_subscription_iam_member" "dependencies_determined_dlq_error_processor_subscription_member_account" {
  subscription = google_pubsub_subscription.dependencies_determined_dlq_error_processor_subscription.name
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.error_processor_invoker_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_topic_iam_member" "error_processor_cloud_run_account_dependencies_determined_dlq_pubsub_subscriber" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.dependencies_determined_queue_topic_dlq_id
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.error_processor_invoker_account.email}"
}

resource "google_pubsub_topic_iam_member" "error_processor_cloud_run_account_dependencies_determined_dlq_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.dependencies_determined_queue_topic_dlq_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.error_processor_invoker_account.email}"
}

resource "google_pubsub_subscription_iam_member" "dependencies_determined_dlq_pubsub_sa_subscription_member_account" {
  subscription = google_pubsub_subscription.dependencies_determined_dlq_error_processor_subscription.name
  role         = "roles/pubsub.subscriber"
  member       = "serviceAccount:service-${data.google_project.deadpendency_action_project.number}@gcp-sa-pubsub.iam.gserviceaccount.com"
  project      = data.google_project.deadpendency_action_project.project_id
}
