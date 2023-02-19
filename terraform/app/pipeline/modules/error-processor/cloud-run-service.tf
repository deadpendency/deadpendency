resource "google_cloud_run_service" "deadpendency_action_error_processor" {
  project  = data.google_project.deadpendency_action_project.project_id
  name     = "${var.app_env}-${var.app_live}-deadpendency-action-error-processor"
  location = "us-central1"

  template {
    spec {
      containers {
        image = "gcr.io/dgtw-deadpendency-action-2/deadpendency-action-error-processor:${var.app_version_sha}"
        env {
          name  = "FAILURE_QUEUE"
          value = var.processing_failure_queue_topic_id
        }
        env {
          name  = "APP_ID"
          value = var.app_id
        }
        env {
          name  = "APP_ENV"
          value = var.app_env
        }
        env {
          name  = "APP_VERSION"
          value = var.app_version
        }
        env {
          name  = "APP_LIVE"
          value = var.app_live
        }
      }
      service_account_name = google_service_account.error_processor_cloud_run_account.email

      timeout_seconds = 60
    }

    metadata {
      labels = {
        app_env = var.app_env
      }
    }
  }

  traffic {
    percent         = 100
    latest_revision = true
  }
}

data "google_iam_policy" "error_processor_pubsub_subscription_service_account_invoker" {
  binding {
    role = "roles/run.invoker"
    members = [
      "serviceAccount:${google_service_account.error_processor_invoker_account.email}",
    ]
  }
}

resource "google_cloud_run_service_iam_policy" "deadpendency_action_error_processor_pubsub_invoker" {
  location = google_cloud_run_service.deadpendency_action_error_processor.location
  project  = data.google_project.deadpendency_action_project.project_id
  service  = google_cloud_run_service.deadpendency_action_error_processor.name

  policy_data = data.google_iam_policy.error_processor_pubsub_subscription_service_account_invoker.policy_data
}
