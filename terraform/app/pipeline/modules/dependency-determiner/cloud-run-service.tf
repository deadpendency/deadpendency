resource "google_cloud_run_service" "deadpendency_action_dependency_determiner" {
  project  = data.google_project.deadpendency_action_project.project_id
  name     = "${var.app_env}-${var.app_live}-deadpendency-action-dependency-determiner"
  location = "us-central1"

  template {
    spec {
      containers {
        image = "gcr.io/dgtw-deadpendency-action-2/deadpendency-action-dependency-determiner:${var.app_version_sha}"
        resources {
          limits = {
            memory = "256Mi"
          }
        }
        env {
          name  = "DOWNSTREAM_QUEUE"
          value = google_pubsub_topic.deadpendency_action_pubsub_dependencies_determined.id
        }
        env {
          name  = "FAILURE_QUEUE"
          value = var.processing_failure_queue_topic_id
        }
        env {
          name  = "DEAD_LETTER_QUEUE"
          value = var.run_prepared_queue_topic_dlq_id
        }
        env {
          name  = "GITHUB_PRIVATE_KEY_SECRET_NAME"
          value = "${var.app_env}-github-app-private-key-secret"
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
      service_account_name = google_service_account.dependency_determiner_cloud_run_account.email

      # ensure the sub ack_deadline_seconds is bigger than this
      timeout_seconds = 120
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

data "google_iam_policy" "run_prepared_pubsub_subscription_service_account_invoker" {
  binding {
    role = "roles/run.invoker"
    members = [
      "serviceAccount:${google_service_account.run_prepared_queue_dependency_determiner_invoker_account.email}",
    ]
  }
}

resource "google_cloud_run_service_iam_policy" "deadpendency_action_dependency_determiner_pubsub_invoker" {
  location = google_cloud_run_service.deadpendency_action_dependency_determiner.location
  project  = data.google_project.deadpendency_action_project.project_id
  service  = google_cloud_run_service.deadpendency_action_dependency_determiner.name

  policy_data = data.google_iam_policy.run_prepared_pubsub_subscription_service_account_invoker.policy_data
}
