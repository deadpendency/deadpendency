resource "google_cloud_run_service" "deadpendency_action_run_finalizer" {
  project  = data.google_project.deadpendency_action_project.project_id
  name     = "${var.app_env}-${var.app_live}-deadpendency-action-run-finalizer"
  location = "us-central1"

  template {
    spec {
      containers {
        image = "gcr.io/dgtw-deadpendency-action-2/deadpendency-action-run-finalizer:${var.app_version_sha}"
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
      service_account_name = google_service_account.run_finalizer_cloud_run_account.email

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

data "google_iam_policy" "run_finalizer_subscription_service_account_invoker" {
  binding {
    role = "roles/run.invoker"
    members = [
      "serviceAccount:${google_service_account.report_generated_queue_run_finalizer_invoker_account.email}",
      "serviceAccount:${google_service_account.processing_failure_queue_run_finalizer_invoker_account.email}",
    ]
  }
}

resource "google_cloud_run_service_iam_policy" "deadpendency_action_run_finalizer_pubsub_invoker" {
  location = google_cloud_run_service.deadpendency_action_run_finalizer.location
  project  = data.google_project.deadpendency_action_project.project_id
  service  = google_cloud_run_service.deadpendency_action_run_finalizer.name

  policy_data = data.google_iam_policy.run_finalizer_subscription_service_account_invoker.policy_data
}
