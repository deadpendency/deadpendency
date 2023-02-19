resource "google_cloud_run_service" "deadpendency_action_front_door" {
  project  = data.google_project.deadpendency_action_project.project_id
  name     = "${var.app_env}-${var.app_live}-deadpendency-action-front-door"
  location = "us-central1"

  template {
    spec {
      containers {
        image = "gcr.io/dgtw-deadpendency-action-2/deadpendency-action-front-door:${var.app_version_sha}"
        env {
          name  = "GITHUB_WEBHOOK_SECRET_NAME"
          value = "${var.app_env}-github-webhook-key-secret"
        }
        env {
          name  = "DOWNSTREAM_QUEUE"
          value = google_pubsub_topic.deadpendency_action_pubsub_init_run.id
        }
        env {
          name  = "APP_VERSION"
          value = var.app_version
        }
        env {
          name  = "APP_ENV"
          value = var.app_env
        }
        env {
          name  = "APP_LIVE"
          value = var.app_live
        }
      }

      service_account_name = google_service_account.front_door_cloud_run_account.email

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

data "google_iam_policy" "noauth" {
  binding {
    role = "roles/run.invoker"
    members = [
      "allUsers",
    ]
  }
}

resource "google_cloud_run_service_iam_policy" "deadpendency_action_front_door_noauth" {
  location = google_cloud_run_service.deadpendency_action_front_door.location
  project  = data.google_project.deadpendency_action_project.project_id
  service  = google_cloud_run_service.deadpendency_action_front_door.name

  policy_data = data.google_iam_policy.noauth.policy_data
}
