resource "google_cloud_run_service" "deadpendency_action_front_proxy" {
  project  = data.google_project.deadpendency_action_project.project_id
  name     = "${var.app_env}-deadpendency-action-front-proxy"
  location = "us-central1"

  template {
    spec {
      containers {
        image = "gcr.io/dgtw-deadpendency-action-2/deadpendency-action-app-front:3"
        env {
          name  = "APP_LIVE_HOST"
          value = var.app_live_host
        }
      }

      service_account_name = google_service_account.front_proxy_cloud_run_account.email

      timeout_seconds = 60
    }

    metadata {
      labels = {
        app_env = var.app_env
        app_live = var.app_live
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

resource "google_cloud_run_service_iam_policy" "deadpendency_action_front_proxy_noauth" {
  location = google_cloud_run_service.deadpendency_action_front_proxy.location
  project  = data.google_project.deadpendency_action_project.project_id
  service  = google_cloud_run_service.deadpendency_action_front_proxy.name

  policy_data = data.google_iam_policy.noauth.policy_data
}
