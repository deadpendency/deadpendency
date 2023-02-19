resource "google_cloud_run_service" "deadpendency_action_dependency_fetcher" {
  project  = data.google_project.deadpendency_action_project.project_id
  name     = "${var.app_env}-${var.app_live}-deadpendency-action-dependency-fetcher"
  location = "us-central1"

  template {
    spec {
      containers {
        image = "gcr.io/dgtw-deadpendency-action-2/deadpendency-action-dependency-fetcher:${var.app_version_sha}"
        resources {
          limits = {
            memory = "768Mi"
          }
        }
        env {
          name  = "DOWNSTREAM_QUEUE"
          value = google_pubsub_topic.deadpendency_action_pubsub_dependencies_fetched.id
        }
        env {
          name  = "FAILURE_QUEUE"
          value = var.processing_failure_queue_topic_id
        }
        env {
          name  = "DEAD_LETTER_QUEUE"
          value = var.dependencies_determined_queue_topic_dlq_id
        }
        env {
          name  = "REDIS_DATABASE_HOST"
          value = data.google_redis_instance.deadpendency_action_cache2.host
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
      container_concurrency = 20

      service_account_name = google_service_account.dependency_fetcher_cloud_run_account.email

      # ensure the sub ack_deadline_seconds is bigger than this
      timeout_seconds = 500
    }

    metadata {
      annotations = {
        "run.googleapis.com/vpc-access-connector" = "dp-cach-vpc-con2"
      }

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

data "google_iam_policy" "dependencies_determined_queue_pubsub_subscription_service_account_invoker" {
  binding {
    role = "roles/run.invoker"
    members = [
      "serviceAccount:${google_service_account.dependencies_determined_queue_dependency_fetcher_invoker_account.email}",
    ]
  }
}

resource "google_cloud_run_service_iam_policy" "deadpendency_action_dependency_fetcher_pubsub_invoker" {
  location = google_cloud_run_service.deadpendency_action_dependency_fetcher.location
  project  = data.google_project.deadpendency_action_project.project_id
  service  = google_cloud_run_service.deadpendency_action_dependency_fetcher.name

  policy_data = data.google_iam_policy.dependencies_determined_queue_pubsub_subscription_service_account_invoker.policy_data
}
