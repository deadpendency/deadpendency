resource "google_secret_manager_secret" "github_webhook_secret" {
  project   = data.google_project.deadpendency_action_project.project_id
  secret_id = "${var.app_env}-github-webhook-key-secret"

  replication {
    automatic = true
  }
}

resource "google_secret_manager_secret" "github_app_private_key_secret" {
  project   = data.google_project.deadpendency_action_project.project_id
  secret_id = "${var.app_env}-github-app-private-key-secret"

  replication {
    automatic = true
  }
}
