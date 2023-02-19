resource "google_cloud_run_domain_mapping" "front_door_custom_domain_mapping" {
  location = "us-central1"
  name     = "${var.app_env}.deadpendency-service.com"
  project  = data.google_project.deadpendency_action_project.project_id

  metadata {
    namespace = data.google_project.deadpendency_action_project.project_id
  }

  spec {
    # hardcoded until there is a cloud run data source
    route_name = "${var.app_env}-deadpendency-action-front-proxy"
  }
}
