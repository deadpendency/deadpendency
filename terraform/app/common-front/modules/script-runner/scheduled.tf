# NO SMOKE
# resource "google_cloud_scheduler_job" "deadpendency_action_script_runner_scheduled_smoke" {
#   name             = "scheduled-smoke-test"
#   schedule         = "*/10 * * * *"
#   attempt_deadline = "60s"

#   count = var.app_env == "prod" ? 1 : 0

#   http_target {
#     http_method = "POST"
#     uri         = "https://${var.app_env}-${var.app_live}-deadpendency-action-script-runner-rnbiybubyq-uc.a.run.app/run-smoke-test?emit-metric=true"

#     oidc_token {
#       service_account_email = "general-cr-invoker-account@dgtw-deadpendency-action-2.iam.gserviceaccount.com"
#     }
#   }
# }

resource "google_cloud_scheduler_job" "deadpendency_action_script_runner_scheduled_total_installs" {
  name             = "emit-total-installs"
  schedule         = "0 * * * *" # hourly
  attempt_deadline = "60s"

  count = var.app_env == "prod" ? 1 : 0

  http_target {
    http_method = "POST"
    uri         = "https://${var.app_env}-${var.app_live}-deadpendency-action-script-runner-rnbiybubyq-uc.a.run.app/emit-total-installs"

    oidc_token {
      service_account_email = "general-cr-invoker-account@dgtw-deadpendency-action-2.iam.gserviceaccount.com"
    }
  }
}
