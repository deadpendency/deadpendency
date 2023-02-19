variable "app_env" {
  type = string
}

variable "app_live" {
  type = string
}

variable "minor_notification_channel" {
  type = string
}

variable "critical_notification_channel" {
  type = string
}

variable "app_live_host" {
  type = string
}

data "google_project" "deadpendency_action_project" {
  project_id = "dgtw-deadpendency-action-2"
}

module "script_runner_modules" {
  source = "./script-runner"

  app_env                                    = var.app_env
  app_live                                   = var.app_live
  minor_notification_channel                 = var.minor_notification_channel
}
