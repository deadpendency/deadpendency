variable "APP_LIVE" {
  type = string
}

variable "APP_LIVE_HOST" {
  type = string
}

terraform {
  backend "gcs" {
    bucket      = "dgtw-products-deadpendency-action-tf-state-central1"
    prefix      = "app-common-front-preprod"
    credentials = "/tmp/credentials.json"
  }

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 4.43.1"
    }
  }
}

provider "google" {
  region      = "us-central1"
  credentials = "/tmp/credentials.json"
}

data "google_project" "deadpendency_action_project" {
  project_id = "dgtw-deadpendency-action-2"
}

data "google_monitoring_notification_channel" "dev_notification_channel" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "Dev DA Alert"
}

module "modules" {
  source = "../modules"

  app_env       = "preprod"
  app_live      = var.APP_LIVE
  app_live_host = var.APP_LIVE_HOST

  minor_notification_channel    = data.google_monitoring_notification_channel.dev_notification_channel.name
  critical_notification_channel = data.google_monitoring_notification_channel.dev_notification_channel.name
}
