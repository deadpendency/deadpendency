terraform {
  backend "gcs" {
    bucket      = "dgtw-products-deadpendency-action-tf-state-central1"
    prefix      = "project"
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

module "modules" {
  source = "./modules"
}
