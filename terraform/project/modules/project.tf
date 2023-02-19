data "google_billing_account" "billing" {
  display_name = "My Billing Account"
  open         = true
}

data "google_folder" "deadpendency_folder" {
  folder = "folders/261219178903" # apparently no way to fetch this by name
}

# deadpendency_project mismatch with deadpendency-action?
resource "google_project" "deadpendency_project" {
  name            = "Deadpendency Action"
  folder_id       = data.google_folder.deadpendency_folder.id
  project_id      = "dgtw-deadpendency-action-2"
  billing_account = data.google_billing_account.billing.id
}
