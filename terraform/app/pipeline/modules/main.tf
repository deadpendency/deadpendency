variable "app_version" {
  type = string
}

variable "app_version_sha" {
  type = string
}

variable "app_env" {
  type = string
}

variable "app_live" {
  type = string
}

variable "app_id" {
  type = string
}

variable "minor_notification_channel" {
  type = string
}

variable "critical_notification_channel" {
  type = string
}

module "failure_modules" {
  source = "./failure"

  app_version = var.app_version
  app_env     = var.app_env
  app_live    = var.app_live
}

module "front_door_modules" {
  source = "./front-door"

  app_version     = var.app_version
  app_version_sha = var.app_version_sha
  app_env         = var.app_env
  app_live        = var.app_live
}

module "check_run_creator_modules" {
  source = "./check-run-creator"

  app_version                       = var.app_version
  app_version_sha                   = var.app_version_sha
  app_env                           = var.app_env
  app_live                          = var.app_live
  app_id                            = var.app_id
  minor_notification_channel        = var.minor_notification_channel
  init_queue_topic_id               = module.front_door_modules.init_queue_topic_id
  init_queue_topic_dlq_id           = module.front_door_modules.init_queue_topic_dlq_id
  processing_failure_queue_topic_id = module.failure_modules.processing_failure_queue_topic_id
}

module "run_preparer_modules" {
  source = "./run-preparer"

  app_version                       = var.app_version
  app_version_sha                   = var.app_version_sha
  app_env                           = var.app_env
  app_live                          = var.app_live
  app_id                            = var.app_id
  minor_notification_channel        = var.minor_notification_channel
  check_run_created_queue_topic_id               = module.check_run_creator_modules.check_run_created_queue_topic_id
  check_run_created_queue_topic_dlq_id           = module.check_run_creator_modules.check_run_created_queue_topic_dlq_id
  processing_failure_queue_topic_id = module.failure_modules.processing_failure_queue_topic_id
}

module "dependency_determiner_modules" {
  source = "./dependency-determiner"

  app_version                        = var.app_version
  app_version_sha                    = var.app_version_sha
  app_env                            = var.app_env
  app_live                           = var.app_live
  app_id                             = var.app_id
  run_prepared_queue_topic_id     = module.run_preparer_modules.run_prepared_queue_topic_id
  run_prepared_queue_topic_dlq_id = module.run_preparer_modules.run_prepared_queue_topic_dlq_id
  processing_failure_queue_topic_id  = module.failure_modules.processing_failure_queue_topic_id
  minor_notification_channel         = var.minor_notification_channel
}

module "dependency_fetcher_modules" {
  source = "./dependency-fetcher"

  app_version                                = var.app_version
  app_version_sha                            = var.app_version_sha
  app_env                                    = var.app_env
  app_live                                   = var.app_live
  app_id                                     = var.app_id
  dependencies_determined_queue_topic_id     = module.dependency_determiner_modules.dependencies_determined_queue_topic_id
  dependencies_determined_queue_topic_dlq_id = module.dependency_determiner_modules.dependencies_determined_queue_topic_dlq_id
  processing_failure_queue_topic_id          = module.failure_modules.processing_failure_queue_topic_id
  minor_notification_channel                 = var.minor_notification_channel
}

module "report_generator_modules" {
  source = "./report-generator"

  app_version                             = var.app_version
  app_version_sha                         = var.app_version_sha
  app_env                                 = var.app_env
  app_live                                = var.app_live
  dependencies_fetched_queue_topic_id     = module.dependency_fetcher_modules.dependencies_fetched_queue_topic_id
  dependencies_fetched_queue_topic_dlq_id = module.dependency_fetcher_modules.dependencies_fetched_queue_topic_dlq_id
  processing_failure_queue_topic_id       = module.failure_modules.processing_failure_queue_topic_id
  minor_notification_channel              = var.minor_notification_channel
}

module "run_finalizer_modules" {
  source = "./run-finalizer"

  app_version                           = var.app_version
  app_version_sha                       = var.app_version_sha
  app_env                               = var.app_env
  app_live                              = var.app_live
  app_id                                = var.app_id
  report_generated_queue_topic_id       = module.report_generator_modules.report_generated_queue_topic_id
  report_generated_queue_topic_dlq_id   = module.report_generator_modules.report_generated_queue_topic_dlq_id
  processing_failure_queue_topic_id     = module.failure_modules.processing_failure_queue_topic_id
  processing_failure_queue_topic_dlq_id = module.failure_modules.processing_failure_queue_topic_dlq_id
  minor_notification_channel            = var.minor_notification_channel
}

module "error_processor_modules" {
  source = "./error-processor"

  app_version                                = var.app_version
  app_version_sha                            = var.app_version_sha
  app_env                                    = var.app_env
  app_live                                   = var.app_live
  app_id                                     = var.app_id
  processing_failure_queue_topic_id          = module.failure_modules.processing_failure_queue_topic_id
  check_run_created_queue_topic_dlq_id       = module.check_run_creator_modules.check_run_created_queue_topic_dlq_id
  run_prepared_queue_topic_dlq_id            = module.run_preparer_modules.run_prepared_queue_topic_dlq_id
  dependencies_determined_queue_topic_dlq_id = module.dependency_determiner_modules.dependencies_determined_queue_topic_dlq_id
}

module "script_runner_modules" {
  source = "./script-runner"

  app_version                                = var.app_version
  app_version_sha                            = var.app_version_sha
  app_env                                    = var.app_env
  app_live                                   = var.app_live
  app_id                                     = var.app_id
  minor_notification_channel                 = var.minor_notification_channel
  init_queue_topic_id                        = module.front_door_modules.init_queue_topic_id
  check_run_created_queue_topic_id           = module.check_run_creator_modules.check_run_created_queue_topic_id
  run_prepared_queue_topic_id                = module.run_preparer_modules.run_prepared_queue_topic_id
  dependencies_determined_queue_topic_id     = module.dependency_determiner_modules.dependencies_determined_queue_topic_id
  dependencies_fetched_queue_topic_id        = module.dependency_fetcher_modules.dependencies_fetched_queue_topic_id
  report_generated_queue_topic_id            = module.report_generator_modules.report_generated_queue_topic_id
}
