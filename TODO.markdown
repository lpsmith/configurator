 * Implement `Alternative` instances for `ConfigParser`,  as well as
   the `fail` method.

 * Strongly consider renaming the modules,  as to not conflict with
   `configurator`.

 * Reimplement subscriptions to configuration changes

 * General reworking of the inherited `configurator` interface.

 * Strongly consider labelling each value with the filename it
   originated from,  for better error reporting.

 * Write some functions to help turn `ConfigErrors` into messages
   suitable for logging and/or human consumption.
