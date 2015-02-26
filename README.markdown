This is a massively breaking fork of Configurator.   It's not intended
for public consumption and will never be officially released.  Rather,
this repo is being used as a stopgap measure in some of my own
projects as well as a playground and laboratory for a new
Configurator-like package that may be released sometime in the
future.

The intention is that this new implementation will retain a high level
of compatibility with Configurator as far as configuration file syntax
is concerned.   However,  I've been deeply unhappy with the Haskell
API that Configurator provides to get access to those files,  for
many reasons.


Consider the following use case:   you have an event processor,  that
watches several named sources for events and then processes them into
some result on a target.   You might like your configuration file to look
something like this:


~~~
my-event-processor {
  sources {
    event-source-01 {
      postgresql {
        host = "pg.mydomain.com"
        port = 5433
      }
    }
    event-source-02 {
      postgresql {
        host = "10.0.0.24"
        dbname = "eventdb2"
      }
      heartbeat-interval = 15
      reconnect-strategy = "fixed"
      reconnect-interval = 5
    }
    some-arbitrary-event-source-name {
      http-event-stream {
        url = "http://www.somewhere.com/event-source"
      }
    }
    default {
      postgresql {
        dbname      = "eventdb"
        sslmode     = "verify-full"
        sslrootcert = "path/to/root.crt"
        sslcert     = "path/to/postgresql.crt"
        sslkey      = "path/to/postgresql.key"
      }
      heartbeat-interval = 60
      reconnect-strategy = "exponential-backoff"
      reconnect-timeout  = 10
    }
  }
  target {
    postgresql {
       host = "/var/run/postgresql"
       dbname = "eventlog"
    }
  }
}
~~~

Note that here,  we have 3 named event sources,  as well as a group of
default values to use for each source.   Obtaining each configuration
binding in the old event-processor was neither efficient nor
convenient.

It's not efficient because I'm not assuming that the event processor
knows the names `event-source-01`,  `event-source-02`, or
`some-arbitrary-event-source-name`.   To fix this,  I moved from
`HashMap` to `CritBit`,  and implemented a function that can
reasonably efficiently list all the non-empty subgroups of any given
group.

In order to make gathering this information concise and convenient,
so far I've introduce the `ConfigPlan` datatype in
`Data.Configurator.ConfigMap`, which allows for union, subgrouping,
and re-grouping,  as well as the `ConfigParser` applicative functor
which makes repeated lookups more convenient as well as generating
comprehensive error messages.

As a convenience,  the configuration file syntax has been extended
to include group comments,  not unlike datum comments in Scheme
and Closure.   For example,  one can disable `event-source-01` by
including a `#;` token before the group name.

Configurator's change notification system is also painful to
use except in the most trivial of cases.  My tentative idea for the
future is to have something like

~~~
subscribe :: Config -> ConfigParser a -> (a -> IO ()) -> IO ()
~~~

This function would invoke the callback any time the result from
`ConfigParser` changes.

Note that there are lots of rough edges with the current API,
some trivial,  some fairly fundamental.
