# [configurator-ng](https://github.com/lpsmith/configurator-ng)
##What is this?

This is a massively breaking revision of the application interface of
[configurator].   The configuration file syntax is backward compatible,
and mostly forward compatible as well.   This fork is not (yet?) intended
for widespread public consumption.  Rather, this repo is being used as
a stopgap measure in some of my own projects as well as a playground
and laboratory for a new configurator-like package that may be
released sometime in the future.

## Manifesto

(Note that this section is mildly aspirational at a few points,
 and/or contains errors.)

The application interface of `configurator` has numerous problems:
  * it makes it easy to introduce race conditions
  * it makes it difficult to write an application that is relatively
    robust to misconfiguration errors
  * does not scale well to moderately complex configuration scenarios
  * the configuration change notifications are particularly difficult
    to use beyond the most trivial of use cases.

The aim of `configurator-ng` is to improve these issues,  with the
initial efforts focused on the first three.   I hope to make more
correct solutions easier,  and less correct solutions harder,  all
wrapped up in a more expressive interface.

### Race conditions

The interface of `configurator` basically is:

    data Config = Config (IORef (HashMap Text Value))

    lookup :: Configured a => Config -> Text -> IO (Maybe a)

The `IORef` is there to support configuration file reloading,  which
is often done automatically.  So this results in the race condition:

    lookup config "key0"
    reload config  {- in another thread -}
    lookup config "key1"

Thus,  we have taken `key0` and `key1` from two versions of the
configuration files,  with a overall result that is not necessarily
consistent with either version.

There is a way to solve this race condition\*, though it is by no
means convenient and it provides even less support for turning the
result into configuration parameters:

    getMap :: Config -> IO (HashMap Text Value)

This obtains a consistent\* snapshot of the configuration,  from which
you can pull out multiple values.   But in addition to being
less obvious and inconvenient,  the fact that the `HashMap` returned
is not an abstract type makes means that changing the representation
breaks client code that uses this approach.

`configurator-ng` makes the latter mode of use much more convenient by
introducing `ConfigParser`s,  a applicative/monadic high-level parsing
interface to read configuration info from a single snapshot.  See the
module `Data.Configurator.Parser`.  The basic ideas behind the revised
interface is as follows:

    data ConfigCache = ConfigCache (IORef Config)

    readConfig :: ConfigCache -> IO Config

    runParser :: ConfigParser m => m a -> Config -> (Maybe a, [ConfigError])

(Here,  `ConfigError` could be an error condition, or it might be more
analogous to a warning or informational message;  thus a parser can
return a result *and* some `ConfigError`s.)

Finally,  we could define a `ConfigParser` to read from key1 and key2
by writing:

    getKeys :: ConfigParser m => m (Text, Int)
    getKeys = (,) <$> key "key0" <*> key "key1"

(\*It's important to point out that `getMap` only avoids introducing
additional race conditions;  commonly used filesystems are racey
software artifacts,  so this is only consistent relative to filesystem
reads.  For a complete solution, one would have to take care in the
precise filesystem calls used to manipulate the configuration file(s).
Most popular text editors should be ok as far as the consistency of a
single file, consistent reads of multiple files is trickier.)

### Configuration validation

Another advantage of the `ConfigParser` interface is that it makes it
easier and more convenient to validate a (sub-)configuration as an
entirety,  and thus also make more intelligent decisions about to do
in cases of misconfigurations.  For example, one might want to continue
running on the last known good configuration,  and raise a big red
flag in a monitoring solution.   The goal is to provide mechanism,
not policy.

### Greater Expressive Power

Consider the following use case:   you have an event processor,  that
watches several named sources for events.   You might like your
configuration file to look something like this:

~~~
event-sources {
    amazon-cloud {
        postgres {
            host    = "cloudevents.mydomain.com"
            port    = 5433
            dbname  = "eventdb"
            sslmode = "verify-full"
            sslcert = "${HOME}/credentials/pgclient.crt"
            sslkey  = "${HOME}/credentials/pgclient.key"
        }
        heartbeat-interval = 15
        heartbeat-timeout  = 15
    }
    chicago-service-center {
        postgres {
            host    = "pgevents.customerdomain.com"
            port    = 5433
            dbname  = "eventdb"
            sslmode = "verify-full"
            sslcert = "${HOME}/credentials/pgclient.crt"
            sslkey  = "${HOME}/credentials/pgclient.key"
        }
        heartbeat-interval = 15
        heartbeat-timeout  = 15
    }
}
~~~

Now, `amazon-cloud` and `chicago-service-center` are names of the source
useful for whatever purposes (logging, API endpoints, etc), that the
event processor doesn't know about in advance.  Since `configurator`
is tied down to `HashMap`,  the data structure offers no support for
efficiently discovering these names.  In order to fix this,
`configurator-ng` moved to [`critbit`][critbit]. which allows us to
efficiently iterate over these keys (in alphabetical order).  So
`configurator-ng` offers the following operator:

    subgroups :: ConfigParser m => Text -> m [Text]

`subgroups` returns the non-empty value groupings of it's argument,
so for example when evaluated in the context of the configuration above:

    subgroups ""              ==> [ "event-sources" ]

    subgroups "event-sources" ==> [ "event-sources.amazon-cloud"
                                  , "event-sources.chicago-service-center" ]

Another issue is that there's a lot of redundancy here,  so maybe we'd like to
refactor the configuration file into something like this:

~~~
event-sources {
    amazon-cloud {
        postgres { host = "cloudevents.mydomain.com" }
    }
    chicago-service-center {
        postgres { host = "pgevents.customerdomain.com" }
    }
    default {
        postgres {
            port    = 5433
            dbname  = "eventdb"
            sslmode = "verify-full"
            sslcert = "${HOME}/credentials/pgclient.crt"
            sslkey  = "${HOME}/credentials/pgclient.key"
        }
        heartbeat-interval = 15
        heartbeat-timeout  = 15
    }
}
~~~

So now the problem is that we want to turn this configuration into a
list of `EventSource`s:

~~~
data EventSource = EventSource {
    name              :: !Text,
    libpqConnParams   :: [(Text,Value)],
    heartbeatInterval :: !Micro,
    heartbeatTimeout  :: !Micro,
  }
~~~

Now, even ignoring the issue of the names mentioned above,  handling
this sort of customizable defaulting in `configurator` would be rather
painful.   But it's actually quite easy with `configurator-ng`:

~~~
{-# LANGUAGE ApplicativeDo, RecordWildCards #-}

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f = foldr (liftA2 (:)) (pure []) . map f

eventSources :: ConfigParserA [EventSource]
eventSources = do
    localConfig (subconfig "event-sources") $ do
        mapA eventSource . filter (/= "default") <$> subgroups ""

eventSource :: Text -> ConfigParserA EventSource
eventSource name = do
    localConfig (union (subconfig name     )
                       (subconfig "default")) $ do
        libpqConnParams   <- localConfig (subconfig "postgres") (subassocs "")
        heartbeatInterval <- key "heartbeat-interval"
        heartbeatTimeout  <- key "heartbeat-timeout"
        pure $! EventSource{..}
~~~

This example uses the `ConfigParserA` variant of `ConfigParser`, so
that the parser continues to run after encountering an error in order
to generate more error messages.   It also uses `localConfig` operator
to run a subparser in a  different configuration context.   There are
a few operators for modifying the configuration context:

~~~
localConfig :: ConfigParser m => (Config -> Config) -> m a -> m a

-- | Left-biased union of two configurations
union :: Config -> Config -> Config

-- | Restrict a configuration to a given group,  and remove that group
--   prefix from all key names.
subconfig :: Text -> Config -> Config

-- | Add a group name as a prefix to all key names
superconfig :: Text -> Config -> Config
~~~

Note that these operators are implemented "symbolically",  so that
they run in sub-linear (Possibly `O(1)`?) time.  Instead,  the cost of
these are paid on each `(key,value)` lookup.

As a convenience for this sort of use case, the configuration file
syntax has been extended to include datum comments,  not unlike those
found in Scheme and Clojure.   For example,  one can disable
`chicago-service-center` by including a `#;` token before the group name;
the next binding must be syntactically correct,  but will be otherwise
ignored by the low-level parser.

### Configuration Change Subscriptions

`Configurator`'s change notification system is also painful to
use except in the most trivial of cases,  not least because
the callback is called for a single changed `(key,value)` pair at a
time.  Determining how that impacts a given configuration record (like
`EventSource` above) is up to the user.

Soon,  configurator-ng will offer something along the lines of the
following function:

~~~
subscribe :: ConfigParser m => ConfigCache -> m a -> (a -> IO ()) -> IO ()
~~~

When the configuration files are reloaded,  every subscribed
`ConfigParser` is rerun,  and the result is passed on to the
callback.  Now, of course,  many callbacks won't want to be called
unless their configuration actually changes.   However,  this is
actually a reasonable thing to punt to the callback,  because
we can write a generic callback wrapper to handle this issue:

~~~
debounce :: (a -> a -> Bool) -> (a -> IO ()) -> IO (a -> IO ())
debounce notEq callback = do
    last_seen <- newIORef Nothing
    return $ \new -> do
        m_old <- readIORef last_seen
        if   case m_old of
               Nothing  -> True
               Just old -> notEq old new
        then do
          writeIORef last_seen
          callback new
        else do
          return ()
~~~

#### Optimizing subscribe

It would be more efficient to run only those `ConfigParser`s that have
the possibility of changing.  If we design the `configurator-ng`
interface carefully,  we can determine all the keys that a parser
depends on.  We can then use this information to rerun only those
parsers whose result might possibly change.  (Though,  `debounce` could
still be useful,  as `ConfigParser`s aren't guaranteed to be 1-1
functions.)

However,  once we have dependency tracking that works,  there are
further applications this could enable,  such as:
   * providing tools to sysadmins to understand which parts of
     the configuration files affect which parts of the system.
   * finding values in configuration files that have no effect at all
   * more speculatively,  using this information to generate sample
     configuration files.

 [configurator]: https://hackage.haskell.org/package/configurator
 [critbit]: https://hackage.haskell.org/package/critbit
