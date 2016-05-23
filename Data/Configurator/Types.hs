-- |
-- Module:      Data.Configurator.Types
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2015-2016 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with configuration files.

module Data.Configurator.Types
    (
      AutoConfig(..)
    , ConfigCache
    , Name
    , Value(..)
    , Configured, convert
    , Worth(..)
    , ConfigErrorWhy(..)
    -- * Exceptions
    , ParseError(..)
    , ConfigError(..)
    , KeyError(..)
    -- * Notification of configuration changes
    , Pattern
    , ChangeHandler
    ) where

import Data.Configurator.Types.Internal
