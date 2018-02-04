module Gdax.Util.Time where

import           Data.Time.Clock

type StartTime = UTCTime

type EndTime = UTCTime

type Granularity = NominalDiffTime

second :: NominalDiffTime
second = 1

minute :: NominalDiffTime
minute = 60 * second

hour :: NominalDiffTime
hour = 60 * minute

day :: NominalDiffTime
day = 24 * hour

week :: NominalDiffTime
week = 7 * day

month :: NominalDiffTime
month = 30 * day
