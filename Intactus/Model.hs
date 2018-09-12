
module Model where

-- ─── MODEL ──────────────────────────────────────────────────────────────────────

data Model = Model { promptNumber :: Int
                   , history      :: [String]
                   } deriving (Show)

-- ─── INITIAL MODEL ──────────────────────────────────────────────────────────────

initalModel = Model { promptNumber = 1
                    , history      = []
                    }

-- ────────────────────────────────────────────────────────────────────────────────
