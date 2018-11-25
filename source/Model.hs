
module Model where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Scientific

-- ─── PROTOTYPE ──────────────────────────────────────────────────────────────────

type ScopePrototype = Map String Scientific

-- ─── MODEL ──────────────────────────────────────────────────────────────────────

data Model = Model { history      :: [ String ]
                   , prototype    :: ScopePrototype
                   } deriving ( Show )

-- ─── INITIAL MODEL ──────────────────────────────────────────────────────────────

initalModel = Model { history   = [ ]
                    , prototype = Map.fromList [ ( "ans", 0 ) ]
                    }

-- ────────────────────────────────────────────────────────────────────────────────
