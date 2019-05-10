
module Model where

-- ─── IMPORTS ────────────────────────────────────────────────────────────────────

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Scientific
import           Data.Number.Fixed

-- ─── PROTOTYPE ──────────────────────────────────────────────────────────────────

type P50 = Fixed Prec50
type ScopePrototype = Map String P50

-- ─── MODEL ──────────────────────────────────────────────────────────────────────

data Model = Model { history      :: [ String ]
                   , prototype    :: ScopePrototype
                   } deriving ( Show )

-- ─── INITIAL MODEL ──────────────────────────────────────────────────────────────

initalModel = Model { history   = [ ]
                    , prototype = Map.fromList [ ( "ans", 0 ) ]
                    }

-- ────────────────────────────────────────────────────────────────────────────────
