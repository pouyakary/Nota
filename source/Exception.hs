
module Exception where

-- ─── SHORTS ─────────────────────────────────────────────────────────────────────

type EEA = Exceptional error answer

-- ─── EXCEPTIONAL TYPE ───────────────────────────────────────────────────────────

data Exceptional error answer
    = Success answer
    | Exception error
      deriving ( Show )

-- ─── CORRESPONDING MONAD ────────────────────────────────────────────────────────

instance Monad ( Exceptional e ) where
    return                 =  Success
    Exception left  >>= _  =  Exception left
    Success  right  >>= x  =  x right

-- ─── MONAD FUNCTIONS ────────────────────────────────────────────────────────────

throw :: error -> EEA
throw = Exception

catch :: EEA -> ( error -> EEA ) -> EEA
catch ( Exception  left ) h  =  h left
catch ( Success right )   _  =  Success right

-- ────────────────────────────────────────────────────────────────────────────────
