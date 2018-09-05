
module Model where
    
-- Model -----------------------------------------
    
data Model = Model { promptNumber :: Int
                   , history :: [String]
                   } deriving (Show)
                   


-- Initial Model ---------------------------------

initalModel = Model { promptNumber = 1
                    , history = []
                    } 
                    

