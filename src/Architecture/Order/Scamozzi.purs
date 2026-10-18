module Architecture.Order.Scamozzi
  ( composite
  , corynthian
  , doric
  , ionic
  , treatise
  , tuscan
  )
  where

import Prelude

import Architecture.Order (Order, OrderKind(..), Treatise, mkOrder, mkPedestal, withPedestal)
import Architecture.Superposition (continuousTaper)
import Data.Maybe (Maybe(..))

-- After Scamozzi, L'idea dell'architettura universale, Book VI, in "The Mirror of
-- Architecture" (1721), which measures in a diameter of sixty minutes. Columns are 7 1/2,
-- 8 1/2, 8 3/4, 9 3/4 (Composite, his "Roman") and 10 diameters; entablatures a fourth of the
-- Tuscan and Doric columns and a fifth of the others; bases half a diameter. The columns
-- diminish by a fourth, fifth, sixth, seventh and eighth. Cornices project as far as they are
-- high ("a perfect square of the whole projecture"). Base and Tuscan, Doric and Ionic capital
-- widths are not given: bases project a sixth of a diameter and those abaci 7/6 of one; the
-- frieze is plumb with the top of the shaft and the architrave projects a sixth of its height.
-- Half minutes are rounded.
-- Pedestals of 1 7/8, 2 4/15, 2 1/2, 3 1/3 and 3 diameters (Tuscan, Doric, Ionic,
-- Corinthian, Composite), with a base of 45 minutes (30 for the Tuscan) and a cimacium of
-- 22 1/2, per the Mirror and Chambers' table.
treatise :: Treatise
treatise =
  { tuscan: Just tuscan
  , doric: Just doric
  , ionic: Just ionic
  , corynthian: Just corynthian
  , composite: Just composite
  -- Book VI, chapter 11: each column as thick below as the one beneath it is above.
  , superposition: Just continuousTaper
  }

-- Entablature 1 7/8 diameters: architrave 32 1/2 minutes, frieze 41, cornice 39.
tuscan :: Order
tuscan = withPedestal { base: 30, die: 60, cornice: 23 } $ mkOrder Tuscan
  { columnHeight: 450
  , upperDiameter: 45
  , base: { width: 80, height: 30 }
  , capital: { width: 70, height: 30 }
  , architrave: { depth: 28, height: 33 }
  , frieze: { depth: 23, height: 41 }
  , cornice: { depth: 62, height: 39 }
  }

-- Entablature 2 1/8 diameters: architrave 35, frieze 45 with a list of 5, cornice 42.
doric :: Order
doric = withPedestal { base: 45, die: 69, cornice: 22 } $ mkOrder Doric
  { columnHeight: 510
  , upperDiameter: 48
  , base: { width: 80, height: 30 }
  , capital: { width: 70, height: 30 }
  , architrave: { depth: 30, height: 35 }
  , frieze: { depth: 24, height: 50 }
  , cornice: { depth: 66, height: 42 }
  }

-- The capital rises 18 5/6 minutes above the astragal. Entablature 1 3/4 diameters:
-- architrave 35, frieze 28, cornice 42.
ionic :: Order
ionic = withPedestal { base: 45, die: 83, cornice: 22 } $ mkOrder Ionic
  { columnHeight: 525
  , upperDiameter: 50
  , base: { width: 80, height: 30 }
  , capital: { width: 70, height: 19 }
  , architrave: { depth: 31, height: 35 }
  , frieze: { depth: 25, height: 28 }
  , cornice: { depth: 67, height: 42 }
  }

-- The capital is 70 minutes high on an abacus drawn in a square of 1 1/2 diameters.
-- Entablature 2 diameters: architrave 40, frieze 32, cornice 48.
corynthian :: Order
corynthian = withPedestal { base: 45, die: 133, cornice: 22 } $ mkOrder Corynthian
  { columnHeight: 600
  , upperDiameter: 53
  , base: { width: 80, height: 30 }
  , capital: { width: 90, height: 70 }
  , architrave: { depth: 33, height: 40 }
  , frieze: { depth: 26, height: 32 }
  , cornice: { depth: 74, height: 48 }
  }

-- The capital as the Corinthian, the top of the shaft "fifty one minutes". Entablature
-- 1 57/60 diameters: architrave 39, frieze 31 1/4, cornice 46 3/4.
composite :: Order
composite = withPedestalOf84 $ mkOrder Composite
  { columnHeight: 585
  , upperDiameter: 51
  , base: { width: 80, height: 30 }
  , capital: { width: 90, height: 70 }
  , architrave: { depth: 32, height: 39 }
  , frieze: { depth: 26, height: 31 }
  , cornice: { depth: 73, height: 47 }
  }
  where
  -- The neck (die) of the Composite pedestal is "one model twenty four minutes" broad.
  withPedestalOf84 order = order
    { pedestal = Just $ mkPedestal
        { base: { width: 118, height: 45 }
        , die: { width: 84, height: 113 }
        , cornice: { width: 128, height: 22 }
        }
    }
