module Architecture.Order.Chambers
  ( composite
  , corynthian
  , doric
  , ionic
  , treatise
  , tuscan
  )
  where

import Prelude

import Architecture.Order (Order, OrderKind(..), Treatise, mkOrder, withPedestal)
import Architecture.Superposition (continuousTaper)
import Data.Maybe (Maybe(..))

-- After Chambers, A Treatise on the Decorative Part of Civil Architecture (Gwilt's 1862
-- edition). The module is half a diameter; columns are 14, 16, 18, 20 and 20 modules, every
-- entablature a fourth of its column and every base one module. Chambers sees "no reason for
-- diminishing the Tuscan column more" than the others, so all diminish to five sixths. Widths
-- and projections are not in the text: bases project a sixth of a diameter, Tuscan, Doric and
-- Ionic abaci are 7/6 of one, the architrave projects a sixth of its height beyond the top of
-- the shaft, the frieze is plumb with it and the cornice projects as far as it is high.
-- His pedestals are "three-tenths of the height of their respective columns", in nine parts:
-- cornice 1, base 2, die 6.
treatise :: Treatise
treatise =
  { tuscan: Just tuscan
  , doric: Just doric
  , ionic: Just ionic
  , corynthian: Just corynthian
  , composite: Just composite
  -- Chambers adopts Scamozzi's rule as "universally esteemed the best".
  , superposition: continuousTaper
  }

-- Entablature in ten parts: architrave 3, frieze 3, cornice 4; capital one module.
tuscan :: Order
tuscan = withPedestal { base: 28, die: 84, cornice: 14 } $ mkOrder Tuscan
  { columnHeight: 420
  , upperDiameter: 50
  , base: { width: 80, height: 30 }
  , capital: { width: 70, height: 30 }
  , architrave: { depth: 30, height: 32 }
  , frieze: { depth: 25, height: 31 }
  , cornice: { depth: 67, height: 42 }
  }

-- Entablature in eight parts: architrave 2, frieze 3, cornice 3; the capital 32 minutes,
-- after the theatre of Marcellus.
doric :: Order
doric = withPedestal { base: 32, die: 96, cornice: 16 } $ mkOrder Doric
  { columnHeight: 480
  , upperDiameter: 50
  , base: { width: 80, height: 30 }
  , capital: { width: 70, height: 32 }
  , architrave: { depth: 30, height: 30 }
  , frieze: { depth: 25, height: 45 }
  , cornice: { depth: 70, height: 45 }
  }

-- The entablature parts "bear the same proportion to each other ... as in the Tuscan". The
-- capital height is not given; Vignola's 2/3 of a module is used.
ionic :: Order
ionic = withPedestal { base: 36, die: 108, cornice: 18 } $ mkOrder Ionic
  { columnHeight: 540
  , upperDiameter: 50
  , base: { width: 80, height: 30 }
  , capital: { width: 70, height: 20 }
  , architrave: { depth: 32, height: 41 }
  , frieze: { depth: 25, height: 40 }
  , cornice: { depth: 79, height: 54 }
  }

-- "Differs but little from that of Vignola": capital 2 1/3 modules, architrave 1 1/2,
-- frieze 1 1/2, cornice 2.
corynthian :: Order
corynthian = withPedestal { base: 40, die: 120, cornice: 20 } $ mkOrder Corynthian
  { columnHeight: 600
  , upperDiameter: 50
  , base: { width: 80, height: 30 }
  , capital: { width: 85, height: 70 }
  , architrave: { depth: 33, height: 45 }
  , frieze: { depth: 25, height: 45 }
  , cornice: { depth: 85, height: 60 }
  }

-- Twenty modules with an entablature of five, and a capital "of the kind which all the
-- moderns have employed"; the parts are divided as in his Corinthian.
composite :: Order
composite = corynthian { kind = Composite }
