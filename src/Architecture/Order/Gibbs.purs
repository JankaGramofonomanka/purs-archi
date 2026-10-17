module Architecture.Order.Gibbs
  ( composite
  , corynthian
  , doric
  , ionic
  , treatise
  , tuscan
  )
  where

import Architecture.Order (Order, OrderKind(..), Treatise, mkOrder)
import Data.Maybe (Maybe(..))

-- After Gibbs, Rules for Drawing the Several Parts of Architecture (1753 edition). Columns
-- are 7, 8, 9, 10 and 10 diameters; entablatures a fourth of the Tuscan and Doric columns and
-- a fifth of the others; bases and the Tuscan, Doric and Ionic capitals half a diameter. Bases
-- project a third of the semidiameter on each side. Projections are measured from the top of
-- the shaft, with which the frieze is plumb.
treatise :: Treatise
treatise =
  { tuscan: Just tuscan
  , doric: Just doric
  , ionic: Just ionic
  , corynthian: Just corynthian
  , composite: Just composite
  }

-- Diminished a fifth of the semidiameter on each side. The capital projects a fourth of the
-- top diameter, the architrave a sixth of its height, the cornice as far as it is high.
-- Entablature in seven parts: architrave 2, frieze 2, cornice 3.
tuscan :: Order
tuscan = mkOrder Tuscan
  { columnHeight: 420
  , upperDiameter: 48
  , base: { width: 80, height: 30 }
  , capital: { width: 72, height: 30 }
  , architrave: { depth: 29, height: 30 }
  , frieze: { depth: 24, height: 30 }
  , cornice: { depth: 69, height: 45 }
  }

-- Diminished a sixth of the semidiameter on each side. The capital projects a fourth of the
-- top diameter, the architrave a sixth of its height, the cornice half the entablature.
-- Entablature in eight parts: architrave 2, frieze 3, cornice 3.
doric :: Order
doric = mkOrder Doric
  { columnHeight: 480
  , upperDiameter: 50
  , base: { width: 80, height: 30 }
  , capital: { width: 75, height: 30 }
  , architrave: { depth: 30, height: 30 }
  , frieze: { depth: 25, height: 45 }
  , cornice: { depth: 85, height: 45 }
  }

-- Diminished a sixth of the semidiameter on each side. The capital projects half the
-- semidiameter on each side, the architrave a fourth of its height, the cornice as far as it
-- is high. Entablature in five parts: architrave 1 1/2, frieze 1 1/2, cornice 2.
ionic :: Order
ionic = mkOrder Ionic
  { columnHeight: 540
  , upperDiameter: 50
  , base: { width: 80, height: 30 }
  , capital: { width: 80, height: 30 }
  , architrave: { depth: 33, height: 32 }
  , frieze: { depth: 25, height: 33 }
  , cornice: { depth: 68, height: 43 }
  }

-- "The height of the Capital is one Diameter and 1/6; the other dimensions are as in the
-- Ionick Order."
corynthian :: Order
corynthian = mkOrder Corynthian
  { columnHeight: 600
  , upperDiameter: 50
  , base: { width: 80, height: 30 }
  , capital: { width: 80, height: 70 }
  , architrave: { depth: 34, height: 36 }
  , frieze: { depth: 25, height: 36 }
  , cornice: { depth: 73, height: 48 }
  }

composite :: Order
composite = corynthian { kind = Composite }
