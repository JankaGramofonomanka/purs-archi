module Architecture.Order.Vignola
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
import Data.Maybe (Maybe(..))

-- After Vignola, Regola delli cinque ordini (Moxon's translation, "Vignola, or the Compleat
-- Architect", 1655). The module is half a diameter (30 minutes here); the columns are 14, 16,
-- 18, 20 and 20 modules with base and capital, each entablature a fourth of its column and
-- each base one module. Vignola diminishes the columns to five sixths. Widths and projections
-- are not in the text: the abaci follow Ware's American Vignola (7/6 of a diameter for the
-- Tuscan and Doric), the bases project a sixth of a diameter, the architrave a sixth of its
-- height beyond the top of the shaft, the frieze is plumb with it, and the cornice projects
-- as far as it is high.
-- Pedestals are a third of the column "in all the five orders", but the Corinthian (and so
-- the Composite) "may be made of 7 Models" for strength. Moxon's text does not divide them;
-- Ware's American Vignola gives the cap a ninth and the base two ninths of the pedestal.
treatise :: Treatise
treatise =
  { tuscan: Just tuscan
  , doric: Just doric
  , ionic: Just ionic
  , corynthian: Just corynthian
  , composite: Just composite
  }

-- Architrave 1 module, frieze 1 1/6, cornice 1 1/3; capital 1.
tuscan :: Order
tuscan = withPedestal { base: 31, die: 93, cornice: 16 } $ mkOrder Tuscan
  { columnHeight: 420
  , upperDiameter: 50
  , base: { width: 80, height: 30 }
  , capital: { width: 70, height: 30 }
  , architrave: { depth: 30, height: 30 }
  , frieze: { depth: 25, height: 35 }
  , cornice: { depth: 65, height: 40 }
  }

-- Architrave 1 module, frieze 1 1/2, cornice 1 1/2; capital 1.
doric :: Order
doric = withPedestal { base: 36, die: 106, cornice: 18 } $ mkOrder Doric
  { columnHeight: 480
  , upperDiameter: 50
  , base: { width: 80, height: 30 }
  , capital: { width: 70, height: 30 }
  , architrave: { depth: 30, height: 30 }
  , frieze: { depth: 25, height: 45 }
  , cornice: { depth: 70, height: 45 }
  }

-- Architrave 1 1/4 modules, frieze 1 1/2, cornice 1 3/4 (37.5 and 52.5 minutes, rounded so
-- the entablature keeps its 135); the capital, without its volutes, 2/3 of a module.
ionic :: Order
ionic = withPedestal { base: 40, die: 120, cornice: 20 } $ mkOrder Ionic
  { columnHeight: 540
  , upperDiameter: 50
  , base: { width: 80, height: 30 }
  , capital: { width: 70, height: 20 }
  , architrave: { depth: 31, height: 38 }
  , frieze: { depth: 25, height: 45 }
  , cornice: { depth: 77, height: 52 }
  }

-- Architrave 1 1/2 modules, frieze 1 1/2, cornice 2; capital 2 1/3.
corynthian :: Order
corynthian = withPedestal { base: 47, die: 140, cornice: 23 } $ mkOrder Corynthian
  { columnHeight: 600
  , upperDiameter: 50
  , base: { width: 80, height: 30 }
  , capital: { width: 85, height: 70 }
  , architrave: { depth: 33, height: 45 }
  , frieze: { depth: 25, height: 45 }
  , cornice: { depth: 85, height: 60 }
  }

-- "The same Measures and Proportions that he does to the Corinthian" (Fréart).
composite :: Order
composite = corynthian { kind = Composite }
