module Architecture.Order.Serlio
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

-- After Serlio's Book IV, as measured in Fréart de Chambray's Parallel of the Antient
-- Architecture with the Modern (Evelyn's translation, 1723). Fréart gives the column and
-- entablature heights; Serlio otherwise "always strives to follow" Vitruvius, so the
-- subdivisions, diminutions, widths and projections are Vitruvius's (see
-- `Architecture.Order.Vitruvius`), scaled to Serlio's entablatures.
treatise :: Treatise
treatise =
  { tuscan: Just tuscan
  , doric: Just doric
  , ionic: Just ionic
  , corynthian: Just corynthian
  , composite: Just composite
  }

-- Six diameters, with base, capital, architrave, frieze and cornice all equally high; the
-- entablature is a fourth of the column. Diminished a fourth, with an abacus as wide as the
-- lower diameter, as in Vitruvius.
tuscan :: Order
tuscan = mkOrder Tuscan
  { columnHeight: 360
  , upperDiameter: 45
  , base: { width: 80, height: 30 }
  , capital: { width: 60, height: 30 }
  , architrave: { depth: 23, height: 30 }
  , frieze: { depth: 23, height: 30 }
  , cornice: { depth: 53, height: 30 }
  }

-- Fourteen modules with base and capital; the entablature "three modules and a little more
-- than two thirds".
doric :: Order
doric = mkOrder Doric
  { columnHeight: 420
  , upperDiameter: 50
  , base: { width: 80, height: 30 }
  , capital: { width: 65, height: 30 }
  , architrave: { depth: 25, height: 30 }
  , frieze: { depth: 25, height: 45 }
  , cornice: { depth: 45, height: 36 }
  }

-- "Seven diameters and a half, allowing only a fifth part" to the entablature.
ionic :: Order
ionic = mkOrder Ionic
  { columnHeight: 450
  , upperDiameter: 50
  , base: { width: 83, height: 30 }
  , capital: { width: 63, height: 22 }
  , architrave: { depth: 34, height: 33 }
  , frieze: { depth: 30, height: 25 }
  , cornice: { depth: 51, height: 32 }
  }

-- Nine diameters, the capital (abacus included) as high as the diameter, and the entablature
-- a fifth of the column.
corynthian :: Order
corynthian = mkOrder Corynthian
  { columnHeight: 540
  , upperDiameter: 50
  , base: { width: 83, height: 30 }
  , capital: { width: 85, height: 60 }
  , architrave: { depth: 34, height: 40 }
  , frieze: { depth: 30, height: 30 }
  , cornice: { depth: 55, height: 38 }
  }

-- Ten diameters, with his Corinthian capital. Fréart only calls its entablature, copied from
-- the top order of the Colosseum, "gigantick"; a fourth of the column is assumed here.
composite :: Order
composite = mkOrder Composite
  { columnHeight: 600
  , upperDiameter: 50
  , base: { width: 83, height: 30 }
  , capital: { width: 85, height: 60 }
  , architrave: { depth: 34, height: 45 }
  , frieze: { depth: 30, height: 45 }
  , cornice: { depth: 70, height: 60 }
  }
