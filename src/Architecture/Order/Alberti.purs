module Architecture.Order.Alberti
  ( corynthian
  , doric
  , ionic
  , treatise
  )
  where

import Architecture.Order (Order, OrderKind(..), Treatise, mkOrder)
import Data.Maybe (Maybe(..))

-- After Alberti, De re aedificatoria, Book VII (Leoni's 1755 translation). Alberti gives the
-- length of the shaft alone: seven diameters for the Doric, eight for the Ionic and ten for
-- the Corinthian. His model column of about thirty feet diminishes to seven eighths. He
-- mentions the Tuscan and his "Italic" (Composite) capital without proportioning the orders.
-- The top of the architrave is never wider than the foot of the shaft, and every member
-- projects as far as it is high.
treatise :: Treatise
treatise =
  { tuscan: Nothing
  , doric: Just doric
  , ionic: Just ionic
  , corynthian: Just corynthian
  , composite: Nothing
  }

-- Base and capital each half a diameter; the plinth at most 1 1/2 diameters wide, the abacus
-- 1 1/12. The architrave is half a diameter, divided into twelve minutes; the triglyphs are
-- eighteen of those minutes high and project half a minute; the cornice above them is a
-- fillet, cyma and platband (2, 2 and 3), the mutules (3), their cymaise (3/4) and the
-- corona (4), 14 3/4 minutes in all.
doric :: Order
doric = mkOrder Doric
  { columnHeight: 480
  , upperDiameter: 53
  , base: { width: 90, height: 30 }
  , capital: { width: 65, height: 30 }
  , architrave: { depth: 30, height: 30 }
  , frieze: { depth: 31, height: 45 }
  , cornice: { depth: 68, height: 37 }
  }

-- The base is half a diameter high and eleven eighths wide. The capital is half a diameter,
-- of which 6 of 19 parts hang beside the top of the shaft; the abacus is as wide as that top.
-- The architrave is a thirteenth of the column, as for columns of fifteen to twenty feet, and
-- the frieze as high. The cornice members add up to twenty "parts", which Alberti does not
-- define; they are read here as twelfths of the architrave, the division of its fascias.
ionic :: Order
ionic = mkOrder Ionic
  { columnHeight: 531
  , upperDiameter: 53
  , base: { width: 83, height: 30 }
  , capital: { width: 53, height: 21 }
  , architrave: { depth: 30, height: 41 }
  , frieze: { depth: 30, height: 41 }
  , cornice: { depth: 98, height: 68 }
  }

-- The capital is a whole diameter high, its abacus ten sevenths wide. In the entablature
-- the Corinthians "added nothing" to the Ionic.
corynthian :: Order
corynthian = mkOrder Corynthian
  { columnHeight: 690
  , upperDiameter: 53
  , base: { width: 83, height: 30 }
  , capital: { width: 86, height: 60 }
  , architrave: { depth: 30, height: 53 }
  , frieze: { depth: 30, height: 53 }
  , cornice: { depth: 118, height: 88 }
  }
