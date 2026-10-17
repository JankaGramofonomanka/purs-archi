module Architecture.Order.Palladio
  ( composite
  , corynthian
  , doric
  , ionic
  , treatise
  , tuscan
  )
  where

import Architecture.Order (Minutes(..), Order, OrderKind(..), Treatise)
import Data.Maybe (Maybe(..))

treatise :: Treatise
treatise =
  { tuscan: Just tuscan
  , doric: Just doric
  , ionic: Just ionic
  , corynthian: Just corynthian
  , composite: Just composite
  }

-- Widths and depths after Palladio, Quattro Libri, Book I (Ware's 1738 translation):
-- bases and capitals project a stated fraction of the lower diameter, the frieze is
-- plumb with the top of the shaft, and the architrave's crowning member and the
-- cornice each project as far as they are high.
-- Palladio's Tuscan: seven diameters, diminished a fourth at the top, the base projecting a
-- sixth of the diameter and the abacus plumb with the lower shaft. He gives no ratio for a
-- stone Tuscan entablature, so it is a fourth of the column, as in the Doric. Half minutes
-- (the upper radius is 22.5) are rounded up.
tuscan :: Order
tuscan =
  { kind: Tuscan
  , height: Minutes 525
  , column:
      { lowerDiameter: Minutes 60
      , upperDiameter: Minutes 45
      , shaftHeight: Minutes 360
      , height: Minutes 420
      }
  , base: { width: Minutes 80, height: Minutes 30 }
  , capital: { width: Minutes 60, height: Minutes 30 }
  , entablature:
      { height: Minutes 105
      , architrave: { depth: Minutes 28, height: Minutes 35 }
      , frieze: { depth: Minutes 23, height: Minutes 35 }
      , cornice: { depth: Minutes 58, height: Minutes 35 }
      }
  }

doric :: Order
doric =
  { kind: Doric
  , height: Minutes 600
  , column:
      { lowerDiameter: Minutes 60
      , upperDiameter: Minutes 50
      , shaftHeight: Minutes 420
      , height: Minutes 480
      }
  , base: { width: Minutes 80, height: Minutes 30 }
  , capital: { width: Minutes 84, height: Minutes 30 }
  , entablature:
      { height: Minutes 120
      , architrave: { depth: Minutes 31, height: Minutes 40 }
      , frieze: { depth: Minutes 25, height: Minutes 40 }
      , cornice: { depth: Minutes 65, height: Minutes 40 }
      }
  }

ionic :: Order
ionic =
  { kind: Ionic
  , height: Minutes 675
  , column:
      { lowerDiameter: Minutes 60
      , upperDiameter: Minutes 50
      , shaftHeight: Minutes 450
      , height: Minutes 540
      }
  , base: { width: Minutes 82, height: Minutes 30 }
  , capital: { width: Minutes 90, height: Minutes 60 }
  , entablature:
      { height: Minutes 135
      , architrave: { depth: Minutes 34, height: Minutes 45 }
      , frieze: { depth: Minutes 25, height: Minutes 45 }
      , cornice: { depth: Minutes 70, height: Minutes 45 }
      }
  }

corynthian :: Order
corynthian =
  { kind: Corynthian
  , height: Minutes 750
  , column:
      { lowerDiameter: Minutes 60
      , upperDiameter: Minutes 50
      , shaftHeight: Minutes 510
      , height: Minutes 600
      }
  , base: { width: Minutes 84, height: Minutes 30 }
  , capital: { width: Minutes 90, height: Minutes 60 }
  , entablature:
      { height: Minutes 150
      , architrave: { depth: Minutes 35, height: Minutes 50 }
      , frieze: { depth: Minutes 25, height: Minutes 50 }
      , cornice: { depth: Minutes 75, height: Minutes 50 }
      }
  }

-- Palladio's Composite is "formed like [the Corinthian] in all its parts, except the capital",
-- whose dimensions are also the same, and ten diameters high.
composite :: Order
composite =
  { kind: Composite
  , height: Minutes 750
  , column:
      { lowerDiameter: Minutes 60
      , upperDiameter: Minutes 50
      , shaftHeight: Minutes 510
      , height: Minutes 600
      }
  , base: { width: Minutes 84, height: Minutes 30 }
  , capital: { width: Minutes 90, height: Minutes 60 }
  , entablature:
      { height: Minutes 150
      , architrave: { depth: Minutes 35, height: Minutes 50 }
      , frieze: { depth: Minutes 25, height: Minutes 50 }
      , cornice: { depth: Minutes 75, height: Minutes 50 }
      }
  }
