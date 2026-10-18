module Architecture.Order.Vitruvius
  ( corynthian
  , doric
  , ionic
  , treatise
  )
  where

import Architecture.Order (Order, OrderKind(..), Treatise, mkOrder)
import Architecture.Superposition (fourthLess)
import Data.Maybe (Maybe(..))

-- After Vitruvius, The Ten Books on Architecture (Morgan's 1914 translation).
-- Vitruvius sizes some members by the absolute size of the column; these are for a column of
-- twelve to fifteen feet. His Tuscan is a timber temple whose beams are "commensurate with the
-- requirements of the size of the building", and he does not describe the Composite.
treatise :: Treatise
treatise =
  { tuscan: Nothing
  , doric: Just doric
  , ionic: Just ionic
  , corynthian: Just corynthian
  , composite: Nothing
  -- Book V, chapters 1 and 7: the upper columns a fourth less than the lower.
  , superposition: fourthLess
  }

-- Book IV, ch. 3: the module is half a diameter; the column, with its capital and without a
-- base, is fourteen modules; the capital one module high and 2 1/6 wide. Architrave one
-- module, triglyphs 1 1/2, their capitals 1/6; the corona with its cymatia 1/2 module,
-- projecting 2/3 of a module; the sima an eighth higher than the corona (Book III, ch. 5).
-- The underside of the architrave answers to the top of the shaft.
doric :: Order
doric = mkOrder Doric
  { columnHeight: 420
  , upperDiameter: 50
  , base: { width: 60, height: 0 }
  , capital: { width: 65, height: 30 }
  , architrave: { depth: 25, height: 30 }
  , frieze: { depth: 25, height: 45 }
  , cornice: { depth: 45, height: 37 }
  }

-- Book III, chs. 3 and 5: a eustyle column is 9 1/2 diameters, diminished to five sixths.
-- The Ionic base is half a diameter high and 1 3/8 wide; the abacus 1 1/18 wide, and the
-- capital half of that high, of which 3 of 9 1/2 parts hang below the top of the shaft.
-- The architrave is half a diameter, its top as wide as the foot of the shaft, its cymatium
-- projecting a seventh of its height; the frieze a fourth less; the dentils, corona and sima
-- follow the fascias of the architrave, the corona and dentils projecting as far as they rise.
ionic :: Order
ionic = mkOrder Ionic
  { columnHeight: 570
  , upperDiameter: 50
  , base: { width: 83, height: 30 }
  , capital: { width: 63, height: 22 }
  , architrave: { depth: 34, height: 30 }
  , frieze: { depth: 30, height: 23 }
  , cornice: { depth: 49, height: 29 }
  }

-- Book IV, ch. 1: like the Ionic in everything but the capital, which is as high as the
-- lower diameter; the abacus' diagonal is two diameters, so a face is about 1.41 wide.
-- The members above the columns are borrowed from the Ionic.
corynthian :: Order
corynthian = mkOrder Corynthian
  { columnHeight: 608
  , upperDiameter: 50
  , base: { width: 83, height: 30 }
  , capital: { width: 85, height: 60 }
  , architrave: { depth: 34, height: 30 }
  , frieze: { depth: 30, height: 23 }
  , cornice: { depth: 49, height: 29 }
  }
