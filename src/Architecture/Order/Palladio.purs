module Architecture.Order.Palladio
  ( composite
  , corynthian
  , doric
  , ionic
  , treatise
  , tuscan
  )
  where

import Prelude

import Architecture.Order (Minutes(..), Order, OrderKind(..), Treatise, mkPedestal, withPedestal, withArcadePedestal)
import Architecture.Superposition (fourthLess)
import Data.Maybe (Maybe(..))

treatise :: Treatise
treatise =
  { tuscan: Just tuscan
  , doric: Just doric
  , ionic: Just ionic
  , corynthian: Just corynthian
  , composite: Just composite
  -- Books II and III repeat Vitruvius: upper columns "one fourth part less than those below".
  , superposition: Just fourthLess
  }

-- Pedestals after Book I: the Tuscan "plain, and one module in height"; the Doric die square,
-- divided into four parts, two for the base and one for the cimacia; the Ionic half the width
-- of an arch, here 7 1/2 parts of 162 1/2 minutes (Chambers' table): base 2, cimacia 1, die
-- 4 1/2; the Corinthian a fourth of the column in eight parts: cimacia 1, base 2, die 5; the
-- Composite a third of the column in 8 1/2 parts: cimacia 1, base 2, die 5 1/2. Widths as in
-- `withPedestal`.
-- Palladio gives arches only with pedestals (Book I, chs. 16-18; the Tuscan and Doric ones
-- are only drawn). The Ionic arch is "two squares high" and its piers "a third part of the
-- void"; its pedestal is half the arch's width, so the arch is twice the pedestal. The
-- Corinthian piers are "two fifths of the void, which void is two squares and a half,
-- including the thickness of the arch"; the Composite ones "half the void of the arch, and the
-- arches are two squares and a half high under the vault". For these two the piers are
-- Chambers' measures of Palladio's plates: "nearly three modules and three quarters" and
-- "four and four-fifths" (of half a diameter); the half minute is rounded up.
-- Palladio's text gives no archivolt (Chambers finds those of his plates "slender"); the
-- archivolts here are BORROWED from Chambers' rule, a tenth of the opening, rounded.
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
  , pedestal: Just $ mkPedestal
      { base: { width: 80, height: 0 }
      , die: { width: 80, height: 60 }
      , cornice: { width: 80, height: 0 }
      }
  , arcadeNoPedestal: Nothing
  , arcadePedestal: Nothing
  , arcadePlinth: Nothing
  }

doric :: Order
doric =
  withPedestal { base: 40, die: 80, cornice: 20 }
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
  , pedestal: Nothing
  , arcadeNoPedestal: Nothing
  , arcadePedestal: Nothing
  , arcadePlinth: Nothing
  }

-- Book I ch. 16: the abacus is 19 eighteenths of a diameter wide; the capital with the volute
-- is 9 1/2 of those parts high, of which the 6 above the eye of the volute, level with the
-- astragal, rise above the shaft.
ionic :: Order
ionic =
  withArcadePedestal { openingWidth: 325, openingHeight: 650, pierWidth: 108, archivoltWidth: 33 }
  $ withPedestal { base: 43, die: 98, cornice: 22 }
  { kind: Ionic
  , height: Minutes 675
  , column:
      { lowerDiameter: Minutes 60
      , upperDiameter: Minutes 50
      , shaftHeight: Minutes 490
      , height: Minutes 540
      }
  , base: { width: Minutes 82, height: Minutes 30 }
  , capital: { width: Minutes 63, height: Minutes 20 }
  , entablature:
      { height: Minutes 135
      , architrave: { depth: Minutes 34, height: Minutes 45 }
      , frieze: { depth: Minutes 25, height: Minutes 45 }
      , cornice: { depth: Minutes 70, height: Minutes 45 }
      }
  , pedestal: Nothing
  , arcadeNoPedestal: Nothing
  , arcadePedestal: Nothing
  , arcadePlinth: Nothing
  }

corynthian :: Order
corynthian =
  withArcadePedestal { openingWidth: 281, openingHeight: 703, pierWidth: 113, archivoltWidth: 28 }
  $ withPedestal { base: 37, die: 94, cornice: 19 }
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
  , pedestal: Nothing
  , arcadeNoPedestal: Nothing
  , arcadePedestal: Nothing
  , arcadePlinth: Nothing
  }

-- Palladio's Composite is "formed like [the Corinthian] in all its parts, except the capital",
-- whose dimensions are also the same, and ten diameters high.
composite :: Order
composite =
  withArcadePedestal { openingWidth: 288, openingHeight: 720, pierWidth: 144, archivoltWidth: 29 }
  $ withPedestal { base: 47, die: 129, cornice: 24 }
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
  , pedestal: Nothing
  , arcadeNoPedestal: Nothing
  , arcadePedestal: Nothing
  , arcadePlinth: Nothing
  }
