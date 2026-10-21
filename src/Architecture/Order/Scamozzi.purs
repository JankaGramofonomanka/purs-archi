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

import Architecture.Order (Order(..), OrderKind(..), Treatise, mkOrder, mkPedestal, withArcadeNoPedestal, withPedestal, withArcadePedestal, withArcadePlinth)
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
-- Arcades after the Mirror's plates of arches. It gives the width of the opening, the height
-- from the foot of the order to the top of the impost and how much the arch rises above a
-- half circle; the opening's height adds half its width to these. The pier is the column's
-- diameter and the strip of pilaster the Mirror gives on each side of it ("the Pilaster is
-- thirty Minutes under on the breadth of the Column"), which Chambers' piers confirm (4 2/3
-- modules for the Tuscan with pedestals, 3 3/4 and 4 for the Corinthian without and with).
-- The Mirror gives no strip with pedestals but the Tuscan's; the Doric, Ionic and Roman piers
-- with pedestals are the widths that bring a triglyph or a modillion over each axis
-- (1 1/4 diameters between triglyphs, and 35, 32 1/2 and 30 minutes between the Ionic, Roman
-- and Corinthian modillions, from the Mirror's galleries) and keep the piers diminishing
-- from the Tuscan to the Corinthian, as Chambers says they do.
-- The archivolt is the Mirror's "Arch or Bow", about a tenth of the opening ("the bigness of
-- the Arch must be one tenth of the breadth"): 26, 27 1/4, 24 1/4, 25 and 28 minutes without
-- pedestals (Tuscan, Doric, Ionic, Corinthian, Roman) and 29, 34, 30 1/2, 30 and 33 with them,
-- quarter minutes rounded down and the half up.
-- Without pedestals the columns stand on "one gross Base under of half a Model high", which the
-- Mirror adds to every gallery and arch without pedestals "to make the Work appear the
-- statelier". With it the key-piece of every arch reaches the architrave, as it does with
-- pedestals.
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
-- Arches 3 diameters 52 minutes wide, rising to the impost 5 diameters 16 minutes and "somewhat
-- more than one eighth" of a diameter (taken as 8 minutes) above a half circle; with pedestals
-- 4 diameters 20 minutes, 6 diameters 12 1/2 minutes and 10 minutes (the half minute rounded
-- up). Piers of 34-minute strips without pedestals and 40-minute ones with them.
tuscan :: Order
tuscan =
  withArcadePlinth 30
  $ withArcadeNoPedestal { openingWidth: 232, openingHeight: 440, pierWidth: 128, archivoltWidth: 26 }
  $ withArcadePedestal { openingWidth: 260, openingHeight: 513, pierWidth: 140, archivoltWidth: 29 }
  $ withPedestal { base: 30, die: 60, cornice: 23 } $ mkOrder Tuscan
  { columnHeight: 450
  , upperDiameter: 45
  , base: { width: 80, height: 30 }
  , capital: { width: 70, height: 30 }
  , architrave: { depth: 28, height: 33 }
  , frieze: { depth: 23, height: 41 }
  , cornice: { depth: 62, height: 39 }
  }

-- Entablature 2 1/8 diameters: architrave 35, frieze 45 with a list of 5, cornice 42.
-- Columns 6 diameters 15 minutes apart, with strips of 32 minutes; the arch rises to the
-- impost 6 diameters 4 1/2 minutes and 10 minutes above a half circle. With pedestals the
-- opening is 5 1/4 diameters, 7 diameters 6 minutes to the impost and 12 1/2 above the half
-- circle; the axes are six triglyphs (7 1/2 diameters) apart.
doric :: Order
doric =
  withArcadePlinth 30
  $ withArcadeNoPedestal { openingWidth: 251, openingHeight: 500, pierWidth: 124, archivoltWidth: 27 }
  $ withArcadePedestal { openingWidth: 315, openingHeight: 596, pierWidth: 135, archivoltWidth: 34 }
  $ withPedestal { base: 45, die: 69, cornice: 22 } $ mkOrder Doric
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
-- Arches 3 diameters 50 minutes wide, with strips of 30 minutes, rising to the impost
-- 6 diameters 18 minutes and 12 minutes above a half circle; with pedestals 4 diameters
-- 50 minutes, 7 diameters 35 minutes and 15, the axes twelve modillions (7 diameters) apart.
ionic :: Order
ionic =
  withArcadePlinth 30
  $ withArcadeNoPedestal { openingWidth: 230, openingHeight: 505, pierWidth: 120, archivoltWidth: 24 }
  $ withArcadePedestal { openingWidth: 290, openingHeight: 615, pierWidth: 130, archivoltWidth: 31 }
  $ withPedestal { base: 45, die: 83, cornice: 22 } $ mkOrder Ionic
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
-- Arches 4 diameters 8 minutes wide, with strips of 26 minutes, rising to the impost
-- 7 diameters 20 minutes and 16 minutes above a half circle; with pedestals 5 diameters,
-- 9 1/2 diameters and 20 minutes, the axes fourteen modillions (7 diameters) apart.
corynthian :: Order
corynthian =
  withArcadePlinth 30
  $ withArcadeNoPedestal { openingWidth: 248, openingHeight: 580, pierWidth: 112, archivoltWidth: 25 }
  $ withArcadePedestal { openingWidth: 300, openingHeight: 740, pierWidth: 120, archivoltWidth: 30 }
  $ withPedestal { base: 45, die: 133, cornice: 22 } $ mkOrder Corynthian
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
-- Arches 4 diameters 34 minutes wide, with strips of 28 minutes, rising to the impost
-- 6 diameters 54 minutes and 14 minutes above a half circle; with pedestals 5 1/2 diameters,
-- 8 diameters 42 1/2 minutes and 17 1/2, the axes fourteen modillions (7 7/12 diameters)
-- apart.
composite :: Order
composite =
  withArcadePlinth 30
  $ withArcadeNoPedestal { openingWidth: 274, openingHeight: 565, pierWidth: 116, archivoltWidth: 28 }
  $ withArcadePedestal { openingWidth: 330, openingHeight: 705, pierWidth: 125, archivoltWidth: 33 }
  $ withPedestalOf84 $ mkOrder Composite
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
  withPedestalOf84 (Order order) = Order order
    { pedestal = Just $ mkPedestal
        { base: { width: 118, height: 45 }
        , die: { width: 84, height: 113 }
        , cornice: { width: 128, height: 22 }
        }
    }
