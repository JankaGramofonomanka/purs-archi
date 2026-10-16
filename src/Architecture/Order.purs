module Architecture.Order
  ( Column
  , Element
  , Entablature
  , EntablatureElement
  , Minutes(..)
  , Order
  , OrderKind(..)
  , SizeDeterminant(..)
  , corynthian
  , doric
  , ionic
  , minuteFromColumnAndEntablatureHeight
  , minuteFromColumnHeight
  , minuteFromEntablatureHeight
  , minuteFromSize
  , minuteFromLowerDiameter
  , minuteFromUpperDiameter
  , minutesPerDiameter
  , orderHeight
  , sizeOf
  )
  where

import Prelude

import Data.Int as Int

newtype Minutes = Minutes Int

type Element =
  { width :: Minutes
  , height :: Minutes
  }

type EntablatureElement =
  { -- | Distance from the column centerline to the outermost face of the element.
    depth :: Minutes
  , height :: Minutes
  }

type Column =
  { lowerDiameter :: Minutes
  , upperDiameter :: Minutes
  , shaftHeight :: Minutes
  , height :: Minutes
  }

type Entablature =
  { height :: Minutes
  , architrave :: EntablatureElement
  , frieze :: EntablatureElement
  , cornice :: EntablatureElement
  }

data OrderKind = Doric | Ionic | Corynthian

type Order =
  { kind :: OrderKind
  , height :: Minutes
  , column :: Column
  , base :: Element
  , capital :: Element
  , entablature :: Entablature
  }

orderHeight :: Number -> Order -> Number
orderHeight minute order =
  sizeOf order.height minute

minutesPerDiameter :: Int
minutesPerDiameter = 60

minuteFromColumnHeight :: Order -> Number -> Number
minuteFromColumnHeight order height =
  minuteFromProportion order.column.height height

minuteFromColumnAndEntablatureHeight :: Order -> Number -> Number
minuteFromColumnAndEntablatureHeight order height =
  minuteFromProportion order.height height

minuteFromEntablatureHeight :: Order -> Number -> Number
minuteFromEntablatureHeight order height =
  minuteFromProportion order.entablature.height height

minuteFromSize :: Order -> SizeDeterminant -> Number
minuteFromSize order = case _ of
  Minute minute -> minute
  LowerDiameter diameter -> minuteFromLowerDiameter order diameter
  UpperDiameter diameter -> minuteFromUpperDiameter order diameter
  ColumnHeight height -> minuteFromColumnHeight order height
  EntablatureHeight height -> minuteFromEntablatureHeight order height
  OrderHeight height -> minuteFromColumnAndEntablatureHeight order height

minuteFromLowerDiameter :: Order -> Number -> Number
minuteFromLowerDiameter order diameter =
  minuteFromProportion order.column.lowerDiameter diameter

minuteFromUpperDiameter :: Order -> Number -> Number
minuteFromUpperDiameter order diameter =
  minuteFromProportion order.column.upperDiameter diameter

sizeOf :: Minutes -> Number -> Number
sizeOf (Minutes proportion) minute =
  minute * Int.toNumber proportion

minuteFromProportion :: Minutes -> Number -> Number
minuteFromProportion (Minutes proportion) measurement =
  measurement / Int.toNumber proportion

-- Widths and depths after Palladio, Quattro Libri, Book I (Ware's 1738 translation):
-- bases and capitals project a stated fraction of the lower diameter, the frieze is
-- plumb with the top of the shaft, and the architrave's crowning member and the
-- cornice each project as far as they are high.
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

data SizeDeterminant = Minute Number | LowerDiameter Number | UpperDiameter Number | ColumnHeight Number | EntablatureHeight Number | OrderHeight Number
