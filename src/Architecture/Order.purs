module Architecture.Order
  ( Column
  , Element
  , Entablature
  , EntablatureElement
  , Minutes(..)
  , Order
  , OrderKind(..)
  , Pedestal
  , PedestalMode(..)
  , SizeDeterminant(..)
  , Treatise
  , mkOrder
  , mkPedestal
  , pedestalHeight
  , minuteFromColumnAndEntablatureHeight
  , minuteFromColumnHeight
  , minuteFromEntablatureHeight
  , minuteFromSize
  , minuteFromLowerDiameter
  , minuteFromUpperDiameter
  , minutesPerDiameter
  , orderHeight
  , sizeOf
  , treatiseOrder
  , withPedestal
  )
  where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)

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

data OrderKind = Tuscan | Doric | Ionic | Corynthian | Composite

-- | The block a column may stand on: a base, a die and a cornice, bottom to top.
type Pedestal =
  { height :: Minutes
  , base :: Element
  , die :: Element
  , cornice :: Element
  }

-- | How a row of columns stands: on the ground, each on its own pedestal, or all on one
-- | continuous pedestal (a podium) with the pedestal's proportions.
data PedestalMode = NoPedestal | Pedestals | Podium

derive instance Eq PedestalMode

type Order =
  { kind :: OrderKind
  , height :: Minutes
  , column :: Column
  , base :: Element
  , capital :: Element
  , entablature :: Entablature
  -- | `Nothing` where the author gives no pedestal for the order. `height` excludes it.
  , pedestal :: Maybe Pedestal
  }

-- | Height of the pedestal the order stands on in the given mode, in minutes.
pedestalHeight :: PedestalMode -> Order -> Minutes
pedestalHeight mode order = case mode of
  NoPedestal -> Minutes 0
  _ -> maybe (Minutes 0) _.height order.pedestal

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

data SizeDeterminant = Minute Number | LowerDiameter Number | UpperDiameter Number | ColumnHeight Number | EntablatureHeight Number | OrderHeight Number

-- | The orders as one author of a treatise proportions them; `Nothing` where the author
-- | does not define the order.
type Treatise =
  { tuscan :: Maybe Order
  , doric :: Maybe Order
  , ionic :: Maybe Order
  , corynthian :: Maybe Order
  , composite :: Maybe Order
  }

treatiseOrder :: Treatise -> OrderKind -> Maybe Order
treatiseOrder treatise = case _ of
  Tuscan -> treatise.tuscan
  Doric -> treatise.doric
  Ionic -> treatise.ionic
  Corynthian -> treatise.corynthian
  Composite -> treatise.composite

-- | Build an order from its parts, in minutes; the shaft and the totals are derived.
mkOrder
  :: OrderKind
  -> { columnHeight :: Int
     , upperDiameter :: Int
     , base :: { width :: Int, height :: Int }
     , capital :: { width :: Int, height :: Int }
     , architrave :: { depth :: Int, height :: Int }
     , frieze :: { depth :: Int, height :: Int }
     , cornice :: { depth :: Int, height :: Int }
     }
  -> Order
mkOrder kind o =
  { kind
  , height: Minutes (o.columnHeight + entablatureHeight)
  , column:
      { lowerDiameter: Minutes minutesPerDiameter
      , upperDiameter: Minutes o.upperDiameter
      , shaftHeight: Minutes (o.columnHeight - o.base.height - o.capital.height)
      , height: Minutes o.columnHeight
      }
  , base: element o.base
  , capital: element o.capital
  , entablature:
      { height: Minutes entablatureHeight
      , architrave: entablatureElement o.architrave
      , frieze: entablatureElement o.frieze
      , cornice: entablatureElement o.cornice
      }
  , pedestal: Nothing
  }
  where
  entablatureHeight = o.architrave.height + o.frieze.height + o.cornice.height
  element e = { width: Minutes e.width, height: Minutes e.height }
  entablatureElement e = { depth: Minutes e.depth, height: Minutes e.height }

-- | Build a pedestal from its parts, in minutes; the total height is derived.
mkPedestal
  :: { base :: { width :: Int, height :: Int }
     , die :: { width :: Int, height :: Int }
     , cornice :: { width :: Int, height :: Int }
     }
  -> Pedestal
mkPedestal p =
  { height: Minutes (p.base.height + p.die.height + p.cornice.height)
  , base: element p.base
  , die: element p.die
  , cornice: element p.cornice
  }
  where
  element e = { width: Minutes e.width, height: Minutes e.height }

-- | Give the order a pedestal of the given heights, in minutes. Where an author gives only
-- | heights, the widths follow Chambers: the die is as wide as the plinth of the column's
-- | base, the cornice projects as far as it is high, and the base somewhat less (three
-- | quarters as far) "that so the whole base may be covered and sheltered by it".
withPedestal :: { base :: Int, die :: Int, cornice :: Int } -> Order -> Order
withPedestal p order = order
  { pedestal = Just $ mkPedestal
      { base: { width: dieWidth + 2 * Int.round (Int.toNumber p.cornice * 0.75), height: p.base }
      , die: { width: dieWidth, height: p.die }
      , cornice: { width: dieWidth + 2 * p.cornice, height: p.cornice }
      }
  }
  where
  Minutes dieWidth = order.base.width
