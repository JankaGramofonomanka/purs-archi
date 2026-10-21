module Architecture.Order
  ( Arcade
  , Column
  , Element
  , Entablature
  , EntablatureElement
  , Minutes(..)
  , Order
  , OrderKind(..)
  , Pedestal
  , PedestalMode(..)
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
  , measureOf
  , lowerMinute
  , upperMinute
  , minutesPerDiameter
  , orderHeight
  , sizeOf
  , treatiseOrder
  , withArcadeNoPedestal
  , withArcadePlinth
  , arcadePlinthHeight
  , withPedestal
  , withArcadePedestal
  , arcadeOf
  , Volutes
  , ionicVolutes
  , capitalWidth
  )
  where

import Prelude

import Architecture.Measure (Measure(..), SizeDeterminant)
import Architecture.Superposition (SuperpositionRule, ratioValue, stepAt)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Proportion (class Measurable)

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

-- | The arched opening between two piers that an order decorates: the opening's width, its
-- | height from the bottom of the order (with the pedestal, if any) to the crown of the arch,
-- | the width of the pier between two openings, and the width of the archivolt, the band of
-- | mouldings around the arch. The axes of the columns are `openingWidth + pierWidth` apart.
type Arcade =
  { openingWidth :: Minutes
  , openingHeight :: Minutes
  , pierWidth :: Minutes
  , archivoltWidth :: Minutes
  }

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
  -- | The arcade of columns standing without and with pedestals; `Nothing` where the author
  -- | gives none.
  , arcadeNoPedestal :: Maybe Arcade
  , arcadePedestal :: Maybe Arcade
  -- | The height of the plain block (a plinth or socle) the columns stand on in an arcade
  -- | without pedestals, which raises them so that the arch with its archivolt stays under the
  -- | architrave; `Nothing` where the author raises them on none.
  , arcadePlinth :: Maybe Minutes
  }

instance Measurable Order Measure Minutes where
  measure order Minute = Minutes 1
  measure order LowerDiameter = order.column.lowerDiameter
  measure order UpperDiameter = order.column.upperDiameter
  measure order ColumnHeight = order.column.height
  measure order EntablatureHeight = order.entablature.height
  measure order OrderHeight = order.height

-- | The arcade of an order whose columns stand as `mode` says.
arcadeOf :: PedestalMode -> Order -> Maybe Arcade
arcadeOf mode order = case mode of
  NoPedestal -> order.arcadeNoPedestal
  _ -> order.arcadePedestal

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

-- | The minute of an order whose columns stand as `mode` says (which matters for `StoreyHeight`).
minuteFromSize :: PedestalMode -> Order -> SizeDeterminant -> Number
minuteFromSize mode order { measure, value } =
  minuteFromProportion (measureOf measure mode order) value

minuteFromLowerDiameter :: Order -> Number -> Number
minuteFromLowerDiameter order diameter =
  minuteFromProportion order.column.lowerDiameter diameter

minuteFromUpperDiameter :: Order -> Number -> Number
minuteFromUpperDiameter order diameter =
  minuteFromProportion order.column.upperDiameter diameter

-- | The volutes of an Ionic capital after Palladio, Book I ch. 16: the abacus is 19 parts wide,
-- | the eye of each volute lies one part inside the end of the abacus, level with the top of the
-- | shaft, and the volute is eight parts high (drawn here as a circle of radius four parts).
-- | `inset` is the distance from the end of the abacus to the eye.
type Volutes = { inset :: Number, radius :: Number }

ionicVolutes :: Number -> Volutes
ionicVolutes abacus =
  let part = abacus / 19.0
  in { inset: part, radius: 4.0 * part }

-- | The full width of the capital: the abacus, widened for an Ionic capital by the volutes
-- | that reach beyond its ends.
capitalWidth :: Order -> Number -> Number
capitalWidth order minute =
  let abacus = sizeOf order.capital.width minute
  in case order.kind of
    Ionic ->
      let { inset, radius } = ionicVolutes abacus
      in abacus + 2.0 * max 0.0 (radius - inset)
    _ -> abacus

sizeOf :: Minutes -> Number -> Number
sizeOf (Minutes proportion) minute =
  minute * Int.toNumber proportion

minuteFromProportion :: Minutes -> Number -> Number
minuteFromProportion (Minutes proportion) measurement =
  measurement / Int.toNumber proportion

-- | The given measure of an order, in minutes.
measureOf :: Measure -> Order -> Minutes
measureOf measure order = case measure of
  Minute -> Minutes 1
  LowerDiameter -> order.column.lowerDiameter
  UpperDiameter -> order.column.upperDiameter
  ColumnHeight -> order.column.height
  EntablatureHeight -> order.entablature.height
  OrderHeight -> order.height

-- | The minute of an order stacked on `lower`, whose minute is given, as `rule` says; `joint` is the
-- | index of `lower` in the stack (0 is the bottom storey).
upperMinute :: SuperpositionRule -> Int -> { order :: Order, pedestal :: PedestalMode } -> Number -> { order :: Order, pedestal :: PedestalMode } -> Number
upperMinute rule joint lower minute upper =
  let step = stepAt joint rule
  in minuteFromProportion (measureOf step.upper upper.pedestal upper.order)
       (ratioValue step.ratio * sizeOf (measureOf step.lower lower.pedestal lower.order) minute)

-- | The minute of the order under `upper`, whose minute is given, as `rule` says; `joint` is the
-- | index of the lower order in the stack. The inverse of `upperMinute`.
lowerMinute :: SuperpositionRule -> Int -> { order :: Order, pedestal :: PedestalMode } -> Number -> { order :: Order, pedestal :: PedestalMode } -> Number
lowerMinute rule joint upper minute lower =
  let step = stepAt joint rule
  in minuteFromProportion (measureOf step.lower lower.pedestal lower.order)
       (sizeOf (measureOf step.upper upper.pedestal upper.order) minute / ratioValue step.ratio)

-- | The orders as one author of a treatise proportions them; `Nothing` where the author
-- | does not define the order.
type Treatise =
  { tuscan :: Maybe Order
  , doric :: Maybe Order
  , ionic :: Maybe Order
  , corynthian :: Maybe Order
  , composite :: Maybe Order
  -- | How large an order stacked on another is; `Nothing` where the author gives no rule.
  , superposition :: Maybe SuperpositionRule
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
  , arcadeNoPedestal: Nothing
  , arcadePedestal: Nothing
  , arcadePlinth: Nothing
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

-- | Give the order the arcade of its columns standing without pedestals, in minutes.
withArcadeNoPedestal :: { openingWidth :: Int, openingHeight :: Int, pierWidth :: Int, archivoltWidth :: Int } -> Order -> Order
withArcadeNoPedestal a order = order { arcadeNoPedestal = Just (mkArcade a) }

-- | Give the order the arcade of its columns standing on pedestals, in minutes.
withArcadePedestal :: { openingWidth :: Int, openingHeight :: Int, pierWidth :: Int, archivoltWidth :: Int } -> Order -> Order
withArcadePedestal a order = order { arcadePedestal = Just (mkArcade a) }

mkArcade :: { openingWidth :: Int, openingHeight :: Int, pierWidth :: Int, archivoltWidth :: Int } -> Arcade
mkArcade a = { openingWidth: Minutes a.openingWidth, openingHeight: Minutes a.openingHeight, pierWidth: Minutes a.pierWidth, archivoltWidth: Minutes a.archivoltWidth }

-- | Give the order the plinth its columns stand on in an arcade without pedestals, in minutes.
withArcadePlinth :: Int -> Order -> Order
withArcadePlinth height order = order { arcadePlinth = Just (Minutes height) }

-- | Height of the plinth under the columns of an order standing as `mode` says, if they frame an
-- | `arcade`: only columns without pedestals stand on one.
arcadePlinthHeight :: PedestalMode -> Boolean -> Order -> Minutes
arcadePlinthHeight mode arcade order = case mode of
  NoPedestal | arcade -> fromMaybe (Minutes 0) order.arcadePlinth
  _ -> Minutes 0
