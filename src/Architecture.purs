module Architecture
  ( Portico
  , SizedOrder
  , Column
  , Entablature
  )
  where

import Architecture.Order (Order, SizeDeterminant)

type SizedOrder = { order :: Order, size :: SizeDeterminant }

newtype Column = Column SizedOrder
newtype Entablature = Entablature SizedOrder

newtype Portico = Portico {
  order :: Order,
  numOfColumns :: Int,
  bayWidth :: Number
}
