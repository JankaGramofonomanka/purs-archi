module Architecture
  ( Minutes(..)
  , Element
  , Column
  , Entablature
  , Order
  , minutesPerDiameter
  , doric
  , ionic
  , corynthian
  )
  where

newtype Minutes = Minutes Int

type Element =
  { width :: Minutes
  , height :: Minutes
  }

type Column =
  { lowerDiameter :: Minutes
  , upperDiameter :: Minutes
  , shaftHeight :: Minutes
  , height :: Minutes
  }

type Entablature =
  { width :: Minutes
  , height :: Minutes
  , architrave :: Element
  , frieze :: Element
  , cornice :: Element
  }

type Order =
  { height :: Minutes
  , column :: Column
  , base :: Element
  , capital :: Element
  , entablature :: Entablature
  }

minutesPerDiameter :: Int
minutesPerDiameter = 60

doric :: Order
doric =
  { height: Minutes 600
  , column:
      { lowerDiameter: Minutes 60
      , upperDiameter: Minutes 50
      , shaftHeight: Minutes 420
      , height: Minutes 480
      }
  , base: { width: Minutes 90, height: Minutes 30 }
  , capital: { width: Minutes 90, height: Minutes 30 }
  , entablature:
      { width: Minutes 90
      , height: Minutes 120
      , architrave: { width: Minutes 90, height: Minutes 40 }
      , frieze: { width: Minutes 90, height: Minutes 40 }
      , cornice: { width: Minutes 90, height: Minutes 40 }
      }
  }

ionic :: Order
ionic =
  { height: Minutes 675
  , column:
      { lowerDiameter: Minutes 60
      , upperDiameter: Minutes 50
      , shaftHeight: Minutes 450
      , height: Minutes 540
      }
  , base: { width: Minutes 90, height: Minutes 30 }
  , capital: { width: Minutes 90, height: Minutes 60 }
  , entablature:
      { width: Minutes 90
      , height: Minutes 135
      , architrave: { width: Minutes 90, height: Minutes 45 }
      , frieze: { width: Minutes 90, height: Minutes 45 }
      , cornice: { width: Minutes 90, height: Minutes 45 }
      }
  }

corynthian :: Order
corynthian =
  { height: Minutes 750
  , column:
      { lowerDiameter: Minutes 60
      , upperDiameter: Minutes 50
      , shaftHeight: Minutes 510
      , height: Minutes 600
      }
  , base: { width: Minutes 90, height: Minutes 30 }
  , capital: { width: Minutes 90, height: Minutes 60 }
  , entablature:
      { width: Minutes 90
      , height: Minutes 150
      , architrave: { width: Minutes 90, height: Minutes 50 }
      , frieze: { width: Minutes 90, height: Minutes 50 }
      , cornice: { width: Minutes 90, height: Minutes 50 }
      }
  }
