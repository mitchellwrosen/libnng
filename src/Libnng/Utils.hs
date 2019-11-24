module Libnng.Utils where

import Foreign.C (CInt)

import Libnng.Error


onZero
  :: IO a
  -> IO CInt
  -> IO ( Either Error a )
onZero f g =
  g >>= \case
    0 -> Right <$> f
    n -> pure ( Left ( cintToError n ) )

errnoToEither
  :: IO CInt
  -> IO ( Either Error () )
errnoToEither =
  fmap
    ( \case
        0 -> Right ()
        n -> Left ( cintToError n )
    )
