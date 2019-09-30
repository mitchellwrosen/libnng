module Nng.Address
  ( Address(..)
  , addressAsCString
  , addressFromText
  , addressToText
  ) where

import Data.Text (Text)
import Foreign.C.String (CString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString as ByteString

-- TODO Address more protocols
data Address
  = Address'Inproc Text
  | Address'Ipc Text
  deriving stock ( Eq, Show )

addressAsCString
  :: Address
  -> ( CString -> IO a )
  -> IO a
addressAsCString address =
  ByteString.useAsCString
    ( Text.encodeUtf8 ( addressToText address ) )

addressFromText
  :: Text
  -> Maybe Address
addressFromText = \case
  (Text.stripPrefix "inproc://" -> Just address) ->
    Just ( Address'Inproc address )

  (Text.stripPrefix "ipc://" -> Just address) ->
    Just ( Address'Ipc address )

  _ ->
    Nothing

addressToText
  :: Address
  -> Text
addressToText = \case
  Address'Inproc address -> "inproc://" <> address
  Address'Ipc    address -> "ipc://"    <> address



