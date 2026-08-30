module Data.Blog where

import Data.Text (Text, pack)
import Data.Functor.Identity
import Org.Parser.Document
import Control.Monad
import Control.Lens
import Data.Monoid
import Text.Printf
import Lucid.Base
import Org.Parser
import Org.Types
import Data.List
import Lucid

u_ :: Functor m => HtmlT m a -> HtmlT m a
u_ = makeElement "u"

s_ :: Functor m => HtmlT m a -> HtmlT m a
s_ = makeElement "s"

dateToString :: Date -> String
dateToString (year, month, day, _) = printf "%d-%d-%d" year month day
timeToString :: Time -> String
timeToString (hour, minute) = printf "T%d:%d" hour minute

instance ToHtml DateTime where
  toHtmlRaw = toHtml

  toHtml (date, Just time, _, _) = time_ [datetime_ s] (toHtml s)
    where
      s  = pack $ (dateToString date <> timeToString time)
  toHtml (date, Nothing, _, _)   = time_ [datetime_ s] (toHtml s)
    where
      s = pack $ dateToString date

instance ToHtml TimestampData where
  toHtmlRaw = toHtml

  toHtml (TimestampData _ t)    = toHtml t

  toHtml (TimestampRange _ a b) = toHtml a <> "-" <> toHtml b

instance ToHtml [OrgObject] where
  toHtmlRaw = toHtml
  toHtml    = foldl (\x y -> x <> toHtml y) mempty

instance ToHtml OrgObject where
  toHtmlRaw = toHtml


  toHtml (Link (UnresolvedLink i) a) = a_ [href_ ("#" <> i)] (toHtml a)
  toHtml (Link (URILink p t) a)      = a_ [href_ h] (toHtml a)
    where
      h = pack (printf "%s://%s" p t :: String)
  toHtml (Quoted DoubleQuote a)      = "\"" <> toHtml a <> "\""
  toHtml (Quoted SingleQuote a)      = "'"  <> toHtml a <> "'"
  toHtml (Strikethrough a)           = s_ (toHtml a)
  toHtml (Superscript a)             = sup_ (toHtml a)
  toHtml (Timestamp a)               = toHtml a
  toHtml (Subscript a)               = sub_ (toHtml a)
  toHtml (Underline a)               = u_ (toHtml a)
  toHtml (Verbatim a)                = pre_ (toHtml a)
  toHtml (Italic a)                  = i_ (toHtml a)
  toHtml (Plain a)                   = toHtml a
  toHtml LineBreak                   = br_ []
  toHtml (Bold a)                    = b_ (toHtml a)
  toHtml (Code a)                    = code_ (toHtml a)
  toHtml a                           = toHtml (show a)

instance ToHtml [OrgSection] where
  toHtmlRaw = toHtml
  toHtml    = foldl (\x y -> x <> toHtml y <> br_ []) mempty

instance ToHtml OrgSection where
  toHtmlRaw = toHtml
  toHtml s  = div_ [class_ "section"] $ do
    title
    toHtml (sectionSubsections s)
    where
      title = case (sectionLevel s) of
        -- reserve h1 for titles
        1 -> h2_ c t
        2 -> h3_ c t
        3 -> h4_ c t
        4 -> h5_ c t
        _ -> h6_ c t
        where
          c = [class_ "level"]
          t = toHtml (sectionTitle s)
