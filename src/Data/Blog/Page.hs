module Data.Blog.Page where

import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.Functor.Identity
import Org.Parser.Objects
import Control.Monad
import Control.Lens
import Data.Monoid
import Lucid.Base
import Org.Parser
import Data.Maybe
import Org.Types
import Data.List
import Data.Blog
import Lucid

data Page = Page
  { _pageName :: [OrgObject]
  , _pageBody :: [OrgSection]
  , _pageMeta :: Meta
  }
  deriving Show

data Meta = Meta
  { _metaAuthor :: Text
  , _metaDate   :: TimestampData
  , _metaPath   :: Text
  }
  deriving Show

makeLenses ''Page
makeLenses ''Meta

collectMeta :: Properties -> Maybe Meta
collectMeta props = do
  author <- Map.lookup "author" props
  date   <- Map.lookup "date"   props
  path   <- Map.lookup "path"   props

  date'  <- parseOrgMaybe
    defaultOrgOptions
    parseTimestamp
    date
 
  pure $ Meta author date' path

toPage :: OrgSection -> Maybe Page
toPage s = do
  meta <- collectMeta (sectionProperties s)
  pure $ Page (sectionTitle s) (sectionSubsections s) meta

instance ToHtml Meta where
  toHtmlRaw           = toHtml
  toHtml (Meta a d _) = div_ [class_ "meta"] $ do
    span_ (toHtml a)
    span_ (toHtml d)

instance ToHtml Page where
  toHtmlRaw = toHtml
  toHtml p  = main_ [class_ "page"] $ do
    h1_ [class_ "title"] $ toHtml (p^.pageName)
    toHtml $ p^.pageMeta
    toHtml $ p^.pageBody

pages :: OrgDocument -> [Page]
pages doc = catMaybes $ map toPage f
  where
    f   = filter g (documentSections doc)
    g x = "blog" `elem` sectionTags x
