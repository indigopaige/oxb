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
  , _pageSect :: [OrgSection]
  , _pageBody :: [OrgElement]
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
  pure $ Page (sectionTitle s) (sectionSubsections s) (sectionChildren s) meta

instance ToHtml Meta where
  toHtmlRaw           = toHtml
  toHtml (Meta a d _) = div_ [class_ "meta"] $ do
    span_ (toHtml a)
    br_ []
    span_ (toHtml d)

instance ToHtml Page where
  toHtmlRaw = toHtml
  toHtml p  = html_ $ do
    head_ $ do
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1" ]
      link_ [rel_ "stylesheet", href_ "https://indigopaige.blog/styles.css"]
      meta_ [charset_ "utf-8"]
      title_ pageName'
    body_ $ do
      header_ $ h1_ pageName'

      main_ [class_ "box page"] $ do
        toHtml $ p^.pageMeta
        toHtml $ p^.pageBody
        toHtml $ p^.pageSect
          where
            pageName' = toHtml (p^.pageName)

pages :: OrgDocument -> [Page]
pages doc = catMaybes $ map toPage f
  where
    f   = filter g (documentSections doc)
    g x = "blog" `elem` sectionTags x
