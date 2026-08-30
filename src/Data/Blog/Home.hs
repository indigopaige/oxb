module Data.Blog.Home where

import qualified Data.Text.Lazy.IO as TIO
import Data.Text (Text, pack, unpack)
import Data.Functor.Identity
import Org.Parser.Document
import System.Directory
import Data.Blog.Page
import Control.Monad
import Control.Lens
import Data.Monoid
import Text.Printf
import Lucid.Base
import Org.Parser
import Org.Types
import Data.List
import Lucid

data Home = Home
  { _homeTitle :: [OrgObject]
  , _homePages :: [Page]
  }
  deriving Show

makeLenses ''Home

toHome :: OrgDocument -> Maybe Home
toHome doc = do
  title <- getTitle (documentChildren doc)
  pure $ Home title (pages doc)
  where
    getTitle ((OrgElement _ (Keyword "title" (ParsedKeyword kw)):_)) = Just kw
    getTitle (_:xs)                                                  = getTitle xs
    getTitle []                                                      = Nothing

instance ToHtml Home where
  toHtmlRaw = toHtml
  toHtml h  = main_ [class_ "home"] $ do
    h1_ $ toHtml (h^.homeTitle)
    ul_ $ foldl f mempty (h^.homePages)
      where
        f last next = do
          last
          br_ []
          li_ [class_ "post"] $ do
            h2_ [class_ "title"] $ do
              a_  [class_ "link", href_ uri ] $ toHtml (next^.pageName)
            where
              uri = "/" <> (next^.pageMeta.metaPath)

writeHome :: Home -> IO ()
writeHome home = do
  createDirectoryIfMissing False "blog"
  homePage >> other
  where
    path x   = "./blog/" <> x <> ".html"
    homePage = TIO.writeFile (path "home") $  renderText (toHtml home)

    other    = mapM_ f (home^.homePages)
    f a      = TIO.writeFile filePath' text
      where
        mp        = a^.pageMeta.metaPath
        filePath' = path (unpack mp)
        text      = renderText (toHtml a)
