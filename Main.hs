{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger


import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, forM_)
import Data.Monoid (mconcat, mempty)
import qualified Data.Aeson as A 
import Data.List 
import qualified Data.Text.Lazy as TL

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Control.Monad.IO.Class  (MonadIO, liftIO)

main :: IO()
main = scotty 3000 $ do 
	middleware logStdoutDev
	middleware $ staticPolicy ((predicate (\s -> isPrefixOf "assets" s) >-> noDots))
	get "/" $ do 
		liftIO $ putStrLn "aa"
	 	html  $ wrapper $ do
	 		H.div ! class_ "row" $ do
	 			H.div ! class_ "span8" $ do 
	 				renderPost firstPost
	 		

wrapper :: H.Html -> TL.Text
wrapper contents' = TL.pack $ renderHtml $ do
	H.docType
	H.html ! H.customAttribute "ng-app" mempty $ do
		H.head $ do
			H.link ! rel "stylesheet" ! type_ "text/css" ! href "/assets/css/bootstrap.min.css"
		H.body $ do
			H.div ! class_ "container" $ do
				navbar
				contents'


navbar :: H.Html 
navbar =  H.div ! class_ "navbar" $ do
	H.div ! class_ "navbar-inner" $ do
		H.a ! class_ "brand" ! href "#" $ "applicative.io"
		H.ul ! class_ "nav" $ do 
			H.li $ H.a ! href "#" $ "Home"
			H.li $ H.a ! href "#" $ "Projects"

data BlogPost = BlogPost { title :: String
						 , date :: String
						 , content :: [H.Html]
						 } 

firstPost :: BlogPost 
firstPost = BlogPost "First Post" "2013-07-05" ["hi"] 

renderPost :: BlogPost -> H.Html 
renderPost post = do 
	H.h2 $ H.toHtml (Main.title post)
	sequence_ (Main.content post)

