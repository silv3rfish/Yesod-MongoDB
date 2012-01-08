{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings, ExtendedDefaultRules #-}
{- |
Module      :  main.hs
Description :  A Yesod MongoDB Demo
License     :  GPLv3

Maintainer  :  jobiscuit@gmail.com
Stability   :  stable
Portability :  non-portable ({-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-})

A simple blog written with the aim of demoing Yesod and MongoDB. 
Yesod is translated as foundation from Hebrew.
MongoDB is a no sql database written in C++
-}
import Yesod
import Yesod.Default.Config
import qualified Yesod.Default.Util
import Language.Haskell.TH.Syntax
import Data.Text (Text)
import Text.Hamlet (hamletFile)
import Database.MongoDB
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.IO.Class (liftIO)
--for ghci 
import Data.CompactString () 
import Data.Either
import Data.Maybe

-- Root Data type for this site
data YesodMongoDemo = YesodMongoDemo 
     {mongoPipe :: Pipe} 

-- Generates URL datatype and site function for the given Resources
mkYesod "YesodMongoDemo" [parseRoutes|
/               RootR  GET
/entry/#String  EntryR GET
/error          ErrorR GET           
|]

-- class MangoDBSite master where
--      runDB :: MonadIO monad => 

-- Create instance of Yesod Datatype. Define settings for a Yesod applications
instance Yesod YesodMongoDemo where
    -- An absolute URL to the root of the application. Do not include trailing slash.
    approot _ = ""

    defaultLayout widget = do
        -- Gets the message in the user's session, if available, and then clears the variable.
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        -- have a look at the tempaltes folder

        pc <- widgetToPageContent $ do
            $(Yesod.Default.Util.widgetFileDebug "normalize")
            $(Yesod.Default.Util.widgetFileDebug "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- MangoDB helper function

runMongoDB action = liftIOHandler $ do
                    YesodMongoDemo pipe <- getYesod
                    r <- access pipe master "blogdb" action
                    case r of 
                         Left results -> redirect RedirectTemporary ErrorR
                         Right results -> return results


getRootR :: Handler RepHtml
getRootR = do
         -- find is a mongodb function to return many db entries.
         -- it returns results we can iterate over using next or rest
         entries <- runMongoDB $ find (select [] "posts") >>= rest
         printDocs "Test" entries
         defaultLayout $ do 
                       setTitle "Silv3rfish homepage" 
                       $(Yesod.Default.Util.widgetFileDebug $ "homepage")

getEntryR :: String -> Handler RepHtml
getEntryR entryTitle = do
         -- findone is a mongodb function to return maybe one db entry.
         entryRes <- runMongoDB $ findOne $ select ["title" =: entryTitle] "posts"
         let entry = fromJust entryRes
         printDocs "Test" [entry]
         defaultLayout $ do 
                       setTitle "Silv3rfish homepage" 
                       $(Yesod.Default.Util.widgetFileDebug $ "entry")

getErrorR :: Handler RepHtml
getErrorR = defaultLayout $ do 
                       setTitle "Silv3rfish homepage" 
                       $(Yesod.Default.Util.widgetFileDebug $ "error")

main :: IO ()
main = do
  pipe <- runIOE $ connect $ host "127.0.0.1"
  e <- access pipe master "blogdb" posts_init
  warpDebug 3000 $ YesodMongoDemo pipe

posts_init = do
     clearPosts
     insertPosts


clearPosts = Database.MongoDB.delete (select [] "posts")

insertPosts = insertMany "posts" [
    ["title" =: "Test 1", "content" =: "This is the first test blog entry for my site"],
    ["title" =: "Test 2", "content" =: "This is my second test blog for my site"] ]

printDocs title docs = liftIO $ putStrLn title >> mapM_ (print . exclude ["_id"]) docs

valueToString :: Database.MongoDB.Value -> String
valueToString value = fromJust $ cast' value
