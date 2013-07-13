{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler RepHtml
getHomeR = do
    sectors <- runDB $ selectList [] [Asc SectorName]
    defaultLayout $ do
        $(widgetFile "homepage")

