module Handler.ViewLocation where

import Import

getViewLocationR :: LocationId -> Handler RepHtml
getViewLocationR locationId = do
    location <- runDB $ get404 locationId
    sector <- runDB $ get404 (locationSector location)
    clearance <- runDB $ get404 (locationClearance location)
    defaultLayout $ do
        setTitle $ toHtml $ locationName location
        $(widgetFile "view-location")

