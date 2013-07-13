module Handler.ViewSector where

import Import

getViewSectorR :: SectorId -> Handler RepHtml
getViewSectorR sectorId = do
    sector <- runDB $ get404 sectorId
    locations <- runDB $ selectList [LocationSector ==. sectorId] [Asc LocationName]
    defaultLayout $ do
        $(widgetFile "view-sector")
