module Handler.ListLocations where

import Import

getListLocationsR :: Handler RepHtml
getListLocationsR = do
    sectors <- runDB $ selectList [] [Asc SectorName]
    groupedLocations <- mapM (\s -> getLocationsForSector (entityKey s) >>= (\locs -> return ((entityVal s), locs))) sectors
    defaultLayout $ do
        setTitle "All Locations"
        $(widgetFile "all-locations")
    where
        getLocationsForSector s = runDB $ selectList [LocationSector ==. s] [Asc LocationName]
