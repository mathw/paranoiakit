module Handler.AddLocation where

import Import

data PotentialLocation = PotentialLocation
    { plName :: Text
    , plClearance :: Entity Clearance
    , plSectorName :: Text
    }

locationForm :: Form PotentialLocation
locationForm = renderDivs $ PotentialLocation
    <$> areq textField "Name" Nothing
    <*> areq (selectField clearances) "Clearance" Nothing
    <*> areq textField "Sector Name" Nothing
    where
        clearances = optionsPersist [] [Asc ClearanceLevel] clearanceName

getOrCreateSector name = do
    r <- insertBy $ Sector name
    case r of
        Left e -> return $ entityKey e
        Right k -> return k

getAddLocationR :: Handler RepHtml
getAddLocationR = do
    (formWidget, encType) <- generateFormPost $ locationForm
    let message = Nothing :: Maybe (Either Text Text)
    defaultLayout $ do
        setTitle "Add Location"
	$(widgetFile "add-location")

postAddLocationR :: Handler RepHtml
postAddLocationR = do
    ((result, widget), enctype) <- runFormPost locationForm
    case result of
        FormSuccess potentialLocation -> do
            sectorKey <- runDB $ getOrCreateSector (plSectorName potentialLocation)
            _ <- runDB $ insert $ Location (plName potentialLocation) sectorKey (entityKey $ plClearance potentialLocation)
            let message = Just $ (Right "Location Created" :: Either Text Text)
            (formWidget, encType) <- generateFormPost $ locationForm
            defaultLayout $ do
                setTitle "Location Created"
                $(widgetFile "add-location")
        _ -> do
            let message = Just $ (Left "Error creating new location" :: Either Text Text)
            (formWidget, encType) <- generateFormPost $ locationForm
            defaultLayout $ do
                setTitle "Add Location"
                $(widgetFile "add-location")
