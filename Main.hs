{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
import Reflex
import Reflex.Dom
import Reflex.Dom.Time
import Data.Map as DM (Map, fromList)
import Data.Text (Text, pack)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans (liftIO)

main :: IO ()
main = mainWidget $ do 
  showBoard 

showBoard :: MonadWidget t m => m ()
showBoard = do
  let boardAttrs = 
         fromList [ ("width" , "400")
                  , ("height", "400")
                  ]
  elSvgns "svg" (constDyn $ boardAttrs ) $ showLine 

showLine :: MonadWidget t m => m ()
showLine = do
  now <- liftIO getCurrentTime 
  ticks <- tickLossy 0.01 now
  counter <- foldDyn (\_ c -> c+1) (0::Int) ticks

  let 
      lineAttrs count = 
         let slope :: Float
             slope = fromIntegral count / 100.0 
             len = 100.0
         in fromList [ ("x1", "0")
                     , ("y1", "0")
                     , ("x2", pack $ show $ len * cos(slope))
                     , ("y2", pack $ show $ len * sin(slope))
                     , ("stroke-width", "2")
                     , ("stroke", "black")
                     ]

  elSvgns "line"  (fmap lineAttrs counter) $ return ()

-- Wrapper around elDynAttrNS'
elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m a
elSvgns t m ma = do
    (el, val) <- elDynAttrNS' (Just "http://www.w3.org/2000/svg") t m ma
    return val

