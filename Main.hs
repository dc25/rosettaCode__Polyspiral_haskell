{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Reflex.Dom.Time
import Data.Text (Text, pack) 
import Data.Map (Map, fromList)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans (liftIO)

width = 800
height = 800

type Point = (Float,Float)
type Segment = (Point,Point)

main = mainWidget $ do 

  let boardAttrs = 
         fromList [ ("width" , pack $ show width)
                  , ("height", pack $ show height)
                  ]

  now <- liftIO getCurrentTime 
  dTicks <- tickLossy 0.05 now
  dCounter <- foldDyn (\_ c -> c+1) (0::Int) dTicks

  let dAngle = fmap (\c -> fromIntegral c / 800.0) dCounter

  el "div" 
    $ elSvgns "svg" (constDyn boardAttrs) 
    $ listWithKey (fmap toLineMap dAngle) showLine

  return ()

showLine :: MonadWidget t m => Int -> Dynamic t Segment -> m ()
showLine _ dSegment = elSvgns "line" (lineAttrs <$> dSegment) $ return ()

advance :: Float -> (Point, Float, Float) -> (Point, Float, Float)
advance angle ((x,y), len, rot) = 
  let new_x = x + len * cos rot
      new_y = y + len * sin rot
      new_len = len + 3.0 
      new_rot = rot + angle
  in ((new_x, new_y), new_len, new_rot)

segments :: [Point] -> [Segment]
segments  pts  =  zip pts $ tail pts

toLineMap :: Float -> Map Int ((Float,Float),(Float,Float))
toLineMap angle =
      fromList   -- changes list to map (for listWithKey)
  $   zip [0..]  -- annotates segments with index
  $   segments   -- changes points to line segments
  $   take 40    -- limit the number of points
  $   (\(pt,_,_) -> pt)  -- cull out the (x,y) values
  <$> iterate (advance angle) ((0.0, 0.0), 0.0, 0.0) 

lineAttrs :: Segment -> Map Text Text
lineAttrs ((x1,y1), (x2,y2)) =
  fromList [ ( "x1",    pack $ show (width/2+x1))
           , ( "y1",    pack $ show (height/2+y1))
           , ( "x2",    pack $ show (width/2+x2))
           , ( "y2",    pack $ show (height/2+y2))
           , ( "style", "stroke:red;stroke-width:2")
           ]    
         
-- Wrapper around elDynAttrNS'
elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m a
elSvgns t m ma = do
    (el, val) <- elDynAttrNS' (Just "http://www.w3.org/2000/svg") t m ma
    return val

