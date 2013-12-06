module Main where
import Haste
import Control.Monad (when)

newtype JQuery = JQuery JSAny

-- Gives jQuery from JS side a things
foreign import ccall js_jquery :: JSString -> IO (JQuery)

-- Click callback from JS
foreign import ccall js_click :: JQuery -> JSFun (String -> IO ()) -> IO ()

-- All the functions which can be ran from Haskell
foreign import ccall selectBoat :: JSString -> IO ()
foreign import ccall flipBoat :: JSString -> IO ()
foreign import ccall lockBoat :: JSString -> IO ()
foreign import ccall startGame :: JSString -> IO ()
foreign import ccall resetGame :: JSString -> IO ()
foreign import ccall addBoat :: JSString -> IO ()

-- Define jQuery as j and a function binding an action onto it on the JS side
j :: String -> (JQuery -> IO ()) -> IO ()
j s action = js_jquery (toJSString s) >>= action

-- Register an onclick callback evaluating the .
click :: (String -> IO ()) -> JQuery -> IO ()
click f jq = js_click jq (mkCallback f)

-- Lets get down to binding to JS functions
-- When clauses are for hooking into game logic
main = do
  j ".boat" $
    click (\idx
              -> when (True) (selectBoat (toJSString idx)))
  j "button#flip" $
      click (\idx
              -> when (True) (flipBoat (toJSString idx)))
  j "button#lock" $
    click (\idx
              -> when (True) (lockBoat (toJSString idx)))
  j "button#start" $
    click (\idx
              -> when (True) (startGame (toJSString idx)))
  j "button#reset" $
    click (\idx
              -> when (True) (resetGame (toJSString idx)))
  j ".boat" $
    click (\idx
              -> when (True) (selectBoat (toJSString idx)))
  j "tbody td:not(.shead)" $
    click (\idx
              -> when (True) (addBoat (toJSString idx)))