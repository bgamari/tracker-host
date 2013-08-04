{-# LANGUAGE OverloadedStrings #-}

import qualified Tracker as T

main = do
    Just t <- T.open
    T.echo t "Hello World!" >>= print
    T.close t
