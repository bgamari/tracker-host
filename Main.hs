{-# LANGUAGE OverloadedStrings #-}

import qualified Tracker as T

main = do
    Just t <- T.open
    T.echo t "asdf" >>= print
    print "hello world"

    T.close t
