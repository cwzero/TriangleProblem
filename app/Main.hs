module Main (main) where

import TriangleTypes

main :: IO ()
main = do 
    let i = [
            "abc",
            "alw",
            "cmw",
            "blv",
            "bmx",
            "vwx"
            ]

    let j = [
            "abc",
            "adhjn",
            "altw",
            "av",
            "bdlv",
            "bhmpx",
            "cjmrw",
            "nptv",
            "vwx"
            ]

    let k = [
            "abc",
            "adfhjn",
            "ailqtw",
            "akv",
            "bdglov",
            "behmpx",
            "cefgik",
            "cjmruw",
            "cnx",
            "koqsux",
            "nprstv",
            "vwx"
            ]

    print $ countTriangles i
    print $ countTriangles j
    print $ countTriangles k
