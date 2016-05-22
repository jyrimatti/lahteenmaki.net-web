#!/usr/bin/ghc -outputdir /tmp/
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
import Network.FastCGI
import System.Environment.Executable 
import System.Directory

import qualified Data.CaseInsensitive as CI
import Prelude hiding (length,catch)
import Data.Maybe
import Data.Char (toLower)
import Data.Either
import Data.Text.Lazy.Encoding
import Data.List (isPrefixOf, isSuffixOf, stripPrefix, isInfixOf)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (ByteString, length,hGetContents, hPut)
import Network.HTTP hiding (getHeaders,Response)
import Network.HTTP.Types.Status (Status(..), badRequest400)
import Network.HTTP.Conduit hiding (queryString)
import Data.Foldable (find)
import Control.Exception.Lifted (catch)

modTime = getScriptPath >>= \script -> getModificationTime $ case script of
    Executable fp -> fp
    RunGHC fp -> fp

main = modTime >>= mainn
mainn started = modTime >>= \modified -> if modified /= started then (return ()) else runOneFastCGIorCGI app >>= \isFast -> if isFast then mainn started else return ()

data Response = Response {
    status :: Status,
    headers :: [Header],
    contentType :: String,
    content :: ByteString
}

getHeaders :: [(String,String)] -> [(String,String)]
getHeaders = map (\(k,v) -> (map toLower k, v)) . map (\(k,v) -> (replaceWith '_' '-' k, v)) . map (\(k,v) -> (fromJust $ stripPrefix "HTTP_" k,v)) . filter (isPrefixOf "HTTP_" . fst)
  where replaceWith char with = map (\c -> case c of cc | cc == char -> with; _ -> c)

app :: CGI CGIResult
app = handleErrors $ do
  content <- getBodyFPS
  q <- queryString
  method <- requestMethod
  vars <- getVars
  let Right headers = parseHeaders $ map (\(k,v) -> k ++ ": " ++ v) $ getHeaders vars
--  liftIO $ putStrLn $ "Incoming headers: " ++ show headers
  Response (Status code _) he ct co <- liftIO $ foo q headers content method
  setStatus code ""
  sequence_ $ map (\(Header hname hval) -> do
--    liftIO $ putStrLn $ "setting header " ++ (show hname) ++ ": " ++ hval
    setHeader (show hname) hval) he
  outputFPS co

foo query hdrs content method = do
  case parseUrl query of
    Nothing -> error ("Could not parse url: " ++ query)
    Just initReq -> do
      let headersToForward = filter (\h -> not $ elem (hdrName h) [HdrHost,HdrContentLength,HdrAcceptEncoding]) hdrs
      let req = initReq {
        secure = "https://" `isPrefixOf` query,
        method = B.pack $ method,
        requestHeaders = map header2conduitHeader headersToForward,
        requestBody = RequestBodyLBS content,
        decompress = const False
      }
--      putStrLn $ "forwarding request to: " ++ show req ++ "\n with headers: " ++ show headersToForward ++ "\n with content: " ++ show content
      tryForward req `catch` \e -> case e of
        StatusCodeException s hs _  -> do
--            putStrLn $ "error: " ++ show e
            let headersToRespond = filter (\h -> not $ elem (show $ hdrName h) ["Content-Length","Content-Type"]) (map conduitHeader2header hs)
            return $ Response s (withACheaders hdrs headersToRespond) "text/plain" "a"
        e@(InvalidUrlException _ _) -> do putStrLn $ "error: " ++ show e; return $ Response badRequest400 [] "text/plain" "InvalidUrlException"
        e@(TooManyRedirects _)      -> do putStrLn $ "error: " ++ show e; return $ Response badRequest400 [] "text/plain" "TooManyRedirects"
        e@(HttpParserException _)   -> do putStrLn $ "error: " ++ show e; return $ Response badRequest400 [] "text/plain" "HttpParserException"
        e@HandshakeFailed           -> do putStrLn $ "error: " ++ show e; return $ Response badRequest400 [] "text/plain" "HandshakeFailed"
  where tryForward req = do
            resp <- withManager $ httpLbs req
            let rCode = responseStatus resp
            let rBody = responseBody resp
            let rHeaders = map conduitHeader2header $ responseHeaders resp
--            putStrLn $ "Received body: " ++ show rBody
            let headersToRespond = filter (\(Header hname _) -> show hname /= "Transfer-Encoding") rHeaders
            let contentLength = fromIntegral $ length rBody
            let contentType = fromMaybe (Header HdrContentType "text/plain") $ find (\(Header hname _) -> show hname == "Content-Type") rHeaders
            return $ Response rCode (withACheaders hdrs headersToRespond) (hdrValue contentType) rBody
        header2conduitHeader h = (CI.mk $ B.pack $ show $ hdrName h, B.pack $ hdrValue h)
        conduitHeader2header (n,v) = mkHeader (HdrCustom $ B.unpack $ CI.original n) (B.unpack v)
        withACheaders hdrs headers =
            let request = find (\(Header hname _) -> show hname == "Access-Control-Request-Headers" ) hdrs
                allowOrigin = mkHeader (HdrCustom "Access-Control-Allow-Origin") "*"
                allowHeaders = case request of
                    Nothing -> []
                    Just (Header _ value) -> [mkHeader (HdrCustom "Access-Control-Allow-Headers") value]
            in
            concat [allowHeaders, [allowOrigin], headers]
