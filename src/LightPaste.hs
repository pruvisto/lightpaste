module Main (main) where

import Network.CGI
import System.Directory
import System.Random
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Search as BSS
import Control.Applicative
import Data.Time
import IsabelleTokens
import Text.Printf (printf)


pasteDir = "/var/pastes/"
webRoot = "/paste/"
isaTokenFile = "isatokens"


globalTemplateSubsts = [(SBS.pack "%{root}", BS.pack webRoot)]

data Paste = Paste {title :: String, author :: String, highlighting :: String,
                    time :: UTCTime, content :: BS.ByteString, 
                    lineNumberWidth :: Int}
                    
highlightings = ["","bash","brainfuck","clojure","cmake","coffeescript","cpp","cs","css","delphi",
                 "diff","django","d","dos","erlang","fsharp","glsl","go","haskell","http","ini",
                 "isabelle", "java","javascript","json","lisp","lua","matlab","mizar","objectivec","perl",
                 "php","python","r","ruby","scala","smalltalk","sql","tex","vala","vbnet",
                 "vbscript","vhdl","xml"]
highlightingNames = ["None","Bash","Brainfuck","Clojure","CMake","CoffeeScript","C++","C#","CSS","Delphi",
                 "diff","Django","D","DOS","Erlang","F#","GLSL","GO","Haskell","HTTP","INI","Isabelle",
                 "Java","JavaScript","JSON","Lisp","Lua","Matlab","Mizar","ObjectiveC","Perl",
                 "PHP","Python","R","Ruby","Scala","Smalltalk","SQL","TeX","Vala","VBnet",
                 "VBScript","VHDL","XML/HTML"]

                 
ensureValidHighlighting hl = if hl `elem` highlightings then hl else ""
                    
firstLine s = case lines s of {[] -> ""; l:_ -> l}
                    
mkPaste' title author hl time content lineNumberWidth = Paste title' author' hl' time content lineNumberWidth
    where title' = firstLine title
          author' = firstLine author
          hl' = ensureValidHighlighting hl
          
mkPaste title author hl time content = mkPaste' title author hl time content
                                           (length $ show $ BS.count '\n'  content)
          
pasteSubsts highlightScript paste = 
    [("title", escapeHtml $ BS.pack $ title paste),
     ("author", escapeHtml $ BS.pack $ author paste), 
     ("highlighting", BS.pack $ (if null (highlighting paste) then "" 
                                 else " class=\"" ++ highlighting paste ++ "\"")),
     ("time", escapeHtml $ BS.pack $ show $ time paste), 
     ("content", content paste),
     ("line_number_width", BS.pack $ printf "%.1fem" $
         fromIntegral (lineNumberWidth paste) * (0.7::Double)),
     ("highlight_script", if null (highlighting paste) then BS.empty else highlightScript)]

pasteMetadataToByteString (Paste title author highlighting time content lineNumberWidth) =
    BS.pack (title ++ "\n" ++ author ++ "\n" ++ 
             highlighting ++ "\n" ++ show time ++ "\n" ++ show lineNumberWidth)

byteStringToPaste bs1 bs2 = mkPaste' title author highlighting (read time) 
                                bs2 (read lineNumberWidth)
    where title:author:highlighting:time:lineNumberWidth:_ = 
              map BS.unpack (BS.lines bs1) ++ repeat ""

loadPaste path = 
    do  bs1 <- BS.readFile path
        bs2 <- BS.readFile (path ++ "_content")
        return (byteStringToPaste bs1 bs2)
        
loadProcessedPaste path = 
    do  bs1 <- BS.readFile path
        bs2 <- BS.readFile (path ++ "_processed")
        return (byteStringToPaste bs1 bs2)

savePaste path paste = 
    do BS.writeFile path (pasteMetadataToByteString paste)
       BS.writeFile (path ++ "_content") (content paste)
       saveProcessedPaste path paste
       
processContent isaTokenMap highlighting bs = 
    BS.unlines $ map (\l -> BS.concat [bsLineSpan1, l, bsLineSpan2]) $
        BS.lines $ BS.filter (/='\r') $
        case isaTokenMap of
            Just m -> if highlighting == "isabelle" then
                          IsabelleTokens.replaceTokens m $ escapeHtml $ bs
                      else
                          escapeHtml $ bs
            Nothing -> escapeHtml $ bs
    where bsLineSpan1 = BS.pack "<span class=\"line\">"
          bsLineSpan2 = BS.pack "</span>"

saveProcessedPaste path paste = 
    do isaTokenMap <- if highlighting paste == "isabelle" then
                          IsabelleTokens.buildTokenMap isaTokenFile
                      else
                          return Nothing
       let bs = processContent isaTokenMap (highlighting paste) (content paste)
       BS.writeFile (path ++ "_processed") bs

    
genHighlightingOptions sel = BS.concat $ zipWith go highlightings highlightingNames
    where go hl hln = BS.pack $ "<option value=\"" ++ hl ++ "\"" ++ 
                          (if Just hl == sel then " selected=\"selected\"" else "") ++
                          ">" ++ hln ++ "</option>\n"

bs_amp = BS.pack "&amp;"
bs_lt = BS.pack "&lt;"
bs_gt = BS.pack "&gt;"

escapeHtml = BS.concatMap escapeChar
    where escapeChar '&' = bs_amp
          escapeChar '<' = bs_lt
          escapeChar '>' = bs_gt
          escapeChar c = BS.singleton c

idChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
idLength = 8

randomString :: [Char] -> Int -> [Int] -> (String, [Int])
randomString alphabet length randoms = 
    (map (idChars !!) beginning, rest)
    where (beginning, rest) = splitAt length randoms

applyTemplateSubsts :: BS.ByteString -> [(String,BS.ByteString)] -> BS.ByteString
applyTemplateSubsts bs substs = foldl (\t (l, r) -> BSS.replace l r t) bs substs'
    where substs' = map (\(l,r) -> (SBS.pack ("%{"++l++"}"), r)) substs ++ globalTemplateSubsts

readTemplate :: String -> [(String, BS.ByteString)] -> CGI BS.ByteString
readTemplate name substs = 
    do  let path = "./" ++ name ++ ".html"
        ex <- liftIO (doesFileExist path)
        if not ex then
            if name == "template_not_found" then
                return (BS.pack "")
            else
                readTemplate "template_not_found" [("template", BS.pack name)]
        else do
            bs <- liftIO (BS.readFile path)
            return (applyTemplateSubsts bs substs)
        

findFreshId :: IO String
findFreshId =
    do g <- newStdGen
       let rs = randomRs (0, length idChars - 1) g
       findFreshId' rs
    where findFreshId' rs =
              do let (id, rs') = randomString idChars idLength rs
                 ex <- doesFileExist (pasteDir ++ id)
                 if ex then findFreshId' rs' else return id
                 
getInputDefault name dflt =
    do value <- getInput name
       case value of
           Nothing -> return dflt
           Just v -> if null v then return dflt else return v
           
getInputOption name = maybe False (const True) <$> getInput name

showPaste id
    | not (all isAlphaNum id) = showCreatePasteForm Nothing
    | otherwise =
        do let pastePath = pasteDir ++ id
           ex <- liftIO (doesFileExist pastePath)
           if not ex then
               showCreatePasteForm Nothing
           else do
               plain <- getInputOption "plain"
               if plain then do
                   bs <- liftIO (BS.readFile (pastePath ++ "_content"))
                   setHeader "Content-type" "text/plain; charset=utf-8"
                   outputFPS bs
               else do
                   paste <- liftIO (loadProcessedPaste pastePath)
                   hlScript <- readTemplate "highlight_script" []
                   html <- readTemplate "show_paste" 
                               (("id", BS.pack id) : pasteSubsts hlScript paste)
                   setHeader "Content-type" "text/html; charset=utf-8"
                   outputFPS html
                   

createPaste :: BS.ByteString -> CGI CGIResult
createPaste content =
    do id <- liftIO findFreshId
       title <- getInputDefault "title" "Untitled paste"
       author <- getInputDefault "author" "Anonymous"
       highlighting <- getInputDefault "highlighting" "getInputContentType"
       time <- liftIO getCurrentTime
       let paste = mkPaste title author highlighting time content
       let pastePath = pasteDir ++ id
       liftIO (savePaste pastePath paste)
       redirect (webRoot ++ id)
       
       
showCreatePasteForm cloneId = 
    do 
       paste <- case cloneId of 
                    Just id -> Just <$> liftIO (loadPaste (pasteDir ++ id))
                    Nothing -> return Nothing
       let hlOptions = genHighlightingOptions (highlighting <$> paste)
       let pTitle = BS.pack (maybe "" title paste)
       let pContent = maybe BS.empty content paste
       tmpl <- readTemplate "new_paste" 
                   [("title", pTitle), ("highlightings", hlOptions), 
                    ("content", pContent)]
       setHeader "Content-type" "text/html; charset=utf-8"
       outputFPS tmpl
 
cgiMain = 
    do  id <- getInput "id"
        paste <- getInputFPS "paste"
        clone <- getInputOption "clone"
        case (id,paste,clone) of
            (Just id, _, False) -> showPaste id
            (Just id, _, True) -> showCreatePasteForm (Just id)
            (_, Just paste, _) -> createPaste paste
            _ -> showCreatePasteForm Nothing
 
main :: IO ()
main = runCGI $ handleErrors cgiMain
        

