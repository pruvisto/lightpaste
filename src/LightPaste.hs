module Main (main) where

import Network.CGI
import System.Directory
import System.Random
import qualified System.Cmd (rawSystem)
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
webDir = "/var/www" ++ webRoot
isaTokenFile = "isatokens"
autoTouchFiles = False


globalTemplateSubsts = [(SBS.pack "%{root}", BS.pack webRoot)]

data Paste = Paste {title :: String, author :: String, highlighting :: String,
                    time :: UTCTime, content :: BS.ByteString, 
                    lineNumberWidth :: Int}
                    
highlightings = ["","bash","brainfuck","clojure","cmake","coffeescript","cpp","cs","css","delphi",
                 "diff","django","d","dos","erlang","fsharp","glsl","go","haskell","http","ini",
                 "isabelle", "java","javascript","json","lisp","lua","matlab","mizar","objectivec","perl",
                 "php","python","r","ruby","scala","smalltalk","sml","sql","tex","vala","vbnet",
                 "vbscript","vhdl","xml"]
highlightingNames = ["None","Bash","Brainfuck","Clojure","CMake","CoffeeScript","C++","C#","CSS","Delphi",
                 "diff","Django","D","DOS","Erlang","F#","GLSL","GO","Haskell","HTTP","INI","Isabelle",
                 "Java","JavaScript","JSON","Lisp","Lua","Matlab","Mizar","ObjectiveC","Perl",
                 "PHP","Python","R","Ruby","Scala","Smalltalk","Standard ML","SQL","TeX","Vala","VBnet",
                 "VBScript","VHDL","XML/HTML"]

                 
ensureValidHighlighting hl = if hl `elem` highlightings then hl else ""
                    
firstLine s = case lines s of {[] -> ""; l:_ -> l}
                    
mkPaste' title author hl time content lineNumberWidth = Paste title' author' hl' time content lineNumberWidth
    where title' = firstLine title
          author' = firstLine author
          hl' = ensureValidHighlighting hl
          
mkPaste title author hl time content = mkPaste' title author hl time content
                                           (length $ show $ BS.count '\n'  content)

setContent (Paste title author hl time _ lineNumberWidth) content =
    Paste title author hl time content lineNumberWidth
          
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

loadPaste id = 
    do  bs1 <- BS.readFile (pasteDir ++ id)
        bs2 <- BS.readFile (webDir ++ "pastes/plain/" ++ id ++ ".txt")
        return (byteStringToPaste bs1 bs2)

loadProcessedPaste id = 
    do  bs1 <- BS.readFile (pasteDir ++ id)
        ex <- doesFileExist (webDir ++ "pastes/" ++ id ++ ".html")
        if ex then do
            bs2 <- BS.readFile (webDir ++ "pastes/" ++ id ++ ".html")
            return (byteStringToPaste bs1 bs2)
        else do
            bs2 <- BS.readFile (webDir ++ "pastes/plain/" ++ id ++ ".txt")
            bs2' <- doProcessContent id (byteStringToPaste bs1 bs2)
            BS.writeFile (webDir ++ "pastes/" ++ id ++ ".html") bs2'
            return (byteStringToPaste bs1 bs2')
        
savePaste id paste = 
    do BS.writeFile (pasteDir ++ id) (pasteMetadataToByteString paste)
       BS.writeFile (webDir ++ "pastes/plain/" ++ id ++ ".txt") (content paste)
       processedContent <- doProcessContent id paste
       BS.writeFile (webDir ++ "pastes/" ++ id ++ ".html") processedContent
       
processContent isaTokenMap highlighting bs = 
    BS.unlines $ map processLine $ BS.lines $ BS.filter (/='\r') $
        case isaTokenMap of
            Just m -> if highlighting == "isabelle" then
                          IsabelleTokens.replaceTokens m $ escapeHtml $ bs
                      else
                          escapeHtml $ bs
            Nothing -> escapeHtml $ bs
    where bsLineSpan = BS.pack "<span class=\"line\"></span>"
          processLine l = BS.append bsLineSpan l

doProcessContent :: String -> Paste -> IO BS.ByteString
doProcessContent id paste = 
    do isaTokenMap <- if highlighting paste == "isabelle" then
                          IsabelleTokens.buildTokenMap isaTokenFile
                      else
                          return Nothing
       let processed = processContent isaTokenMap (highlighting paste) (content paste)
       let paste' = setContent paste processed
       hlScript <- readTemplate "highlight_script" []
       readTemplate "show_paste" (("id", BS.pack id) : pasteSubsts hlScript paste')

    
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

readTemplate :: String -> [(String, BS.ByteString)] -> IO BS.ByteString
readTemplate name substs = 
    do  let path = "./" ++ name ++ ".html"
        ex <- doesFileExist path
        if not ex then
            if name == "template_not_found" then
                return (BS.pack "")
            else
                readTemplate "template_not_found" [("template", BS.pack name)]
        else do
            bs <- BS.readFile path
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

createPaste :: BS.ByteString -> CGI CGIResult
createPaste content =
    do id <- liftIO findFreshId
       title <- getInputDefault "title" "Untitled paste"
       author <- getInputDefault "author" "Anonymous"
       highlighting <- getInputDefault "highlighting" "getInputContentType"
       time <- liftIO getCurrentTime
       let paste = mkPaste title author highlighting time content
       liftIO (savePaste id paste)
       redirect (webRoot ++ id)

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
                   bs <- liftIO (BS.readFile (webDir ++ "pastes/plain/" ++ id ++ ".txt"))
                   setHeader "Content-type" "text/plain; charset=utf-8"
                   outputFPS bs
               else do
                   paste <- liftIO (loadProcessedPaste id)
                   setHeader "Content-type" "text/html; charset=utf-8"
                   outputFPS (content paste)
       
showCreatePasteForm cloneId = 
    do 
       paste <- case cloneId of 
                    Just id -> Just <$> liftIO (loadPaste id)
                    Nothing -> return Nothing
       let hlOptions = genHighlightingOptions (highlighting <$> paste)
       let pTitle = BS.pack (maybe "" title paste)
       let pContent = maybe BS.empty content paste
       tmpl <- liftIO $ readTemplate "new_paste" 
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
        

