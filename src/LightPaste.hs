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


pasteDir = "/var/pastes/"
webRoot = "/paste/"


globalTemplateSubsts = [(SBS.pack "%{root}", BS.pack webRoot)]

data Paste = Paste {title :: String, author :: String, highlighting :: String,
                    time :: UTCTime, content :: BS.ByteString}
                    
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
                    
mkPaste title author hl time content = Paste title' author' hl' time content
    where title' = firstLine title
          author' = firstLine author
          hl' = ensureValidHighlighting hl
          
pasteSubsts isaTokenMap paste = 
    [("title", escapeHtml $ BS.pack $ title paste),
     ("author", escapeHtml $ BS.pack $ author paste), 
     ("highlighting", BS.pack $ highlighting paste),
     ("time", escapeHtml $ BS.pack $ show $ time paste), 
     ("content", case isaTokenMap of
                     Just m -> if highlighting paste == "isabelle" then
                                   IsabelleTokens.replaceTokens m $ escapeHtml $ content paste
                               else
                                   escapeHtml $ content paste
                     Nothing -> escapeHtml $ content paste),
     ("line_numbers", genLineNumbers (content paste))]

pasteToByteString (Paste title author highlighting time content) =
    BS.append (BS.pack (title ++ "\n" ++ author ++ "\n" ++ 
                      highlighting ++ "\n" ++ show time ++ "\n")) content

splitOffLines :: Int -> BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitOffLines n bs = case (splitOffLines' n ([], bs)) of (xs,bs) -> (reverse xs,bs)
    where splitOffLines' 0 s = s
          splitOffLines' n (xs,bs) =
              case BS.break isLineSep bs of
                  (x,bs) -> let bs' = BS.drop 1 bs
                             in splitOffLines' (n-1) (x:xs, bs')
          isLineSep c = c == '\n' || c == '\r'

byteStringToPaste bs = mkPaste title author highlighting (read time) content
    where (header, content) = splitOffLines 4 bs
          [title, author, highlighting, time] = map BS.unpack header

loadPaste path = byteStringToPaste <$> BS.readFile path

savePaste path paste = BS.writeFile path (pasteToByteString paste)

genLineNumbers t = BS.intercalate (BS.pack "<br/>") (map (BS.pack . show) [1..n])
    where n = length (BS.lines t)
    
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
               paste <- liftIO (loadPaste pastePath)
               let bs = content paste
               plain <- getInputOption "plain"
               if plain then do
                   setHeader "Content-type" "text/plain; charset=utf-8"
                   outputFPS bs
               else do
                   isaTokenMap <- liftIO (IsabelleTokens.buildTokenMap "isatokens")
                   html <- readTemplate "show_paste" 
                               (("id", BS.pack id) : pasteSubsts isaTokenMap paste)
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
        

