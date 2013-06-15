import Network.CGI
import System.Directory
import System.Random
import Data.Char
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.ByteString.Lazy as BS
import Control.Applicative
import Data.Time


pasteDir = "/var/pastes/"
webRoot = "/paste/"


globalTemplateSubsts = [(T.pack "%{root}", T.pack webRoot)]

data Paste = Paste {title :: String, author :: String, highlighting :: String,
                    time :: UTCTime, content :: T.Text}
                    
highlightings = ["","bash","brainfuck","clojure","cmake","coffeescript","cpp","cs","css","delphi",
                 "diff","django","d","dos","erlang","fsharp","glsl","go","haskell","http","ini",
                 "java","javascript","json","lisp","lua","matlab","mizar","objectivec","perl",
                 "php","python","r","ruby","scala","smalltalk","sql","tex","vala","vbnet",
                 "vbscript","vhdl","xml"]
highlightingNames = ["None","Bash","Brainfuck","Clojure","CMake","CoffeeScript","C++","C#","CSS","Delphi",
                 "diff","Django","D","DOS","Erlang","F#","GLSL","GO","Haskell","HTTP","INI",
                 "Java","JavaScript","JSON","Lisp","Lua","Matlab","Mizar","ObjectiveC","Perl",
                 "PHP","Python","R","Ruby","Scala","Smalltalk","SQL","TeX","Vala","VBnet",
                 "VBScript","VHDL","XML/HTML"]
                 
ensureValidHighlighting hl = if hl `elem` highlightings then hl else ""
                    
firstLine s = case lines s of {[] -> ""; l:_ -> l}
                    
mkPaste title author hl time content = Paste title' author' hl' time content
    where title' = firstLine title
          author' = firstLine author
          hl' = ensureValidHighlighting hl

pasteSubsts paste = [("title", escapeHtml $ T.pack $ title paste),
                     ("author", escapeHtml $ T.pack $ author paste), 
                     ("highlighting", T.pack $ highlighting paste),
                     ("time", escapeHtml $ T.pack $ show $ time paste), 
                     ("content", escapeHtml $ content paste),
                     ("line_numbers", genLineNumbers (content paste))]

pasteToText (Paste title author highlighting time content) =
    T.append (T.pack (title ++ "\n" ++ author ++ "\n" ++ 
                      highlighting ++ "\n" ++ show time ++ "\n")) content

textToPaste text = mkPaste title author highlighting (read time) content
    where (firstThree, rest) = splitAt 4 (T.lines text)
          [title, author, highlighting, time] = map T.unpack firstThree
          content = T.unlines rest

loadPaste path = textToPaste <$> TIO.readFile path

savePaste path paste = TIO.writeFile path (pasteToText paste)

genLineNumbers t = T.intercalate (T.pack "<br/>") (map (T.pack . show) [1..n])
    where n = length (T.lines t)
    
genHighlightingOptions sel = T.concat $ zipWith go highlightings highlightingNames
    where go hl hln = T.pack $ "<option value=\"" ++ hl ++ "\"" ++ 
                          (if Just hl == sel then " selected=\"selected\"" else "") ++
                          ">" ++ hln ++ "</option>\n"

text_amp = T.pack "&amp;"
text_lt = T.pack "&lt;"
text_gt = T.pack "&gt;"

escapeHtml = T.concatMap escapeChar
    where escapeChar '&' = text_amp
          escapeChar '<' = text_lt
          escapeChar '>' = text_gt
          escapeChar c = T.singleton c

idChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
idLength = 8

randomString :: [Char] -> Int -> [Int] -> (String, [Int])
randomString alphabet length randoms = 
    (map (idChars !!) beginning, rest)
    where (beginning, rest) = splitAt length randoms

applyTemplateSubsts text substs = foldl (\t (l, r) -> T.replace l r t) text substs'
    where substs' = map (\(l,r) -> (T.pack ("%{"++l++"}"), r)) substs ++ globalTemplateSubsts

readTemplate :: String -> [(String, T.Text)] -> CGI T.Text
readTemplate name substs = 
    do  let path = "./" ++ name ++ ".html"
        ex <- liftIO (doesFileExist path)
        if not ex then
            if name == "template_not_found" then
                return (T.pack "")
            else
                readTemplate "template_not_found" [("template", T.pack name)]
        else do
            text <- liftIO (TIO.readFile path)
            return (applyTemplateSubsts text substs)
        

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
               let bs = TE.encodeUtf8 (content paste)
               plain <- getInputOption "plain"
               if plain then do
                   setHeader "Content-type" "text/plain"
                   outputFPS bs
               else do 
                   html <- readTemplate "show_paste" 
                               (("id", T.pack id) : pasteSubsts paste)
                   outputFPS (TE.encodeUtf8 html)
                   

createPaste :: BS.ByteString -> CGI CGIResult
createPaste content =
    do id <- liftIO findFreshId
       title <- getInputDefault "title" "Untitled paste"
       author <- getInputDefault "author" "Anonymous"
       highlighting <- getInputDefault "highlighting" ""
       time <- liftIO getCurrentTime
       let paste = mkPaste title author highlighting time (TE.decodeUtf8 content)
       let pastePath = pasteDir ++ id
       liftIO (savePaste pastePath paste)
       redirect (webRoot ++ id)
       
       
showCreatePasteForm cloneId = 
    do 
       paste <- case cloneId of 
                    Just id -> Just <$> liftIO (loadPaste (pasteDir ++ id))
                    Nothing -> return Nothing
       let hlOptions = genHighlightingOptions (highlighting <$> paste)
       let pTitle = T.pack (maybe "" title paste)
       let pContent = maybe T.empty content paste
       tmpl <- readTemplate "new_paste" 
                   [("title", pTitle), ("highlightings", hlOptions), 
                    ("content", pContent)]
       outputFPS (TE.encodeUtf8 tmpl)
 
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
        

