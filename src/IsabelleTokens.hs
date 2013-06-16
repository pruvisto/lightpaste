module IsabelleTokens (buildTokenMap, replaceTokens) where

import qualified Data.ByteString.Char8 as SBS
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy.Search as BSS
import qualified Data.HashMap as HM

data Attribute = Bold | Underline deriving (Show,Eq,Read)

bsBold = BS.pack "font-weight:bold"
bsUnderline = BS.pack "text-decoration:underline"
attribToCSS Bold = bsBold
attribToCSS Underline = bsUnderline

bsSemi = BS.pack ";"
bsSpan1 = BS.pack "<span style=\""
bsSpan2 = BS.pack "\">"
bsSpan3 = BS.pack "</span>"

addAttributes [] s = s
addAttributes as s = BS.concat [bsSpan1, BS.intercalate bsSemi (map attribToCSS as), bsSpan2, s, bsSpan3]

type TokenMap = HM.Map String (String,BS.ByteString,[Attribute])

buildTokenMap :: String -> IO (Maybe TokenMap)
buildTokenMap path = 
    do s <- readFile path
       let rawTokens = case reads s of
                           (x, _):_ -> Just (x::[(String,String,[Attribute])])
                           _         -> Nothing
       case rawTokens of
           Just rawTokens -> 
             do let tokens = map (\(s,r,a) -> (s, BS.pack r, a)) rawTokens
                return (Just (foldl (\m t@(s,_,_) -> HM.insert s t m) HM.empty tokens))
           Nothing -> return Nothing

tokenStart = "\\&lt;"
tokenStartLength = length tokenStart
bsTokenStart = BS.pack tokenStart
sbsTokenStart = SBS.pack tokenStart

tokenEnd = "&gt;"
tokenEndLength = length tokenEnd
bsTokenEnd = BS.pack tokenEnd
sbsTokenEnd = SBS.pack tokenEnd

bsSup = BS.pack "\\&lt;^sup&gt;"
bsiSup = BS.pack "\\&lt;^isup&gt;"
bsSub = BS.pack "\\&lt;^sub&gt;"
bsiSub = BS.pack "\\&lt;^isub&gt;"
bsBSub = BS.pack "\\&lt;^bsub&gt;"
bsESub = BS.pack "\\&lt;^esub&gt;"
bsBSup = BS.pack "\\&lt;^bsup&gt;"
bsESup = BS.pack "\\&lt;^esup&gt;"

lookupToken t tokenMap = 
    HM.lookup (drop tokenStartLength $ take (length t' - tokenEndLength) t')
               tokenMap
    where t' = BS.unpack t

tokenToBS (_, r, as)  = addAttributes as r

isToken bs = BS.isPrefixOf bsTokenStart bs && isSuffixOf bsTokenEnd bs
    where isSuffixOf suffix bs = suffix == BS.drop (BS.length bs - BS.length suffix) bs

processSubSup acc (mod:bs:bss)
    | mod == bsSup || mod == bsiSup = addModifier bsBSup bsESup bs bss
    | mod == bsSub || mod == bsiSub = addModifier bsBSub bsESub bs bss
    where addModifier mb me bs bss
              | isToken bs = processSubSup (me : bs : mb : acc) bss
              | otherwise = processSubSup (me : BS.take 1 bs : mb : acc) (BS.drop 1 bs : bss)
processSubSup acc (bs:bss) = processSubSup (bs:acc) bss
processSubSup acc [] = acc

replaceTokens :: TokenMap -> BS.ByteString -> BS.ByteString
replaceTokens m bs =
    let bs' = concatMap (BSS.splitKeepEnd sbsTokenEnd) $ BSS.splitKeepFront sbsTokenStart bs
        bss' = processSubSup [] bs'
        bss'' = foldl (\bss bs -> replaceToken bs : bss) [] bss'
     in BS.concat bss''
    where replaceToken t
              | isToken t = maybe t tokenToBS (lookupToken t m)
              | otherwise = t


