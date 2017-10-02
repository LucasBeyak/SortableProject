{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Control.Monad (mzero)
import Data.Aeson (
    FromJSON, ToJSON, Value(Object),
    (.:), (.:?), (.=),
    decode, decodeStrict, defaultOptions, encode, genericToEncoding, object, pairs, parseJSON, toEncoding, toJSON)
import Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import qualified Data.ByteString.Lazy.Char8 as BLC (lines, unlines)
import Data.HashMap.Lazy (fromListWith, lookupDefault)
import Data.List (groupBy, sortBy)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>))
import Data.Text (Text, toUpper, unpack)
import GHC.Generics (Generic)
import Text.Regex.TDFA (CompOption(..), ExecOption(..), defaultCompOpt, defaultExecOpt, makeRegexOpts, matchTest)

-- Product
type ProductName = Text
type Manufacturer = Text
type Model = Text
type Family = Text

data Product = Product {
      productName :: ProductName
    , productManufacturer :: Manufacturer
    , model :: Model
    , family :: Maybe Family
    } deriving (Generic, Show)

instance ToJSON Product where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Product where
    parseJSON (Object p) =
        Product <$> p .:  "product_name"
                <*> p .:  "manufacturer"
                <*> p .:  "model"
                <*> p .:? "family"
    parseJSON _ = mzero

-- Listing
type Title = Text

data Listing = Listing {
      title :: Title
    , listingManufacturer :: Manufacturer
    } deriving (Generic, Show)

instance ToJSON Listing where
    toEncoding (Listing t m) = pairs ("title" .= t <> "manufacturer" .= m)
instance FromJSON Listing where
    parseJSON (Object l) =
        Listing <$> l .: "title"
                <*> l .: "manufacturer"
    parseJSON _ = mzero

-- Result
data Result = Result {
      resultProductName :: ProductName
    , resultListings :: [Listing]        
    } deriving (Generic, Show)

instance ToJSON Result where
    toEncoding (Result n ls) = pairs ("product_name" .= n <> "listings" .= ls)
instance FromJSON Result

-- Getting Products/Listings from JSON files
productsFile, listingsFile, resultsFile :: FilePath
productsFile = "../data/products.txt"
listingsFile = "../data/listings.txt"
resultsFile = "results_test.txt"

extractJSON :: FromJSON a => ByteString -> [a]
extractJSON = map fromJust . filter isJust . map decode . BLC.lines

produceJSON :: ToJSON a => [a] -> ByteString
produceJSON = BLC.unlines . map encode

listingSort (Listing t1 m1) (Listing t2 m2)
    | m1 < m2 = LT
    | m1 > m2 = GT
    | m1 == m2 = compare t1 t2

createListingLookup :: [Listing] -> [(Manufacturer, [(Listing, Listing)])]
createListingLookup ls = map p gs
    where
        p g = (listingManufacturer (fst ((t g) !! 0)), t g)
            where t = map (\l -> (Listing (toUpper (title l)) (toUpper (listingManufacturer l)), l))
        gs = groupBy (\l1 l2 -> listingManufacturer l1 == listingManufacturer l2) ls'
        ls' = map (\l -> Listing (title l) (listingManufacturer l)) ls

makeResult :: Product -> [(Listing, Listing)] -> Result
makeResult p [] = Result (productName p) []
-- makeResult p ls = Result (productName p) $ map snd $ filter (\(upper, _) -> isInfixOf (model p) (title upper)) ls
makeResult p ls = Result (productName p) $ map snd $ filter matcher ls
    where
        matcher (upper, _) = matchTest regex (unpack (title upper))
        regex = makeRegexOpts compOptions execOptions pat
        compOptions = defaultCompOpt{caseSensitive=False, multiline=False, lastStarGreedy=True}
        execOptions = defaultExecOpt{captureGroups=False}
        pat = "^" ++ m ++ " | " ++ m ++ " | " ++ m ++ "$"
        -- pat = "\b" ++ m ++ "\b"
        m = unpack $ model p

createResultsFile :: ByteString -> IO ()
createResultsFile = BL.writeFile resultsFile

main :: IO ()
main = do
    productsContent <- BL.readFile productsFile
    listingsContent <- BL.readFile listingsFile
    let ps = extractJSON productsContent :: [Product]
    let ls = extractJSON listingsContent :: [Listing]
    let ts = createListingLookup $ sortBy listingSort ls

    -- put all listings into a hashmap according to manufacturer as the key
    let h = fromListWith (++) ts
    let rs = map (\p -> makeResult p (lookupDefault [] (toUpper (productManufacturer p)) h)) ps

    createResultsFile $ produceJSON (rs :: [Result])
    putStrLn "results.txt file written"
