module Extract.PDF
  ( textFromPDF
  , takeFromPDF
  ) where

import Pdf.Toolbox.Document.Pdf
import Pdf.Toolbox.Document.Document
import Pdf.Toolbox.Document.Catalog
import Pdf.Toolbox.Document.PageNode
import Pdf.Toolbox.Document.Page
import Pdf.Toolbox.Document.Monad
import Pdf.Toolbox.Core.Error
import Pdf.Toolbox.Core.Object.Types
import Control.Monad ((>=>), mapM)
import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either (lefts, rights)

withPDFFile name action = withBinaryFile name ReadMode $ \handle ->
  runPdfWithHandle handle knownFilters action

textFromPDF name = withPDFFile name $ do
  document >>= documentCatalog >>= catalogPageNode >>= pageNodeNKids >>= liftIO . print
  refs <- getPageRefs  
  t <- textFromRefs refs
  return t

takeFromPDF name n = withPDFFile name $ do
  refs <- take n <$> getPageRefs
  t <- textFromRefs refs
  return t

textFromRefs :: (MonadPdf m, MonadIO m) => [Ref] -> PdfE m [Text]
textFromRefs refs = do
  trees <- mapM loadPageNode refs
  texts <- mapM getTextFromTree trees
  return $ concat texts

getPageRefs =
  document
  >>= (documentCatalog
        >=> catalogPageNode
        >=> pageNodeKids)

getTextFromTree :: (MonadPdf m, MonadIO m) => PageTree -> PdfE m [Text]
getTextFromTree =
  either getTextFromNode (fmap (:[]) . pageExtractText) . fromTree

getTextFromNode node =
  pageNodeKids node
  >>= mapM loadPageNode
  >>= return . onlyPages
  >>= mapM pageExtractText

fromTree :: PageTree -> Either PageNode Page
fromTree (PageTreeNode n) = Left n
fromTree (PageTreeLeaf p) = Right p

onlyNodes = lefts . map fromTree
onlyPages = rights . map fromTree
