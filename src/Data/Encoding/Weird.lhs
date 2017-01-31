\documentclass{beamer}
%include polycode.fmt

\usepackage{color}
\usepackage{fontspec}
\usepackage{xunicode}
\usepackage[utf8]{inputenc}

\newcommand{\appdollar}{\mathbin{\langle\$\rangle}}

%if style /= newcode
%format <$> = "\appdollar "
%subst keyword a = "\Varid{\color[rgb]{0,0,0.7}{" a "}}"
%else
%endif

%options ghci -pgmL lhs2TeX -optL--pre

\title{QuickCheck}
\subtitle{An introduction}
\author{Daniel~Larsson}
\institute[GroupTalk AB] % (optional)
{
  GroupTalk AB
}

\begin{document}

%if style /= poly
\begin{code}
module Data.Encoding.Weird 
       ( encode
       , decode
       ) where

import Control.Applicative
import Data.Binary.Builder (Builder)
import qualified Data.Binary.Builder as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Instances
\end{code}
%endif

\frame{\titlepage}

\begin{frame}
  \frametitle{What is QuickCheck?}

  \begin{itemize}[<+->]
    \item Library for unit testing your code
    \item You write \emph{invariants} for your code
    \item The library generates test data
  \end{itemize}
 
  \pause
  
\begin{code}
prop_reverse_identity :: Eq a => [a] -> Bool
prop_reverse_identity s = s == (reverse . reverse) s
\end{code}

\pause

\perform{quickCheck prop_reverse_identity}

\pause

\begin{code}
prop_reverse_ends s = compareEnds s (reverse s)
  where  compareEnds []       _  = True
         compareEnds s@(h:t)  r  =
           let  rl  = last r
                ri  = init r 
           in h == rl && compareEnds t ri
\end{code}

\pause

\perform{quickCheck prop_reverse_ends}

\end{frame}

\begin{frame}
  \frametitle{An example, encoding a string}

Here is a nonsense encoder, converting a Text into a ByteString.

\begin{code}
encode :: Text -> BS.ByteString
encode = B.toLazyByteString . prepend terminator <$> enc
  where  enc         :: Text -> Builder
         enc         = T.foldr (B.append . encChar) B.empty
         encChar     = B.singleton . fromIntegral  . ord
         prepend     = flip B.append
         terminator  = encChar '\0'
\end{code}
\end{frame}

\begin{frame}
  \frametitle{An example, decoding a string}

This is the inverse of encode, we decode a ByteString, returning a Text
\begin{code}
decode  :: BS.ByteString -> Text
decode  = T.unfoldr decodeChar
  where  decodeChar :: ByteString -> Maybe (Char, ByteString)
         decodeChar bs = 
           let ch = BS.head bs
           in if ch == 0
              -- We found the end, stop the unfold
              then Nothing
              -- Return the converted character, and the
              --  remainder of the string we need to convert
              else Just (chr $ fromIntegral ch, BS.tail bs)
\end{code}
\end{frame}

\begin{frame}
  \frametitle{An example, property}

As mentioned, \texttt{decode} and \texttt{encode} should be inverses of each other. We can express
this with the following property:


\begin{code}
prop_reversible s = s == (decode . encode) s
\end{code}

QuickCheck can help us verify that this property is true:

\begin{spec}
quickCheck prop_reversible
\end{spec}

\pause

\eval{quickCheck prop_reversible}

\end{frame}

\begin{frame}
  \frametitle{An example, fixing the bug}

\begin{code}
encode2 :: Text -> BS.ByteString
encode2 = B.toLazyByteString . prepend terminator <$> enc
  where  enc         :: Text -> Builder
         enc         = T.foldr (B.append . encChar) B.empty
         prepend     = flip B.append
         terminator  = encChar '\0'

charBuilder  :: Char -> Builder
charBuilder  = B.singleton . fromIntegral . ord

encChar       :: Char -> Builder
encChar '\0'  = charBuilder '\\' `B.append` charBuilder '\0'
encChar '\\'  = charBuilder '\\' `B.append` charBuilder '\\'
encChar c     = charBuilder c
\end{code}
\end{frame}

\begin{frame}
  \frametitle{An example, fixing the bug}

\begin{code}
decode2   :: BS.ByteString -> Text
decode2   = T.unfoldr decodeChar
  where  decodeChar     :: ByteString -> Maybe (Char, ByteString)
         decodeChar bs  = 
           let  ch    = BS.head bs
                chr'  = chr . fromIntegral
           in if ch == 0
                 -- We found the end, stop the unfold
                then Nothing
                else if chr' ch /= '\\'
                      -- Return the converted character, and the
                      --  remainder of the string we need to convert
                      then Just (chr' ch, BS.tail bs)
                      -- Read the next character
                      else let ch' = BS.head $ BS.tail bs
                           in Just (chr' ch', BS.tail $ BS.tail bs)
\end{code}
\end{frame}

\begin{frame}
  \frametitle{An example, property}

Identical property, with the updated encode/decode implementations
\begin{code}
prop_reversible2 s = s == (decode2 . encode2) s
\end{code}

Let us check the property again with QuickCheck

\begin{spec}
quickCheck prop_reversible2
\end{spec}

\pause

\perform{quickCheck prop_reversible2}

\end{frame}

\begin{frame}
  \frametitle{An example, fixing the bug}

\begin{code}
encode3 :: Text -> BS.ByteString
encode3 = B.toLazyByteString . prepend terminator <$> enc
  where  enc         :: Text -> Builder
         enc         = T.foldr (B.append . encChar) B.empty
         prepend     = flip B.append
         -- This line changed. We can't call encChar here,
         -- since it will escape the character!
         terminator  = charBuilder '\0'
\end{code}
\end{frame}

\begin{frame}
  \frametitle{An example, property}

Okay, testing with the 3rd version of encode.

\begin{code}
prop_reversible3 s = s == (decode2 . encode3) s
\end{code}

\begin{spec}
quickCheck prop_reversible3
\end{spec}

\pause

\perform{quickCheck prop_reversible3}

Finally!

\end{frame}

\begin{frame}
  \frametitle{An example, property}

...or?

\begin{spec}
prop_reversible3 "5.2‰"
\end{spec}

\pause

\eval{False}
% Not getting any output from this within lhs2tex??
%\eval{prop_reversible3 "5.2‰"}

\pause

Hmmmm... our encoder/decoder is too simplistic, it doesn't handle
multibyte unicode characters. And the standard test data generator for
the Text datatype doesn't generate multibyte characters either, so
this isn't being detected.

\end{frame}

\begin{frame}
  \frametitle{Other libraries and tools for testing}

  \begin{itemize}[<+->]
    \item SmallCheck - Similar to QuickCheck, but exhaustive test case 
          generation, rather than randomized
    \item HUnit - Your regular xUnit test tool
    \item HSPec - Inspired by Ruby's RSpec. Can incorporate QuickCheck tests
    \item tasty - A testing framework for organizing tests. The actual
          test cases can be written using any of the above
    \item But, use the type system and the compiler to your advantage!
  \end{itemize}
\end{frame}

\begin{frame}
  \frametitle{QuickCheck internals}

So, Haskell is a (very) strongly typed language, what the heck is the type of \texttt{quickCheck}?

\pause

\eval{:t quickCheck}

\pause

\begin{spec}
class Testable prop where
  property    :: prop -> Property
  exhaustive  :: prop -> Bool
  exhaustive _ = False

instance Testable Bool

instance (Arbitrary a, Show a, Testable prop) =>
         Testable (a -> prop)
\end{spec}

  \begin{itemize}[<+->]
    \item To be \texttt{Testable}, the type needs to be convertable to a \texttt{Property}.
    \item A function is \texttt{Testable} if the result is \texttt{Testable}, 
          and the argument is \texttt{Arbitrary} and can be converted to a 
          string (\texttt{Show})
  \end{itemize}

\end{frame}

\begin{frame}
  \frametitle{QuickCheck internals}

\begin{spec}
class Arbitrary a where
  -- A generator for values of the given type.
  arbitrary :: Gen a
  arbitrary = error "no default generator"

  -- Produces a (possibly) empty list of all the possible
  -- immediate shrinks of the given value.
  shrink :: a -> [a]
  shrink _ = []
\end{spec}

\texttt{Gen} is a monad for producing random test data. Instances of \texttt{Arbitrary} are
responsible for creating random values.

\end{frame}

\begin{frame}
  \frametitle{QuickCheck internals}

\begin{spec}
instance Arbitrary Bool where
  arbitrary = choose (False,True)
  shrink True   = [False]
  shrink False  = []

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = frequency  [ (1, return Nothing)
                         , (3, liftM Just arbitrary)
                         ]
  shrink (Just x)  = Nothing : [ Just x' | x' <- shrink x ]
  shrink _         = []

instance Arbitrary TS.Text where
    arbitrary = TS.pack <$> arbitrary
    shrink xs = TS.pack <$> shrink (TS.unpack xs)

instance Arbitrary Char where
  arbitrary = chr `fmap` oneof [choose (0,127), choose (0,255)]
\end{spec}


\end{frame}

\end{document}
