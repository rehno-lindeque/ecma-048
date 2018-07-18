module Codec.Terminal.ECMA048 where

import Codec.Terminal.ECMA048.C0
import Codec.Terminal.ECMA048.C1 as C1 (isC1, decodeC1, C1(..))
import Codec.Terminal.ECMA048.ControlSequences
import Codec.Terminal.ECMA048.ControlFunctions
import Codec.Terminal.ECMA048.ICF

import Control.Applicative
import Control.Monad
import Data.Attoparsec as AP
import Data.ByteString

c0 :: Parser C0
c0 = decodeC0 <$> satisfy isC0 <?> "C0"

c1_7bit :: Parser C1
c1_7bit = esc >> fe <?> "C1"
    where fe = decodeC1 0x40 <$> satisfy (isC1 0x40) <?> "FE"

c1_8bit :: Parser C1
c1_8bit = decodeC1 0x80 <$> satisfy (isC1 0x80) <?> "C1"

c1_8bit_announced :: Parser C1
c1_8bit_announced = c1_8bit <|> (announcer >> c1_7bit)
    where 
        -- not using 'string' because it returns 'Partial' until there are 3
        -- bytes available, even when it already has enough to tell there's
        -- no match.
        announcer = mapM_ word8 [0x1b, 0x20, 0x46] <?> "7-bit announcer"

csi :: Parser C1 -> Parser C1
csi c1  = do CSI <- c1; return CSI

controlSequence :: Parser C1 -> Parser (Either ByteString ControlFunction, ByteString)
controlSequence c1 = do
    csi c1
    params          <- AP.takeWhile isParam
    intermediates   <- AP.takeWhile isIntermediate
    opcode          <- satisfy isOpcode
    
    let cmd = snoc intermediates opcode
    
    case decodeControlFunction cmd of
        Nothing   -> return (Left cmd, params)
        Just func -> return (Right func, params)

icf :: Parser ICF
icf = esc >> decodeICF <$> satisfy isICF <?> "independent control function"

startControlString :: Parser C1 -> Parser C1
startControlString c1 = do
        c <- c1
        case c of
            APC   -> return c
            DCS   -> return c
            OSC   -> return c
            C1.PM -> return c
            SOS   -> return c
            _     -> mzero
    <?> "Start of control string"

stringTerminator :: Parser C1 -> Parser C1
stringTerminator c1 = do ST <- c1; return ST <?> "ST"

data Control
    = C0 !C0
    | C1 !C1
    | CF !ControlFunction !ByteString
    | PCF !ByteString !ByteString
    | ICF !ICF
    | StartCS !C1
    | EndCS
    deriving (Eq, Ord, Show, Read)

control :: Parser C1 -> Parser Control
control c1 = choice
    [ do
        (cf, params) <- controlSequence c1
        return $! case cf of
            Right cf -> CF  cf params
            Left  cf -> PCF cf params
    , ICF <$> icf
    , C1 <$> c1
    , C0 <$> c0
    ]

