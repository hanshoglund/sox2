
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sound.Sox2 where

import Data.Semigroup
import qualified System.IO.Temp
import qualified System.Process
import qualified Control.Concurrent.Async

data Audio = Audio FilePath
type Sox a = IO a

type Seconds = Double
type Cents = Integer

data SoxExpr where
  File       :: FilePath -> SoxExpr
  Repeat     :: Int -> SoxExpr -> SoxExpr
  Delay      :: Seconds -> SoxExpr -> SoxExpr
  PitchShift :: Cents -> SoxExpr -> SoxExpr
  Mix        :: SoxExpr -> SoxExpr -> SoxExpr
  Reverb     :: ReverbMode -> Reverberance -> HighFreqDamping -> SoxExpr -> SoxExpr

instance Semigroup SoxExpr where
  (<>) = Mix
instance Monoid SoxExpr where
  mappend = Mix

compile :: SoxExpr -> Sox Audio
compile = go where
  go (File p)    = return (Audio p)
  go (Repeat n a) = do
    a' <- compile a
    repeat' n a'
  go (Delay t a) = do
    a' <- compile a
    pad t a'
  go (PitchShift c a) = do
    a' <- compile a
    pitch c a'
  go (Reverb m r hfd a) = do
    a' <- compile a
    reverb m r hfd a'
  go (Mix a b)   = do
    -- a' <- compile a
    -- b' <- compile b
    (a', b') <- Control.Concurrent.Async.concurrently (compile a) (compile b)
    mix2 a' b'

compileAndPlay :: SoxExpr -> IO ()
compileAndPlay e = do
  (Audio p) <- compile e
  System.Process.system $ "play " ++ p ++ " 1>/dev/null 2>/dev/null"
  return ()

compileAndShow :: SoxExpr -> IO ()
compileAndShow e = do
  (Audio p) <- compile e
  System.Process.system $ "open -a audacity " ++ p
  return ()

-- Internal
newOutput :: Sox FilePath
newOutput = do
  System.Process.system "mkdir -p .sox2"
  (p,_) <-  System.IO.Temp.openTempFile ".sox2" "sox2.wav"
  return p

runSystem x = do
  -- putStrLn x
  System.Process.system x

-- End internal



type Frequency = Double
type Width = String
-- allpass frequency[k] width[h|k|o|q]
allpass :: Frequency -> Width -> Audio -> Sox Audio
allpass f w (Audio inf) = do
  outf <- newOutput
  runSystem $ "sox "++inf++" "++outf++" allpass " ++ show (round f)-- ++ " " ++ w
  return $ Audio outf

mix2 :: Audio -> Audio -> Sox Audio
mix2 (Audio inf1) (Audio inf2) = do
  outf <- newOutput
  runSystem $ "sox -m "++inf1++" "++inf2++" "++outf
  return $ Audio outf

-- band [−n] center[k] [width[h|k|o|q]]
band :: Audio -> Sox Audio
band = error "No band"

-- bandpass|bandreject [−c] frequency[k] width[h|k|o|q]
bandpass :: Audio -> Sox Audio
bandpass = error "No bandpass"

-- bandreject frequency[k] width[h|k|o|q]
bandreject :: Audio -> Sox Audio
bandreject = error "No bandreject"

trebleGain :: Audio -> Sox Audio
trebleGain = error "No trebleGain"

bassGain :: Audio -> Sox Audio
bassGain = error "No bassGain"

-- bend [−f frame-rate(25)] [−o over-sample(16)] { delay,cents,duration }
bend :: Audio -> Sox Audio
bend = error "No bend"

-- biquad b0 b1 b2 a0 a1 a2
biquad :: Audio -> Sox Audio
biquad = error "No biquad"

-- channels CHANNELS
channels :: Audio -> Sox Audio
channels = error "No channels"

-- chorus gain-in gain-out <delay decay speed depth −s|−t>
chorus :: Audio -> Sox Audio
chorus = error "No chorus"

-- compand attack1,decay1{,attack2,decay2}
compand :: Audio -> Sox Audio
compand = error "No compand"

-- contrast [enhancement-amount(75)]
contrast :: Audio -> Sox Audio
contrast = error "No contrast"

-- dcshift shift [limitergain]
dcshift :: Audio -> Sox Audio
dcshift = error "No dcshift"

-- deemph
deemph :: Audio -> Sox Audio
deemph = error "No deemph"

-- TODO this is tape delay, not what you think!
-- delay {length}
delay :: Seconds -> Audio -> Sox Audio
delay s (Audio inf) = do
  outf <- newOutput
  runSystem $ "sox "++inf++" "++outf++" delay " ++ show s
  return $ Audio outf

-- dither [−S|−s|−f filter] [−a] [−p precision]
dither :: Audio -> Sox Audio
dither = error "No dither"

-- downsample [factor(2)]
downsample :: Audio -> Sox Audio
downsample = error "No downsample"

-- earwax
earwax :: Audio -> Sox Audio
earwax = error "No earwax"

-- echo gain-in gain-out <delay decay>
echo :: Audio -> Sox Audio
echo = error "No echo"

-- echos gain-in gain-out <delay decay>
echos :: Audio -> Sox Audio
echos = error "No echos"

-- equalizer frequency[k] width[q|o|h|k] gain
equalizer :: Audio -> Sox Audio
equalizer = error "No equalizer"


frequency :: Audio -> Sox Audio
frequency = error "No frequency"

-- fade [type] fade-in-length [stop-time [fade-out-length]]
fade :: Audio -> Sox Audio
fade = error "No fade"

-- fir [coefs-file|coefs]
fir :: Audio -> Sox Audio
fir = error "No fir"

-- flanger [delay depth regen width speed shape phase interp]
flanger :: Audio -> Sox Audio
flanger = error "No flanger"

-- gain [−e|−B|−b|−r] [−n] [−l|−h] [gain-dB]
gain :: Audio -> Sox Audio
gain = error "No gain"


-- highpass|lowpass [−1|−2] frequency[k] [width[q|o|h|k]]
highpass :: Audio -> Sox Audio
highpass = error "No highpass"

-- hilbert [−n taps]
hilbert :: Audio -> Sox Audio
hilbert = error "No hilbert"

-- loudness [gain [reference]]
loudness :: Audio -> Sox Audio
loudness = error "No loudness"

-- lowpass [−1|−2] frequency[k] [width[q|o|h|k]]
lowpass :: Audio -> Sox Audio
lowpass = error "No lowpass"

-- mcompand "attack1,decay1{,attack2,decay2}
mcompand :: Audio -> Sox Audio
mcompand = error "No mcompand"

-- noiseprof [profile-file]
noiseprof :: Audio -> Sox Audio
noiseprof = error "No noiseprof"

-- noisered [profile-file [amount]]
noisered :: Audio -> Sox Audio
noisered = error "No noisered"

-- norm [dB-level]
norm :: Audio -> Sox Audio
norm = error "No norm"

-- oops
oops :: Audio -> Sox Audio
oops = error "No oops"

-- pad { length[@position] }
pad :: Seconds -> Audio -> Sox Audio
pad s (Audio inf) = do
  outf <- newOutput
  runSystem $ "sox "++inf++" "++outf++" pad " ++ show s
  return $ Audio outf

-- phaser gain-in gain-out delay decay speed [−s|−t]
phaser :: Audio -> Sox Audio
phaser = error "No phaser"


shift :: Audio -> Sox Audio
shift = error "No shift"

pitch :: Cents -> Audio -> Sox Audio
pitch c (Audio inf) = do
  outf <- newOutput
  runSystem $ "sox "++inf++" "++outf++" pitch " ++ show c
  return $ Audio outf

{-
pitch, speed, rate :: Audio -> Sox Audio
-- rate [−q|−l|−m|−h|−v] [override-options] RATE[k]
-}

-- remix [−a|−m|−p] <out-spec>
remix :: Audio -> Sox Audio
remix = error "No remix"

-- repeat [count (1)]
repeat' :: Int -> Audio -> Sox Audio
repeat' n (Audio inf) = do
  outf <- newOutput
  runSystem $ "sox "++inf++" "++outf++" repeat " ++ show n
  return $ Audio outf

-- reverb [−w|−−wet-only] [reverberance (50%) [HF-damping (50%)
newtype Percent = Percent Int
  deriving (Eq, Ord, Num, Enum, Real, Integral)
instance Show Percent where
  show (Percent x) = show (limit (0,100) x)
    where
      limit (m,n) x = (m `max` x) `min` n

data ReverbMode = DryWet | Wet
instance Show ReverbMode where
  show DryWet = ""
  show Wet    = "--wet-only"
type Reverberance = Percent
type HighFreqDamping = Percent
reverb :: ReverbMode -> Reverberance -> HighFreqDamping -> Audio -> Sox Audio
reverb m r hfd (Audio inf) = do
  outf <- newOutput
  runSystem $ "sox "++inf++" "++outf++" reverb " ++ show m ++ " " ++ show r ++ " " ++ show hfd
  return $ Audio outf

-- reverse
reverse :: Audio -> Sox Audio
reverse = error "No reverse"

-- riaa
riaa :: Audio -> Sox Audio
riaa = error "No riaa"

-- silence [−l] above-periods [duration threshold[d|%]
silence :: Audio -> Sox Audio
silence = error "No silence"

-- sinc [−a att|−b beta] [−p phase|−M|−I|−L] [−t tbw|−n taps] [freqHP]
-- [−freqLP [−t tbw|−n taps]]
sinc :: Audio -> Sox Audio
sinc = error "No sinc"

-- spectrogram [options]
spectrogram :: Audio -> Sox Audio
spectrogram = error "No spectrogram"

-- speed factor[c]
speed :: Audio -> Sox Audio
speed = error "No speed"

-- splice [−h|−t|−q] { position[,excess[,leeway]] }
splice :: Audio -> Sox Audio
splice = error "No splice"

-- stat [−s scale] [−rms] [−freq] [−v] [−d]
stat :: Audio -> Sox Audio
stat = error "No stat"

-- stats [−b bits|−x bits|−s scale] [−w window-time]
stats :: Audio -> Sox Audio
stats = error "No stats"

-- swap
swap :: Audio -> Sox Audio
swap = error "No swap"

-- tempo [−q] [−m|−s|−l] factor [segment [search [overlap]]]
tempo :: Audio -> Sox Audio
tempo = error "No tempo"

-- treble gain [frequency[k] [width[s|h|k|o|q]]]
treble :: Audio -> Sox Audio
treble = error "No treble"

-- tremolo speed [depth]
tremolo :: Audio -> Sox Audio
tremolo = error "No tremolo"

-- trim {[=|−]position}
trim :: Audio -> Sox Audio
trim = error "No trim"

-- upsample [factor]
upsample :: Audio -> Sox Audio
upsample = error "No upsample"

-- vad [options]
vad :: Audio -> Sox Audio
vad = error "No vad"

-- vol gain [type [limitergain]]
vol :: Audio -> Sox Audio
vol = error "No vol"
