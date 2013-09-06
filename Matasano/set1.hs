import Crypto
import qualified Data.ByteString.Internal as B (c2w, w2c)
import qualified Data.Map as M
import System.IO  
import qualified Data.List as L

freqs = getFreq "I was a war baby, but having been conceived just a couple of months prior to the Japanese attack on Pearl Harbor, I like to think of myself as a pre-war baby, too—just like Superman, who was born into the pages of a comic book in 1938, as a fully grown, fully clothed and / or costumed man. He burst forth with powerful muscles already intact and could fly, like, immediately. There was no learning curve, no sweaty workouts at the gym, and no need for a diploma. I, on the other hand, was nowhere near as strong. I never once thought about digging in my heels to stop a speeding locomotive. And tilting the axis of the world was totally out of my wheelhouse. I didn’t have any particular feelings for anyone who could perform these feats, either—braggarts and schoolyard bullies all. When I discovered comic books, at the age of nine or ten, I gravitated toward horror, crime, war, and science fiction—probably due to their higher mayhem quotient. Sure, Superman beat up a lot of bad guys, but he never seemed to want to tear their heads off and then pitch them really hard into the side of a tall building. Also, “Weird Science” and other comic books of that ilk were, to my eyes, better drawn and colorized than Superman (or Batman or any of the other caped or non-caped crusaders), which also may be what drew me to them in the first place. And the stories were more exciting and intense. Bear in mind, however, that these were the days before any of the superdudes became fashionably pop-psychologized with inner feelings of indecisiveness, anguish, dread, and romance. Oh, sure, Clark Kent and Lois Lane had their fleeting moments, but those moments never seemed particularly real or teen-age-fantasy carnal. But why am I going on and on about the so-called Man of Steel? Well, for one thing, they tell me he’s got a new movie out, one of those multi-million-dollar blockbusters that seem to be all the rage these days. (Give me the simplicity of a “High Noon” or, better yet, a “Purple Noon,” I say, my voice crying out in the wilderness.) And for the other thing, the New Yorker cartoon editor, Bob Mankoff, asked me to be a guest blogger while he’s off somewhere on “assignment.” The “Man of Steel” opening and Bob’s “vacation” conveniently coinciding, he recruited me by pointing out that, to date, I have published more Superman cartoons in The New Yorker than anyone else in its long and glorious history, thirteen to be precise—which is not a lot, considering that I’ve been cartooning for the magazine for forty years now. And the first of the Superman cartoons didn’t even appear until the issue of October 24, 1988, deep into my second decade with the magazine."

main = do
  putStrLn "1."
  putStrLn "-----------------------------"
  putStrLn "2."
  putStrLn "1c0111001f010100061a024b53535009181c XOR 686974207468652062756c6c277320657965 = " 
  putStrLn $ bytesToHex $ (hexToBytes "1c0111001f010100061a024b53535009181c") `fixedXOR` (hexToBytes "686974207468652062756c6c277320657965")
  putStrLn "-----------------------------"
  putStrLn "3."
  putStrLn "Decrypt 1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736:"
  putStrLn $ problem3 "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  putStrLn "-----------------------------"
  putStrLn "4."
  handle <- openFile "gistfile1.txt" ReadMode  
  contents <- hGetContents handle  
  --putStrLn $ problem4 $ lines $ contents  
  hClose handle  
  putStrLn "-----------------------------"
  putStrLn "5."
  putStrLn $ bytesToHex $ repKeyXOR (stringToBytes "ICE") (stringToBytes "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal")
  putStrLn "-----------------------------"
  putStrLn "6."
  handle2 <- openFile "gistfile2.txt" ReadMode  
  contents2 <- hGetContents handle2  
  putStrLn $ show $ problem5 $ concat $ lines contents2 
  hClose handle2  


problem3 :: String -> String
problem3 s = show $ decryptSingleCharXOR freqs (hexToBytes s) 

problem4 :: [String] -> String
problem4 s = show $ L.minimumBy (\(x,y,z) (a,b,c) -> compare z c) $ map (decryptSingleCharXOR freqs) $ map hexToBytes s 
 
problem5 :: String -> (String,String)
problem5 s = problem5' $ base64ToBytes s
  where problem5' Nothing = ("","ERROR")
        problem5' (Just a) = decryptRepKeyXOR freqs a
