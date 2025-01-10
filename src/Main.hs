module Main (main) where

import DiscordHandler
import Network.DigitalOcean.CloudFunctions.Handler

main ∷ IO ()
main = handle discordHandler
