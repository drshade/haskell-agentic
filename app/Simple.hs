module Simple (prompt, extractWith, injectWith, extract, inject, Dhall, Json, run, Text, (>>>)) where

import           Agentic                      hiding (extract, inject, run)
import           Control.Arrow                (Kleisli, (>>>))
import           Control.Monad.RWS            (RWST)
import           Data.Text
import           Dhall                        (FromDhall, ToDhall)
import           Progress
import           Protocol.Class
import           Protocol.DhallSchema.Marshal
import           Protocol.JSONSchema.Marshal

extract :: forall s m. (FromDhall s, ToDhall s) => Agentic m Prompt s
extract = extractWith @Dhall @s

inject :: forall s m. (FromDhall s, ToDhall s) => s -> Agentic m Prompt Prompt
inject = injectWith @Dhall @s

run :: Kleisli (RWST Environment Events State IO) Text b -> Text -> IO b
run = runWithProgress
