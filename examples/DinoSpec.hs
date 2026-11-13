-- Exercise the real example with deterministic models, including both routes.
module DinoSpec (check) where

import Agentic
import Agentic.IO (concurrently)
import Agentic.Runtime (Conversation (..))
import Agentic.Schema (Schema (..))
import Agentic.Scripted (modelContract, respond)
import Control.Monad (unless)
import Data.IORef
import Dino

check :: IO ()
check = do
  let dinosaur = Creature "Triceratops" "A horned dinosaur"
      other = Creature "Pteranodon" "A flying reptile"
      picture = DinoPic " /^^/ "
      card = TrumpCard (Stat 8) (Stat 3) (Stat 4) (Stat 5)
      fact = FactCard "Not a dinosaur!" "Pteranodon was a pterosaur."
      poster = Poster "Prehistoric creatures" "Both kinds of exhibit"
      equal label a b = unless (a == b) (fail label)
      encode :: Contract a => a -> Value
      encode = modelContract.encode
  -- The route depends on the chosen kind, not a threshold that could turn a
  -- low-confidence dinosaur into a purported non-dinosaur.
  case route (dinosaur, Choice Dinosaur [(Dinosaur,0.4)] 0.4) of
    Left c -> equal "low-confidence dinosaur lost" (encode c) (encode dinosaur)
    Right _ -> fail "low-confidence dinosaur mislabelled as another kind"
  mapM_ (\kind -> case route (other, Choice kind [(kind,1)] 1) of
    Right c -> equal "classification lost" c.actually kind
    Left _ -> fail "non-dinosaur took dinosaur branch") [Pterosaur,MarineReptile,Fish,Mammal,Other]

  -- Script by output contract, not call order, so the same assertions exercise
  -- the example's real concurrent runtime as well as sequential execution.
  let run parallel creatures = do
        calls <- newIORef []
        questions <- newIORef (0 :: Int)
        selected <- newIORef []
        let expected c = if encode c == encode dinosaur
              then DinosaurExhibit (Entry c picture card) else OtherExhibit fact
            one = SystemOne $ \request -> do
              atomicModifyIORef' questions (\n -> (n+1,()))
              let label = if request.input == encode dinosaur then "Dinosaur" else "Pterosaur"
              pure [ChoiceAnswer label [(label,1)] 1]
            two = SystemTwo $ \conversation -> do
              atomicModifyIORef' calls (\cs -> (conversation.outputSchema.shape:cs,()))
              answer <- if conversation.outputSchema.shape == (modelContract @[Creature]).schema.shape
                then pure (respond creatures)
                else if conversation.outputSchema.shape == (modelContract @DinoPic).schema.shape
                  then equal "picture input" conversation.input (encode dinosaur) >> pure (respond picture)
                else if conversation.outputSchema.shape == (modelContract @TrumpCard).schema.shape
                  then equal "trump card input" conversation.input (encode dinosaur) >> pure (respond card)
                else if conversation.outputSchema.shape == (modelContract @FactCard).schema.shape
                  then equal "fact card input" conversation.input (encode (NotADinosaur other Pterosaur)) >> pure (respond fact)
                else if conversation.outputSchema.shape == (modelContract @Poster).schema.shape
                  then do
                    equal "poster must receive both tagged entries in input order" conversation.input
                      (encode (Exhibit (map expected creatures)))
                    pure (respond poster)
                else fail "unexpected model contract"
              pure (Turn (Raw Null) answer)
            observeChoice e = case e.happened of
              Selected (EdgeId _ side) -> atomicModifyIORef' selected (\ss -> (side:ss,()))
              _ -> pure ()
            rt = observing observeChoice runtime{systemOne=one,systemTwo=two}
        result <- interpret (if parallel then concurrently rt else rt) dinoProject ()
        equal "poster result" (encode result) (encode poster)
        readIORef questions >>= equal "classify each creature exactly once" (length creatures)
        cs <- readIORef calls
        let dinosaurs = length [() | c <- creatures, encode c == encode dinosaur]
            others = length creatures - dinosaurs
        equal "one picture per dinosaur" dinosaurs (length (filter (== (modelContract @DinoPic).schema.shape) cs))
        equal "one trump card per dinosaur" dinosaurs (length (filter (== (modelContract @TrumpCard).schema.shape) cs))
        equal "one fact card per other creature" others (length (filter (== (modelContract @FactCard).schema.shape) cs))
        equal "no extra model calls" (2 + 2 * dinosaurs + others) (length cs)
        ss <- readIORef selected
        equal "left selections" dinosaurs (length (filter (== LeftSide) ss))
        equal "right selections" others (length (filter (== RightSide) ss))
  run False [dinosaur,other]
  run True [other,dinosaur]
  run True []
